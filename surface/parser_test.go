package surface_test

import (
	"errors"
	"strings"
	"testing"

	"goforge.dev/rune/core"
	"goforge.dev/rune/surface"
)

// resolveSrc parses and resolves a closed expression to core, failing on error.
func resolveSrc(t *testing.T, src string) core.Tm {
	t.Helper()
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	c, err := (&surface.Resolver{}).ResolveExp(e)
	if err != nil {
		t.Fatalf("resolve %q: %v", src, err)
	}
	return c
}

// TestParenDisambiguation pins GRAMMAR.md §5.1: one token past ')' decides between a
// dependent Pi binder, a non-dependent arrow, and a parenthesized ascription.
func TestParenDisambiguation(t *testing.T) {
	cases := []struct {
		src, want string
	}{
		{"(A : U) -> A", "(Π U. #0)"}, // dependent: A bound in the codomain
		{"(U) -> U", "(Π U. U)"},      // non-dependent: parenthesized domain, binder unused
		{"U -> U", "(Π U. U)"},        // bare non-dependent arrow
		{"(U : U)", "(U : U)"},        // ascription standing alone
		{"(A : U) -> A -> A", "(Π U. (Π #0. #1))"},
	}
	for _, c := range cases {
		got := surface.DebugCore(resolveSrc(t, c.src))
		if got != c.want {
			t.Errorf("%q: got %q, want %q", c.src, got, c.want)
		}
	}
}

// TestSeqEquivalence pins the §6 desugaring on concrete inputs, across both separators.
func TestSeqEquivalence(t *testing.T) {
	cases := []struct {
		seq, let string
	}{
		{"seq U end", "U"},
		{"seq let a = U; a end", "let a = U in a"},
		{"seq\n  let a = U\n  let b : U = a\n  b\nend", "let a = U in let b : U = a in b"},
		{"seq let a = U; let b = a; a end", "let a = U in let b = a in a"},
	}
	for _, c := range cases {
		gotSeq := core.HashTerm(resolveSrc(t, c.seq))
		gotLet := core.HashTerm(resolveSrc(t, c.let))
		if gotSeq != gotLet {
			t.Errorf("seq %q != let %q\n  %s\n  %s", c.seq, c.let, gotSeq, gotLet)
		}
	}
}

// TestSeqErrors pins the §5.3 well-formedness rules: non-final items must be bindings,
// bindings take no `in`, and a seq must end in a result.
func TestSeqErrors(t *testing.T) {
	cases := []struct {
		src, want string
	}{
		{"seq end", "must produce a value"},
		{"seq let x = U end", "must end with a result expression"},
		{"seq U; U end", "non-final seq item must be a binding"},
		{"seq let x = U in x; x end", "does not take 'in'"},
	}
	for _, c := range cases {
		_, err := surface.ParseExpr(c.src)
		if err == nil {
			t.Errorf("%q: expected an error", c.src)
			continue
		}
		if !strings.Contains(err.Error(), c.want) {
			t.Errorf("%q: error %q does not contain %q", c.src, err.Error(), c.want)
		}
	}
}

// TestIncomplete pins ErrIncomplete: an input that runs out mid-form is distinguishable
// from a hard syntax error, which is what the REPL uses to decide on a continuation.
func TestIncomplete(t *testing.T) {
	exprs := []string{
		"fn (x : U) is x", // missing 'end'
		"seq let a = U",   // missing 'end'
		"(x : U",          // missing ')'
		"fn (x : U) is",   // missing body + 'end'
	}
	for _, src := range exprs {
		_, err := surface.ParseExpr(src)
		if !errors.Is(err, surface.ErrIncomplete) {
			t.Errorf("ParseExpr(%q): want ErrIncomplete, got %v", src, err)
		}
	}
	files := []string{
		"id : U is U", // missing definition 'end'
		"id :",        // missing type, 'is', body, 'end'
	}
	for _, src := range files {
		_, err := surface.ParseFile(src)
		if !errors.Is(err, surface.ErrIncomplete) {
			t.Errorf("ParseFile(%q): want ErrIncomplete, got %v", src, err)
		}
	}
}

// TestComments pins §2: line comments to end of line, and nestable block comments.
func TestComments(t *testing.T) {
	cases := []string{
		"U -- trailing comment",
		"{- block -} U",
		"{- outer {- nested -} still outer -} U",
		"U {- after -}",
	}
	for _, src := range cases {
		if got := surface.DebugCore(resolveSrc(t, src)); got != "U" {
			t.Errorf("%q: got %q, want \"U\"", src, got)
		}
	}
	if _, err := surface.ParseExpr("{- unterminated U"); err == nil {
		t.Errorf("unterminated block comment should error")
	}
}

// TestNoBareAscription pins §5.1: ascription exists only as the parenthesized atom
// `(e : T)`; a bare top-level `e : T` is not an expression.
func TestNoBareAscription(t *testing.T) {
	if _, err := surface.ParseExpr("U : U"); err == nil {
		t.Errorf("bare `U : U` should not parse as an expression")
	}
}
