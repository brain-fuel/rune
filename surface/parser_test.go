package surface_test

import (
	"errors"
	"reflect"
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

// TestInfixOperators pins GRAMMAR §3/§5.4: the closed two-level operator table,
// left associativity, and that `x + y` is nothing but the application `(+) x y`.
func TestInfixOperators(t *testing.T) {
	v := func(n string) surface.Exp { return surface.EVar{Name: n} }
	bin := func(op string, l, r surface.Exp) surface.Exp {
		return surface.EApp{Fn: surface.EApp{Fn: surface.EVar{Name: op}, Arg: l}, Arg: r}
	}
	eq := func(l, r surface.Exp) surface.Exp {
		return surface.EApp{Fn: surface.EApp{Fn: surface.EApp{Fn: surface.EEq{}, Arg: surface.EHole{}}, Arg: l}, Arg: r}
	}
	cases := []struct {
		src  string
		want surface.Exp
	}{
		{"a + b * c", bin("+", v("a"), bin("*", v("b"), v("c")))},
		{"a * b + c", bin("+", bin("*", v("a"), v("b")), v("c"))},
		{"a - b - c", bin("-", bin("-", v("a"), v("b")), v("c"))},
		{"a / b % c", bin("%", bin("/", v("a"), v("b")), v("c"))},
		{"f x + g y", bin("+", surface.EApp{Fn: v("f"), Arg: v("x")}, surface.EApp{Fn: v("g"), Arg: v("y")})},
		{"(+) x y", surface.EApp{Fn: surface.EApp{Fn: v("+"), Arg: v("x")}, Arg: v("y")}},
		{"a + b -> c + d", surface.EPi{Param: "_", Dom: bin("+", v("a"), v("b")), Cod: bin("+", v("c"), v("d"))}},
		{"x = y", eq(v("x"), v("y"))},
		{"a + b = b + a", eq(bin("+", v("a"), v("b")), bin("+", v("b"), v("a")))},
		{"x = y -> y = x", surface.EPi{Param: "_", Dom: eq(v("x"), v("y")), Cod: eq(v("y"), v("x"))}},
	}
	for _, c := range cases {
		got, err := surface.ParseExpr(c.src)
		if err != nil {
			t.Errorf("parse %q: %v", c.src, err)
			continue
		}
		if !reflect.DeepEqual(got, c.want) {
			t.Errorf("%q: got %#v, want %#v", c.src, got, c.want)
		}
	}
}

// TestEqNonAssociative pins §5.4: `=` does not chain.
func TestEqNonAssociative(t *testing.T) {
	_, err := surface.ParseExpr("x = y = z")
	if err == nil || !strings.Contains(err.Error(), "cannot be chained") {
		t.Errorf("x = y = z: want a chaining error, got %v", err)
	}
}

// TestLetAnnotationCarveOut pins §5.4: in a let/seq binding annotation, a
// spine-level `=` ends the annotation; a parenthesized equality type is fine,
// and the carve-out survives arrows and dependent binders in the annotation.
func TestLetAnnotationCarveOut(t *testing.T) {
	srcs := []string{
		"let p : U = U in p",
		"let f : U -> U = fn (x : U) is x end in f",
		"let p : (x = y) = q in p",
		"let g : (A : U) -> A = h in g",
		"seq let p : U = U; p end",
	}
	for _, src := range srcs {
		if _, err := surface.ParseExpr(src); err != nil {
			t.Errorf("parse %q: %v", src, err)
		}
	}
}

// TestOperatorDefinition pins §3 DefName: an operator names a top-level definition.
func TestOperatorDefinition(t *testing.T) {
	defs, err := surface.ParseFile("+ : U -> U -> U is fn (a : U) (b : U) is a end end")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if len(defs) != 1 || defs[0].Name != "+" {
		t.Fatalf("got %+v, want one definition named \"+\"", defs)
	}
}

// TestInfixPrinting pins §8: saturated operator applications print infix, and the
// printed form re-parses to the same core (parse ∘ pretty = id over operators).
func TestInfixPrinting(t *testing.T) {
	plusHash := core.HashTerm(core.Univ{})  // any stable hash to stand for (+)
	timesHash := core.HashTerm(core.Prop{}) // and a distinct one for (*)
	refs := map[string]core.Hash{"+": plusHash, "*": timesHash}
	names := map[core.Hash]string{plusHash: "+", timesHash: "*"}

	cases := []struct {
		src, want string
	}{
		{"fn (a : U) (b : U) (c : U) is a + b * c end", "fn (a : U) (b : U) (c : U) is a + b * c end"},
		{"fn (a : U) (b : U) (c : U) is (a + b) * c end", "fn (a : U) (b : U) (c : U) is (a + b) * c end"},
		{"fn (a : U) (b : U) is f (a + b) end", ""}, // unresolved f: parse-only case below skips
		{"fn (a : U) (b : U) is (+) a end", "fn (a : U) (b : U) is (+) a end"},
	}
	for _, c := range cases {
		if c.want == "" {
			continue
		}
		e, err := surface.ParseExpr(c.src)
		if err != nil {
			t.Errorf("parse %q: %v", c.src, err)
			continue
		}
		tm, err := (&surface.Resolver{Refs: refs}).ResolveExp(e)
		if err != nil {
			t.Errorf("resolve %q: %v", c.src, err)
			continue
		}
		printed := surface.PrettyWith(tm, names)
		if printed != c.want {
			t.Errorf("%q printed as %q, want %q", c.src, printed, c.want)
		}
		e2, err := surface.ParseExpr(printed)
		if err != nil {
			t.Errorf("re-parse %q: %v", printed, err)
			continue
		}
		tm2, err := (&surface.Resolver{Refs: refs}).ResolveExp(e2)
		if err != nil {
			t.Errorf("re-resolve %q: %v", printed, err)
			continue
		}
		if core.HashTerm(tm) != core.HashTerm(tm2) {
			t.Errorf("round-trip changed the core for %q (printed %q)", c.src, printed)
		}
	}
}

// TestNumerals pins §5.5: literals expand against the registered builtin nat at
// parse time; without a binding they are a parse error; quantities still parse.
func TestNumerals(t *testing.T) {
	prog := `data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
two : Nat is 2 end
zeroLit : Nat is 0 end
qty : (0 A : U) -> (1 x : A) -> A is fn {- placeholder -} (0 A : U) (1 x : A) is x end end`
	items, err := surface.ParseProgram(prog)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	var two surface.Exp
	var sawBuiltin bool
	for _, it := range items {
		switch d := it.(type) {
		case surface.BuiltinNat:
			sawBuiltin = true
			if d.TyName != "Nat" || d.Zero != "zero" || d.Succ != "succ" {
				t.Fatalf("builtin parsed as %+v", d)
			}
		case surface.Def:
			if d.Name == "two" {
				two = d.Body
			}
		}
	}
	if !sawBuiltin {
		t.Fatal("builtin nat item missing")
	}
	want := surface.EApp{Fn: surface.EVar{Name: "succ"},
		Arg: surface.EApp{Fn: surface.EVar{Name: "succ"}, Arg: surface.EVar{Name: "zero"}}}
	if !reflect.DeepEqual(two, surface.Exp(want)) {
		t.Errorf("2 expanded to %#v, want %#v", two, want)
	}

	if _, err := surface.ParseExpr("2"); err == nil || !strings.Contains(err.Error(), "no `builtin nat`") {
		t.Errorf("bare numeral without binding: want parse error, got %v", err)
	}
	if _, err := surface.ParseExprNat("succ 41", "zero", "succ"); err != nil {
		t.Errorf("ParseExprNat: %v", err)
	}
	if _, err := surface.ParseExprNat("99999999999", "zero", "succ"); err == nil {
		t.Errorf("oversized numeral should fail")
	}
}
