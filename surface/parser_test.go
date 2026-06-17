package surface_test

import (
	"errors"
	"reflect"
	"strings"
	"testing"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/surface"
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

// TestDebugCoreNamed pins the :ast renderer: the same structural tree as :core but
// with NAMED binders (not unnamed Π/λ + bare de Bruijn indices). References would
// print as their definition names; with no defs in scope the binder naming is what
// this checks.
func TestDebugCoreNamed(t *testing.T) {
	cases := []struct {
		src, want string
	}{
		{"(A : U) -> A", "(Π (A : U). A)"},                         // dependent binder, named
		{"(A : U) -> (B : A) -> A", "(Π (A : U). (Π (B : A). A))"}, // nested, both named
		{"(U : U)", "(U : U)"},                                     // ascription
		{"U -> U", "(Π (x : U). U)"},                               // non-dependent: unused binder freshens to x
	}
	for _, c := range cases {
		got := surface.DebugCoreNamed(resolveSrc(t, c.src), nil)
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
	programs := []string{
		"data Nat : U is",                                // before any constructor
		"data Nat : U is zero : Nat",                     // after a constructor, missing 'end'
		"data Nat : U is zero : Nat | succ : Nat -> Nat", // mid constructor list
	}
	for _, src := range programs {
		_, err := surface.ParseProgram(src)
		if !errors.Is(err, surface.ErrIncomplete) {
			t.Errorf("ParseProgram(%q): want ErrIncomplete, got %v", src, err)
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
		{"a // b % c", bin("%", bin("//", v("a"), v("b")), v("c"))},
		{"a + b // c", bin("+", v("a"), bin("//", v("b"), v("c")))},
		{"(//) x y", surface.EApp{Fn: surface.EApp{Fn: v("//"), Arg: v("x")}, Arg: v("y")}},
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

// TestNumerals pins §5.5: a literal parses to an unexpanded ENum (its meaning is
// fixed downstream by the `builtin nat` expected type), so no binding is needed
// at parse time; quantities still parse.
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
	var sawNat bool
	for _, it := range items {
		switch d := it.(type) {
		case surface.BuiltinNat:
			sawNat = true
			if d.TyName != "Nat" || d.Zero != "zero" || d.Succ != "succ" {
				t.Fatalf("builtin nat parsed as %+v", d)
			}
		case surface.Def:
			if d.Name == "two" {
				two = d.Body
			}
		}
	}
	if !sawNat {
		t.Fatalf("builtin nat item missing")
	}
	// `builtin bin` is retired (C7 / R-NUM, Decision 5): the kind is unknown and
	// no longer parses.
	if _, err := surface.ParseProgram("builtin bin BN bn0 bnP Pos pH pO pI"); err == nil {
		t.Errorf("`builtin bin` should no longer parse (the kind is retired)")
	}
	if got, ok := two.(surface.ENum); !ok || got.Val != 2 {
		t.Errorf("2 parsed to %#v, want surface.ENum{Val: 2}", two)
	}
	// A bare numeral parses with no binding at all — lowering, not parsing, is
	// where a missing binding is reported.
	if e, err := surface.ParseExpr("2"); err != nil {
		t.Errorf("bare numeral should parse: %v", err)
	} else if n, ok := e.(surface.ENum); !ok || n.Val != 2 {
		t.Errorf("bare 2 parsed to %#v, want ENum{Val: 2}", e)
	}
	if _, err := surface.ParseExpr("99999999999999999999999999"); err == nil {
		t.Errorf("numeral exceeding int range should fail to parse")
	}
}

// TestCaseParsing pins §5.6: leading-pipe clauses, flat patterns, optional
// `with`, '_' binders, and nesting inside seq blocks.
func TestCaseParsing(t *testing.T) {
	e, err := surface.ParseExpr("case m of | zero -> n | succ k with ih -> succ ih end")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	cs, ok := e.(surface.ECase)
	if !ok {
		t.Fatalf("got %#v, want ECase", e)
	}
	if !reflect.DeepEqual(cs.Scrut, surface.Exp(surface.EVar{Name: "m"})) || len(cs.Clauses) != 2 {
		t.Fatalf("unexpected case shape %#v", cs)
	}
	c1 := cs.Clauses[1]
	if c1.Ctor != "succ" || !reflect.DeepEqual(c1.Binders, []string{"k"}) || !reflect.DeepEqual(c1.IHs, []string{"ih"}) {
		t.Fatalf("unexpected succ clause %#v", c1)
	}

	if _, err := surface.ParseExpr("case p of | npair _ b -> b end"); err != nil {
		t.Errorf("'_' pattern binder: %v", err)
	}
	if _, err := surface.ParseExpr("seq let x = case m of | zero -> n end; x end"); err != nil {
		t.Errorf("case inside seq: %v", err)
	}
	if _, err := surface.ParseExpr("case m of | zero -> n"); !errors.Is(err, surface.ErrIncomplete) {
		t.Errorf("unterminated case: want ErrIncomplete, got %v", err)
	}
	if _, err := surface.ParseExpr("case m of | succ k with -> k end"); err == nil {
		t.Errorf("empty 'with' should be a parse error")
	}
}

// TestCalcParsing pins §5.7: a calc desugars to an ascribed first step chained
// through subst, the motive binder is freshened, and malformed ladders error.
func TestCalcParsing(t *testing.T) {
	e, err := surface.ParseExpr("calc a = b by p = c by q end")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	app, ok := e.(surface.EApp)
	if !ok {
		t.Fatalf("got %#v, want a subst application", e)
	}
	head, args := surface.SpineOf(app)
	if _, isSubst := head.(surface.ESubst); !isSubst || len(args) != 6 {
		t.Fatalf("head %#v with %d args, want subst with 6", head, len(args))
	}
	if _, isAnn := args[5].(surface.EAnn); !isAnn {
		t.Fatalf("the accumulator should be the ascribed first step, got %#v", args[5])
	}

	// One-step calc is just the ascribed proof.
	e1, err := surface.ParseExpr("calc a = b by p end")
	if err != nil {
		t.Fatalf("parse one-step: %v", err)
	}
	if _, isAnn := e1.(surface.EAnn); !isAnn {
		t.Fatalf("one-step calc should be an ascription, got %#v", e1)
	}

	// The motive binder freshens away from identifiers in the first expression.
	e2, err := surface.ParseExpr("calc w = b by p = c by q end")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	app2 := e2.(surface.EApp)
	_, args2 := surface.SpineOf(app2)
	lam := args2[4].(surface.ELam)
	if lam.Param == "w" {
		t.Fatalf("motive binder captured the step variable w")
	}

	if _, err := surface.ParseExpr("calc a end"); err == nil {
		t.Errorf("a calc needs at least one step")
	}
	if _, err := surface.ParseExpr("calc a = b by p"); !errors.Is(err, surface.ErrIncomplete) {
		t.Errorf("unterminated calc: want ErrIncomplete, got %v", err)
	}
}
