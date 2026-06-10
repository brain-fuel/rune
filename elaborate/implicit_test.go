package elaborate_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/internal/session"
	"goforge.dev/rune/surface"
)

const implicitPrelude = `
id : {A : U} -> A -> A is
  fn {A : U} (x : A) is x end
end
`

func TestImplicitInsertion(t *testing.T) {
	cases := []string{
		// The implicit type argument is inserted and solved from the explicit one.
		implicitPrelude + `idU : U -> U is id end
two : U is id U end`,
		// Explicit override of an implicit argument.
		implicitPrelude + `viaOverride : U -> U is id {U} end`,
		// const with two implicits, both inferred.
		implicitPrelude + `
const : {A : U} -> {B : U} -> A -> B -> A is
  fn {A : U} {B : U} (x : A) (y : B) is x end
end
k : U is const U (U -> U) end`,
		// Implicit lambda inserted when checking a plain term against {A} -> ...
		implicitPrelude + `idAgain : {A : U} -> A -> A is fn (x : _) is x end end`,
		// Holes solved by unification with the expected type.
		implicitPrelude + `viaHole : U -> U is (id : _ -> _) end`,
		// Composition with implicits, applied.
		`
comp : {A : U} -> {B : U} -> {C : U} -> (B -> C) -> (A -> B) -> A -> C is
  fn {A : U} {B : U} {C : U} (g : B -> C) (f : A -> B) (x : A) is g (f x) end
end
idU : U -> U is fn (x : U) is x end end
twice : U -> U is comp idU idU end
`,
	}
	for i, src := range cases {
		s := session.New()
		if _, err := s.LoadSource(src); err != nil {
			t.Errorf("case %d: %v", i, err)
		}
	}
}

func TestImplicitErrors(t *testing.T) {
	cases := []struct{ src, want string }{
		// Underconstrained: nothing determines the implicit.
		{implicitPrelude + `bad : U is (fn (x : U) is x end) (id _) end`, "unsolved"},
		// Implicit lambda against an explicit Pi.
		{`bad : U -> U is fn {x : U} is x end end`, "implicit lambda"},
		// Too many implicit arguments: id {U} {U}.
		{implicitPrelude + `bad : U -> U is id {U} {U} end`, "expecting an explicit"},
		// Occurs check: h : ?a -> ?b applied to itself needs ?a = ?a -> ?b.
		{`bad : _ is fn (h : _ -> _) is h h end end`, "occurs"},
		// Scope escape: the domain hole is created outside x's scope, so a
		// solution mentioning x is rejected (no pruning in v1).
		{`bad : _ is fn (x : _) is x x end end`, "escapes"},
	}
	for i, c := range cases {
		s := session.New()
		_, err := s.LoadSource(c.src)
		if err == nil {
			t.Errorf("case %d: expected error containing %q, got none", i, c.want)
			continue
		}
		if !strings.Contains(err.Error(), c.want) {
			t.Errorf("case %d: error %q does not contain %q", i, err, c.want)
		}
	}
}

// TestImplicitElabAndNormalize: REPL-style expression elaboration inserts
// implicits, zonks, and normalizes.
func TestImplicitElabAndNormalize(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(implicitPrelude); err != nil {
		t.Fatal(err)
	}
	e, err := surface.ParseExpr(`id U`)
	if err != nil {
		t.Fatal(err)
	}
	tm, ty, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	if got := surface.PrettyWith(ty, s.RefNames()); got != "U" {
		t.Fatalf("type = %q, want U", got)
	}
	if got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames()); got != "U" {
		t.Fatalf("normal form = %q, want U", got)
	}
	// The elaborated term shows the inserted implicit: id {U} U.
	if got := surface.PrettyWith(tm, s.RefNames()); got != "id {U} U" {
		t.Fatalf("elaborated = %q, want id {U} U", got)
	}
}

// TestHoleInType: a hole in a definition's type is solved from the body.
func TestHoleInType(t *testing.T) {
	s := session.New()
	src := implicitPrelude + `
f : U -> _ is fn (x : U) is x end end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
	d := s.Defs()[1]
	if got := surface.PrettyWith(d.Ty, s.RefNames()); got != "U -> U" {
		t.Fatalf("solved type = %q, want U -> U", got)
	}
}
