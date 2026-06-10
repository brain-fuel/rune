package elaborate_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/internal/session"
	"goforge.dev/rune/surface"
)

func TestEqualityStratumPrograms(t *testing.T) {
	cases := []string{
		// Eq formation, refl at a base type.
		`p : Eq U1 U U is refl U end`,
		// Eq lives in Prop; Prop : U.
		`P : U1 is Prop end
T : Prop is Eq U1 U U end
p : T is refl U end`,
		// FUNEXT COMPUTES: an equality of functions IS the pointwise equality
		// function type, so its proof is a lambda of refls.
		`idU : U -> U is fn (x : U) is x end end
idU' : U -> U is fn (x : U) is let y = x in y end end
ext : Eq (U -> U) idU idU' is
  fn (x : U) is refl end
end`,
		// refl x at a function type eta-expands automatically.
		`idU : U -> U is fn (x : U) is x end end
ext : Eq (U -> U) idU idU is refl idU end`,
		// cast along refl is the identity (convertible endpoints reduce).
		`c : U -> U is fn (x : U) is cast U U (refl U) x end end`,
		// A dependent statement: symmetry of a *definitionally* true equality.
		`sym : {A : U} -> {x : A} -> {y : A} -> Eq A x y -> Prop is
  fn {A : U} {x : A} {y : A} (p : Eq A x y) is Eq A y x end
end`,
		// Proofs as implicit-heavy lemmas: transporting between equal types.
		`transport : (A : U1) -> (B : U1) -> Eq U1 A B -> A -> B is
  fn (A : U1) (B : U1) (p : Eq U1 A B) (x : A) is cast A B p x end
end
use : U -> U is fn (x : U) is transport U U (refl U) x end end`,
		// UIP at canonical proofs: two refls of the same equation are equal.
		`uipish : (p : Eq U1 U U) -> Prop is
  fn (p : Eq U1 U U) is Eq (Eq U1 U U) (refl U) (refl U) end
end
uipproof : Eq (Eq U1 U U) (refl U) (refl U) is refl end`,
	}
	for i, src := range cases {
		s := session.New()
		if _, err := s.LoadSource(src); err != nil {
			t.Errorf("case %d: %v", i, err)
		}
	}
}

func TestEqualityErrors(t *testing.T) {
	cases := []struct{ src, want string }{
		// refl cannot prove a false (non-convertible) equation.
		{`bad : Eq U1 U (U -> U) is refl end`, "not definitionally equal"},
		// Eq needs its type argument to be a type.
		{`f : U -> U is fn (x : U) is x end end
bad : Prop is Eq f f f end`, "not a type"},
		// Under-applied former.
		{`bad : Prop is Eq U1 U end`, "3 arguments"},
		// Prop is a universe, not a proposition: refl U : Eq U U U, not Prop itself.
		{`bad : U -> U is refl end`, "not an equality type"},
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

// TestCastComputesThroughFunctions: cast at a function type reduces to a
// wrapper lambda; applying it computes through to the subject.
func TestCastComputesThroughFunctions(t *testing.T) {
	s := session.New()
	src := `
idU : U -> U is fn (x : U) is x end end
moved : U -> U is cast (U -> U) (U -> U) (refl (U -> U)) idU end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
	e, err := surface.ParseExpr(`moved`)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	// cast between convertible endpoints reduces away entirely.
	if got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames()); got != "fn (x : U) is x end" {
		t.Fatalf("moved normalized to %q, want the identity", got)
	}
}

// TestProofIrrelevanceAtCasts: two casts differing only in their proofs are
// definitionally equal (conversion skips the proof).
func TestProofIrrelevanceAtCasts(t *testing.T) {
	s := session.New()
	src := `
p1 : Eq U1 U U is refl U end
p2 : Eq U1 U U is refl U end
eqcasts : (A : U) -> Prop is
  fn (A : U) is Eq U1 (cast U U p1 A) (cast U U p2 A) end
end
proof : (A : U) -> Eq U1 (cast U U p1 A) (cast U U p2 A) is
  fn (A : U) is refl end
end
`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
}
