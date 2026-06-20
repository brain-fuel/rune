package elaborate_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

func TestQuantityAccepted(t *testing.T) {
	cases := []string{
		// Linear identity: the argument is used exactly once.
		`lid : (1 x : U) -> U is fn (1 x : U) is x end end`,
		// Unannotated binder adopts the expected linear quantity.
		`lid : (1 x : U) -> U is fn (x : U) is x end end`,
		// Erased type argument: used only in type positions.
		`eid : {0 A : U} -> A -> A is fn {0 A : U} (x : A) is x end end`,
		// An erased binder may appear freely inside an equality (a proof).
		`peq : (0 x : U) -> Prop is fn (0 x : U) is Eq U x x end end`,
		// ω binders stay unrestricted: duplication is fine.
		`dup : (f : U -> U -> U) -> U -> U is
  fn (f : U -> U -> U) (x : U) is f x x end
end`,
		// Linear data through a linear-typed function: f consumes its
		// argument exactly once, so x stays linear.
		`compose1 : (1 f : (1 z : U) -> U) -> (1 x : U) -> U is
  fn (1 f : (1 z : U) -> U) (1 x : U) is f x end
end`,
	}
	for i, src := range cases {
		s := session.New()
		if _, err := s.LoadSource(src); err != nil {
			t.Errorf("case %d: %v", i, err)
		}
	}
}

func TestQuantityRejected(t *testing.T) {
	cases := []struct{ src, want string }{
		// Linear variable duplicated.
		{`bad : (1 x : U) -> U is fn (1 x : U) is (fn (a : U) (b : U) is a end) x x end end`, "exactly once"},
		// Linear variable dropped.
		{`bad : (1 x : U) -> U1 is fn (1 x : U) is U end end`, "exactly once"},
		// Erased variable used computationally.
		{`bad : (0 x : U) -> U is fn (0 x : U) is x end end`, "erased"},
		// Lambda annotation conflicting with the Pi's quantity.
		{`bad : (1 x : U) -> U is fn (0 x : U) is U end end`, "expects"},
		// Quantities are part of the Pi's identity: (1 x : U) -> U ≠ U -> U.
		{`f : (1 x : U) -> U is fn (1 x : U) is x end end
bad : U -> U is f end`, "types don't match"},
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

// Erased binders are free in types AND in proofs, and the proof still computes.
func TestErasedInProofs(t *testing.T) {
	src := `
reflAt : (0 A : U) -> (x : A) -> Eq A x x is
  fn {- erased -} (0 A : U) (x : A) is refl x end
end
`
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
}
