package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/prelude"
)

// A minimal, self-contained program with a slow recursive `sleb` and a fast
// monus-based twin `sleW`, mirroring the prelude's leb/leW/lebEquiv shape at
// listing scale. Not reused from nataccel_test.go's natAccelSrc verbatim
// (that fixture has no Bool and recurses over `Nat`, not `Whole`) but built
// from the same pieces: `data Whole` + `builtin nat` (as in natAccelSrc),
// plus `data Bool` (as in nataccel_test.go's own accelDataShadowSrc /
// quot_test.go), plus `pred`/`monus`/`isZero` mirrored from
// internal/prelude/prelude.rune (pred/subW/isZero).
const lowerPreamble = `
data Whole : U is
  zero : Whole
| succ : Whole -> Whole
end
builtin nat Whole zero succ
pred : Whole -> Whole is
  fn (n : Whole) is case n of | zero -> zero | succ k -> k end end
end
monus : Whole -> Whole -> Whole is
  fn (m : Whole) (n : Whole) is
    case n of
    | zero -> m
    | succ k with ih -> pred ih
    end
  end
end
data Bool : U is
  true : Bool
| false : Bool
end
isZero : Whole -> Bool is
  fn (n : Whole) is case n of | zero -> true | succ k -> false end end
end
`

const lowerProg = lowerPreamble + `
sleb : Whole -> Whole -> Bool is
  fn (m : Whole) is
    case m of
    | zero -> fn (n : Whole) is true end
    | succ k with ih -> fn (n : Whole) is
        case n of | zero -> false | succ j -> ih j end end
    end end end
sleW : Whole -> Whole -> Bool is fn (a : Whole)(b : Whole) is isZero (monus a b) end end
`

// slebEquiv would prove (a b) -> Eq Bool (sleb a b)(sleW a b) by induction (as
// the prelude's lebEquiv does for leb/leW). The gate checks the TYPE, so a
// `foreign` axiom of the right type exercises the acceptance path, and a
// `foreign` axiom of a WRONG type exercises rejection, without needing the
// full induction proof in this fixture.
func TestAddLoweringAcceptsWellTypedProof(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(lowerProg +
		"foreign slebEquiv : (a : Whole) -> (b : Whole) -> Eq Bool (sleb a b) (sleW a b) end\n" +
		"lower sleb to sleW by slebEquiv\n"); err != nil {
		t.Fatalf("expected acceptance, got %v", err)
	}
	if s.Lowering() == nil || len(s.Lowering()) != 1 {
		t.Fatalf("registry not populated: %v", s.Lowering())
	}
}

func TestAddLoweringRejectsWrongProof(t *testing.T) {
	cases := map[string]string{
		// codomain is not an Eq between sleb and sleW (constant refl-shaped
		// claim about sleW vs itself).
		"not-an-eq-of-the-pair": "foreign bad : (a : Whole) -> (b : Whole) -> Eq Bool (sleW a b) (sleW a b) end\nlower sleb to sleW by bad\n",
		// wrong arity (one binder short).
		"arity-mismatch": "foreign bad : (a : Whole) -> Eq Bool (sleb a a) (sleW a a) end\nlower sleb to sleW by bad\n",
		// codomain not an Eq at all.
		"not-eq-former": "foreign bad : (a : Whole) -> (b : Whole) -> Bool end\nlower sleb to sleW by bad\n",
	}
	for name, tail := range cases {
		t.Run(name, func(t *testing.T) {
			s := New()
			_, err := s.LoadSource(lowerProg + tail)
			if err == nil {
				t.Fatalf("expected rejection for %s, got acceptance", name)
			}
			if !strings.Contains(err.Error(), "lower") {
				t.Fatalf("error should mention the lower directive, got %v", err)
			}
		})
	}
}

// The shipped prelude registers leb -> leW (v4 Ord Plan C, Task 4). Loaded
// the same way tower_hash_test.go loads it: prelude.Source() fed through
// LoadSource, the same pipeline the REPL and CLI use (prelude.go has no
// special status, it is ordinary surface rune).
func TestPreludeLowersLeb(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("prelude load: %v", err)
	}
	if len(s.Lowering()) == 0 {
		t.Fatalf("prelude did not register any lowering")
	}
}
