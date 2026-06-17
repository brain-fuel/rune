package session

import (
	"strings"
	"testing"
)

// injPrelude is the integers Z as the difference quotient of pairs of naturals
// (the ch12 tower rung, reduced to the injection), with `builtin int Z intOf`
// registering the typed numeral injection (numeric-tower rung C4).
const injPrelude = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
+ : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is case m of | zero -> n | succ k with ih -> succ ih end end
end
builtin natAdd +
data NatPair : U is npair : Nat -> Nat -> NatPair end
fst : NatPair -> Nat is fn (p : NatPair) is NatPairElim (fn (x : NatPair) is Nat end) (fn (a : Nat) (b : Nat) is a end) p end end
snd : NatPair -> Nat is fn (p : NatPair) is NatPairElim (fn (x : NatPair) is Nat end) (fn (a : Nat) (b : Nat) is b end) p end end
ZRel : NatPair -> NatPair -> Prop is fn (p : NatPair) (q : NatPair) is Eq Nat (fst p + snd q) (fst q + snd p) end end
Z : U is Quot NatPair ZRel end
intOf : Nat -> Z is fn (n : Nat) is qin NatPair ZRel (npair n 0) end end
builtin int Z intOf
`

func injSession(t *testing.T) *Session {
	t.Helper()
	s := New()
	if _, err := s.LoadSource(injPrelude); err != nil {
		t.Fatalf("loading injection prelude: %v", err)
	}
	return s
}

// TestNumInjLowersAtCodomain: a numeral CHECKED AT Z lowers to intOf(NatLit) —
// definitionally equal to the explicit injection, so a refl proof goes through
// even though Eq is stuck at a quotient (both sides ARE intOf 7).
func TestNumInjLowersAtCodomain(t *testing.T) {
	s := injSession(t)
	if _, err := s.LoadSource(`sevenZ : Z is 7 end`); err != nil {
		t.Fatalf("(7 : Z) should elaborate via the int injection: %v", err)
	}
	if _, err := s.LoadSource(`sevenIsIntOf : Eq Z 7 (intOf 7) is refl (intOf 7) end`); err != nil {
		t.Fatalf("(7 : Z) must be defeq to intOf 7: %v", err)
	}
}

// TestNumInjCoexistsWithNat: the base `builtin nat` lowering is untouched —
// `7 : Nat` is still the bare NatLit, and Nat and Z numerals coexist, dispatched
// by the expected type (the injection only fires on a rigid non-Nat expectation).
func TestNumInjCoexistsWithNat(t *testing.T) {
	s := injSession(t)
	if got := numNorm(t, s, "(7 : Nat)"); got != "7" {
		t.Fatalf("(7 : Nat) = %q, want 7 (base nat lowering must be unchanged)", got)
	}
	if _, err := s.LoadSource(`sevenN : Nat is 7 end`); err != nil {
		t.Fatalf("(7 : Nat) should still elaborate: %v", err)
	}
}

// TestNumInjUnknownTypeErrors: a numeral checked at a type with no registered
// injection and no nat unification is a clean error, not a wrong injection.
func TestNumInjUnknownTypeErrors(t *testing.T) {
	s := injSession(t)
	if _, err := s.LoadSource(`data Color : U is red : Color end` + "\n" + `bad : Color is 7 end`); err == nil {
		t.Fatal("(7 : Color) should be rejected — no injection to Color")
	}
}

// TestBigNumInjUnifies is the regression for the unifySpine NNatLit fast path: a
// big literal injected into a quotient and proved by refl forces unification of
// two equal NatLits. Without the O(1) Cmp case in unifySpine, the elaborator
// δ-peels both literals one succ-layer at a time and overflows the stack at
// N ~ 10^12. With it, this checks promptly.
func TestBigNumInjUnifies(t *testing.T) {
	s := injSession(t)
	if _, err := s.LoadSource(`bigZok : Eq Z 1000000000000 (intOf 1000000000000) is refl (intOf 1000000000000) end`); err != nil {
		t.Fatalf("big literal at Z must unify without peeling: %v", err)
	}
}

// TestNumInjRequiresNat: an int/rat injection declared before any `builtin nat`
// is rejected (the injection's domain is the Nat).
func TestNumInjRequiresNat(t *testing.T) {
	s := New()
	src := `
data D : U is mk : D end
inj : D -> D is fn (x : D) is x end end
builtin int D inj
`
	if _, err := s.LoadSource(src); err == nil || !strings.Contains(err.Error(), "builtin nat") {
		t.Fatalf("builtin int without builtin nat should be rejected, got err=%v", err)
	}
}
