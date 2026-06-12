package session

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/surface"
)

// The quotient pipeline end to end: the builtins are ambient, the ι-rules
// compute, qsound checks, qind eliminates into Prop, and a lift with a wrong
// respect proof is rejected.

const quotSrc = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
data Bool : U is true : Bool | false : Bool end
not : Bool -> Bool is fn (b : Bool) is BoolElim (fn (x : Bool) is Bool end) false true b end end
isEven : Nat -> Bool is
  fn (n : Nat) is NatElim (fn (x : Nat) is Bool end) true (fn (k : Nat) (ih : Bool) is not ih end) n end
end
R : Nat -> Nat -> Prop is fn (m : Nat) (n : Nat) is Eq Bool (isEven m) (isEven n) end end
Par : U is Quot Nat R end
parity : Par -> Bool is
  qlift Nat R Bool isEven (fn (a : Nat) (b : Nat) (r : R a b) is r end)
end
`

func loadQuot(t *testing.T) *Session {
	t.Helper()
	s := New()
	if _, err := s.LoadSource(quotSrc); err != nil {
		t.Fatal(err)
	}
	return s
}

func normalize(t *testing.T, s *Session, src string) string {
	t.Helper()
	e, err := surface.ParseExpr(src)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("%s: %v", src, err)
	}
	return surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
}

func TestQuotBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"Quot", "qin", "qsound", "qlift", "qind"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound in a fresh session", n)
		}
	}
	// Builtins survive a reset (they are re-registered, same hashes).
	h1, _ := s.Lookup("qlift")
	s.Reset()
	h2, ok := s.Lookup("qlift")
	if !ok || h1 != h2 {
		t.Fatal("qlift hash changed across Reset")
	}
}

func TestQuotLiftComputesOnPoints(t *testing.T) {
	s := loadQuot(t)
	if got := normalize(t, s, `parity (qin Nat R (succ (succ zero)))`); got != "true" {
		t.Fatalf("ι-rule did not fire: got %s", got)
	}
	if got := normalize(t, s, `parity (qin Nat R (succ zero))`); got != "false" {
		t.Fatalf("ι-rule did not fire: got %s", got)
	}
}

func TestQuotLiftStuckOnNeutral(t *testing.T) {
	s := loadQuot(t)
	// Under a binder the scrutinee is neutral: the lift must NOT reduce.
	got := normalize(t, s, `fn (q : Par) is parity q end`)
	if !strings.Contains(got, "qlift") {
		t.Fatalf("lift over a neutral scrutinee should stay stuck, got %s", got)
	}
}

func TestQSoundAndQInd(t *testing.T) {
	s := loadQuot(t)
	defs := `
soundTwoZero : Eq Par (qin Nat R (succ (succ zero))) (qin Nat R zero) is
  qsound Nat R (succ (succ zero)) zero refl
end
allSelfEq : (q : Par) -> Eq Par q q is
  fn (q : Par) is
    qind Nat R (fn (z : Par) is Eq Par z z end) (fn (a : Nat) is refl end) q
  end
end
`
	if _, err := s.LoadSource(defs); err != nil {
		t.Fatal(err)
	}
}

func TestQuotBadRespectRejected(t *testing.T) {
	s := loadQuot(t)
	bad := `
badLift : Par -> Nat is
  qlift Nat R Nat (fn (n : Nat) is n end) (fn (a : Nat) (b : Nat) (r : R a b) is r end)
end
`
	if _, err := s.LoadSource(bad); err == nil {
		t.Fatal("a lift whose function does not respect R must be rejected")
	}
}

func TestQuotSoundFalsePremiseRejected(t *testing.T) {
	s := loadQuot(t)
	bad := `
oops : Eq Par (qin Nat R (succ zero)) (qin Nat R zero) is
  qsound Nat R (succ zero) zero refl
end
`
	if _, err := s.LoadSource(bad); err == nil {
		t.Fatal("qsound with an unprovable premise must be rejected")
	}
}
