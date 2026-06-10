package elaborate_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/internal/session"
	"goforge.dev/rune/surface"
)

const natDecl = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
`

const natOps = natDecl + `
add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end
`

// evalTo loads src, elaborates expr, and asserts its normal form prints as want.
func evalTo(t *testing.T, src, expr, want string) {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("load: %v", err)
	}
	e, err := surface.ParseExpr(expr)
	if err != nil {
		t.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elaborate %s: %v", expr, err)
	}
	got := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if got != want {
		t.Fatalf("%s normalized to %q, want %q", expr, got, want)
	}
}

func TestNatIotaComputation(t *testing.T) {
	evalTo(t, natOps, `add (succ (succ zero)) (succ zero)`, "succ (succ (succ zero))")
	evalTo(t, natOps, `add zero zero`, "zero")
}

func TestBoolAndCase(t *testing.T) {
	src := `
data Bool : U is
  true : Bool
| false : Bool
end
not : Bool -> Bool is
  fn (b : Bool) is
    BoolElim (fn (x : Bool) is Bool end) false true b
  end
end
`
	evalTo(t, src, `not true`, "false")
	evalTo(t, src, `not (not (not false))`, "true")
}

func TestListWithParameter(t *testing.T) {
	src := `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
`
	evalTo(t, src, `length Nat (cons Nat zero (cons Nat zero (nil Nat)))`, "succ (succ zero)")
}

// Induction: a real PROOF by NatElim with a Prop-valued motive. add zero n = n
// holds definitionally; add n zero = n needs induction, and the successor case
// needs subst (Leibniz transport along the induction hypothesis).
func TestInductionProof(t *testing.T) {
	src := natOps + `
addZeroRight : (n : Nat) -> Eq Nat (add n zero) n is
  fn (n : Nat) is
    NatElim (fn (k : Nat) is Eq Nat (add k zero) k end)
      refl
      (fn (k : Nat) (ih : Eq Nat (add k zero) k) is
        subst Nat (add k zero) k ih
          (fn (z : Nat) is Eq Nat (succ (add k zero)) (succ z) end)
          refl
      end)
      n
  end
end
`
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
	// And the proof COMPUTES at canonical numbers: applying it to 2 reduces
	// through ι and subst all the way to a canonical refl proof.
	e, err := surface.ParseExpr(`addZeroRight (succ (succ zero))`)
	if err != nil {
		t.Fatal(err)
	}
	tm, ty, err := s.ElabExpr(e)
	if err != nil {
		t.Fatal(err)
	}
	if got := surface.PrettyWith(ty, s.RefNames()); !strings.Contains(got, "Eq Nat") {
		t.Fatalf("proof type = %q", got)
	}
	nf := surface.PrettyWith(s.NormalizeExpr(tm), s.RefNames())
	if !strings.HasPrefix(nf, "refl") {
		t.Fatalf("proof at a canonical number did not compute to refl: %q", nf)
	}
}

// Symmetry and transitivity of Eq, derived with subst — the standard Leibniz
// arguments, as a book listing would write them.
func TestSymmetryTransitivity(t *testing.T) {
	src := `
sym : (A : U) -> (x : A) -> (y : A) -> Eq A x y -> Eq A y x is
  fn (A : U) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq A z x end) refl
  end
end
trans : (A : U) -> (x : A) -> (y : A) -> (z : A) -> Eq A x y -> Eq A y z -> Eq A x z is
  fn (A : U) (x : A) (y : A) (z : A) (p : Eq A x y) (q : Eq A y z) is
    subst A y z q (fn (w : A) is Eq A x w end) p
  end
end
`
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
}

// A Prop-valued motive is accepted thanks to Prop <: U.
func TestPropMotive(t *testing.T) {
	src := natDecl + `
alwaysTrue : (n : Nat) -> Prop is
  fn (n : Nat) is
    NatElim (fn (k : Nat) is Prop end)
      (Eq Nat zero zero)
      (fn (k : Nat) (ih : Prop) is ih end)
      n
  end
end
proof : alwaysTrue (succ (succ zero)) is refl end
`
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatal(err)
	}
}

func TestDataErrors(t *testing.T) {
	cases := []struct{ src, want string }{
		// Strict positivity violation: D in a function argument's domain.
		{`data Bad : U is mk : (Bad -> Bad) -> Bad end`, "positivity"},
		// Constructor returning the wrong type.
		{`data D : U is mk : U end`, "must return"},
		// Constructor not repeating the parameter telescope.
		{`data P : U -> U is mk : P U end`, "parameter"},
		// Former type not ending in U.
		{`data W : U -> Prop is mk : W U end`, "must end in U"},
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

// Constructor injectivity/disjointness via conversion: succ-headed and zero
// never convert, so refl cannot prove them equal.
func TestCtorDisjointness(t *testing.T) {
	s := session.New()
	src := natDecl + `bad : Eq Nat zero (succ zero) is refl end`
	_, err := s.LoadSource(src)
	if err == nil || !strings.Contains(err.Error(), "not definitionally equal") {
		t.Fatalf("zero = succ zero accepted by refl (err=%v)", err)
	}
}

// Same declaration twice: same content hashes (datatype identity is content).
func TestDataContentAddressed(t *testing.T) {
	s := session.New()
	if _, err := s.LoadSource(natDecl); err != nil {
		t.Fatal(err)
	}
	h1, _ := s.Lookup("Nat")
	if _, err := s.LoadSource(natDecl); err != nil {
		t.Fatal(err)
	}
	h2, _ := s.Lookup("Nat")
	if h1 != h2 {
		t.Fatal("identical data declarations got different hashes")
	}
}
