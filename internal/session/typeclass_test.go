package session

import "testing"

const tcPrelude = `
data Bool : U is true : Bool | false : Bool end
Self : U -> U is fn (A : U) is A -> A end end
instance selfBool : Self Bool is fn (b : Bool) is b end end
applySelf : (A : U) -> {s : Self A} -> A -> A is
  fn (A : U) {s : Self A} (x : A) is s x end
end
`

// C2: the dictionary is resolved by instance search at the use site.
func TestTypeclassInstanceSearch(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(tcPrelude + `
ok : Eq Bool (applySelf Bool true) true is refl true end
`); err != nil {
		t.Fatalf("instance {s : Self Bool} must resolve to selfBool by search: %v", err)
	}
}

// A class with no registered instance does not resolve — the implicit stays an
// unsolved metavariable, an honest elaboration error (never a wrong proof).
func TestTypeclassMissingInstanceFails(t *testing.T) {
	s := New()
	src := tcPrelude + `
data Unit : U is unit : Unit end
bad : Unit is applySelf Unit unit end
`
	if _, err := s.LoadSource(src); err == nil {
		t.Fatal("applySelf at Unit has no instance; resolution must fail, not silently succeed")
	}
}

// tcParametric extends the prelude with List + the PARAMETRIC instance
// `{A} -> {Self A} -> Self (List A)` (C2b).
const tcParametric = tcPrelude + `
data List : U -> U is nil : (A : U) -> List A | cons : (A : U) -> A -> List A -> List A end
map : (A : U) -> (A -> A) -> List A -> List A is
  fn (A : U) (f : A -> A) (xs : List A) is
    ListElim A (fn (z : List A) is List A end) (nil A)
      (fn (x : A) (rest : List A) (ih : List A) is cons A (f x) ih end) xs
  end
end
instance selfList : {A : U} -> {s : Self A} -> Self (List A) is
  fn {A : U} {s : Self A} is fn (xs : List A) is map A s xs end end
end
`

// C2b: a parametric instance resolves by RECURSIVE search — `Self (List Bool)`
// unifies the codomain to fix A := Bool, then discharges the premise `Self Bool`
// by a nested search finding selfBool.
func TestParametricInstanceSearch(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(tcParametric + `
one : List Bool is cons Bool true (nil Bool) end
ok : Eq (List Bool) (applySelf (List Bool) one) one is refl one end
`); err != nil {
		t.Fatalf("Self (List Bool) must resolve to selfList {Bool} {selfBool}: %v", err)
	}
}

// C2b recurses to arbitrary depth: `Self (List (List Bool))` resolves to
// `selfList {List Bool} {selfList {Bool} {selfBool}}`.
func TestParametricInstanceNested(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(tcParametric + `
one : List Bool is cons Bool true (nil Bool) end
two : List (List Bool) is cons (List Bool) one (nil (List Bool)) end
ok : Eq (List (List Bool)) (applySelf (List (List Bool)) two) two is refl two end
`); err != nil {
		t.Fatalf("nested Self (List (List Bool)) must resolve two layers deep: %v", err)
	}
}

// C2b is honest: if a premise has no instance, the whole resolution fails (it does
// not silently leave a hole). Here `Self (List Unit)` needs `Self Unit`, which is
// unregistered.
func TestParametricInstancePremiseFails(t *testing.T) {
	s := New()
	src := tcParametric + `
data Unit : U is unit : Unit end
bad : List Unit is applySelf (List Unit) (cons Unit unit (nil Unit)) end
`
	if _, err := s.LoadSource(src); err == nil {
		t.Fatal("Self (List Unit) needs Self Unit, which is unregistered; resolution must fail")
	}
}
