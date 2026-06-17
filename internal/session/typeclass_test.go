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
