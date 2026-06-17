package session

import "testing"

// R-BOX / A2 increment: hcomp with an EMPTY system (φ = ⊥) is the floor — no
// walls to fill. hcomp A fbot u u0 ~> u0, for any A.
const hcompBotFacts = `
hcompBot : {A : UF} -> (u : I -> holds fbot -> El A) -> (u0 : El A) ->
    Eq (El A) (hcomp A fbot u u0) u0 is
  fn {A : UF} (u : I -> holds fbot -> El A) (u0 : El A) is refl u0 end
end
`

func TestHcompEmptySystem(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(hcompBotFacts); err != nil {
		t.Fatalf("hcomp on an empty system must reduce to the floor: %v", err)
	}
}
