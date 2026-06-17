package session

import "testing"

// hcomp total-system rule (§F phase 3d) and comp's two honest endpoints (§F
// phase 3e). When the face is ⊤ the partial path is total, so the lid is the
// system at i1: `hcomp A ⊤ u u0 ~> u i1 htop` (and likewise for comp). When the
// face is ⊥ on a constant type-line, comp collapses to the identity transport:
// `comp (fn i is A) ⊥ u u0 ~> u0`. Each fact checks only if the rule reduces.
const kanFacts = `
hcompTop : {A : UF} -> (u : I -> holds ftop -> El A) -> (u0 : El A) ->
    Eq (El A) (hcomp A ftop u u0) (u i1 htop) is
  fn {A : UF} (u : I -> holds ftop -> El A) (u0 : El A) is refl (u i1 htop) end
end
compTop : {A : I -> UF} -> (u : (i : I) -> holds ftop -> El (A i)) -> (u0 : El (A i0)) ->
    Eq (El (A i1)) (comp A ftop u u0) (u i1 htop) is
  fn {A : I -> UF} (u : (i : I) -> holds ftop -> El (A i)) (u0 : El (A i0)) is refl (u i1 htop) end
end
compBot : {A : UF} -> (u : (i : I) -> holds fbot -> El A) -> (u0 : El A) ->
    Eq (El A) (comp (fn (i : I) is A end) fbot u u0) u0 is
  fn {A : UF} (u : (i : I) -> holds fbot -> El A) (u0 : El A) is refl u0 end
end
`

func TestKanEndpoints(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(kanFacts); err != nil {
		t.Fatalf("hcomp/comp endpoint facts should check (the Kan rules must reduce): %v", err)
	}
}
