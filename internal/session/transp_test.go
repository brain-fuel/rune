package session

import "testing"

// transp regularity (§F phase 3c): transport along a CONSTANT type-line is the
// identity — the freshness machinery detects the constancy. Each fact checks
// only if transp reduces.
const transpFacts = `
constLine : {A : UF} -> (a : El A) -> Eq (El A) (transp (fn (i : I) is A end) a) a is
  fn {A : UF} (a : El A) is refl a end
end
constFib : {X : U} -> (x : X) -> Eq X (transp (fn (i : I) is fib X end) x) x is
  fn {X : U} (x : X) is refl x end
end
constPath : {A : UF} -> {x : El A} -> {y : El A} -> (p : El (pathF A x y)) ->
    Eq (El (pathF A x y)) (transp (fn (i : I) is pathF A x y end) p) p is
  fn {A : UF} {x : El A} {y : El A} (p : El (pathF A x y)) is refl p end
end
`

func TestTranspRegularity(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(transpFacts); err != nil {
		t.Fatalf("transp regularity facts should check (transp must reduce on constant lines): %v", err)
	}
}
