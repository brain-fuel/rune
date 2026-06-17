package session

import (
	"strings"
	"testing"
)

// The cubical path operations (§F phase 2): each fact below type-checks ONLY if
// the corresponding papp ι-rule fires, so loading the source is the test.
const pathFacts = `
-- boundary: a path at i0/i1 is its endpoint — for ANY path, even a variable.
bnd0 : {A : UF} -> {x : El A} -> {y : El A} -> (p : El (pathF A x y)) -> Eq (El A) (papp A x y p i0) x is
  fn {A : UF} {x : El A} {y : El A} (p : El (pathF A x y)) is refl x end
end
bnd1 : {A : UF} -> {x : El A} -> {y : El A} -> (p : El (pathF A x y)) -> Eq (El A) (papp A x y p i1) y is
  fn {A : UF} {x : El A} {y : El A} (p : El (pathF A x y)) is refl y end
end
-- reflexivity is the constant path.
reflConst : {A : UF} -> {x : El A} -> (i : I) -> Eq (El A) (papp A x x (preflF A x) i) x is
  fn {A : UF} {x : El A} (i : I) is refl x end
end
-- β: applying an abstraction is the function.
betaPath : {A : UF} -> (f : I -> El A) -> (i : I) -> Eq (El A) (papp A (f i0) (f i1) (pabs A f) i) (f i) is
  fn {A : UF} (f : I -> El A) (i : I) is refl (f i) end
end
-- the symmetric path, by reversing the interval: sym p i = p (ineg i).
symPath : {A : UF} -> {x : El A} -> {y : El A} -> El (pathF A x y) -> El (pathF A y x) is
  fn {A : UF} {x : El A} {y : El A} (p : El (pathF A x y)) is
    pabs A (fn (i : I) is papp A x y p (ineg i) end)
  end
end
`

func TestPathReductions(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(pathFacts); err != nil {
		t.Fatalf("path facts should elaborate and check (the ι-rules must fire): %v", err)
	}
}

// TestPathComputeAwayDeploys: a path application that COMPUTES AWAY to an outer
// value deploys (B5 / R-ERASE2): papp A x x (preflF A x) i ~> x (refl is the
// constant path), so `pt` normalizes to the constant function λ…(i). x and has a
// genuine erased meaning. A bare path VALUE (a pabs intro) still does not deploy.
func TestPathComputeAwayDeploys(t *testing.T) {
	s := New()
	src := `pt : {A : UF} -> {x : El A} -> (i : I) -> El A is
  fn {A : UF} {x : El A} (i : I) is papp A x x (preflF A x) i end
end
genuinePath : {A : UF} -> {x : El A} -> El (pathF A x x) is
  fn {A : UF} {x : El A} is pabs A (fn (i : I) is x end) end
end`
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("path definitions should check: %v", err)
	}
	// pt's inner papp reduces off — it deploys.
	if _, err := s.EmitProgram("pt"); err != nil {
		t.Fatalf("a path that computes away to a constant must deploy (B5): %v", err)
	}
	// a bare pabs intro is a genuine inner path value — refused.
	if _, err := s.EmitProgram("genuinePath"); err == nil || !strings.Contains(err.Error(), "inner layer") {
		t.Fatalf("emitting a bare path value must be refused, got err=%v", err)
	}
}
