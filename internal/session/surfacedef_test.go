package session

import (
	"testing"

	"goforge.dev/rune/v3/surface"
)

// TestSurfaceDefRetention: LoadSource retains the QUALIFIED surface def, so a
// body reference rewritten by import resolution is visible to the explainer.
func TestSurfaceDefRetention(t *testing.T) {
	s := New()
	src := "module M is\n  t : U1 is U end\nend\nimport M\nu : U1 is t end\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	d, ok := s.SurfaceDef("u")
	if !ok {
		t.Fatal("SurfaceDef(u): not retained")
	}
	v, ok := d.Body.(surface.EVar)
	if !ok {
		t.Fatalf("u body = %T, want surface.EVar", d.Body)
	}
	if v.Name != "M.t" {
		t.Errorf("u body name = %q, want import-qualified \"M.t\"", v.Name)
	}
	if _, ok := s.SurfaceDef("M.t"); !ok {
		t.Error("SurfaceDef(M.t): module-qualified def not retained")
	}
	if _, ok := s.SurfaceDef("nosuch"); ok {
		t.Error("SurfaceDef(nosuch): want ok=false")
	}
}

// TestSurfaceDefResetClears: Reset drops retained surface defs with the rest
// of the session.
func TestSurfaceDefResetClears(t *testing.T) {
	s := New()
	if _, err := s.LoadSource("w : U1 is U end\n"); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	if _, ok := s.SurfaceDef("w"); !ok {
		t.Fatal("SurfaceDef(w): not retained before Reset")
	}
	s.Reset()
	if _, ok := s.SurfaceDef("w"); ok {
		t.Error("SurfaceDef(w): still retained after Reset")
	}
}

// TestAccelerated: a `builtin natAdd` registration is visible by name, so the
// explainer can keep accelerated defs one-line instead of showing fuel loops.
func TestAccelerated(t *testing.T) {
	s := New()
	src := "data Nat : U is zero : Nat | succ : Nat -> Nat end\n" +
		"builtin nat Nat zero succ\n" +
		"addN : Nat -> Nat -> Nat is\n" +
		"  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end\n" +
		"end\n" +
		"builtin natAdd addN\n"
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("LoadSource: %v", err)
	}
	if !s.Accelerated("addN") {
		t.Error("Accelerated(addN) = false, want true")
	}
	if s.Accelerated("zero") {
		t.Error("Accelerated(zero) = true, want false")
	}
	if s.Accelerated("nosuch") {
		t.Error("Accelerated(nosuch) = true, want false")
	}
}
