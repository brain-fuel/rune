package session

import (
	"strings"
	"testing"
)

// A9 (R-HIT) — the inner higher-inductive-type kit, shipped as the circle. These
// pin the recursor's two ι-rules, the loop boundary, constant-line transport, and
// loop/refl distinctness (the coexistence argument). See ref_docs/wootz/R-HIT.md.

// The circle's four members are bound ambiently in a fresh session and survive a
// reset with the same hashes (a builtin group, like quot/glue/pabsU).
func TestCircleBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"Circle", "base", "loop", "circElim"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound in a fresh session", n)
		}
	}
	h1, _ := s.Lookup("circElim")
	s.Reset()
	h2, ok := s.Lookup("circElim")
	if !ok || h1 != h2 {
		t.Fatalf("circElim hash not stable across reset")
	}
}

// circElim P b l base ~> b (the point branch, identical in shape to a data
// eliminator firing on a saturated constructor).
func TestCircElimBase(t *testing.T) {
	s := New()
	src := `fn (P : UF) (b : El P) (l : El (pathF P b b)) is
	          circElim P b l base
	        end`
	got := normalize(t, s, src)
	// The whole closure normalizes to the projection of b: the body is just b.
	if strings.Contains(got, "circElim") {
		t.Fatalf("circElim on base must reduce (no residual circElim), got %q", got)
	}
	if !strings.Contains(got, "b") {
		t.Fatalf("circElim P b l base must reduce to b, got %q", got)
	}
}

// circElim P b l (papp Circle base base loop i) ~> papp P b b l i (the loop
// branch: the recursor runs the loop method at the same interval coordinate).
func TestCircElimLoop(t *testing.T) {
	s := New()
	src := `fn (P : UF) (b : El P) (l : El (pathF P b b)) (i : I) is
	          circElim P b l (papp Circle base base loop i)
	        end`
	got := normalize(t, s, src)
	if strings.Contains(got, "circElim") {
		t.Fatalf("circElim along loop must reduce (no residual circElim), got %q", got)
	}
	if !strings.Contains(got, "papp") || !strings.Contains(got, "l") || !strings.Contains(got, "i") {
		t.Fatalf("circElim … (loop i) must reduce to papp P b b l i, got %q", got)
	}
}

// The loop boundary computes (via the path group's boundary rule, pinned for the
// circle's loop): papp Circle base base loop i0 ~> base, and at i1.
func TestLoopBoundary(t *testing.T) {
	s := New()
	for _, end := range []string{"i0", "i1"} {
		src := "papp Circle base base loop " + end
		got := normalize(t, s, src)
		if got != "base" {
			t.Fatalf("loop boundary at %s must be base, got %q", end, got)
		}
	}
}

// Constant-line transport over the circle is the identity — and this needs NO new
// code: the Kan regularity rule already returns the argument for a constant former
// line. transp (λ_. Circle) base ~> base.
func TestCircleTranspRegularity(t *testing.T) {
	s := New()
	src := `transp (fn (i : I) is Circle end) base`
	got := normalize(t, s, src)
	if got != "base" {
		t.Fatalf("transp over a constant Circle line must be base, got %q", got)
	}
}

// The circle is a genuine Kan HIT: a proper-face hcomp at Circle is a canonical
// generator (the formal cell, CHM's hcomp-as-constructor), and the recursor
// COMMUTES with it — circElim P b l (hcomp Circle φ u u0) reduces to an hcomp in
// the motive type P, with the recursor pushed under the walls and floor.
func TestCircElimHcomp(t *testing.T) {
	s := New()
	src := `fn (P : UF) (b : El P) (l : El (pathF P b b)) (phi : F)
	          (u : I -> holds phi -> El Circle) (u0 : El Circle) is
	          circElim P b l (hcomp Circle phi u u0)
	        end`
	got := normalize(t, s, src)
	if strings.Contains(got, "circElim Circle") {
		t.Fatalf("the eliminated scrutinee should not keep circElim at Circle, got %q", got)
	}
	if !strings.Contains(got, "hcomp") {
		t.Fatalf("circElim over an hcomp cell must commute to an hcomp in P, got %q", got)
	}
	// The recursor must be pushed INTO the box (its walls/floor), eliminating the
	// sub-terms in P rather than leaving the composite at Circle.
	if !strings.Contains(got, "circElim P b l u0") {
		t.Fatalf("the recursor should be pushed onto the floor (circElim P b l u0), got %q", got)
	}
}

// The loop is NOT refl: `loop` is a distinct neutral path head, so it does not
// normalize to `preflF Circle base`. (The strict outer Eq neither identifies nor
// refutes them — the same coexistence as ua ≠ ureflU.)
func TestLoopNotRefl(t *testing.T) {
	s := New()
	loop := normalize(t, s, `loop`)
	refl := normalize(t, s, `preflF Circle base`)
	if loop == refl {
		t.Fatalf("loop must be distinct from refl, both normalized to %q", loop)
	}
	if !strings.Contains(loop, "loop") {
		t.Fatalf("loop should stay a neutral loop head, got %q", loop)
	}
}
