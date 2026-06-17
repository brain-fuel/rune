package session

import (
	"strings"
	"testing"
)

// A9 (dim-2 slice) — set-truncation ‖A‖₀: statable with the dim-2 path
// constructor squash, recursor computing on the point constructor inc.

func TestTruncBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"Trunc0", "inc", "squash", "trunc0Rec"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound", n)
		}
	}
	h1, _ := s.Lookup("trunc0Rec")
	s.Reset()
	h2, ok := s.Lookup("trunc0Rec")
	if !ok || h1 != h2 {
		t.Fatalf("trunc0Rec hash not stable across reset")
	}
}

// trunc0Rec A B setB f (inc A a) ~> f a (the point branch computes).
func TestTrunc0RecInc(t *testing.T) {
	s := New()
	src := `fn (A : UF) (B : UF)
	   (setB : (x : El B) -> (y : El B) -> (p : El (pathF B x y)) -> (q : El (pathF B x y))
	      -> El (pathF (pathF B x y) p q))
	   (f : El A -> El B) (a : El A) is
	   trunc0Rec A B setB f (inc A a)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "trunc0Rec") {
		t.Fatalf("trunc0Rec on inc must reduce, got %q", got)
	}
	if !strings.Contains(got, "f a") {
		t.Fatalf("trunc0Rec A B setB f (inc A a) must reduce to f a, got %q", got)
	}
}

// The dim-2 path constructor squash is statable and typechecks: it identifies any
// two paths between the same truncated points (the set-truncation's defining
// generator).
func TestSquashStatable(t *testing.T) {
	s := New()
	src := `fn (A : UF) (x : El (Trunc0 A)) (y : El (Trunc0 A))
	   (p : El (pathF (Trunc0 A) x y)) (q : El (pathF (Trunc0 A) x y)) is
	   squash A x y p q
	 end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "squash") {
		t.Fatalf("squash should be a statable neutral dim-2 generator, got %q", got)
	}
}

// truncHdr is the shared binder prologue for the dim-2 interior probes: the
// recursor's arguments (A, B, the set-witness setB, the map f), two truncated
// points x y, two paths p q between them, and two interval coordinates i j.
const truncHdr = `fn (A : UF) (B : UF)
   (setB : (x : El B) -> (y : El B) -> (p : El (pathF B x y)) -> (q : El (pathF B x y))
      -> El (pathF (pathF B x y) p q))
   (f : El A -> El B)
   (x : El (Trunc0 A)) (y : El (Trunc0 A))
   (p : El (pathF (Trunc0 A) x y)) (q : El (pathF (Trunc0 A) x y))
   (i : I) (j : I) is `

// DIM-2 INTERIOR (A9, the M3 cubical tail): trunc0Rec runs along the `squash`
// SQUARE. The scrutinee is a squash 2-cell observed at interval coordinates i
// (outer, between the two paths) and j (inner, along a path); the recursor
// reduces to the corresponding square in B, built from the set-witness setB,
// observed at the SAME coordinates — the dim-2 analog of the dim-1 loop rule.
// The squash generator is consumed (it FIRES); the outer head is `papp B …`
// (the B-square), and the residual trunc0Rec sub-terms are the recursive calls
// rec x / rec y / ap_rec p / ap_rec q on the (neutral) point/path variables.
func TestTrunc0RecSquashFires(t *testing.T) {
	s := New()
	src := truncHdr + `
	  trunc0Rec A B setB f
	    (papp (Trunc0 A) x y
	      (papp (pathF (Trunc0 A) x y) p q (squash A x y p q) i) j)
	end`
	got := normalize(t, s, src)
	if strings.Contains(got, "squash") {
		t.Fatalf("dim-2: the squash square must be consumed, got %q", got)
	}
	if !strings.Contains(got, "setB") {
		t.Fatalf("dim-2: reduct should build the setB square, got %q", got)
	}
	body := got[strings.Index(got, " is ")+4:]
	if !strings.HasPrefix(strings.TrimSpace(body), "papp B") {
		t.Fatalf("dim-2 reduct head should be papp B (the B-square), got %q", got)
	}
}

// Boundary coherence j=i0: the inner observation at j=i0 reads off the square's
// left edge, which must equal the recursor on the left endpoint x.
func TestTrunc0RecSquashBoundaryJ0(t *testing.T) {
	s := New()
	lhs := truncHdr + `
	  trunc0Rec A B setB f
	    (papp (Trunc0 A) x y
	      (papp (pathF (Trunc0 A) x y) p q (squash A x y p q) i) i0)
	end`
	rhs := truncHdr + ` trunc0Rec A B setB f x end`
	if gl, gr := normalize(t, s, lhs), normalize(t, s, rhs); gl != gr {
		t.Fatalf("dim-2 boundary j=i0 mismatch:\n  lhs=%s\n  rhs=%s", gl, gr)
	}
}

// Boundary coherence i=i0: the outer observation at i=i0 reads off the path p,
// so the recursor along it must equal rec applied to the p-point at j.
func TestTrunc0RecSquashBoundaryI0(t *testing.T) {
	s := New()
	lhs := truncHdr + `
	  trunc0Rec A B setB f
	    (papp (Trunc0 A) x y
	      (papp (pathF (Trunc0 A) x y) p q (squash A x y p q) i0) j)
	end`
	rhs := truncHdr + ` trunc0Rec A B setB f (papp (Trunc0 A) x y p j) end`
	if gl, gr := normalize(t, s, lhs), normalize(t, s, rhs); gl != gr {
		t.Fatalf("dim-2 boundary i=i0 mismatch:\n  lhs=%s\n  rhs=%s", gl, gr)
	}
}

// Negative probe: the dim-2 rule must STAY STUCK on a plain (non-squash) inner
// path — it must only fire when the doubly-nested papp's path argument is a
// genuine `squash` generator, never fabricating a B-square otherwise.
func TestTrunc0RecNonSquashStaysStuck(t *testing.T) {
	s := New()
	src := truncHdr + ` trunc0Rec A B setB f (papp (Trunc0 A) x y p i) end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "trunc0Rec") {
		t.Fatalf("dim-2 rule must stay stuck on a non-squash path, got %q", got)
	}
}
