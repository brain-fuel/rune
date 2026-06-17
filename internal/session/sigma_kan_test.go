package session

import (
	"strings"
	"testing"
)

// A5a: transport over a NON-DEPENDENT inner-Σ (product) line fires componentwise
// — it pushes into a pairF of the two component transports (rather than staying a
// stuck transp at the Σ type). Here the first component (fib X) is constant in i
// so its transport vanishes by regularity, and the second (B i) varies; the
// result is a pairF, which a stuck transport is not.
func TestTranspSigmaProductFires(t *testing.T) {
	s := New()
	src := `fn (X : U) (B : I -> UF)
	          (p : El (sigmaF (fib X) (fn (z : El (fib X)) is B i0 end))) is
	          transp (fn (i : I) is sigmaF (fib X) (fn (z : El (fib X)) is B i end) end) p
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("transp over a non-dependent Σ product must push to a pairF, got %q", got)
	}
}

// A5a (homogeneous): hcomp over a non-dependent inner-Σ product pushes to a
// pairF of two componentwise hcomps.
func TestHcompSigmaProductFires(t *testing.T) {
	s := New()
	src := `fn (X : UF) (Y : UF) (phi : F)
	          (u : I -> holds phi -> El (sigmaF X (fn (z : El X) is Y end)))
	          (u0 : El (sigmaF X (fn (z : El X) is Y end))) is
	          hcomp (sigmaF X (fn (z : El X) is Y end)) phi u u0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("hcomp over a non-dependent Σ product must push to a pairF, got %q", got)
	}
}

// A5a (heterogeneous): comp over a non-dependent inner-Σ product LINE pushes to
// a pairF of two componentwise comps.
func TestCompSigmaProductFires(t *testing.T) {
	s := New()
	src := `fn (X : UF) (B : I -> UF) (phi : F)
	          (u : (i : I) -> holds phi -> El (sigmaF X (fn (z : El X) is B i end)))
	          (u0 : El (sigmaF X (fn (z : El X) is B i0 end))) is
	          comp (fn (i : I) is sigmaF X (fn (z : El X) is B i end) end) phi u u0
	        end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "pairF") {
		t.Fatalf("comp over a non-dependent Σ product must push to a pairF, got %q", got)
	}
}
