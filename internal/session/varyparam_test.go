package session

import (
	"strings"
	"testing"
)

// R-FILL / R-HIT — varying-PARAMETER transport over a parameterised HIT line. The
// point constructors re-index the parameter (suspension poles carry no fiber
// element; the quotient's qinc transports its carried element along the carrier
// line). The path constructors (merid/qrel) and formal hcomp cells are the
// labelled R-FILL remainder.

// transp (λi. Susp (Afam i)) (north (Afam i0)) ~> north (Afam i1): the pole
// re-indexes along a genuinely varying parameter line.
func TestSuspVaryingParamNorth(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) is
	   transp (fn (i : I) is Susp (Afam i) end) (north (Afam i0))
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp") {
		t.Fatalf("varying-parameter transp of a pole must reduce, got %q", got)
	}
	if !strings.Contains(got, "north") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must re-index to north (Afam i1), got %q", got)
	}
}

func TestSuspVaryingParamSouth(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) is
	   transp (fn (i : I) is Susp (Afam i) end) (south (Afam i0))
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp") || !strings.Contains(got, "south") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must re-index to south (Afam i1), got %q", got)
	}
}

// transp (λi. Quotient (Afam i) (Rfam i)) (qinc (Afam i0) (Rfam i0) a)
//
//	~> qinc (Afam i1) (Rfam i1) (transp (λi. Afam i) a)
//
// The carried element transports along the carrier line; the indices re-index.
func TestQuotVaryingParamInc(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) (Rfam : (i : I) -> El (Afam i) -> El (Afam i) -> UF)
	   (a : El (Afam i0)) is
	   transp (fn (i : I) is Quotient (Afam i) (Rfam i) end)
	     (qinc (Afam i0) (Rfam i0) a)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp (fn") {
		t.Fatalf("varying-parameter transp of qinc must reduce the outer transp, got %q", got)
	}
	if !strings.Contains(got, "qinc") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must re-index to qinc (Afam i1) …, got %q", got)
	}
}

// The quotient RELATION point transports both carriers and the witness, then
// rebuilds qrel: transp (λi. Quotient (A i)(R i)) (qrel … a b r @ k)
//
//	~> qrel (A i1)(R i1) (transp a)(transp b)(transp_R r) @ k.
func TestQuotVaryingParamRel(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) (Rfam : (i : I) -> El (Afam i) -> El (Afam i) -> UF)
	   (a : El (Afam i0)) (b : El (Afam i0)) (r : El (Rfam i0 a b)) (k : I) is
	   transp (fn (i : I) is Quotient (Afam i) (Rfam i) end)
	     (papp (Quotient (Afam i0) (Rfam i0))
	           (qinc (Afam i0) (Rfam i0) a) (qinc (Afam i0) (Rfam i0) b)
	           (qrel (Afam i0) (Rfam i0) a b r) k)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp (fn") {
		t.Fatalf("qrel varying transport must reduce the outer transp, got %q", got)
	}
	if !strings.Contains(got, "qrel") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must rebuild qrel (Afam i1) …, got %q", got)
	}
}

// The MERIDIAN point transports its argument along the carrier line and rebuilds
// the meridian (R-FILL meridian filler): transp (λj. Susp (Afam j)) (merid (Afam
// i0) a @ k) ~> merid (Afam i1) (transp (λj. Afam j) a) @ k.
func TestSuspVaryingParamMerid(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) (a : El (Afam i0)) (k : I) is
	   transp (fn (j : I) is Susp (Afam j) end)
	     (papp (Susp (Afam i0)) (north (Afam i0)) (south (Afam i0)) (merid (Afam i0) a) k)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp (fn") {
		t.Fatalf("meridian varying transport must reduce the outer transp, got %q", got)
	}
	if !strings.Contains(got, "merid") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must rebuild merid (Afam i1) (transp … a), got %q", got)
	}
}

// Boundary coherence of the transported meridian: at k=i0 it is north (Afam i1),
// at k=i1 south (Afam i1) — matching the point-ctor transport (refl-pinnable).
func TestSuspVaryingParamMeridBoundary(t *testing.T) {
	s := New()
	for end, pole := range map[string]string{"i0": "north", "i1": "south"} {
		src := `fn (Afam : I -> UF) (a : El (Afam i0)) is
		   transp (fn (j : I) is Susp (Afam j) end)
		     (papp (Susp (Afam i0)) (north (Afam i0)) (south (Afam i0)) (merid (Afam i0) a) ` + end + `)
		 end`
		got := normalize(t, s, src)
		if !strings.Contains(got, pole) || !strings.Contains(got, "Afam i1") {
			t.Fatalf("meridian boundary at %s must be %s (Afam i1), got %q", end, pole, got)
		}
	}
}
