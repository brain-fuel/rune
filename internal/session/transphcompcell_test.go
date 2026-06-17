package session

import (
	"strings"
	"testing"
)

// R-HIT / A9 — the LAST cubical filler the HIT kit left honest-stuck: transport of
// a formal `hcomp` CELL along a varying HIT line. A proper-face `hcomp (D p) φ u u0`
// is the canonical Kan generator (CHM hcomp-as-constructor); `transp` over a HIT
// line COMMUTES with it because the cell's system is indexed by the hcomp's own
// filling dimension, not the transport dimension:
//
//	transp (λi. D(A i)) φ (hcomp (D(A i0)) ψ u u0)
//	  ~> hcomp (D(A i1)) ψ (λj h. transp (λi. D(A i)) φ (u j h))
//	                       (transp (λi. D(A i)) φ u0)
//
// The reduct is a cell at the i1 type, which the recursor/inductor already commute
// with — so closed-term canonicity over a parameterised HIT is preserved with no
// new elimination branch.

// transp over a varying SUSPENSION line of a formal hcomp cell reduces to a cell at
// the i1 type, with the wall system and the cap transported along the line.
func TestSuspTranspHcompCell(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) (phi : F)
	   (u : (i : I) -> holds phi -> El (Susp (Afam i0)))
	   (u0 : El (Susp (Afam i0))) is
	   transp (fn (i : I) is Susp (Afam i) end) (hcomp (Susp (Afam i0)) phi u u0)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp (fn (i : I) is Susp") {
		t.Fatalf("transp of an hcomp cell over a Susp line must reduce the outer transp, got %q", got)
	}
	if !strings.Contains(got, "hcomp") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must land on hcomp (Susp (Afam i1)) …, got %q", got)
	}
}

// transp over a varying QUOTIENT line of a formal hcomp cell, dual to the Susp case.
func TestQuotTranspHcompCell(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) (Rfam : (i : I) -> El (Afam i) -> El (Afam i) -> UF)
	   (phi : F)
	   (u : (i : I) -> holds phi -> El (Quotient (Afam i0) (Rfam i0)))
	   (u0 : El (Quotient (Afam i0) (Rfam i0))) is
	   transp (fn (i : I) is Quotient (Afam i) (Rfam i) end)
	     (hcomp (Quotient (Afam i0) (Rfam i0)) phi u u0)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "transp (fn (i : I) is Quotient") {
		t.Fatalf("transp of an hcomp cell over a Quotient line must reduce the outer transp, got %q", got)
	}
	if !strings.Contains(got, "hcomp") || !strings.Contains(got, "Afam i1") {
		t.Fatalf("must land on hcomp (Quotient (Afam i1) (Rfam i1)) …, got %q", got)
	}
}

// CONFLUENCE with the recursor: transp gives an hcomp cell, and suspElim then
// commutes with that cell (the existing hcomp branch), pushing the composition into
// the motive — so eliminating the transported cell computes. The outer suspElim and
// the outer transp must both discharge.
func TestSuspElimTranspHcompCell(t *testing.T) {
	s := New()
	src := `fn (Afam : I -> UF) (P : UF) (phi : F)
	   (n : El P) (sp : El P) (mer : (a : El (Afam i1)) -> El (pathF P n sp))
	   (u : I -> holds phi -> El (Susp (Afam i0)))
	   (u0 : El (Susp (Afam i0))) is
	   suspElim (Afam i1) P n sp mer
	     (transp (fn (i : I) is Susp (Afam i) end) (hcomp (Susp (Afam i0)) phi u u0))
	 end`
	got := normalize(t, s, src)
	// The top-level suspElim + transp + hcomp all discharge: the result is the
	// composition pushed into the motive, `hcomp P phi …`. (Inner suspElims over the
	// transported wall slices / floor stay neutral — those scrutinees are neutral.)
	if !strings.Contains(got, "hcomp P phi") {
		t.Fatalf("the recursor must commute with the transp-produced cell into the motive (hcomp P phi …), got %q", got)
	}
}
