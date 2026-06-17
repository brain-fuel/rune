package session

import (
	"strings"
	"testing"
)

// A9 — dependent eliminators (induction principles) for the parameterised HITs:
// suspInd (suspension) and quotInd (fibrant quotient). Each computes on its point
// ctors and along its path ctor into a dependent motive, via pathP.

func TestHitIndBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"suspInd", "quotInd"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound", n)
		}
	}
	h1, _ := s.Lookup("suspInd")
	q1, _ := s.Lookup("quotInd")
	s.Reset()
	h2, _ := s.Lookup("suspInd")
	q2, _ := s.Lookup("quotInd")
	if h1 != h2 || q1 != q2 {
		t.Fatalf("suspInd/quotInd hashes not stable across reset")
	}
}

const suspIndCtx = `fn (A : UF) (P : El (Susp A) -> UF)
   (pn : El (P (north A))) (ps : El (P (south A)))
   (pm : (a : El A) -> El (pathP (fn (i : I) is P (papp (Susp A) (north A) (south A) (merid A a) i) end) pn ps))`

func TestSuspIndPoints(t *testing.T) {
	s := New()
	gotN := normalize(t, s, suspIndCtx+` is suspInd A P pn ps pm (north A) end`)
	if strings.Contains(gotN, "suspInd") || !strings.Contains(gotN, "pn") {
		t.Fatalf("suspInd … north must reduce to pn, got %q", gotN)
	}
	gotS := normalize(t, s, suspIndCtx+` is suspInd A P pn ps pm (south A) end`)
	if strings.Contains(gotS, "suspInd") || !strings.Contains(gotS, "ps") {
		t.Fatalf("suspInd … south must reduce to ps, got %q", gotS)
	}
}

func TestSuspIndMerid(t *testing.T) {
	s := New()
	src := suspIndCtx + ` (a : El A) (i : I) is
	   suspInd A P pn ps pm (papp (Susp A) (north A) (south A) (merid A a) i)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "suspInd") {
		t.Fatalf("suspInd along a meridian must reduce, got %q", got)
	}
	if !strings.Contains(got, "pappP") || !strings.Contains(got, "pm a") {
		t.Fatalf("suspInd … (merid a i) must reduce to pappP … (pm a) i, got %q", got)
	}
}

// suspInd dependent hcomp branch: induction over an hcomp cell commutes via comp
// over the motive line — suspension induction is total on all generators.
func TestSuspIndHcomp(t *testing.T) {
	s := New()
	src := suspIndCtx + ` (phi : F) (u : I -> holds phi -> El (Susp A)) (u0 : El (Susp A)) is
	   suspInd A P pn ps pm (hcomp (Susp A) phi u u0)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "suspInd A") && !strings.Contains(got, "suspInd A P pn ps pm u0") {
		t.Fatalf("suspInd over an hcomp cell should commute, got %q", got)
	}
	if !strings.Contains(got, "comp") || !strings.Contains(got, "suspInd A P pn ps pm u0") {
		t.Fatalf("suspInd over an hcomp cell must commute to a comp with recursor on the floor, got %q", got)
	}
}

const quotIndCtx = `fn (A : UF) (R : El A -> El A -> UF) (P : El (Quotient A R) -> UF)
   (f : (a : El A) -> El (P (qinc A R a)))
   (rel : (a : El A) -> (b : El A) -> (r : El (R a b))
      -> El (pathP (fn (i : I) is P (papp (Quotient A R) (qinc A R a) (qinc A R b) (qrel A R a b r) i) end) (f a) (f b)))`

func TestQuotIndInc(t *testing.T) {
	s := New()
	got := normalize(t, s, quotIndCtx+` (a : El A) is quotInd A R P f rel (qinc A R a) end`)
	if strings.Contains(got, "quotInd") || !strings.Contains(got, "f a") {
		t.Fatalf("quotInd … (qinc a) must reduce to f a, got %q", got)
	}
}

func TestQuotIndRel(t *testing.T) {
	s := New()
	src := quotIndCtx + ` (a : El A) (b : El A) (r : El (R a b)) (i : I) is
	   quotInd A R P f rel
	     (papp (Quotient A R) (qinc A R a) (qinc A R b) (qrel A R a b r) i)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "quotInd") {
		t.Fatalf("quotInd along a relation path must reduce, got %q", got)
	}
	if !strings.Contains(got, "pappP") || !strings.Contains(got, "rel a b r") {
		t.Fatalf("quotInd … (qrel a b r i) must reduce to pappP … (rel a b r) i, got %q", got)
	}
}

// quotInd dependent hcomp branch: induction over an hcomp cell commutes via comp
// over the motive line — quotient induction is total on all generators.
func TestQuotIndHcomp(t *testing.T) {
	s := New()
	src := quotIndCtx + ` (phi : F) (u : I -> holds phi -> El (Quotient A R))
	   (u0 : El (Quotient A R)) is
	   quotInd A R P f rel (hcomp (Quotient A R) phi u u0)
	 end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "comp") || !strings.Contains(got, "quotInd A R P f rel u0") {
		t.Fatalf("quotInd over an hcomp cell must commute to a comp with recursor on the floor, got %q", got)
	}
}
