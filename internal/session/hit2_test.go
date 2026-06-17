package session

import (
	"strings"
	"testing"
)

// A9 (R-HIT) — the suspension and fibrant-quotient HITs, the dim-≤1 parameterised
// slice completing the kit alongside the circle. Each recursor computes by ι on
// its point ctors, along its path ctor, and commutes with hcomp.

// --- Suspension ------------------------------------------------------------

func TestSuspBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"Susp", "north", "south", "merid", "suspElim"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound", n)
		}
	}
	h1, _ := s.Lookup("suspElim")
	s.Reset()
	h2, ok := s.Lookup("suspElim")
	if !ok || h1 != h2 {
		t.Fatalf("suspElim hash not stable across reset")
	}
}

const suspCtx = `fn (A : UF) (P : UF) (n : El P) (s : El P)
   (m : (a : El A) -> El (pathF P n s))`

func TestSuspElimPoints(t *testing.T) {
	s := New()
	gotN := normalize(t, s, suspCtx+` is suspElim A P n s m (north A) end`)
	if strings.Contains(gotN, "suspElim") || !strings.Contains(gotN, "n") {
		t.Fatalf("suspElim … north must reduce to n, got %q", gotN)
	}
	gotS := normalize(t, s, suspCtx+` is suspElim A P n s m (south A) end`)
	if strings.Contains(gotS, "suspElim") || !strings.Contains(gotS, " s") {
		t.Fatalf("suspElim … south must reduce to s, got %q", gotS)
	}
}

func TestSuspElimMerid(t *testing.T) {
	s := New()
	src := suspCtx + ` (a : El A) (i : I) is
	   suspElim A P n s m (papp (Susp A) (north A) (south A) (merid A a) i)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "suspElim") {
		t.Fatalf("suspElim along a meridian must reduce, got %q", got)
	}
	if !strings.Contains(got, "papp") || !strings.Contains(got, "m a") {
		t.Fatalf("suspElim … (merid a i) must reduce to papp P n s (m a) i, got %q", got)
	}
}

func TestMeridBoundary(t *testing.T) {
	s := New()
	gotN := normalize(t, s, `fn (A : UF) (a : El A) is
	   papp (Susp A) (north A) (south A) (merid A a) i0 end`)
	if !strings.Contains(gotN, "north A") {
		t.Fatalf("merid boundary at i0 must be north A, got %q", gotN)
	}
	gotS := normalize(t, s, `fn (A : UF) (a : El A) is
	   papp (Susp A) (north A) (south A) (merid A a) i1 end`)
	if !strings.Contains(gotS, "south A") {
		t.Fatalf("merid boundary at i1 must be south A, got %q", gotS)
	}
}

func TestSuspTranspRegularity(t *testing.T) {
	s := New()
	got := normalize(t, s, `fn (A : UF) is transp (fn (i : I) is Susp A end) (north A) end`)
	if !strings.Contains(got, "north A") {
		t.Fatalf("transp over a constant Susp line must be north A, got %q", got)
	}
}

func TestSuspElimHcomp(t *testing.T) {
	s := New()
	src := suspCtx + ` (phi : F) (u : I -> holds phi -> El (Susp A)) (u0 : El (Susp A)) is
	   suspElim A P n s m (hcomp (Susp A) phi u u0)
	 end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "hcomp") || !strings.Contains(got, "suspElim A P n s m u0") {
		t.Fatalf("suspElim over an hcomp cell must commute (recursor pushed onto floor), got %q", got)
	}
}

// --- Fibrant quotient -------------------------------------------------------

func TestQuotHitBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"Quotient", "qinc", "qrel", "quotElim"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound", n)
		}
	}
	h1, _ := s.Lookup("quotElim")
	s.Reset()
	h2, ok := s.Lookup("quotElim")
	if !ok || h1 != h2 {
		t.Fatalf("quotElim hash not stable across reset")
	}
}

const quotCtx = `fn (A : UF) (R : El A -> El A -> UF) (P : UF) (f : El A -> El P)
   (rel : (a : El A) -> (b : El A) -> (r : El (R a b)) -> El (pathF P (f a) (f b)))`

func TestQuotElimInc(t *testing.T) {
	s := New()
	got := normalize(t, s, quotCtx+` (a : El A) is quotElim A R P f rel (qinc A R a) end`)
	if strings.Contains(got, "quotElim") {
		t.Fatalf("quotElim … (qinc a) must reduce, got %q", got)
	}
	if !strings.Contains(got, "f a") {
		t.Fatalf("quotElim … (qinc a) must reduce to f a, got %q", got)
	}
}

func TestQuotElimRel(t *testing.T) {
	s := New()
	src := quotCtx + ` (a : El A) (b : El A) (r : El (R a b)) (i : I) is
	   quotElim A R P f rel
	     (papp (Quotient A R) (qinc A R a) (qinc A R b) (qrel A R a b r) i)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "quotElim") {
		t.Fatalf("quotElim along a relation path must reduce, got %q", got)
	}
	if !strings.Contains(got, "papp") || !strings.Contains(got, "rel a b r") {
		t.Fatalf("quotElim … (qrel a b r i) must reduce to papp P (f a) (f b) (rel a b r) i, got %q", got)
	}
}

func TestQrelBoundary(t *testing.T) {
	s := New()
	got := normalize(t, s, `fn (A : UF) (R : El A -> El A -> UF) (a : El A) (b : El A)
	   (r : El (R a b)) is
	   papp (Quotient A R) (qinc A R a) (qinc A R b) (qrel A R a b r) i0 end`)
	if !strings.Contains(got, "qinc A R a") {
		t.Fatalf("qrel boundary at i0 must be qinc A R a, got %q", got)
	}
}

func TestQuotElimHcomp(t *testing.T) {
	s := New()
	src := quotCtx + ` (phi : F) (u : I -> holds phi -> El (Quotient A R))
	   (u0 : El (Quotient A R)) is
	   quotElim A R P f rel (hcomp (Quotient A R) phi u u0)
	 end`
	got := normalize(t, s, src)
	if !strings.Contains(got, "hcomp") || !strings.Contains(got, "quotElim A R P f rel u0") {
		t.Fatalf("quotElim over an hcomp cell must commute (recursor pushed onto floor), got %q", got)
	}
}
