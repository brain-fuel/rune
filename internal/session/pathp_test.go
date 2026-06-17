package session

import (
	"strings"
	"testing"
)

// A9 prerequisite — dependent paths (pathP) and the circle's dependent eliminator
// (circInd), the induction principle. See store/pathp.go, store/circind.go.

func TestPathPBuiltinsAmbient(t *testing.T) {
	s := New()
	for _, n := range []string{"pathP", "pabsP", "pappP", "circInd"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("builtin %s not bound", n)
		}
	}
	h1, _ := s.Lookup("circInd")
	s.Reset()
	h2, ok := s.Lookup("circInd")
	if !ok || h1 != h2 {
		t.Fatalf("circInd hash not stable across reset")
	}
}

// pappP β: pappP A x y (pabsP A f) i ~> f i.
func TestPappPBeta(t *testing.T) {
	s := New()
	src := `fn (A : I -> UF) (f : (i : I) -> El (A i)) (i : I) is
	   pappP A (f i0) (f i1) (pabsP A f) i
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "pappP") || strings.Contains(got, "pabsP") {
		t.Fatalf("pappP β must reduce to f i, got %q", got)
	}
	if !strings.Contains(got, "f i") {
		t.Fatalf("pappP A x y (pabsP A f) i must reduce to f i, got %q", got)
	}
}

// pappP boundary: at i0 the left endpoint, at i1 the right, for ANY path.
func TestPappPBoundary(t *testing.T) {
	s := New()
	ctx := `fn (A : I -> UF) (x : El (A i0)) (y : El (A i1)) (p : El (pathP A x y))`
	gotL := normalize(t, s, ctx+` is pappP A x y p i0 end`)
	if !strings.Contains(gotL, "x") || strings.Contains(gotL, "pappP") {
		t.Fatalf("pappP … i0 must be x, got %q", gotL)
	}
	gotR := normalize(t, s, ctx+` is pappP A x y p i1 end`)
	if !strings.Contains(gotR, "y") || strings.Contains(gotR, "pappP") {
		t.Fatalf("pappP … i1 must be y, got %q", gotR)
	}
}

// circInd point branch: circInd P b l base ~> b.
func TestCircIndBase(t *testing.T) {
	s := New()
	src := `fn (P : El Circle -> UF) (b : El (P base))
	   (l : El (pathP (fn (i : I) is P (papp Circle base base loop i) end) b b)) is
	   circInd P b l base
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "circInd") {
		t.Fatalf("circInd … base must reduce, got %q", got)
	}
	if !strings.Contains(got, "b") {
		t.Fatalf("circInd P b l base must reduce to b, got %q", got)
	}
}

// circInd dependent hcomp branch: eliminating a formal hcomp cell into a
// dependent motive commutes via comp over the motive line λi. P (hfill … i). The
// induction principle is total on all three generators (base, loop, hcomp cell).
func TestCircIndHcomp(t *testing.T) {
	s := New()
	src := `fn (P : El Circle -> UF) (b : El (P base))
	   (l : El (pathP (fn (i : I) is P (papp Circle base base loop i) end) b b))
	   (phi : F) (u : I -> holds phi -> El Circle) (u0 : El Circle) is
	   circInd P b l (hcomp Circle phi u u0)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "circInd Circle") {
		t.Fatalf("the eliminated scrutinee should not keep circInd at Circle, got %q", got)
	}
	if !strings.Contains(got, "comp") {
		t.Fatalf("circInd over an hcomp cell must commute to a comp over the motive line, got %q", got)
	}
	if !strings.Contains(got, "circInd P b l u0") {
		t.Fatalf("the recursor should be pushed onto the floor (circInd P b l u0), got %q", got)
	}
}

// circInd loop branch: circInd P b l (loop i) ~> pappP (λi. P (loop@i)) b b l i.
func TestCircIndLoop(t *testing.T) {
	s := New()
	src := `fn (P : El Circle -> UF) (b : El (P base))
	   (l : El (pathP (fn (i : I) is P (papp Circle base base loop i) end) b b)) (i : I) is
	   circInd P b l (papp Circle base base loop i)
	 end`
	got := normalize(t, s, src)
	if strings.Contains(got, "circInd") {
		t.Fatalf("circInd along loop must reduce, got %q", got)
	}
	if !strings.Contains(got, "pappP") || !strings.Contains(got, "l") {
		t.Fatalf("circInd … (loop i) must reduce to pappP … l i, got %q", got)
	}
}
