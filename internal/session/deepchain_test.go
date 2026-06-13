package session

import (
	"testing"
	"time"
)

// The deep-chain prelude: just enough unary Nat arithmetic to build constructor
// chains thousands of nodes deep, self-contained so the guard does not depend
// on the REPL prelude file.
const deepChainSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end

builtin nat Nat zero succ

+ : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end

* : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> 0
    | succ k with ih -> ih + n
    end
  end
end
`

func deepChainSession(t *testing.T) *Session {
	t.Helper()
	s := New()
	if _, err := s.LoadSource(deepChainSrc); err != nil {
		t.Fatalf("loading deep-chain prelude: %v", err)
	}
	return s
}

// pipeline runs the full REPL expression path — parse, elaborate (type check),
// normalize, pretty — and returns the printed normal form.
func pipeline(t *testing.T, s *Session, src string) string {
	t.Helper()
	e, err := s.ParseSrcExpr(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elaborate %q: %v", src, err)
	}
	return s.Pretty(s.NormalizeExpr(tm))
}

// TestDeepChainLiteralLinear guards the spine-sharing fix: checking an n-deep
// numeral (a succ-chain literal) must scale roughly linearly in n. Before the
// fix, the elaborator re-evaluated every argument subterm at each application
// node — O(n²), ~7s for a 4096 literal. The guard passes outright if the deep
// literal is fast in absolute terms (generous 50x headroom over the fixed
// ~25ms), and only on very slow machines falls back to a loose scaling check:
// quadratic growth makes T(4096)/T(1024) ≈ 16, linear ≈ 4.
func TestDeepChainLiteralLinear(t *testing.T) {
	s := deepChainSession(t)
	pipeline(t, s, "1024") // warm-up: first run pays one-time costs

	t0 := time.Now()
	if got := pipeline(t, s, "1024"); got != "1024" {
		t.Fatalf("1024 normalized to %q", got)
	}
	small := time.Since(t0)

	t1 := time.Now()
	if got := pipeline(t, s, "4096"); got != "4096" {
		t.Fatalf("4096 normalized to %q", got)
	}
	big := time.Since(t1)

	if big < 1250*time.Millisecond {
		return // fast in absolute terms: the quadratic (~7s) cannot hide here
	}
	if small > 0 && float64(big)/float64(small) > 10 {
		t.Fatalf("deep literal scaling looks superlinear: 1024 took %v, 4096 took %v (ratio %.1f, want <10)",
			small, big, float64(big)/float64(small))
	}
}

// TestDeepChainMulNormalizes pins the headline repro: 80 * 100 builds an
// 8000-deep constructor chain through unary multiplication. The bound is
// deliberately generous (the fixed pipeline takes well under a second; the
// pre-fix one took several) — the guard is against gross regression, and the
// equality check pins correctness.
func TestDeepChainMulNormalizes(t *testing.T) {
	s := deepChainSession(t)
	t0 := time.Now()
	if got := pipeline(t, s, "80 * 100"); got != "8000" {
		t.Fatalf("80 * 100 normalized to %q, want 8000", got)
	}
	if d := time.Since(t0); d > 10*time.Second {
		t.Fatalf("80 * 100 took %v through the session pipeline, want well under 10s", d)
	}
}

// BenchmarkDeepLiteralCheck records the cost of checking + normalizing the
// deepest numeral the parser admits.
func BenchmarkDeepLiteralCheck(b *testing.B) {
	s := New()
	if _, err := s.LoadSource(deepChainSrc); err != nil {
		b.Fatal(err)
	}
	e, err := s.ParseSrcExpr("4096")
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		tm, _, err := s.ElabExpr(e)
		if err != nil {
			b.Fatal(err)
		}
		s.NormalizeExpr(tm)
	}
}

// BenchmarkDeepChainMul records the cost of normalizing 80 * 100 (an 8000-deep
// result chain; the ι-step count is intrinsic to unary multiplication).
func BenchmarkDeepChainMul(b *testing.B) {
	s := New()
	if _, err := s.LoadSource(deepChainSrc); err != nil {
		b.Fatal(err)
	}
	e, err := s.ParseSrcExpr("80 * 100")
	if err != nil {
		b.Fatal(err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		b.Fatal(err)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s.NormalizeExpr(tm)
	}
}
