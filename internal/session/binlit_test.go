package session

import (
	"testing"
)

// The numeral prelude: unary Nat (the only literal meaning, as of C7 / R-NUM)
// alongside a hand-written binary Pos/BN datatype and its readback to Nat. The
// `builtin bin` binary-literal lowering is RETIRED (Decision 5); Pos/BN survive
// as ordinary user data, populated from Nat via fromNat.
const numPrelude = `
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ

data Pos : U is pH : Pos | pO : Pos -> Pos | pI : Pos -> Pos end
data BN : U is bn0 : BN | bnP : Pos -> BN end

psucc : Pos -> Pos is
  fn (p : Pos) is case p of | pH -> pO pH | pO a -> pI a | pI a with ih -> pO ih end end
end
bnsucc : BN -> BN is
  fn (b : BN) is case b of | bn0 -> bnP pH | bnP p -> bnP (psucc p) end end
end
fromNat : Nat -> BN is
  fn (n : Nat) is case n of | zero -> bn0 | succ k with ih -> bnsucc ih end end
end

double : Nat -> Nat is
  fn (n : Nat) is case n of | zero -> 0 | succ k with ih -> succ (succ ih) end end
end
toNatP : Pos -> Nat is
  fn (p : Pos) is case p of | pH -> 1 | pO a with ih -> double ih | pI a with ih -> succ (double ih) end end
end
toNat : BN -> Nat is
  fn (b : BN) is case b of | bn0 -> 0 | bnP p -> toNatP p end end
end
`

func numSession(t *testing.T) *Session {
	t.Helper()
	s := New()
	if _, err := s.LoadSource(numPrelude); err != nil {
		t.Fatalf("loading numeral prelude: %v", err)
	}
	return s
}

// numNorm normalizes src and prints with the folding printer (s.Pretty), so
// unary nat chains read back as numerals.
func numNorm(t *testing.T, s *Session, src string) string {
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

// TestNatLiteralRoundTripsThroughBN: a numeral is a NatLit; routed into the
// hand-written binary BN via fromNat and read back via toNat it recovers its
// value. The Pos/BN datatype + its agreement functions still type-check and
// compute without the retired `builtin bin` literal lowering.
func TestNatLiteralRoundTripsThroughBN(t *testing.T) {
	s := numSession(t)
	for _, n := range []string{"0", "1", "35", "255", "5000"} {
		if got := numNorm(t, s, "toNat (fromNat "+n+")"); got != n {
			t.Fatalf("toNat (fromNat %s) = %q, want %s", n, got, n)
		}
	}
	// A genuine Nat literal reads back as itself.
	if got := numNorm(t, s, "(7 : Nat)"); got != "7" {
		t.Fatalf("(7 : Nat) = %q, want 7", got)
	}
}

// TestNatLiteralHasNoCap: as of C7 / R-NUM the default `builtin nat` lowering
// emits a compressed NatLit, so the old 4096 unary cap is GONE — a bare numeral
// of any size elaborates as a single node and computes.
func TestNatLiteralHasNoCap(t *testing.T) {
	s := numSession(t)
	if got := numNorm(t, s, "toNat (fromNat 9001)"); got != "9001" {
		t.Fatalf("toNat (fromNat 9001) = %q, want 9001", got)
	}
	e, err := s.ParseSrcExpr("(9001 : Nat)")
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if _, _, err := s.ElabExpr(e); err != nil {
		t.Fatalf("(9001 : Nat) should now elaborate (no cap), got err=%v", err)
	}
}
