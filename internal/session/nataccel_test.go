package session

import (
	"fmt"
	"testing"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/surface"
)

// C7 / R-NUM Decision-1 SESSION WIRING tests. The core mechanism (NatLit,
// tryNatAccel, m.Na) is unit-tested in core/natlit_test.go against a hand-built
// Machine; these tests pin that a REAL session, loading `builtin natAdd add`
// (etc.), actually threads the acceleration table onto the Machines it builds so
// the fast path fires for the user's own ops — AND that the registration's
// soundness gate (the metatheory-review event) rejects a wrong op masquerade.

// natAccelSrc is a standard nat + add/mul/monus by the eliminator (the user's
// ordinary recursive bodies), then the three acceleration declarations.
const natAccelSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end
mul : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is add b ih end) a
  end
end
pred : Nat -> Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is k end) n
  end
end
monus : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) a (fn (k : Nat) (ih : Nat) is pred ih end) b
  end
end
builtin nat Nat zero succ
builtin natAdd add
builtin natMul mul
builtin natMonus monus
`

// litValue normalizes a session expression and reads its nat value back, so the
// tests compare integers regardless of whether the result stayed a NatLit or
// folded to a succ-chain.
func litValue(t *testing.T, s *Session, src string) int64 {
	t.Helper()
	return litValueWith(t, s, src, s.NormalizeExpr)
}

// litValueFast reads the result by FOLDED normalization (Normalize, not
// NormalizeUnfold), which keeps an accel-produced literal compact instead of
// materialising its succ-chain — so it can read a huge accel result (e.g.
// mul 4096 4096) without blowing the stack. The accel ι fires during eval at the
// registered head, so the head needs no δ-unfold; the literal it yields quotes
// back compactly. (The unfold-quote materialisation of a literal is the documented
// remaining C7 display step — see the report.)
func litValueFast(t *testing.T, s *Session, src string) int64 {
	t.Helper()
	return litValueWith(t, s, src, func(tm core.Tm) core.Tm {
		return s.newNormalizer(true).Normalize(tm)
	})
}

func litValueWith(t *testing.T, s *Session, src string, norm func(core.Tm) core.Tm) int64 {
	t.Helper()
	e, err := s.ParseSrcExpr(src)
	if err != nil {
		t.Fatalf("parse %q: %v", src, err)
	}
	tm, _, err := s.ElabExpr(e)
	if err != nil {
		t.Fatalf("elab %q: %v", src, err)
	}
	nf := norm(tm)
	n, ok := s.natLitValueOf(nf)
	if !ok {
		t.Fatalf("%q did not normalize to a numeral: %s", src, s.Pretty(nf))
	}
	return n
}

// TestNatAccelSessionWiringDifferential is the differential soundness gate at the
// SESSION level: for many (a,b), the accelerated result (m.Na set, the fast path)
// equals the def's UNFOLDED recursive result (acceleration OFF) AND the expected
// integer. A second session with NO acceleration declared runs the same calls by
// the ordinary eliminator body; both must agree with each other and with Go.
func TestNatAccelSessionWiringDifferential(t *testing.T) {
	fast := New()
	if _, err := fast.LoadSource(natAccelSrc); err != nil {
		t.Fatalf("load accel session: %v", err)
	}
	// A control session WITHOUT the accel declarations: same defs, no fast path.
	slow := New()
	if _, err := slow.LoadSource(`
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end
mul : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is add b ih end) a
  end
end
builtin nat Nat zero succ
`); err != nil {
		t.Fatalf("load control session: %v", err)
	}

	cases := []struct{ a, b int }{{0, 0}, {0, 5}, {5, 0}, {3, 4}, {7, 9}, {12, 11}, {20, 13}}
	for _, op := range []struct {
		name string
		want func(a, b int) int
	}{
		{"add", func(a, b int) int { return a + b }},
		{"mul", func(a, b int) int { return a * b }},
	} {
		for _, c := range cases {
			expr := fmt.Sprintf("%s %d %d", op.name, c.a, c.b)
			gotFast := litValue(t, fast, expr)
			gotSlow := litValue(t, slow, expr)
			want := int64(op.want(c.a, c.b))
			if gotFast != want {
				t.Fatalf("accel %q = %d, want %d", expr, gotFast, want)
			}
			if gotSlow != want {
				t.Fatalf("unfolded %q = %d, want %d", expr, gotSlow, want)
			}
			if gotFast != gotSlow {
				t.Fatalf("accel ≠ unfolded for %q: %d vs %d", expr, gotFast, gotSlow)
			}
		}
	}
	// monus only on the accel session (truncated subtraction).
	for _, c := range []struct{ a, b, want int }{{8, 3, 5}, {3, 8, 0}, {5, 5, 0}, {10, 0, 10}} {
		got := litValue(t, fast, fmt.Sprintf("monus %d %d", c.a, c.b))
		if got != int64(c.want) {
			t.Fatalf("monus %d %d = %d, want %d", c.a, c.b, got, c.want)
		}
	}
}

// TestNatAccelFiresLargeNoBlowup is the performance pin: mul 4096 4096 computes
// via the accel table in one bigint step. Without wiring this would peel O(a·b)
// eliminator steps; with the table threaded onto the session Machine it returns
// the literal promptly. (The test simply asserts correctness — a hang would fail
// the suite's own timeout.)
func TestNatAccelFiresLargeNoBlowup(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(natAccelSrc); err != nil {
		t.Fatalf("load: %v", err)
	}
	got := litValueFast(t, s, "mul 4096 4096")
	if got != 4096*4096 {
		t.Fatalf("mul 4096 4096 = %d, want %d", got, 4096*4096)
	}
}

// TestNatAccelDoesNotFireOnNeutral pins that acceleration preserves abstract
// reasoning: an open-term proof `add zero n = n` still checks by the def's
// ordinary body (the accel rule cannot fire on a variable). If the wiring had
// made accel fire on neutrals, this elaboration would diverge from the kernel's
// open-term behaviour.
func TestNatAccelDoesNotFireOnNeutral(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(natAccelSrc); err != nil {
		t.Fatalf("load: %v", err)
	}
	// add zero n ≡ n by refl — holds iff the def reduces by its body on a neutral
	// n exactly as without acceleration (add recurses on its first arg, hits zero,
	// returns the base b = n).
	if _, err := s.LoadSource(`
addZeroL : (n : Nat) -> Eq Nat (add zero n) n is
  fn (n : Nat) is refl n end
end
`); err != nil {
		t.Fatalf("open-term proof `add zero n = n` failed to check (accel may have fired on a neutral): %v", err)
	}
}

// TestNatAccelBridgeIsRefl pins R-NUM's soundness crux: the accelerated op IS the
// user's own def, so `add 4096 4096 = 8192` is provable by refl — both sides
// normalize to the same literal whether or not the fast path fired.
func TestNatAccelBridgeIsRefl(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(natAccelSrc); err != nil {
		t.Fatalf("load: %v", err)
	}
	if _, err := s.LoadSource(`
fast : Eq Nat (add 4096 4096) 8192 is refl 8192 end
`); err != nil {
		t.Fatalf("bridge-is-refl `add 4096 4096 = 8192` failed: %v", err)
	}
}

// --- The metatheory-review GATE: mutation / soundness checks -----------------

// TestNatAccelRejectsWrongOp is the mutation/soundness gate: registering `mul`'s
// def as `natAdd` (a WRONG op) MUST be refused at the `builtin natAdd` site,
// because the accel would produce a+b while mul's body peels to a*b — two values
// that should be equal yet differ, i.e. unsound. The registration-time
// differential validation catches it. This is the single genuine review event of
// R-NUM, made a hard gate rather than a trust assumption.
func TestNatAccelRejectsWrongOp(t *testing.T) {
	s := New()
	// Load the defs + builtin nat WITHOUT the accel decls.
	if _, err := s.LoadSource(`
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end
mul : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is add b ih end) a
  end
end
builtin nat Nat zero succ
`); err != nil {
		t.Fatalf("load: %v", err)
	}
	// Register mul AS natAdd — the masquerade. Must be rejected.
	if err := s.AddBuiltinNatOp(surface.BuiltinNatOp{Kind: "natAdd", DefName: "mul"}); err == nil {
		t.Fatal("registering mul as natAdd was accepted — an unsound op masquerade slipped past the gate")
	}
	// And the genuine correct registration of add as natAdd succeeds.
	if err := s.AddBuiltinNatOp(surface.BuiltinNatOp{Kind: "natAdd", DefName: "add"}); err != nil {
		t.Fatalf("correct registration of add as natAdd was rejected: %v", err)
	}
}

// TestNatAccelRejectsIllTyped: a def that is not Nat -> Nat -> Nat is refused
// (the rung-C4 type validation half of the gate).
func TestNatAccelRejectsIllTyped(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(`
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
data Bool : U is
  true : Bool
| false : Bool
end
notAdd : Nat -> Nat -> Bool is fn (a : Nat) (b : Nat) is true end end
builtin nat Nat zero succ
`); err != nil {
		t.Fatalf("load: %v", err)
	}
	if err := s.AddBuiltinNatOp(surface.BuiltinNatOp{Kind: "natAdd", DefName: "notAdd"}); err == nil {
		t.Fatal("registering a Nat -> Nat -> Bool def as natAdd was accepted")
	}
}

// TestNatAccelRequiresBuiltinNat: an acceleration declaration without an active
// `builtin nat` is refused (the literal the accel folds into is relative to it).
func TestNatAccelRequiresBuiltinNat(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(`
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end
`); err != nil {
		t.Fatalf("load: %v", err)
	}
	if err := s.AddBuiltinNatOp(surface.BuiltinNatOp{Kind: "natAdd", DefName: "add"}); err == nil {
		t.Fatal("natAdd registration without a `builtin nat` was accepted")
	}
}

// TestNatAccelOnlyFiresForRegisteredHash: acceleration is keyed on the def's
// content hash. A second, DIFFERENT nat function that is NOT registered must run
// by its ordinary body — the table must not accelerate an unregistered head. We
// register `add` only, then check a NON-registered `mul` still computes correctly
// (it would, accel or not — but crucially the table's NatOpOf must return None
// for mul's hash, proven by mul not being accelerated as add).
func TestNatAccelOnlyFiresForRegisteredHash(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(`
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
add : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end
mul : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is add b ih end) a
  end
end
builtin nat Nat zero succ
builtin natAdd add
`); err != nil {
		t.Fatalf("load: %v", err)
	}
	tab, ok := s.natAccelInfo().(natAccelTable)
	if !ok {
		t.Fatal("session natAccelInfo is not a natAccelTable")
	}
	addH, _ := s.Lookup("add")
	mulH, _ := s.Lookup("mul")
	if tab.NatOpOf(addH) != core.NatOpAdd {
		t.Fatal("registered add is not NatOpAdd in the table")
	}
	if tab.NatOpOf(mulH) != core.NatOpNone {
		t.Fatal("unregistered mul is accelerated — the table fires on a non-matching def")
	}
	// mul still computes correctly by its body (it calls the accelerated add).
	if got := litValue(t, s, "mul 6 7"); got != 42 {
		t.Fatalf("mul 6 7 = %d, want 42", got)
	}
}
