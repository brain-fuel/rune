package store

import (
	"testing"

	"goforge.dev/rune/core"
)

// The quotient builtin group: five bodiless members, content-addressed as a
// unit, with stable hashes and the roles the evaluator's ι-rules consult.

func TestAddQuotDeterministicAndDistinct(t *testing.T) {
	a := New().AddQuot()
	b := New().AddQuot()
	if a != b {
		t.Fatalf("quotient group hashes differ across stores:\n%v\n%v", a, b)
	}
	seen := map[core.Hash]bool{}
	for _, h := range a {
		if seen[h] {
			t.Fatalf("duplicate member hash %s", h.Short())
		}
		seen[h] = true
	}
}

func TestQuotRolesAndBarrier(t *testing.T) {
	s := New()
	hs := s.AddQuot()
	want := []core.QuotRole{core.QRoleQuot, core.QRoleIn, core.QRoleSound, core.QRoleLift, core.QRoleInd}
	for i, h := range hs {
		if got := s.QuotRoleOf(h); got != want[i] {
			t.Fatalf("member %d (%s): role %v, want %v", i, QuotNames()[i], got, want[i])
		}
		ty, ok := s.TypeOf(h)
		if !ok || ty == nil {
			t.Fatalf("member %s has no stored type", QuotNames()[i])
		}
		if _, ok := s.Unfold(h); ok {
			t.Fatalf("member %s has a body; builtins must be bodiless (permanently neutral)", QuotNames()[i])
		}
	}
	if s.QuotRoleOf(core.Hash{}) != core.QRoleNone {
		t.Fatal("unknown hash should have no quotient role")
	}
	// Names are bound.
	for i, n := range QuotNames() {
		h, ok := s.Lookup(n)
		if !ok || h != hs[i] {
			t.Fatalf("name %s not bound to its member hash", n)
		}
	}
}

func TestQuotTypesMentionNoPlaceholders(t *testing.T) {
	s := New()
	hs := s.AddQuot()
	for i, h := range hs {
		ty, _ := s.TypeOf(h)
		for j := 0; j < 5; j++ {
			if mentionsRef(ty, Placeholder(j)) {
				t.Fatalf("member %s: stored type still mentions Placeholder(%d)", QuotNames()[i], j)
			}
		}
	}
}

// mentionsRef is a test-local walk (the production one lives in elaborate).
func mentionsRef(t core.Tm, h core.Hash) bool {
	switch x := t.(type) {
	case nil:
		return false
	case core.Ref:
		return x.Hash == h
	case core.Pi:
		return mentionsRef(x.Dom, h) || mentionsRef(x.Cod.Body, h)
	case core.Lam:
		return mentionsRef(x.Body.Body, h)
	case core.App:
		return mentionsRef(x.Fn, h) || mentionsRef(x.Arg, h)
	case core.Let:
		return mentionsRef(x.Ty, h) || mentionsRef(x.Val, h) || mentionsRef(x.Body.Body, h)
	case core.Ann:
		return mentionsRef(x.Term, h) || mentionsRef(x.Ty, h)
	case core.Eq:
		return mentionsRef(x.Ty, h) || mentionsRef(x.L, h) || mentionsRef(x.R, h)
	case core.Refl:
		return mentionsRef(x.Tm, h)
	case core.Cast:
		return mentionsRef(x.A, h) || mentionsRef(x.B, h) || mentionsRef(x.P, h) || mentionsRef(x.X, h)
	case core.Subst:
		return mentionsRef(x.A, h) || mentionsRef(x.X, h) || mentionsRef(x.Y, h) ||
			mentionsRef(x.Prf, h) || mentionsRef(x.P, h) || mentionsRef(x.Px, h)
	default:
		return false
	}
}
