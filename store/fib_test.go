package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// The fibrant builtin group (v3): eleven bodiless members, content-addressed
// as a unit, with stable hashes and the roles the two-level ι-rules consult.

func TestAddFibDeterministicAndDistinct(t *testing.T) {
	a := New().AddFib()
	b := New().AddFib()
	if a != b {
		t.Fatalf("fibrant group hashes differ across stores:\n%v\n%v", a, b)
	}
	seen := map[core.Hash]bool{}
	for _, h := range a {
		if seen[h] {
			t.Fatalf("duplicate member hash %s", h.Short())
		}
		seen[h] = true
	}
	// The fibrant and quotient groups must not collide either.
	s := New()
	qs := s.AddQuot()
	fs := s.AddFib()
	for _, q := range qs {
		for _, f := range fs {
			if q == f {
				t.Fatal("quotient and fibrant member hashes collide")
			}
		}
	}
}

func TestFibRolesAndBarrier(t *testing.T) {
	s := New()
	hs := s.AddFib()
	want := []core.FibRole{
		core.FRoleUF, core.FRoleEl, core.FRoleFib, core.FRolePiF,
		core.FRolePathF, core.FRolePrefl, core.FRoleJ,
		core.FRolePathU, core.FRoleUrefl, core.FRoleUa, core.FRoleCastU,
	}
	for i, h := range hs {
		if got := s.FibRoleOf(h); got != want[i] {
			t.Fatalf("member %d (%s): role %v, want %v", i, FibNames()[i], got, want[i])
		}
		ty, ok := s.TypeOf(h)
		if !ok || ty == nil {
			t.Fatalf("member %s has no stored type", FibNames()[i])
		}
		if _, ok := s.Unfold(h); ok {
			t.Fatalf("member %s has a body; builtins must be bodiless", FibNames()[i])
		}
		n, ok := s.Lookup(FibNames()[i])
		if !ok || n != h {
			t.Fatalf("name %s not bound to its member hash", FibNames()[i])
		}
	}
	if s.FibRoleOf(core.Hash{}) != core.FRoleNone {
		t.Fatal("unknown hash should have no fibrant role")
	}
}

func TestFibTypesMentionNoPlaceholders(t *testing.T) {
	s := New()
	hs := s.AddFib()
	for i, h := range hs {
		ty, _ := s.TypeOf(h)
		for j := 0; j < 11; j++ {
			if mentionsRef(ty, Placeholder(j)) {
				t.Fatalf("member %s: stored type still mentions Placeholder(%d)", FibNames()[i], j)
			}
		}
	}
}
