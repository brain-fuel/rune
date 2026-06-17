package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// The interval builtin group (§F phase 1): six bodiless members, content-
// addressed as a unit, on a hash space disjoint from the other builtin groups,
// with the roles the De Morgan ι-rules consult.

func TestAddIntervalDeterministicAndDistinct(t *testing.T) {
	a := New().AddInterval()
	b := New().AddInterval()
	if a != b {
		t.Fatalf("interval group hashes differ across stores:\n%v\n%v", a, b)
	}
	seen := map[core.Hash]bool{}
	for _, h := range a {
		if seen[h] {
			t.Fatalf("duplicate member hash %s", h.Short())
		}
		seen[h] = true
	}
	// The interval must not collide with the quotient or fibrant groups — its
	// own hash-space prefix ('I'/'i') keeps ch09–ch10 hashes untouched.
	s := New()
	qs := s.AddQuot()
	fs := s.AddFib()
	is := s.AddInterval()
	for _, i := range is {
		for _, q := range qs {
			if i == q {
				t.Fatal("interval and quotient member hashes collide")
			}
		}
		for _, f := range fs {
			if i == f {
				t.Fatal("interval and fibrant member hashes collide")
			}
		}
	}
}

func TestIntervalRolesAndHashLookup(t *testing.T) {
	s := New()
	hs := s.AddInterval()
	want := []core.IntervalRole{
		core.IRoleI, core.IRoleI0, core.IRoleI1,
		core.IRoleNeg, core.IRoleMin, core.IRoleMax,
	}
	for i, h := range hs {
		if got := s.IntervalRoleOf(h); got != want[i] {
			t.Errorf("member %d role = %v, want %v", i, got, want[i])
		}
		// Reverse lookup round-trips.
		if rh, ok := s.IntervalHash(want[i]); !ok || rh != h {
			t.Errorf("IntervalHash(%v) did not round-trip to member %d", want[i], i)
		}
	}
	// A non-member hash has no role.
	if s.IntervalRoleOf(core.Hash{}) != core.IRoleNone {
		t.Error("zero hash classified as an interval member")
	}
	// An unregistered store reports nothing.
	if (New()).IntervalRoleOf(hs[0]) != core.IRoleNone {
		t.Error("unregistered store classified an interval member")
	}
}
