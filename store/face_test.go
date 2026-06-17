package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// The face-lattice builtin group (§F phase 3a): seven members built against the
// interval group, content-addressed, with stable hashes and the roles the
// lattice ι-rules consult.

func TestAddFaceDeterministicAndDistinct(t *testing.T) {
	mk := func() [7]core.Hash {
		s := New()
		i := s.AddInterval()
		return s.AddFace(i)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("face group hashes differ across stores:\n%v\n%v", a, b)
	}
	seen := map[core.Hash]bool{}
	for _, h := range a {
		if seen[h] {
			t.Fatalf("duplicate member hash %s", h.Short())
		}
		seen[h] = true
	}
	// No collision with the interval / fibrant / path groups.
	s := New()
	is := s.AddInterval()
	fs := s.AddFib()
	ps := s.AddPath(fs, is)
	cs := s.AddFace(is)
	for _, c := range cs {
		for _, other := range append(append(append([]core.Hash{}, is[:]...), fs[:]...), ps[:]...) {
			if c == other {
				t.Fatal("face member collides with another builtin group")
			}
		}
	}
}

func TestFaceRolesAndHashLookup(t *testing.T) {
	s := New()
	i := s.AddInterval()
	hs := s.AddFace(i)
	want := []core.FaceRole{
		core.CRoleF, core.CRoleEq0, core.CRoleEq1,
		core.CRoleAnd, core.CRoleOr, core.CRoleTop, core.CRoleBot,
	}
	for k, h := range hs {
		if got := s.FaceRoleOf(h); got != want[k] {
			t.Errorf("member %d role = %v, want %v", k, got, want[k])
		}
		if rh, ok := s.FaceHash(want[k]); !ok || rh != h {
			t.Errorf("FaceHash(%v) did not round-trip to member %d", want[k], k)
		}
	}
	if s.FaceRoleOf(core.Hash{}) != core.CRoleNone {
		t.Error("zero hash classified as a face member")
	}
	if (New()).FaceRoleOf(hs[0]) != core.CRoleNone {
		t.Error("unregistered store classified a face member")
	}
}
