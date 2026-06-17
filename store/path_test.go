package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

// The cubical path builtin group (§F phase 2): two members built against the
// fibrant and interval groups, content-addressed, with stable hashes.

func TestAddPathDeterministicAndDistinct(t *testing.T) {
	mk := func() [2]core.Hash {
		s := New()
		f := s.AddFib()
		i := s.AddInterval()
		return s.AddPath(f, i)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("path group hashes differ across stores:\n%v\n%v", a, b)
	}
	if a[0] == a[1] {
		t.Fatal("path members share a hash")
	}
	// No collision with the other builtin groups.
	s := New()
	fs := s.AddFib()
	is := s.AddInterval()
	qs := s.AddQuot()
	ps := s.AddPath(fs, is)
	for _, p := range ps {
		for _, other := range append(append(append([]core.Hash{}, fs[:]...), is[:]...), qs[:]...) {
			if p == other {
				t.Fatal("path member collides with another builtin group")
			}
		}
	}
}

func TestPathRoles(t *testing.T) {
	s := New()
	f := s.AddFib()
	i := s.AddInterval()
	hs := s.AddPath(f, i)
	if s.PathRoleOf(hs[0]) != core.PRoleAbs {
		t.Error("member 0 is not pabs")
	}
	if s.PathRoleOf(hs[1]) != core.PRoleApp {
		t.Error("member 1 is not papp")
	}
	if s.PathRoleOf(core.Hash{}) != core.PRoleNone {
		t.Error("zero hash classified as a path member")
	}
	if (New()).PathRoleOf(hs[0]) != core.PRoleNone {
		t.Error("unregistered store classified a path member")
	}
}

func TestPathHashRoundTrip(t *testing.T) {
	s := New()
	f := s.AddFib()
	iv := s.AddInterval()
	hs := s.AddPath(f, iv)
	for k, role := range []core.PathRole{core.PRoleAbs, core.PRoleApp} {
		if got := s.PathRoleOf(hs[k]); got != role {
			t.Errorf("member %d role = %v, want %v", k, got, role)
		}
		if rh, ok := s.PathHash(role); !ok || rh != hs[k] {
			t.Errorf("PathHash(%v) did not round-trip to member %d", role, k)
		}
	}
	if _, ok := s.PathHash(core.PRoleNone); ok {
		t.Error("PathHash(PRoleNone) should not resolve")
	}
}
