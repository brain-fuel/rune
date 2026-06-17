package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestAddSystemsDeterministicAndDistinct(t *testing.T) {
	mk := func() [5]core.Hash {
		s := New()
		i := s.AddInterval()
		c := s.AddFace(i)
		return s.AddSystems(c)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("systems group hashes differ across stores:\n%v\n%v", a, b)
	}
	seen := map[core.Hash]bool{}
	for _, h := range a {
		if seen[h] {
			t.Fatalf("duplicate member hash %s", h.Short())
		}
		seen[h] = true
	}
}

func TestSysRoles(t *testing.T) {
	s := New()
	i := s.AddInterval()
	c := s.AddFace(i)
	hs := s.AddSystems(c)
	want := []core.SysRole{core.SRoleHolds, core.SRoleTop, core.SRoleAnd, core.SRoleOrL, core.SRoleOrR}
	for k, h := range hs {
		if got := s.SysRoleOf(h); got != want[k] {
			t.Errorf("member %d role = %v, want %v", k, got, want[k])
		}
	}
	if s.SysRoleOf(core.Hash{}) != core.SRoleNone {
		t.Error("zero hash classified as a systems member")
	}
}
