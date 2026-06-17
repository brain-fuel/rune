package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestAddKanDeterministicAndRole(t *testing.T) {
	mk := func() [4]core.Hash {
		s := New()
		f := s.AddFib()
		i := s.AddInterval()
		c := s.AddFace(i)
		sy := s.AddSystems(c)
		return s.AddKan(f, i, c, sy)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("kan group hashes differ across stores:\n%v\n%v", a, b)
	}
	s := New()
	f := s.AddFib()
	i := s.AddInterval()
	c := s.AddFace(i)
	sy := s.AddSystems(c)
	hs := s.AddKan(f, i, c, sy)
	if s.KanRoleOf(hs[0]) != core.KRoleTransp {
		t.Error("member 0 is not transp")
	}
	if s.KanRoleOf(hs[1]) != core.KRoleHcomp {
		t.Error("member 1 is not hcomp")
	}
	if s.KanRoleOf(hs[2]) != core.KRoleComp {
		t.Error("member 2 is not comp")
	}
	if s.KanRoleOf(hs[3]) != core.KRoleTranspG {
		t.Error("member 3 is not transpG")
	}
	if s.KanRoleOf(core.Hash{}) != core.KRoleNone {
		t.Error("zero hash classified as a kan member")
	}
	// KanHash is the reverse of KanRoleOf.
	for k, role := range []core.KanRole{core.KRoleTransp, core.KRoleHcomp, core.KRoleComp, core.KRoleTranspG} {
		if rh, ok := s.KanHash(role); !ok || rh != hs[k] {
			t.Errorf("KanHash(%v) did not round-trip to member %d", role, k)
		}
	}
}
