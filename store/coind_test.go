package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestAddCoindDeterministicAndRole(t *testing.T) {
	mk := func() [4]core.Hash {
		s := New()
		f := s.AddFib()
		return s.AddCoind(f)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("coind group hashes differ across stores:\n%v\n%v", a, b)
	}
	s := New()
	f := s.AddFib()
	hs := s.AddCoind(f)
	want := []core.CoindRole{core.NRoleNu, core.NRoleOut, core.NRoleUnfold, core.NRoleNuCons}
	for k, role := range want {
		if got := s.CoindRoleOf(hs[k]); got != role {
			t.Errorf("member %d role = %v, want %v", k, got, role)
		}
		if rh, ok := s.CoindHash(role); !ok || rh != hs[k] {
			t.Errorf("CoindHash(%v) did not round-trip to member %d", role, k)
		}
	}
	if s.CoindRoleOf(core.Hash{}) != core.NRoleNone {
		t.Error("zero hash classified as a coind member")
	}
}
