package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestAddSigmaDeterministicAndRole(t *testing.T) {
	mk := func() [4]core.Hash {
		s := New()
		f := s.AddFib()
		return s.AddSigma(f)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("sigma group hashes differ across stores:\n%v\n%v", a, b)
	}
	s := New()
	f := s.AddFib()
	hs := s.AddSigma(f)
	want := []core.SigmaRole{core.GRoleSigma, core.GRolePair, core.GRoleFst, core.GRoleSnd}
	for k, role := range want {
		if got := s.SigmaRoleOf(hs[k]); got != role {
			t.Errorf("member %d role = %v, want %v", k, got, role)
		}
		if rh, ok := s.SigmaHash(role); !ok || rh != hs[k] {
			t.Errorf("SigmaHash(%v) did not round-trip to member %d", role, k)
		}
	}
	if s.SigmaRoleOf(core.Hash{}) != core.GRoleNone {
		t.Error("zero hash classified as a sigma member")
	}
	// No collision with the fibrant group.
	for _, g := range hs {
		for _, fh := range f {
			if g == fh {
				t.Fatal("sigma member collides with a fibrant member")
			}
		}
	}
}
