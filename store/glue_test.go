package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestAddEquivDeterministic(t *testing.T) {
	mk := func() [6]core.Hash {
		s := New()
		f := s.AddFib()
		g := s.AddSigma(f)
		return s.AddEquiv(f, g)
	}
	if a, b := mk(), mk(); a != b {
		t.Fatalf("equiv group hashes differ across stores:\n%v\n%v", a, b)
	}
}

func TestAddGlueDeterministicAndRole(t *testing.T) {
	mk := func() [4]core.Hash {
		s := New()
		f := s.AddFib()
		_ = s.AddInterval()
		c := s.AddFace(intervalHashesOf(s))
		sy := s.AddSystems(c)
		sg := s.AddSigma(f)
		ev := s.AddEquiv(f, sg)
		return s.AddGlue(f, c, sy, ev)
	}
	a, b := mk(), mk()
	if a != b {
		t.Fatalf("glue group hashes differ across stores:\n%v\n%v", a, b)
	}

	s := New()
	f := s.AddFib()
	_ = s.AddInterval()
	c := s.AddFace(intervalHashesOf(s))
	sy := s.AddSystems(c)
	sg := s.AddSigma(f)
	ev := s.AddEquiv(f, sg)
	hs := s.AddGlue(f, c, sy, ev)

	want := []core.GlueRole{core.URoleGlue, core.URoleGlueIn, core.URoleUnglue}
	for k, role := range want {
		if got := s.GlueRoleOf(hs[k]); got != role {
			t.Errorf("member %d role = %v, want %v", k, got, role)
		}
		if rh, ok := s.GlueHash(role); !ok || rh != hs[k] {
			t.Errorf("GlueHash(%v) did not round-trip to member %d", role, k)
		}
	}
	if s.GlueRoleOf(core.Hash{}) != core.URoleNone {
		t.Error("zero hash classified as a glue member")
	}
	// EquivFunHash points at the equiv group's forward-map accessor (member 1).
	if efh, ok := s.EquivFunHash(); !ok || efh != ev[4] {
		t.Error("EquivFunHash did not return the equiv group's equivFun")
	}
	// No collision with the equiv group or fibrant group.
	for _, g := range hs {
		for _, eh := range ev {
			if g == eh {
				t.Fatal("glue member collides with an equiv member")
			}
		}
		for _, fh := range f {
			if g == fh {
				t.Fatal("glue member collides with a fibrant member")
			}
		}
	}
}

// intervalHashesOf reaches the registered interval group's hashes (AddFace needs
// them); the interval group is registered ambiently by the callers above.
func intervalHashesOf(s *Store) [6]core.Hash {
	hs, _ := s.IntervalHashes()
	return hs
}
