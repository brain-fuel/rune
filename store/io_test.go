package store

import (
	"testing"

	"goforge.dev/rune/v3/core"
)

func TestAddIODeterministicAndDistinct(t *testing.T) {
	a, b := New().AddIO(), New().AddIO()
	if a != b {
		t.Fatalf("io group hashes differ across stores:\n%v\n%v", a, b)
	}
	seen := map[core.Hash]bool{}
	for _, h := range a {
		if seen[h] {
			t.Fatalf("duplicate io member hash %s", h.Short())
		}
		seen[h] = true
	}
	s := New()
	hs := s.AddIO()
	if got, ok := s.IOHashes(); !ok || got != hs {
		t.Fatal("IOHashes did not return the registered group")
	}
	// No collision with the fibrant group.
	f := s.AddFib()
	for _, o := range hs {
		for _, fh := range f {
			if o == fh {
				t.Fatal("io member collides with a fibrant member")
			}
		}
	}
}
