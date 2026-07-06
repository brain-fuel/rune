package session

import (
	"testing"

	"goforge.dev/rune/v3/internal/prelude"
)

// TestTowerClassHashesDistinct is the collision audit the positional
// Sigma-record discipline demands: every class former in the tower must
// hash distinctly, or instance registration silently merges two classes
// onto one key (the Add/Mul trap the old Num comment documents).
func TestTowerClassHashesDistinct(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	names := []string{"Semiring", "Ring", "DivRing", "Monoid", "Div", "NegR", "SubR", "Binary", "Show"}
	seen := map[string]string{}
	for _, n := range names {
		h, ok := s.Lookup(n)
		if !ok {
			t.Fatalf("class %s not found in prelude", n)
		}
		key := h.String()
		if prev, dup := seen[key]; dup {
			t.Fatalf("hash collision: %s and %s share %s", prev, n, key)
		}
		seen[key] = n
	}
}
