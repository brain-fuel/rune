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

// TestWholeSemiringLawsPresent pins that the prelude PROVES Whole's
// semiring laws (the proven tier of the v4 hierarchy): the laws value
// must exist and elaborate against the laws record over semiringWhole.
func TestWholeSemiringLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"SemiringLaws", "semiringLawsWhole", "addWAssoc", "addWComm", "mulWAssoc", "distribWL"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestLawRecordTypesPresent pins that the prelude carries the next rung of
// the v4 law hierarchy: the RingLaws/DivRingLaws/CommLaws record formers and
// Whole's commutativity instance (mulWComm packaged as CommLaws).
func TestLawRecordTypesPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"RingLaws", "DivRingLaws", "CommLaws", "commLawsWhole"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}
