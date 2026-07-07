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
	names := []string{"Semiring", "Ring", "DivRing", "Magma", "Monoid", "Group", "Div", "NegR", "SubR", "Binary", "Show"}
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

// TestLowerLadderPresent pins the lower law records (Task 2): SemigroupLaws/
// MonoidLaws/CommMonoidLaws formers and Whole's two commutative-monoid
// positions (under addition and under multiplication).
func TestLowerLadderPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"SemigroupLaws", "MonoidLaws", "CommMonoidLaws", "monoidWholeAdd", "monoidWholeMul", "commMonoidLawsWholeAdd", "commMonoidLawsWholeMul"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestUpperLadderPresent pins the upper law records (Task 3): the Group ops
// shape and the Group/AbGroup/CommRing/Field law-record formers.
func TestUpperLadderPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"Group", "GroupLaws", "AbGroupLaws", "CommRingLaws", "FieldLaws"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestLadderBridgesPresent pins the ladder's edges (Task 4): the bridge
// functions between rungs, the SemiringLaws slot extractors, and the
// proven law transports (a ring IS an abelian group under addition and a
// monoid under multiplication; a semiring carries two monoids).
func TestLadderBridgesPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"groupOfRing", "addMonoidOfSemiring", "addCommMonoidLawsOfSemiring", "mulMonoidLawsOfSemiring", "groupLawsOfRing", "abGroupLawsOfRing"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestTowerGraphLemmasPresent pins the tower conversion graph (Task 5): the
// ratOfInt injection, the intOf homomorphism certificates over iadd/imul
// (now direct refl lemmas under junk-free Int, no normalization lemma
// needed), and the definitional coherence triangle ratOfInt . intOf = fracOf.
func TestTowerGraphLemmasPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"ratOfInt", "intOfAdd", "intOfMul", "ratOfIntOf"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}
