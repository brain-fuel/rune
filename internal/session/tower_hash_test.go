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
	names := []string{"Semiring", "Ring", "DivRing", "Magma", "Monoid", "Group", "Div", "NegR", "SubR", "Binary", "Show", "DecEq", "Ord", "Ordering"}
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

// TestOrdScaffoldPresent pins Task 1 of the v4 Ord campaign: the Ordering
// type, the DecEq and Ord ops classes with their accessors, the erased Le
// proof-view, the two comparison defaults, and the DecEqLaws/OrdLaws law
// records. The prelude must resolve every one (elaboration is the proof
// check, so their presence means they are well-formed).
func TestOrdScaffoldPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"Ordering", "flipOrd",
		"DecEq", "mkDecEq", "eqbOf",
		"Ord", "mkOrd", "leOf", "compareOf", "Le",
		"compareFromLe", "leFromCompare",
		"DecEqLaws", "OrdLaws",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestWholeOrdInstancesPresent pins that the prelude PROVES Whole's decidable
// equality and total order: the DecEq/Ord instances plus their laws records
// (soundness/reflexivity for DecEq; the six total-order slots for Ord), all
// built by reusing the existing leb/eqW lemma corpus. Elaboration is the proof
// check, so their presence means the total order over Whole holds.
func TestWholeOrdInstancesPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"decEqWhole", "ordWhole", "decEqLawsWhole", "ordLawsWhole",
		"compareFromLeEq", "compareFromLeLt",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestIntOrdInstancesPresent pins Task 3 of the v4 Ord campaign: the Int
// comparison primitives (ileb/ieqb) plus the DecEq/Ord instances and their
// laws records, all built by constructor-case analysis reducing to the Whole
// leb/eqW corpus. Elaboration is the proof check, so their presence means the
// total order over the signed integers holds.
func TestIntOrdInstancesPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"ileb", "ieqb", "decEqInt", "ordInt", "decEqLawsInt", "ordLawsInt",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestFracOrdInstancesPresent pins Task 4 of the v4 Ord campaign: the Frac
// comparison lifted out of the quotient (leF/eqF), its positive-scaling
// respect proof (ilebMulPosR + leRespL/leRespR), the DecEq/Ord instances,
// and their laws records (the six total-order slots + DecEq soundness). The
// substantial content is ilebMulPosR (Int positive-scaling order
// monotonicity, NOT cancellation) and leAntisym via qsound. Elaboration is
// the proof check, so their presence means the total order over Frac holds.
func TestFracOrdInstancesPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"ilebMulPosR", "lebMulPosR", "leRespL", "leRespR",
		"leF", "eqF", "decEqFrac", "ordFrac", "decEqLawsFrac", "ordLawsFrac",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestOrderedAlgebraRecordsPresent pins Task 1 of the v4 Ord campaign Plan B
// (ordered-algebra bridge): the two order-compat statement formers (AddMonoT =
// addition monotone on the right; MulNonnegT = product of nonnegatives is
// nonnegative) and the three ordered-algebra law records (OrderedSemiringLaws,
// OrderedRingLaws, OrderedFieldLaws). Elaboration is the proof check, so their
// presence means the records project the Semiring/Ring/DivRing ops at the
// intended depths and are well-formed.
func TestOrderedAlgebraRecordsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"AddMonoT", "MulNonnegT",
		"OrderedSemiringLaws", "OrderedRingLaws", "OrderedFieldLaws",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestWholeOrderedSemiringPresent pins Task 2 of the v4 Ord campaign Plan B:
// Whole's ordered-semiring instance. addMonoWhole proves addition is monotone
// on the right (from lebAddMono + addWComm to reach add-on-right); mulNonnegWhole
// is definitional (leb zero _ reduces to true, so Whole nonnegativity is
// universal); orderedSemiringLawsWhole assembles them with semiringLawsWhole +
// ordLawsWhole into the OrderedSemiringLaws record. Elaboration is the proof.
func TestWholeOrderedSemiringPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"addMonoWhole", "mulNonnegWhole", "orderedSemiringLawsWhole",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
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

// TestMonusLibraryPresent pins the monus/order library + wsubEq keystone
// (Task 2 of the Int ring-laws campaign): the completeness of leb, the monus
// cancellation lemma, left cancellation of addW, and wsubEq (wsub respects
// cross-equal differences). These carry the Int ring laws proved downstream.
func TestMonusLibraryPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"lebComplete", "subWAddCancel", "addWCancelL", "wsubEq"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestIntSpecLemmasPresent pins the difference-pair transport spec lemmas
// (Task 3 of the Int ring-laws campaign): every Int op rewritten as a wsub of
// Whole polynomials. wsubRep represents an Int as wsub of its parts;
// wsubPosNeg is the cross-equality bridge; iadd/imul/inegSpec transport the
// ops; iaddWsub/imulWsub push a wsub through the ops (the ring-law engine).
func TestIntSpecLemmasPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"wsubRep", "wsubPosNeg", "iaddSpec", "imulSpec", "inegSpec", "iaddWsub", "imulWsub"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestIntLawsPresent pins the summit of the Int ring-laws campaign (Task 4):
// the assembled RingLaws/CommLaws/CommRingLaws records over ringInt and the
// AbGroupLaws value cashed through the 1b bridge (abGroupLawsOfRing), plus a
// representative of the thirteen underlying lemmas (additive associativity,
// multiplicative commutativity, left distributivity).
func TestIntLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"ringLawsInt", "commLawsInt", "commRingLawsInt", "abGroupLawsInt", "iaddAssoc", "imulComm", "idistribL"} {
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

// TestFracQuotientPresent pins the Frac quotient re-foundation (Frac field-laws
// campaign, Plan A / Task 1): Frac is Quot QPair QRel with unreduced qlift ops
// (respect proofs closed by Int ring algebra), the derived sub/div family, and
// the computational reduceQ (the Go display fold's rune-side mirror).
func TestFracQuotientPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"QPair", "QRel", "Frac", "fracOf", "addF", "mulF", "recipF", "divF", "reduceQ"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestToRadixPipelinePresent pins the Task-2 re-foundation: the class-invariant
// observers ported from ch203 (toRadixRep + toRadixRespects, the decimal
// expansion's QRel-invariance; floorRep + floorUnique, the floor's) and their
// qlifts onto the quotient Frac (to_radix, floorQ), plus the re-lifted rounding
// family and the rebuilt demotion trio (toWhole/toInt/toNat over the observers).
func TestToRadixPipelinePresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"toRadixRep", "toRadixRespects", "to_radix", "floorRep", "floorUnique", "floorQ",
		"to_radix_sigplace", "to_radix_sigfig", "toWhole", "toInt", "toNat",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestFracNzPresent pins Task 1 of the Frac field-laws campaign (Plan B): the
// nonzero predicate nzFrac, a Bool zero-test lifted out of the quotient. The
// lift's respect proof rides the canonical-Int zero-product lemma imulPosZero
// (the one sanctioned cancellation-adjacent fact, scoped to a positive right
// factor), which itself rides nonnegInj and the constructor discrimination.
func TestFracNzPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{"isZeroInt", "nonnegInj", "imulPosZero", "isZeroResp", "isZeroF", "nzFrac"} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestFracAddLawsPresent pins Task 2 of the Frac field-laws campaign (Plan B):
// the four ADDITIVE laws over the quotient Frac. Each is proven at the
// representative level (a QRel cross-multiplication identity over canonical
// Int, closed by the ringLawsInt ingredient lemmas) and lifted to Frac by
// qind + qsound.
func TestFracAddLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"addAssocP", "addCommP", "addZeroLP", "addZeroRP",
		"addFAssoc", "addFComm", "addFZeroL", "addFZeroR",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestFracMulLawsPresent pins Task 3 of the Frac field-laws campaign (Plan B):
// the eight MULTIPLICATIVE/distributive/annihilation laws over the quotient
// Frac. Each is proven at the representative level (a QRel cross-multiplication
// identity over canonical Int, closed by the ringLawsInt ingredient lemmas plus
// imulInterchange) and lifted to Frac by qind + qsound.
func TestFracMulLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"mulAssocP", "mulCommP", "mulOneLP", "mulOneRP",
		"distribLP", "distribRP", "mulZeroLP", "mulZeroRP",
		"mulFAssoc", "mulFComm", "mulFOneL", "mulFOneR",
		"distribFL", "distribFR", "mulFZeroL", "mulFZeroR",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestFracRingLawsPresent pins Task 4 of the Frac field-laws campaign (Plan B):
// the two negation-inverse laws, the five ring-level law-record assemblies
// (mirroring the Int assembly nesting over divRingFrac's projections), and the
// four tower-graph frac-edge homomorphisms (fracOf / ratOfInt preserve + and *,
// each a single qsound over denominator-1 representatives).
func TestFracRingLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"negInvLP", "negInvRP", "fnegInvL", "fnegInvR",
		"semiringLawsFrac", "ringLawsFrac", "commLawsFrac",
		"commRingLawsFrac", "abGroupLawsFrac",
		"fracOfAddHom", "fracOfMulHom", "ratOfIntAddHom", "ratOfIntMulHom",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}

// TestFracFieldLawsPresent pins Task 5 of the Frac field-laws campaign (Plan B):
// the guarded recip inverse laws and the FieldLaws bundle's first inhabitant.
// recipInvLP/RP are the campaign's one genuinely new proof - ch113/ch116 only
// closed the positive subtype; canonical Int closes negsucc too (its two-negative
// numerator computes to the SAME nonneg product as the positive case). They lift
// to recipFInvL/R (guarded by nzFrac) and assemble into divRingLawsFrac and
// fieldLawsFrac.
func TestFracFieldLawsPresent(t *testing.T) {
	s := New()
	if _, err := s.LoadSource(prelude.Source()); err != nil {
		t.Fatalf("loading prelude: %v", err)
	}
	for _, n := range []string{
		"recipInvLP", "recipInvRP", "recipFInvL", "recipFInvR",
		"divRingLawsFrac", "fieldLawsFrac",
	} {
		if _, ok := s.Lookup(n); !ok {
			t.Fatalf("%s not found in prelude", n)
		}
	}
}
