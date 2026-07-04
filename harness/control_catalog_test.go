package harness

import (
	"testing"

	"goforge.dev/rune/v3/control"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/ledger"
)

// TestControlCatalogElaborates pins that the catalog listing loads and that the
// in-region control's fold computes to true (so its refl is a real proof).
func TestControlCatalogElaborates(t *testing.T) {
	s := loadListing(t, "ch538_control_catalog.rune")
	normalizesTo(t, s, `allInRegion usEast demoServices`, "true")
	normalizesTo(t, s, `allEncrypted demoEdges`, "true")
	normalizesTo(t, s, `eqCodes granted needed`, "true")
	// either merge order converges: replica A counted 2, replica B counted 1
	const three = "succ (succ (succ zero))"
	normalizesTo(t, s, `value (merge replicaA replicaB)`, three)
	normalizesTo(t, s, `value (merge replicaB replicaA)`, three)
}

// findEntry returns the ledger entry for a control name.
func findEntry(es []ledger.Entry, name string) (ledger.Entry, bool) {
	for _, e := range es {
		if e.Name == name {
			return e, true
		}
	}
	return ledger.Entry{}, false
}

// TestCatalogLedgerTiers: the four flagships are proven, the tail is postulate.
func TestCatalogLedgerTiers(t *testing.T) {
	s := loadListing(t, "ch538_control_catalog.rune")
	es := ledger.Build(s)
	for _, name := range control.Flagships() {
		e, ok := findEntry(es, name)
		if !ok {
			t.Fatalf("flagship %q absent from the ledger", name)
		}
		if e.Tier != ledger.Proven {
			t.Fatalf("flagship %q want proven, got %v", name, e.Tier)
		}
	}
	for _, name := range control.AllowedPostulates() {
		e, ok := findEntry(es, name)
		if !ok {
			t.Fatalf("tail control %q absent from the ledger", name)
		}
		if e.Tier != ledger.Postulate {
			t.Fatalf("tail control %q want postulate, got %v", name, e.Tier)
		}
	}
}

// TestCatalogGatePassesAndFails: the catalog passes the assurance gate; demoting a
// flagship to a postulate (the over-broad / unproven case) fails it.
func TestCatalogGatePassesAndFails(t *testing.T) {
	cfg := ledger.GateConfig{
		Flagships:         control.Flagships(),
		AllowedPostulates: control.AllowedPostulates(),
	}

	s := loadListing(t, "ch538_control_catalog.rune")
	es := ledger.Build(s)
	if errs := ledger.Gate(es, nil, cfg); len(errs) != 0 {
		t.Fatalf("the catalog must pass its own gate, got %v", errs)
	}

	// Demote a flagship: replace convergesProof's proof with a postulate of the
	// same proposition (a flagship leaving `proven`). The gate must fail.
	const demoted = `
data Unit : U is unit : Unit end
data Bool : U is false : Bool | true : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Reg : U is reg : Nat -> Reg end
merge : Reg -> Reg -> Reg is fn (x : Reg) (y : Reg) is x end end
data Conv : (R : U) -> (m : R -> R -> R) -> U is
  conv : (R : U) -> (m : R -> R -> R)
      -> ((x : R) -> (y : R) -> Eq R (m x y) (m y x))
      -> ((x : R) -> Eq R (m x x) x)
      -> ((x : R) -> (y : R) -> (z : R) -> Eq R (m (m x y) z) (m x (m y z)))
      -> Conv R m
end
postulate convergesProof : Conv Reg merge because "demoted for the gate test" end
`
	ds := session.New()
	if _, err := ds.LoadSource(demoted); err != nil {
		t.Fatalf("demoted source must still load (it is well-typed): %v", err)
	}
	des := ledger.Build(ds)
	if errs := ledger.Gate(des, nil, cfg); len(errs) == 0 {
		t.Fatalf("a flagship left at postulate must fail the gate")
	}
}

// TestOverBroadIAMRejected proves the least-privilege control has teeth: an
// over-broad granted policy (an extra grant code 99 the workload never uses)
// makes eqCodes granted needed reduce to false, so a refl proof of
// `Eq Bool (eqCodes granted needed) true` does NOT type-check. The over-broad
// policy is REJECTED at elaboration - a compile error, not a runtime check.
func TestOverBroadIAMRejected(t *testing.T) {
	const src = `
data Unit : U is unit : Unit end
data Bool : U is false : Bool | true : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
eqNat : Nat -> Nat -> Bool is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat -> Bool end)
      (fn (m : Nat) is NatElim (fn (y : Nat) is Bool end) true (fn (j : Nat) (ih : Bool) is false end) m end)
      (fn (k : Nat) (eqK : Nat -> Bool) is fn (m : Nat) is NatElim (fn (y : Nat) is Bool end) false (fn (j : Nat) (ih : Bool) is eqK j end) m end end)
      a b
  end
end
andB : Bool -> Bool -> Bool is fn (a : Bool) (b : Bool) is BoolElim (fn (w : Bool) is Bool end) false b a end end
data CodeList : U is cnil : CodeList | ccons : Nat -> CodeList -> CodeList end
eqCodes : CodeList -> CodeList -> Bool is
  fn (xs : CodeList) is
    CodeListElim (fn (w : CodeList) is CodeList -> Bool end)
      (fn (ys : CodeList) is CodeListElim (fn (w : CodeList) is Bool end) true (fn (h : Nat) (t : CodeList) (ih : Bool) is false end) ys end)
      (fn (x : Nat) (xt : CodeList) (eqXt : CodeList -> Bool) is fn (ys : CodeList) is CodeListElim (fn (w : CodeList) is Bool end) false (fn (y : Nat) (yt : CodeList) (ih : Bool) is andB (eqNat x y) (eqXt yt) end) ys end end)
      xs
  end
end
needed  : CodeList is ccons 10 (ccons 11 cnil) end
granted : CodeList is ccons 10 (ccons 11 (ccons 99 cnil)) end
leastPrivProof : Eq Bool (eqCodes granted needed) true is refl end
`
	s := session.New()
	if _, err := s.LoadSource(src); err == nil {
		t.Fatalf("an over-broad IAM policy must be rejected: eqCodes reduces to false, so the refl proof should not type-check")
	}
}
