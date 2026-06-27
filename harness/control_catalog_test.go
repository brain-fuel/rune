package harness

import (
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

// TestControlCatalogElaborates pins that the catalog listing loads and that the
// in-region control's fold computes to true (so its refl is a real proof).
func TestControlCatalogElaborates(t *testing.T) {
	s := loadListing(t, "ch538_control_catalog.rune")
	normalizesTo(t, s, `allInRegion usEast demoServices`, "true")
	normalizesTo(t, s, `allEncrypted demoEdges`, "true")
	normalizesTo(t, s, `eqCodes granted needed`, "true")
	// either merge order converges to max(3, 7) = 7
	normalizesTo(t, s, `val (merge r3 r7)`, "7")
	normalizesTo(t, s, `val (merge r7 r3)`, "7")
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
