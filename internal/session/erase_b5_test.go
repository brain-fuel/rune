package session

import (
	"testing"
)

// B5 / R-ERASE2 (deploy-ban slice): a definition whose SOURCE uses inner cubical
// operations but whose NORMAL FORM computes them away to an outer value now
// DEPLOYS — its normal form is definitionally equal (NbE) and taint-free, so it
// has a genuine erased runtime meaning. Here `transp (λ_. fib A) x` is a constant
// line, so it reduces to `x` (the identity) — the normalized body is λA x. x.
func TestB5InnerComputesAwayDeploys(t *testing.T) {
	s := New()
	def := mustParse(t, `idTransp : (A : U) -> A -> A is
	   fn (A : U) (x : A) is transp (fn (i : I) is fib A end) x end
	 end`)
	if _, err := s.AddDef(def); err != nil {
		t.Fatalf("idTransp failed to check: %v", err)
	}
	p, err := s.EmitProgram("idTransp")
	if err != nil {
		t.Fatalf("a def whose normal form computes the inner op away must deploy, got: %v", err)
	}
	found := false
	for _, d := range p.Defs {
		if d.Name == "idTransp" {
			found = true
		}
	}
	if !found {
		t.Fatalf("idTransp should appear in the emitted defs (deployed via normal form)")
	}
}

// The ban still holds for genuinely-inner definitions: an inner value whose normal
// form STILL carries inner heads (a postulated ua-path transported) stays tainted
// and does not deploy. (Negative control for the B5 slice — it only ever ADDS
// sound deployments, never lifts the ban for terms with no erased meaning.)
func TestB5GenuinelyInnerStillBanned(t *testing.T) {
	s := New()
	// `loop` is a path constructor with no outer erased meaning; a definition
	// returning it normalizes to `loop` (still inner) — must stay banned.
	def := mustParse(t, `theLoop : El (pathF Circle base base) is loop end`)
	if _, err := s.AddDef(def); err != nil {
		t.Fatalf("theLoop failed to check: %v", err)
	}
	if _, err := s.EmitProgram("theLoop"); err == nil {
		t.Fatalf("a genuinely-inner def (normal form still inner) must NOT deploy")
	}
}
