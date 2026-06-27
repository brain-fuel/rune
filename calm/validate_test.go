// calm/validate_test.go
package calm

import (
	"testing"
)

func TestValidateRoundTripClean(t *testing.T) {
	m := demoModel(t)
	if errs := Validate(m.ToDoc(), m); len(errs) != 0 {
		t.Fatalf("the emitted document must validate against its own source, got %v", errs)
	}
}

func TestValidateMissingNode(t *testing.T) {
	m := demoModel(t)
	d := m.ToDoc()
	d.Nodes = d.Nodes[1:] // drop a node
	errs := Validate(d, m)
	if len(errs) == 0 {
		t.Fatalf("a missing node must fail validation")
	}
}

func TestValidateTamperedProposition(t *testing.T) {
	m := demoModel(t)
	d := m.ToDoc()
	// tamper the in-region control's proposition hash on the store node
	for i := range d.Nodes {
		if d.Nodes[i].UniqueID == "store" {
			cb := d.Nodes[i].Controls["in-region"]
			cb.Requirements[0].Config.Proposition = "deadbeef"
			d.Nodes[i].Controls["in-region"] = cb
		}
	}
	errs := Validate(d, m)
	if len(errs) == 0 {
		t.Fatalf("a tampered proposition hash must fail validation (the macro-micro tie)")
	}
}

func TestValidateTierDowngrade(t *testing.T) {
	m := demoModel(t)
	d := m.ToDoc()
	for i := range d.Nodes {
		if d.Nodes[i].UniqueID == "relay" {
			cb := d.Nodes[i].Controls["least-privilege-iam"]
			cb.Requirements[0].Config.Tier = "postulate" // downgrade proven -> postulate
			d.Nodes[i].Controls["least-privilege-iam"] = cb
		}
	}
	errs := Validate(d, m)
	if len(errs) == 0 {
		t.Fatalf("a tier downgrade must fail validation")
	}
}
