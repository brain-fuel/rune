package ledger

import "testing"

func TestGateRejectsUnapprovedPostulate(t *testing.T) {
	cur := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: mkHash(1)}}
	errs := Gate(cur, nil, GateConfig{})
	if len(errs) != 1 {
		t.Fatalf("an unapproved postulate must fail the gate, got %d errors", len(errs))
	}
}

func TestGateAllowsApprovedPostulate(t *testing.T) {
	cur := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: mkHash(1)}}
	errs := Gate(cur, nil, GateConfig{AllowedPostulates: []string{"inRegion"}})
	if len(errs) != 0 {
		t.Fatalf("an approved postulate must pass, got %v", errs)
	}
}

func TestGateRejectsFlagshipLeavingProven(t *testing.T) {
	cur := []Entry{{Name: "iamLeastPriv", Tier: Guard, PropHash: mkHash(2)}}
	errs := Gate(cur, nil, GateConfig{Flagships: []string{"iamLeastPriv"}})
	if len(errs) != 1 {
		t.Fatalf("a flagship not proven must fail, got %d", len(errs))
	}
}

func TestGatePassesProvenFlagship(t *testing.T) {
	cur := []Entry{{Name: "iamLeastPriv", Tier: Proven, PropHash: mkHash(2)}}
	errs := Gate(cur, nil, GateConfig{Flagships: []string{"iamLeastPriv"}})
	if len(errs) != 0 {
		t.Fatalf("a proven flagship must pass, got %v", errs)
	}
}
