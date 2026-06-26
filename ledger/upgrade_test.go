package ledger

import "testing"

func TestUpgradesDetectsPostulateToProven(t *testing.T) {
	// same proposition hash, postulate -> proven
	prop := mkHash(1)
	base := []Entry{{Name: "inRegion", Tier: Postulate, PropHash: prop}}
	cur := []Entry{{Name: "inRegion", Tier: Proven, PropHash: prop, ProofHash: mkHash(2)}}
	ups := Upgrades(base, cur)
	if len(ups) != 1 {
		t.Fatalf("want 1 upgrade, got %d", len(ups))
	}
	if ups[0].From != Postulate || ups[0].To != Proven {
		t.Fatalf("want postulate->proven, got %v->%v", ups[0].From, ups[0].To)
	}
}

func TestUpgradesIgnoresUnchanged(t *testing.T) {
	prop := mkHash(1)
	base := []Entry{{Name: "x", Tier: Proven, PropHash: prop}}
	cur := []Entry{{Name: "x", Tier: Proven, PropHash: prop}}
	if ups := Upgrades(base, cur); len(ups) != 0 {
		t.Fatalf("unchanged tier must not be an upgrade, got %d", len(ups))
	}
}
