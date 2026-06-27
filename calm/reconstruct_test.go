// calm/reconstruct_test.go
package calm

import (
	"reflect"
	"testing"

	"goforge.dev/rune/v3/control"
	"goforge.dev/rune/v3/infra"
	"goforge.dev/rune/v3/ledger"
)

func demoModel(t *testing.T) Model {
	t.Helper()
	rs := []infra.Resource{infra.PaaS{Name: "web"}, infra.Compute{Name: "relay"}, infra.KV{Name: "store"}}
	catalog := []control.Control{
		{Name: "inRegionProof", Kind: "in-region", Element: "store"},
		{Name: "encryptedProof", Kind: "encrypted-in-transit", Element: "web->relay"},
		{Name: "leastPrivProof", Kind: "least-privilege-iam", Element: "relay"},
		{Name: "convergesProof", Kind: "convergence", Element: "store"},
		{Name: "liveInRegion", Kind: "tail", Element: "store"},
	}
	entries := []ledger.Entry{
		{Name: "inRegionProof", Tier: ledger.Proven},
		{Name: "encryptedProof", Tier: ledger.Proven},
		{Name: "leastPrivProof", Tier: ledger.Proven},
		{Name: "convergesProof", Tier: ledger.Proven},
		{Name: "liveInRegion", Tier: ledger.Postulate, Why: "not yet modeled"},
	}
	m, err := BuildModel(rs, catalog, entries)
	if err != nil {
		t.Fatalf("BuildModel: %v", err)
	}
	return m
}

func TestModelDocReconstruct(t *testing.T) {
	m := demoModel(t)
	got := Reconstruct(m.ToDoc())
	if !reflect.DeepEqual(m, got) {
		t.Fatalf("model did not round-trip through Doc:\nwant %#v\ngot  %#v", m, got)
	}
}

func TestToDocShape(t *testing.T) {
	d := demoModel(t).ToDoc()
	if len(d.Nodes) != 3 || len(d.Relationships) != 1 {
		t.Fatalf("want 3 nodes + 1 relationship, got %d nodes %d rels", len(d.Nodes), len(d.Relationships))
	}
	// the tail control is postulate-tier and carries its reason
	var store Node
	for _, n := range d.Nodes {
		if n.UniqueID == "store" {
			store = n
		}
	}
	cb, ok := store.Controls["tail"]
	if !ok || cb.Requirements[0].Config.Tier != "postulate" || cb.Requirements[0].Config.Why != "not yet modeled" {
		t.Fatalf("store tail control should be postulate with a reason, got %+v", store.Controls["tail"])
	}
}
