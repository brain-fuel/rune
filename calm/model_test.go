// calm/model_test.go
package calm

import (
	"testing"

	"goforge.dev/rune/v3/control"
	"goforge.dev/rune/v3/infra"
	"goforge.dev/rune/v3/ledger"
)

func TestNodeType(t *testing.T) {
	cases := map[string]string{"kv": "database", "compute": "service", "paas": "webclient", "iam": "actor", "object": "store"}
	for kind, want := range cases {
		if got := NodeType(kind); got != want {
			t.Fatalf("NodeType(%q) = %q, want %q", kind, got, want)
		}
	}
	if got := NodeType("totally-unknown"); got != "service" {
		t.Fatalf("unknown kind should default to service, got %q", got)
	}
}

func TestBuildModel(t *testing.T) {
	rs := []infra.Resource{
		infra.PaaS{Name: "web"},
		infra.Compute{Name: "relay"},
		infra.KV{Name: "store"},
	}
	catalog := []control.Control{
		{Name: "inRegionProof", Kind: "in-region", Element: "store"},
		{Name: "encryptedProof", Kind: "encrypted-in-transit", Element: "web->relay"},
		{Name: "leastPrivProof", Kind: "least-privilege-iam", Element: "relay"},
	}
	entries := []ledger.Entry{
		{Name: "inRegionProof", Tier: ledger.Proven},
		{Name: "encryptedProof", Tier: ledger.Proven},
		{Name: "leastPrivProof", Tier: ledger.Proven},
	}
	m, err := BuildModel(rs, catalog, entries)
	if err != nil {
		t.Fatalf("BuildModel: %v", err)
	}
	if len(m.Nodes) != 3 {
		t.Fatalf("want 3 nodes, got %d", len(m.Nodes))
	}
	// nodes are sorted by ID: relay, store, web
	if m.Nodes[0].ID != "relay" || m.Nodes[1].ID != "store" || m.Nodes[2].ID != "web" {
		t.Fatalf("nodes not sorted by id: %v", []string{m.Nodes[0].ID, m.Nodes[1].ID, m.Nodes[2].ID})
	}
	// store carries the in-region control as a NODE control
	store := m.Nodes[1]
	if len(store.Controls) != 1 || store.Controls[0].Definition != "inRegionProof" {
		t.Fatalf("store should carry inRegionProof, got %+v", store.Controls)
	}
	// relay carries least-priv
	relay := m.Nodes[0]
	if len(relay.Controls) != 1 || relay.Controls[0].Definition != "leastPrivProof" {
		t.Fatalf("relay should carry leastPrivProof, got %+v", relay.Controls)
	}
	// web->relay is a relationship carrying the encrypted control
	if len(m.Rels) != 1 {
		t.Fatalf("want 1 relationship, got %d", len(m.Rels))
	}
	rel := m.Rels[0]
	if rel.ID != "web->relay" || rel.Source != "web" || rel.Dest != "relay" {
		t.Fatalf("relationship wrong: %+v", rel)
	}
	if len(rel.Controls) != 1 || rel.Controls[0].Definition != "encryptedProof" {
		t.Fatalf("relationship should carry encryptedProof, got %+v", rel.Controls)
	}
}

func TestBuildModelMissingNode(t *testing.T) {
	// a control attached to a node that is not in the resource graph is an error
	rs := []infra.Resource{infra.KV{Name: "store"}}
	catalog := []control.Control{{Name: "leastPrivProof", Kind: "least-privilege-iam", Element: "relay"}}
	entries := []ledger.Entry{{Name: "leastPrivProof", Tier: ledger.Proven}}
	if _, err := BuildModel(rs, catalog, entries); err == nil {
		t.Fatalf("a control on a missing node must be an error")
	}
}

func TestBuildModelMultiRelNoLoss(t *testing.T) {
	rs := []infra.Resource{
		infra.PaaS{Name: "a"}, infra.Compute{Name: "b"}, infra.KV{Name: "c"}, infra.KV{Name: "d"},
	}
	// catalog order: a->b control, then c->d control, then a SECOND a->b control.
	// If the relPos/pointer handling reallocated rels between the first and third,
	// a pointer strategy would lose the third control on a->b.
	catalog := []control.Control{
		{Name: "ctlA", Kind: "k1", Element: "a->b"},
		{Name: "ctlB", Kind: "k2", Element: "c->d"},
		{Name: "ctlC", Kind: "k3", Element: "a->b"},
	}
	entries := []ledger.Entry{
		{Name: "ctlA", Tier: ledger.Proven},
		{Name: "ctlB", Tier: ledger.Proven},
		{Name: "ctlC", Tier: ledger.Proven},
	}
	m, err := BuildModel(rs, catalog, entries)
	if err != nil {
		t.Fatalf("BuildModel: %v", err)
	}
	// find the a->b relationship; it must carry BOTH ctlA and ctlC
	var ab ModelRel
	found := false
	for _, r := range m.Rels {
		if r.ID == "a->b" {
			ab = r
			found = true
		}
	}
	if !found {
		t.Fatalf("a->b relationship missing")
	}
	if len(ab.Controls) != 2 {
		t.Fatalf("a->b must carry both controls, got %d: %+v", len(ab.Controls), ab.Controls)
	}
}
