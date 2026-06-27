// calm/calm_test.go
package calm

import (
	"reflect"
	"testing"
)

func sampleDoc() Doc {
	return Doc{
		Nodes: []Node{
			{UniqueID: "store", NodeType: "database", Name: "store", Description: "persistence",
				Controls: map[string]ControlBlock{
					"in-region": {
						Description: "data stays in the pinned region",
						Requirements: []Requirement{{
							RequirementURL: "https://wavelet-lang.org/controls/in-region",
							Config:         Config{Definition: "inRegionProof", Tier: "proven", Proposition: "ab12cd34"},
						}},
					},
				}},
		},
		Relationships: []Relationship{
			{UniqueID: "web->relay", RelationshipType: RelType{Connects: &Connects{
				Source: Endpoint{Node: "web"}, Destination: Endpoint{Node: "relay"}}},
				Controls: map[string]ControlBlock{
					"encrypted-in-transit": {
						Description: "the link is encrypted",
						Requirements: []Requirement{{
							RequirementURL: "https://wavelet-lang.org/controls/encrypted-in-transit",
							Config:         Config{Definition: "encryptedProof", Tier: "proven", Proposition: "ef56ab78"},
						}},
					},
				}},
		},
		Metadata: []map[string]any{{"wavelet": map[string]any{"generated-by": "rune calm"}}},
	}
}

func TestDocRoundTrip(t *testing.T) {
	d := sampleDoc()
	data, err := Marshal(d)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	got, err := Parse(data)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if !reflect.DeepEqual(d, got) {
		t.Fatalf("round-trip mismatch:\nwant %#v\ngot  %#v", d, got)
	}
}
