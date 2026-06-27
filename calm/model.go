// calm/model.go
package calm

import (
	"fmt"
	"sort"
	"strings"

	"goforge.dev/rune/v3/control"
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/infra"
	"goforge.dev/rune/v3/ledger"
)

// nodeTypes maps an agnostic infra Kind to a CALM node-type. Unmapped kinds default
// to "service" (NodeType handles the default).
var nodeTypes = map[string]string{
	"kv": "database", "object": "store", "database": "database",
	"compute": "service", "paas": "webclient", "k8s": "service",
	"queue": "message-broker", "stream": "message-broker",
	"iam": "actor", "secret": "data-asset", "kms": "data-asset",
	"network": "network", "firewall": "network", "lb": "network", "cdn": "network",
}

// NodeType maps an infra Kind to a CALM node-type, defaulting to "service".
func NodeType(kind string) string {
	if t, ok := nodeTypes[kind]; ok {
		return t
	}
	return "service"
}

// ControlAttachment is one proven (or postulated) control bound to a node or
// relationship, carrying its assurance from the ledger.
type ControlAttachment struct {
	ID          string // the control class, e.g. "in-region"
	Definition  string // the rune def name, e.g. "inRegionProof"
	Tier        string
	Proposition string
	Proof       string
	Why         string
}

// ModelNode is one architecture node and the controls attached to it.
type ModelNode struct {
	ID       string
	NodeType string
	Controls []ControlAttachment
}

// ModelRel is one directed relationship and the controls attached to it.
type ModelRel struct {
	ID       string // "<source>-><dest>"
	Source   string
	Dest     string
	Controls []ControlAttachment
}

// Model is the provider-independent, ledger-backed architecture view that emit
// renders and validate compares against.
type Model struct {
	Nodes []ModelNode
	Rels  []ModelRel
}

// BuildModel assembles the source model: one node per resource, relationships and
// control attachments derived from the catalog, assurance read from the ledger. A
// control whose element names a node or relationship endpoint absent from the
// resource graph is an error (the catalog and the deployment disagree).
func BuildModel(rs []infra.Resource, catalog []control.Control, entries []ledger.Entry) (Model, error) {
	byName := map[string]ledger.Entry{}
	for _, e := range entries {
		byName[e.Name] = e
	}
	nodeIdx := map[string]*ModelNode{}
	nodes := make([]ModelNode, 0, len(rs))
	for _, r := range rs {
		nodes = append(nodes, ModelNode{ID: r.LogicalName(), NodeType: NodeType(r.Kind())})
	}
	for i := range nodes {
		nodeIdx[nodes[i].ID] = &nodes[i]
	}
	relPos := map[string]int{}
	var rels []ModelRel

	attachment := func(c control.Control) (ControlAttachment, error) {
		e, ok := byName[c.Name]
		if !ok {
			return ControlAttachment{}, fmt.Errorf("control %q has no ledger entry (the listing does not define it)", c.Name)
		}
		a := ControlAttachment{ID: c.Kind, Definition: c.Name, Tier: e.Tier.String(), Proposition: e.PropHash.Short(), Why: e.Why}
		if e.ProofHash != (core.Hash{}) {
			a.Proof = e.ProofHash.Short()
		}
		return a, nil
	}

	for _, c := range catalog {
		a, err := attachment(c)
		if err != nil {
			return Model{}, err
		}
		if src, dst, ok := splitEdge(c.Element); ok {
			if nodeIdx[src] == nil {
				return Model{}, fmt.Errorf("control %q attaches to relationship %q but node %q is not in the resource graph", c.Name, c.Element, src)
			}
			if nodeIdx[dst] == nil {
				return Model{}, fmt.Errorf("control %q attaches to relationship %q but node %q is not in the resource graph", c.Name, c.Element, dst)
			}
			if _, exists := relPos[c.Element]; !exists {
				rels = append(rels, ModelRel{ID: c.Element, Source: src, Dest: dst})
				relPos[c.Element] = len(rels) - 1
			}
			rels[relPos[c.Element]].Controls = append(rels[relPos[c.Element]].Controls, a)
			continue
		}
		n := nodeIdx[c.Element]
		if n == nil {
			return Model{}, fmt.Errorf("control %q attaches to node %q which is not in the resource graph", c.Name, c.Element)
		}
		n.Controls = append(n.Controls, a)
	}

	// Copy the populated nodes (rels were attached in place by index via relPos, so
	// no reallocation hazard remains), then sort everything for deterministic output.
	finalNodes := make([]ModelNode, len(nodes))
	copy(finalNodes, nodes)
	sortModel(finalNodes, rels)
	return Model{Nodes: finalNodes, Rels: rels}, nil
}

// splitEdge parses an element of the form "src->dst"; ok is false for a plain node id.
func splitEdge(element string) (src, dst string, ok bool) {
	parts := strings.SplitN(element, "->", 2)
	if len(parts) != 2 || parts[0] == "" || parts[1] == "" {
		return "", "", false
	}
	return parts[0], parts[1], true
}

// sortModel orders nodes, relationships, and their controls for deterministic output.
func sortModel(nodes []ModelNode, rels []ModelRel) {
	sort.Slice(nodes, func(i, j int) bool { return nodes[i].ID < nodes[j].ID })
	sort.Slice(rels, func(i, j int) bool { return rels[i].ID < rels[j].ID })
	for i := range nodes {
		sort.Slice(nodes[i].Controls, func(a, b int) bool { return nodes[i].Controls[a].ID < nodes[i].Controls[b].ID })
	}
	for i := range rels {
		sort.Slice(rels[i].Controls, func(a, b int) bool { return rels[i].Controls[a].ID < rels[i].Controls[b].ID })
	}
}

// requirementURL is the stable control-requirement URL for a control class.
func requirementURL(id string) string {
	return "https://wavelet-lang.org/controls/" + id
}

// ToDoc renders the model as a CALM document. Each control attachment becomes a
// single CALM requirement carrying the assurance config.
func (m Model) ToDoc() Doc {
	toBlocks := func(cs []ControlAttachment) map[string]ControlBlock {
		if len(cs) == 0 {
			return nil
		}
		out := map[string]ControlBlock{}
		for _, c := range cs {
			out[c.ID] = ControlBlock{
				Description: "Wavelet control " + c.ID + " discharged by " + c.Definition,
				Requirements: []Requirement{{
					RequirementURL: requirementURL(c.ID),
					Config:         Config{Definition: c.Definition, Tier: c.Tier, Proposition: c.Proposition, Proof: c.Proof, Why: c.Why},
				}},
			}
		}
		return out
	}
	d := Doc{Metadata: []map[string]any{{"wavelet": map[string]any{"generated-by": "rune calm"}}}}
	for _, n := range m.Nodes {
		d.Nodes = append(d.Nodes, Node{
			UniqueID: n.ID, NodeType: n.NodeType, Name: n.ID,
			Description: "Wavelet " + n.NodeType + " " + n.ID,
			Controls:    toBlocks(n.Controls),
		})
	}
	for _, r := range m.Rels {
		d.Relationships = append(d.Relationships, Relationship{
			UniqueID:         r.ID,
			RelationshipType: RelType{Connects: &Connects{Source: Endpoint{Node: r.Source}, Destination: Endpoint{Node: r.Dest}}},
			Controls:         toBlocks(r.Controls),
		})
	}
	return d
}

// Reconstruct rebuilds the model from a parsed CALM document. It is the inverse of
// ToDoc on the structural and assurance fields; cosmetic fields (descriptions,
// metadata) are not part of the model and are not reconstructed.
func Reconstruct(d Doc) Model {
	fromBlocks := func(blocks map[string]ControlBlock) []ControlAttachment {
		if len(blocks) == 0 {
			return nil
		}
		var out []ControlAttachment
		for id, cb := range blocks {
			if len(cb.Requirements) == 0 {
				continue
			}
			cfg := cb.Requirements[0].Config
			out = append(out, ControlAttachment{ID: id, Definition: cfg.Definition, Tier: cfg.Tier, Proposition: cfg.Proposition, Proof: cfg.Proof, Why: cfg.Why})
		}
		return out
	}
	var m Model
	for _, n := range d.Nodes {
		m.Nodes = append(m.Nodes, ModelNode{ID: n.UniqueID, NodeType: n.NodeType, Controls: fromBlocks(n.Controls)})
	}
	for _, r := range d.Relationships {
		mr := ModelRel{ID: r.UniqueID}
		if r.RelationshipType.Connects != nil {
			mr.Source = r.RelationshipType.Connects.Source.Node
			mr.Dest = r.RelationshipType.Connects.Destination.Node
		}
		mr.Controls = fromBlocks(r.Controls)
		m.Rels = append(m.Rels, mr)
	}
	sortModel(m.Nodes, m.Rels)
	return m
}
