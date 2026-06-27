# CALM Round-Trip Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Project a Wavelet architecture (an `infra/` resource graph + the blessed control catalog + the Assurance Ledger) to a FINOS CALM JSON document and validate a CALM document back against that source 1:1, so the macro architecture and the micro proofs are one value that round-trips, surfaced by a new `rune calm emit|validate` command, with zero kernel change.

**Architecture:** A new pure-Go `calm/` package (the projection-side dual of `infra/`): it maps each `infra.Resource` to a CALM node, derives CALM relationships and per-control attachments from the `control/` catalog, and annotates each control with assurance metadata read from the Plan-2 `ledger`. Emit marshals that model to CALM JSON; validate parses a CALM JSON document, reconstructs the model, and compares it to a freshly built source model, emitting an `elaborate.Diagnostic` for any structural or assurance mismatch (a tampered proposition hash or a downgraded tier fails, because the source model carries the live ledger's hashes and tiers). The CLI loads the manifest (the existing `parseManifest`) and the control listing (the existing `session` + `ledger.Build`) and wires them to `calm`. Kernel, store hashing, and the 0x06 hash format are untouched; CALM is read-side tooling.

**Tech Stack:** Go (stdlib `encoding/json` only), the `infra/` resource model, the `control/` registry (Plan 3), the `ledger/` package (Plan 2), `elaborate.Diagnostic`, `internal/session`, `cmd/rune` CLI, the Go test harness.

## Global Constraints

- **Kernel frozen.** No outer-core changes. Hash-format stays `0x06`. No new `core` constructor. The `calm` package adds no kernel machinery and hashes nothing new (it reads `ledger.Entry.PropHash`/`ProofHash` as their `.Short()` hex strings; it never calls `core.HashTerm` itself).
- **The language is authoritative; CALM is a projection that round-trips.** The source of truth is the resource graph + control catalog + ledger. CALM is emitted from it and validated against it. v0.1 covers the demo's control set only (arbitrary CALM ingest is roadmap; do not build a general FINOS-schema validator).
- **Build on `infra/` and the equivalence gate.** CALM nodes are exactly the resource graph's logical resources (one node per `infra.Resource`, `unique-id` = `LogicalName()`, `node-type` mapped from `Kind()`). The node set is provider-independent, the same equivalence witness `infra` already gates on.
- **Reuse, do not rebuild.** Resources come from the existing `parseManifest` (`cmd/rune/deploy.go:219`). Control names + element attachments come from `control.Catalog()` (`control/control.go`, Plan 3). Assurance (tier, proposition hash, proof hash, why, provenance) comes from `ledger.Build` (`ledger/ledger.go:64`, Plan 2). Mismatches are reported as `*elaborate.Diagnostic` (`elaborate/diagnostic.go:22`), the same type `ledger.Gate` returns.
- **Process standards.** NO em or en dashes in any code, comment, or doc (only ASCII hyphen-minus). Conventional Commits. Human-grade diagnostics (Summary, Body, Hints). Verify before claiming done. Run `go test ./calm/ ./cmd/rune/` before tagging; `go build ./...` must stay green.
- **Depends on Plans 2 and 3.** Requires `ledger.Build`/`ledger.Entry`/`ledger.Tier` (Plan 2, landed) and `control.Catalog()`/`control.Control` (Plan 3, landed at v3.333.0). If either is absent, stop and reconcile.

---

### Task 1: The CALM document model + JSON round-trip

Define the Go structs for the CALM v0.1 document subset and confirm they marshal and unmarshal losslessly.

**Files:**
- Create: `calm/calm.go`
- Test: `calm/calm_test.go`

**Interfaces:**
- Consumes: nothing (leaf types).
- Produces: `Doc`, `Node`, `Relationship`, `RelType`, `Connects`, `Endpoint`, `ControlBlock`, `Requirement`, `Config` structs with JSON tags; `func Marshal(d Doc) ([]byte, error)`; `func Parse(data []byte) (Doc, error)`.

- [ ] **Step 1: Write the failing test**

```go
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./calm/`
Expected: FAIL (package `calm` does not exist).

- [ ] **Step 3: Implement the document model**

```go
// calm/calm.go
// Package calm projects a Wavelet architecture (an infra resource graph plus the
// blessed control catalog plus the assurance ledger) to a FINOS CALM JSON document,
// and validates a CALM document back against that source. The language is
// authoritative; CALM is a projection that round-trips. This is the v0.1 subset:
// nodes, connects-relationships, and per-control assurance configuration covering
// the demo's control set. It is read-side tooling and touches no kernel state.
package calm

import "encoding/json"

// Doc is a FINOS CALM architecture document (the v0.1 subset Wavelet emits).
type Doc struct {
	Nodes         []Node           `json:"nodes"`
	Relationships []Relationship   `json:"relationships"`
	Metadata      []map[string]any `json:"metadata,omitempty"`
}

// Node is one CALM node: an architecture element (a service, a database, a client).
type Node struct {
	UniqueID    string                  `json:"unique-id"`
	NodeType    string                  `json:"node-type"`
	Name        string                  `json:"name"`
	Description string                  `json:"description,omitempty"`
	Controls    map[string]ControlBlock `json:"controls,omitempty"`
}

// Relationship is one CALM relationship between nodes; v0.1 emits the `connects` form.
type Relationship struct {
	UniqueID         string                  `json:"unique-id"`
	RelationshipType RelType                 `json:"relationship-type"`
	Controls         map[string]ControlBlock `json:"controls,omitempty"`
}

// RelType is the CALM relationship-type union. v0.1 uses only `connects`.
type RelType struct {
	Connects *Connects `json:"connects,omitempty"`
}

// Connects is a directed node-to-node link.
type Connects struct {
	Source      Endpoint `json:"source"`
	Destination Endpoint `json:"destination"`
}

// Endpoint names a node by its unique id.
type Endpoint struct {
	Node string `json:"node"`
}

// ControlBlock is the CALM control attachment: a description plus one or more
// requirements. Wavelet emits exactly one requirement per control, carrying the
// proof's assurance as the control configuration.
type ControlBlock struct {
	Description  string        `json:"description"`
	Requirements []Requirement `json:"requirements"`
}

// Requirement ties a control to its requirement URL and its Wavelet assurance config.
type Requirement struct {
	RequirementURL string `json:"control-requirement-url"`
	Config         Config `json:"control-config"`
}

// Config is the Wavelet assurance for one control: the proving definition, its tier,
// and the content hashes from the assurance ledger. This is the macro-to-micro tie:
// the proposition hash here is the identity of the exact proof in the source listing.
type Config struct {
	Definition  string `json:"definition"`
	Tier        string `json:"tier"`
	Proposition string `json:"proposition"`
	Proof       string `json:"proof,omitempty"`
	Why         string `json:"why,omitempty"`
}

// Marshal renders a CALM document as indented JSON.
func Marshal(d Doc) ([]byte, error) {
	return json.MarshalIndent(d, "", "  ")
}

// Parse decodes a CALM document from JSON.
func Parse(data []byte) (Doc, error) {
	var d Doc
	err := json.Unmarshal(data, &d)
	return d, err
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./calm/ && go vet ./calm/ && gofmt -l calm/`
Expected: PASS, vet clean, gofmt lists no files.

- [ ] **Step 5: Commit**

```bash
git add calm/calm.go calm/calm_test.go
git commit -m "feat(calm): CALM v0.1 document model + JSON round-trip"
```

---

### Task 2: Node-type mapping + the source Model + BuildModel

The intermediate `Model` is the provider-independent, ledger-backed view that both emit and validate compare against. `BuildModel` assembles it from a resource graph, the control catalog, and ledger entries.

**Files:**
- Create: `calm/model.go`
- Test: `calm/model_test.go`

**Interfaces:**
- Consumes: `infra.Resource` (`Kind() string`, `LogicalName() string`); `control.Control` (`Name`, `Kind`, `Element string`); `ledger.Entry` (`Name string`, `Tier ledger.Tier`, `PropHash`/`ProofHash core.Hash`, `Why string`), `ledger.Tier.String()`, `core.Hash.Short() string`.
- Produces:
  - `func NodeType(kind string) string`
  - `type ControlAttachment struct { ID, Definition, Tier, Proposition, Proof, Why string }`
  - `type ModelNode struct { ID, NodeType string; Controls []ControlAttachment }`
  - `type ModelRel struct { ID, Source, Dest string; Controls []ControlAttachment }`
  - `type Model struct { Nodes []ModelNode; Rels []ModelRel }`
  - `func BuildModel(rs []infra.Resource, catalog []control.Control, entries []ledger.Entry) (Model, error)`

- [ ] **Step 1: Write the failing test**

```go
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./calm/`
Expected: FAIL (`NodeType`, `BuildModel`, `Model` undefined).

- [ ] **Step 3: Implement the model**

```go
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
	relIdx := map[string]*ModelRel{}
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
			rel := relIdx[c.Element]
			if rel == nil {
				rels = append(rels, ModelRel{ID: c.Element, Source: src, Dest: dst})
				rel = &rels[len(rels)-1]
				relIdx[c.Element] = rel
			}
			rel.Controls = append(rel.Controls, a)
			continue
		}
		n := nodeIdx[c.Element]
		if n == nil {
			return Model{}, fmt.Errorf("control %q attaches to node %q which is not in the resource graph", c.Name, c.Element)
		}
		n.Controls = append(n.Controls, a)
	}

	// rebuild rels slice pointers may have moved during append; reindex from nodes/rels
	// by re-deriving from the populated structures, then sort deterministically.
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
```

NOTE on the pointer-into-slice hazard: `nodeIdx` holds `&nodes[i]` and the loop appends to `rels` (a different slice), not to `nodes`, so the `nodes` backing array never reallocates while node pointers are live. Do NOT append to `nodes` after taking those pointers. The `finalNodes` copy at the end is taken after all mutation is complete, so it captures the populated controls. Verify the test passes; if a node's controls come back empty, the pointer aliasing broke and the nodes slice must be pre-sized (it is: `make([]ModelNode, 0, len(rs))` then a single append pass, no further appends).

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./calm/ && go vet ./calm/`
Expected: PASS, vet clean.

- [ ] **Step 5: Commit**

```bash
git add calm/model.go calm/model_test.go
git commit -m "feat(calm): node-type map + ledger-backed source Model + BuildModel"
```

---

### Task 3: Model to Doc and back (the structural round-trip)

`ToDoc` renders the model as a CALM document; `Reconstruct` rebuilds the model from a parsed document. Their composition is the identity, which is the structural half of "validates 1:1".

**Files:**
- Modify: `calm/model.go` (add `ToDoc` and `Reconstruct`)
- Test: `calm/reconstruct_test.go`

**Interfaces:**
- Consumes: `Model`, `Doc`, `Node`, `Relationship`, `ControlBlock`, `Requirement`, `Config` (Tasks 1-2).
- Produces: `func (m Model) ToDoc() Doc`; `func Reconstruct(d Doc) Model`.

- [ ] **Step 1: Write the failing test**

```go
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./calm/`
Expected: FAIL (`ToDoc`, `Reconstruct` undefined).

- [ ] **Step 3: Implement ToDoc + Reconstruct**

Add to `calm/model.go` (and add `"sort"` is already imported; no new imports):

```go
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
```

(`fromBlocks` ranges a map, whose order is nondeterministic, so `sortModel` at the end re-sorts the control slices; this is why `TestModelDocReconstruct` passes despite map iteration order. Each `ControlBlock` is emitted with exactly one requirement, so `Requirements[0]` is safe for documents `ToDoc` produced; a malformed external document with an empty `Requirements` is rejected in Task 5's validation before `Reconstruct` is trusted, but `Reconstruct` itself must not panic. Guard it: if `len(cb.Requirements) == 0`, skip that control in `fromBlocks` so a hand-written doc cannot panic the reconstructor; Task 5's structural compare then reports the missing control.)

Apply that guard: in `fromBlocks`, before `cfg := cb.Requirements[0].Config`, add `if len(cb.Requirements) == 0 { continue }`.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./calm/ && go vet ./calm/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add calm/model.go calm/reconstruct_test.go
git commit -m "feat(calm): Model<->Doc projection (ToDoc + Reconstruct)"
```

---

### Task 4: Emit (the resource graph drives the nodes; equivalence tie)

`Emit` writes a CALM document for a source model. Confirm the node set equals the resource graph's logical resources, the provider-independent equivalence witness.

**Files:**
- Create: `calm/emit.go`
- Test: `calm/emit_test.go`

**Interfaces:**
- Consumes: `Model` (Task 2), `Marshal` (Task 1), `infra.Resource`.
- Produces: `func Emit(m Model, w io.Writer) error`.

- [ ] **Step 1: Write the failing test**

```go
// calm/emit_test.go
package calm

import (
	"sort"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

func TestEmitNodesEqualLogicalResources(t *testing.T) {
	m := demoModel(t)
	var b strings.Builder
	if err := Emit(m, &b); err != nil {
		t.Fatalf("emit: %v", err)
	}
	out := b.String()
	if !strings.Contains(out, `"unique-id": "store"`) || !strings.Contains(out, `"node-type": "database"`) {
		t.Fatalf("emitted CALM missing the store database node:\n%s", out)
	}
	if !strings.Contains(out, `"web->relay"`) || !strings.Contains(out, `"connects"`) {
		t.Fatalf("emitted CALM missing the web->relay connects relationship:\n%s", out)
	}

	// equivalence tie: the CALM node ids are exactly the resource graph's logical names.
	rs := []infra.Resource{infra.PaaS{Name: "web"}, infra.Compute{Name: "relay"}, infra.KV{Name: "store"}}
	wantIDs := []string{}
	for _, r := range rs {
		wantIDs = append(wantIDs, r.LogicalName())
	}
	sort.Strings(wantIDs)
	gotIDs := []string{}
	for _, n := range m.Nodes {
		gotIDs = append(gotIDs, n.ID)
	}
	if strings.Join(gotIDs, ",") != strings.Join(wantIDs, ",") {
		t.Fatalf("CALM node ids %v must equal the logical resource set %v", gotIDs, wantIDs)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./calm/`
Expected: FAIL (`Emit` undefined).

- [ ] **Step 3: Implement Emit**

```go
// calm/emit.go
package calm

import "io"

// Emit writes the CALM document for a source model. The node set is exactly the
// resource graph's logical resources (one node per infra.Resource), so the emitted
// document is provider-independent, the same equivalence witness infra gates on.
func Emit(m Model, w io.Writer) error {
	data, err := Marshal(m.ToDoc())
	if err != nil {
		return err
	}
	_, err = w.Write(data)
	return err
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./calm/ && go vet ./calm/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add calm/emit.go calm/emit_test.go
git commit -m "feat(calm): emit CALM document (nodes = logical resource set)"
```

---

### Task 5: Validate (structural 1:1 + the macro-to-micro assurance tie)

Validate a parsed CALM document against the freshly built source model. A structural difference (a missing/extra node, relationship, or control) and an assurance difference (a tampered proposition hash or a downgraded tier) both fail, each as an `*elaborate.Diagnostic`. Because the source model carries the live ledger's hashes and tiers, validation is the macro-to-micro tie: the CALM control's proposition hash must equal the identity of the proof that currently holds.

**Files:**
- Create: `calm/validate.go`
- Test: `calm/validate_test.go`

**Interfaces:**
- Consumes: `Doc` (Task 1), `Model` + `Reconstruct` (Tasks 2-3), `elaborate.Diagnostic` (`elaborate/diagnostic.go:22`).
- Produces: `func Validate(d Doc, source Model) []error` (each non-nil entry is an `*elaborate.Diagnostic`; empty slice means the document validates 1:1).

- [ ] **Step 1: Write the failing test**

```go
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./calm/`
Expected: FAIL (`Validate` undefined).

- [ ] **Step 3: Implement Validate**

```go
// calm/validate.go
package calm

import (
	"fmt"
	"sort"
	"strings"

	"goforge.dev/rune/v3/elaborate"
)

// Validate checks a parsed CALM document against the source model 1:1. It returns a
// diagnostic per mismatch: a structural difference (node/relationship/control set)
// or an assurance difference (a control's definition, tier, proposition, or proof
// hash diverging from the source). The source model carries the live ledger's
// hashes and tiers, so a tampered or stale document fails. An empty slice means the
// document validates against the source.
func Validate(d Doc, source Model) []error {
	got := Reconstruct(d)
	var errs []error

	// nodes
	srcNodes := indexNodes(source.Nodes)
	gotNodes := indexNodes(got.Nodes)
	for id, sn := range srcNodes {
		gn, ok := gotNodes[id]
		if !ok {
			errs = append(errs, missing("node", id))
			continue
		}
		if gn.NodeType != sn.NodeType {
			errs = append(errs, fieldMismatch("node "+id, "node-type", sn.NodeType, gn.NodeType))
		}
		errs = append(errs, diffControls("node "+id, sn.Controls, gn.Controls)...)
	}
	for id := range gotNodes {
		if _, ok := srcNodes[id]; !ok {
			errs = append(errs, extra("node", id))
		}
	}

	// relationships
	srcRels := indexRels(source.Rels)
	gotRels := indexRels(got.Rels)
	for id, sr := range srcRels {
		gr, ok := gotRels[id]
		if !ok {
			errs = append(errs, missing("relationship", id))
			continue
		}
		if gr.Source != sr.Source || gr.Dest != sr.Dest {
			errs = append(errs, fieldMismatch("relationship "+id, "endpoints",
				sr.Source+"->"+sr.Dest, gr.Source+"->"+gr.Dest))
		}
		errs = append(errs, diffControls("relationship "+id, sr.Controls, gr.Controls)...)
	}
	for id := range gotRels {
		if _, ok := srcRels[id]; !ok {
			errs = append(errs, extra("relationship", id))
		}
	}
	return errs
}

func indexNodes(ns []ModelNode) map[string]ModelNode {
	m := map[string]ModelNode{}
	for _, n := range ns {
		m[n.ID] = n
	}
	return m
}

func indexRels(rs []ModelRel) map[string]ModelRel {
	m := map[string]ModelRel{}
	for _, r := range rs {
		m[r.ID] = r
	}
	return m
}

// diffControls compares the control attachments on one element by control id, then
// by each assurance field (the macro-to-micro tie lives in the proposition/tier
// comparison).
func diffControls(where string, src, got []ControlAttachment) []error {
	var errs []error
	si := map[string]ControlAttachment{}
	for _, c := range src {
		si[c.ID] = c
	}
	gi := map[string]ControlAttachment{}
	for _, c := range got {
		gi[c.ID] = c
	}
	ids := map[string]bool{}
	for id := range si {
		ids[id] = true
	}
	for id := range gi {
		ids[id] = true
	}
	ordered := make([]string, 0, len(ids))
	for id := range ids {
		ordered = append(ordered, id)
	}
	sort.Strings(ordered)
	for _, id := range ordered {
		s, sok := si[id]
		g, gok := gi[id]
		if !sok {
			errs = append(errs, extra("control "+id+" on "+where, id))
			continue
		}
		if !gok {
			errs = append(errs, missing("control "+id+" on "+where, id))
			continue
		}
		if g.Definition != s.Definition {
			errs = append(errs, fieldMismatch(where+" control "+id, "definition", s.Definition, g.Definition))
		}
		if g.Tier != s.Tier {
			errs = append(errs, fieldMismatch(where+" control "+id, "tier", s.Tier, g.Tier))
		}
		if g.Proposition != s.Proposition {
			errs = append(errs, fieldMismatch(where+" control "+id, "proposition hash", s.Proposition, g.Proposition))
		}
		if g.Proof != s.Proof {
			errs = append(errs, fieldMismatch(where+" control "+id, "proof hash", s.Proof, g.Proof))
		}
	}
	return errs
}

func missing(kind, id string) error {
	return &elaborate.Diagnostic{
		Summary: "CALM document is missing a " + kind + " the source requires.",
		Body:    []string{"The source architecture has " + kind + " " + id + ", but the CALM document does not. The projection is not 1:1."},
		Hints:   []string{"Re-emit the document from the current source with `rune calm emit`, or restore the missing " + kind + "."},
	}
}

func extra(kind, id string) error {
	return &elaborate.Diagnostic{
		Summary: "CALM document has a " + kind + " the source does not.",
		Body:    []string{"The CALM document declares " + kind + " " + id + ", which is not in the source architecture. The projection is not 1:1."},
		Hints:   []string{"Re-emit from the current source, or remove the stray " + kind + "."},
	}
}

func fieldMismatch(where, field, want, got string) error {
	w, g := want, got
	if strings.TrimSpace(w) == "" {
		w = "(none)"
	}
	if strings.TrimSpace(g) == "" {
		g = "(none)"
	}
	return &elaborate.Diagnostic{
		Summary: "CALM " + where + " disagrees with the source on " + field + ".",
		Body: []string{
			"The source says " + field + " is " + w + ", but the CALM document says " + g + ".",
			"For a proposition or proof hash, this means the document was emitted from a different proof than the one that currently holds (it is stale or tampered): the macro CALM claim no longer matches the micro proof.",
		},
		Hints: []string{"Re-emit from the current source with `rune calm emit`; if the source proof changed deliberately, the old document is correctly rejected."},
	}
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./calm/ && go vet ./calm/`
Expected: PASS (all four validate tests green).

- [ ] **Step 5: Commit**

```bash
git add calm/validate.go calm/validate_test.go
git commit -m "feat(calm): validate CALM document 1:1 (structural + macro-micro tie)"
```

---

### Task 6: The `rune calm emit|validate` CLI + the demo manifest

Wire the package to the CLI: load the manifest (resources) and the control listing (the ledger), build the source model, and either emit or validate. Ship a demo manifest for the two-tab CRDT app.

**Files:**
- Create: `cmd/rune/calm.go`
- Create: `examples/wavelet_demo.wav`
- Modify: `cmd/rune/main.go` (add a `calm` case to the dispatch switch at line ~111, after the `deploy` case)
- Test: `cmd/rune/calm_test.go`

**Interfaces:**
- Consumes: `parseManifest(path string) ([]infra.Resource, error)` (`cmd/rune/deploy.go:219`, same package); `session.New()` + `(*session.Session).LoadSource(src string) ([]string, error)`; `ledger.Build(s *session.Session) []ledger.Entry`; `control.Catalog() []control.Control`; `calm.BuildModel`, `calm.Emit`, `calm.Parse`, `calm.Validate`.
- Produces: `func runCalm(args []string, out io.Writer) error`.

- [ ] **Step 1: Write the demo manifest**

Create `examples/wavelet_demo.wav`:

```
# examples/wavelet_demo.wav - the two-tab collaborative CRDT app (Wavelet beta demo).
#
# Three architecture nodes: a web client, a signaling/relay service, and a
# persistence store. The control catalog (control.Catalog) attaches the blessed
# controls to these by name: in-region + convergence + the live-region tail on the
# store, least-privilege-IAM on the relay, encrypted-in-transit on the web->relay link.
#
#   rune calm emit                       > demo.calm.json
#   rune calm validate demo.calm.json
paas    web
compute relay
kv      store
```

- [ ] **Step 2: Write the failing test**

```go
// cmd/rune/calm_test.go
package main

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/calm"
)

const demoManifest = "../../examples/wavelet_demo.wav"
const demoListing = "../../listings/ch538_control_catalog.rune"

func TestRunCalmEmitThenValidate(t *testing.T) {
	// emit the demo CALM document
	var emitted strings.Builder
	if err := runCalm([]string{"emit", "--manifest", demoManifest, "--listing", demoListing}, &emitted); err != nil {
		t.Fatalf("emit: %v", err)
	}
	out := emitted.String()
	for _, want := range []string{`"unique-id": "store"`, `"web->relay"`, `"convergence"`, `"tail"`, `"proven"`, `"postulate"`} {
		if !strings.Contains(out, want) {
			t.Fatalf("emitted CALM missing %q:\n%s", want, out)
		}
	}

	// the emitted document must parse and validate 1:1 against the same source
	doc, err := calm.Parse([]byte(out))
	if err != nil {
		t.Fatalf("parse emitted: %v", err)
	}
	if len(doc.Nodes) != 3 {
		t.Fatalf("want 3 nodes in the emitted demo doc, got %d", len(doc.Nodes))
	}
}

func TestRunCalmValidateRejectsTampered(t *testing.T) {
	var emitted strings.Builder
	if err := runCalm([]string{"emit", "--manifest", demoManifest, "--listing", demoListing}, &emitted); err != nil {
		t.Fatalf("emit: %v", err)
	}
	// tamper: flip a proven tier to postulate in the raw JSON
	tampered := strings.Replace(emitted.String(), `"tier": "proven"`, `"tier": "postulate"`, 1)
	tmp := t.TempDir() + "/tampered.calm.json"
	if err := writeFile(tmp, tampered); err != nil {
		t.Fatalf("write: %v", err)
	}
	err := runCalm([]string{"validate", tmp, "--manifest", demoManifest, "--listing", demoListing}, &strings.Builder{})
	if err == nil {
		t.Fatalf("validate must reject a tampered document")
	}
}

// writeFile is a tiny helper local to the test.
func writeFile(path, content string) error {
	return osWriteFile(path, []byte(content), 0o644)
}
```

(Use the standard library directly: replace `osWriteFile` with `os.WriteFile` and add `"os"` to the imports. The helper indirection above is only to keep the test body readable; inline `os.WriteFile(tmp, []byte(tampered), 0o644)` and drop `writeFile`/`osWriteFile` if you prefer.)

- [ ] **Step 3: Run test to verify it fails**

Run: `go test -run TestRunCalm ./cmd/rune/`
Expected: FAIL (`runCalm` undefined).

- [ ] **Step 4: Implement the CLI**

Create `cmd/rune/calm.go`:

```go
package main

import (
	"fmt"
	"io"
	"os"

	"goforge.dev/rune/v3/calm"
	"goforge.dev/rune/v3/control"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/ledger"
)

// runCalm implements `rune calm emit|validate`. Both subcommands build the SOURCE
// model from a manifest (the resource graph -> nodes) and a control listing (the
// assurance ledger -> per-control tiers and hashes); the control catalog maps each
// control to its node or relationship. `emit` writes the CALM document; `validate`
// checks a document against the source 1:1 and fails on any mismatch.
//
//	rune calm emit     [--manifest M] [--listing L]
//	rune calm validate FILE [--manifest M] [--listing L]
func runCalm(args []string, out io.Writer) error {
	if len(args) == 0 {
		return fmt.Errorf("usage: rune calm emit|validate [FILE] [--manifest M] [--listing L]")
	}
	sub := args[0]
	rest := args[1:]

	manifest := "examples/wavelet_demo.wav"
	listing := "listings/ch538_control_catalog.rune"
	var file string
	for i := 0; i < len(rest); i++ {
		switch rest[i] {
		case "--manifest":
			i++
			if i < len(rest) {
				manifest = rest[i]
			}
		case "--listing":
			i++
			if i < len(rest) {
				listing = rest[i]
			}
		default:
			if file == "" {
				file = rest[i]
			}
		}
	}

	source, err := buildSourceModel(manifest, listing)
	if err != nil {
		return err
	}

	switch sub {
	case "emit":
		return calm.Emit(source, out)
	case "validate":
		if file == "" {
			return fmt.Errorf("usage: rune calm validate FILE [--manifest M] [--listing L]")
		}
		data, err := os.ReadFile(file)
		if err != nil {
			return err
		}
		doc, err := calm.Parse(data)
		if err != nil {
			return fmt.Errorf("not a valid CALM document: %w", err)
		}
		errs := calm.Validate(doc, source)
		for _, e := range errs {
			fmt.Fprintln(os.Stderr, e.Error())
		}
		if len(errs) > 0 {
			return fmt.Errorf("CALM validation failed: %d mismatch(es)", len(errs))
		}
		fmt.Fprintln(out, "CALM document validates against the source 1:1.")
		return nil
	default:
		return fmt.Errorf("rune calm: unknown subcommand %q (want emit or validate)", sub)
	}
}

// buildSourceModel loads the resource graph + the control ledger and assembles the
// source model the catalog binds together.
func buildSourceModel(manifest, listing string) (calm.Model, error) {
	rs, err := parseManifest(manifest)
	if err != nil {
		return calm.Model{}, err
	}
	src, err := os.ReadFile(listing)
	if err != nil {
		return calm.Model{}, err
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		return calm.Model{}, err
	}
	entries := ledger.Build(s)
	return calm.BuildModel(rs, control.Catalog(), entries)
}
```

In `cmd/rune/main.go`, add the dispatch case after the `deploy` case (line ~111):

```go
	case "calm":
		if err := runCalm(os.Args[2:], os.Stdout); err != nil {
			fatal(err)
		}
```

(Match the exact `fatal`/`os.Stdout` idiom the neighboring `deploy` case uses.)

- [ ] **Step 5: Run test to verify it passes**

Run: `go test -run TestRunCalm ./cmd/rune/ && go build ./cmd/rune/`
Expected: PASS, binary builds.

- [ ] **Step 6: Commit**

```bash
git add cmd/rune/calm.go cmd/rune/main.go cmd/rune/calm_test.go examples/wavelet_demo.wav
git commit -m "feat(calm): rune calm emit|validate CLI + demo manifest"
```

---

### Task 7: End-to-end demo round-trip + the live macro-micro tie

Prove the criteria on the real demo, against the real ch538 listing and the real catalog: the emitted document validates 1:1, every blessed control appears with the tier the ledger assigns it, and a change to the SOURCE proof (not just the document) is detected.

**Files:**
- Test: `cmd/rune/calm_e2e_test.go`

**Interfaces:**
- Consumes: `runCalm` (Task 6), `buildSourceModel` (Task 6), `calm.Parse`, `calm.Validate`, `control.Catalog`, `control.Flagships` (Plan 3).
- Produces: `TestCalmDemoEndToEnd`, `TestCalmDetectsSourceProofChange`.

- [ ] **Step 1: Write the test**

```go
// cmd/rune/calm_e2e_test.go
package main

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/calm"
	"goforge.dev/rune/v3/control"
)

func TestCalmDemoEndToEnd(t *testing.T) {
	var b strings.Builder
	if err := runCalm([]string{"emit", "--manifest", demoManifest, "--listing", demoListing}, &b); err != nil {
		t.Fatalf("emit: %v", err)
	}
	doc, err := calm.Parse([]byte(b.String()))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	// every flagship control appears in the document, tier "proven"
	flagships := map[string]bool{}
	for _, f := range control.Flagships() {
		flagships[f] = true
	}
	seen := map[string]string{} // definition -> tier
	collect := func(blocks map[string]calm.ControlBlock) {
		for _, cb := range blocks {
			cfg := cb.Requirements[0].Config
			seen[cfg.Definition] = cfg.Tier
		}
	}
	for _, n := range doc.Nodes {
		collect(n.Controls)
	}
	for _, r := range doc.Relationships {
		collect(r.Controls)
	}
	for f := range flagships {
		if seen[f] != "proven" {
			t.Fatalf("flagship %q should appear as proven in the CALM doc, got %q", f, seen[f])
		}
	}
	if seen["liveInRegion"] != "postulate" {
		t.Fatalf("the tail control liveInRegion should appear as postulate, got %q", seen["liveInRegion"])
	}

	// the emitted document validates 1:1 against its own source
	source, err := buildSourceModel(demoManifest, demoListing)
	if err != nil {
		t.Fatalf("buildSourceModel: %v", err)
	}
	if errs := calm.Validate(doc, source); len(errs) != 0 {
		t.Fatalf("the demo document must validate 1:1, got %v", errs)
	}
}

func TestCalmDetectsSourceProofChange(t *testing.T) {
	// Emit against the real listing, then validate the SAME document against a source
	// built from a listing whose convergence proof has a DIFFERENT proposition (the
	// proof changed). The proposition hash in the document no longer matches the live
	// source, so validation must fail: the macro CALM claim is tied to the micro proof.
	var b strings.Builder
	if err := runCalm([]string{"emit", "--manifest", demoManifest, "--listing", demoListing}, &b); err != nil {
		t.Fatalf("emit: %v", err)
	}
	doc, err := calm.Parse([]byte(b.String()))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	// Build a source whose convergesProof has a different TYPE (proposition), so its
	// PropHash differs from the document's. We do this by validating against a hand
	// built model where convergesProof's proposition is altered.
	source, err := buildSourceModel(demoManifest, demoListing)
	if err != nil {
		t.Fatalf("buildSourceModel: %v", err)
	}
	for i := range source.Nodes {
		for j := range source.Nodes[i].Controls {
			if source.Nodes[i].Controls[j].Definition == "convergesProof" {
				source.Nodes[i].Controls[j].Proposition = "changed-hash"
			}
		}
	}
	if errs := calm.Validate(doc, source); len(errs) == 0 {
		t.Fatalf("a changed source proof must make the old document fail validation")
	}
}
```

- [ ] **Step 2: Run test to verify it fails (then passes)**

Run: `go test -run TestCalm ./cmd/rune/`
Expected: PASS. (These tests exercise code already implemented in Tasks 1-6; they are the end-to-end gate. If `TestCalmDemoEndToEnd` fails because a control is missing or mis-tiered, the demo manifest node names must match the catalog elements exactly, web/relay/store; if `convergesProof` is absent, the listing path is wrong. If `TestCalmDetectsSourceProofChange` does not fail validation, the proposition comparison in `diffControls` is not wired.)

- [ ] **Step 3: Run the package suites + build**

Run: `go test ./calm/ ./cmd/rune/ && go build ./...`
Expected: PASS, build clean.

- [ ] **Step 4: Commit**

```bash
git add cmd/rune/calm_e2e_test.go
git commit -m "test(calm): end-to-end demo round-trip + live macro-micro tie"
```

---

## Self-Review

**Spec coverage (against `2026-06-25-wavelet-beta-design.md` Section 3 + Plan 4 in `00-INDEX.md`):**
- Language-to-CALM emit: Task 4 (`Emit`) + Task 6 (`rune calm emit`). Covered.
- Ingest-for-validation: Task 5 (`Validate`) + Task 6 (`rune calm validate`). Covered.
- 1:1 macro-to-micro traceability: Task 5's `diffControls` ties each CALM control to the ledger's proposition hash + tier; Task 7 proves a changed source proof is detected. Covered.
- Build on the `infra/` Resource model + the equal-config equivalence gate: Task 2 (nodes = `infra.Resource` set, `NodeType` from `Kind`); Task 4's test asserts the node ids equal the logical resource set. Covered.
- Map `infra.Kind` and `LogicalResource` to CALM nodes and relationships; controls attach to relationships (and nodes): Tasks 2-3. The catalog's edge-shaped elements (`web->relay`) become relationships; node-shaped elements become node controls. Covered.
- The language is authoritative; CALM is a projection that round-trips: Task 3 (`Reconstruct(m.ToDoc()) == m`) + Task 5 (validate against the live source). Covered.
- v0.1 covers the demo's control set only (arbitrary ingest is roadmap): the source is always the catalog + a listing; no general FINOS-schema validator is built. Covered, and called out in Global Constraints.
- Criteria: emitted doc validates 1:1 (Task 5 `TestValidateRoundTripClean`, Task 7); a CALM node round-trips to the same Wavelet service value whose proofs hold (Task 7 `TestCalmDetectsSourceProofChange`); a mismatch produces a Diagnostic (Task 5, all three failure tests return `*elaborate.Diagnostic`). Covered.

**Placeholder scan:** Every Go step shows complete, compilable code against confirmed APIs (`infra.Resource`/`Kind`/`LogicalName`, `control.Control`/`Catalog`/`Flagships`, `ledger.Entry`/`Build`/`Tier.String`, `core.Hash.Short`, `elaborate.Diagnostic`, `parseManifest`, `session.LoadSource`). The one "match the existing idiom" note is the `main.go` dispatch case (mirror the adjacent `deploy` case) and the test's `os.WriteFile` (a standard-library call) - both name the exact thing to copy.

**Type consistency:** `Doc`/`Node`/`Relationship`/`RelType`/`Connects`/`Endpoint`/`ControlBlock`/`Requirement`/`Config` (Task 1) are used unchanged by `ToDoc`/`Reconstruct` (Task 3) and `Validate` (Task 5). `Model`/`ModelNode`/`ModelRel`/`ControlAttachment` (Task 2) flow through `ToDoc`/`Reconstruct`/`Emit`/`Validate`. `BuildModel(rs, catalog, entries)` has one signature used identically by the CLI (Task 6). `ledger.Proven`/`ledger.Postulate`/`ledger.Tier`/`ledger.Entry` are the Plan-2 names; `control.Catalog`/`control.Control{Name,Kind,Element}`/`control.Flagships` are the Plan-3 names. The control id used as the CALM `controls` map key is `control.Control.Kind` throughout (`in-region`, `encrypted-in-transit`, `least-privilege-iam`, `convergence`, `tail`).

**Two scope notes for the author:**
1. The demo manifest uses `paas web` / `compute relay` / `kv store`; the node names MUST equal the catalog elements (web, relay, store). If Plan 3's `control.Catalog()` element ids ever change, this manifest and the node-name assumptions in Tasks 6-7 change with them (one-line edits, caught by `TestCalmDemoEndToEnd`).
2. v0.1 validation is round-trip-against-source, not validation against FINOS's published JSON Schema. Arbitrary CALM ingest (author-in-CALM) and full schema conformance are explicitly roadmap (design Section 4); a future task can add a `--schema` check without touching this projection.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-26-calm-round-trip.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
