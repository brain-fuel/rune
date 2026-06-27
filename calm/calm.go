// calm/calm.go
// Package calm projects a Wavelet architecture (an infra resource graph plus the
// blessed control catalog plus the assurance ledger) to a FINOS CALM JSON document,
// and validates a CALM document back against that source. The language is
// authoritative; CALM is a projection that round-trips. This is the v0.1 subset:
// nodes, connects-relationships, and per-control assurance configuration covering
// the demo's control set. It is read-side tooling and touches no kernel state.
package calm

import (
	"bytes"
	"encoding/json"
)

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

// Marshal renders a CALM document as indented JSON. HTML escaping is disabled so
// relationship IDs containing ">" (e.g. "web->relay") are preserved literally.
func Marshal(d Doc) ([]byte, error) {
	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetEscapeHTML(false)
	enc.SetIndent("", "  ")
	if err := enc.Encode(d); err != nil {
		return nil, err
	}
	// json.Encoder.Encode appends a trailing newline; trim it to match MarshalIndent behaviour.
	b := buf.Bytes()
	if len(b) > 0 && b[len(b)-1] == '\n' {
		b = b[:len(b)-1]
	}
	return b, nil
}

// Parse decodes a CALM document from JSON.
func Parse(data []byte) (Doc, error) {
	var d Doc
	err := json.Unmarshal(data, &d)
	return d, err
}
