package ledger

import (
	"encoding/json"
	"io"

	"goforge.dev/rune/v3/core"
)

type jsonRow struct {
	Name        string `json:"name"`
	Tier        string `json:"tier"`
	Proposition string `json:"proposition"`
	Proof       string `json:"proof,omitempty"`
	Why         string `json:"why,omitempty"`
	File        string `json:"file,omitempty"`
	Line        int    `json:"line,omitempty"`
	Author      string `json:"author,omitempty"`
	Date        string `json:"date,omitempty"`
	Commit      string `json:"commit,omitempty"`
}

// RenderJSON writes the ledger as a stable JSON array (hashes as hex). This is
// the form CALM emits (Plan 4) and CI consumes.
func RenderJSON(es []Entry, w io.Writer) error {
	rows := make([]jsonRow, 0, len(es))
	for _, e := range es {
		r := jsonRow{
			Name:        e.Name,
			Tier:        e.Tier.String(),
			Proposition: e.PropHash.Short(),
			Why:         e.Why,
			File:        e.File,
			Line:        e.Line,
			Author:      e.Provenance.Author,
			Date:        e.Provenance.Date,
			Commit:      e.Provenance.Commit,
		}
		if e.ProofHash != (core.Hash{}) {
			r.Proof = e.ProofHash.Short()
		}
		rows = append(rows, r)
	}
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(rows)
}
