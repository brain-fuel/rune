// calm/validate.go
package calm

import (
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
