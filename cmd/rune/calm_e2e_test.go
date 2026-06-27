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
