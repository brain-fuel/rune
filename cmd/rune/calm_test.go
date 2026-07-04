// cmd/rune/calm_test.go
package main

import (
	"os"
	"strings"
	"testing"

	"goforge.dev/rune/v3/calm"
)

const demoManifest = "../../examples/wavelet_demo.rune"
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
	if len(doc.Nodes) != 4 {
		t.Fatalf("want 4 nodes in the emitted demo doc, got %d", len(doc.Nodes))
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
	if err := os.WriteFile(tmp, []byte(tampered), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	err := runCalm([]string{"validate", tmp, "--manifest", demoManifest, "--listing", demoListing}, &strings.Builder{})
	if err == nil {
		t.Fatalf("validate must reject a tampered document")
	}
}
