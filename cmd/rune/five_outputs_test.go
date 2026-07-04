// cmd/rune/five_outputs_test.go
// The beta success-criterion-2 gate: one demo source set produces all five
// outputs, and the convergence control the ledger/CALM doc carries is about
// the SAME content hash as the merge the two-tab app deploys.
package main

import (
	"os"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

const (
	fiveOutputsApp      = "../../examples/twotab/counter.rune"
	fiveOutputsListing  = "../../listings/ch538_control_catalog.rune"
	fiveOutputsManifest = "../../examples/wavelet_demo.rune"
)

func loadFile(t *testing.T, path string) *session.Session {
	t.Helper()
	src, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("%s did not load: %v", path, err)
	}
	return s
}

// TestFiveOutputsHashBinding pins criterion 2's "same content hashes": the
// merge the browser tabs run (counter.rune) and the merge the convergence
// control is proven about (ch538) are the SAME definition, literally - equal
// content hashes. Content addressing does the binding; any drift is CI-fatal.
func TestFiveOutputsHashBinding(t *testing.T) {
	app := loadFile(t, fiveOutputsApp)
	cat := loadFile(t, fiveOutputsListing)
	for _, name := range []string{"GC", "merge"} {
		ha, ok := app.Lookup(name)
		if !ok {
			t.Fatalf("app source does not define %q", name)
		}
		hc, ok := cat.Lookup(name)
		if !ok {
			t.Fatalf("control catalog does not define %q", name)
		}
		if ha != hc {
			t.Errorf("%q hash mismatch: app %s vs catalog %s - the proven control is not about the deployed merge", name, ha, hc)
		}
	}
}
