// cmd/rune/five_outputs_test.go
// The beta success-criterion-2 gate: one demo source set produces all five
// outputs, and the convergence control the ledger/CALM doc carries is about
// the SAME content hash as the merge the two-tab app deploys.
package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/ledger"
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

// TestFiveOutputsSim folds the EXACT source the app deploys under the
// partition-and-heal schedule and the CvRDT law linter (output 2 of 5).
func TestFiveOutputsSim(t *testing.T) {
	src, err := os.ReadFile(fiveOutputsApp)
	if err != nil {
		t.Fatal(err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("simulate: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "verdict: CONVERGED") {
		t.Fatalf("app protocol did not converge under the fault schedule:\n%s", got)
	}
	if !strings.Contains(got, "join laws hold") {
		t.Fatalf("CvRDT law linter did not certify the join:\n%s", got)
	}
}

// TestFiveOutputsLedger renders the assurance ledger from the control listing
// (output 1 of 5) and requires the convergence control to be tier `proven`.
func TestFiveOutputsLedger(t *testing.T) {
	var out strings.Builder
	if err := runLedger([]string{fiveOutputsListing, "--json"}, &out); err != nil {
		t.Fatalf("ledger: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "convergesProof") {
		t.Fatalf("ledger JSON missing the convergence control:\n%s", got)
	}
	s := loadFile(t, fiveOutputsListing)
	for _, e := range ledger.Build(s) {
		if e.Name == "convergesProof" {
			if e.Tier.String() != "proven" {
				t.Fatalf("convergesProof tier = %q, want proven", e.Tier.String())
			}
			return
		}
	}
	t.Fatal("convergesProof has no ledger entry")
}

// TestFiveOutputsTerraform emits the demo architecture for all three clouds
// from the ONE manifest (output 3 of 5): fmt-clean, least-priv grants present.
func TestFiveOutputsTerraform(t *testing.T) {
	for _, backend := range []string{"aws", "azure", "gcp"} {
		var out strings.Builder
		if err := runDeploy([]string{"--manifest", fiveOutputsManifest, "--backend", backend}, &out); err != nil {
			t.Fatalf("[%s] deploy: %v", backend, err)
		}
		tf := out.String()
		if !strings.Contains(tf, "kv:Get") || !strings.Contains(tf, "kv:Set") {
			t.Fatalf("[%s] HCL missing the scoped IAM grants", backend)
		}
		fmtClean(t, tf)
	}
}

// TestFiveOutputsApp builds the running two-tab app from the same source
// (output 4 of 5). Skips cleanly without node/wabt, like the harness gates.
func TestFiveOutputsApp(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	if _, err := os.Stat("../../harness/node_modules/wabt"); err != nil {
		t.Skip("wabt not installed (bin/setup.sh section 7)")
	}
	cmd := exec.Command("node", "build.mjs")
	cmd.Dir = "../../examples/twotab"
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("twotab build failed: %v\n%s", err, out)
	}
	for _, f := range []string{"counter.wasm", "counter.glue.js"} {
		if _, err := os.Stat("../../examples/twotab/" + f); err != nil {
			t.Fatalf("twotab build did not produce %s", f)
		}
	}
}

// TestFiveOutputsCALM emits the CALM doc from the one manifest + listing and
// re-validates it 1:1 against the source (output 5 of 5, the round-trip).
func TestFiveOutputsCALM(t *testing.T) {
	var doc strings.Builder
	if err := runCalm([]string{"emit", "--manifest", fiveOutputsManifest, "--listing", fiveOutputsListing}, &doc); err != nil {
		t.Fatalf("calm emit: %v", err)
	}
	f := filepath.Join(t.TempDir(), "demo.calm.json")
	if err := os.WriteFile(f, []byte(doc.String()), 0o644); err != nil {
		t.Fatal(err)
	}
	var out strings.Builder
	if err := runCalm([]string{"validate", f, "--manifest", fiveOutputsManifest, "--listing", fiveOutputsListing}, &out); err != nil {
		t.Fatalf("calm validate: %v", err)
	}
	if !strings.Contains(out.String(), "validates against the source 1:1") {
		t.Fatalf("unexpected validate output: %s", out.String())
	}
}
