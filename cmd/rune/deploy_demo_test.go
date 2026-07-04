// cmd/rune/deploy_demo_test.go
package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

const demoDeployManifest = "../../examples/wavelet_demo.rune"

// validateClean runs `terraform validate` on one cloud's main.tf. It needs provider
// init; if init fails (offline / no network), it SKIPS rather than failing, so the
// gate is robust in disconnected CI. When init succeeds it catches schema errors
// (e.g. an invalid resource argument) that `terraform fmt` cannot.
func validateClean(t *testing.T, tf string) {
	t.Helper()
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "main.tf"), []byte(tf), 0o644); err != nil {
		t.Fatal(err)
	}
	init := exec.Command("terraform", "init", "-backend=false", "-input=false", "-no-color")
	init.Dir = dir
	if out, err := init.CombinedOutput(); err != nil {
		t.Skipf("terraform init unavailable (offline?), skipping validate:\n%s", out)
	}
	val := exec.Command("terraform", "validate", "-no-color")
	val.Dir = dir
	if out, err := val.CombinedOutput(); err != nil {
		t.Fatalf("emitted HCL failed terraform validate:\n%s", out)
	}
}

// fmtClean runs `terraform fmt -check` on one main.tf; skips if terraform is absent.
func fmtClean(t *testing.T, tf string) {
	t.Helper()
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	dir := t.TempDir()
	p := filepath.Join(dir, "main.tf")
	if err := os.WriteFile(p, []byte(tf), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("terraform", "fmt", "-check", p).CombinedOutput()
	if err != nil {
		t.Fatalf("emitted HCL is not fmt-clean:\n%s", out)
	}
}

func TestDeployDemoHCL(t *testing.T) {
	rs, err := parseManifest(demoDeployManifest)
	if err != nil {
		t.Fatalf("parseManifest: %v", err)
	}

	// the region each cloud pins (the in-region control's realization)
	regionPin := map[string]string{
		"aws":   "us-east-1",
		"gcp":   "us-central1",
		"azure": "var.azure_location",
	}

	for _, tgt := range []string{"aws", "gcp", "azure"} {
		e, ok := infra.ByTarget(tgt)
		if !ok {
			t.Fatalf("no emitter for %s", tgt)
		}
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		tf := art.Files["main.tf"]

		// least privilege: the scoped grants appear, and NO extra capability does
		if !strings.Contains(tf, "kv:Get") || !strings.Contains(tf, "kv:Set") {
			t.Fatalf("[%s] HCL missing the scoped grants\n%s", tgt, tf)
		}
		if strings.Contains(tf, "kv:Delete") || strings.Contains(tf, `"*:*"`) {
			t.Fatalf("[%s] HCL contains a capability beyond the declared grants\n%s", tgt, tf)
		}
		// region pin present
		if !strings.Contains(tf, regionPin[tgt]) {
			t.Fatalf("[%s] HCL missing the pinned region %q\n%s", tgt, regionPin[tgt], tf)
		}
		// fmt-clean
		fmtClean(t, tf)
		// schema-valid
		validateClean(t, tf)
	}
}
