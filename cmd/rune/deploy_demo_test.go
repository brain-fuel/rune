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

const demoDeployManifest = "../../examples/wavelet_deploy.wav"

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
	}
}
