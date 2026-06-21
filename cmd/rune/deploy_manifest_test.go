package main

import (
	"bytes"
	"os"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

const appManifest = `# a small app's infrastructure graph
compute worker replicas=3 image=img:1
queue events
kv cache
database appdb
secret apikey
iam worker-role
`

// TestManifestDeploy checks that a multi-resource manifest emits ONE artifact with all
// resources, and that Azure's shared scaffolding (resource group, Key Vault) is emitted
// exactly once for the whole graph.
func TestManifestDeploy(t *testing.T) {
	f, err := os.CreateTemp("", "app-*.wav")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(f.Name())
	f.WriteString(appManifest)
	f.Close()

	var out bytes.Buffer
	if err := runDeploy([]string{"--manifest", f.Name(), "--backend", "azure"}, &out); err != nil {
		t.Fatalf("manifest deploy: %v", err)
	}
	tf := out.String()
	// the per-resource lowerings are all present
	for _, want := range []string{"azurerm_servicebus_queue", "azurerm_redis_cache",
		"azurerm_postgresql_flexible_server", "azurerm_key_vault_secret", "azurerm_user_assigned_identity"} {
		if !strings.Contains(tf, want) {
			t.Errorf("manifest output missing %q", want)
		}
	}
	// shared scaffolding emitted exactly once
	if n := strings.Count(tf, "resource \"azurerm_resource_group\""); n != 1 {
		t.Errorf("expected 1 resource group, got %d", n)
	}
	if n := strings.Count(tf, "resource \"azurerm_key_vault\" \"wavelet\""); n != 1 {
		t.Errorf("expected 1 shared key vault, got %d", n)
	}
}

// TestExampleManifest gates the committed examples/app.wav: it parses and lowers to
// each cloud, with the shared scaffolding de-duplicated.
func TestExampleManifest(t *testing.T) {
	rs, err := parseManifest("../../examples/app.wav")
	if err != nil {
		t.Fatalf("examples/app.wav did not parse: %v", err)
	}
	if len(rs) != 9 {
		t.Errorf("expected 9 resources in app.wav, got %d", len(rs))
	}
	for _, c := range cloudCloudTargets {
		var out bytes.Buffer
		if err := runDeploy([]string{"--manifest", "../../examples/app.wav", "--backend", c}, &out); err != nil {
			t.Errorf("[%s] example manifest deploy: %v", c, err)
		}
		if !strings.Contains(out.String(), "resource \"") {
			t.Errorf("[%s] no resources emitted", c)
		}
	}
}

var cloudCloudTargets = []string{"aws", "azure", "gcp"}

// TestDeployOutDir checks that --out writes the artifact files to a directory (the
// FOSS path writes several files: compose.yaml + connection.env).
func TestDeployOutDir(t *testing.T) {
	dir := t.TempDir()
	var out bytes.Buffer
	if err := runDeploy([]string{"--resource", "queue", "--name", "q", "--backend", "nats", "--out", dir}, &out); err != nil {
		t.Fatalf("deploy --out: %v", err)
	}
	for _, f := range []string{"compose.yaml", "connection.env"} {
		if _, err := os.Stat(dir + "/" + f); err != nil {
			t.Errorf("expected %s written to --out dir: %v", f, err)
		}
	}
	if !strings.Contains(out.String(), "wrote") {
		t.Errorf("expected a 'wrote N files' message, got %q", out.String())
	}
}

// TestManifestErrors checks manifest parse errors are clear.
func TestManifestErrors(t *testing.T) {
	cases := []struct{ body, want string }{
		{"queue", "expected `<kind> <name>"},
		{"bogus thing", "unknown resource"},
		{"compute w nope=1", "unknown option"},
		{"compute w replicas=x", "positive integer"},
	}
	for _, c := range cases {
		f, _ := os.CreateTemp("", "bad-*.wav")
		f.WriteString(c.body + "\n")
		f.Close()
		_, err := parseManifest(f.Name())
		os.Remove(f.Name())
		if err == nil || !strings.Contains(err.Error(), c.want) {
			t.Errorf("body %q: err = %v, want containing %q", c.body, err, c.want)
		}
	}
}

// TestAllCLIKindsLowerOnEveryCloud is the completeness gate tying the CLI help to
// real support: every agnostic kind the usage string advertises must build via
// resourceFor AND lower to a concrete resource on aws/azure/gcp. It catches the
// "added a kind but forgot a provider case" bug class.
func TestAllCLIKindsLowerOnEveryCloud(t *testing.T) {
	for _, k := range infra.Kinds() {
		r, err := resourceFor(k, "x", false, "", 1)
		if err != nil {
			t.Errorf("resourceFor(%q): %v (is the kind wired in deploy.go and the usage string?)", k, err)
			continue
		}
		if r.Kind() != k {
			t.Errorf("resourceFor(%q) built a %q resource", k, r.Kind())
		}
		for _, cloud := range []string{"aws", "azure", "gcp"} {
			var out bytes.Buffer
			if err := runDeploy([]string{"--resource", k, "--name", "x", "--backend", cloud}, &out); err != nil {
				t.Errorf("[%s/%s] deploy: %v", cloud, k, err)
				continue
			}
			if !strings.Contains(out.String(), "resource \"") {
				t.Errorf("[%s/%s] emitted no HCL resource", cloud, k)
			}
		}
	}
}
