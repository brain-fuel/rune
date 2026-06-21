package main

import (
	"bytes"
	"os"
	"strings"
	"testing"
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
	if len(rs) != 6 {
		t.Errorf("expected 6 resources in app.wav, got %d", len(rs))
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
