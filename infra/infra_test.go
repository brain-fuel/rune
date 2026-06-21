package infra

import (
	"os/exec"
	"reflect"
	"strings"
	"testing"
)

// cloudTargets are the IaC providers every agnostic config must lower to equivalently.
var cloudTargets = []string{"aws", "azure", "gcp"}

// TestQueueLogicalEquivalence is the "equal config -> equivalent deployment" gate:
// one agnostic Queue graph lowers to the SAME logical resource set on every cloud,
// despite each provider's different concrete resources + plumbing.
func TestQueueLogicalEquivalence(t *testing.T) {
	rs := []Resource{Queue{Name: "events"}, Queue{Name: "jobs"}}
	want := []LogicalResource{{Kind: "queue", Name: "events"}, {Kind: "queue", Name: "jobs"}}

	for _, tgt := range cloudTargets {
		e, ok := ByTarget(tgt)
		if !ok {
			t.Fatalf("no emitter for %q", tgt)
		}
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		if !reflect.DeepEqual(art.Logical, want) {
			t.Errorf("[%s] logical set = %v, want %v", tgt, art.Logical, want)
		}
	}
}

// TestProviderResources checks each cloud emits its expected concrete resource type
// for each agnostic resource kind (the provider-specific lowering).
func TestProviderResources(t *testing.T) {
	cases := []struct {
		kind string
		res  Resource
		want map[string]string // provider -> expected concrete resource type
	}{
		{"queue", Queue{Name: "x"}, map[string]string{
			"aws": "aws_sqs_queue", "azure": "azurerm_servicebus_queue", "gcp": "google_pubsub_topic"}},
		{"kv", KV{Name: "x"}, map[string]string{
			"aws": "aws_elasticache_cluster", "azure": "azurerm_redis_cache", "gcp": "google_redis_instance"}},
		{"object", Bucket{Name: "x"}, map[string]string{
			"aws": "aws_s3_bucket", "azure": "azurerm_storage_container", "gcp": "google_storage_bucket"}},
	}
	for _, c := range cases {
		for tgt, res := range c.want {
			e, _ := ByTarget(tgt)
			art, err := e.Emit([]Resource{c.res})
			if err != nil {
				t.Fatalf("[%s/%s] emit: %v", tgt, c.kind, err)
			}
			tf := art.Files["main.tf"]
			if !strings.Contains(tf, res) {
				t.Errorf("[%s/%s] main.tf missing %q\n%s", tgt, c.kind, res, tf)
			}
			if !strings.Contains(tf, "\"x\"") {
				t.Errorf("[%s/%s] main.tf missing the resource name", tgt, c.kind)
			}
		}
	}
}

// TestKVObjectEquivalence extends the equal-config->equivalent-deployment gate to the
// KV and Object abstractions: one agnostic graph lowers to the same logical set on
// every cloud.
func TestKVObjectEquivalence(t *testing.T) {
	cases := []struct {
		rs   []Resource
		want []LogicalResource
	}{
		{[]Resource{KV{Name: "cache"}}, []LogicalResource{{Kind: "kv", Name: "cache"}}},
		{[]Resource{Bucket{Name: "assets"}}, []LogicalResource{{Kind: "object", Name: "assets"}}},
	}
	for _, c := range cases {
		for _, tgt := range cloudTargets {
			e, _ := ByTarget(tgt)
			art, err := e.Emit(c.rs)
			if err != nil {
				t.Fatalf("[%s] emit: %v", tgt, err)
			}
			if !reflect.DeepEqual(art.Logical, c.want) {
				t.Errorf("[%s] logical = %v, want %v", tgt, art.Logical, c.want)
			}
		}
	}
}

// TestKVObjectFOSS checks the Valkey (kv) and Garage (object) self-hosted backends.
func TestKVObjectFOSS(t *testing.T) {
	v, _ := ByTarget("valkey")
	art, err := v.Emit([]Resource{KV{Name: "cache"}})
	if err != nil {
		t.Fatalf("valkey emit: %v", err)
	}
	if !strings.Contains(art.Files["connection.env"], "WAVELET_KV_BACKEND=valkey") {
		t.Errorf("valkey connection.env wrong:\n%s", art.Files["connection.env"])
	}
	g, _ := ByTarget("garage")
	art, err = g.Emit([]Resource{Bucket{Name: "assets"}})
	if err != nil {
		t.Fatalf("garage emit: %v", err)
	}
	if art.Files["garage.toml"] == "" {
		t.Errorf("garage emit missing garage.toml")
	}
	if !strings.Contains(art.Files["connection.env"], "WAVELET_OBJECT_BACKEND=garage") {
		t.Errorf("garage connection.env wrong:\n%s", art.Files["connection.env"])
	}
}

// TestQueueFOSSBackends checks the self-hosted queue backends emit a runnable Compose
// spec + a connection.env, with the same logical set as the clouds.
func TestQueueFOSSBackends(t *testing.T) {
	rs := []Resource{Queue{Name: "events"}}
	for _, tgt := range []string{"rabbitmq", "nats"} {
		e, ok := ByTarget(tgt)
		if !ok {
			t.Fatalf("no emitter for %q", tgt)
		}
		if e.Cloud() {
			t.Errorf("[%s] should be a FOSS (non-cloud) backend", tgt)
		}
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		if !strings.Contains(art.Files["compose.yaml"], "services:") {
			t.Errorf("[%s] compose.yaml missing services block", tgt)
		}
		env := art.Files["connection.env"]
		if !strings.Contains(env, "WAVELET_QUEUE_BACKEND="+tgt) {
			t.Errorf("[%s] connection.env missing backend marker:\n%s", tgt, env)
		}
		if !strings.Contains(env, "WAVELET_QUEUE_EVENTS=events") {
			t.Errorf("[%s] connection.env missing the queue binding", tgt)
		}
	}
}

// TestQueueHCLFormatted verifies the emitted HCL is syntactically well-formed by
// running `tofu fmt` (or `terraform fmt`) over it: the formatter rejects malformed
// HCL and is a no-op on already-canonical input. Skipped when no IaC binary is
// present (CI without OpenTofu/Terraform installed).
func TestQueueHCLFormatted(t *testing.T) {
	bin := iacBinary()
	if bin == "" {
		t.Skip("no tofu/terraform binary in PATH")
	}
	graphs := map[string][]Resource{
		"queue":  {Queue{Name: "events"}},
		"kv":     {KV{Name: "cache"}},
		"object": {Bucket{Name: "assets"}},
	}
	for kind, rs := range graphs {
		for _, tgt := range cloudTargets {
			e, _ := ByTarget(tgt)
			art, err := e.Emit(rs)
			if err != nil {
				t.Fatalf("[%s/%s] emit: %v", tgt, kind, err)
			}
			tf := art.Files["main.tf"]
			cmd := exec.Command(bin, "fmt", "-check", "-")
			cmd.Stdin = strings.NewReader(tf)
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Errorf("[%s/%s] %s fmt rejected the emitted HCL: %v\n%s\n--- HCL ---\n%s",
					tgt, kind, bin, err, out, tf)
			}
		}
	}
}

func iacBinary() string {
	for _, b := range []string{"tofu", "terraform"} {
		if _, err := exec.LookPath(b); err == nil {
			return b
		}
	}
	return ""
}
