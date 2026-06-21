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

// TestQueueProviderResources checks each cloud emits its expected concrete resource
// type for a queue (the provider-specific lowering), with valid main.tf output.
func TestQueueProviderResources(t *testing.T) {
	rs := []Resource{Queue{Name: "events"}}
	wantResource := map[string]string{
		"aws":   "aws_sqs_queue",
		"azure": "azurerm_servicebus_queue",
		"gcp":   "google_pubsub_topic",
	}
	for tgt, res := range wantResource {
		e, _ := ByTarget(tgt)
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		tf := art.Files["main.tf"]
		if tf == "" {
			t.Fatalf("[%s] no main.tf emitted", tgt)
		}
		if !strings.Contains(tf, res) {
			t.Errorf("[%s] main.tf missing %q\n%s", tgt, res, tf)
		}
		if !strings.Contains(tf, "\"events\"") {
			t.Errorf("[%s] main.tf missing the queue name", tgt)
		}
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
	rs := []Resource{Queue{Name: "events"}}
	for _, tgt := range cloudTargets {
		e, _ := ByTarget(tgt)
		art, _ := e.Emit(rs)
		tf := art.Files["main.tf"]
		cmd := exec.Command(bin, "fmt", "-check", "-")
		cmd.Stdin = strings.NewReader(tf)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Errorf("[%s] %s fmt rejected the emitted HCL: %v\n%s\n--- HCL ---\n%s",
				tgt, bin, err, out, tf)
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
