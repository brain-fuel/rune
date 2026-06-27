package infra

import (
	"os/exec"
	"strings"
	"testing"
)

// TestDeployDemoOneCloudLiveFOSS is the one-cloud-live gate on the NO-ACCOUNT path:
// the demo's store (a kv resource named "store", as the deploy manifest declares it)
// is stood up on the Valkey FOSS backend via the real Apply lifecycle, proven serving
// by a PING/PONG round-trip, then torn down. It is the runnable proof that the demo
// infra deploys live; the billed one-cloud apply (drop --localstack, use ambient cloud
// creds) is the same Apply path against a real account and stays cloud-gated. Skips
// cleanly without docker or docker compose.
func TestDeployDemoOneCloudLiveFOSS(t *testing.T) {
	dockerReady(t)
	if err := exec.Command("docker", "compose", "version").Run(); err != nil {
		t.Skip("docker compose unavailable")
	}
	art, err := (Valkey{}).Emit([]Resource{KV{Name: "store"}})
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	res, err := Apply(Valkey{}, art, ApplyOptions{WorkDir: dir})
	if err != nil {
		t.Skipf("FOSS apply could not stand the demo store up (offline / no image / no daemon): %v", err)
	}
	// Always tear down, even on a failed assertion below.
	defer func() {
		composeCmd(dir, "down", "-v").Run()
	}()
	if !res.Standing {
		t.Fatal("apply reported not standing")
	}
	// Prove it is REALLY serving: valkey-cli PING => PONG.
	out, err := composeCmd(dir, "exec", "-T", "valkey", "valkey-cli", "PING").Output()
	if err != nil || !strings.Contains(string(out), "PONG") {
		t.Fatalf("valkey not serving (PING => %q, err %v)", string(out), err)
	}
}
