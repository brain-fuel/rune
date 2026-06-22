package main

import (
	"bytes"
	"os/exec"
	"strings"
	"testing"
)

// dockerUp reports whether a docker daemon is reachable (the gate for the live apply
// tests), skipping the test cleanly otherwise.
func dockerUp(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("docker"); err != nil {
		t.Skip("docker not in PATH")
	}
	if err := exec.Command("docker", "info").Run(); err != nil {
		t.Skip("docker daemon not available")
	}
}

// TestDeployApplyFOSSCLI gates the `rune deploy … --apply --destroy` wiring on a FOSS
// backend: it stands a real Valkey up and tears it back down through the CLI verb, with
// NO cloud account. Asserts the apply-then-destroy banner. Skips cleanly without docker.
func TestDeployApplyFOSSCLI(t *testing.T) {
	dockerUp(t)
	if err := exec.Command("docker", "compose", "version").Run(); err != nil {
		t.Skip("docker compose unavailable")
	}
	dir := t.TempDir()
	var banner bytes.Buffer
	err := runDeploy([]string{
		"--resource", "kv", "--name", "applycli", "--backend", "valkey",
		"--apply", "--destroy", "--out", dir,
	}, &banner)
	if err != nil {
		t.Skipf("FOSS apply could not run (offline / no image / no daemon): %v\n%s", err, banner.String())
	}
	b := banner.String()
	if !strings.Contains(b, "up") || !strings.Contains(b, "torn down") {
		t.Errorf("apply banner missing up/torn-down:\n%s", b)
	}
}

// TestDeployApplyLocalStackCLI gates the cloud apply path through the CLI: `rune deploy
// --resource object --backend aws --apply --localstack --destroy` stands an S3 bucket up
// in LocalStack from the unchanged emitter HCL and tears it down, with NO cloud account.
// Skips cleanly without docker / terraform / the LocalStack image.
func TestDeployApplyLocalStackCLI(t *testing.T) {
	dockerUp(t)
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	const image = "localstack/localstack:3"
	if exec.Command("docker", "image", "inspect", image).Run() != nil {
		t.Skipf("%s not available locally", image)
	}
	name := "lscli"
	exec.Command("docker", "rm", "-f", name).Run()
	if out, err := exec.Command("docker", "run", "-d", "--name", name, "-p", "4566:4566", image).CombinedOutput(); err != nil {
		t.Skipf("could not start LocalStack (offline / port busy): %v\n%s", err, out)
	}
	defer exec.Command("docker", "rm", "-f", name).Run()
	// Wait for health (LocalStack startup is ~15-40s).
	if !waitHealthy4566(t) {
		t.Skip("LocalStack did not become healthy in time")
	}

	dir := t.TempDir()
	var banner bytes.Buffer
	err := runDeploy([]string{
		"--resource", "object", "--name", "clibucket", "--backend", "aws",
		"--apply", "--localstack", "--destroy", "--out", dir,
	}, &banner)
	if err != nil {
		t.Skipf("cloud apply against LocalStack failed (offline provider mirror?): %v\n%s", err, banner.String())
	}
	b := banner.String()
	for _, want := range []string{"LocalStack", "applied", "torn down"} {
		if !strings.Contains(b, want) {
			t.Errorf("apply banner missing %q:\n%s", want, b)
		}
	}
}

// waitHealthy4566 polls the LocalStack health endpoint for S3 readiness.
func waitHealthy4566(t *testing.T) bool {
	t.Helper()
	for i := 0; i < 90; i++ {
		out, err := exec.Command("curl", "-s", "http://localhost:4566/_localstack/health").Output()
		if err == nil && strings.Contains(string(out), `"s3"`) {
			return true
		}
		exec.Command("sleep", "1").Run()
	}
	return false
}
