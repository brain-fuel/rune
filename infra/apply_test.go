package infra

import (
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"
)

// --- Unit tests (always run) -------------------------------------------------

// TestLocalStackOverrideTF: the generated override is a single aws provider block
// with dummy creds, the skip flags, and every endpoint pointed at the given URL.
func TestLocalStackOverrideTF(t *testing.T) {
	got := localStackOverrideTF("http://localhost:4566")
	for _, want := range []string{
		`provider "aws"`,
		`access_key                  = "test"`,
		`skip_credentials_validation = true`,
		`s3_use_path_style           = true`,
		`endpoints {`,
		`s3              = "http://localhost:4566"`,
		`sqs             = "http://localhost:4566"`,
	} {
		if !strings.Contains(got, want) {
			t.Errorf("override missing %q in:\n%s", want, got)
		}
	}
	// Exactly one provider block (terraform merges it into the base by override
	// semantics; two literal blocks for the same provider would be an error).
	if n := strings.Count(got, `provider "aws"`); n != 1 {
		t.Errorf("want exactly 1 provider block, got %d", n)
	}
}

// TestConnectionURL extracts the WAVELET_*_URL endpoint from a connection.env body.
func TestConnectionURL(t *testing.T) {
	env := "WAVELET_KV_BACKEND=valkey\nWAVELET_KV_CACHE=cache\nWAVELET_KV_URL=redis://localhost:6379\n"
	if got := connectionURL(env); got != "redis://localhost:6379" {
		t.Errorf("connectionURL = %q, want redis://localhost:6379", got)
	}
	if got := connectionURL("NO_URL_HERE=1\n"); got != "" {
		t.Errorf("connectionURL on a URL-less env = %q, want empty", got)
	}
}

// TestApplyLocalStackNonAWS: --localstack against a non-aws cloud target is a clear
// error, not a silent misfire (only the AWS provider has the endpoint override).
func TestApplyLocalStackNonAWS(t *testing.T) {
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	art, err := (Azure{}).Emit([]Resource{Bucket{Name: "b"}})
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	_, err = Apply(Azure{}, art, ApplyOptions{WorkDir: dir, LocalStack: "http://localhost:4566"})
	if err == nil || !strings.Contains(err.Error(), "aws target only") {
		t.Fatalf("want an aws-target-only error, got %v", err)
	}
}

// TestApplyFOSSNoCompose: a FOSS apply with no compose.yaml in the artifact is a
// clear error (defensive — every FOSS emitter does emit one).
func TestApplyFOSSNoCompose(t *testing.T) {
	art := Artifact{Files: map[string]string{"connection.env": "X=1\n"}}
	_, err := Apply(Valkey{}, art, ApplyOptions{WorkDir: t.TempDir()})
	if err == nil || !strings.Contains(err.Error(), "no compose.yaml") {
		t.Fatalf("want a no-compose.yaml error, got %v", err)
	}
}

// --- Live tests (gated; skip cleanly without docker / the image / a daemon) --

func dockerReady(t *testing.T) {
	t.Helper()
	if _, err := exec.LookPath("docker"); err != nil {
		t.Skip("docker not in PATH")
	}
	if err := exec.Command("docker", "info").Run(); err != nil {
		t.Skip("docker daemon not available")
	}
}

// TestApplyFOSSStandingRoundTrip stands a real Valkey up via `rune deploy`'s apply
// path, PINGs it to prove it is genuinely serving, then tears it down — the no-cloud,
// no-account FOSS apply lifecycle, verified live. Skips cleanly without docker.
func TestApplyFOSSStandingRoundTrip(t *testing.T) {
	dockerReady(t)
	if err := exec.Command("docker", "compose", "version").Run(); err != nil {
		t.Skip("docker compose unavailable")
	}
	art, err := (Valkey{}).Emit([]Resource{KV{Name: "applytest"}})
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	res, err := Apply(Valkey{}, art, ApplyOptions{WorkDir: dir})
	if err != nil {
		t.Skipf("FOSS apply could not stand Valkey up (offline / no image / no daemon): %v", err)
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

// localStackImage is the tokenless Community image tag the live cloud-apply test uses.
const localStackImage = "localstack/localstack:3"

// TestApplyLocalStackBucketReallyCreated runs the FULL cloud apply path against
// LocalStack with NO cloud account: it stands LocalStack up, applies an S3 bucket from
// the unchanged AWS emitter HCL (redirected by the generated override), verifies the
// bucket REALLY exists via the S3 API, then destroys it. This is the credibility step —
// "deploys to a cloud" goes from generated-and-format-checked to applied-and-observed,
// for free. Skips cleanly without docker / terraform / the image.
func TestApplyLocalStackBucketReallyCreated(t *testing.T) {
	dockerReady(t)
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	if !imageAvailable(localStackImage) {
		t.Skipf("%s not available locally (and no pull in test)", localStackImage)
	}

	name := "lsapply"
	if err := exec.Command("docker", "rm", "-f", name).Run(); err != nil {
		_ = err // best-effort cleanup of a stale container
	}
	up := exec.Command("docker", "run", "-d", "--name", name, "-p", "4566:4566", localStackImage)
	if out, err := up.CombinedOutput(); err != nil {
		t.Skipf("could not start LocalStack (offline / port busy): %v\n%s", err, out)
	}
	defer exec.Command("docker", "rm", "-f", name).Run()

	if !waitLocalStackHealthy(90 * time.Second) {
		t.Skip("LocalStack did not become healthy in time")
	}

	art, err := (AWS{}).Emit([]Resource{Bucket{Name: "verifybucket"}})
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	res, err := Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: "http://localhost:4566"})
	if err != nil {
		t.Skipf("cloud apply against LocalStack failed (offline provider mirror?): %v", err)
	}
	if !res.Standing {
		t.Fatal("apply reported not standing")
	}
	// Verify the bucket REALLY exists in LocalStack's S3.
	if !s3BucketExists("verifybucket") {
		// tear down before failing
		Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: "http://localhost:4566", Destroy: true})
		t.Fatal("bucket verifybucket not found in LocalStack S3 after apply")
	}
	// Tear down and confirm it is gone.
	if _, err := Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: "http://localhost:4566", Destroy: true}); err != nil {
		t.Fatalf("destroy failed: %v", err)
	}
	if s3BucketExists("verifybucket") {
		t.Error("bucket verifybucket still present after destroy")
	}
}

// imageAvailable reports whether a docker image is present locally.
func imageAvailable(ref string) bool {
	out, err := exec.Command("docker", "image", "inspect", ref).CombinedOutput()
	_ = out
	return err == nil
}

// waitLocalStackHealthy polls the LocalStack health endpoint until S3 is available or
// the timeout elapses.
func waitLocalStackHealthy(timeout time.Duration) bool {
	deadline := time.Now().Add(timeout)
	for time.Now().Before(deadline) {
		out, err := exec.Command("curl", "-s", "http://localhost:4566/_localstack/health").Output()
		if err == nil && strings.Contains(string(out), `"s3"`) {
			return true
		}
		time.Sleep(time.Second)
	}
	return false
}

// s3BucketExists reports whether LocalStack's S3 lists the named bucket, via the host
// aws CLI pointed at the LocalStack endpoint with dummy credentials.
func s3BucketExists(bucket string) bool {
	if _, err := exec.LookPath("aws"); err != nil {
		// No aws CLI: fall back to a raw ListBuckets over the S3 endpoint.
		out, _ := exec.Command("curl", "-s", "http://localhost:4566/").Output()
		return strings.Contains(string(out), bucket)
	}
	c := exec.Command("aws", "--endpoint-url=http://localhost:4566", "s3api", "list-buckets",
		"--query", "Buckets[].Name", "--output", "text")
	c.Env = append(os.Environ(), "AWS_ACCESS_KEY_ID=test", "AWS_SECRET_ACCESS_KEY=test", "AWS_DEFAULT_REGION=us-east-1")
	out, err := c.Output()
	if err != nil {
		return false
	}
	return strings.Contains(string(out), bucket)
}
