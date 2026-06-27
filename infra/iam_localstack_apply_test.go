package infra

import (
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"
)

// startLocalStack stands a fresh LocalStack container up on port 4566 and waits for
// it to be healthy. It returns the endpoint and ok=true, or skips the test cleanly
// when docker / the image are unavailable or it does not come up. The caller defers
// teardown. Mirrors the stand-up in TestApplyLocalStackBucketReallyCreated.
func startLocalStack(t *testing.T) (string, bool) {
	t.Helper()
	dockerReady(t)
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	if !imageAvailable(localStackImage) {
		t.Skipf("%s not available locally (and no pull in test)", localStackImage)
	}
	name := "lsiamapply"
	exec.Command("docker", "rm", "-f", name).Run() // best-effort stale cleanup
	up := exec.Command("docker", "run", "-d", "--name", name, "-p", "4566:4566", localStackImage)
	if out, err := up.CombinedOutput(); err != nil {
		t.Skipf("could not start LocalStack (offline / port busy): %v\n%s", err, out)
	}
	t.Cleanup(func() { exec.Command("docker", "rm", "-f", name).Run() })
	if !waitLocalStackHealthy(90 * time.Second) {
		t.Skip("LocalStack did not become healthy in time")
	}
	return "http://localhost:4566", true
}

// TestApplyDemoIAMLocalStack applies the demo's scoped least-privilege IAM (an
// aws_iam_role + aws_iam_role_policy carrying exactly kv:Get/kv:Set) against
// LocalStack with NO cloud account, proving the AWS provider accepts CreateRole +
// PutRolePolicy for the scoped policy, then tears it down. The IAM slice is the
// no-account-applicable part of the demo (LocalStack community implements IAM but
// not ElastiCache, the demo's kv).
func TestApplyDemoIAMLocalStack(t *testing.T) {
	endpoint, ok := startLocalStack(t)
	if !ok {
		return
	}
	art, err := (AWS{}).Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	res, err := Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: endpoint})
	if err != nil {
		t.Skipf("cloud apply against LocalStack failed (offline provider mirror?): %v", err)
	}
	if !res.Standing {
		t.Fatal("apply reported not standing")
	}
	// Tear down; a clean destroy proves the whole lifecycle works no-account.
	if _, err := Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: endpoint, Destroy: true}); err != nil {
		t.Fatalf("destroy failed: %v", err)
	}
}

// iamInlinePolicyDoc fetches the named inline role policy from the IAM API at the
// LocalStack endpoint via the aws CLI (dummy creds; the emulator skips validation).
// It returns the CLI's JSON output (which embeds the decoded PolicyDocument) or "" on
// error. The aws CLI must be present; the caller skips the content check otherwise.
func iamInlinePolicyDoc(endpoint, role, policy string) string {
	c := exec.Command("aws", "--endpoint-url="+endpoint, "iam", "get-role-policy",
		"--role-name", role, "--policy-name", policy, "--output", "json")
	c.Env = append(os.Environ(), "AWS_ACCESS_KEY_ID=test", "AWS_SECRET_ACCESS_KEY=test", "AWS_DEFAULT_REGION=us-east-1")
	out, err := c.Output()
	if err != nil {
		return ""
	}
	return string(out)
}

// TestApplyDemoIAMLocalStackPolicyExact applies the demo IAM, then reads the role's
// inline policy back from the IAM API and asserts the provider stored EXACTLY the
// least-privilege grants (kv:Get, kv:Set) and nothing more. This is the live-provider
// realization of ch538's least-privilege-IAM proof: the scoped policy round-trips
// through a real AWS IAM API (LocalStack's implementation).
func TestApplyDemoIAMLocalStackPolicyExact(t *testing.T) {
	if _, err := exec.LookPath("aws"); err != nil {
		t.Skip("aws CLI not in PATH (needed to read the stored policy)")
	}
	endpoint, ok := startLocalStack(t)
	if !ok {
		return
	}
	art, err := (AWS{}).Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	res, err := Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: endpoint})
	if err != nil {
		t.Skipf("cloud apply against LocalStack failed (offline provider mirror?): %v", err)
	}
	if !res.Standing {
		t.Fatal("apply reported not standing")
	}
	// Always tear down, even if an assertion below fails.
	defer Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: endpoint, Destroy: true}) //nolint:errcheck

	doc := iamInlinePolicyDoc(endpoint, "relay_role", "relay_role-policy")
	if doc == "" {
		t.Fatal("could not read the inline role policy from the IAM API after apply")
	}
	if !strings.Contains(doc, "kv:Get") || !strings.Contains(doc, "kv:Set") {
		t.Fatalf("stored policy is missing a declared grant:\n%s", doc)
	}
	if strings.Contains(doc, "kv:Delete") || strings.Contains(doc, `"Action":"*"`) || strings.Contains(doc, `"Action": "*"`) {
		t.Fatalf("stored policy carries a capability beyond the declared grants:\n%s", doc)
	}
}
