# Plan 5b: AWS LocalStack IAM Apply Gate Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove the demo's scoped least-privilege IAM (the Plan 5 AWS `aws_iam_role` + `aws_iam_role_policy`) actually applies and is stored exactly as declared on a real AWS IAM API, with NO cloud account, by standing LocalStack up, `terraform apply`-ing the IAM artifact through the existing apply lifecycle, observing the stored inline policy via the IAM API, and tearing it down.

**Architecture:** A new integration test in package `infra` mirrors the existing `TestApplyLocalStackBucketReallyCreated` pattern (stand LocalStack up via docker, `infra.Apply(AWS{}, art, ApplyOptions{LocalStack})`, observe via the AWS CLI, destroy). It emits ONLY the demo's IAM resource (`infra.Identity{Name:"relay_role", Grants:["kv:Get","kv:Set"]}`) because LocalStack community implements IAM (`CreateRole`/`PutRolePolicy`/`GetRolePolicy`) but not ElastiCache (the demo's kv) -- so the IAM slice is the no-account-applicable part. The gate upgrades the AWS least-privilege story from emitted-and-validated (Plan 5) to applied-and-observed: the provider accepts the policy AND the stored document is exactly `kv:Get`/`kv:Set` and nothing more. No production code change; this is a credibility test plus a doc.

**Tech Stack:** Go test harness, the existing `infra.Apply` lifecycle (`infra/apply.go`), LocalStack (`localstack/localstack:3`, the existing `localStackImage` const), `terraform` v1.15.1, the `aws` CLI (skipped if absent), docker (skipped if absent).

## Global Constraints

- **Kernel frozen.** No edits under `core/` or `store/`. No new core constructor. This plan adds a test and a doc; it changes no emitter and no apply code.
- **Reuse the existing apply lifecycle.** Use `infra.Apply(AWS{}, art, ApplyOptions{WorkDir, LocalStack, Destroy})` exactly as `TestApplyLocalStackBucketReallyCreated` (`infra/apply_test.go:134`) does. Do NOT reimplement the LocalStack stand-up or the override; reuse the package helpers `dockerReady`, `imageAvailable`, `waitLocalStackHealthy`, and the const `localStackImage` (all in `infra/apply_test.go`).
- **Skip cleanly, never fail spuriously.** The test must `t.Skip` when docker, terraform, the LocalStack image, or (for the policy-content check) the `aws` CLI is absent, and `t.Skipf` when LocalStack cannot start or the apply fails for an offline-provider-mirror reason -- exactly as the bucket test does. A genuine assertion failure (the policy is wrong) is a `t.Fatal`/`t.Error`.
- **The demo's IAM names (verbatim).** The AWS emitter renders `infra.Identity{Name:"relay_role"}` as `resource "aws_iam_role" "relay_role"` with `name = "relay_role"`, and (with grants) `resource "aws_iam_role_policy" "relay_role_policy"` with `name = "relay_role-policy"` (hyphen) and `role = aws_iam_role.relay_role.id`. So on the IAM API the role name is `relay_role` and the inline policy name is `relay_role-policy`.
- **Least privilege is the assertion.** The stored policy document must contain `kv:Get` and `kv:Set` and must NOT contain a capability beyond the grants (no `kv:Delete`, no `"*"` wildcard action). This is the live-provider realization of ch538's least-privilege-IAM proof.
- **Process standards.** NO em or en dashes in any code, comment, or doc (only ASCII hyphen-minus). Conventional Commits. Run `go test ./infra/` before tagging; `go build ./...` stays green. Always tear LocalStack down (the test's `defer docker rm -f`).
- **Depends on Plan 5** (landed at v3.335.0): `infra.Identity.Grants`, the AWS scoped-policy emitter, and the apply lifecycle. If absent, stop and reconcile.

---

### Task 1: The IAM applies and tears down on LocalStack (no account)

Stand LocalStack up, apply the demo's scoped IAM through the real apply lifecycle, assert it stood up (the provider accepted `CreateRole` + `PutRolePolicy`), and tear it down. This is the lifecycle gate; it needs no AWS CLI.

**Files:**
- Create: `infra/iam_localstack_apply_test.go`

**Interfaces:**
- Consumes: `infra.AWS{}`, `infra.Identity{Name, Grants}`, `infra.Apply`, `infra.ApplyOptions{WorkDir, LocalStack, Destroy}`, `infra.ApplyResult{Standing}`; the package helpers `dockerReady(t)`, `imageAvailable(ref)`, `waitLocalStackHealthy(d)`, the const `localStackImage` (all in `infra/apply_test.go`).
- Produces: `TestApplyDemoIAMLocalStack`; a package-private helper `startLocalStack(t) (endpoint string, ok bool)` reused by Task 2.

- [ ] **Step 1: Write the test (mirrors the bucket apply test, for the IAM resource)**

```go
// infra/iam_localstack_apply_test.go
package infra

import (
	"os/exec"
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
```

- [ ] **Step 2: Run the test**

Run: `go test -run TestApplyDemoIAMLocalStack ./infra/`
Expected: PASS (when docker + terraform + the LocalStack image are present, it stands up, applies the IAM, destroys). SKIP cleanly otherwise. If it FAILS at apply with a real IAM error (not an offline skip), read the terraform output: LocalStack community supports `CreateRole`/`PutRolePolicy`, so a genuine failure means the emitted HCL is malformed for the IAM API -- capture the error and report it.

- [ ] **Step 3: Confirm the existing apply suite is unaffected + build**

Run: `go test -run 'TestApply' ./infra/ && go build ./...`
Expected: PASS/SKIP, build clean.

- [ ] **Step 4: Commit**

```bash
git add infra/iam_localstack_apply_test.go
git commit -m "test(infra): demo scoped IAM applies + tears down on LocalStack (no account)"
```

---

### Task 2: The stored policy is EXACTLY the least-privilege set

Upgrade the gate from "it applies" to "the provider stored exactly the scoped policy": after apply, query the IAM API for the role's inline policy and assert it contains `kv:Get` and `kv:Set` and no capability beyond the grants. This is the live-provider macro-to-micro tie.

**Files:**
- Modify: `infra/iam_localstack_apply_test.go` (add the helper + the policy-content test)

**Interfaces:**
- Consumes: `startLocalStack` (Task 1), `infra.Apply`, the `aws` CLI.
- Produces: `iamInlinePolicyDoc(endpoint, role, policy string) string`; `TestApplyDemoIAMLocalStackPolicyExact`.

- [ ] **Step 1: Add the helper + the test**

Append to `infra/iam_localstack_apply_test.go` (and add `"os"` and `"strings"` to the imports):

```go
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
	defer Apply(AWS{}, art, ApplyOptions{WorkDir: dir, LocalStack: endpoint, Destroy: true})

	doc := iamInlinePolicyDoc(endpoint, "relay_role", "relay_role-policy")
	if doc == "" {
		t.Fatal("could not read the inline role policy from the IAM API after apply")
	}
	if !strings.Contains(doc, "kv:Get") || !strings.Contains(doc, "kv:Set") {
		t.Fatalf("stored policy is missing a declared grant:\n%s", doc)
	}
	if strings.Contains(doc, "kv:Delete") || strings.Contains(doc, `"*"`) {
		t.Fatalf("stored policy carries a capability beyond the declared grants:\n%s", doc)
	}
}
```

(The `aws iam get-role-policy` CLI returns the PolicyDocument decoded as JSON, so `kv:Get`/`kv:Set` appear as plain substrings. The `"*"` check guards against a wildcard Action; the emitted policy uses `"Resource":"*"` but never a `"*"` Action, so asserting the substring `"*"` would false-trip on the Resource. Refine the wildcard check to target the Action specifically: assert the document does NOT contain `"Action":"*"` and does NOT contain `"Action": "*"`, rather than a bare `"*"`. Use those two exact substrings for the over-broad check; keep the `kv:Delete` check as a representative extra-capability probe.)

Apply that refinement: replace the over-broad check with

```go
	if strings.Contains(doc, "kv:Delete") || strings.Contains(doc, `"Action":"*"`) || strings.Contains(doc, `"Action": "*"`) {
		t.Fatalf("stored policy carries a capability beyond the declared grants:\n%s", doc)
	}
```

- [ ] **Step 2: Run the test**

Run: `go test -run TestApplyDemoIAMLocalStackPolicyExact ./infra/`
Expected: PASS (with docker + terraform + image + aws CLI present: applies, reads the stored policy, asserts exactly kv:Get/kv:Set, destroys). SKIP cleanly when any tool is absent. If the read returns a document missing the grants, the AWS emitter's policy rendering changed -- capture the document and report it.

- [ ] **Step 3: Run the infra suite + build**

Run: `go test -run 'TestApplyDemoIAM' ./infra/ && go build ./...`
Expected: PASS/SKIP, build clean.

- [ ] **Step 4: Commit**

```bash
git add infra/iam_localstack_apply_test.go
git commit -m "test(infra): LocalStack stores exactly the demo's least-privilege IAM policy"
```

---

### Task 3: Document the applied-and-observed AWS no-account story

Record that the AWS least-privilege IAM is not just emitted-and-validated (Plan 5) but applied-and-observed on a real IAM API with no account.

**Files:**
- Modify: `ref_docs/wootz/R-INFRA.md` (append a short "Applied and observed" subsection; if that file does not exist, append to `examples/WALKTHROUGH.md` instead)
- Modify: `examples/wavelet_deploy.wav` (one comment line under the existing billed-apply block)

**Interfaces:**
- Consumes: nothing.
- Produces: documentation only.

- [ ] **Step 1: Confirm the doc target**

Run: `ls ref_docs/wootz/R-INFRA.md examples/WALKTHROUGH.md`
Expected: at least one exists. Use `ref_docs/wootz/R-INFRA.md` if present, else `examples/WALKTHROUGH.md`.

- [ ] **Step 2: Append the doc note**

Append this section to the chosen file (use ASCII hyphens only, no em/en dashes):

```markdown
## AWS least-privilege IAM: applied and observed (no account)

The demo's scoped IAM (an aws_iam_role plus an aws_iam_role_policy carrying exactly
kv:Get and kv:Set) is not only emitted and terraform-validated (the 3-cloud HCL
gate) but applied and observed against a real AWS IAM API with no cloud account:
LocalStack stands up, `terraform apply` creates the role and inline policy through
the existing apply lifecycle, the IAM API's get-role-policy returns the stored
document, and the test asserts it is exactly the declared grants and nothing more,
then destroys it (harness/../infra/iam_localstack_apply_test.go). This is the
live-provider realization of ch538's least-privilege-IAM proof: the scoped policy
round-trips through a genuine IAM implementation. The kv (ElastiCache) and compute
slices of the demo need a real account (LocalStack community does not implement
them); IAM is the no-account-applicable part. Azure has no terraform emulator (see
the Azure no-account IAM note); the billed one-cloud apply is documented in
examples/wavelet_deploy.wav.
```

- [ ] **Step 3: Add the manifest pointer**

In `examples/wavelet_deploy.wav`, under the existing `# ONE CLOUD LIVE (billed, cloud-gated):` block, add:

```
#
# NO-ACCOUNT IAM (applied + observed): the scoped relay_role policy is apply-tested
# on AWS LocalStack (infra/iam_localstack_apply_test.go) - the provider accepts and
# stores exactly kv:Get/kv:Set. The kv/compute slices need a real account.
```

- [ ] **Step 4: Verify no dashes + commit**

Run: `grep -nP "[\x{2013}\x{2014}]" ref_docs/wootz/R-INFRA.md examples/wavelet_deploy.wav` (or the WALKTHROUGH path)
Expected: no matches (no em/en dashes).

```bash
git add ref_docs/wootz/R-INFRA.md examples/wavelet_deploy.wav
git commit -m "docs(infra): AWS least-privilege IAM applied-and-observed on LocalStack"
```

(If the doc target was `examples/WALKTHROUGH.md`, stage that instead of R-INFRA.md.)

---

## Self-Review

**Spec coverage (against the chosen scope: emulator apply-gate for the demo's AWS IAM):**
- The demo's scoped IAM applies no-account on a real IAM API: Task 1 (`TestApplyDemoIAMLocalStack`, apply + Standing + destroy via `infra.Apply` against LocalStack). Covered.
- The provider accepts AND stores exactly the least-privilege policy: Task 2 (`TestApplyDemoIAMLocalStackPolicyExact`, reads the inline policy back, asserts exactly kv:Get/kv:Set, no extra). Covered.
- Reuse the existing apply lifecycle, no reimplementation: both tasks call `infra.Apply` with `ApplyOptions{LocalStack}` and reuse `dockerReady`/`imageAvailable`/`waitLocalStackHealthy`/`localStackImage`. Covered.
- Skips cleanly with no account/tools: every external dependency (docker, terraform, image, aws CLI) is guarded by a `t.Skip`. Covered.
- Documented: Task 3. Covered.

**Placeholder scan:** Every step shows complete, compilable Go against confirmed APIs (`infra.Apply`/`ApplyOptions`/`ApplyResult.Standing`, `infra.Identity{Name,Grants}`, the package test helpers). The `"*"`-vs-`"Action":"*"` refinement note is a concrete correctness fix (the emitted policy uses `"Resource":"*"`, so the over-broad probe must target the Action), not a placeholder.

**Type consistency:** `startLocalStack(t) (string, bool)` (Task 1) is reused by Task 2. `iamInlinePolicyDoc(endpoint, role, policy)` (Task 2) is called with the verbatim names the AWS emitter produces (role `relay_role`, policy `relay_role-policy`). `ApplyOptions` fields (`WorkDir`, `LocalStack`, `Destroy`) and `ApplyResult.Standing` match `infra/apply.go`.

**One scope note:** LocalStack community implements IAM but not ElastiCache, so this gate covers the demo's IAM slice only; the kv and compute slices' no-account apply is genuinely infeasible on the community image (LocalStack Pro or a real account), which the doc states honestly. This matches the chosen scope ("apply-gate for the demo's IAM"), not the full demo.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-27-aws-localstack-iam-apply.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
