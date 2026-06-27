# Plan 5c: Azure No-Account IAM Ceiling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Pin and document the honest no-account ceiling for the demo's Azure IAM: the scoped `azurerm_role_definition` (carrying exactly kv:Get/kv:Set) is terraform-VALIDATED with no cloud account (proving it is provider-valid), and the emulator-APPLY path is proven impossible-by-design (azurerm is the ARM control plane; Azurite emulates only the storage data plane), with FOSS-via-Podman documented as Azure's no-account path for data shapes (not IAM).

**Architecture:** Unlike AWS (LocalStack implements IAM, so the demo IAM can be applied-and-observed no-account -- Plan 5b), Azure has NO terraform emulator: `infra.applyCloud` returns a clear error for an Azure `--localstack` apply, and FOSS-via-Podman covers Azure's DATA shapes (kv/object/queue) but not IAM (a cloud control-plane concept with no FOSS equivalent). So the no-account Azure IAM story is `terraform validate` (schema, no account) plus the documented apply-impossibility. This plan adds one consolidating gate test (validate passes AND emulator-apply errors clearly) and a doc that states the ceiling honestly. No production code change.

**Tech Stack:** Go test harness, the existing `infra.Apply` lifecycle + the `infra.ByTarget("azure")` emitter, `terraform` v1.15.1 (for `validate`, skipped if absent), the `validateClean` helper landed in Plan 5 (`cmd/rune/deploy_demo_test.go`).

## Global Constraints

- **Kernel frozen.** No edits under `core/` or `store/`. No new core constructor. This plan adds a test and a doc; it changes no emitter and no apply code.
- **Honesty over a fake emulator.** Azure's terraform provider (azurerm) targets ARM (the control plane); Azurite emulates only the storage data plane, so there is NO per-service emulator endpoint to redirect. The existing `infra.applyCloud` already returns this as a clear, specific error (`infra/apply.go`, the `case "azure"` in the `--localstack` switch). Do NOT add a fake Azure emulator; the deliverable is to pin the ceiling and document the real no-account path (validate + FOSS-via-Podman for data shapes).
- **Reuse, do not rebuild.** Use the `validateClean(t, tf string)` helper landed in Plan 5 (`cmd/rune/deploy_demo_test.go`, package `main`) -- so this test lives in package `main` (cmd/rune). Use `infra.ByTarget("azure")`, `infra.Apply`, `infra.ApplyOptions{WorkDir, LocalStack}`, `infra.Identity{Name, Grants}`. Do NOT reimplement validate or apply.
- **The demo's Azure IAM names (verbatim).** The Azure emitter renders `infra.Identity{Name:"relay_role", Grants:["kv:Get","kv:Set"]}` as `azurerm_user_assigned_identity "relay_role"` plus `azurerm_role_definition "relay_role_role"` whose nested `permissions { actions = ["kv:Get", "kv:Set"] }`.
- **The pinned error.** An Azure `--localstack` apply must return an error whose message contains `no terraform emulator endpoint` (the exact substring `infra/apply.go` emits). The test asserts that substring.
- **Process standards.** NO em or en dashes in any code, comment, or doc (only ASCII hyphen-minus). Conventional Commits. Run `go test ./cmd/rune/ -run 'TestAzure'` before tagging; `go build ./...` stays green.
- **Depends on Plan 5** (landed at v3.335.0): the Azure scoped-role emitter and the `validateClean` helper. If absent, stop and reconcile.

---

### Task 1: The Azure demo IAM no-account gate (validate passes, emulator-apply errors)

One test that ties the Azure no-account ceiling together for the demo IAM: it is schema-valid with no account (terraform validate), and an emulator apply is cleanly refused.

**Files:**
- Create: `cmd/rune/azure_iam_noaccount_test.go`

**Interfaces:**
- Consumes: `validateClean(t *testing.T, tf string)` (`cmd/rune/deploy_demo_test.go`, package main); `infra.ByTarget`, `infra.Apply`, `infra.ApplyOptions{WorkDir, LocalStack}`, `infra.Identity{Name, Grants}`; `infra.Artifact.Files`.
- Produces: `TestAzureDemoIAMNoAccount`.

- [ ] **Step 1: Write the test**

```go
// cmd/rune/azure_iam_noaccount_test.go
package main

import (
	"os/exec"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

// TestAzureDemoIAMNoAccount pins the honest no-account ceiling for the demo's Azure
// IAM. Unlike AWS (LocalStack implements IAM, so the scoped policy is applied-and-
// observed no-account, see infra/iam_localstack_apply_test.go), Azure has no
// terraform emulator: azurerm targets ARM (the control plane) and Azurite emulates
// only the storage data plane. So the no-account Azure IAM story is exactly two facts,
// asserted here:
//  1. the scoped azurerm_role_definition (actions = kv:Get, kv:Set) is provider-VALID
//     with no account (terraform validate), and
//  2. an emulator apply is cleanly REFUSED with a specific error.
// The no-account path for Azure's DATA shapes (kv/object/queue) is FOSS-via-Podman;
// IAM is a cloud control-plane concept with no FOSS equivalent (see the doc note).
func TestAzureDemoIAMNoAccount(t *testing.T) {
	e, ok := infra.ByTarget("azure")
	if !ok {
		t.Fatal("no azure emitter")
	}
	art, err := e.Emit([]infra.Resource{infra.Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	tf := art.Files["main.tf"]

	// Sanity: the scoped role with exactly the grants is what we are validating.
	if !strings.Contains(tf, "kv:Get") || !strings.Contains(tf, "kv:Set") {
		t.Fatalf("azure demo IAM HCL missing the scoped grants:\n%s", tf)
	}

	// Fact 1: schema-valid with no account.
	validateClean(t, tf)

	// Fact 2: an emulator apply is cleanly refused (azurerm has no emulator endpoint).
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH (needed to reach the apply dispatch)")
	}
	_, aerr := infra.Apply(e, art, infra.ApplyOptions{WorkDir: t.TempDir(), LocalStack: "http://localhost:4566"})
	if aerr == nil {
		t.Fatal("azure emulator apply must be refused (azurerm has no emulator endpoint)")
	}
	if !strings.Contains(aerr.Error(), "no terraform emulator endpoint") {
		t.Fatalf("azure emulator apply error should name the missing emulator endpoint, got: %v", aerr)
	}
}
```

(`validateClean` runs `terraform init` then `terraform validate` and skips when init is offline, so Fact 1 degrades to a skip with no network -- acceptable. `infra.Apply` reaches `applyCloud`, which checks terraform-in-PATH first, then the `--localstack` switch returns the Azure error; that is why Fact 2 guards on terraform presence and needs no docker. If `infra.ApplyOptions` field names differ, read `infra/apply.go`; they are `WorkDir` and `LocalStack`.)

- [ ] **Step 2: Run the test**

Run: `go test -run TestAzureDemoIAMNoAccount ./cmd/rune/`
Expected: PASS (with terraform present and network for init, both facts assert; offline, Fact 1 skips and Fact 2 still asserts the refusal). If Fact 2 fails because the error text changed, read `infra/apply.go`'s azure `--localstack` case and update the expected substring to match the real message (it is the source of truth). If Fact 1 fails validate, the Azure scoped-role HCL is not provider-valid -- capture the `terraform validate` output and report it (that would be an emitter bug from Plan 5).

- [ ] **Step 3: Build + confirm the existing apply-dispatch test still holds**

Run: `go build ./... && go test -run 'TestApplyEmulatorDispatch' ./infra/`
Expected: PASS/SKIP (the pre-existing generic azure-unsupported test stays green; this plan does not change the apply code).

- [ ] **Step 4: Commit**

```bash
git add cmd/rune/azure_iam_noaccount_test.go
git commit -m "test(infra): azure demo IAM no-account ceiling (validates; emulator apply refused)"
```

---

### Task 2: Document the Azure no-account IAM ceiling

State the ceiling honestly so a reader knows exactly what Azure can and cannot do with no account, beside the AWS applied-and-observed note.

**Files:**
- Modify: `ref_docs/wootz/R-INFRA.md` (append the Azure note; if absent, use `examples/WALKTHROUGH.md`)
- Modify: `examples/wavelet_deploy.wav` (one comment line in the no-account section)

**Interfaces:**
- Consumes: nothing.
- Produces: documentation only.

- [ ] **Step 1: Confirm the doc target**

Run: `ls ref_docs/wootz/R-INFRA.md examples/WALKTHROUGH.md`
Expected: at least one exists. Use `ref_docs/wootz/R-INFRA.md` if present, else `examples/WALKTHROUGH.md`.

- [ ] **Step 2: Append the Azure note**

Append to the chosen file (ASCII hyphens only, no em/en dashes):

```markdown
## Azure least-privilege IAM: the no-account ceiling

Azure has no terraform emulator: the azurerm provider targets ARM (the control
plane), and Azurite emulates only the storage DATA plane, so there is no per-service
emulator endpoint to redirect. An Azure `rune deploy --apply --localstack` is
therefore refused with a specific error (infra/apply.go), and a no-account emulator
apply of the demo's IAM is impossible by design, not unbuilt.

The honest no-account Azure story for the demo IAM is two facts, both gated by a test
(cmd/rune/azure_iam_noaccount_test.go): the scoped azurerm_role_definition (actions =
kv:Get, kv:Set) is provider-VALID with no account (terraform validate), and the
emulator apply is cleanly refused. For Azure's DATA shapes (kv, object, queue) the
no-account path is FOSS-via-Podman (Valkey, Garage, RabbitMQ/NATS); IAM is a cloud
control-plane concept with no FOSS equivalent, so for the IAM specifically the
no-account ceiling is validate, and real provisioning needs a billed account (the
billed apply is documented in examples/wavelet_deploy.wav). AWS, by contrast, is
applied-and-observed no-account because LocalStack implements IAM (see the AWS note).
```

- [ ] **Step 3: Add the manifest pointer**

In `examples/wavelet_deploy.wav`, in the no-account section, add:

```
#
# AZURE no-account ceiling: azurerm has no terraform emulator, so the demo IAM cannot
# be applied no-account. It IS terraform-validated no-account (provider-valid), and
# Azure's data shapes use FOSS-via-Podman; real IAM needs a billed account
# (cmd/rune/azure_iam_noaccount_test.go).
```

- [ ] **Step 4: Verify no dashes + commit**

Run: `grep -nP "[\x{2013}\x{2014}]" ref_docs/wootz/R-INFRA.md examples/wavelet_deploy.wav` (or the WALKTHROUGH path)
Expected: no matches.

```bash
git add ref_docs/wootz/R-INFRA.md examples/wavelet_deploy.wav
git commit -m "docs(infra): Azure no-account IAM ceiling (validate + FOSS, no emulator)"
```

(If the doc target was `examples/WALKTHROUGH.md`, stage that instead of R-INFRA.md.)

---

## Self-Review

**Spec coverage (against the chosen scope: emulator apply-gate for the demo's Azure IAM, honest about the ceiling):**
- The scoped Azure IAM is provider-valid no-account: Task 1 Fact 1 (`validateClean` on the emitted azure role definition). Covered.
- The emulator apply is impossible-by-design and proven so: Task 1 Fact 2 (asserts the `no terraform emulator endpoint` error). Covered.
- FOSS-via-Podman is Azure's no-account data-shape path; IAM has no FOSS equivalent: documented in Task 2. Covered.
- No fake Azure emulator is built (honesty constraint): the plan adds only a test + a doc; no apply/emitter change. Covered.

**Placeholder scan:** Every step shows complete, compilable Go against confirmed APIs (`infra.ByTarget`, `infra.Apply`, `infra.ApplyOptions{WorkDir,LocalStack}`, `infra.Identity{Name,Grants}`, `infra.Artifact.Files`, the Plan-5 `validateClean`). The "if the error text changed, read apply.go" note names the source of truth, not a placeholder.

**Type consistency:** `TestAzureDemoIAMNoAccount` uses `validateClean(t, tf)` (Plan 5's signature) and `infra.Apply(e, art, infra.ApplyOptions{...})`. The role name `relay_role` matches the AWS plan (5b) and the Plan 5 demo manifest. The asserted error substring `no terraform emulator endpoint` is the exact text in `infra/apply.go`.

**One scope note:** This plan is deliberately small because Azure's no-account ceiling is a genuine, documented limitation, not a gap -- the value is pinning that ceiling with a test (so a future change that silently "fixes" or breaks it is caught) and documenting it beside the AWS applied-and-observed note (5b), so the one-cloud-live story reads honestly across all three clouds (AWS applied, GCP storage-only emulator, Azure validate-plus-FOSS).

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-27-azure-iam-noaccount-ceiling.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
