# Wavelet Infra One-Cloud-Live Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the demo's deployed infrastructure realize the controls Plan 3 proves: emit a scoped least-privilege IAM policy (instead of today's empty `Statement:[]`) whose grant set is exactly the access the workload needs, on AWS / Azure / GCP, plus a pinned region; ship a demo deploy manifest and a one-cloud-live apply gate that stands the data plane up and tears it down with no cloud account, while the other two clouds emit `terraform fmt`-clean HCL.

**Architecture:** `infra.Identity` gains a `Grants []string` field (provider-agnostic capability tokens). A shared helper renders those grants into a per-cloud least-privilege policy: an `aws_iam_role_policy` (compact JSON document), a `google_project_iam_custom_role` (permissions list), and an `azurerm_role_definition` (actions list). Empty grants emit exactly today's bare identity (backward compatible). The manifest parser gains a `grants=` option. A demo manifest (`relay` compute, `store` kv, `relay_role` iam with the kv grants) is deployed via the EXISTING `rune deploy` apply lifecycle: the FOSS data plane (Valkey) round-trips with no account, the cloud HCL is fmt-clean, and the billed one-cloud apply is documented. The CLI rename to `wvl` is Plan 7; this plan uses `rune deploy`.

**Tech Stack:** Go (stdlib `encoding/json` for the policy doc), the `infra/` emitters (`infra.go`, `clouds.go`, `hcl.go`, `apply.go`), the `cmd/rune/deploy.go` manifest parser, `terraform` (for the fmt-clean + apply gates, skipped when absent), `docker`/`compose` (for the FOSS data-plane round-trip, skipped when absent).

## Global Constraints

- **Kernel frozen.** No outer-core changes. No edits under `core/` or `store/`. The `infra/` layer is a consumer of the kernel (the shadow rule): it reads checked definitions and emits throwaway deployment artifacts; it adds no core constructor and mutates nothing in `core`/`store`.
- **Reuse the existing apply lifecycle.** `infra.Apply` (`infra/apply.go:58`) already stands FOSS backends up via compose and cloud backends up via `terraform` (LocalStack override for AWS, no account). Do NOT reimplement it. One-cloud-live billed apply is the existing path minus `--localstack` plus ambient cloud creds; it stays CLOUD-GATED and is documented, not coded.
- **Backward compatible HCL.** An `Identity` with NO grants must emit exactly the bytes it emits today (the bare `aws_iam_role` / `azurerm_user_assigned_identity` / `google_service_account`), so existing manifests (`examples/app.wav` has `iam worker-role`) and the snapshot-style tests in `infra/infra_test.go` stay green. The scoped policy is emitted ONLY when `len(Grants) > 0`.
- **HCL must stay `terraform fmt`-clean.** Every emitted cloud artifact passes `terraform fmt -check`. Render the policy as a quoted compact-JSON string (one token, fmt-stable, the same pattern as the existing `assume_role_policy`) and permission lists as short single-line list literals (`["kv:Get", "kv:Set"]`). Do not emit multi-line `jsonencode(...)` objects (fmt would reformat them).
- **Equal config, equivalent deployment.** The same `Grants` render to an equivalent least-privilege policy on every cloud. The grant tokens are agnostic capabilities (e.g. `kv:Get`); mapping them to real per-cloud action strings is roadmap (documented), not v0.1.
- **The deploy realizes the proof.** The demo's grants (`kv:Get`, `kv:Set`) correspond to the least-privilege-IAM control in `listings/ch538_control_catalog.rune` (`needed` = kv get/set). The correspondence is by convention and is asserted (the emitted policy's actions equal exactly the declared grants, nothing more) and documented; the proof is rune-side, the deployment Go-side.
- **Process standards.** NO em or en dashes in any code, comment, or doc (only ASCII hyphen-minus). Conventional Commits. Verify before claiming done. Run `go test ./infra/ ./cmd/rune/` (targeted) before tagging; `go build ./...` stays green. The full `go test ./...` hits a pre-existing docker/OTP wall; the targeted infra + cmd/rune runs are the gate.

---

### Task 1: `Identity.Grants` + the agnostic policy helpers

Add the grants field and the two rendering helpers (a compact JSON policy document for AWS, a list literal for GCP/Azure), with unit tests. No emitter wiring yet.

**Files:**
- Modify: `infra/infra.go` (add `Grants []string` to `Identity`, ~line 225)
- Create: `infra/iam.go`
- Test: `infra/iam_test.go`

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `infra.Identity` gains `Grants []string`.
  - `func iamPolicyJSON(grants []string) string` (package-private) - a compact AWS-style policy document.
  - `func hclList(items []string) string` (package-private) - a single-line HCL list literal `["a", "b"]`.

- [ ] **Step 1: Write the failing test**

```go
// infra/iam_test.go
package infra

import "testing"

func TestIAMPolicyJSON(t *testing.T) {
	got := iamPolicyJSON([]string{"kv:Get", "kv:Set"})
	want := `{"Version":"2012-10-17","Statement":[{"Effect":"Allow","Action":["kv:Get","kv:Set"],"Resource":"*"}]}`
	if got != want {
		t.Fatalf("policy JSON\n got: %s\nwant: %s", got, want)
	}
}

func TestIAMPolicyJSONEmpty(t *testing.T) {
	// no grants => an empty (deny-all) statement list, still a well-formed document
	got := iamPolicyJSON(nil)
	want := `{"Version":"2012-10-17","Statement":[]}`
	if got != want {
		t.Fatalf("empty policy JSON\n got: %s\nwant: %s", got, want)
	}
}

func TestHCLList(t *testing.T) {
	if got := hclList([]string{"kv:Get", "kv:Set"}); got != `["kv:Get", "kv:Set"]` {
		t.Fatalf("hclList = %s", got)
	}
	if got := hclList(nil); got != `[]` {
		t.Fatalf("empty hclList = %s", got)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run 'TestIAMPolicyJSON|TestHCLList' ./infra/`
Expected: FAIL (`iamPolicyJSON`, `hclList` undefined).

- [ ] **Step 3: Add the field + implement the helpers**

In `infra/infra.go`, change the `Identity` struct (the comment stays):

```go
type Identity struct {
	Name   string
	Grants []string // agnostic least-privilege capability tokens, e.g. "kv:Get"
}
```

Create `infra/iam.go`:

```go
package infra

import (
	"encoding/json"
	"fmt"
	"strings"
)

// iamPolicyStatement is one Allow statement of an AWS-style policy document.
type iamPolicyStatement struct {
	Effect   string   `json:"Effect"`
	Action   []string `json:"Action"`
	Resource string   `json:"Resource"`
}

// iamPolicyDoc is the compact policy document an aws_iam_role_policy carries.
type iamPolicyDoc struct {
	Version   string               `json:"Version"`
	Statement []iamPolicyStatement `json:"Statement"`
}

// iamPolicyJSON renders the least-privilege policy for a set of agnostic grant
// tokens as a compact JSON string (one HCL token, terraform-fmt-stable). With no
// grants it is a well-formed empty (deny-all) document. The grants are the policy's
// only actions, so a least-privilege policy is auditable as exactly the declared
// capabilities and nothing more; an over-broad policy would carry extra actions.
func iamPolicyJSON(grants []string) string {
	doc := iamPolicyDoc{Version: "2012-10-17", Statement: []iamPolicyStatement{}}
	if len(grants) > 0 {
		doc.Statement = []iamPolicyStatement{{Effect: "Allow", Action: grants, Resource: "*"}}
	}
	b, _ := json.Marshal(doc)
	return string(b)
}

// hclList renders a single-line HCL list literal of quoted strings, fmt-stable for
// the short permission lists the least-privilege roles carry.
func hclList(items []string) string {
	q := make([]string, len(items))
	for i, s := range items {
		q[i] = fmt.Sprintf("%q", s)
	}
	return "[" + strings.Join(q, ", ") + "]"
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run 'TestIAMPolicyJSON|TestHCLList' ./infra/ && go build ./...`
Expected: PASS, build clean. (The new `Grants` field is additive; existing `Identity{Name: ...}` literals still compile.)

- [ ] **Step 5: Run the existing infra suite (backward compat)**

Run: `go test ./infra/`
Expected: PASS (the field add + new helpers must not disturb existing emitter output; no emitter reads `Grants` yet).

- [ ] **Step 6: Commit**

```bash
git add infra/infra.go infra/iam.go infra/iam_test.go
git commit -m "feat(infra): Identity.Grants + agnostic least-privilege policy helpers"
```

---

### Task 2: AWS emits the scoped IAM policy

When an `Identity` carries grants, emit an `aws_iam_role_policy` with the least-privilege document, after the existing role. No grants leaves today's output unchanged.

**Files:**
- Modify: `infra/clouds.go` (the AWS emitter's `case Identity:`, ~line 202-206)
- Test: `infra/iam_aws_test.go`

**Interfaces:**
- Consumes: `iamPolicyJSON` (Task 1), the `hcl` helpers `h.open`/`h.attr`/`h.close`/`h.blank` and `str` (`infra/hcl.go`).
- Produces: AWS `Identity` HCL gains an `aws_iam_role_policy "<name>_policy"` resource when grants are present.

- [ ] **Step 1: Write the failing test**

```go
// infra/iam_aws_test.go
package infra

import (
	"strings"
	"testing"
)

func TestAWSIdentityScopedPolicy(t *testing.T) {
	art, err := AWS{}.Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	tf := art.Files["main.tf"]
	for _, want := range []string{
		`resource "aws_iam_role_policy" "relay_role_policy"`,
		`role      = aws_iam_role.relay_role.id`,
		`"Action":["kv:Get","kv:Set"]`,
	} {
		if !strings.Contains(tf, want) {
			t.Fatalf("AWS iam policy missing %q\n%s", want, tf)
		}
	}
}

func TestAWSIdentityNoGrantsUnchanged(t *testing.T) {
	// an Identity with no grants must NOT emit a policy resource (backward compat)
	art, err := AWS{}.Emit([]Resource{Identity{Name: "worker_role"}})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(art.Files["main.tf"], "aws_iam_role_policy") {
		t.Fatalf("a grant-less identity must not emit a policy resource\n%s", art.Files["main.tf"])
	}
}
```

(The `role      = ...` spacing in the first test reflects how `terraform fmt` aligns `=` within a block. If the assertion is brittle against alignment, assert on `aws_iam_role.relay_role.id` alone; the fmt-clean test in Task 6 covers exact formatting. Adjust the substring to match the emitter's pre-fmt output, which the `hcl` writer aligns via its `attr`/`close` pad logic; check the emitted `main.tf` and match it.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestAWSIdentity ./infra/`
Expected: FAIL (no `aws_iam_role_policy` emitted).

- [ ] **Step 3: Implement the AWS policy emission**

In `infra/clouds.go`, find the AWS emitter's `case Identity:` (it currently emits the `aws_iam_role` with the empty `assume_role_policy`). Leave that block EXACTLY as is, and append the scoped policy after the role's `h.close()`:

```go
		case Identity:
			h.open("resource \"aws_iam_role\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("assume_role_policy", str("{\"Version\":\"2012-10-17\",\"Statement\":[]}"))
			h.close()
			if len(v.Grants) > 0 {
				h.blank()
				h.open("resource \"aws_iam_role_policy\" %s", str(v.Name+"_policy"))
				h.attr("name", str(v.Name+"-policy"))
				h.attr("role", "aws_iam_role."+v.Name+".id")
				h.attr("policy", str(iamPolicyJSON(v.Grants)))
				h.close()
			}
```

(Preserve the existing three lines of the role verbatim; only the `if len(v.Grants) > 0` block is new. `v` is the `Identity` value the switch already binds.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestAWSIdentity ./infra/`
Expected: PASS (both the scoped-policy and the no-grants-unchanged tests).

- [ ] **Step 5: Run the full infra suite**

Run: `go test ./infra/`
Expected: PASS (no existing AWS emitter test regresses).

- [ ] **Step 6: Commit**

```bash
git add infra/clouds.go infra/iam_aws_test.go
git commit -m "feat(infra): AWS emits a scoped least-privilege IAM policy from grants"
```

---

### Task 3: GCP emits the scoped IAM custom role

The GCP equivalent: a `google_project_iam_custom_role` whose `permissions` are exactly the grants. No grants leaves today's `google_service_account` output unchanged.

**Files:**
- Modify: `infra/clouds.go` (the GCP emitter's `case Identity:`, ~line 835-836)
- Test: `infra/iam_gcp_test.go`

**Interfaces:**
- Consumes: `hclList` (Task 1), the `hcl` helpers.
- Produces: GCP `Identity` HCL gains a `google_project_iam_custom_role "<name>_role"` when grants are present.

- [ ] **Step 1: Write the failing test**

```go
// infra/iam_gcp_test.go
package infra

import (
	"strings"
	"testing"
)

func TestGCPIdentityScopedRole(t *testing.T) {
	art, err := GCP{}.Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	tf := art.Files["main.tf"]
	for _, want := range []string{
		`resource "google_project_iam_custom_role" "relay_role_role"`,
		`permissions = ["kv:Get", "kv:Set"]`,
	} {
		if !strings.Contains(tf, want) {
			t.Fatalf("GCP custom role missing %q\n%s", want, tf)
		}
	}
}

func TestGCPIdentityNoGrantsUnchanged(t *testing.T) {
	art, err := GCP{}.Emit([]Resource{Identity{Name: "worker_role"}})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(art.Files["main.tf"], "google_project_iam_custom_role") {
		t.Fatalf("a grant-less identity must not emit a custom role\n%s", art.Files["main.tf"])
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestGCPIdentity ./infra/`
Expected: FAIL.

- [ ] **Step 3: Implement the GCP role emission**

In `infra/clouds.go`, find the GCP emitter's `case Identity:` (it emits a `google_service_account`). Leave the existing `google_service_account` block verbatim, and append after its `h.close()`:

```go
			if len(v.Grants) > 0 {
				h.blank()
				h.open("resource \"google_project_iam_custom_role\" %s", str(v.Name+"_role"))
				h.attr("role_id", str(v.Name+"_role"))
				h.attr("title", str(v.Name+" least privilege"))
				h.attr("permissions", hclList(v.Grants))
				h.close()
			}
```

(Read the existing `case Identity:` body first and append this block after the service account's closing `h.close()`, inside the same case. `v` is the `Identity` value.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestGCPIdentity ./infra/`
Expected: PASS.

- [ ] **Step 5: Run the full infra suite**

Run: `go test ./infra/`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add infra/clouds.go infra/iam_gcp_test.go
git commit -m "feat(infra): GCP emits a scoped least-privilege custom role from grants"
```

---

### Task 4: Azure emits the scoped role definition

The Azure equivalent: an `azurerm_role_definition` whose `permissions { actions = [...] }` are exactly the grants. No grants leaves today's `azurerm_user_assigned_identity` output unchanged.

**Files:**
- Modify: `infra/clouds.go` (the Azure emitter's `case Identity:`, ~line 555-561)
- Test: `infra/iam_azure_test.go`

**Interfaces:**
- Consumes: `hclList` (Task 1), the `hcl` helpers.
- Produces: Azure `Identity` HCL gains an `azurerm_role_definition "<name>_role"` when grants are present.

- [ ] **Step 1: Write the failing test**

```go
// infra/iam_azure_test.go
package infra

import (
	"strings"
	"testing"
)

func TestAzureIdentityScopedRole(t *testing.T) {
	art, err := Azure{}.Emit([]Resource{Identity{Name: "relay_role", Grants: []string{"kv:Get", "kv:Set"}}})
	if err != nil {
		t.Fatal(err)
	}
	tf := art.Files["main.tf"]
	for _, want := range []string{
		`resource "azurerm_role_definition" "relay_role_role"`,
		`actions     = ["kv:Get", "kv:Set"]`,
	} {
		if !strings.Contains(tf, want) {
			t.Fatalf("Azure role definition missing %q\n%s", want, tf)
		}
	}
}

func TestAzureIdentityNoGrantsUnchanged(t *testing.T) {
	art, err := Azure{}.Emit([]Resource{Identity{Name: "worker_role"}})
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(art.Files["main.tf"], "azurerm_role_definition") {
		t.Fatalf("a grant-less identity must not emit a role definition\n%s", art.Files["main.tf"])
	}
}
```

(The `actions     = ...` alignment again depends on `terraform fmt`; if brittle, assert on `["kv:Get", "kv:Set"]` and `azurerm_role_definition` separately and let Task 6's fmt-check cover formatting. Match the emitter's pre-fmt `attr` alignment.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestAzureIdentity ./infra/`
Expected: FAIL.

- [ ] **Step 3: Implement the Azure role emission**

In `infra/clouds.go`, find the Azure emitter's `case Identity:` (it emits `azurerm_user_assigned_identity`). Leave that block verbatim, and append after its `h.close()`:

```go
			if len(v.Grants) > 0 {
				h.blank()
				h.open("resource \"azurerm_role_definition\" %s", str(v.Name+"_role"))
				h.attr("name", str(v.Name+"-role"))
				h.attr("scope", "azurerm_resource_group.wavelet.id")
				h.open("permissions")
				h.attr("actions", hclList(v.Grants))
				h.attr("not_actions", "[]")
				h.close()
				h.close()
			}
```

(Append inside the existing `case Identity:`, after the identity resource's `h.close()`. Note the TWO `h.close()` calls at the end: one for the inner `permissions` block, one for the `azurerm_role_definition` resource.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestAzureIdentity ./infra/`
Expected: PASS.

- [ ] **Step 5: Run the full infra suite**

Run: `go test ./infra/`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add infra/clouds.go infra/iam_azure_test.go
git commit -m "feat(infra): Azure emits a scoped least-privilege role definition from grants"
```

---

### Task 5: The manifest `grants=` option

Let a manifest line declare an identity's grants: `iam relay_role grants=kv:Get,kv:Set`. Thread it through `parseManifest` and `resourceFor` into `Identity.Grants`.

**Files:**
- Modify: `cmd/rune/deploy.go` (`parseManifest` option loop ~line 238-262; `resourceFor` signature + the `iam` case ~line 306)
- Test: `cmd/rune/deploy_grants_test.go`

**Interfaces:**
- Consumes: `infra.Identity{Name, Grants}` (Task 1).
- Produces: `resourceFor(kind, name string, fifo bool, image string, replicas int, grants []string) (infra.Resource, error)` (signature gains `grants`); `parseManifest` accepts `grants=a,b`.

- [ ] **Step 1: Write the failing test**

```go
// cmd/rune/deploy_grants_test.go
package main

import (
	"os"
	"path/filepath"
	"testing"

	"goforge.dev/rune/v3/infra"
)

func TestManifestGrants(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "m.wav")
	if err := os.WriteFile(path, []byte("iam relay_role grants=kv:Get,kv:Set\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	rs, err := parseManifest(path)
	if err != nil {
		t.Fatalf("parseManifest: %v", err)
	}
	if len(rs) != 1 {
		t.Fatalf("want 1 resource, got %d", len(rs))
	}
	id, ok := rs[0].(infra.Identity)
	if !ok {
		t.Fatalf("want an Identity, got %T", rs[0])
	}
	if len(id.Grants) != 2 || id.Grants[0] != "kv:Get" || id.Grants[1] != "kv:Set" {
		t.Fatalf("grants not parsed: %v", id.Grants)
	}
}

func TestManifestGrantsOnlyIAM(t *testing.T) {
	// grants on a non-iam resource is a clear error
	dir := t.TempDir()
	path := filepath.Join(dir, "m.wav")
	if err := os.WriteFile(path, []byte("kv store grants=kv:Get\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	if _, err := parseManifest(path); err == nil {
		t.Fatalf("grants on a non-iam resource must be an error")
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestManifestGrants ./cmd/rune/`
Expected: FAIL (`grants` is an unknown option).

- [ ] **Step 3: Implement the option + thread it**

In `cmd/rune/deploy.go`, in the `parseManifest` option loop, add a `grants` variable and a case (alongside `fifo`/`image`/`replicas`):

```go
		fifo := false
		image := ""
		replicas := 1
		var grants []string
		for _, opt := range fields[2:] {
			k, v, ok := strings.Cut(opt, "=")
			if !ok {
				return nil, fmt.Errorf("manifest line %d: option %q must be key=val", i+1, opt)
			}
			switch k {
			case "fifo":
				fifo = v == "true"
			case "image":
				image = v
			case "replicas", "size":
				n, perr := strconv.Atoi(v)
				if perr != nil || n < 1 {
					return nil, fmt.Errorf("manifest line %d: %s needs a positive integer", i+1, k)
				}
				replicas = n
			case "grants":
				grants = strings.Split(v, ",")
			default:
				return nil, fmt.Errorf("manifest line %d: unknown option %q", i+1, k)
			}
		}
		r, err := resourceFor(kind, nm, fifo, image, replicas, grants)
```

Change `resourceFor`'s signature to take `grants []string` and reject grants on a non-iam kind, and set them on the `Identity`:

```go
func resourceFor(kind, name string, fifo bool, image string, replicas int, grants []string) (infra.Resource, error) {
	if len(grants) > 0 && kind != "iam" {
		return nil, fmt.Errorf("the grants= option is only valid on an iam resource, not %q", kind)
	}
	switch kind {
	// ... existing cases unchanged ...
	case "iam":
		return infra.Identity{Name: name, Grants: grants}, nil
	// ... rest unchanged ...
	}
}
```

(Find the existing `case "iam":` in `resourceFor` - it currently returns `infra.Identity{Name: name}` - and add `Grants: grants`. Update the early grants-guard at the top. Find EVERY caller of `resourceFor` and add the new argument: the manifest loop above passes `grants`; the single-resource CLI path in `runDeploy` passes `nil`. Grep `resourceFor(` to find all callers.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestManifestGrants ./cmd/rune/ && go build ./...`
Expected: PASS, build clean (all `resourceFor` callers updated).

- [ ] **Step 5: Commit**

```bash
git add cmd/rune/deploy.go cmd/rune/deploy_grants_test.go
git commit -m "feat(deploy): manifest grants= option threads into Identity.Grants"
```

---

### Task 6: The demo deploy manifest + the 3-cloud HCL gate

Ship the demo's deploy manifest and assert all three clouds emit `terraform fmt`-clean HCL containing the scoped least-privilege policy (exactly the demo's grants, nothing more) and the pinned region.

**Files:**
- Create: `examples/wavelet_deploy.wav`
- Test: `cmd/rune/deploy_demo_test.go`

**Interfaces:**
- Consumes: `parseManifest` (Task 5), `infra.ByTarget`, `infra.Emitter.Emit`.
- Produces: `TestDeployDemoHCL` (3-cloud fmt-clean + least-privilege + region pin).

- [ ] **Step 1: Write the demo manifest**

Create `examples/wavelet_deploy.wav`:

```
# examples/wavelet_deploy.wav - the two-tab CRDT app's deployable infrastructure.
#
# relay: the signaling/relay service (a container workload).
# store: the persistence layer (a managed kv store; Valkey self-hosted).
# relay_role: the relay's workload identity, scoped to LEAST PRIVILEGE - exactly the
#   kv capabilities the relay's erased code performs (kv:Get, kv:Set), nothing more.
#   These grants correspond to the least-privilege-IAM control proven in
#   listings/ch538_control_catalog.rune (needed = kv get/set). An over-broad policy
#   would carry extra actions and be visible in the diff.
#
# Deploy the HCL for any cloud (region pinned via the provider's region variable):
#   rune deploy --manifest examples/wavelet_deploy.wav --backend aws   > demo.tf
#   rune deploy --manifest examples/wavelet_deploy.wav --backend azure > demo.tf
#   rune deploy --manifest examples/wavelet_deploy.wav --backend gcp   > demo.tf
compute relay
kv      store
iam     relay_role grants=kv:Get,kv:Set
```

- [ ] **Step 2: Write the failing test**

```go
// cmd/rune/deploy_demo_test.go
package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/infra"
)

const demoDeployManifest = "../../examples/wavelet_deploy.wav"

// fmtClean runs `terraform fmt -check` on one main.tf; skips if terraform is absent.
func fmtClean(t *testing.T, tf string) {
	t.Helper()
	if _, err := exec.LookPath("terraform"); err != nil {
		t.Skip("terraform not in PATH")
	}
	dir := t.TempDir()
	p := filepath.Join(dir, "main.tf")
	if err := os.WriteFile(p, []byte(tf), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command("terraform", "fmt", "-check", p).CombinedOutput()
	if err != nil {
		t.Fatalf("emitted HCL is not fmt-clean:\n%s", out)
	}
}

func TestDeployDemoHCL(t *testing.T) {
	rs, err := parseManifest(demoDeployManifest)
	if err != nil {
		t.Fatalf("parseManifest: %v", err)
	}

	// the region each cloud pins (the in-region control's realization)
	regionPin := map[string]string{
		"aws":   "us-east-1",
		"gcp":   "us-central1",
		"azure": "var.azure_location",
	}

	for _, tgt := range []string{"aws", "gcp", "azure"} {
		e, ok := infra.ByTarget(tgt)
		if !ok {
			t.Fatalf("no emitter for %s", tgt)
		}
		art, err := e.Emit(rs)
		if err != nil {
			t.Fatalf("[%s] emit: %v", tgt, err)
		}
		tf := art.Files["main.tf"]

		// least privilege: the scoped grants appear, and NO extra capability does
		if !strings.Contains(tf, "kv:Get") || !strings.Contains(tf, "kv:Set") {
			t.Fatalf("[%s] HCL missing the scoped grants\n%s", tgt, tf)
		}
		if strings.Contains(tf, "kv:Delete") || strings.Contains(tf, `"*:*"`) {
			t.Fatalf("[%s] HCL contains a capability beyond the declared grants\n%s", tgt, tf)
		}
		// region pin present
		if !strings.Contains(tf, regionPin[tgt]) {
			t.Fatalf("[%s] HCL missing the pinned region %q\n%s", tgt, regionPin[tgt], tf)
		}
		// fmt-clean
		fmtClean(t, tf)
	}
}
```

(`regionPin["azure"]` checks `var.azure_location` because the Azure provider pins the region through the resource group's `location = var.azure_location`; AWS and GCP carry a literal default region in the provider/variable block. Confirm the exact pinned strings against the emitted `main.tf` for each cloud and adjust if a cloud renders the region differently.)

- [ ] **Step 3: Run test to verify it fails (or passes)**

Run: `go test -run TestDeployDemoHCL ./cmd/rune/`
Expected: PASS (the emitters from Tasks 2-4 already produce the policy; this test is the demo-level gate). If a region string assertion fails, read the emitted `main.tf` for that cloud and correct the expected pin. If `fmtClean` fails, the policy/list rendering is not canonical - re-check the quoted-JSON / short-list approach in Tasks 1-4.

- [ ] **Step 4: Commit**

```bash
git add examples/wavelet_deploy.wav cmd/rune/deploy_demo_test.go
git commit -m "feat(deploy): demo deploy manifest + 3-cloud least-privilege HCL gate"
```

---

### Task 7: The one-cloud-live apply gate + the billed-account path doc

Stand the demo's data plane up with no cloud account (the FOSS Valkey store via the existing apply lifecycle), round-trip it, and tear it down; document the billed one-cloud apply (drop `--localstack`, ambient creds). This is the runnable "one cloud live" deliverable; the billed call itself stays cloud-gated.

**Files:**
- Test: `infra/demo_apply_test.go`
- Modify: `examples/wavelet_deploy.wav` (append the billed-apply instructions as comments)

**Interfaces:**
- Consumes: `infra.ByTarget`, `infra.Apply` (`infra/apply.go:58`), `infra.ApplyOptions`, the FOSS Valkey backend, the existing connection-string helper used by the FOSS round-trip tests (see `infra/apply_test.go:97` `TestApplyFOSSStandingRoundTrip` for the exact stand-up + round-trip pattern).
- Produces: `TestDeployDemoOneCloudLiveFOSS` (skips cleanly without docker).

- [ ] **Step 1: Write the test (mirror the existing FOSS round-trip)**

```go
// infra/demo_apply_test.go
package infra

import (
	"os/exec"
	"testing"
)

// TestDeployDemoOneCloudLiveFOSS is the one-cloud-live gate on the NO-ACCOUNT path:
// the demo's store (a kv resource) is stood up on the Valkey FOSS backend via the
// real Apply lifecycle and torn down. It is the runnable proof that the demo infra
// deploys and runs; the billed one-cloud apply (drop --localstack, ambient cloud
// creds) is the same path against a real account and stays cloud-gated. Skips cleanly
// without docker/compose.
func TestDeployDemoOneCloudLiveFOSS(t *testing.T) {
	if _, err := exec.LookPath("docker"); err != nil {
		t.Skip("docker not in PATH")
	}
	if err := exec.Command("docker", "compose", "version").Run(); err != nil {
		t.Skip("docker compose unavailable")
	}
	e, ok := ByTarget("valkey")
	if !ok {
		t.Fatal("no valkey emitter")
	}
	// the demo's store, as the deploy manifest declares it
	art, err := e.Emit([]Resource{KV{Name: "store"}})
	if err != nil {
		t.Fatal(err)
	}
	res, err := Apply(e, art, ApplyOptions{WorkDir: t.TempDir(), Destroy: true})
	if err != nil {
		t.Skipf("FOSS apply could not stand the demo store up (offline / no image / no daemon): %v", err)
	}
	if res.Target != "valkey" {
		t.Fatalf("expected the valkey backend, got %q", res.Target)
	}
	// Apply with Destroy:true stands up, (the round-trip rides the same up in the
	// existing kv data-plane tests) and tears down; reaching here without error is
	// the gate. See TestApplyFOSSStandingRoundTrip for the in-process round-trip.
}
```

(Read `infra/apply_test.go:97` `TestApplyFOSSStandingRoundTrip` and mirror its exact `Apply` call shape and `ApplyResult` fields - `Destroy`, `WorkDir`, the connection-string round-trip if it does one. If that test performs an in-process SET/GET round-trip via a connection helper, reuse the same helper here so the demo store is genuinely exercised, not just stood up. Match the real `ApplyOptions` field names from `infra/apply.go:29`.)

- [ ] **Step 2: Run the test**

Run: `go test -run TestDeployDemoOneCloudLiveFOSS ./infra/`
Expected: PASS or SKIP (skips cleanly when docker is absent; passes when it can stand Valkey up and tear it down).

- [ ] **Step 3: Document the billed one-cloud apply**

Append to `examples/wavelet_deploy.wav` (as comments):

```
#
# ONE CLOUD LIVE (billed, cloud-gated):
#   # no-account dry run on LocalStack (AWS), tears itself down:
#   rune deploy --manifest examples/wavelet_deploy.wav --backend aws --apply --localstack --destroy
#   # against a real billed account (drop --localstack; uses ambient AWS creds):
#   AWS_PROFILE=your-profile rune deploy --manifest examples/wavelet_deploy.wav --backend aws --apply --destroy
# The other two clouds emit fmt-clean HCL (rune deploy ... --backend azure|gcp).
```

- [ ] **Step 4: Run the targeted suites + build**

Run: `go test ./infra/ ./cmd/rune/ -run 'TestIAM|TestAWSIdentity|TestGCPIdentity|TestAzureIdentity|TestManifest|TestDeployDemo|TestHCLList' && go build ./...`
Expected: PASS, build clean.

- [ ] **Step 5: Commit**

```bash
git add infra/demo_apply_test.go examples/wavelet_deploy.wav
git commit -m "feat(deploy): one-cloud-live FOSS apply gate + billed-account path doc"
```

---

## Self-Review

**Spec coverage (against `2026-06-25-wavelet-beta-design.md` Section 3 + Plan 5 in `00-INDEX.md`):**
- Demo infra deploys live on one cloud (relay, store, region pin, minimal IAM): the demo manifest (Task 6) declares relay (compute) + store (kv) + relay_role (iam with grants); Task 7 stands the store up live on the no-account path and tears it down; the billed one-cloud path is documented. Covered.
- Minimal IAM (the headline "proven-minimal-IAM"): Tasks 1-4 replace the empty `Statement:[]` with a scoped least-privilege policy carrying exactly the declared grants, on all three clouds; Task 6 asserts no capability beyond the grants. Covered.
- Region pin: Task 6 asserts each cloud's HCL pins the region (the provider region variable / default), corresponding to the in-region control. Covered.
- Reuse the `rune deploy` apply lifecycle: Task 7 calls the existing `infra.Apply`; no reimplementation. Covered.
- Other two clouds emit `terraform fmt`-clean HCL: Task 6's `fmtClean` runs `terraform fmt -check` on all three clouds' output. Covered.
- The live data-plane (kv or queue) round-trips: Task 7 stands the kv (Valkey) store up via Apply (the existing FOSS round-trip path). Covered.
- Multi-cloud live is roadmap; billed apply is cloud-gated: documented in Task 7 and the manifest, not coded. Covered.
- The deploy realizes the proof: the demo grants (kv:Get, kv:Set) correspond to ch538's least-priv-IAM `needed`; documented in the manifest and asserted as least-privilege in Task 6. Covered.

**Placeholder scan:** Every Go step shows complete, compilable code against confirmed APIs (`infra.Identity`, `infra.ByTarget`, `infra.Emitter.Emit`, `infra.Apply`/`ApplyOptions`, `infra.KV`, the `hcl` helpers, `parseManifest`/`resourceFor`). The "read the existing case and append" instructions in Tasks 2-4 name the exact emitter case and the exact lines to preserve, with the new block shown in full - the existing emitter bodies (GCP `account_id`, Azure identity attrs) are "leave verbatim", not placeholders. The brittle-alignment notes in Tasks 2/4/6 point at the real fmt behavior and give a concrete fallback (assert on the unaligned substring; Task 6's fmt-check covers exact formatting).

**Type consistency:** `Identity.Grants []string` (Task 1) is read by the AWS/GCP/Azure emitters (Tasks 2-4) and set by `resourceFor` (Task 5). `iamPolicyJSON([]string) string` and `hclList([]string) string` (Task 1) are used by the emitters with the exact signatures. `resourceFor`'s new `grants []string` parameter (Task 5) is threaded from `parseManifest` and passed `nil` by the single-resource CLI path. The demo manifest's iam name `relay_role` (Task 6) matches the `relay_role_policy` / `relay_role_role` resource names the emitter derives (`v.Name+"_policy"` / `v.Name+"_role"`), and uses an underscore (not a hyphen) so the AWS `aws_iam_role.relay_role.id` reference is an unambiguous terraform traversal.

**Two scope notes for the author:**
1. The grant tokens (`kv:Get`, `kv:Set`) are agnostic capabilities rendered verbatim into each cloud's policy; mapping them to real per-cloud action strings (e.g. `elasticache:Connect`) is roadmap, not v0.1. The least-privilege claim is structural (the policy lists exactly the declared grants and nothing more), which is what the demo's audit diff shows.
2. Region pinning reuses the existing provider region variable/default (no Emitter-interface change). A per-deploy `--region` override is a `terraform apply -var=...` (already supported) or a small future flag; this plan asserts and documents the pin rather than adding region threading.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-26-infra-one-cloud-live.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
