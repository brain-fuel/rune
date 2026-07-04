# Five-Outputs Integration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One demo source set produces all five beta outputs (ledger, fault sim, 3-cloud Terraform, the two-tab app, a re-validating CALM doc) under one gate test that pins the content-hash binding between the proven convergence control and the merge the app deploys.

**Architecture:** Unify the two overlapping infra manifests into `examples/wavelet_demo.rune` (consumed by both `rune deploy` and `rune calm`); re-prove ch538's convergence control over the app's actual GC protocol so the ledger/CALM proof hashes literally equal the deployed merge's hash (content addressing does the binding); add sim-convention aliases to the app source; assemble `TestFiveOutputs` in `cmd/rune` driving all five outputs plus the hash pin.

**Tech Stack:** Go (cmd/rune CLI internals: runLedger/runSimulate/runDeploy/runCalm, internal/session), rune surface language (listings/ch538, examples/twotab/counter.rune), terraform fmt/validate (skip-gated), node+wabt (skip-gated).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-04-five-outputs-integration-design.md`.
- Manifest files use the `.rune` extension from this work onward (author amendment); NO `.wav` files remain in the repo. They become `.wvl` only at the Plan 7 rename (not now).
- NO changes to codegen/, core/, store/, ch565's proofs, or the twotab app/glue (examples/twotab/*.js, *.mjs, *.html, build scripts).
- `control.Catalog()` in control/control.go is UNCHANGED (same names, kinds, elements).
- Rule 5 (delete the superseded): `examples/wavelet_deploy.wav` and ch538's Reg max-register block are deleted, not kept alongside.
- Historical records (docs/superpowers/plans/*, docs/superpowers/specs/* other than this feature's own docs) are NOT rewritten for the extension change. Living docs (README.md, examples/README.md, examples/WALKTHROUGH.md, PARKING-LOT.md, ref_docs/wootz/R-INFRA.md, pitch/, CLAUDE.md) ARE migrated.
- Conventional Commits. Run package tests per task; the controller runs the full `go test -timeout 30m ./...` before merge.
- Multiple agents may share this repo's object store; commit with explicit pathspecs (`git commit -m "..." -- path1 path2`), never bare `git commit -a`.

---

### Task 1: One manifest, `.rune` extension everywhere

**Files:**
- Create: `examples/wavelet_demo.rune` (unified content below)
- Delete: `examples/wavelet_demo.wav`, `examples/wavelet_deploy.wav`
- Rename: `examples/app.wav` -> `examples/app.rune`, `examples/serving.wav` -> `examples/serving.rune`
- Modify: `cmd/rune/calm.go:29`, `cmd/rune/calm_test.go:12` and its 3-node assertion (line ~33), `cmd/rune/deploy_demo_test.go:14`, `cmd/rune/deploy_manifest_test.go:56-68`, `cmd/rune/deploy.go:141` (comment)
- Modify (docs): `README.md`, `examples/README.md`, `examples/WALKTHROUGH.md`, `PARKING-LOT.md`, `ref_docs/wootz/R-INFRA.md`, `pitch/01-DEMO.md`, `pitch/04-CLOUD-PLAN.md`, `CLAUDE.md` — every `app.wav`/`serving.wav`/`wavelet_demo.wav`/`wavelet_deploy.wav` reference becomes the `.rune` name (wavelet_deploy references become `wavelet_demo.rune` since the file is unified away)
- Test: existing `cmd/rune` suite (TestDeployDemoHCL, TestExampleManifest, calm tests)

**Interfaces:**
- Produces: `examples/wavelet_demo.rune` — the ONE manifest both `rune deploy --manifest` and `rune calm` (default path) consume. Task 4's gate reads this exact path.

- [ ] **Step 1: Write the unified manifest**

Create `examples/wavelet_demo.rune` with exactly:

```
# examples/wavelet_demo.rune - the two-tab collaborative CRDT app (Wavelet beta demo):
# ONE manifest for BOTH the architecture model (rune calm) and the deployable
# infrastructure (rune deploy). Four nodes:
#
# web: the browser client (the two-tab page).
# relay: the signaling/relay service (a container workload).
# store: the persistence layer (a managed kv store; Valkey self-hosted).
# relay_role: the relay's workload identity, scoped to LEAST PRIVILEGE - exactly the
#   kv capabilities the relay's erased code performs (kv:Get, kv:Set), nothing more.
#   These grants correspond to the least-privilege-IAM control proven in
#   listings/ch538_control_catalog.rune (needed = kv get/set). An over-broad policy
#   would carry extra actions and be visible in the diff.
#
# The control catalog (control.Catalog) attaches the blessed controls by name:
# in-region + convergence + the live-region tail on the store, least-privilege-IAM
# on the relay, encrypted-in-transit on the web->relay link.
#
#   rune calm emit                       > demo.calm.json
#   rune calm validate demo.calm.json
#   rune deploy --manifest examples/wavelet_demo.rune --backend aws   > demo.tf
#   rune deploy --manifest examples/wavelet_demo.rune --backend azure > demo.tf
#   rune deploy --manifest examples/wavelet_demo.rune --backend gcp   > demo.tf
#
# ONE CLOUD LIVE (billed, cloud-gated):
#   # no-account dry run on LocalStack (AWS), tears itself down:
#   rune deploy --manifest examples/wavelet_demo.rune --backend aws --apply --localstack --destroy
#   # against a real billed account (drop --localstack; uses ambient AWS creds):
#   AWS_PROFILE=your-profile rune deploy --manifest examples/wavelet_demo.rune --backend aws --apply --destroy
# The other two clouds emit fmt-clean HCL (rune deploy ... --backend azure|gcp).
#
# NO-ACCOUNT IAM (applied + observed): the scoped relay_role policy is apply-tested
# on AWS LocalStack (infra/iam_localstack_apply_test.go) - the provider accepts and
# stores exactly kv:Get/kv:Set. The kv/compute slices need a real account.
#
# AZURE no-account ceiling: azurerm has no terraform emulator, so the demo IAM cannot
# be applied no-account. It IS terraform-validated no-account (provider-valid), and
# Azure's data shapes use FOSS-via-Podman; real IAM needs a billed account
# (cmd/rune/azure_iam_noaccount_test.go).
paas    web
compute relay
kv      store
iam     relay_role grants=kv:Get,kv:Set
```

- [ ] **Step 2: Rename/delete the .wav files**

```bash
git mv examples/app.wav examples/app.rune
git mv examples/serving.wav examples/serving.rune
git rm examples/wavelet_demo.wav examples/wavelet_deploy.wav
git add examples/wavelet_demo.rune
```

- [ ] **Step 3: Migrate the Go consumers**

- `cmd/rune/calm.go:29`: `manifest := "examples/wavelet_demo.wav"` -> `manifest := "examples/wavelet_demo.rune"`
- `cmd/rune/calm_test.go:12`: `const demoManifest = "../../examples/wavelet_demo.wav"` -> `"../../examples/wavelet_demo.rune"`
- `cmd/rune/calm_test.go` (~line 33): the emitted demo doc now has FOUR nodes (web, relay, store, relay_role): `if len(doc.Nodes) != 3` -> `!= 4`, and the message `"want 3 nodes..."` -> `"want 4 nodes..."`. Scan the rest of calm_test.go and calm_e2e_test.go for any other node-count or node-list assertions and extend them with `relay_role` (kind `iam`).
- `cmd/rune/deploy_demo_test.go:14`: `const demoDeployManifest = "../../examples/wavelet_deploy.wav"` -> `"../../examples/wavelet_demo.rune"`
- `cmd/rune/deploy_manifest_test.go`: `"../../examples/app.wav"` -> `"../../examples/app.rune"` (3 occurrences incl. messages), comment on line 56.
- `cmd/rune/deploy.go:141`: comment `app.wav` -> `app.rune`.

- [ ] **Step 4: Run the cmd/rune suite**

Run: `go test -timeout 20m ./cmd/rune/`
Expected: PASS. TestDeployDemoHCL now emits from the unified manifest — the `paas web` row emits alongside relay/store/iam on all three clouds; the grants/region assertions still hold. If a CALM test fails on node shape, fix its expectation per Step 3 (source-model change is intended).

- [ ] **Step 5: Migrate the living docs**

In `README.md`, `examples/README.md`, `examples/WALKTHROUGH.md`, `PARKING-LOT.md`, `ref_docs/wootz/R-INFRA.md`, `pitch/01-DEMO.md`, `pitch/04-CLOUD-PLAN.md`, `CLAUDE.md`: replace `app.wav` -> `app.rune`, `serving.wav` -> `serving.rune`, `wavelet_demo.wav` -> `wavelet_demo.rune`, `wavelet_deploy.wav` -> `wavelet_demo.rune`. Where a sentence described TWO manifests, rewrite to the one unified manifest. Do NOT touch docs/superpowers/plans/* or older specs.

- [ ] **Step 6: Verify no stragglers and commit**

Run: `grep -rn "\.wav" --include="*.go" . ; ls examples/*.wav 2>/dev/null`
Expected: no Go hits, no files. (docs/superpowers historical hits are fine.)

```bash
git add -A examples/ cmd/rune/ README.md PARKING-LOT.md CLAUDE.md ref_docs/wootz/R-INFRA.md pitch/
git commit -m "feat(examples): unify demo+deploy manifests into wavelet_demo.rune; .wav -> .rune" -- examples/ cmd/rune/ README.md PARKING-LOT.md CLAUDE.md ref_docs/wootz/R-INFRA.md pitch/
```

---

### Task 2: ch538 convergence over the app's GC + the hash-binding test

**Files:**
- Create: `cmd/rune/five_outputs_test.go`
- Modify: `listings/ch538_control_catalog.rune:139-296` (the convergence section)
- Test: the new binding test + `go test ./harness/ -run 'Listings|ControlCatalog'` + `go test ./cmd/rune/ -run 'Ledger|Calm'`

**Interfaces:**
- Consumes: `internal/session.Session.Lookup(name string) (core.Hash, bool)`; `session.New().LoadSource(src)`.
- Produces: ch538 defines `GC`, `merge : GC -> GC -> GC`, `convergesProof : Conv GC merge` (name unchanged — control.Catalog() untouched). `TestFiveOutputsHashBinding` in `cmd/rune/five_outputs_test.go` (Task 4 adds the other subtests to this file).

- [ ] **Step 1: Write the failing hash-binding test**

Create `cmd/rune/five_outputs_test.go`:

```go
// cmd/rune/five_outputs_test.go
// The beta success-criterion-2 gate: one demo source set produces all five
// outputs, and the convergence control the ledger/CALM doc carries is about
// the SAME content hash as the merge the two-tab app deploys.
package main

import (
	"os"
	"testing"

	"goforge.dev/rune/v3/internal/session"
)

const (
	fiveOutputsApp      = "../../examples/twotab/counter.rune"
	fiveOutputsListing  = "../../listings/ch538_control_catalog.rune"
	fiveOutputsManifest = "../../examples/wavelet_demo.rune"
)

func loadFile(t *testing.T, path string) *session.Session {
	t.Helper()
	src, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		t.Fatalf("%s did not load: %v", path, err)
	}
	return s
}

// TestFiveOutputsHashBinding pins criterion 2's "same content hashes": the
// merge the browser tabs run (counter.rune) and the merge the convergence
// control is proven about (ch538) are the SAME definition, literally - equal
// content hashes. Content addressing does the binding; any drift is CI-fatal.
func TestFiveOutputsHashBinding(t *testing.T) {
	app := loadFile(t, fiveOutputsApp)
	cat := loadFile(t, fiveOutputsListing)
	for _, name := range []string{"GC", "merge"} {
		ha, ok := app.Lookup(name)
		if !ok {
			t.Fatalf("app source does not define %q", name)
		}
		hc, ok := cat.Lookup(name)
		if !ok {
			t.Fatalf("control catalog does not define %q", name)
		}
		if ha != hc {
			t.Errorf("%q hash mismatch: app %s vs catalog %s - the proven control is not about the deployed merge", name, ha, hc)
		}
	}
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `go test ./cmd/rune/ -run TestFiveOutputsHashBinding -v`
Expected: FAIL — ch538's `merge` is over `Reg` today (`"GC"` not defined / hash mismatch).

- [ ] **Step 3: Swap ch538's convergence section to the app's GC**

In `listings/ch538_control_catalog.rune`, replace the whole region from the line `-- ===== convergence: the verified max-register CvRDT (reused from ch453) =====` (line 139) through the runnable-witness block's final `end` of `main` (line ~296, JUST BEFORE the `-- ===== the guarded-tail framework...` comment) with:

1. A new section comment:
```
-- ===== convergence: the two-tab demo's G-Counter (the SHAPES ARE THE APP'S) =====
-- add/max/GC/value/merge below are copied VERBATIM from examples/twotab/counter.rune
-- (the source the browser tabs deploy). Content addressing binds them: identical
-- structure = identical hash, so the convergesProof the ledger/CALM doc carries is
-- about the EXACT merge the app runs (TestFiveOutputsHashBinding pins it). Edit the
-- shapes there, copy back here; the proofs below are the gcounter.rune/ch72 family.
```
2. `add`, `max` copied VERBATIM from `examples/twotab/counter.rune` (lines 21-37 for `add`... use the current file: `add` is the `case`-style two-branch def; `max` is counter.rune lines 225-238).
3. `data GC ... end`, `value`, `merge` copied VERBATIM from `examples/twotab/counter.rune` (lines 211-245: `data GC : U is gc : Nat -> Nat -> GC end`, `value` via case, `merge` via nested case). Do NOT copy `initGC`/`bump`/codec/foreign defs — ch538 needs only the proposition's subjects.
4. The proof block copied from `examples/gcounter.rune` lines 57-227: `symNat`, `congSucc`, `maxZeroR`, `maxComm`, `maxIdem`, `maxAssoc`, `congGC`, `mergeComm`, `mergeIdem`, `mergeAssoc` (these are proven against the definitional max/merge equations, which the case-style defs satisfy identically; if any proof fails to elaborate, the definitional equations differ — STOP and report, do not hand-patch proofs).
5. The `Conv` data decl and `convergesProof` (keep ch538's existing `Conv` verbatim), with the proof now:
```
convergesProof : Conv GC merge is conv GC merge mergeComm mergeIdem mergeAssoc end
```
6. The runnable witness, GC-shaped (replaces the Reg r3/r7 one; `foreign printNat` line kept):
```
-- ===== a runnable witness (the Lambert spirit gate: it deploys + runs) =====
-- Replica A counted 2, replica B counted 1; either merge order observes 3.
foreign printNat : Nat -> IO Nat end
replicaA : GC is gc (succ (succ zero)) zero end
replicaB : GC is gc zero (succ zero) end

main : IO Unit is
  bindIO Nat Unit (printNat (value (merge replicaA replicaB))) (fn (u1 : Nat) is
    bindIO Nat Unit (printNat (value (merge replicaB replicaA))) (fn (u2 : Nat) is
      pureIO Unit unit
    end)
  end)
end
```

DELETED by this replacement: `cong`, `symEq`, `natMax`, the natMax law family, `data Reg`, `val`, the Reg `merge` + its three laws, `r3`, `r7`, the old `main`. Before deleting `cong`/`symEq`, grep ch538 above line 139 for uses (`grep -n "cong \|symEq" listings/ch538_control_catalog.rune`); if the in-region/IAM sections use them, keep those two defs. Update ch538's HEADER comment (lines 1-14) to say the convergence control is proven over the two-tab app's GC (hash-bound to examples/twotab/counter.rune), not the max-register.

- [ ] **Step 4: Run the binding test to verify it passes**

Run: `go test ./cmd/rune/ -run TestFiveOutputsHashBinding -v`
Expected: PASS. If `merge` hashes differ while both load, the case-desugar of counter.rune's shapes and your copy diverge — diff your copied block against counter.rune character-by-character (comments don't matter; structure does; also confirm `add`/`max` were BOTH copied from counter.rune, since `merge`'s hash depends on `max`'s and `value`'s on `add`'s).

- [ ] **Step 5: Run the neighbors**

Run: `go test -timeout 20m ./harness/ -run 'Listings|ControlCatalog' && go test -timeout 10m ./cmd/rune/ -run 'Ledger|Calm'`
Expected: PASS — ch538 still elaborates+checks under the listings gate; the sabotage tests in harness/control_catalog_test.go still pass (they target the IAM/in-region folds, untouched); ledger renders convergesProof as `proven`; CALM emit/validate round-trips with the new proof hash.

- [ ] **Step 6: Commit**

```bash
git add listings/ch538_control_catalog.rune cmd/rune/five_outputs_test.go
git commit -m "feat(listings): ch538 convergence control proven over the two-tab app's GC (hash-bound)" -- listings/ch538_control_catalog.rune cmd/rune/five_outputs_test.go
```

---

### Task 3: Simulate the app source directly

**Files:**
- Modify: `examples/twotab/counter.rune` (append 3 aliases at end of file)
- Test: add `TestFiveOutputsSim` to `cmd/rune/five_outputs_test.go`

**Interfaces:**
- Consumes: `runSimulate(src string, n int, out io.Writer) error` (cmd/rune/main.go) — the simulate convention needs `init`, `op0`, `op1`, `merge`, `value`.
- Produces: `examples/twotab/counter.rune` satisfies the simulate convention; Task 4's gate lists Sim as covered.

- [ ] **Step 1: Write the failing test**

Append to `cmd/rune/five_outputs_test.go`:

```go
// TestFiveOutputsSim folds the EXACT source the app deploys under the
// partition-and-heal schedule and the CvRDT law linter (output 2 of 5).
func TestFiveOutputsSim(t *testing.T) {
	src, err := os.ReadFile(fiveOutputsApp)
	if err != nil {
		t.Fatal(err)
	}
	var out strings.Builder
	if err := runSimulate(string(src), 2, &out); err != nil {
		t.Fatalf("simulate: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "verdict: CONVERGED") {
		t.Fatalf("app protocol did not converge under the fault schedule:\n%s", got)
	}
	if !strings.Contains(got, "join laws hold") {
		t.Fatalf("CvRDT law linter did not certify the join:\n%s", got)
	}
}
```

Add `"strings"` to the test file's imports.

- [ ] **Step 2: Run it to verify it fails**

Run: `go test ./cmd/rune/ -run TestFiveOutputsSim -v`
Expected: FAIL — counter.rune defines `initGC`/`bump`, not `init`/`op0`/`op1` (runSimulate errors on the missing `init`).

- [ ] **Step 3: Append the aliases to counter.rune**

At the very end of `examples/twotab/counter.rune`:

```
-- ============================================================================
-- The simulate convention (rune simulate examples/twotab/counter.rune 2):
-- thin aliases so the fault simulator folds THIS deployed source directly.
-- ============================================================================

init : GC is initGC end

op0 : GC -> GC is bump end

op1 : GC -> GC is bump end
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `go test ./cmd/rune/ -run 'TestFiveOutputsSim|TestFiveOutputsHashBinding' -v`
Expected: both PASS (the aliases must not perturb the GC/merge hashes — aliases are new defs, not edits).

- [ ] **Step 5: Verify the twotab build is unaffected**

Run: `go test -timeout 20m ./harness/ -run 'TestTwoTab|TestWasmBrowserLibrary'`
Expected: PASS (or clean SKIP where node/wabt absent). The aliases are inert for the library build (exports are explicit flags in build.mjs).

- [ ] **Step 6: Commit**

```bash
git add examples/twotab/counter.rune cmd/rune/five_outputs_test.go
git commit -m "feat(examples): twotab counter satisfies the simulate convention (init/op0/op1)" -- examples/twotab/counter.rune cmd/rune/five_outputs_test.go
```

---

### Task 4: The five-outputs gate

**Files:**
- Modify: `cmd/rune/five_outputs_test.go` (add TestFiveOutputsLedger, TestFiveOutputsTerraform, TestFiveOutputsApp, TestFiveOutputsCALM)
- Test: itself

**Interfaces:**
- Consumes: `runLedger(args []string, w io.Writer) error`, `runDeploy(args []string, w io.Writer) error`, `runCalm(args []string, out io.Writer) error` (all cmd/rune package-internal), `fmtClean(t, tf)` (deploy_demo_test.go, same package). NOTE: runCalm/buildSourceModel default paths are relative to the REPO ROOT (`examples/...`, `listings/...`) but tests run in `cmd/rune/` — pass the `../../`-prefixed constants explicitly via `--manifest`/`--listing` flags.
- Produces: the criterion-2 gate, five sub-outputs + binding, all named `TestFiveOutputs*`.

- [ ] **Step 1: Write the four remaining output tests**

Append to `cmd/rune/five_outputs_test.go`:

```go
// TestFiveOutputsLedger renders the assurance ledger from the control listing
// (output 1 of 5) and requires the convergence control to be tier `proven`.
func TestFiveOutputsLedger(t *testing.T) {
	var out strings.Builder
	if err := runLedger([]string{fiveOutputsListing, "--json"}, &out); err != nil {
		t.Fatalf("ledger: %v", err)
	}
	got := out.String()
	if !strings.Contains(got, "convergesProof") {
		t.Fatalf("ledger JSON missing the convergence control:\n%s", got)
	}
	s := loadFile(t, fiveOutputsListing)
	for _, e := range ledger.Build(s) {
		if e.Name == "convergesProof" {
			if e.Tier.String() != "proven" {
				t.Fatalf("convergesProof tier = %q, want proven", e.Tier.String())
			}
			return
		}
	}
	t.Fatal("convergesProof has no ledger entry")
}

// TestFiveOutputsTerraform emits the demo architecture for all three clouds
// from the ONE manifest (output 3 of 5): fmt-clean, least-priv grants present.
func TestFiveOutputsTerraform(t *testing.T) {
	for _, backend := range []string{"aws", "azure", "gcp"} {
		var out strings.Builder
		if err := runDeploy([]string{"--manifest", fiveOutputsManifest, "--backend", backend}, &out); err != nil {
			t.Fatalf("[%s] deploy: %v", backend, err)
		}
		tf := out.String()
		if !strings.Contains(tf, "kv:Get") || !strings.Contains(tf, "kv:Set") {
			t.Fatalf("[%s] HCL missing the scoped IAM grants", backend)
		}
		fmtClean(t, tf)
	}
}

// TestFiveOutputsApp builds the running two-tab app from the same source
// (output 4 of 5). Skips cleanly without node/wabt, like the harness gates.
func TestFiveOutputsApp(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil {
		t.Skip("node not in PATH")
	}
	if _, err := os.Stat("../../harness/node_modules/wabt"); err != nil {
		t.Skip("wabt not installed (bin/setup.sh section 7)")
	}
	cmd := exec.Command("node", "build.mjs")
	cmd.Dir = "../../examples/twotab"
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("twotab build failed: %v\n%s", err, out)
	}
	for _, f := range []string{"counter.wasm", "counter.glue.js"} {
		if _, err := os.Stat("../../examples/twotab/" + f); err != nil {
			t.Fatalf("twotab build did not produce %s", f)
		}
	}
}

// TestFiveOutputsCALM emits the CALM doc from the one manifest + listing and
// re-validates it 1:1 against the source (output 5 of 5, the round-trip).
func TestFiveOutputsCALM(t *testing.T) {
	var doc strings.Builder
	if err := runCalm([]string{"emit", "--manifest", fiveOutputsManifest, "--listing", fiveOutputsListing}, &doc); err != nil {
		t.Fatalf("calm emit: %v", err)
	}
	f := filepath.Join(t.TempDir(), "demo.calm.json")
	if err := os.WriteFile(f, []byte(doc.String()), 0o644); err != nil {
		t.Fatal(err)
	}
	var out strings.Builder
	if err := runCalm([]string{"validate", f, "--manifest", fiveOutputsManifest, "--listing", fiveOutputsListing}, &out); err != nil {
		t.Fatalf("calm validate: %v", err)
	}
	if !strings.Contains(out.String(), "validates against the source 1:1") {
		t.Fatalf("unexpected validate output: %s", out.String())
	}
}
```

Extend the file's imports to `"os"`, `"os/exec"`, `"path/filepath"`, `"strings"`, `"testing"`, plus `"goforge.dev/rune/v3/internal/session"` and `"goforge.dev/rune/v3/ledger"`.

- [ ] **Step 2: Run the whole gate**

Run: `go test -timeout 20m ./cmd/rune/ -run TestFiveOutputs -v`
Expected: HashBinding/Sim/Ledger/Terraform/CALM PASS; App PASS where node+wabt exist, else SKIP. If CALM validate fails on node count or an iam node, the calm_test expectations from Task 1 Step 3 were incomplete — fix the source-model expectation, never the validator.

- [ ] **Step 3: Run the package**

Run: `go test -timeout 20m ./cmd/rune/`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add cmd/rune/five_outputs_test.go
git commit -m "test(cmd/rune): TestFiveOutputs gate - five outputs from one source + hash binding" -- cmd/rune/five_outputs_test.go
```

---

### Task 5: Docs — the five-outputs walkthrough

**Files:**
- Modify: `examples/WALKTHROUGH.md` (new section), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (strike item 6)

**Interfaces:**
- Consumes: the five commands as they exist after Tasks 1-4.

- [ ] **Step 1: Add the five-outputs section to examples/WALKTHROUGH.md**

After the existing pipeline sections, add:

```markdown
## The five outputs — one source set, one gate

Beta success criterion 2, checkable: from {`examples/twotab/counter.rune`,
`examples/wavelet_demo.rune`, `listings/ch538_control_catalog.rune`} the same
content hashes produce all five outputs:

```sh
rune ledger listings/ch538_control_catalog.rune            # 1 proof + ledger view
rune simulate examples/twotab/counter.rune 2               # 2 fault sim (partition + heal)
rune deploy --manifest examples/wavelet_demo.rune --backend aws   > demo.tf   # 3 Terraform
rune deploy --manifest examples/wavelet_demo.rune --backend azure > demo.tf   #   (x3 clouds)
rune deploy --manifest examples/wavelet_demo.rune --backend gcp   > demo.tf
(cd examples/twotab && node build.mjs)                     # 4 the running two-tab app
rune calm emit > demo.calm.json && rune calm validate demo.calm.json   # 5 CALM, re-validates 1:1
```

The binding is literal: the convergence control in the ledger/CALM doc and the
`merge` the browser tabs run are the SAME content hash (`rune hash` shows it;
`TestFiveOutputs*` in cmd/rune gates it, including the hash pin).
```

- [ ] **Step 2: Strike item 6 in the beta checklist**

In `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md`, mark item 6 (Five-outputs integration) DONE with a one-line note: gate `TestFiveOutputs*` in cmd/rune; unified manifest `examples/wavelet_demo.rune`; ch538 convergence hash-bound to the app's GC.

- [ ] **Step 3: Commit**

```bash
git add examples/WALKTHROUGH.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "docs: five-outputs walkthrough section; beta checklist item 6 done" -- examples/WALKTHROUGH.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```
