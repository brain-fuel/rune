# Wavelet Beta: Implementation Plans Index

Date: 2026-06-25
Design source: `docs/superpowers/specs/2026-06-25-wavelet-beta-design.md`
Seed: `goforge.dev/rune` @ v3.329.0

## Provenance of this index

Built from: the approved design spec; this Claude Code conversation (the brainstorming
session); `ref_docs/wootz/R-FFI.md` and `R-INTEROP.md` (the locked interop design);
`R-INFRA.md` and the as-built `infra/` package; the verified CRDT and distributed corpus;
`humble-humming-elephant.md` (the grand DAG); project memory and CLAUDE.md.

Two gaps recorded honestly:

- The referenced `claude.ai/chat/622e7420-...` conversation returned HTTP 403 and could not be
  read. No plan assumes its content. If it held decisions, they must be supplied and reconciled.
- The prior plan cited as CALM-related (`calm-soaring-mccarthy.md`) is in fact the
  mutual-recursion/trampoline plan (a slug collision). There is no inherited CALM design; plan #4
  is greenfield on top of the existing `infra/` model.

## Global constraints (every plan honors these)

- **Kernel frozen.** No outer-core changes. Every capability lands as a contained group, library,
  stratum, or tooling layer. Hash-format stays 0x06; new builtin groups get their own hash space;
  singletons delegate to existing adders for zero cache churn; cert-key supersets keep the empty
  assumption-set byte-identical (no cache nuke).
- **Content-addressing and byte-identical erasure are non-negotiable** (the no-leak invariant).
  The gate is `harness/backend_conformance_test.go`: same source, byte-identical observable on
  every backend (pure, partial, IO, FFI, distributed).
- **Backend count is 9 emitters, 8 full-featured.** JS, Py, Go, Rust, BEAM, JVM, C, LLVM are
  full; WASM (added v3.328.49) emits and runs on wasmtime but is pure-compute only (no GC, strings,
  FFI, or IO). Any plan claiming a behavior on "all targets" states which set, and WASM-hardening
  is an explicit dependency where the demo needs it.
- **Reuse, do not rebuild.** The Assurance Ledger reuses R-FFI's tier and `A`-set machinery; CALM
  round-trip reuses the `infra/` Resource model and the `equal-config -> equivalent-deployment`
  equivalence gate; the demo reuses the verified CvRDT corpus (`serveG`, G-Counter, MV-Register,
  vector clocks) and the live-actor projection (the BEAM fault trilogy, ADEQUACY-TIE.md); infra
  reuses `rune deploy` apply lifecycle (FOSS via compose, cloud via terraform, LocalStack override).
- **Process standards.** REPL-acceptance (a surface feature is not done until it works in the REPL
  plus a REPL test); human-grade error diagnostics (the Diagnostic type, parse carets); no em or en
  dashes in any writing; Conventional Commits; incremental verified MO (small green increments,
  additive then delete, consumer-driven conformance, stage backends source then JVM then native,
  verify before claiming done); Standing Rule 1 (cap by demonstrated need); Rule 5 (delete the
  superseded, do not hoard aliases).

## The plans

Listed in dependency order. Each is its own spec-to-plan-to-implementation cycle.

### Plan 1: Evaluation model (lazy core, seq/do, concurrent-effect frontier)

> STATUS: DONE (v3.330.0). Plan + task file: `2026-06-25-eval-model.md`. do/par seeded frontier on Go+JS byte-identical, order-independence proof (ch_frontier_order_independent), REPL acceptance.

- **Goal.** Lazy call-by-need pure layer; effectful actions as a data-flow DAG dispatched M-on-N
  across executors with nondeterministic interleaving; `seq ... end` as the linearization barrier;
  `do` as the nested concurrent block inside `seq`; correctness-under-every-interleaving as a
  first-class proof target.
- **Constraints.** Kernel untouched (laziness and scheduling are codegen and runtime). Reuse the
  existing `seq` desugar and the IO monad. The N-executor runtime maps to the four live concurrency
  backends (Go goroutines, JVM virtual threads, JS async, BEAM native) and must extend to the rest
  under the conformance gate. The scheduler must be seeded so the conformance suite is reproducible
  despite nondeterminism. Recursion already runs flat via the shared-IR trampoline; respect it.
- **Criteria.** `seq` orders effects deterministically; outside `seq`, independent effects can be
  observed in different orders run to run; a worked order-independence proof (a CvRDT converges
  under all schedules) discharges via the existing commutativity and bisimulation machinery; the
  seeded scheduler yields byte-identical conformance across backends; REPL acceptance.
- **Open research.** The fully-general all-process refinement under arbitrary interleaving is the
  genuine open E3 residue. Plan 1 ships the model and the demonstrable order-independence path; it
  does not claim to close the general theorem.

### Plan 2: Assurance Ledger

> STATUS: DONE (v3.332.0). Plan + task file: `2026-06-26-assurance-ledger.md`. `ledger/` package + `rune ledger <file> [--json] [--check]`; tiers postulate/assume/guard/proven, content-hashed proposition+proof, git-blame provenance, upgrade detector, CI gate, `postulate ... because "..." end`. Follow-ups: partial/mutual Pos threading; `--baseline`/Upgrades CLI wiring.

- **Goal.** Tiers `postulate` / `assume` / `guard` / `proven` on every control and foreign claim;
  content-hashed propositions; git-blame provenance; verifiable postulate-to-proof upgrades; a CI
  gate.
- **Constraints.** Reuse R-FFI's `A`-set (assume), the codegen guard-flag set (guard), and
  discharge (proven); `postulate` is an axiom with no contract clause. Read both the static `A`-set
  and the runtime guard-flags to be complete (guard does not enter `A` by design). `rune
  assumptions <def>` and `check --safe` are the substrate. Kernel untouched (a postulate is a
  bodiless content-addressed definition). Diagnostics conform to the Diagnostic standard.
- **Criteria.** The emitted artifact labels each control with tier, proposition-hash, proof-hash
  when proven, and reason when postulated; `git blame` resolves committer and date per witness;
  replacing a postulate with a proof of the same proposition-hash is detected and reported as an
  upgrade; CI fails on a new postulate without sign-off and on a flagship control leaving `proven`.

### Plan 3: Control catalog

> STATUS: DONE. Task file: `2026-06-26-control-catalog.md`. 8 tasks landed: catalog listing `ch538` (model + 4 proven flagships in-region/encrypted/least-priv-IAM/convergence reusing ch453) + postulated tail `liveInRegion`; over-broad-IAM rejected at elaboration (`TestOverBroadIAMRejected`); `control/` registry (flagship set + CALM element map); plugs into the Plan-2 ledger + gate (`TestCatalogLedgerTiers`/`TestCatalogGatePassesAndFails`). Whole-branch review: ready to merge.

- **Goal.** A small blessed set of controls fully proven (least-privilege IAM, in-region,
  encrypted-in-transit, CRDT-convergence-under-all-schedules) plus the guarded-tail framework.
- **Constraints.** Each control is a content-hashed proposition over the architecture model. The
  IAM control is a set-equality or subset proof over the access relation. Convergence reuses the
  CvRDT corpus. Tail controls are guarded with explicit labels via Plan 2.
- **Criteria.** Each flagship control proven on the demo model; a deliberately over-broad policy is
  rejected (the proof does not go through); the catalog plugs into the Ledger and the CALM emit.

### Plan 4: CALM round-trip

> STATUS: PLANNED. Task file: `2026-06-26-calm-round-trip.md`. 7 tasks: new `calm/` package (CALM v0.1 doc model + JSON; node-type map + ledger-backed Model + BuildModel; ToDoc/Reconstruct; Emit with nodes=logical-resource-set; Validate 1:1 structural + macro-micro assurance tie via Diagnostic); `rune calm emit|validate` CLI + `examples/wavelet_demo.wav`; e2e demo round-trip + live proof-change detection. Builds on infra/ (nodes), control/ (Plan 3, attachments), ledger/ (Plan 2, assurance). Greenfield, no kernel change.

- **Goal.** Language-to-CALM emit plus ingest-for-validation; 1:1 macro-to-micro traceability.
- **Constraints.** Greenfield (no prior CALM plan). Build on the `infra/` Resource model and the
  `equal-config -> equivalent-deployment` equivalence gate. Map `infra.Kind` and LogicalResource to
  CALM nodes and relationships; controls attach to relationships. The language is authoritative;
  CALM is a projection that round-trips. CALM is the FINOS Common Architecture Language Model JSON
  schema; v0.1 covers the demo's control set only (arbitrary ingest is roadmap).
- **Criteria.** The emitted CALM doc validates against the source 1:1; a CALM node round-trips to
  the same Wavelet service value whose control proofs hold; a mismatch produces a Diagnostic.

> NOTE (added during Plan 4): "wvl deploy" in Plan 5's criteria is aspirational; the CLI rename to wvl is Plan 7. Until then the verb is `rune deploy`.

### Plan 5: Wavelet infra, one cloud live

> STATUS: DONE (v3.335.0). Task file: `2026-06-26-infra-one-cloud-live.md`. 7 tasks landed: Identity.Grants + scoped least-privilege policy on AWS (aws_iam_role_policy) / GCP (custom role) / Azure (role definition), manifest `grants=` option, `examples/wavelet_deploy.wav`, a 3-cloud fmt-clean + terraform-validate least-privilege HCL gate, and the one-cloud-live FOSS apply gate reusing infra.Apply. Grants correspond to ch538's least-priv-IAM `needed`. (Whole-branch review caught + fixed: GCP account_id underscore failed `terraform validate`.)
>
> FOLLOW-UPS (emulator apply-gates for the demo's IAM, no account):
> - Plan 5b: `2026-06-27-aws-localstack-iam-apply.md` (PLANNED). 3 tasks: apply the demo's scoped AWS IAM on LocalStack no-account (provider accepts CreateRole+PutRolePolicy), then read the inline policy back via the IAM API and assert it is EXACTLY kv:Get/kv:Set (live-provider macro-micro tie), + doc. Reuses the TestApplyLocalStackBucketReallyCreated pattern.
> - Plan 5c: `2026-06-27-azure-iam-noaccount-ceiling.md` (PLANNED). 2 tasks: pin Azure's honest no-account ceiling - the scoped role definition is terraform-validated no-account but emulator-apply is impossible-by-design (azurerm=ARM control plane; Azurite=storage data plane); FOSS-via-Podman covers data shapes not IAM. Test + doc, no fake emulator.

- **Goal.** The demo infra (relay, store, region pin, minimal IAM) deploys live on one cloud; the
  other two emit HCL only.
- **Constraints.** Reuse the `rune deploy` apply lifecycle. One-cloud-live is the existing apply
  engine minus the LocalStack override plus credentials from environment. Multi-cloud live is
  roadmap.
- **Criteria.** `wvl deploy --apply` provisions the demo on one cloud and tears it down; the live
  data-plane (kv or queue) round-trips; the other two clouds produce valid `terraform fmt`-clean HCL.

### Plan 6: The demo artifact (2-tab CRDT)

- **Goal.** A shared counter or note CRDT in two browser tabs over WebRTC, wired to plans 1 through
  5, plus the go-to-market script or book chapter.
- **Constraints.** Bounded browser scope: WASM merge plus JS and WebRTC glue plus two divs, not a
  platform. **Forces WASM-hardening** (GC closure runtime, strings and bytes, and the WebRTC FFI
  shim in the no-native/sandbox interop class). Reuse a proven CvRDT and the live-actor projection.
- **Criteria.** Two tabs converge live; the merge is proven convergent under all schedules; the same
  source runs byte-identical across the live backends; one source produces all five outputs (proof,
  sim, Terraform, running app, validating CALM); the script lands the proven-minimal-IAM and Ledger
  moments.

### Plan 7: Rune to Wavelet rename

- **Goal.** Rename the language and tooling to Wavelet; CLI `wvl` (umbrella), `wvli` (REPL or
  interpreter), `wvlc` (compiler).
- **Constraints.** Pick one terminal name and delete the superseded ones (Rule 5); do not layer a
  third alias set over the in-progress Rune-to-Wootz state. The CLI verbs wrap the existing `rune
  deploy`, `rune simulate`, `rune run`, and `rune emit|run --target`. Content-addressed identities
  are name-independent, so the rename does not perturb hashes. REPL acceptance for `wvl repl`.
- **Criteria.** No user-facing `rune` remains; `wvl`, `wvli`, `wvlc` work; the module path and docs
  are consistent; the conformance suite stays green.

## Open items for the author

1. Supply any decisions from the unreachable `claude.ai` conversation, or confirm none are load-bearing.
2. Confirm WASM-hardening is in beta scope (forced by Plan 6) rather than deferring the browser facet.
3. Confirm the one live cloud for Plan 5 (AWS is the working default).
