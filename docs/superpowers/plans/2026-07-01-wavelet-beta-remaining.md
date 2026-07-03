# Wavelet Beta: Remaining Work

Date: 2026-07-01
Source: full-repo review (mission/vision docs + code-state audit + wavelet inventory)
against `2026-06-25-wavelet-beta-design.md` and `00-INDEX.md`.
Status baseline: Plans 1, 2, 3, 4, 5, 5b, 5c, 6a, 6b DONE. This doc is the ordered
checklist of everything between HEAD and a shippable beta.

## What is already true (do not re-plan)

- infra/: 27-row resource matrix x AWS/Azure/GCP, fmt-clean HCL, 15+ FOSS Compose
  backends, apply lifecycle (compose up/down, terraform init/apply, LocalStack
  override), least-privilege IAM applied and read back on LocalStack.
- Queue/KV/Object data plane: typed .rune interfaces, host bodies per backend, live
  RESP round-trip against a real Valkey (Go/JVM/JS/Rust).
- Simulator (internal/sim): kernel-NbE-driven replicas, fault policies, CvRDT law
  linter with counterexamples, liveness check. CLI `rune simulate`.
- Assurance Ledger, control catalog (4 proven flagships, over-broad IAM rejected at
  elaboration), CALM emit/validate round-trip. CLIs `rune ledger`, `rune calm`.
- Eval model (Plan 1): do/par seeded frontier on Go+JS, order-independence proof.
- WASM: ARC runtime (6a), Perceus ownership pass (6b), leak residuals closed +
  PerceusBalanceable re-opened (6b-2, v3.337.0, verified 2026-07-02: realistic
  programs steady-flat), partial support with ARC trampoline (v3.341.0), foreign
  ops, printNat, IO-main, packed-String codec, and `Bin` refcounted bytes over ARC
  (6c, branch `feat/wasm-bin-arc`: `K_BIN` leaf kind + big buckets + full op parity,
  the message-loop shape steady-flat, the >64KB orphan boundary pinned).

## Tier A: the headline artifact (critical path, in dependency order)

1. **WASM bible-ops parity** (in flight, feat/bible-ops-cross-backend-wasm).
   Finish the tier-4 divergence-lock so WASM joins the 8-way byte-identical gate
   for the ops the demo touches. Acceptance: WASM row added to
   `harness/bible_conformance_test.go` (or the demo-relevant subset gate).
2. ~~6b-2: leak residuals~~ DONE (v3.337.0, verified 2026-07-02; all four
   residuals closed, PerceusBalanceable re-opened, realistic programs steady-flat;
   WASM partial support landed too, v3.341.0). Was listed here off the stale index.
3. ~~6c: strings/bytes as refcounted heap objects~~ DONE (branch `feat/wasm-bin-arc`,
   `f4f49d3`..`e00b270`). `K_BIN = 8` packed-byte ARC leaf
   kind + power-of-two big buckets [256B,64KB] in the free list (so a `Bin` payload
   over 256B is still pooled) + full op parity (`binEmpty`/`binCons`/`binLen`/
   `binAt`/`printBin`, the `b"..."` literal, `$show`) joining the ch483 cross-backend
   gate; the steady-flat payoff (`TestPerceusBinMessageLoopFlat`, the 6f message-loop
   shape at zero per-run `$live` delta) and the >64KB orphan-boundary pin
   (`TestARCBinHugeOrphanBalanced`) close it. The packed-String codec (bignum
   packing, landed for bible ops) stays a SEPARATE representation; 6c is the
   heap-object one, now also done.
4. **6d: WebRTC FFI shim.** WASM imports; the sandbox/no-native interop class.
   Smallest sufficient surface: open channel, send bytes, receive callback.
5. **6f: the two-tab CRDT browser app.** WASM merge + JS/WebRTC glue + two divs.
   Reuses a proven CvRDT (G-Counter or MV-Register) and the convergence proof.
6. **Five-outputs integration.** One demo source producing, from the same content
   hashes: (1) proof + ledger view, (2) fault sim run, (3) 3-cloud Terraform,
   (4) the running two-tab app, (5) a CALM doc that re-validates 1:1. The pieces
   all exist; the wiring and a single gate test do not. Acceptance = design-spec
   criteria 2 and 3.
7. **6g: the GTM script / book chapter.** The proven-minimal-IAM diff moment and
   the Ledger view moment, scripted. Depends on 6f and the rename decision (below).

## Tier B: credibility gates (parallelizable with Tier A)

8. **Billed-account apply on one real cloud** (AWS default per index open item 3).
   The lone CLOUD-GATED node in the DAG; also funder milestone M1. Everything
   today is LocalStack/no-account. Needs: credentialed env, `--apply` against real
   AWS, teardown proven, cost cap documented. Design exists in Plan 5; only the
   account is missing.
9. **Azure/GCP local-emulation ceilings made explicit in README** (5c pinned
   Azure; GCP has no emulator story documented at the README level).
10. **Ledger CI gate exercised on this repo itself.** `rune ledger --check` in CI
    so the "CI can gate on it" acceptance criterion is demonstrated, not asserted.
11. **Plan-2 follow-ups:** partial/mutual Pos threading; `--baseline`/Upgrades CLI
    wiring.

## Tier C: productization

12. **Plan 7: the rename.** rune -> wavelet; `wvl`, `wvli`, `wvlc`; module/docs
    consistent; conformance green. RECOMMENDATION from the review: do this BEFORE
    6g and any recorded demo artifacts, not after; a demo video showing `rune`
    verbs is rework at the funder-contact moment. Rename is mechanical (hashes are
    name-independent).
13. **6e: ARC on C + LLVM** (replace mark-sweep). Post-beta acceptable; the demo
    does not need it.
14. **Doc sweep on release:** README matrix count and FOSS list (fixed 2026-07-01),
    index statuses (fixed 2026-07-01), keep R-INFRA.md the single as-built source.

## Explicitly NOT beta scope (docs already say so; hold the line)

- The fully-general all-process refinement under arbitrary interleaving (the
  par-interleave fuel wall / dfix greatest-fixpoint fairness residue).
- D4 general `Array dt sh` ndarray handle; gemm surface sugar.
- Multi-cloud LIVE apply (one cloud live, two emit-only, per the design spec).
- R-UFH, cubical coinduction tails (no consumer).

## Open items needing an author decision

1. RESOLVED 2026-07-02: author confirmed nothing load-bearing was in the
   unreachable claude.ai conversation; index caveat closed.
2. Rename timing (item 12): before or after the demo artifact.
3. Which cloud goes live first for item 8 (AWS is the working default).
