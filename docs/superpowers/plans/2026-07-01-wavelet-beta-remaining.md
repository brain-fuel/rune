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
4. ~~6d: WebRTC FFI shim~~ DONE (branch `feat/wasm-browser-library`,
   `420cb7d`..`50bdd09`). The design narrowed to a passive, browser-
   consumable WASM LIBRARY artifact rather than a WebRTC-specific shim: a proven
   G-Counter wire codec (`ch565_gc_codec.rune`), `codegen.Wasm.EmitLibrary`
   (library-mode WAT export ABI + generated `glue.js`, app-mode emission
   untouched), the node+wabt harness substrate, and a two-instance convergence
   gate (`TestWasmBrowserLibraryConverge`) proving two independent module
   instances gossip a G-Counter's state as plain bytes and converge under both
   gossip orders, live-delta 0. 6f's remaining surface: WebRTC signaling + DOM +
   two tabs consuming `glue.js` verbatim.
5. ~~6f: the two-tab CRDT browser app~~ DONE (branch `feat/twotab-demo`,
   `7049c63`..`209ca42`). `examples/twotab`: a static page (`index.html`/
   `app.js`) driving a G-Counter compiled to `counter.wasm` via 6d's
   `EmitLibrary`, BroadcastChannel signaling paired with a real
   `RTCDataChannel` (`sync.js`), and an optional puppeteer gate
   (`TestTwoTabDemo`) that opens two real headless-chrome tabs and asserts DOM
   convergence. The browser gate caught a real bug the node-only fakes missed:
   a `DataCloneError` in the signaling payload, fixed by JSON-encoding the wire
   messages before `postMessage`.

The remaining Tier A:

6. ~~Five-outputs integration~~ DONE (gate `TestFiveOutputs*` in cmd/rune; unified manifest `examples/wavelet_demo.rune`; ch538 convergence hash-bound to the app's GC).
7. ~~6g: the GTM script / book chapter~~ DONE. The proven-minimal-IAM diff moment
   and the Ledger view moment, scripted: `pitch/01-DEMO.md` rewritten on the
   two-tab/five-outputs/IAM-diff/ledger/schedule-luck/hash-binding/close spine,
   plus the long-form technical-diligence chapter `pitch/11-DEMO-CHAPTER.md`.
   Depended on 6f (done). Scripted with `rune` verbs per the 2026-07-04
   sequencing decision (the author decided the project stays RUNE for now; the
   rename comes later, see item 12).

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
    consistent; conformance green. SEQUENCING DECIDED 2026-07-04 (author):
    the project stays RUNE for now; THREE major rune version bumps (module path
    v3 -> v4 -> v5 -> v6, contents to be defined) precede cutting a Wavelet
    beta, and the rename rides that sequence, not the 6g artifact. The earlier
    rename-before-6g recommendation is superseded. Rename remains mechanical
    (hashes are name-independent).
13. ~~6e: ARC on C + LLVM (replace mark-sweep)~~ DONE (two stages). Stage 1,
    the ARC conversion (v3.370.0-v3.371.0, branches feat/native-arc-c +
    feat/native-arc-ll): both native backends fully ARC -- rc header,
    malloc/free, per-kind free walker, the shared Perceus pass wired at Emit,
    PATH B ownership rules on every prim body; TestCARC (12 tests, pressure +
    ASAN), TestLLConformsToC and the LLVM ARC gate family green; mark-sweep
    retired. All nine backends now share one memory discipline. Stage 2, the
    stated payoff -- native c/ll rejoining the bible real-data SCALE gate --
    landed on branch feat/divmod-small (2026-07-06): profiling showed the
    residual cliff was NOT ARC traffic but a missing small-divisor division
    specialization WASM already had (base-256 packed decode divides by small
    constants; without the fast lane each fell through to full bignum long
    division). Ported to both native runtimes (~47-68x on the packed-decode
    shape; C microshape 22.3s -> 0.47s), exclusion removed, and
    TestBibleConformanceRealData PASSED 9-way at N=1500 real Greek+Hebrew in
    258s: c 27.2s / ll 27.1s per row (~18.1 ms/entry), mid-pack versus js
    23.2s / go 22.8s / py 25.7s / rs 31.5s / erl 49.6s / jvm 24.9s / wasm
    26.1s -- versus the pre-fix ~956 ms/entry (c) and DNF-at-30m (ll). The
    PARKING-LOT entry ("Native codec-over-corpus impractically slow") is
    CLOSED with the corrected attribution.
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
2. Rename timing (item 12): DECIDED 2026-07-04 - stays rune; 3 major bumps then rename then beta cut.
3. Which cloud goes live first for item 8 (AWS is the working default).
