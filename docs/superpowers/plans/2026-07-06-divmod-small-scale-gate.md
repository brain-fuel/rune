# divmod-small port + scale-gate re-admission Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port WASM's small-divisor bignum division to the C and LLVM runtimes (measured 68x on the bible workload), then re-admit c/ll to the N=1500 real-data scale gate.

**Architecture:** One new C function `big_divmod_small` in cRuntime + its llRuntimeC twin, dispatched from `big_divmod` when the divisor fits one base-1e9 limb; no caller changes. Then the exclusion block in the bible gate is removed and the honest bookkeeping corrected.

**Tech Stack:** Go (codegen), C (runtime strings), the bible corpus gate (BIBLE_REPO=/home/brainfuel/matt/bible - verify).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-06-divmod-small-scale-gate-design.md`.
- Byte-identity absolute: no expected output changes anywhere. `big_divmod`'s signature and semantics (fresh non-aliased `*rem` bignum, Plan A rewrite) are preserved - the small path is internal.
- Frozen: wasm*.go (the reference, already correct), perceus.go, closure.go, kernel dirs, c_arc_test.go/ll_arc_test.go existing tests (additive only).
- The WASM reference routine: codegen/wasm_runtime.go:845 `$big_divmod_small` - single descending-limb pass, i64 running remainder (`cur = rem*1e9 + limb; q_limb = cur/d; rem = cur%d`), one quotient alloc, big_norm at the end. A working C prototype from the profiling session exists at `/tmp/claude-1000/-home-brainfuel-matt-goforge-dev/27daa454-0640-452a-9d4a-1b279449f353/scratchpad/perf/prog_fast.c` (grep `big_divmod_small` there) - crib it, but the repo version must integrate with the CURRENT runtime helpers (big_alloc/big_setlimb/big_nlimbs/big_norm names as they exist in c.go).
- ARC: the fast path allocates exactly the quotient (rc=1); the `*rem` out-value must be a fresh K_BIG (build via the existing small-value constructor, e.g. big_from_long) so callers' release discipline is unchanged.
- Conventional Commits, explicit pathspecs, `-timeout 30m` suites.

---

### Task 1: the port + regression gates (C and LLVM)

**Files:**
- Modify: `codegen/c.go` (cRuntime: add `big_divmod_small`, dispatch from `big_divmod`), `codegen/ll_runtime.go` (llRuntimeC twin, same insertion, `== c.go` sync markers)
- Modify: `codegen/c_arc_test.go`, `codegen/ll_arc_test.go` (one new gate each - additive)
- Test: the new gates + existing families

**Interfaces:**
- Produces: `static Value big_divmod_small(Value a, long d, long* rem_out)` (or the closest shape fitting the existing helpers - implementer picks, twins must match) called from `big_divmod` when `big_nlimbs(b) <= 1`.

- [ ] **Step 1: failing perf-shaped test first (C)**

Add to codegen/c_arc_test.go:

```go
// TestCARCPackedDecodeFast pins the small-divisor division specialization:
// decoding a ~1KB packed-String payload must complete in seconds, not the
// tens of seconds the general binary-search divmod takes (the v3.371.0
// scale-gate miss, profiled to d6_s2h -> big_divmod). Correctness is pinned
// by comparing the program's stdout against the go backend's for the same
// source; the ceiling pins the specialization.
```

The test: a rune program that builds a ~1KB string (use the d6 byteLen/splitOn listing shapes - crib the program shape from the profiling prototype's source, scratchpad perf/prog.c came from `rune emit` of the ch559-family listing; a SMALL standalone program that packs ~1KB via d6_h2s and decodes via a splitOn/byteLen call suffices), run on c via the existing build helper with a `time.Since` wall-clock assert `< 10*time.Second` (generous; pre-fix measures much worse) AND stdout equal to the go backend's output for the same program (emit+run go via the package's existing go-run helper or exec `go run ./cmd/rune run ... --target go` - match the package's conventions). Run it: expect FAIL on the ceiling (pre-fix the 1KB decode is slow) - if it unexpectedly passes, enlarge the payload until the pre-fix path demonstrably exceeds the ceiling, and record the sizing in the report.

- [ ] **Step 2: implement in cRuntime**

In codegen/c.go's bignum section, add `big_divmod_small` per the WASM reference + prototype, and at the TOP of `big_divmod` add the dispatch:

```c
/* Small-divisor fast path (== wasm $big_divmod_small): one descending-limb
   pass with an i64 running remainder; one quotient alloc, no temps. The
   general binary-search path below serves multi-limb divisors only. */
if (big_nlimbs(b) <= 1) { ... call big_divmod_small, build *rem via big_from_long, return quotient ... }
```

Preserve: fresh non-aliased `*rem`, big_norm'd quotient, rc=1 outputs, zero temps leaked (the ARC bignum balance gate re-checks).

- [ ] **Step 3: C gates green**

Run: `go test ./codegen/ -run 'TestCARC' -count=1 -v` (new test now PASS, family still green) then `go test -timeout 20m ./codegen/ -run 'TestC|TestCodegen' -count=1`.

- [ ] **Step 4: mirror into llRuntimeC + LL gate**

Same function + dispatch into codegen/ll_runtime.go (twin linkage: static suffices - only big_divmod calls it; update sync markers). Twin the new test into ll_arc_test.go as `TestLLARCPackedDecodeFast` (same program, same ceiling, buildAndRunLLWithReport plumbing). Run: `go test ./codegen/ -run 'TestLLARC' -count=1 -v` then `go test -timeout 20m ./codegen/ -count=1` (whole package incl TestLLConformsToC + WASM guards).

- [ ] **Step 5: conformance sweep + commit**

Run: `go test -timeout 30m ./harness/ -run 'BackendConformance|Bible|D3|D4|D6|Foreign' -count=1` - PASS byte-identical.

```bash
git add codegen/c.go codegen/ll_runtime.go codegen/c_arc_test.go codegen/ll_arc_test.go
git commit -m "perf(codegen): small-divisor bignum division on native runtimes (wasm parity, 68x on packed decode)" -- codegen/c.go codegen/ll_runtime.go codegen/c_arc_test.go codegen/ll_arc_test.go
```

---

### Task 2: scale-gate re-admission + honest bookkeeping + full suite

**Files:**
- Modify: `harness/bible_conformance_test.go` (remove the c/ll exclusion block + its measured-miss comment; keep the t.Logf timings), `PARKING-LOT.md` (close the per-alloc-cliff entry with the corrected attribution), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (item 13 fully DONE, strike style)
- Test: the N=1500 gate + full suite

**Interfaces:**
- Consumes: Task 1's fast path.

- [ ] **Step 1: remove the exclusion, run the real gate**

Delete the exclusion `if bk.name == "c" || bk.name == "ll" { ... }` and its comment. Run with the corpus: `BIBLE_REPO=/home/brainfuel/matt/bible go test -timeout 30m ./harness/ -run 'BibleConformanceRealData' -count=1 -v 2>&1 | tail -30` (verify the env var path first: the repo must exist and be what the gate expects - read the gate's skip message). RECORD per-backend ms/entry from the t.Logf lines. Expected: c and ll in the same order of magnitude as the source backends (prototype says c ~6.4ms/entry). Contingency unchanged: material budget blow = STOP with numbers.

- [ ] **Step 2: bookkeeping**

PARKING-LOT: the "residual per-alloc ARC cliff" entry -> CLOSED, with the correction: profiling (2026-07-06) showed the attribution wrong - the cause was the missing small-divisor division specialization WASM already had; ported, measured, gate re-admitted with timings. Beta checklist item 13: strike fully DONE citing both the ARC conversion (v3.370-371) and the re-admission (this branch), with the measured numbers. No em/en dashes.

- [ ] **Step 3: full suite + commit**

Run: `go test -timeout 30m ./...` (background + poll inside one Bash call). ALL PASS.

```bash
git add harness/bible_conformance_test.go PARKING-LOT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "feat(harness): native c/ll rejoin the bible real-data scale gate (divmod-small, measured)" -- harness/bible_conformance_test.go PARKING-LOT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```
