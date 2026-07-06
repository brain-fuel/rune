# ARC on the native C + LLVM backends (Wavelet beta item 6e): Design

Date: 2026-07-05
Status: Approved by the author in-session (2026-07-05; 6e reclassified PRIMARY
by author directive the same day - in-beta, next work item).
Seed: main @ v3.369.0.

## Premise

The two native backends (C in codegen/c.go, LLVM in codegen/ll.go +
codegen/ll_runtime.go) run a conservative-roots non-moving MARK-SWEEP
collector. Its `gc_find_obj` membership test is O(N_live) per stack word
scanned, which makes codec-over-corpus workloads impractically slow and is
exactly why the bible real-data scale gate excludes c/ll
(harness/bible_conformance_test.go:388, "native GC too slow at N=1500";
PARKING-LOT.md:476). Meanwhile the 9th backend (WASM) ships a complete,
review-hardened ARC discipline: the Perceus pass (codegen/perceus.go)
annotating the SHARED closure-converted IR with CDup/CDrop, an rc-header
runtime, and a PATH B ownership model - written, per R-PERCEUS.md:54-57, with
"portability to the C/LLVM ARC port" as a stated design goal.

6e ports that discipline to the native backends and DELETES mark-sweep. This
closes R-NATIVE's deliberately-open GC-vs-refcounting research seam in favor
of Perceus ARC, aligns all nine backends on one memory discipline, and
re-admits c/ll to the scale gate.

## Decision 1: Perceus ARC replaces mark-sweep outright

No hybrid, no fallback flag (Rule 5: delete the superseded). Rejected
alternatives: a faster mark-sweep membership structure (allocation bitmap per
the parking-lot note) keeps two memory disciplines and only shrinks the
cliff; Cheney copying (R-NATIVE's other seam candidate) still needs the
static-root story mark-sweep was chosen to dodge and reuses none of the
tested Perceus machinery.

Soundness argument (same as WASM's): rune heap values are immutable inductive
data - no mutation, no cycles - so reference counting is complete; the
`partial` trampoline's CBounce is the one exotic shape and its ARC ownership
protocol is already designed and tested on WASM (the bounce owns its args,
the cached `_step` head is borrowed, the trampoline releases intermediate
apply-closures).

## Decision 2: the pass is shared, the call sites are new

`Perceus(cp)` already operates on the `ClosureProgram`/`CIr` that c.go:63 and
ll.go:50 consume; today its only call site is wasm.go:52. 6e adds the call in
the C and LLVM emission paths and teaches both emitters to lower `CDup` /
`CDrop` to `rt_retain(v)` / `rt_release(v)` calls. The pass's WASM-shaped
spine recognition (accel NatElim spines; it already references c.go's
`accelMatchC`) is completed for the native accel paths so accelerated
arithmetic gets correct ownership annotations. Any place the pass's
annotations prove WASM-specific is fixed IN THE PASS (it was designed to be
backend-portable), not worked around in the emitters.

## Decision 3: native runtime shape

Simpler than WASM's, same semantics:

- Every heap object gains an `rc` field in its header. `rt_retain` increments;
  `rt_release` decrements and at zero runs the per-kind free walker then
  `free()`s the block. Allocation is plain `malloc` - no freelist port, no
  bump arena; malloc IS the native freelist.
- Per-kind children for the recursive walker: K_CLO releases its env slots,
  K_CON its fields, K_PAIR both halves, K_BOUNCE its collected args. Leaves:
  K_STR, K_PTR, K_UNIT, K_FLOAT, K_BYTES (frees its byte buffer), K_BIG
  (frees its limb buffer). Kind census taken from the CURRENT c.go runtime
  (K_CLO..K_BOUNCE, 10 kinds), not from WASM's 8.
- DELETED wholesale: the roots table and static-root registration, alloc
  thresholds, `gc_collect`, `gc_mark_obj`/`gc_mark_value`, `gc_find_obj`, the
  conservative stack scan. Static thunk caches - the reason mark-sweep beat
  copying in the original design - become ordinary owned references (rc held
  forever), no registration needed.
- `rt_live` counter (alloc increments, free decrements), readable by tests;
  the process does not assert balance at exit in production emission (main's
  result and cached thunks legitimately hold references at exit), tests
  assert steady-state/return-to-baseline exactly as the WASM arc tests do.

## Decision 4: ownership model verbatim from WASM (PATH B)

Stated as rules in the runtime header comment, inherited not rediscovered:

- Arguments passed to `apply` are CONSUMED (moved); the caller dups first if
  it retains use.
- Borrowed values are dup'd on consumption (dup-on-consume-borrowed); in
  particular bare CForeign leaves and cached nullary foreigns are BORROWED
  (the 6c consumeOwning use-after-free lesson, now a rule).
- Constructor/pair/closure builders take ownership of their slot arguments.
- Foreign prim bodies (float/BLAS/net/fs/bin/crypto/CPython-embed blocks in
  c.go:717-1268 and their ll twins) receive BORROWED args and return OWNED
  results. The CPython embed keeps PyObject refcounting entirely on Python's
  side of the boundary; rune-side rc applies only to rune heap values
  marshalled at the edge.
- The trampoline: CBounce owns its collected args; `rt_tramp` releases each
  intermediate apply-closure; driver collect/saturate blocks retain forwarded
  env captures (the arity>=3 use-after-free found in the WASM review, ported
  as a rule).

## Decision 5: C first, LLVM mirrors, one spec, two plans

codegen/ll_runtime.go is an external-linkage TWIN of cRuntime, manually kept
in sync ("== c.go" markers) because the two cannot share a translation unit.
6e keeps that discipline: Plan A lands the full C-backend conversion and
proves it against every gate; Plan B mirrors the same surgery into
ll_runtime.go/ll.go with the sync markers updated, then removes the scale-gate
exclusion for BOTH backends. Runtime-source unification (generating the twin
from one source) is out of scope - parked with a rationale if the mirror pass
proves painful.

## Acceptance

1. **Byte-identity is absolute.** ARC is memory-only: every existing c/ll
   gate (codegen_test.go C tests, ll_test.go conformance incl
   TestLLConformsToC, backend-conformance corpus rows, bible builders
   byte-identity, FFI/BLAS/CPython/OpenBLAS gates) passes with UNCHANGED
   expected output.
2. **Native ARC balance gates.** The WASM arc-test family
   (codegen/wasm_arc_test.go: header/live-counter, recursive release, pair,
   closure, leaf, bignum, bytes) gets native twins asserting `rt_live`
   returns to baseline; a steady-state gate over the Perceus-balanceable
   corpus subset (the wasm_steady_test.go pattern) runs on C.
3. **The superseded tests die with the collector.**
   TestLLGCConformanceUnderTinyHeap (and any C tiny-heap GC test) is DELETED
   and replaced by an ARC-pressure equivalent (a deep-allocation program that
   would exhaust memory if release never fired).
4. **The prize.** The scale-gate exclusion at bible_conformance_test.go:388
   is REMOVED; c/ll run the N=1500 real-data gate green in sane time. The
   PARKING-LOT.md O(N_live) entry (line ~476) closes as superseded by this
   design.
5. **REPL acceptance** is not applicable (backend-internal; `rune run
   --target c|ll` on existing listings is the surface), but `rune run`
   smoke on a native target is part of the final gate.

## Risks (named, with mitigations)

- **Use-after-free class.** WASM's reviews caught several; the mitigations
  are the stated ownership rules plus the balance gates (a leak shows as
  rt_live drift; a double-free as a crash under the ARC-pressure test).
  Native additionally gets ASAN as a development tool: the plan's test steps
  run the ARC gates under `-fsanitize=address` where the toolchain supports
  it (gate skips cleanly otherwise).
- **LLVM twin drift.** Mitigated by sequencing (C proves the design first)
  and by TestLLConformsToC catching behavioral divergence.
- **Accel-path ownership.** The kernel-accel bignum fast lanes on native
  (accelMatchC) allocate K_BIG results; the pass's spine handling must mark
  their operands correctly - covered by the bignum balance twin and the
  corpus byte-identity.

## Non-goals

- No freelist/arena on native (malloc suffices; revisit only with a measured
  consumer).
- No runtime-source unification between c.go and ll_runtime.go (parked).
- No Perceus changes that alter WASM emission (WASM bytes stay identical;
  guarded by the existing WASM conformance + divergence-lock gates).
- No new language surface, no core/store changes, no hash impact.

## Consumers

- The bible-ops scale gate (all nine backends, closing the last exclusion).
- The beta claim "nine backends, one memory discipline" for 6g's successor
  materials and the funding pitch.
- Future native `rune build --kind library` work inherits deterministic
  ARC lifetimes instead of a collector.

## Testing summary

Regression: full existing native + WASM suites, byte-identical. New: native
arc balance family, C steady-state gate, ARC-pressure test (replacing
tiny-heap GC tests), ASAN-gated runs, scale-gate re-admission. Final:
full `go test -timeout 30m ./...` before each tag (Plan A tag after C,
Plan B tag after LLVM + gate removal).
