# R-ARC: Automatic Reference Counting for the portable runtime

## Why ARC, and why no cycle collector

The erased IR builds only immutable, total, acyclic functional values: a value cannot
point back to one constructed after it, and nothing is ever mutated to create a cycle.
So reference counting WITHOUT a cycle collector is sound and complete -- every
unreachable object has refcount zero and is freed deterministically.

This is the Perceus / "Counting Immutable Beans" setting (Koka, Lean 4): precise
reference counting for a pure functional language, Swift-style determinism without
Swift's cycle hazards. Do NOT add a cycle collector -- it would be both unsound (the
acyclicity argument needs no collector) and unnecessary (a missed release leaks; a
spurious release is a use-after-free; the `$live` probe catches both).

## The runtime substrate (Plan 6a, this runtime; codegen/wasm_runtime.go)

Every heap block has a hidden 8-byte header immediately below the returned payload
pointer:

```
[payload - 8]  payload size in bytes (rounded to 4-byte alignment)
[payload - 4]  refcount (initialised to 1 on alloc)
```

The returned pointer is the PAYLOAD pointer. The header is invisible to the program
and to the kind-word layout below.

### Symbol inventory

- `$alloc(n)` -- rounds the requested payload size to 4-byte alignment, checks the
  size-classed free list first (see below); if a matching free block exists, pops it,
  resets its refcount to 1, increments `$live`, and returns it without touching `$hp`;
  otherwise bumps `$hp` by 8 (header) + rounded-n, stores the size at `[payload-8]`
  and rc=1 at `[payload-4]`, increments `$live`, and returns the payload pointer.

- `$rt_counted(v)` -- the guard: returns true iff v participates in refcounting. True
  when all three hold: low bit is clear (v is a pointer, not an immediate int), v is
  non-null, and v is not the immortal UNIT singleton. Used as the fast-path guard in
  both `$rt_retain` and `$rt_release`.

- `$rt_retain(v)` -- no-op on immediates (low bit set) and the immortal UNIT; otherwise
  increments the refcount at `[v-4]` by 1.

- `$rt_release(v)` -- no-op on immediates and UNIT; otherwise decrements the refcount
  at `[v-4]` by 1; if the result is zero, calls `$rt_free(v)`.

- `$rt_free(v)` -- releases the object's child pointers by kind (so the whole
  immutable structure is reclaimed transitively), decrements `$live`, then pushes the
  block onto its size-class bucket for reuse (Task 1's power-of-two rounding makes a
  BIG payload's stored size hit a bucket too, not just the sub-256B ones -- see below).
  Child dispatch by kind word (word 0):
  - K_CLO = 0: env slots start at word 3, count at word 2; releases each slot.
  - K_CON = 1: field slots start at word 4, count at word 3; releases each slot.
  - K_PAIR = 2: releases words 1 and 2 (the two halves).
  - K_BIG = 6, K_BIN = 8 (Plan 6c's packed-byte `Bin` container), and anything else:
    leaf -- limbs/bytes are raw, no child pointers.

- `$w(o, i)` -- the word accessor: loads the i32 at object offset i*4. Used by
  `$rt_free` and the kind-inspection paths.

- `$live` -- a global counter of live heap blocks. `$alloc` increments it; `$rt_free`
  decrements it. A balanced program (all allocations freed) returns `$live` to its
  baseline. This is the leak and double-free probe the four wasmtime tests assert.

- `$rt_live()` -- exports `$live` to test code.

- `$rt_hp()` -- exports `$hp` (the current bump pointer) to test code; the free-list
  reuse test confirms `$hp` does not advance under same-size alloc/free churn once the
  free list is seeded.

### Size-classed free list

73 buckets total, stored in a reserved low-memory region at base 8192 (global
`$freelist`), occupying `[8192, 8484)`:

- **64 exact-size buckets**, indices 0..63 at `[8192, 8448)`, indexed
  `rounded_size / 4` (covering payload sizes 0 to 252 bytes).
- **9 big power-of-two buckets** (Plan 6c Task 1), indices 64..72 at `[8448, 8484)`,
  one per power of two from 256B to 64KB (256, 512, 1024, 2048, 4096, 8192, 16384,
  32768, 65536). `$rt_bucket_addr` maps a size in `[256, 65536]` that is already an
  exact power of two to `64 + (23 - clz(size))` (`clz(256) = 23`, `clz(65536) = 15`).

The list is intrusive and singly-linked: the first word of the freed payload block
holds the next-block link. Push stores the old head at the freed block's word 0 and
writes the payload pointer as the new bucket head. Pop reads the head, advances the
bucket to the link at the head block's word 0, resets rc=1, and returns the head.

**Big-bucket rounding.** `$alloc` rounds any request landing in `[256, 65536]` UP to
the next power of two before the size-class lookup, so every payload that reaches the
big-bucket range hits one of the 9 buckets exactly (no partial-fit waste beyond the
power-of-two rounding itself) -- this is what makes a large payload (the `Bin`
message-loop shape: a few-hundred-byte buffer built and dropped every "message")
reach the SAME free block on every subsequent allocation, not a fresh bump each time.
Below 256B, allocation is exact-size (no rounding); the two regimes meet at 256B,
the smallest big bucket.

**The >64KB orphan boundary.** A payload requesting MORE than 65536 bytes is
deliberately NOT power-of-two-rounded and NOT pooled: `$rt_bucket_addr` returns 0 for
any size outside `[256, 65536]`, so `$rt_free` skips the free-list push for it. The
block is still correctly released (`$live` decrements, any child pointers recurse)
but its memory is orphaned -- `$hp` never rewinds to reclaim it, and a later
same-size request bump-allocates a fresh block rather than reusing the freed one.
This is a DOCUMENTED boundary, not a silent gap: v1's runtime allocates only small,
few-sized records (closures, constructors, pairs, bignums, and now `Bin` payloads),
so a >64KB single value is rare, and capping the bucket table at 9 entries keeps the
free-list scan O(1) without an unbounded size-class table. `TestARCBinHugeOrphanBalanced`
(`codegen/wasm_arc_test.go`) pins the boundary: `$live` balances across two 70000-byte
alloc/release cycles, but `$hp` strictly advances between them (no reuse).

Exact-size reuse only -- no splitting or coalescing in v1.

### The four wasmtime tests (codegen/wasm_arc_test.go)

Each test assembles a minimal WAT module via `arcTestModule` around `WasmRuntime()`
and runs it under wasmtime (the `runWasm` helper from wasm_test.go). The UNIT
singleton is allocated in the preamble, so the baseline `$live` is 1.

- `TestARCHeaderLiveCounter` -- allocates two constructors without releasing them;
  asserts `$rt_live()` returns 3 (UNIT + 2 cons). Pins that `$alloc` bumps `$live`
  and that the hidden header does not corrupt the object layout.

- `TestARCLeafRetainRelease` -- allocates one bignum (rc=1), retains it twice
  (rc=3), passes immediates and UNIT through retain/release (must be no-ops),
  then releases the bignum three times (rc reaches 0, freed); asserts `$live` returns
  1 (back to UNIT only). Pins the retain/release no-op rule and the leaf-free path.

- `TestARCRecursiveRelease` -- allocates two bignums, stores them as fields of a
  2-field constructor, releases the root; asserts `$live` returns 1. Without recursive
  child-release the two bignum fields would leak and `$live` would be 3.

- `TestARCFreeListReuse` -- warms the free list with one alloc+free of a 1-limb
  K_BIG (12-byte payload, poolable), captures `$hp`, then churns 100 alloc+free
  cycles of the same size; asserts `$rt_hp() - hp0 = 0` (every alloc hit the free
  list, `$hp` did not advance).

## The portable contract (for Plan 6e: port to C and LLVM)

The same four operations (retain, release, recursive-release-by-kind, size-classed
free list) port verbatim to the C and LLVM runtimes (`codegen/c.go`,
`codegen/ll_runtime.go`), REPLACING their existing mark-sweep GC. The record layouts
already match across backends (the bignum twin is documented at the top of
`wasm_runtime.go`; K_CLO/K_CON/K_PAIR/K_BIG have the same kind-word offsets in all
three runtimes); add the same hidden 8-byte header and the same retain/release helpers.

The win: deterministic, pause-free deallocation (no stop-the-world collector) and one
shared discipline instead of per-backend collectors. The `$live` / live-block-count
observability pattern ports directly -- it becomes a C global or an LLVM global, and
the same "balanced program returns live to baseline" invariant is the regression net
for the C and LLVM ports.

## The codegen insertion pass (Plan 6b: the hard part, its own plan + design)

A runtime with retain/release does nothing until codegen INSERTS the calls. Plan 6b
is the Perceus-style ownership pass over the closure-converted IR (`codegen/closure.go`:
`CVar`/`CEnv`/`CGlobal`/`CLet`/`CApp`/`CLam`/`CCon`/`CCase`/`CField`/`CPair`/`CFst`/
`CSnd`/`CBounce`). The rules, in brief: an owned binding used zero times is dropped
(release); used N > 1 times is retained N-1 times; a variable consumed by a
constructor or returned transfers ownership; borrowed positions (a scrutinee inspected
but not stored) are not consumed. Reuse (the "Beans" in-place update) is a later
optimization.

This pass must be designed against the exact IR in a dedicated brainstorm before
implementation. Correct insertion is the subtle part: a missed release leaks, a missed
retain is a use-after-free. The `$live` probe from Plan 6a is the regression net --
any balanced program must return `$live` to its baseline after main.

## Downstream (the demo path)

- **Plan 6c DONE** (`f4f49d3`..`571cdd4` + the Task 4 payoff commit, branch
  `feat/wasm-bin-arc`): bytes as a refcounted heap object in WASM. `K_BIN = 8`, a
  packed-byte leaf container (`[kind][byte-len][raw bytes, 4-rounded]`), plus the
  power-of-two big buckets above (Task 1) so a `Bin` payload over 256B still gets
  pooled. Full op parity -- `binEmpty`/`binCons`/`binLen`/`binAt`/`printBin`, the
  `b"..."` `LitBytes` literal, and the `$show` arm -- joins the ch483 cross-backend
  conformance gate. The Task 4 payoff:
  `TestPerceusBinMessageLoopFlat` (`codegen/perceus_test.go`) proves the 6f message-
  loop shape -- build a ~300-byte `Bin` via a saturated `NatElim` fold, read one byte,
  drop it -- is TRUE steady-flat (zero `$live` delta across runs 2..5), and
  `TestARCBinHugeOrphanBalanced` (`codegen/wasm_arc_test.go`) pins the >64KB orphan
  boundary. Two fixes surfaced along the way, both outside Tasks 1-3's own bodies: (1)
  `WasmSteadyModule` (`codegen/wasm_steady_test.go`), the steady-state test harness,
  never called `emitForeignPrimsWasm` -- no prior steady receiver used a `foreign` op,
  so the gap was latent until a program under the loop referenced `binCons`/`binEmpty`;
  fixed by mirroring `Wasm.Emit`'s own two-step bake. (2) `consumeOwning`
  (`codegen/perceus.go`) grouped `CForeign` with the "already owned" default arm
  instead of the borrowed-leaf arm alongside `CGlobal`, even though `annotate`'s leaf
  case already treated the two identically; harmless for every FUNCTION-arity foreign
  (always consumed via an AppClosure result, never a bare consumeOwning'd leaf) but a
  genuine use-after-free for a bare-VALUE nullary foreign like `binEmpty` -- a
  memoized `$cache_binEmpty` thunk exactly like a `CGlobal` def, so treating it as
  freshly owned let the first fold step release the SHARED cached pointer out from
  under every later reference. Fixed by adding `CForeign` to `consumeOwning`'s
  dup-on-consume-borrowed case.
- Plan 6d: the WebRTC FFI shim (the sandbox/no-native interop class) as WASM imports.
- Plan 6e: port the ARC discipline to C and LLVM runtimes (replace mark-sweep GC).
- Plan 6f: the two-tab CRDT browser app (WASM merge + JS/WebRTC glue + two divs).
- Plan 6g: the go-to-market script / book chapter (the proven-minimal-IAM + Ledger
  moments).
