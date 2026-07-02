# WASM Bin over ARC (6c) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give WASM a first-class refcounted byte buffer (`K_BIN = 8`) with full Bin op parity, plus power-of-two big buckets in the ARC free list so large payloads stay steady-flat.

**Architecture:** Four tasks. (1) Extend the free list with power-of-two size classes [256B, 64KB] (runtime-only, benefits every kind). (2) Add the K_BIN heap kind + constructors/accessors + release leaf arm (runtime-only). (3) Wire codegen: `LitBytes`, the five bin foreign ops, `$show` arm, `usesBin` gate; WASM joins the ch483 conformance family. (4) The steady-flat payoff receiver + docs.

**Tech Stack:** Go (`codegen/`); WAT in `codegen/wasm_runtime.go` + `codegen/wasm.go`; tests in `codegen/wasm_arc_test.go` (6a probe idiom), `codegen/perceus_test.go` (steady helpers), `harness/bytes_test.go` (conformance gates); wasmtime on PATH.

**Design source:** `docs/superpowers/specs/2026-07-02-wasm-bin-arc-design.md`.

## Global Constraints

- **Kernel frozen.** No edits under `core/` or `store/`; hash format 0x06 unchanged. This plan touches only `codegen/` + `harness/` + `ref_docs/wootz/` + `docs/`.
- **Work in a fresh git worktree off main.** The main checkout has a LIVE parallel agent with uncommitted `codegen/perceus.go` work; do not touch the main checkout's working tree. Commit with explicit pathspec only (`git commit -m "..." -- <files>`).
- **Output-invariance for everything that exists today.** All prior WASM gates (`TestWasmConformsToJS`, the ARC tests `TestARCRecursiveRelease`/`TestARCLeafRetainRelease`/`TestARCFreeListReuse`/`TestARCPairRelease`/`TestARCClosureRelease`/`TestARCHeaderLiveCounter`, the Perceus receivers, the 9-way bible lock) stay byte-for-byte green. The packed-String `K_BIG` codec is UNTOUCHED.
- **The Bin op semantic contract is `codegen/ioprims.go:140-141`** (binCons prepends the LOW byte of the nat, & 255; binAt out-of-range returns nat 0) **and the printed rendering contract is whatever `binWant` in `harness/bytes_test.go` pins** — WASM must match it byte-for-byte; read the JS `printBin` body (`codegen/js.go`, `usesBin` block near line 62) as the reference renderer.
- **WASM-only.** No behavior change for the other 8 backends; the shared matchers/gates for them stay untouched.
- **Retain/release/`$live`/`rt_counted`/header layout NOT changed.** K_BIN is one new kind + one allocator extension; the ARC primitives are frozen.
- NO em or en dashes anywhere (ASCII hyphen-minus only). Conventional Commits. `go build ./...` + `go vet ./codegen/ ./harness/` clean before each commit.

## Reference: runtime facts (verified 2026-07-02 @ v3.354.0)

- Kinds: `K_CLO=0, K_CON=1, K_PAIR=2, K_UNIT=5, K_BIG=6, K_BOUNCE=7` (`wasm_runtime.go:17-21`). 3/4/8 unused on WASM; native uses `K_BYTES=8`, so K_BIN takes 8.
- Header: `[payload-8]=size (4-rounded), [payload-4]=rc`; `$alloc` at `wasm_runtime.go:96` tries `$rt_bucket_addr` (`:519-529`, 64 buckets at `$freelist=8192`, indexed size/4, exact-size, sizes < 256 only) then bumps `$hp`.
- Memory map comment at `wasm_runtime.go:71`: show buffer [4096,...), freelist [8192,8448), bible-codec windows from 65536. The region [8448, 65536) is unclaimed: the 9 new bucket words go at [8448, 8484). VERIFY the comment still says this before choosing the address; if anything claimed [8448,8484) since, shift the big-bucket base and update the comment.
- `$rt_free` (`:571-628`): per-kind child recursion; K_BIG + default = leaf.
- `$rt_hp` probe exists (`:533`) for hp-must-not-advance reuse tests; `$live` counts live blocks.
- Helpers `$w`/`$sw` load/store word i of a payload (`:127-131`).
- `1e9 mod 256 = 0`, so for a `K_BIG` nat, value mod 256 = limb0 & 255 (higher base-1e9 limbs contribute multiples of 256). Low byte of a bignum = `nlimbs==0 ? 0 : limb0 & 255`.
- WASM foreign-op pattern: `wasmSupportedForeign` whitelist + `emitForeignPrimsWasm` WAT bodies + `uses*` gates; curried IO/value ops use the c-chain closure template (see the committed `binLen`-shaped precedents: `printNat`, and the 2-arg curried `writeChunk` chain in `codegen/wasm.go`).
- ARC test idiom: `codegen/wasm_arc_test.go` assembles `codegen.WasmRuntime()` + a probe `(func $probe ...)` and runs it under wasmtime, asserting printed `$live`/values (6a pattern, see `TestARCFreeListReuse`).

---

### Task 1: Power-of-two big buckets in the free list

**Files:**
- Modify: `codegen/wasm_runtime.go` (`$rt_bucket_addr` ~:519, `$alloc` ~:96, the memory-map comment ~:71, `$freelist` region comment ~:89)
- Test: `codegen/wasm_arc_test.go`

**Interfaces:**
- Consumes: `$alloc`, `$rt_bucket_addr`, `$rt_free` push path, `$rt_hp`, `$live` (all existing).
- Produces: `$alloc` rounds any 4-rounded payload size in [256, 65536] UP to the next power of two and pools it in one of 9 new buckets (indices 64..72 at `[$freelist+256, $freelist+292)`, i.e. [8448, 8484) unless the map check forced a shift); sizes > 65536 keep the orphan behavior. Task 2's K_BIN and existing K_BIG both flow through this automatically.

- [ ] **Step 1: Write the failing reuse test**

```go
// codegen/wasm_arc_test.go
// TestARCBigBucketReuse: a 1KB payload (a big K_BIG) allocated, released, and
// re-allocated must reuse the SAME block: hp does not advance across the second
// alloc, and $live returns to baseline after each release. BEFORE Task 1 the
// >=256B free is orphaned, so the second alloc bumps hp.
func TestARCBigBucketReuse(t *testing.T) {
	probe := `
  (func $probe (result i32)
    (local $a i32) (local $hp0 i32) (local $b i32)
    ;; 1024-byte payload via big_alloc (254 limbs -> 8+1016 bytes, rounds to 1024)
    (local.set $a (call $big_alloc (i32.const 254)))
    (call $rt_release (local.get $a))
    (local.set $hp0 (call $rt_hp))
    (local.set $b (call $big_alloc (i32.const 254)))
    (call $rt_release (local.get $b))
    ;; result: 1 iff hp unchanged AND the two payloads were the same block
    (i32.and
      (i32.eq (call $rt_hp) (local.get $hp0))
      (i32.eq (local.get $a) (local.get $b))))
`
	got := runARCProbe(t, probe) // the existing 6a probe runner in this file
	if got != "1" {
		t.Fatalf("big payload not pooled: probe=%q want 1", got)
	}
}
```

Match the actual probe-runner name/signature used by `TestARCFreeListReuse` in this file (it may inline the WAT assembly rather than use a helper; mirror it exactly).

- [ ] **Step 2: Run it to confirm the orphan**

Run: `go test -run TestARCBigBucketReuse ./codegen/`
Expected: FAIL (probe prints 0: hp advanced, block not reused).

- [ ] **Step 3: Extend `$rt_bucket_addr` and `$alloc`**

Replace `$rt_bucket_addr` (keep the sub-256 path identical):

```wat
  (func $rt_bucket_addr (param $size i32) (result i32)
    (local $idx i32)
    (local.set $idx (i32.shr_u (local.get $size) (i32.const 2)))
    (if (i32.lt_u (local.get $idx) (i32.const 64))
      (then (return (i32.add (global.get $freelist) (i32.shl (local.get $idx) (i32.const 2))))))
    ;; big buckets: exact power-of-two sizes 256..65536 -> indices 64..72.
    ;; $alloc rounds big requests up to a power of two, so every stored size
    ;; reaching $rt_free in this range hits a bucket exactly.
    (if (i32.and
          (i32.and (i32.ge_u (local.get $size) (i32.const 256))
                   (i32.le_u (local.get $size) (i32.const 65536)))
          (i32.eqz (i32.and (local.get $size) (i32.sub (local.get $size) (i32.const 1)))))
      (then
        ;; clz(256)=23 -> idx 64 ... clz(65536)=15 -> idx 72
        (return (i32.add (global.get $freelist)
          (i32.shl (i32.add (i32.const 64)
            (i32.sub (i32.const 23) (i32.clz (local.get $size)))) (i32.const 2))))))
    (i32.const 0))
```

In `$alloc`, immediately after the existing 4-byte rounding line, add the power-of-two round-up so the STORED size is the bucket size:

```wat
    ;; big-bucket rounding: pool sizes in [256, 65536] as powers of two
    (if (i32.and (i32.ge_u (local.get $n) (i32.const 256))
                 (i32.le_u (local.get $n) (i32.const 65536)))
      (then (local.set $n (i32.shl (i32.const 1)
        (i32.sub (i32.const 32) (i32.clz (i32.sub (local.get $n) (i32.const 1))))))))
```

Update the two comments: the `$freelist` global comment (`:89`) now says 73 bucket heads at [8192, 8484); the memory-map comment (`:71`) reserves [8448, 8484) for the big buckets. (n=65536 exactly: clz(65535)=16 gives 1<<16=65536, stays in range; n=65537..: untouched, orphan path.)

- [ ] **Step 4: Run the new test + the full ARC/Perceus/conformance sweep**

Run: `go test -run TestARCBigBucketReuse ./codegen/`
Expected: PASS.

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/`
Expected: PASS byte-for-byte (small-object path untouched; K_BIG programs still print identically; steady-state receivers unaffected or improved, never worse).

- [ ] **Step 5: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_arc_test.go
git commit -m "feat(wasm): power-of-two big buckets (256B-64KB) in the ARC free list" -- codegen/wasm_runtime.go codegen/wasm_arc_test.go
```

---

### Task 2: The K_BIN heap kind

**Files:**
- Modify: `codegen/wasm_runtime.go` (kind constant comment block ~:17-21, new functions beside `$big_alloc`, `$rt_free` ~:571)
- Test: `codegen/wasm_arc_test.go`

**Interfaces:**
- Consumes: `$alloc` (Task 1's version), `$w`/`$sw`, `$rt_free`, `$live`.
- Produces (Task 3 relies on these exact names):
  - `$rt_mkbin (param $len i32) (result i32)` - fresh K_BIN, rc=1, kind word 8, length word, zero-filled payload NOT guaranteed (callers set every byte).
  - `$rt_bin_len (param $b i32) (result i32)` - the byte length.
  - `$rt_bin_set (param $b i32) (param $i i32) (param $c i32)` - store byte i (no bounds check; callers guarantee i < len).
  - `$rt_bin_at (param $b i32) (param $i i32) (result i32)` - load byte i (no bounds check; callers guarantee).
  - `$rt_free` treats kind 8 as an explicit leaf.

- [ ] **Step 1: Write the failing balance test**

```go
// codegen/wasm_arc_test.go
// TestARCBinLeafRelease: build a 300-byte K_BIN (over the small-bucket line, so
// it also exercises Task 1's big path), read a byte back, release it; $live must
// return to baseline and the byte must round-trip.
func TestARCBinLeafRelease(t *testing.T) {
	probe := `
  (func $probe (result i32)
    (local $b i32) (local $l0 i32) (local $ok i32)
    (local.set $l0 (global.get $live))
    (local.set $b (call $rt_mkbin (i32.const 300)))
    (call $rt_bin_set (local.get $b) (i32.const 0) (i32.const 65))
    (call $rt_bin_set (local.get $b) (i32.const 299) (i32.const 90))
    (local.set $ok (i32.and
      (i32.and (i32.eq (call $rt_bin_at (local.get $b) (i32.const 0)) (i32.const 65))
               (i32.eq (call $rt_bin_at (local.get $b) (i32.const 299)) (i32.const 90)))
      (i32.eq (call $rt_bin_len (local.get $b)) (i32.const 300))))
    (call $rt_release (local.get $b))
    (i32.and (local.get $ok) (i32.eq (global.get $live) (local.get $l0))))
`
	got := runARCProbe(t, probe)
	if got != "1" {
		t.Fatalf("K_BIN leaf lifecycle broken: probe=%q want 1", got)
	}
}
```

(Adjust `global.get $live` access to however the existing probes read the live counter; some use a `$rt_live` accessor. Mirror the file's idiom.)

- [ ] **Step 2: Run it to confirm the functions do not exist**

Run: `go test -run TestARCBinLeafRelease ./codegen/`
Expected: FAIL (wasmtime assembly error: unknown function `$rt_mkbin`).

- [ ] **Step 3: Add the kind**

Beside `$big_alloc` in `codegen/wasm_runtime.go`:

```wat
  ;; ---- bin: [K_BIN=8][byte-len][raw bytes, allocation 4-rounded] ----
  ;; The real-byte Bin container (native K_BYTES=8 analogue, packed not
  ;; slot-per-byte). A leaf: no child pointers, release never recurses.
  (func $rt_mkbin (param $len i32) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.add (i32.const 8)
      (i32.and (i32.add (local.get $len) (i32.const 3)) (i32.const -4)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 8))
    (call $sw (local.get $o) (i32.const 1) (local.get $len))
    (local.get $o))
  (func $rt_bin_len (param $b i32) (result i32)
    (call $w (local.get $b) (i32.const 1)))
  (func $rt_bin_set (param $b i32) (param $i i32) (param $c i32)
    (i32.store8 (i32.add (i32.add (local.get $b) (i32.const 8)) (local.get $i)) (local.get $c)))
  (func $rt_bin_at (param $b i32) (param $i i32) (result i32)
    (i32.load8_u (i32.add (i32.add (local.get $b) (i32.const 8)) (local.get $i))))
```

In `$rt_free`'s kind dispatch, add an explicit leaf arm beside K_BIG's with a comment (the default already treats unknown kinds as leaves; the explicit case documents that K_BIN is intentional):

```wat
      ;; K_BIN=8: raw bytes, a leaf like K_BIG -- no child pointers to release
```

(Concretely: if the dispatch is an if/else-if chain that falls through to a default leaf, add the comment at the default; if it is a br_table, add kind 8 to the leaf target. Match the existing structure.)

Update the kind-list comment at `:17-21` to include `K_BIN=8`.

- [ ] **Step 4: Run the test + sweep**

Run: `go test -run TestARCBinLeafRelease ./codegen/`
Expected: PASS.

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/`
Expected: PASS (nothing existing emits kind 8 yet; pure addition).

- [ ] **Step 5: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_arc_test.go
git commit -m "feat(wasm): K_BIN heap kind (packed-byte ARC leaf)" -- codegen/wasm_runtime.go codegen/wasm_arc_test.go
```

---

### Task 3: Codegen wiring - LitBytes, the five bin ops, $show, the conformance gate

**Files:**
- Modify: `codegen/wasm.go` (`wasmCheckSupported` ILit switch ~:158-166, `emitIn`'s ILit case, `wasmSupportedForeign`, `emitForeignPrimsWasm`, a `usesBin(p)` gate for the fragment)
- Modify: `codegen/wasm_runtime.go` (`$show` arm ~:462-498)
- Test: `harness/bytes_test.go`

**Interfaces:**
- Consumes: Task 2's `$rt_mkbin`/`$rt_bin_len`/`$rt_bin_set`/`$rt_bin_at`; `$rt_big_from_long`; K_BIG limb accessors (`$big_nlimbs`-style, match the actual names used by the codec at `wasm_runtime.go:751-801`); the c-chain curried foreign template (writeChunk precedent); `usesBin` (`ioprims.go:147`).
- Produces: `binEmpty : Bin`, `binCons : Nat -> Bin -> Bin`, `binLen : Bin -> Nat`, `binAt : Bin -> Nat -> Nat`, `printBin : Bin -> IO Bin` on WASM; `LitBytes` literals emit; `$show` renders a K_BIN exactly as `binWant` pins; `TestBytesWasm` gate green.

- [ ] **Step 1: Read the contract**

Read `harness/bytes_test.go` top-to-bottom: the `binWant` constant, `TestBytesConformance` (source backends), `runBytesNative`/`runBytesJVM` (how a compile-step backend joined). Read `codegen/js.go`'s `usesBin` block (~:62) for the reference `printBin` rendering and `listings/ch483_bytes.rune` for what the program exercises. Write down the exact expected stdout. This is the byte-for-byte target; do not improvise a rendering.

- [ ] **Step 2: Write the failing gate**

```go
// harness/bytes_test.go
// TestBytesWasm gates the Bin vocabulary on the WASM backend: ch483 under
// wasmtime must print byte-identically to the 8-way reference (binWant).
func TestBytesWasm(t *testing.T) {
	wt := wasmtimePathHarness()
	if wt == "" {
		t.Skip("wasmtime not found")
	}
	// emit + run, mirroring runWasmListing in bible_conformance_test.go (pure
	// compute: no --dir/--env needed unless ch483 does IO beyond printBin)
	got, ok := runWasmListing(t, "ch483_bytes.rune", "main", "")
	if !ok {
		t.Skip("wasmtime absent")
	}
	if got != strings.TrimSpace(binWant) {
		t.Fatalf("wasm bin divergence:\n got %q\nwant %q", got, binWant)
	}
}
```

(`runWasmListing` lives in `harness/bible_conformance_test.go` same package; reuse it. Match how `binWant` is compared by the sibling gates - trimmed or exact - and mirror it so the assertion is identical in strength.)

- [ ] **Step 3: Run it to confirm the wall**

Run: `go test -run TestBytesWasm ./harness/`
Expected: FAIL (emit error: the bin ops are not in WASM's foreign vocabulary, or LitBytes unsupported).

- [ ] **Step 4: Wire the emitter**

a. `wasmCheckSupported` ILit switch: add `case LitBytes:` as supported (only when the emit path below exists; keep LitStr/LitPtr errors).

b. `emitIn` ILit: find where LitNat lowers; add the LitBytes sibling:

```go
case LitBytes:
    // constant Bin: allocate once per evaluation, fill bytes
    o := f.fresh()
    fmt.Fprintf(b, "    (local.set %s (call $rt_mkbin (i32.const %d)))\n", o, len(x.Bytes))
    for i, c := range x.Bytes {
        fmt.Fprintf(b, "    (call $rt_bin_set (local.get %s) (i32.const %d) (i32.const %d))\n", o, i, c)
    }
    return "(local.get " + o + ")"
```

(Field name of the IR literal's byte payload: check `codegen/ir.go`'s LitBytes definition and use the real accessor. If ch483 has no LitBytes literal and nothing else consumes it, still land this case - it is 5 lines and the ILit switch otherwise silently mis-lowers.)

c. `wasmSupportedForeign`: add the five names from `binPrims` (`ioprims.go:143`).

d. `emitForeignPrimsWasm`: a new fragment gated `usesBin(p)`, WAT bodies:

- `binEmpty` (value accessor): `(func $binEmpty (result i32) (call $rt_mkbin (i32.const 0)))`
- `binLen` (1-arg): `(func $binLen (param $b i32) (result i32) (call $rt_big_from_long (call $rt_bin_len (local.get $b))))` wrapped in the 1-arg curried template the other value ops use; the Bin arg is BORROWED (read, not consumed) - follow the printNat retain discipline for the c-chain.
- `binCons` (2-arg curried, returns fresh Bin): low byte of the nat via the 1e9-mod-256 fact: `byte = nlimbs==0 ? 0 : limb0 & 255`. Body: `n = rt_bin_len(b); o = rt_mkbin(n+1); rt_bin_set(o,0,byte); loop i in 0..n: rt_bin_set(o,i+1,rt_bin_at(b,i)); return o`. The source Bin is borrowed; the returned Bin is fresh owned.
- `binAt` (2-arg curried, returns Nat): index low-limb with an nlimbs guard (an index with >1 limb is >= 1e9, out of any real range): `k = (nlimbs==1 ? limb0 : 0xFFFFFFFF); if nlimbs==0 k=0; if k < rt_bin_len(b) return rt_big_from_long(rt_bin_at(b,k)) else return rt_big_from_long(0)` - careful: nlimbs>1 must go to the oob arm, not index.
- `printBin` (IO, returns the Bin): render per Step 1's recorded contract via `$puts`/the show buffer, then retain-and-return the borrowed Bin (the printNat/printStrCode precedent).

e. `$show` arm in `wasm_runtime.go`: kind 8 renders exactly what Step 1 recorded (whatever the shared gates print when a Bin is a program's final value). If ch483's main never surfaces a raw Bin through `$show` (only through printBin), still add the arm with the same rendering so a future `rune run` of a Bin-valued main matches the other backends; note which path ch483 actually exercises in the commit body.

ARC discipline for every body: fresh results owned (no retain), borrowed args read-only with the retain-before-return pattern ONLY where the op returns its own argument (printBin). Intermediate scratch (none expected: these ops allocate no temporaries beyond the result) - if any body does allocate scratch, release it before returning.

- [ ] **Step 5: Run the gate + the full sweep**

Run: `go test -run TestBytesWasm ./harness/`
Expected: PASS, byte-identical to binWant.

Run: `go test -run 'Wasm|WASM|ARC|Perceus' ./codegen/ && go test ./harness/ -run 'Bible|Bytes|D6' -timeout 20m`
Expected: PASS everywhere (bible 9-way lock untouched; bytes gates on the other backends untouched).

- [ ] **Step 6: Commit**

```bash
git add codegen/wasm.go codegen/wasm_runtime.go harness/bytes_test.go
git commit -m "feat(wasm): Bin vocabulary (LitBytes + 5 ops + show) over K_BIN; joins ch483 gate" -- codegen/wasm.go codegen/wasm_runtime.go harness/bytes_test.go
```

---

### Task 4: Steady-flat payoff + orphan documentation + docs

**Files:**
- Test: `codegen/perceus_test.go`, `codegen/wasm_arc_test.go`
- Modify: `ref_docs/wootz/R-ARC.md` (big buckets + K_BIN section; 6c marked landed in the Plan-6 list), `docs/superpowers/plans/00-INDEX.md` (6c DONE), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (Tier A item 3 done)
- Modify (only if Step 1 fails): whichever task's body leaks

**Interfaces:**
- Consumes: everything from Tasks 1-3; `wasmSteadyLivePInts`/`assertSteadyFlatInts` steady helpers in `codegen/perceus_test.go` (landed in 6b-2).
- Produces: the 6f message-loop shape proven steady-flat; the >64KB orphan boundary pinned by a test; docs current.

- [ ] **Step 1: Write the message-loop receiver**

```go
// codegen/perceus_test.go
// TestPerceusBinMessageLoopFlat: the 6f demo shape - each "message" builds a
// >256B Bin (forcing Task 1's big path), reads it, and drops it. Per-run $live
// increment must be ZERO: large payload churn is steady-flat.
func TestPerceusBinMessageLoopFlat(t *testing.T) {
	src := perceusBinLoopSrc // a NatElim-driven loop; each iteration builds a
	// ~300-byte Bin via repeated binCons from binEmpty, reads binAt 0, discards
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainBinLoop"))
	if got != binLoopWant {
		t.Fatalf("output changed: got %q want %q", got, binLoopWant)
	}
	p := mustProgram(t, src, "mainBinLoop")
	assertSteadyFlatInts(t, p, 5)
}
```

The implementer defines `perceusBinLoopSrc` (a listing-style source with `data Nat` + `builtin nat` + the bin foreign decls, a fold that builds a 300-cons Bin per iteration; follow the source shape of the existing perceus receivers in this file) and `binLoopWant`. Note: binCons is O(n) copy per cons, so a 300-cons build allocates ~300 intermediate Bins per iteration - each must be released by the normal owned-arg discipline; if the steady gate shows growth, the leak is in binCons's body (Task 3), not here.

- [ ] **Step 2: Run it**

Run: `go test -run TestPerceusBinMessageLoopFlat ./codegen/`
Expected: PASS if Tasks 1-3 are correct. If it grows per-run: fix the responsible body (do NOT weaken the assertion).

- [ ] **Step 3: Pin the >64KB orphan boundary**

```go
// codegen/wasm_arc_test.go
// TestARCBinHugeOrphanBalanced: a >64KB K_BIN is above the big-bucket ceiling:
// released memory is NOT pooled (hp does not rewind, the block is orphaned) but
// $live still balances. Pins the documented boundary.
func TestARCBinHugeOrphanBalanced(t *testing.T) {
	probe := `
  (func $probe (result i32)
    (local $l0 i32) (local $b i32) (local $hp0 i32) (local $c i32)
    (local.set $l0 (global.get $live))
    (local.set $b (call $rt_mkbin (i32.const 70000)))
    (call $rt_release (local.get $b))
    (local.set $hp0 (call $rt_hp))
    (local.set $c (call $rt_mkbin (i32.const 70000)))
    (call $rt_release (local.get $c))
    ;; balanced live AND the second alloc did NOT reuse (hp advanced): the
    ;; orphan boundary, documented not hidden
    (i32.and (i32.eq (global.get $live) (local.get $l0))
             (i32.gt_u (call $rt_hp) (local.get $hp0))))
`
	got := runARCProbe(t, probe)
	if got != "1" {
		t.Fatalf("orphan boundary moved: probe=%q want 1", got)
	}
}
```

Run: `go test -run TestARCBinHugeOrphanBalanced ./codegen/` - Expected: PASS.

- [ ] **Step 4: Docs**

- `ref_docs/wootz/R-ARC.md`: extend the free-list section with the big buckets (9 pow2 classes, [8448,8484) heads, >64KB orphan boundary + rationale) and add K_BIN=8 to the kind table + the leaf-release list; mark 6c landed in the Plan-6 downstream list (:141-146).
- `docs/superpowers/plans/00-INDEX.md`: 6c line -> DONE with the commit range.
- `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md`: Tier A item 3 -> DONE, same style as item 2's closure note.
- Verify no em/en dashes in every edited doc: `grep -nP '[\x{2013}\x{2014}]' <files>` must print nothing for the ADDED lines.

- [ ] **Step 5: Full gate + commit**

Run: `go build ./... && go vet ./codegen/ ./harness/ && go test ./codegen/ && go test ./harness/ -run 'Bible|Bytes|D6|Conformance|Backend' -timeout 30m`
Expected: all green.

```bash
git add codegen/perceus_test.go codegen/wasm_arc_test.go ref_docs/wootz/R-ARC.md docs/superpowers/plans/00-INDEX.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "test(wasm): Bin message-loop steady-flat + orphan boundary; 6c docs" -- codegen/perceus_test.go codegen/wasm_arc_test.go ref_docs/wootz/R-ARC.md docs/superpowers/plans/00-INDEX.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```

---

## Self-Review

**Spec coverage:** Decision 1 (K_BIN leaf, packed, kind 8) -> Task 2. Decision 2 (big buckets + >64KB orphan) -> Tasks 1 + 4. Decision 3 (LitBytes, 5 ops, show, usesBin gate, Perceus-no-change claim) -> Tasks 3 + 4 (the steady receiver checks the claim). Testing section items 1-4 -> Task 3 Step 2 (conformance), Task 2 Step 1 + Task 1 Step 1 (ARC units), Task 4 Step 1 (steady-flat), Task 1 Step 4 (existing ARC green). Non-goals respected: no LitStr, no codec migration, no K_BOUNCE renumber, no 6d/6e work.

**Placeholder scan:** Two deliberate read-the-contract instructions (Task 3 Step 1: binWant rendering; probe-runner naming in Tasks 1/2) - these reference exact files and constants that exist, with the requirement stated (byte-for-byte match, mirror the idiom), not open-ended TBDs. All WAT and Go code is concrete otherwise.

**Type consistency:** `$rt_mkbin`/`$rt_bin_len`/`$rt_bin_set`/`$rt_bin_at` defined in Task 2, consumed in Task 3/4 with identical signatures. Bucket indices 64..72 and region [8448,8484) consistent across Task 1 and Task 4 docs. Kind 8 everywhere. Steady helpers named as landed in 6b-2 (`wasmSteadyLivePInts`/`assertSteadyFlatInts`).

**Honest notes:** (1) Task 1's 254-limb probe assumes `$big_alloc(254)` = 8+1016 bytes -> pow2-rounds to 1024; if the header math differs on read, the implementer picks a limb count that lands in [256,65536] - the test's teeth are hp-stability + pointer equality, not the exact size. (2) binCons O(n) copy makes the ch483 gate also a mild perf canary; no perf assertion is made. (3) If `runBytesNative`-style compile plumbing turns out to be needed for the harness gate (it should not - WASM needs no compile step beyond emit), mirror the JVM gate's structure instead.
