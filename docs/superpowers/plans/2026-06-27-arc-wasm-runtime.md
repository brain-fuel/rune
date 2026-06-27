# Plan 6a: ARC Heap Runtime for the WASM Backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the WASM backend's leak-forever bump allocator with a Swift-style automatic-reference-counting (ARC) heap runtime -- every heap object carries a refcount, `rt_retain`/`rt_release` adjust it, a release to zero recursively releases the object's children and returns its block to a size-classed free list for reuse -- as the portable memory-management substrate that the Perceus-style codegen insertion pass (Plan 6b) and the C/LLVM ports (Plan 6e) build on.

**Architecture:** The current WASM runtime (`codegen/wasm_runtime.go`) bump-allocates heap records (`[kind][fields...]`) and never frees them. This plan prepends a HIDDEN two-word header `[size][rc]` to every allocation: `alloc` reserves it and returns the PAYLOAD pointer (header at `payload-8`), so every existing record offset is unchanged and the existing WASM conformance gate stays green. `rt_retain(v)`/`rt_release(v)` ignore immediates (low bit set) and the immortal UNIT singleton, otherwise adjust `*(v-4)` (the rc); a release to zero recurses over the object's child slots by kind (closure env, constructor fields, pair halves; bignums are leaf) then pushes the block onto a free list keyed by size, which `alloc` consults first. A `$live` counter (incremented per alloc, decremented per free) makes leaks and double-frees observable in a wasmtime test. ARC without a cycle collector is SOUND here because the erased IR's value graph is acyclic (immutable, total, functional data: no value can point back to one constructed after it). Codegen does not yet call retain/release -- that is Plan 6b; this plan delivers and unit-tests the runtime substrate.

**Tech Stack:** WAT (WebAssembly text) in `codegen/wasm_runtime.go`; the Go test harness `codegen/wasm_test.go` (`runWasm`, `wasmtimePath`); wasmtime v42.0.1 (present; tests skip gracefully without it or under `-short`).

## Global Constraints

- **Kernel frozen.** No edits under `core/` or `store/`. No new core constructor, hash 0x06 unchanged. This plan changes only the WASM codegen runtime + its tests + a design doc.
- **Behavior-preserving for existing programs.** Adding the header must NOT change any emitted program's observable output: the existing WASM gate (`codegen/wasm_test.go`) must stay green after every task. The header is hidden below the returned payload pointer, so all existing record field offsets (`kind` at word 0, constructor fields, closure env slots, bignum limbs) are byte-for-byte unchanged.
- **ARC soundness rests on acyclicity.** The erased IR builds only immutable, acyclic functional values, so reference counting with NO cycle collector is correct. Do not add a cycle collector; do document the acyclicity assumption (Task 5).
- **Immediates and immortals are never counted.** A value with the low bit set is an immediate int (`(n<<1)|1`), not a pointer -- `rt_retain`/`rt_release` must no-op on it. The boxed UNIT singleton is immortal -- never freed. Guard both.
- **Codegen does not call retain/release yet.** This plan is the runtime substrate only; the Perceus insertion pass that makes real programs refcount is Plan 6b. The substrate is unit-tested at the WAT level (direct `rt_*` calls), not through a Rune program.
- **The free list reuses by exact size class.** A freed block of N bytes is reused only for a later request of the same N (the records are small and few-sized: closures, constructors, pairs, bignums). No splitting or coalescing in v1.
- **Process standards.** NO em or en dashes in any code, comment, or doc (only ASCII hyphen-minus). Conventional Commits. Run `go test ./codegen/ -run 'Wasm|WASM|ARC'` before tagging; `go build ./...` stays green.
- **Portability is the point.** Keep `rt_retain`/`rt_release`/the free-list shape simple and language-neutral so Plan 6e can port the identical discipline to the C and LLVM runtimes (replacing their mark-sweep). Document the portable contract (Task 5).

---

### Task 1: The hidden ARC header (size + refcount), behavior-preserving

Prepend a two-word header `[size][rc]` to every allocation and return the payload pointer, plus a `$live` allocation counter. No retain/release yet. Existing programs are byte-for-byte unaffected.

**Files:**
- Modify: `codegen/wasm_runtime.go` (the `$alloc` function + add the `$live` global)
- Test: `codegen/wasm_arc_test.go`

**Interfaces:**
- Consumes: the existing `$hp` global and the `$alloc(n) -> i32` contract (`codegen/wasm_runtime.go`).
- Produces: `$alloc(n)` now returns a payload pointer with an 8-byte header at `[payload-8]=size`, `[payload-4]=rc(=1)`; a mutable global `$live` (count of live blocks); a WAT helper expectation `rt_live() -> i32` exported for tests; the package const `wasmRuntime` still emits a valid module.

- [ ] **Step 1: Write the failing test**

```go
// codegen/wasm_arc_test.go
package codegen

import (
	"strings"
	"testing"
)

// arcTestModule wraps the wasmRuntime with the module scaffolding (the codety type and
// a minimal funcref table so rt_apply's call_indirect validates) plus a custom _start
// body that exercises the ARC primitives and prints a result. It mirrors the module
// shell in Wasm.Emit but with a test entrypoint instead of rune_main.
func arcTestModule(startBody string) string {
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(wasmRuntime)
	b.WriteString("\n  (table 1 funcref)\n")
	b.WriteString("  (func $_start (export \"_start\")\n")
	b.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (i32.const 64) (i32.const 0)))\n")
	b.WriteString(startBody)
	b.WriteString("  )\n)\n")
	return b.String()
}

// TestARCHeaderLiveCounter: after allocating two constructors (no release), the live
// counter reads the number of blocks allocated so far. The UNIT singleton in the
// preamble is one allocation; two more constructors make three. The test prints
// rt_live and asserts it counted them. This pins that alloc now bumps $live and that
// the hidden header did not break allocation.
func TestARCHeaderLiveCounter(t *testing.T) {
	body := `
    (drop (call $rt_mkcon (i32.const 0) (i32.const 64) (i32.const 0)))
    (drop (call $rt_mkcon (i32.const 0) (i32.const 64) (i32.const 0)))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "3" {
		t.Fatalf("rt_live after UNIT + 2 cons = %q, want 3", out)
	}
}
```

(This test needs three small runtime helpers the next steps add: `$rt_live` (read `$live`), `$rt_print_u32` (print an unsigned int to stdout via the existing show/puts path), and the `$live` global. The `arcTestModule` helper also assumes `rt_mkcon` and the show path exist, which they do. If `runWasm` skips because wasmtime is absent, the test skips -- wasmtime v42 is present here, so it runs. If the module fails to validate, read the wasmtime error: the most likely cause is a missing scaffolding element -- the `(table 1 funcref)` is there for `rt_apply`'s `call_indirect`; add a `(global $unit_name ...)` or other referenced global only if the validator names it.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestARCHeaderLiveCounter ./codegen/`
Expected: FAIL (`$rt_live` / `$rt_print_u32` / `$live` not defined, or the count is wrong because `$live` is not yet bumped).

- [ ] **Step 3: Implement the header + the live counter**

In `codegen/wasm_runtime.go`, find the `$alloc` function (it currently bumps `$hp` by `n` and returns the base). Add the `$live` global near `$hp`, and rewrite `$alloc` to reserve the 8-byte header, store the size, initialize rc to 1, bump `$live`, and return the payload pointer:

```wat
  (global $live (mut i32) (i32.const 0))   ;; count of live heap blocks (ARC leak probe)

  ;; ARC alloc: reserve an 8-byte hidden header [size][rc] below the payload. Stores
  ;; the requested payload size and rc=1, bumps $live, returns the PAYLOAD pointer so
  ;; every existing record offset is unchanged. (Plan 6a; Plan 6b inserts retain/release.)
  (func $alloc (param $n i32) (result i32)
    (local $base i32) (local $payload i32)
    (local.set $base (global.get $hp))
    (local.set $payload (i32.add (local.get $base) (i32.const 8)))
    (global.set $hp (i32.add (local.get $payload) (local.get $n)))
    (i32.store (local.get $base) (local.get $n))                       ;; [payload-8] = size
    (i32.store (i32.add (local.get $base) (i32.const 4)) (i32.const 1)) ;; [payload-4] = rc
    (global.set $live (i32.add (global.get $live) (i32.const 1)))
    (local.get $payload))
```

(Preserve the existing 4-byte alignment: `$hp` starts at 65536 and payloads stay 4-aligned because the header is 8 bytes and `$n` is 4-aligned by every caller. If the existing `$alloc` had a different signature or did alignment rounding, keep that logic and only add the header + `$live`.)

Add the two test helpers to the runtime (near the show helpers):

```wat
  ;; ARC live-block count (alloc bumps, free drops): the leak/double-free probe.
  (func $rt_live (result i32) (global.get $live))

  ;; print an unsigned i32 in decimal to stdout (reuses the show buffer + fd_write).
  (func $rt_print_u32 (param $n i32)
    (global.set $sbuf (i32.const 4096))
    (call $emit_u32 (local.get $n))
    (call $puts (i32.const 1) (i32.const 4096)
      (i32.sub (global.get $sbuf) (i32.const 4096))))
```

(`$emit_u32` and `$puts` already exist in the runtime -- `$emit_u32` is the base-10 unsigned emitter used for bignum limbs; `$puts` is the WASI fd_write wrapper. If they are named differently, grep `wasm_runtime.go` for the unsigned-decimal emitter and the fd_write wrapper and use the real names. `$sbuf` is the existing show cursor starting at 4096.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestARCHeaderLiveCounter ./codegen/`
Expected: PASS (prints "3": UNIT + 2 constructors).

- [ ] **Step 5: Confirm the existing WASM gate is unbroken**

Run: `go test -run 'Wasm|WASM' ./codegen/`
Expected: PASS (every existing emitted program still produces its expected output -- the header is invisible below the payload pointer, so all field offsets are unchanged).

- [ ] **Step 6: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_arc_test.go
git commit -m "feat(wasm): hidden ARC header (size + refcount) + live-block counter"
```

---

### Task 2: `rt_retain` / `rt_release` for leaf objects

Add the refcount primitives. They no-op on immediates and the UNIT singleton; otherwise retain increments and release decrements the rc, freeing (dropping `$live`) at zero. No child recursion yet (leaf objects only, e.g. bignums).

**Files:**
- Modify: `codegen/wasm_runtime.go` (add `$rt_retain`, `$rt_release`, `$rt_free`)
- Test: `codegen/wasm_arc_test.go` (add a leaf retain/release balance test)

**Interfaces:**
- Consumes: the ARC header (`[payload-8]=size`, `[payload-4]=rc`), `$live`, `$UNIT` (Task 1); the immediate tag (low bit set = immediate).
- Produces: `$rt_retain(v)` (no result), `$rt_release(v)` (no result), `$rt_free(payload)` (internal; drops `$live`, no result yet -- Task 4 adds the free list).

- [ ] **Step 1: Write the failing test**

```go
// add to codegen/wasm_arc_test.go

// TestARCLeafRetainRelease: a bignum (a leaf object, no child pointers) is allocated
// (rc=1, live=2 with UNIT), retained twice (rc=3), released three times (rc 0 -> freed,
// live back to 1, just UNIT). An immediate int and the UNIT singleton are also passed
// to retain/release and must be ignored (no crash, live unchanged).
func TestARCLeafRetainRelease(t *testing.T) {
	body := `
    (local $b i32) (local $imm i32)
    (local.set $b (call $rt_big_from_i32 (i32.const 7)))
    (call $rt_retain (local.get $b))
    (call $rt_retain (local.get $b))
    ;; immediates and UNIT are never counted
    (local.set $imm (i32.const 11))   ;; (5<<1)|1 style immediate; low bit set
    (call $rt_retain (local.get $imm))
    (call $rt_release (local.get $imm))
    (call $rt_retain (global.get $UNIT))
    (call $rt_release (global.get $UNIT))
    ;; now release the bignum to zero
    (call $rt_release (local.get $b))
    (call $rt_release (local.get $b))
    (call $rt_release (local.get $b))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("after freeing the bignum, live = %q, want 1 (just UNIT)", out)
	}
}
```

(`$rt_big_from_i32` is the runtime's "build a bignum from a small i32" helper seen in `wasm_runtime.go`; if it is named differently, e.g. `$big_from_i32`, use the real name. The immediate `11` is `(5<<1)|1` -- any odd i32 is an immediate; retain/release must skip it by the low-bit test.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestARCLeafRetainRelease ./codegen/`
Expected: FAIL (`$rt_retain` / `$rt_release` not defined).

- [ ] **Step 3: Implement the primitives**

Add to `codegen/wasm_runtime.go`:

```wat
  ;; ARC retain: a no-op on immediates (low bit set) and the immortal UNIT; otherwise
  ;; increment the refcount at [v-4].
  (func $rt_retain (param $v i32)
    (if (call $rt_counted (local.get $v))
      (then
        (i32.store (i32.sub (local.get $v) (i32.const 4))
          (i32.add (i32.load (i32.sub (local.get $v) (i32.const 4))) (i32.const 1))))))

  ;; ARC release: a no-op on immediates and UNIT; otherwise decrement the refcount; at
  ;; zero, free the block (Task 3 adds child recursion before the free).
  (func $rt_release (param $v i32)
    (local $rc i32)
    (if (call $rt_counted (local.get $v))
      (then
        (local.set $rc (i32.sub (i32.load (i32.sub (local.get $v) (i32.const 4))) (i32.const 1)))
        (i32.store (i32.sub (local.get $v) (i32.const 4)) (local.get $rc))
        (if (i32.eqz (local.get $rc))
          (then (call $rt_free (local.get $v)))))))

  ;; rt_counted: true iff v is a heap pointer that participates in counting -- not an
  ;; immediate (low bit set), not null, and not the immortal UNIT singleton.
  (func $rt_counted (param $v i32) (result i32)
    (i32.and
      (i32.and
        (i32.eqz (i32.and (local.get $v) (i32.const 1)))   ;; low bit clear => pointer
        (i32.ne (local.get $v) (i32.const 0)))             ;; non-null
      (i32.ne (local.get $v) (global.get $UNIT))))         ;; not the immortal unit

  ;; rt_free: drop the live count and (Task 4) return the block to the free list. For now
  ;; it only decrements $live so leaks/double-frees are observable; the bytes leak until
  ;; Task 4 adds reuse.
  (func $rt_free (param $v i32)
    (global.set $live (i32.sub (global.get $live) (i32.const 1))))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestARCLeafRetainRelease ./codegen/`
Expected: PASS (prints "1"). The immediate and UNIT retain/release were no-ops; the bignum freed at rc 0.

- [ ] **Step 5: Confirm the existing WASM gate**

Run: `go test -run 'Wasm|WASM' ./codegen/`
Expected: PASS (no emitted program calls retain/release yet, so behavior is unchanged).

- [ ] **Step 6: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_arc_test.go
git commit -m "feat(wasm): rt_retain/rt_release primitives (immediates + UNIT excluded)"
```

---

### Task 3: Recursive child release by kind

When a release reaches zero, release the object's child pointers before freeing it, so a whole immutable structure is reclaimed. Dispatch on the kind word over the four heap shapes (closure env, constructor fields, pair halves, bignum leaf).

**Files:**
- Modify: `codegen/wasm_runtime.go` (`$rt_free` recurses over children before dropping `$live`)
- Test: `codegen/wasm_arc_test.go` (a nested structure releases fully)

**Interfaces:**
- Consumes: the record layouts -- K_CLO=0 `[kind][code][nenv][env..]`, K_CON=1 `[kind][tag][name][nfield][slot..]`, K_PAIR=2 `[kind][a][b]`, K_BIG=6 `[kind][nlimbs][limb..]`; `$rt_release`, `$live`; the `$lw`/`$sw` word accessors.
- Produces: `$rt_free(v)` recursively releases child slots by kind, then drops `$live`.

- [ ] **Step 1: Write the failing test**

```go
// add to codegen/wasm_arc_test.go

// TestARCRecursiveRelease: build a constructor holding two bignum fields (a small
// tree), then release the root to zero. The root's release must recurse into both
// field bignums (each rc 1 -> 0), freeing all three blocks. live returns to 1 (UNIT).
// Without recursion, the two field bignums would leak and live would be 3.
func TestARCRecursiveRelease(t *testing.T) {
	body := `
    (local $f0 i32) (local $f1 i32) (local $root i32)
    (local.set $f0 (call $rt_big_from_i32 (i32.const 3)))
    (local.set $f1 (call $rt_big_from_i32 (i32.const 4)))
    ;; a 2-field constructor (tag 0, name ptr 64, nfield 2) holding the two bignums
    (local.set $root (call $rt_mkcon (i32.const 0) (i32.const 64) (i32.const 2)))
    (call $rt_con_set (local.get $root) (i32.const 0) (local.get $f0))
    (call $rt_con_set (local.get $root) (i32.const 1) (local.get $f1))
    ;; release the root: must recurse into f0 and f1
    (call $rt_release (local.get $root))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("after releasing the root tree, live = %q, want 1 (all 3 blocks freed)", out)
	}
}
```

(`$rt_con_set` writes a constructor field slot -- if the runtime exposes constructor-field writes under a different name, e.g. `$rt_con_field_set` or a direct `$sw` at the field offset, use that; the constructor field slots start at word 4 of the K_CON record, so `$rt_con_set(c,i,x)` is `$sw c (i+4) x`. If no such helper exists, write the fields directly with `$sw`: field i is at word index `4+i`.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestARCRecursiveRelease ./codegen/`
Expected: FAIL (prints "3" -- the two field bignums leak because `$rt_free` does not yet recurse).

- [ ] **Step 3: Implement recursive release**

Replace `$rt_free` in `codegen/wasm_runtime.go` with a version that releases children by kind before dropping `$live`:

```wat
  ;; rt_free: release the object's child pointers (so the whole immutable structure is
  ;; reclaimed), then drop the live count. The value graph is acyclic (immutable
  ;; functional data), so this terminates. Kinds: closure env, constructor fields, pair
  ;; halves; bignum is a leaf (limbs are immediate-int values, not pointers).
  (func $rt_free (param $v i32)
    (local $kind i32) (local $n i32) (local $i i32) (local $base i32)
    (local.set $kind (call $lw (local.get $v) (i32.const 0)))
    (block $done
      ;; K_CLO=0: env slots start at word 3, count at word 2
      (if (i32.eqz (local.get $kind))
        (then
          (local.set $n (call $lw (local.get $v) (i32.const 2)))
          (local.set $base (i32.const 3))
          (br $done)))
      ;; K_CON=1: field slots start at word 4, count at word 3
      (if (i32.eq (local.get $kind) (i32.const 1))
        (then
          (local.set $n (call $lw (local.get $v) (i32.const 3)))
          (local.set $base (i32.const 4))
          (br $done)))
      ;; K_PAIR=2: two slots at words 1 and 2
      (if (i32.eq (local.get $kind) (i32.const 2))
        (then
          (call $rt_release (call $lw (local.get $v) (i32.const 1)))
          (call $rt_release (call $lw (local.get $v) (i32.const 2)))
          (local.set $n (i32.const 0))
          (local.set $base (i32.const 0))
          (br $done)))
      ;; K_BIG and anything else: leaf, no child pointers
      (local.set $n (i32.const 0))
      (local.set $base (i32.const 0)))
    ;; release the $n slots starting at word $base (closures + constructors)
    (local.set $i (i32.const 0))
    (block $brk (loop $lp
      (br_if $brk (i32.ge_u (local.get $i) (local.get $n)))
      (call $rt_release (call $lw (local.get $v) (i32.add (local.get $base) (local.get $i))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    (global.set $live (i32.sub (global.get $live) (i32.const 1))))
```

(Verify the word indices against the real layout in `wasm_runtime.go`'s header comment: K_CLO env at word 3 with count at word 2, K_CON fields at word 4 with count at word 3, K_PAIR halves at words 1 and 2. If the runtime's K_CON stores `name_ptr` at a different word, adjust `base`/count indices to match the documented layout exactly. `$lw o i` loads word i of object o.)

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestARCRecursiveRelease ./codegen/`
Expected: PASS (prints "1": the root and both field bignums freed).

- [ ] **Step 5: Re-run the prior ARC tests + the WASM gate**

Run: `go test -run 'ARC|Wasm|WASM' ./codegen/`
Expected: PASS (the leaf test and the existing programs still hold).

- [ ] **Step 6: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_arc_test.go
git commit -m "feat(wasm): recursive child release by kind (closure/con/pair; bignum leaf)"
```

---

### Task 4: Size-classed free list (block reuse)

Make freed blocks reusable: `rt_free` pushes the block onto a free list keyed by exact size; `alloc` pops a matching-size block before bumping `$hp`. This bounds the heap under allocate/free churn.

**Files:**
- Modify: `codegen/wasm_runtime.go` (`$alloc` consults the free list; `$rt_free` pushes onto it)
- Test: `codegen/wasm_arc_test.go` (alloc/free churn reuses memory; `$hp` stays bounded)

**Interfaces:**
- Consumes: the header `[payload-8]=size`; `$hp`, `$live`, `$rt_free` (Tasks 1-3).
- Produces: a free list (a small hash/bucket array by size, or a single intrusive linked list per size found at runtime) that `alloc` reuses; a `$rt_hp() -> i32` accessor for the test.

- [ ] **Step 1: Write the failing test**

```go
// add to codegen/wasm_arc_test.go

// TestARCFreeListReuse: allocate-and-free the same size in a loop; with a free list,
// the heap pointer must NOT keep growing (the freed block is reused). Record $hp after
// the first alloc/free, churn many more, and assert $hp did not advance past a small
// bound. Without reuse, $hp grows by one block per iteration.
func TestARCFreeListReuse(t *testing.T) {
	body := `
    (local $i i32) (local $b i32) (local $hp0 i32)
    ;; warm: one alloc + free to seed the free list, capture hp
    (local.set $b (call $rt_big_from_i32 (i32.const 1)))
    (call $rt_release (local.get $b))
    (local.set $hp0 (call $rt_hp))
    ;; churn: 100 alloc+free of the same size; hp must not advance (block reused)
    (local.set $i (i32.const 0))
    (block $brk (loop $lp
      (br_if $brk (i32.ge_u (local.get $i) (i32.const 100)))
      (local.set $b (call $rt_big_from_i32 (i32.const 1)))
      (call $rt_release (local.get $b))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    ;; print whether hp advanced at all (0 = perfectly reused)
    (call $rt_print_u32 (i32.sub (call $rt_hp) (local.get $hp0))))`
	out := runWasm(t, arcTestModule(body))
	if out != "0" {
		t.Fatalf("hp advanced by %q bytes under same-size churn, want 0 (free list should reuse)", out)
	}
}
```

(`rt_big_from_i32(1)` allocates the same-size bignum each time. The test asserts perfect reuse for a single size class, which a correct same-size free list gives. If `rt_big_from_i32` allocates a size that varies with the value, use a fixed-size constructor instead: `rt_mkcon(0, 64, 0)` allocates a constant-size 0-field constructor -- swap to that if the bignum size is value-dependent.)

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -run TestARCFreeListReuse ./codegen/`
Expected: FAIL (`$rt_hp` undefined, or `$hp` advanced by 100 blocks because there is no reuse).

- [ ] **Step 3: Implement the free list**

Add a small size-bucketed free list to `codegen/wasm_runtime.go`. Use an intrusive singly-linked list per size, with a fixed-size bucket table indexed by `size/4` (records are tiny and 4-aligned; cap the table at, say, 64 buckets = sizes up to 256 bytes, and fall back to bump for larger). The freed block's first payload word holds the "next" link.

```wat
  ;; Free list: 64 buckets indexed by size/4 (sizes 0..252 bytes). Each bucket head is a
  ;; pointer to a freed PAYLOAD whose first word links to the next free block of that
  ;; size (intrusive list). Larger sizes are not pooled (bump only). Reserved low region.
  (global $freelist (mut i32) (i32.const 4608))  ;; 64 i32 bucket heads at [4608, 4864)

  ;; bucket address for a payload size (0 if out of range).
  (func $rt_bucket (param $size i32) (result i32) (result i32)
    (local $idx i32)
    (local.set $idx (i32.shr_u (local.get $size) (i32.const 2)))
    (if (result i32 i32) (i32.lt_u (local.get $idx) (i32.const 64))
      (then (i32.add (global.get $freelist) (i32.shl (local.get $idx) (i32.const 2))) (i32.const 1))
      (else (i32.const 0) (i32.const 0))))

  (func $rt_hp (result i32) (global.get $hp))
```

Then:
- In `$rt_free`, AFTER releasing children and dropping `$live`, push the block (the BASE = `v-8`) onto its size bucket: read `size = [v-8]`, get the bucket via `$rt_bucket`; if in range, store the current bucket head into the block's first payload word (the link), and set the bucket head to this payload.
- In `$alloc`, BEFORE bumping `$hp`, check the bucket for size `$n`: if the head is non-zero, pop it (set bucket head to the popped block's link word), re-init its header (`[base-... ]` -- since we pooled the PAYLOAD, the header is at payload-8 and stays valid; just reset rc=1 and bump `$live`), and return it; else bump as today.

Concretely, the reuse-aware `$alloc` (replacing Task 1's body) and the push in `$rt_free`:

```wat
  (func $alloc (param $n i32) (result i32)
    (local $base i32) (local $payload i32) (local $bkt i32) (local $ok i32) (local $head i32)
    ;; try the free list first
    (call $rt_bucket (local.get $n))
    (local.set $ok)            ;; second result: in-range flag
    (local.set $bkt)           ;; first result: bucket address
    (if (local.get $ok)
      (then
        (local.set $head (i32.load (local.get $bkt)))
        (if (local.get $head)
          (then
            ;; pop: bucket head := link word of the popped payload
            (i32.store (local.get $bkt) (i32.load (local.get $head)))
            (i32.store (i32.sub (local.get $head) (i32.const 4)) (i32.const 1)) ;; rc=1
            (global.set $live (i32.add (global.get $live) (i32.const 1)))
            (return (local.get $head))))))
    ;; else bump
    (local.set $base (global.get $hp))
    (local.set $payload (i32.add (local.get $base) (i32.const 8)))
    (global.set $hp (i32.add (local.get $payload) (local.get $n)))
    (i32.store (local.get $base) (local.get $n))
    (i32.store (i32.add (local.get $base) (i32.const 4)) (i32.const 1))
    (global.set $live (i32.add (global.get $live) (i32.const 1)))
    (local.get $payload))
```

And the push at the end of `$rt_free` (after the `$live` decrement), before returning:

```wat
    ;; push onto the size bucket (if poolable): link word = old head, head = this payload
    (local $size i32) (local $bkt i32) (local $ok i32)
    (local.set $size (i32.load (i32.sub (local.get $v) (i32.const 8))))
    (call $rt_bucket (local.get $size))
    (local.set $ok) (local.set $bkt)
    (if (local.get $ok)
      (then
        (i32.store (local.get $v) (i32.load (local.get $bkt)))  ;; link := old head
        (i32.store (local.get $bkt) (local.get $v))))           ;; head := this payload
```

(WAT multi-result `(func ... (result i32 i32))` and the two `local.set` pops are valid in wasmtime v42. If the two-result form is awkward, split `$rt_bucket` into `$rt_bucket_addr(size)->i32` returning 0 when out of range and test `addr != 0` instead -- simpler and equivalent. Pick whichever validates; the simpler single-result `0-means-out-of-range` form is recommended. Also: declare the extra locals at the TOP of `$rt_free` -- WAT requires all locals declared in the function header, so move `$size`/`$bkt`/`$ok` up with the other locals from Task 3.)

RECOMMENDED simplification (use this instead of the multi-result bucket): `$rt_bucket_addr(size) -> i32` returns the bucket address or 0 when out of range; callers test non-zero. Rewrite the three call sites accordingly.

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -run TestARCFreeListReuse ./codegen/`
Expected: PASS (prints "0": `$hp` did not advance under same-size churn).

- [ ] **Step 5: Re-run all ARC tests + the WASM gate**

Run: `go test -run 'ARC|Wasm|WASM' ./codegen/`
Expected: PASS (header, leaf, recursive, reuse, and every existing program).

- [ ] **Step 6: Commit**

```bash
git add codegen/wasm_runtime.go codegen/wasm_arc_test.go
git commit -m "feat(wasm): size-classed free list (block reuse bounds the heap)"
```

---

### Task 5: The ARC design doc + roadmap (the portable discipline)

Record the runtime contract, the acyclicity soundness argument, and the roadmap so the follow-on plans (the Perceus insertion pass, strings/bytes, the WebRTC FFI, and the C/LLVM port) are unambiguous.

**Files:**
- Create: `ref_docs/wootz/R-ARC.md`

**Interfaces:**
- Consumes: nothing.
- Produces: documentation only.

- [ ] **Step 1: Write the design doc**

Create `ref_docs/wootz/R-ARC.md` (ASCII hyphens only, no em/en dashes):

```markdown
# R-ARC: Automatic Reference Counting for the portable runtime

## Why ARC, and why no cycle collector
The erased IR builds only immutable, total, acyclic functional values: a value
cannot point back to one constructed after it, and nothing is ever mutated to
create a cycle. So reference counting WITHOUT a cycle collector is sound and
complete -- every unreachable object has refcount zero and is freed deterministically.
This is the Perceus / "Counting Immutable Beans" setting (Koka, Lean 4): precise
reference counting for a pure functional language, Swift-style determinism without
Swift's cycle hazards.

## The runtime substrate (Plan 6a, this runtime; codegen/wasm_runtime.go)
- Every heap block has a hidden 8-byte header below the returned payload pointer:
  [payload-8] = payload size in bytes, [payload-4] = refcount (init 1).
- rt_retain(v): no-op on immediates (low bit set) and the immortal UNIT; else rc += 1.
- rt_release(v): no-op on immediates/UNIT; else rc -= 1; at zero, release children by
  kind (closure env slots, constructor fields, pair halves; bignum is a leaf), then
  return the block to a size-classed free list.
- alloc(n): pop a same-size free block if available, else bump $hp; init rc=1; $live += 1.
- $live counts live blocks: alloc bumps, free drops. A balanced program returns $live
  to its baseline -- the leak / double-free probe the tests assert.

## The portable contract (for Plan 6e: port to C and LLVM)
The same four operations (retain, release, recursive-release-by-kind, size-classed
free list) port verbatim to the C and LLVM runtimes (codegen/c.go, codegen/ll_runtime.go),
REPLACING their mark-sweep GC. The record layouts already match across backends
(the bignum twin is documented in wasm_runtime.go); add the same hidden header and the
same retain/release. The win: deterministic, pause-free deallocation and one shared
discipline instead of per-backend collectors.

## The codegen insertion pass (Plan 6b: the hard part, its own plan + design)
A runtime with retain/release does nothing until codegen INSERTS the calls. Plan 6b is
the Perceus-style ownership pass over the closure-converted IR (codegen/closure.go:
CVar/CEnv/CGlobal/CLet/CApp/CLam/CCon/CCase/CField/CPair/CFst/CSnd/CBounce). The rules,
in brief: an owned binding used zero times is dropped (release); used N>1 times is
retained N-1 times; a variable consumed by a constructor/return transfers ownership;
borrowed positions (a scrutinee inspected but not stored) are not consumed. Reuse
(the "Beans" in-place update) is a later optimization. This pass must be designed
against the exact IR in a dedicated brainstorm before implementation, because correct
insertion is the subtle part (a missed release leaks; a missed retain is a
use-after-free). The $live probe from 6a is the regression net: any balanced program
must return $live to baseline after main.

## Downstream (the demo path)
- Plan 6c: strings and bytes as refcounted heap objects in WASM (Bin over the ARC heap).
- Plan 6d: the WebRTC FFI shim (the sandbox/no-native interop class) as WASM imports.
- Plan 6e: port the ARC discipline to C and LLVM (replace mark-sweep).
- Plan 6f: the two-tab CRDT browser app (WASM merge + JS/WebRTC glue + two divs).
- Plan 6g: the go-to-market script / book chapter (the proven-minimal-IAM + Ledger moments).
```

- [ ] **Step 2: Verify no dashes + commit**

Run: `grep -nP "[\x{2013}\x{2014}]" ref_docs/wootz/R-ARC.md`
Expected: no matches.

```bash
git add ref_docs/wootz/R-ARC.md
git commit -m "docs(wasm): R-ARC design + roadmap (substrate, portable contract, insertion pass)"
```

---

## Self-Review

**Spec coverage (against the chosen scope: full WASM-hardening with Swift-style ARC, portable to C/LL):**
- A refcounting (ARC) heap runtime for WASM: Tasks 1-4 (header, retain/release, recursive release, free list). Covered.
- Swift-style deterministic deallocation, no tracing GC: the free-at-zero design; the acyclicity argument (no cycle collector needed). Covered (Task 5 documents why it is sound).
- The discipline carries to C/LLVM: Task 5's portable contract documents the verbatim port (Plan 6e); the runtime ops are language-neutral. Covered (as the documented next plan).
- Observable correctness: the `$live` leak/double-free probe + the four wasmtime tests (header count, leaf balance, recursive balance, reuse). Covered.
- The codegen insertion pass + strings/bytes + WebRTC FFI + the browser app + GTM: explicitly scoped as Plans 6b-6g in Task 5's roadmap (this plan is the substrate they require). Covered as roadmap.

**Placeholder scan:** Every code step shows concrete WAT against the real heap layout documented in `wasm_runtime.go` (K_CLO/K_CON/K_PAIR/K_BIG offsets) and the real test harness (`runWasm`, `wasmtimePath`). The "use the real name if it differs" notes (`$rt_big_from_i32`, `$emit_u32`, `$puts`, `$rt_con_set`) name the exact runtime helper to find and mirror, because this plan cannot see every private WAT identifier without the file open; they are match-the-symbol instructions with the precise role stated, not placeholders. The free-list `$rt_bucket` two-result-vs-single-result note gives a concrete recommended form.

**Type consistency:** `$alloc` (Task 1) is extended in Task 4 with the same signature. `$rt_retain`/`$rt_release`/`$rt_counted`/`$rt_free` (Task 2) are used by Task 3 (recursion) and Task 4 (push). `$live`/`$rt_live`/`$rt_print_u32`/`$rt_hp` are the test observables threaded through all four tests. The header offsets (`[payload-8]=size`, `[payload-4]=rc`) are consistent across alloc, retain, release, free, and the free-list push/pop.

**Three honest scope notes for the author:**
1. This plan delivers the runtime SUBSTRATE only. No Rune program refcounts until Plan 6b inserts the calls; the substrate is unit-tested at the WAT level. The demo (Plan 6f) is several plans downstream -- this is the foundation the heavy-WASM-hardening path requires, sequenced honestly.
2. The WAT test scaffolding (`arcTestModule`) assembles a minimal module around `wasmRuntime`; the first task may need small adjustments (a referenced global, the table) until wasmtime validates it. The plan flags this; it is harness plumbing, not a design risk.
3. The free list pools only exact sizes up to a fixed cap (no splitting/coalescing). That is sufficient for the small, few-sized records (closures, constructors, pairs, bignums) the runtime allocates; a general allocator is out of scope for v1 and unneeded.

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-27-arc-wasm-runtime.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
