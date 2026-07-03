package codegen_test

import (
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// wasm_arc_test.go -- Plan 6a ARC header + refcount primitives tests.
// Converted to package codegen_test so it reuses the runWasm helper in wasm_test.go
// and reaches the runtime via codegen.WasmRuntime() (the exported accessor).

// splitWatLocals separates leading (local ...) declarations from the rest of a WAT
// function body. WAT requires all local declarations to precede every instruction
// in a function, but arcTestModule emits (global.set $UNIT ...) as the first
// instruction - so any locals in the body must be hoisted above it.
func splitWatLocals(body string) (locals, rest string) {
	i := 0
	n := len(body)
	for i < n {
		// skip whitespace
		for i < n && (body[i] == ' ' || body[i] == '\t' || body[i] == '\n' || body[i] == '\r') {
			i++
		}
		isLocalDecl := strings.HasPrefix(body[i:], "(local ") ||
			strings.HasPrefix(body[i:], "(local\t") ||
			strings.HasPrefix(body[i:], "(local$")
		if i >= n || !isLocalDecl {
			break
		}
		// scan balanced parens for one (local ...) s-expression
		depth := 0
		j := i
		for j < n {
			if body[j] == '(' {
				depth++
			}
			if body[j] == ')' {
				depth--
				if depth == 0 {
					j++
					break
				}
			}
			j++
		}
		i = j
	}
	return body[:i], body[i:]
}

// arcTestModule wraps the wasmRuntime with the module scaffolding (the codety type and
// a minimal funcref table so rt_apply's call_indirect validates) plus a custom _start
// body that exercises the ARC primitives and prints a result. It mirrors the module
// shell in Wasm.Emit but with a test entrypoint instead of rune_main.
//
// The four globals ($fn_msg, $abort_msg, $abort_len, $unit_name) are referenced by
// $show/$rt_abort in wasmRuntime but emitted by emitData in the real module. Dummy
// constant values are sufficient here because the test never calls $show or $rt_abort.
//
// WAT rule: all (local ...) declarations must precede every instruction in a function.
// Because arcTestModule emits (global.set $UNIT ...) as the first instruction, any
// locals in startBody are hoisted above it automatically.
func arcTestModule(startBody string) string {
	locals, code := splitWatLocals(startBody)
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(codegen.WasmRuntime())
	// Scaffolding globals referenced by the runtime but supplied by emitData in a real
	// module. Dummy constants suffice (test never calls $show or $rt_abort).
	b.WriteString("  (global $fn_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_len i32 (i32.const 0))\n")
	b.WriteString("  (global $unit_name i32 (i32.const 32))\n")
	b.WriteString("\n  (table 1 funcref)\n")
	b.WriteString("  (func $_start (export \"_start\")\n")
	// Hoist any leading (local ...) declarations above the first instruction.
	if locals != "" {
		b.WriteString("    ")
		b.WriteString(strings.TrimSpace(locals))
		b.WriteString("\n")
	}
	b.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (i32.const 64) (i32.const 0)))\n")
	b.WriteString(code)
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

// TestARCRecursiveRelease: build a constructor holding two bignum fields (a small
// tree), then release the root to zero. The root's release must recurse into both
// field bignums (each rc 1 -> 0), freeing all three blocks. live returns to 1 (UNIT).
// Without recursion, the two field bignums would leak and live would be 3.
func TestARCRecursiveRelease(t *testing.T) {
	body := `
    (local $f0 i32) (local $f1 i32) (local $root i32)
    (local.set $f0 (call $rt_big_from_long (i32.const 3)))
    (local.set $f1 (call $rt_big_from_long (i32.const 4)))
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

// TestARCFreeListReuse: alloc+free the same size in a loop; with a working free list the
// heap pointer must NOT advance (the freed block is reused). We warm the list with one
// alloc+free to seed the bucket, capture $hp, then churn 100 more alloc+free cycles of
// the SAME size. hp_final - hp0 must be 0 (perfect reuse: every alloc hits the free list).
// Without reuse $hp would grow by one block per iteration. Uses $rt_big_from_long(1) --
// a 1-limb K_BIG with a fixed 12-byte payload (size < 256 so it is poolable), then
// $rt_hp to observe whether $hp advanced.
func TestARCFreeListReuse(t *testing.T) {
	body := `
    (local $i i32) (local $b i32) (local $hp0 i32)
    ;; warm: one alloc + free to seed the free list, capture hp
    (local.set $b (call $rt_big_from_long (i32.const 1)))
    (call $rt_release (local.get $b))
    (local.set $hp0 (call $rt_hp))
    ;; churn: 100 alloc+free of the same size; hp must not advance (block reused)
    (local.set $i (i32.const 0))
    (block $brk (loop $lp
      (br_if $brk (i32.ge_u (local.get $i) (i32.const 100)))
      (local.set $b (call $rt_big_from_long (i32.const 1)))
      (call $rt_release (local.get $b))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    ;; print whether hp advanced at all (0 = perfectly reused)
    (call $rt_print_u32 (i32.sub (call $rt_hp) (local.get $hp0)))`
	out := runWasm(t, arcTestModule(body))
	if out != "0" {
		t.Fatalf("hp advanced by %q bytes under same-size churn, want 0 (free list should reuse)", out)
	}
}

// TestARCPairRelease: allocate two bignums, build a pair holding them, release the pair
// root; assert rt_live returns "1" (back to just UNIT, all three blocks freed). Without
// the K_PAIR child-release branch in $rt_free the two bignums would leak (live = 3).
// $rt_mkpair stores its two args directly at words 1 and 2 (no separate set call needed).
func TestARCPairRelease(t *testing.T) {
	body := `
    (local $f0 i32) (local $f1 i32) (local $root i32)
    (local.set $f0 (call $rt_big_from_long (i32.const 3)))
    (local.set $f1 (call $rt_big_from_long (i32.const 4)))
    ;; a pair whose two halves ARE the two bignums (stored at words 1 and 2 by $rt_mkpair)
    (local.set $root (call $rt_mkpair (local.get $f0) (local.get $f1)))
    ;; release the root: must recurse into f0 and f1 via the K_PAIR branch
    (call $rt_release (local.get $root))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("after releasing the pair root, live = %q, want 1 (all 3 blocks freed)", out)
	}
}

// TestARCClosureRelease: allocate a closure with nenv=2, fill its two env slots with
// bignums via $rt_clo_set, release the closure root; assert rt_live returns "1". Without
// the K_CLO env-release loop in $rt_free the two bignums would leak (live = 3). The
// closure's code index is 0 -- the test never CALLS the closure, only releases it.
func TestARCClosureRelease(t *testing.T) {
	body := `
    (local $f0 i32) (local $f1 i32) (local $c i32)
    (local.set $f0 (call $rt_big_from_long (i32.const 3)))
    (local.set $f1 (call $rt_big_from_long (i32.const 4)))
    ;; closure: code index 0, nenv=2
    (local.set $c (call $rt_mkclo (i32.const 0) (i32.const 2)))
    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $f0))
    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $f1))
    ;; release the closure: must walk env slots 0 and 1 via the K_CLO branch
    (call $rt_release (local.get $c))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("after releasing the closure, live = %q, want 1 (all 3 blocks freed)", out)
	}
}

// TestARCLeafRetainRelease: a bignum (a leaf object, no child pointers) is allocated
// (rc=1, live=2 with UNIT), retained twice (rc=3), released three times (rc 0 -> freed,
// live back to 1, just UNIT). An immediate int and the UNIT singleton are also passed
// to retain/release and must be ignored (no crash, live unchanged).
func TestARCLeafRetainRelease(t *testing.T) {
	body := `
    (local $b i32) (local $imm i32)
    (local.set $b (call $rt_big_from_long (i32.const 7)))
    (call $rt_retain (local.get $b))
    (call $rt_retain (local.get $b))
    ;; immediates and UNIT are never counted
    (local.set $imm (i32.const 11))
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

// TestARCBigBucketReuse: a 1KB payload (a big K_BIG) allocated, released, and
// re-allocated must reuse the SAME block: hp does not advance across the second
// alloc, and $live returns to baseline after each release. BEFORE Task 1 the
// >=256B free is orphaned, so the second alloc bumps hp.
func TestARCBigBucketReuse(t *testing.T) {
	body := `
    (local $a i32) (local $hp0 i32) (local $b i32)
    ;; 1024-byte payload via big_alloc (254 limbs -> 8+1016 bytes, rounds to 1024)
    (local.set $a (call $big_alloc (i32.const 254)))
    (call $rt_release (local.get $a))
    (local.set $hp0 (call $rt_hp))
    (local.set $b (call $big_alloc (i32.const 254)))
    (call $rt_release (local.get $b))
    ;; result: 1 iff hp unchanged AND the two payloads were the same block
    (call $rt_print_u32 (i32.and
      (i32.eq (call $rt_hp) (local.get $hp0))
      (i32.eq (local.get $a) (local.get $b))))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("big payload not pooled: probe=%q want 1", out)
	}
}

// TestARCBinLeafRelease: build a 300-byte K_BIN (over the small-bucket line, so
// it also exercises Task 1's big-bucket path), set/read two bytes and the length,
// then release it. $live must return to baseline and the bytes must round-trip.
func TestARCBinLeafRelease(t *testing.T) {
	body := `
    (local $b i32) (local $l0 i32) (local $ok i32)
    (local.set $l0 (call $rt_live))
    (local.set $b (call $rt_mkbin (i32.const 300)))
    (call $rt_bin_set (local.get $b) (i32.const 0) (i32.const 65))
    (call $rt_bin_set (local.get $b) (i32.const 299) (i32.const 90))
    (local.set $ok (i32.and
      (i32.and (i32.eq (call $rt_bin_at (local.get $b) (i32.const 0)) (i32.const 65))
               (i32.eq (call $rt_bin_at (local.get $b) (i32.const 299)) (i32.const 90)))
      (i32.eq (call $rt_bin_len (local.get $b)) (i32.const 300))))
    (call $rt_release (local.get $b))
    (call $rt_print_u32 (i32.and (local.get $ok) (i32.eq (call $rt_live) (local.get $l0))))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("K_BIN leaf lifecycle broken: probe=%q want 1", out)
	}
}

// TestARCBinShowClampNoCorruption: showing a value whose rendering overflows the
// [4096,8192) show window must NOT corrupt the ARC freelist bucket heads that sit
// immediately past it at [8192,8484). A 2000-byte all-0xFF Bin renders as ~8000
// emitted bytes (each 0xFF byte expands to the 4-byte "\xff" escape), almost double
// the window -- BEFORE the $emit_byte clamp this walked straight through the freelist
// heads and overwrote them, so the next allocation popped a corrupted bucket head and
// wasmtime trapped (exit 134). This pins the fix: show the oversized Bin (truncating
// on WASM, per the $emit_byte doc comment), release it, then allocate + round-trip a
// fresh 300-byte Bin -- if a freelist head were corrupted this either traps or reads
// back garbage. $live must also return to its pre-test baseline.
func TestARCBinShowClampNoCorruption(t *testing.T) {
	body := `
    (local $l0 i32) (local $b i32) (local $i i32) (local $c i32) (local $ok i32)
    (local.set $l0 (call $rt_live))
    ;; a 2000-byte Bin, every byte 0xFF (the non-printable escape path, 4 emitted
    ;; bytes per input byte -- the window-busting shape)
    (local.set $b (call $rt_mkbin (i32.const 2000)))
    (local.set $i (i32.const 0))
    (block $brk (loop $lp
      (br_if $brk (i32.ge_u (local.get $i) (i32.const 2000)))
      (call $rt_bin_set (local.get $b) (local.get $i) (i32.const 255))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    ;; show it: without the $emit_byte clamp this overruns [4096,8192) into the
    ;; freelist bucket heads at [8192,8484) and corrupts them
    (call $rt_show_line (local.get $b))
    (call $rt_release (local.get $b))
    ;; allocate again: if a freelist bucket head was corrupted by the overrun, this
    ;; pops a garbage pointer and wasmtime traps
    (local.set $c (call $rt_mkbin (i32.const 300)))
    (call $rt_bin_set (local.get $c) (i32.const 0) (i32.const 42))
    (local.set $ok (i32.and
      (i32.eq (call $rt_bin_at (local.get $c) (i32.const 0)) (i32.const 42))
      (i32.eq (call $rt_bin_len (local.get $c)) (i32.const 300))))
    (call $rt_release (local.get $c))
    (local.set $ok (i32.and (local.get $ok) (i32.eq (call $rt_live) (local.get $l0))))
    (call $rt_print_u32 (local.get $ok))`
	out := runWasm(t, arcTestModule(body))
	// out is the truncated show text (only '\', 'x', 'f', '"' bytes -- no digits)
	// immediately followed by the ok flag, so a bare digit suffix is unambiguous.
	if !strings.HasSuffix(out, "1") {
		tail := out
		if len(tail) > 40 {
			tail = tail[len(tail)-40:]
		}
		t.Fatalf("post-show allocation broken or unbalanced: probe tail=%q want ...1", tail)
	}
}

// TestARCBinHugeOrphanBalanced: a >64KB K_BIN is above the big-bucket ceiling
// ($alloc only power-of-two-rounds sizes in [256, 65536]): released memory is NOT
// pooled ($hp does not rewind, the block is orphaned) but $live still balances. Pins
// the documented boundary from Task 1/4 -- a >64KB payload is deliberately excluded
// from the free list, not silently mishandled.
func TestARCBinHugeOrphanBalanced(t *testing.T) {
	body := `
    (local $l0 i32) (local $b i32) (local $hp0 i32) (local $c i32)
    (local.set $l0 (call $rt_live))
    (local.set $b (call $rt_mkbin (i32.const 70000)))
    (call $rt_release (local.get $b))
    (local.set $hp0 (call $rt_hp))
    (local.set $c (call $rt_mkbin (i32.const 70000)))
    (call $rt_release (local.get $c))
    ;; balanced live AND the second alloc did NOT reuse (hp advanced): the
    ;; orphan boundary, documented not hidden
    (call $rt_print_u32 (i32.and (i32.eq (call $rt_live) (local.get $l0))
             (i32.gt_u (call $rt_hp) (local.get $hp0))))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("orphan boundary moved: probe=%q want 1", out)
	}
}
