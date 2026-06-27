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
		if i >= n || !strings.HasPrefix(body[i:], "(local") {
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
