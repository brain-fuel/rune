package codegen

// wasm_steady_test.go -- the Perceus steady-state balance harness builder.
//
// This is a WHITEBOX test file (package codegen, not package codegen_test) so it
// can access unexported internals (wasmEmitter, wasmFunc, wasmRuntime, emitDefs,
// etc.) while still being compiled only during testing (the _test.go suffix). Its
// exported symbol WasmSteadyModule is accessible to perceus_test.go via the cg.
// import in package codegen_test -- the standard Go "export for test" pattern.
//
// go build ./... never compiles this file, so importing "testing" does not pull
// test flags into the shipped binary.

import (
	"fmt"
	"strings"
	"testing"
)

// WasmSteadyModule builds a self-contained WAT module whose _start:
//  1. Seeds the immortal UNIT singleton.
//  2. Loops `runs` times, each iteration:
//     a. Evaluates the main definition's body DIRECTLY (not through the cached
//        thunk, so each run freshly executes the body -- see note below).
//     b. Releases the result with $rt_release.
//     c. Prints $rt_live (the live-block count) as a decimal integer.
//     d. Prints a newline.
//
// A leak-free program (balanced under Perceus) reaches steady state after the
// first run: runs 2..N report the same $rt_live value. Under IDENTITY Perceus
// (Task 2), intermediate allocations not linked into the result (e.g. the K_BIG
// temporaries in a bignum succ chain) accumulate each run, making $rt_live grow --
// demonstrating the gate has teeth before the algorithm exists.
//
// Note on DIRECT body emission: evaluating the main def's CIr body directly
// (rather than calling the memoized thunk via CGlobal) causes each loop iteration
// to perform the body's allocations fresh. Sub-expression thunks (zero, succ, add)
// are still memoized (first-call caches), so only the intermediate values that are
// NOT linked into the final result leak per-run under identity Perceus.
//
// TEST-ONLY: takes *testing.T so it is compiled only in the test binary (never in
// the shipped codegen package).
func WasmSteadyModule(t *testing.T, p Program, runs int) string {
	t.Helper()
	cp := Perceus(ClosureConvert(p))
	em := newWasmEmitter(cp)

	// Find the main def's CIr body so we can emit it directly in the loop.
	// Fall back to CGlobal (thunk) if the def is not found (should not happen).
	var mainBody CIr = CGlobal{Name: cp.Main}
	for _, d := range cp.Defs {
		if d.Name == cp.Main {
			mainBody = d.Body
			break
		}
	}

	// Emit all code blocks + def thunks into `defs`.
	var defs strings.Builder
	em.emitDefs(&defs)

	// Emit the main body into a temp buffer to collect fresh locals.
	// These locals are hoisted into the $_start function header below.
	var inner strings.Builder
	f := &wasmFunc{em: em}
	mainV := f.emitIn(&inner, mainBody, nil)

	// Assemble the module.
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(wasmRuntime)
	b.WriteString("\n")
	// rt_print_nl is test-only (steady-state harness); not part of the frozen runtime.
	b.WriteString("  (func $rt_print_nl\n")
	b.WriteString("    (i32.store8 (i32.const 4096) (i32.const 10))\n")
	b.WriteString("    (call $puts (i32.const 1) (i32.const 4096) (i32.const 1)))\n")
	b.WriteString("\n")
	em.emitData(&b)
	b.WriteString("\n")
	b.WriteString(defs.String())
	em.emitTable(&b)

	// Build $_start with all locals hoisted before the first instruction.
	// WAT requires every (local ...) declaration to precede every instruction in
	// the function; we satisfy this by writing f.localDecls() (the locals
	// accumulated by emitting the main body) before the seed-UNIT instruction.
	b.WriteString("  (func $_start (export \"_start\")\n")
	b.WriteString("    (local $i i32) (local $r i32)\n")
	b.WriteString(f.localDecls())
	// Seed the immortal UNIT singleton (first instruction after all locals).
	b.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (global.get $unit_name) (i32.const 0)))\n")
	// Loop: evaluate main body, release result, print live count, print newline.
	b.WriteString("    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $brk (loop $lp\n")
	fmt.Fprintf(&b, "      (br_if $brk (i32.ge_u (local.get $i) (i32.const %d)))\n", runs)
	b.WriteString(inner.String())
	fmt.Fprintf(&b, "      (local.set $r %s)\n", mainV)
	b.WriteString("      (call $rt_release (local.get $r))\n")
	b.WriteString("      (call $rt_print_u32 (call $rt_live))\n")
	b.WriteString("      (call $rt_print_nl)\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $lp)))\n")
	b.WriteString("  )\n)\n")
	return b.String()
}
