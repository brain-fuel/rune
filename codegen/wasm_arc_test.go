package codegen

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// wasm_arc_test.go -- Plan 6a Task 1: hidden ARC header (size + refcount) + live-block
// counter. Internal test package so arcTestModule can reference wasmRuntime directly
// without exporting it.

// arcWasmtimePath resolves the wasmtime binary (mirrors the helper in wasm_test.go,
// which lives in package codegen_test and is not accessible here).
func arcWasmtimePath() string {
	if home, err := os.UserHomeDir(); err == nil {
		cand := filepath.Join(home, ".wasmtime", "bin", "wasmtime")
		if _, err := os.Stat(cand); err == nil {
			return cand
		}
	}
	if p, err := exec.LookPath("wasmtime"); err == nil {
		return p
	}
	return ""
}

// runWasm writes the WAT to a temp file and runs it with wasmtime, returning trimmed
// stdout. Skips when wasmtime is absent or -short is set.
func runWasm(t *testing.T, wat string) string {
	t.Helper()
	if testing.Short() {
		t.Skip("skipping wasmtime run in -short mode")
	}
	wt := arcWasmtimePath()
	if wt == "" {
		t.Skip("wasmtime not found (~/.wasmtime/bin/wasmtime or PATH)")
	}
	dir := t.TempDir()
	f := dir + "/module.wat"
	if err := os.WriteFile(f, []byte(wat), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command(wt, "run", f).CombinedOutput()
	if err != nil {
		t.Fatalf("wasmtime: %v\n%s\n--- emitted .wat ---\n%s", err, out, wat)
	}
	return strings.TrimSpace(string(out))
}

// arcTestModule wraps the wasmRuntime with the module scaffolding (the codety type and
// a minimal funcref table so rt_apply's call_indirect validates) plus a custom _start
// body that exercises the ARC primitives and prints a result. It mirrors the module
// shell in Wasm.Emit but with a test entrypoint instead of rune_main.
//
// The four globals ($fn_msg, $abort_msg, $abort_len, $unit_name) are referenced by
// $show/$rt_abort in wasmRuntime but emitted by emitData in the real module. Dummy
// constant values are sufficient here because the test never calls $show or $rt_abort.
func arcTestModule(startBody string) string {
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(wasmRuntime)
	// Scaffolding globals referenced by the runtime but supplied by emitData in a real
	// module. Dummy constants suffice (test never calls $show or $rt_abort).
	b.WriteString("  (global $fn_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_len i32 (i32.const 0))\n")
	b.WriteString("  (global $unit_name i32 (i32.const 32))\n")
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
