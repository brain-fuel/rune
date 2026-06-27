package codegen_test

// perceus_test.go -- Plan 6b Task 2: CDup/CDrop nodes, WASM rendering, and the
// $live steady-state gate.
//
// Three tests:
//
//  1. TestPerceusDupDropBalances -- verifies the ARC runtime balances a hand-written
//     dup/drop sequence: allocate a bignum, retain twice (rc=3), release three times
//     (rc->0 -> freed). $rt_live returns 1 (back to just UNIT).
//
//  2. TestPerceusEmitterRendersDupDrop -- verifies that Perceus (currently identity)
//     does not corrupt the emitted module structure; the WAT contains $rune_main.
//
//  3. TestPerceusSteadyTrivial -- verifies that the steady-state gate has teeth:
//     under IDENTITY Perceus a succ chain with builtin-nat K_BIG bignums leaks
//     intermediate bignum allocations each run (K_BIG has no child pointers, so a
//     single $rt_release on the final result does not free the intermediates). The
//     test asserts counts GROW run-over-run, proving the gate detects the leak.

import (
	"strconv"
	"strings"
	"testing"

	cg "goforge.dev/rune/v3/codegen"
	"goforge.dev/rune/v3/internal/session"
)

// mustProgram builds an erased program from rune source, failing on error.
// Mirrors mustEmitProgram from closure_test.go for use in perceus_test.go.
func mustProgram(t *testing.T, src, main string) cg.Program {
	t.Helper()
	s := session.New()
	if _, err := s.LoadSource(src); err != nil {
		t.Fatalf("mustProgram load: %v", err)
	}
	p, err := s.EmitProgram(main)
	if err != nil {
		t.Fatalf("mustProgram emit: %v", err)
	}
	return p
}

// wasmSteadyLiveP runs WasmSteadyModule with the given pre-built Program and
// returns the per-run $rt_live counts as strings (one per line of output).
func wasmSteadyLiveP(t *testing.T, p cg.Program, runs int) []string {
	t.Helper()
	mod := cg.WasmSteadyModule(t, p, runs)
	out := runWasm(t, mod)
	return strings.Split(strings.TrimSpace(out), "\n")
}

// TestPerceusDupDropBalances: allocate a bignum, retain twice (rc 1->3), release
// three times (rc 3->0 -> freed). With a correctly balanced dup/drop sequence the
// ARC runtime returns $rt_live to 1 (UNIT only). This pins $rt_retain/$rt_release
// balance through the runtime path that CDup/CDrop rendering will exercise.
func TestPerceusDupDropBalances(t *testing.T) {
	body := `
    (local $v i32)
    (local.set $v (call $rt_big_from_long (i32.const 9)))
    (call $rt_retain (local.get $v))
    (call $rt_retain (local.get $v))
    (call $rt_release (local.get $v))
    (call $rt_release (local.get $v))
    (call $rt_release (local.get $v))
    (call $rt_print_u32 (call $rt_live))`
	out := runWasm(t, arcTestModule(body))
	if out != "1" {
		t.Fatalf("dup/drop balance: live = %q, want 1", out)
	}
}

// TestPerceusEmitterRendersDupDrop: verifies the Perceus identity pass does not
// corrupt the emitted WAT module structure. The module must contain $rune_main
// (the entry function emitted by Wasm.Emit) -- proving Perceus is wired but
// leaves the output unchanged.
func TestPerceusEmitterRendersDupDrop(t *testing.T) {
	src := natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`
	wat := emitWith(t, cg.Wasm{}, src, "three")
	if !strings.Contains(wat, "rune_main") {
		t.Fatalf("Perceus identity altered module structure: $rune_main not found in WAT")
	}
}

// steadySrc is a minimal rune source with builtin-nat bignums. With builtin nat,
// $rt_big_succ creates a NEW K_BIG each time (no child-pointer link to the input).
// So releasing the final succ^N result does not recursively free the intermediate
// K_BIG objects created during evaluation -- they are properly-allocated leaks under
// identity Perceus, making $rt_live grow per run.
const steadySrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
three : Nat is succ (succ (succ zero)) end
`

// TestPerceusSteadyTrivial: the steady-state gate shows that identity Perceus
// leaks. With builtin-nat succ, each loop iteration of WasmSteadyModule freshly
// evaluates the three body:
//
//	succ (succ (succ zero))
//
// = three rt_big_succ calls. Each call allocates a fresh K_BIG without linking it
// to the argument (K_BIG is a leaf: no child pointers). The single $rt_release on
// the final K_BIG(3) does NOT free K_BIG(1) or K_BIG(2) -- they leak. After the
// first run the cached zero and succ thunks stabilize; runs 2..N each add 2 leaked
// K_BIG objects. The test asserts counts GROW (run 3 > run 2), proving the gate
// detects leaks and has teeth before Task 3 fills in the real algorithm.
func TestPerceusSteadyTrivial(t *testing.T) {
	p := mustProgram(t, steadySrc, "three")
	counts := wasmSteadyLiveP(t, p, 3)
	if len(counts) != 3 {
		t.Fatalf("want 3 per-run live counts, got %d: %v", len(counts), counts)
	}
	// Parse as integers to avoid lexicographic comparison anomalies on multi-digit values.
	c1, err1 := strconv.Atoi(counts[1])
	c2, err2 := strconv.Atoi(counts[2])
	if err1 != nil || err2 != nil {
		t.Fatalf("non-integer live counts: %v", counts)
	}
	// Identity Perceus: intermediates leak each run -> counts GROW. The gate has teeth.
	// Task 3+ will make a balanced program show flat counts here.
	if !(c2 > c1) {
		t.Fatalf("expected a visible leak under identity Perceus (run3 > run2), got flat %v", counts)
	}
}
