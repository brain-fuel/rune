package codegen_test

// perceus_test.go -- Plan 6b Tasks 2-3: CDup/CDrop nodes, WASM rendering, and the
// $live steady-state gate.
//
// Four tests:
//
//  1. TestPerceusDupDropBalances -- verifies the ARC runtime balances a hand-written
//     dup/drop sequence: allocate a bignum, retain twice (rc=3), release three times
//     (rc->0 -> freed). $rt_live returns 1 (back to just UNIT).
//
//  2. TestPerceusEmitterRendersDupDrop -- verifies that Perceus does not corrupt the
//     emitted module structure; the WAT contains $rune_main.
//
//  3. TestPerceusSteadyTrivial -- verifies that the steady-state gate has teeth:
//     under the real Perceus pass a succ chain with builtin-nat K_BIG bignums still
//     leaks (all closures are CGlobal -> no CLet+CDrop inserted). Counts GROW.
//
//  4. TestPerceusCoreDupDrop -- the Task 3 receiver: a closure used twice (forces
//     CDup) and a dead let-binding (forces CDrop) are balanced -- output correct AND
//     $rt_live flat after run 1.

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
	// Real Perceus does not insert CDrop for CGlobal-clo AppClosures (scope limitation
	// Task 3), so succ-chain intermediates still leak each run -> counts GROW.
	if !(c2 > c1) {
		t.Fatalf("expected a visible leak for succ-chain (run3 > run2), got flat %v", counts)
	}
}

// perceusCoreSrc is the Task 3 receiver program. It forces:
//
//   - ONE CDup: f (the identity closure over (Nat->Nat), a K_CLO) is owned
//     (CVar) in applyTwice's code block and used in BOTH the Clo and Arg
//     positions of the outer AppClosure in "f (f (fn z is z end))".
//
//   - ONE CDrop: "unused = fn (y : Nat) is y end" is a dead let-binding;
//     the K_CLO is allocated and immediately released.
//
//   - ONE CLet+CDrop on $clo (the post-call closure release): applyTwice's
//     code-block closure is a CVar so the AppClosure wrapper inserts
//     CLet("$clo", ...) / CDrop to release the closure after rt_apply.
//
// The argument to f is "fn (z : Nat) is z end" -- a fresh K_CLO with an
// empty env, allocated in the hot path. No bignum literals or bignum
// arithmetic appear in the hot path, so rt_big_parse's intermediate
// K_BIG allocations never fire and $rt_live stays flat after run 1.
//
// Output: "<function>" (the result is a Nat -> Nat closure).
const perceusCoreSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
applyTwice : ((Nat -> Nat) -> (Nat -> Nat)) -> (Nat -> Nat) is
  fn (f : (Nat -> Nat) -> (Nat -> Nat)) is
    let unused : Nat -> Nat = fn (y : Nat) is y end in
    f (f (fn (z : Nat) is z end))
  end
end
idFun : (Nat -> Nat) -> (Nat -> Nat) is fn (g : Nat -> Nat) is g end end
main : Nat -> Nat is applyTwice idFun end
`

// TestPerceusCoreDupDrop is the Task 3 receiver gate.
//
// OUTPUT-INVARIANCE: Perceus must not alter the computed value.
// applyTwice receives the identity on (Nat->Nat); it applies it twice
// to a fresh identity closure, yielding that closure back.  The WASM
// show function prints "<function>" for any K_CLO.
//
// STEADY-STATE: $rt_live must be flat after run 1. Each run allocates:
//
//   K_CLO_g      -- fn (g : Nat->Nat) is g end  (passed to applyTwice)
//   K_CLO_unused -- fn (y : Nat)      is y end  (dead let-binding, CDrop'd)
//   K_CLO_z      -- fn (z : Nat)      is z end  (argument to f, result)
//
// All three are allocated fresh each run and released before the loop
// advances: K_CLO_unused by CDrop, K_CLO_g by the two CLet+CDrop wrappers
// (CDup gives rc=2, two CDrop's bring it to 0), and K_CLO_z by the
// harness's $rt_release after printing.
func TestPerceusCoreDupDrop(t *testing.T) {
	// --- output-invariance gate ---
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusCoreSrc, "main"))
	if got != "<function>" {
		t.Fatalf("Perceus output-invariance: got %q, want \"<function>\"", got)
	}

	// --- steady-state gate ---
	p := mustProgram(t, perceusCoreSrc, "main")
	counts := wasmSteadyLiveP(t, p, 4)
	if len(counts) != 4 {
		t.Fatalf("want 4 per-run live counts, got %d: %v", len(counts), counts)
	}
	// Parse as integers to avoid lexicographic anomalies.
	c2, err2 := strconv.Atoi(counts[1])
	c3, err3 := strconv.Atoi(counts[2])
	c4, err4 := strconv.Atoi(counts[3])
	if err2 != nil || err3 != nil || err4 != nil {
		t.Fatalf("non-integer live counts: %v", counts)
	}
	// Balanced Perceus: runs 2-4 must all be the same (steady state).
	if !(c2 == c3 && c3 == c4) {
		t.Fatalf("Perceus steady-state FAIL: counts not flat after run 1: %v", counts)
	}
}
