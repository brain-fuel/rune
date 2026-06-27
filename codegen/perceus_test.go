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

// assertSteadyFlat runs the PATH B gate for a receiver: OUTPUT-INVARIANCE (the
// Perceus-emitted WASM prints wantOut) AND STEADY-STATE (after run 1, $rt_live is
// flat: runs 2..4 are equal). The two together pin balance + correctness.
func assertSteadyFlat(t *testing.T, src, main, wantOut string) {
	t.Helper()
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, main))
	if got != wantOut {
		t.Fatalf("output-invariance: got %q, want %q", got, wantOut)
	}
	p := mustProgram(t, src, main)
	counts := wasmSteadyLiveP(t, p, 4)
	if len(counts) != 4 {
		t.Fatalf("want 4 per-run live counts, got %d: %v", len(counts), counts)
	}
	c2, err2 := strconv.Atoi(counts[1])
	c3, err3 := strconv.Atoi(counts[2])
	c4, err4 := strconv.Atoi(counts[3])
	if err2 != nil || err3 != nil || err4 != nil {
		t.Fatalf("non-integer live counts: %v", counts)
	}
	if !(c2 == c3 && c3 == c4) {
		t.Fatalf("steady-state FAIL: counts not flat after run 1: %v", counts)
	}
}

// pathBArgSrc is the PATH B argument-ownership receiver source. `dropArg` takes a
// (Nat -> Nat) argument it never uses and returns a fresh identity closure bound via a
// `let` (so the block body is a CLet -- a REAL CONSUMER, not a bare-MkClosure
// curry-through), so its code block OWNS CVar{0} and (under PATH B) DROPS it as a dead
// owned local. Two mains feed it the same shape from two different ownership origins:
//
//   - mainFresh: a freshly-allocated closure (owned). The drop frees it each run
//     (balanced: alloc +1, drop -1). Under the old argCount skip the arg is NOT
//     dropped, so the fresh closure LEAKS and $rt_live grows -- TestPerceusDeadFreshArg
//     FAILS before this task, PASSES after.
//   - mainRoot: a CGlobal thunk-cached root (`root`). dup-on-consume-borrowed dups it
//     before the call, so the callee drops a FRESH reference and the cache's reference
//     survives -- no use-after-free. Without the dup, removing the skip would
//     cascade-free the cached root and corrupt the cache (TestPerceusCachedRootArg).
const pathBArgSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
dropArg : (Nat -> Nat) -> (Nat -> Nat) is
  fn (a : Nat -> Nat) is
    let r : Nat -> Nat = fn (z : Nat) is z end in r
  end
end
root : Nat -> Nat is fn (n : Nat) is n end end
mainFresh : Nat -> Nat is dropArg (fn (w : Nat) is w end) end
mainRoot  : Nat -> Nat is dropArg root end
`

// TestPerceusDeadFreshArg: a dead freshly-allocated owned argument is DROPPED, so
// steady-state is flat (the closure is freed each run). This proves removing the
// argCount dead-drop skip works. Under the old workaround the closure leaks
// (counts grow) and this test fails.
func TestPerceusDeadFreshArg(t *testing.T) {
	assertSteadyFlat(t, pathBArgSrc, "mainFresh", "<function>")
}

// TestPerceusCachedRootArg: a CGlobal thunk-cached root passed as a function
// argument runs to steady state with correct output and flat $rt_live. This proves
// dup-on-consume-borrowed protects the cache: the caller dups the borrowed root, so
// the callee dropping its (now fresh) argument leaves the cache intact. Without the
// dup, removing the skip would free the cached root under the cache and corrupt it.
func TestPerceusCachedRootArg(t *testing.T) {
	assertSteadyFlat(t, pathBArgSrc, "mainRoot", "<function>")
}

// pathBCaptureSrc is the capture-then-use-later receiver. `g` is the owned argument
// of capUse; it is CAPTURED into `cap`'s closure environment (one consuming store)
// AND applied again as `cap g` (a second consume). A var consumed twice needs one
// dup; the MkClosure-only escape-dup of Task 3 missed the cross-let case. Under PATH
// B the CLet annotation inserts the dup, so the two consumes are balanced and
// steady-state is flat. The body of `cap` returns its captured `g` (a CEnv), which
// dup-on-consume-borrowed dups at the return position.
const pathBCaptureSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
capUse : (Nat -> Nat) -> (Nat -> Nat) is
  fn (g : Nat -> Nat) is
    let cap : (Nat -> Nat) -> (Nat -> Nat) = fn (x : Nat -> Nat) is g end in
    cap g
  end
end
mainCap : Nat -> Nat is capUse (fn (m : Nat) is m end) end
`

// TestPerceusCaptureUseLater: an owned local captured into a closure env AND used
// later in the continuation is consumed twice and must be dup'd. Steady-state flat +
// correct output proves the cross-let capture dup (Task 3.5 item 4) balances.
func TestPerceusCaptureUseLater(t *testing.T) {
	assertSteadyFlat(t, pathBCaptureSrc, "mainCap", "<function>")
}

// perceusCaseSrc is the Task 4 receiver: a two-constructor datatype OptF, matched by
// its lowered eliminator OptFElim, whose `someF x` arm KEEPS the projected field.
//
// The final lambda-x block of the lowered OptFElim is a CCase whose scrutinee is the
// block's owned argument CVar{0} (the value `someF (fn w is w end)`). It arrives OWNED
// (a fresh K_CON the constructor application built). The receiver forces every Task 4
// ownership event:
//
//   - BORROWED SCRUTINEE, OWNED-AT-END: the scrutinee is borrowed for the tag read and
//     the arm's field read (rt_con_tag / rt_con_get are aliases); after the arm it is
//     owned and is DROPPED on the arm path (drop-after). Without the drop the K_CON
//     leaks each run ($live grows); the steady gate catches it.
//   - CField DUP-ON-ESCAPE (the Task 3.5 Minor): the `someF x` arm returns the
//     projected field (a kept closure). consumeOwning dups that field at the AppClosure
//     argument position so it carries its own reference PAST the scrutinee's free. With
//     no dup the scrutinee drop would free the kept closure -> use-after-free (corrupt
//     output / trap), which the output-invariance gate catches.
//   - PER-ARM LIVENESS + CURRY SPINE: two arms (the dead `noneF` arm's case capture is
//     owned per arm), and the saturated 4-argument OptFElim curry spine's intermediate
//     closures are released (PATH B: every application returns an OWNED closure).
//
// The eliminator is PARTIAL-APPLIED to its motive + cases at top level (`matchF`), so
// the saturated curry spine -- which allocates one fresh intermediate K_CLO per applied
// argument -- is a CACHED THUNK built ONCE, not rebuilt per run. (The pass cannot free
// curry-spine intermediates in-band: the WASM emitter recognizes the frozen nat-fold /
// accel spine by pattern-matching the raw AppClosure chain, and wrapping an
// AppClosure-headed Clo in a CDrop would break that recognition -- and the emitter is out
// of edit scope. Caching the spine sidesteps it.) Per run, only the scrutinee `someF
// (fn w is w end)` (a K_CON wrapping a K_CLO) allocates; the single application `matchF
// scrut` hands it to the cached final-block closure, which OWNS and drops it. The motive
// `optFMot` is likewise a cached def (the carve-out leaves the ignored motive argument
// borrowed; a cached root's rc inflates but $live does not). The payload is a CLOSURE,
// not a bignum literal: exactly as the Task 3 / 3.5 receivers avoid bignums, since
// rt_big_parse's intermediate K_BIG temps are a deferred 6b-2 leak that would mask the
// balance. The kept-field-is-a-bignum path is exercised for OUTPUT-invariance by
// perceusCaseLitSrc.
const perceusCaseSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
data OptF : U is
  noneF : OptF
| someF : (Nat -> Nat) -> OptF
end
optFMot : OptF -> U is fn (o : OptF) is Nat -> Nat end end
matchF : OptF -> (Nat -> Nat) is
  OptFElim optFMot (fn (z : Nat) is z end) (fn (x : Nat -> Nat) is x end)
end
mainOptF : Nat -> Nat is matchF (someF (fn (w : Nat) is w end)) end
`

// perceusCaseLitSrc keeps a projected BIGNUM field (`some 7` -> 7). It is an
// OUTPUT-invariance receiver only: rt_big_parse leaks K_BIG temps (the deferred 6b-2
// path), so its $live is not flat -- but the OUTPUT proves the kept field survives the
// scrutinee's recursive free. If the `some x` arm did not dup the field, dropping the
// scrutinee `some 7` would free the K_BIG(7) before it is returned -> a wrong value or a
// trap. Output 7 confirms CField dup-on-escape holds for a recursively-freed field too.
const perceusCaseLitSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
data Opt : U is
  none : Opt
| some : Nat -> Opt
end
optMot : Opt -> U is fn (o : Opt) is Nat end end
mainOpt : Nat is OptElim optMot zero (fn (x : Nat) is x end) (some 7) end
`

// TestPerceusConstructorCase is the Task 4 gate: build a datatype value, match it via
// the lowered eliminator, KEEP the projected field. Two assertions:
//
//   - assertSteadyFlat on the closure-payload receiver: OUTPUT "<function>" (the kept
//     closure) AND $live flat after run 1 (balanced: K_CON scrutinee dropped, kept
//     field dup'd out, curry-spine intermediates released). Under the pre-Task-4
//     carry-through this program leaked +6/run.
//   - OUTPUT-invariance on the bignum-payload receiver: `some 7` -> "7", proving the
//     kept field survives the scrutinee's recursive free (CField dup-on-escape).
func TestPerceusConstructorCase(t *testing.T) {
	// Balanced gate: scrutinee dropped, kept field dup'd, steady flat.
	assertSteadyFlat(t, perceusCaseSrc, "mainOptF", "<function>")

	// Kept BIGNUM field survives the scrutinee free (output-invariance only).
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusCaseLitSrc, "mainOpt"))
	if got != "7" {
		t.Fatalf("kept field use-after-free under scrutinee drop: got %q, want 7", got)
	}
}

// perceusWasmPairsSrc is the Task 5 receiver. Build a pair of closures (Fst=f, Snd=g),
// project Fst (the kept half), drop the pair (which also frees g). Each run:
//   - Allocates K_CLO_f, K_CLO_g, K_PAIR.
//   - consumeOwning on Fst dup's f (f.rc 1->2).
//   - drop-after releases the pair: f.rc 2->1, g freed, pair freed.
//   - Harness releases result (f): f freed.
//   - Net change to $live: 0 (steady flat).
//
// Uses closures (not bignums) as components to avoid rt_big_parse temp leaks in the
// steady gate. Uses separate `let p` and `let a` bindings so the pair is a let-bound
// owned local and the drop-after fires (not an anonymous inline Fst(Pair f g)).
const perceusWasmPairsSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
mainPairs : Nat -> Nat is
  let f : Nat -> Nat = fn (x : Nat) is x end in
  let g : Nat -> Nat = fn (y : Nat) is y end in
  let p : Sig (Nat -> Nat) (fn (z : Nat -> Nat) is Nat -> Nat end) = Pair (Nat -> Nat) (fn (z : Nat -> Nat) is Nat -> Nat end) f g in
  let a : Nat -> Nat = Fst p in
  a
end
`

// TestPerceusWasmPairs is the Task 5 receiver gate (pairs: CPair / CFst / CSnd).
//
// OUTPUT-INVARIANCE: Perceus must not alter the computed value. The def evaluates
// to f = fn (x is x) : Nat -> Nat, printed as "<function>".
//
// STEADY-STATE: $live must be flat after run 1. Each run allocates K_CLO_f,
// K_CLO_g, K_PAIR. The pair drop (after projecting Fst) frees K_PAIR and K_CLO_g;
// the harness releases K_CLO_f. Net: 0 change per run from run 2 onward.
func TestPerceusWasmPairs(t *testing.T) {
	assertSteadyFlat(t, perceusWasmPairsSrc, "mainPairs", "<function>")
}
