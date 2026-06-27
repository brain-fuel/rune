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

// wasmSteadyLivePInts is the numeric variant of wasmSteadyLiveP: it runs
// WasmSteadyModule with the given pre-built Program and returns the per-run
// $rt_live counts parsed to ints (one per line). Used by the 6b-2 delta tests,
// which measure the per-run leak increment counts[i]-counts[i-1] directly.
func wasmSteadyLivePInts(t *testing.T, p cg.Program, runs int) []int {
	t.Helper()
	strs := wasmSteadyLiveP(t, p, runs)
	out := make([]int, len(strs))
	for i, s := range strs {
		n, err := strconv.Atoi(s)
		if err != nil {
			t.Fatalf("non-integer live count %q at index %d (counts %v): %v", s, i, strs, err)
		}
		out[i] = n
	}
	return out
}

// assertSteadyFlatInts asserts a pre-built Program reaches TRUE steady-flat:
// across runs 2..runs the per-run $rt_live delta is exactly 0 (zero per-run
// leak). The numeric companion to assertSteadyFlat for receivers that build the
// Program directly (and that assert flat rather than just non-growing).
func assertSteadyFlatInts(t *testing.T, p cg.Program, runs int) {
	t.Helper()
	counts := wasmSteadyLivePInts(t, p, runs)
	if len(counts) != runs {
		t.Fatalf("want %d per-run live counts, got %d: %v", runs, len(counts), counts)
	}
	for i := 2; i < len(counts); i++ {
		if d := counts[i] - counts[i-1]; d != 0 {
			t.Fatalf("steady-state not flat: run %d per-run delta=%d (counts %v)", i+1, d, counts)
		}
	}
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

// perceusInlineCurrySrc is the Phase 6-pre receiver: an INLINE saturated curried
// application of an ORDINARY (non-accel, non-eliminator) two-argument user function.
// `const2 a b = a` is applied INLINE to two fresh identity closures. The inline
// application `const2 A B` closure-converts to
//
//	AppClosure(AppClosure(CGlobal const2, A), B)
//
// whose inner AppClosure `(const2 A)` is a CURRY INTERMEDIATE -- a fresh closure
// (capturing A) allocated every run. Before Phase 6-pre the pass released only
// CVar/MkClosure-headed closures (Task 3), so this AppClosure-HEADED intermediate
// was left bare and LEAKED (+2/run: the intermediate K_CLO plus the A it captures).
//
// Phase 6-pre's recognize-then-skip rule releases it: const2 is not a recognized accel
// or nat-elim spine, so its AppClosure-headed Clo is released after the call (the same
// CLet+CDrop pattern Task 3 uses for CVar/MkClosure Clos). Bignum-free (closures only)
// so no rt_big_parse temps mask the balance.
//
// Output: "<function>" (const2 A B = A, the first identity closure).
const perceusInlineCurrySrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
const2 : (Nat -> Nat) -> (Nat -> Nat) -> (Nat -> Nat) is
  fn (a : Nat -> Nat) is fn (b : Nat -> Nat) is a end end
end
mainCurry : Nat -> Nat is const2 (fn (x : Nat) is x end) (fn (y : Nat) is y end) end
`

// TestPerceusInlineCurry is the Phase 6-pre gate: an inline saturated curried
// application of an ordinary user function reaches STEADY-FLAT (its curry intermediate
// is released), with correct output. Before Phase 6-pre the intermediate leaked
// (+2/run); after, $rt_live is flat from run 2.
func TestPerceusInlineCurry(t *testing.T) {
	assertSteadyFlat(t, perceusInlineCurrySrc, "mainCurry", "<function>")
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

// perceusCtorMkXXSrc is the FIX B receiver: a 2-field constructor whose BOTH fields
// are the SAME owned local. Without FIX B, annotateBareSpine omits the shared-owned-
// local dup for constructor spines, so the K_CON double-owns x at rc=1 (two store=MOVE
// slots pointing to the same heap object). When the K_CON is released, both field
// releases fire: x.rc 1->0 (freed), then x.rc 0->-1 (use-after-free / trap). After
// FIX B, x is dup'd to rc=2 before the constructor spine, so the two field releases
// bring rc 2->1->0 correctly.
//
// The eliminator prElim is cached as a top-level partial application so its curry
// intermediates are built once (not per run), isolating the per-run allocation to:
//   - K_CLO_x (the shared owned field, fn y is y end)
//   - K_CLO_mk1 (the arity-2 constructor intermediate from the first rt_apply in mk x x,
//     kept BARE by the recognize-then-skip rule because releasing it would UAF the field)
//   - K_CON (the mk x x result)
//
// With FIX B: K_CON is released correctly (field[0] rc 2->1, field[1] rc 1->0, freed);
// K_CLO_mk1 leaks +1/run (the arity-2 ctor container leak, a known 6b-2 residual).
// Without FIX B: the double-free of x corrupts the WASM output or causes a trap.
const perceusCtorMkXXSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
data Pr : U is mk : (Nat -> Nat) -> (Nat -> Nat) -> Pr end
prMot : Pr -> U is fn (p : Pr) is Nat -> Nat end end
prCase : (Nat -> Nat) -> (Nat -> Nat) -> Nat -> Nat is
  fn (a : Nat -> Nat) is fn (b : Nat -> Nat) is a end end
end
prElim : Pr -> (Nat -> Nat) is PrElim prMot prCase end
mainMkXX : Nat -> Nat is
  let x : Nat -> Nat = fn (y : Nat) is y end in
  prElim (mk x x)
end
`

// TestPerceusCtorMkXX is the FIX B gate: shared-owned-local dup in constructor spines.
//
// OUTPUT-INVARIANCE: the primary assertion. Without FIX B, the K_CON double-owns x at
// rc=1, so releasing it double-frees x -> corrupted output or trap. After FIX B, x is
// dup'd to rc=2 before the mk spine, so both field releases are balanced. Output must be
// "<function>" (prElim projects field[0] = x, the identity closure).
//
// STEADY: the arity-2 constructor intermediate K_CLO_mk1 leaks +1/run (the recognize-
// then-skip rule keeps the ctor backbone BARE; emitCtorBlock does not release the
// intermediate). This is the known 6b-2 residual (arity>=2 ctor container leak). We
// assert the delta is EXACTLY +1/run -- proving x itself is balanced (FIX B working)
// and only K_CLO_mk1 leaks.
func TestPerceusCtorMkXX(t *testing.T) {
	// Output gate: double-free of x corrupts the value or traps; "<function>" proves FIX B.
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusCtorMkXXSrc, "mainMkXX"))
	if got != "<function>" {
		t.Fatalf("mk x x double-free: got %q, want \"<function>\" (FIX B shared-var dup missing?)", got)
	}
	// Steady: expect +1/run from K_CLO_mk1 (arity-2 ctor intermediate, 6b-2 boundary).
	// A delta > 1 would indicate x itself is leaking (FIX B not working or additional UAF).
	p := mustProgram(t, perceusCtorMkXXSrc, "mainMkXX")
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
	d32 := c3 - c2
	d43 := c4 - c3
	if d32 > 1 || d43 > 1 {
		t.Fatalf("mk x x steady: counts %v, deltas %d/%d exceed +1/run (expected only K_CLO_mk1 intermediate; FIX B broken?)", counts, d32, d43)
	}
	t.Logf("mk x x steady: %v (delta=%d/%d; +1/run from arity-2 K_CLO_mk1 intermediate, 6b-2 boundary; double-free fixed)", counts, d32, d43)
}

// perceusInlineElimSrc is the FIX A receiver: an INLINE saturated general-eliminator
// application (the 4-argument OptElim applied directly in the def body, not cached as a
// partial-application def). Before FIX A, OptElim was in bareSpineHead, so its 3
// curry-intermediate K_CLOs were kept BARE and leaked per run (+3/run). After FIX A,
// OptElim is removed from bareSpineHead; the intermediates are ordinary curry closures
// and are released after each application.
//
// The key question FIX A answers: does releasing the eliminator intermediates cause a
// use-after-free? The eliminator's curry blocks consumeOwning their CEnv prefix captures
// (dup forward), so each intermediate's env carries an independent reference. Releasing
// the intermediate frees the env copy; the next application still holds its own dup'd
// reference. Output "7" confirms no UAF.
//
// STEADY: the dead-motive carve-out leak (+1/run: the dup'd motive arg that the motive's
// curry-through block leaves borrowed) and the rt_big_parse K_BIG temporaries (the 7
// literal, deferred 6b-2 bignum path) mean inline-eliminator programs do not reach flat.
// We document the residual without asserting flat.
const perceusInlineElimSrc = `
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
mainInlineElim : Nat is OptElim optMot zero (fn (x : Nat) is x end) (some 7) end
`

// ---------------------------------------------------------------------------
// Task 6-gate: corpus output-invariance, corpus steady-state, frontier boundary,
// and the remaining minors (CSnd receiver, bignum-pair output-invariance).
// ---------------------------------------------------------------------------

// corpusListSrc is the wasm-conformance list-length program (mirrors wasm_test.go
// listSrc, defined separately because wasm_test.go's constant is function-local).
const corpusListSrc = natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`

// corpusPairSrc is the wasm-conformance multi-field constructor program (mirrors
// wasm_test.go pairSrc, defined separately for the same reason as corpusListSrc).
const corpusPairSrc = natSrc + `
data Pairing : U -> U -> U is
  mk : (A : U) -> (B : U) -> A -> B -> Pairing A B
end
p : Pairing Nat Nat is mk Nat Nat (succ zero) (succ (succ zero)) end
`

// wasmCorpusEntry is one entry in the WASM conformance corpus: a test name, the
// rune source, and the main definition to emit.
type wasmCorpusEntry struct {
	name string
	src  string
	main string
}

// wasmCorpusCases returns the same corpus used by TestWasmConformsToJS: the ten
// WASM-supported listings that the backend-conformance gate requires to be
// byte-identical to the JS reference.
func wasmCorpusCases() []wasmCorpusEntry {
	return []wasmCorpusEntry{
		{"three", natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`, "three"},
		{"two", corpusListSrc, "two"},
		{"p", corpusPairSrc, "p"},
		{"big", bigNatSrc, "big"},
		{"bigger", bigNatSrc, "bigger"},
		{"product", bigNatSrc, "product"},
		{"sum", accelNatSrc, "sum"},
		{"prod", accelNatSrc, "prod"},
		{"diff", accelNatSrc, "diff"},
		{"diffZero", accelNatSrc, "diffZero"},
	}
}

// TestPerceusCorpusOutputInvariance asserts the WASM Perceus output equals the JS
// reference for every WASM-supported corpus listing. dup/drop operations never
// change the computed value (they only adjust refcounts), so Perceus output must
// be byte-identical to non-Perceus output. This test NAMES that guarantee and
// asserts it across the full corpus.
//
// The Perceus pass is always-on in Wasm.Emit (TestWasmConformsToJS already passes),
// so this test is a thin documentation gate: it names the invariance explicitly and
// will catch any future regression in the pass that changes computed values.
func TestPerceusCorpusOutputInvariance(t *testing.T) {
	for _, c := range wasmCorpusCases() {
		c := c
		t.Run(c.name, func(t *testing.T) {
			jsOut := runNode(t, emitJS(t, c.src, c.main))
			wasmOut := runWasm(t, emitWith(t, cg.Wasm{}, c.src, c.main))
			if jsOut != wasmOut {
				t.Fatalf("%s: Perceus changed output: WASM=%q JS=%q", c.name, wasmOut, jsOut)
			}
		})
	}
}

// TestPerceusCorpusSteady asserts that every corpus listing in the v1 Perceus
// balanceable fragment (PerceusBalanceable = true) reaches a TRUE steady state:
// after the first run, $rt_live is flat (no per-run leak). Listings outside the
// fragment (PerceusBalanceable = false) are SKIPPED -- they are the 6b-2 frontier
// documented in R-PERCEUS.md. The test logs how many were balanced vs skipped.
//
// The four exclusion conditions are:
//  1. CBounce (partial trampoline)
//  2. Inline builtin-nat-fold (NatElimSpine)
//  3. Inline saturated general-eliminator (dead-motive carve-out + bignum temps)
//  4. Inline arity>=2 constructor (K_CLO_mk1 container leak)
func TestPerceusCorpusSteady(t *testing.T) {
	cases := wasmCorpusCases()
	nBalanced, nSkipped := 0, 0
	for _, c := range cases {
		p := mustProgram(t, c.src, c.main)
		if !cg.PerceusBalanceable(p) {
			nSkipped++
			continue
		}
		nBalanced++
		counts := wasmSteadyLiveP(t, p, 4)
		if len(counts) < 4 {
			t.Fatalf("%s: want 4 per-run live counts, got %d: %v", c.name, len(counts), counts)
		}
		c2, err2 := strconv.Atoi(counts[1])
		c3, err3 := strconv.Atoi(counts[2])
		c4, err4 := strconv.Atoi(counts[3])
		if err2 != nil || err3 != nil || err4 != nil {
			t.Fatalf("%s: non-integer live counts: %v", c.name, counts)
		}
		if !(c2 == c3 && c3 == c4) {
			t.Fatalf("%s leaks under Perceus: counts=%v", c.name, counts)
		}
	}
	t.Logf("corpus steady: %d balanced (steady flat), %d skipped (6b-2 frontier)", nBalanced, nSkipped)
}

// countdownSrc is the ch39-style partial-recursive countdown: a `partial` def that
// calls itself via CBounce (the T2 native trampoline). It is the canonical 6b-2
// frontier program: the WASM Perceus pass does not model ownership across CBounce
// tail calls (the driver-loop reuse pattern). PerceusBalanceable must return false
// for it; when 6b-2 lands and CBounce ownership is wired in, this test flips.
const countdownSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
partial countdown : Nat -> Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero
      (fn (k : Nat) (ih : Nat) is countdown k end)
      n
  end
end
countdownMain : Nat is countdown 3 end
`

// TestPerceusFrontierBoundary pins that a partial-recursive countdown program (which
// uses CBounce under closure conversion) is NOT in the v1 Perceus balanceable
// fragment. The assertion is executable: it catches any future accidental widening
// of PerceusBalanceable that would include trampoline programs without the 6b-2
// CBounce ownership model. When 6b-2 lands and the pass handles CBounce, this test
// flips (PerceusBalanceable returns true and the steady gate asserts flat).
func TestPerceusFrontierBoundary(t *testing.T) {
	p := mustProgram(t, countdownSrc, "countdownMain")
	if cg.PerceusBalanceable(p) {
		t.Fatalf("countdown uses CBounce (partial trampoline); it must be OUTSIDE the v1 balanceable fragment until 6b-2 lands")
	}
}

// perceusWasmSndSrc is the CSnd receiver (Task 5 minor, folded into Task 6). It
// builds a pair of closures (Fst=f, Snd=g), projects Snd (the kept second half),
// and drops the pair -- which also frees f (the first half, not dup'd). The
// ownership events mirror TestPerceusWasmPairs but exercise the CSnd arm:
//
//   - consumeOwning on the CSnd result inserts CDup (g.rc 1 -> 2).
//   - drop-after releases the pair: f.rc 1 -> 0 (freed), g.rc 2 -> 1.
//   - Harness releases result (g): g.rc 1 -> 0 (freed).
//   - Net per run: +3 allocs, -3 frees = 0. Steady flat.
//
// Uses closures (not bignums) as components to avoid rt_big_parse temp leaks.
const perceusWasmSndSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
mainSnd : Nat -> Nat is
  let f : Nat -> Nat = fn (x : Nat) is x end in
  let g : Nat -> Nat = fn (y : Nat) is y end in
  let p : Sig (Nat -> Nat) (fn (z : Nat -> Nat) is Nat -> Nat end) = Pair (Nat -> Nat) (fn (z : Nat -> Nat) is Nat -> Nat end) f g in
  let b : Nat -> Nat = Snd p in
  b
end
`

// TestPerceusWasmSnd is the CSnd receiver gate.
//
// OUTPUT-INVARIANCE: the def evaluates to g = fn (y is y) : Nat -> Nat, "<function>".
//
// STEADY-STATE: $live flat after run 1. Snd dup-on-escape keeps g alive past the
// pair drop (which frees f). Net: 0 change per run from run 2 onward. This proves
// the symmetric CSnd arm in the pass (pairProjSrc / drop-after) is correctly wired.
func TestPerceusWasmSnd(t *testing.T) {
	assertSteadyFlat(t, perceusWasmSndSrc, "mainSnd", "<function>")
}

// perceusWasmBignumPairSrc is the bignum-pair output-invariance witness. It builds
// a pair of Nat bignum literals (3, 4) and projects the first half (Fst). This
// exercises the CPair / CFst path with BIGNUM components: if the Fst component were
// not dup'd before the pair drop, dropping the pair would free the K_BIG(3) value
// before it is returned -- a use-after-free visible as wrong output or a trap.
//
// Output "3" confirms the dup-on-escape holds for bignum pair components.
//
// STEADY is NOT asserted: rt_big_parse bignum temporaries from the `3` and `4`
// literals leak per run (the 6b-2 boundary), so $rt_live grows. This is an
// output-invariance-only receiver, analogous to perceusCaseLitSrc.
const perceusWasmBignumPairSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
mainBignumPair : Nat is
  let p : Sig Nat (fn (z : Nat) is Nat end) = Pair Nat (fn (z : Nat) is Nat end) 3 4 in
  Fst p
end
`

// TestPerceusWasmBignumPair is the bignum-pair output-invariance gate. The pair
// holds Nat bignum literals; Fst must survive the pair drop with correct value "3".
// This is the first runtime proof that CPair's dup-on-escape holds for bignum
// components (the CLit case in pairProjSrc / drop-after).
func TestPerceusWasmBignumPair(t *testing.T) {
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusWasmBignumPairSrc, "mainBignumPair"))
	if got != "3" {
		t.Fatalf("bignum-pair Fst: got %q, want \"3\" (pair drop freed K_BIG(3) before return? CDup missing?)", got)
	}
}

// TestPerceusInlineElim is the FIX A gate: general eliminator intermediates are now
// released (not leaked like before).
//
// OUTPUT-INVARIANCE: the primary assertion. Before FIX A the intermediates leaked but
// output was still correct (UAF cannot occur for a spine-kept eliminator). After FIX A,
// releasing the intermediates must also be safe (no UAF from the release itself, because
// each intermediate's env captures are independently dup'd by the curry block). Output
// must be "7".
//
// STEADY: logged but not asserted flat. Residuals are the dead-motive carve-out leak
// (the dup'd optMot that the motive curry-through block leaves BORROWED) and the
// rt_big_parse bignum temporaries from the `7` literal -- both 6b-2 boundaries.
func TestPerceusInlineElim(t *testing.T) {
	// Primary gate: output correct = no UAF from releasing the eliminator intermediates.
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusInlineElimSrc, "mainInlineElim"))
	if got != "7" {
		t.Fatalf("inline-eliminator output: got %q, want \"7\" (UAF from releasing intermediates? FIX A broken?)", got)
	}
	// Steady: log residual. Inline-eliminator programs are excluded from perceusBalanceable
	// (6b-2 boundary: dead-motive + bignum temp leaks).
	p := mustProgram(t, perceusInlineElimSrc, "mainInlineElim")
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
	if c2 == c3 && c3 == c4 {
		t.Logf("inline-eliminator steady: %v (flat; FIX A eliminates per-run intermediate leaks)", counts)
	} else {
		t.Logf("inline-eliminator steady: %v (residual = dead-motive+bignum leak; 6b-2 boundary; FIX A released intermediates correctly)", counts)
	}
}

// ---------------------------------------------------------------------------
// Task 6-fix: bare-variable-rebind use-after-free regression receivers.
// ---------------------------------------------------------------------------

// perceusBareRebindSrc is the FIX 1 regression receiver: a bare-variable rebind
// `let h = g in h` where g is an owned argument. Before the fix, the CLet case has no
// arm for Val=CVar{k} with owned[k]=true, so g stays owned in bodyOwned and ownScope
// dead-drops it (premature release) while h (the same heap pointer) is returned and
// used by the harness -- use-after-free / corrupted refcount. After the fix,
// bodyOwned[k+1]=false prevents the dead-drop; h is the sole owner and is released by
// the harness normally, reaching steady-flat.
const perceusBareRebindSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
rebindFn : (Nat -> Nat) -> (Nat -> Nat) is
  fn (g : Nat -> Nat) is
    let h : Nat -> Nat = g in h
  end
end
mainRebind : Nat -> Nat is rebindFn (fn (x : Nat) is x end) end
`

// TestPerceusBareRebind is the FIX 1 gate (critical bug): bare-variable rebind
// `let h = g in h` where g is an owned argument.
//
// BEFORE FIX: g stays owned in bodyOwned; ownScope dead-drops g (rt_release, rc=1->0,
// freed); h is the same pointer and is returned dangling. The harness rt_release on the
// freed pointer corrupts the refcount / live count. Steady-state is NOT flat (live count
// goes below baseline or oscillates). Output may be accidentally correct (the freed K_CLO
// tag byte is unchanged before reuse) but the steady gate catches the imbalance.
//
// AFTER FIX: bodyOwned[k+1]=false; ownScope does NOT dead-drop g; h owns the value and
// is released by the harness once (rc=1->0). Steady-flat.
func TestPerceusBareRebind(t *testing.T) {
	assertSteadyFlat(t, perceusBareRebindSrc, "mainRebind", "<function>")
}

// perceusBareRebindBothSrc is the aliasing-correctness companion receiver. The body uses
// BOTH h and g: `let h = g in const2 h g`. The CLet shared-var dup loop (which already
// exists) inserts CDup{CVar{k}} when Val=CVar{k} and body uses k+1 -- so g.rc=2 before
// the let, covering both h (one consume) and g (one consume). After FIX 1
// (bodyOwned[k+1]=false), g is not owned in body scope and is not dead-dropped by
// ownScope; h owns the reference. The CDup ensures rc=2 so both h-use and g-use are safe
// and the program reaches steady-flat. This proves the shared-var dup correctly handles
// aliased uses under the fix.
const perceusBareRebindBothSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
const2 : (Nat -> Nat) -> (Nat -> Nat) -> (Nat -> Nat) is
  fn (a : Nat -> Nat) is fn (b : Nat -> Nat) is a end end
end
rebindBothFn : (Nat -> Nat) -> (Nat -> Nat) is
  fn (g : Nat -> Nat) is
    let h : Nat -> Nat = g in
    const2 h g
  end
end
mainRebindBoth : Nat -> Nat is rebindBothFn (fn (x : Nat) is x end) end
`

// TestPerceusBareRebindUsesBoth proves aliasing-dup correctness under FIX 1: when the
// body uses BOTH h and g after `let h = g`, the CLet shared-var dup provides rc=2 so
// both uses are balanced. With bodyOwned[k+1]=false (the fix), g is not dead-dropped by
// ownScope; the net refcount per run is zero. Steady-flat + correct output.
func TestPerceusBareRebindUsesBoth(t *testing.T) {
	assertSteadyFlat(t, perceusBareRebindBothSrc, "mainRebindBoth", "<function>")
}

// ---------------------------------------------------------------------------
// Task 6-fix-guard: deterministic WAT-structural regression guard for the
// bare-variable-rebind use-after-free, plus edge-case receivers.
// ---------------------------------------------------------------------------

// TestPerceusBareRebindWATGuard is the DETERMINISTIC regression guard for the
// bare-variable-rebind use-after-free fix (FIX 1 critical).
//
// WHY the $live STEADY GATE is BLIND to this UAF:
// TestPerceusBareRebind / TestPerceusBareRebindUsesBoth both pass pre-fix because
// a freshly-allocated closure that is prematurely freed via CDrop(g) BALANCES its
// own alloc -- the rc goes 1->0 (freed, $live--), and the harness release on the
// now-dangling h may be a no-op if the runtime guards rc<=0. So $live stays flat
// both pre-fix and post-fix; the steady gate cannot distinguish the UAF from the
// correct program.
//
// THE DETERMINISTIC GUARD -- WAT structural check:
// Pre-fix (bodyOwned[k+1] NOT cleared for Val=CVar{k}): g remains owned in
// bodyOwned; ownScope dead-drops it with CDrop{CVar{1}, ...}. The WASM emitter
// lowers CDrop to:
//
//	(call $rt_release (local.get $arg))   ;; releases g = h, UAF!
//	(local.get $t1)                        ;; returns h (dangling pointer)
//
// Post-fix (bodyOwned[cv.Idx+1]=false, perceus.go:~323): g is NOT owned in
// bodyOwned; ownScope does NOT insert CDrop; the release disappears entirely:
//
//	(local.set $t1 (local.get $arg))      ;; h = g (move)
//	(local.get $t1)                        ;; return h (rc=1, alive)
//
// ASSERTION: the emitted WAT must NOT contain "(call $rt_release (local.get $arg))".
//   Pre-fix:  FAILS -- the release is present (catches the regression).
//   Post-fix: PASSES -- the release is absent (validates the fix).
//
// This is the EXACT one-line diff the controller verified against 2f3c027:perceus.go.
func TestPerceusBareRebindWATGuard(t *testing.T) {
	wat := emitWith(t, cg.Wasm{}, perceusBareRebindSrc, "mainRebind")
	const bugLine = "(call $rt_release (local.get $arg))"
	if strings.Contains(wat, bugLine) {
		t.Fatalf(
			"use-after-free regression: the rebindFn code block releases $arg before returning it\n"+
				"  WAT contains: %s\n"+
				"  g and h alias the same heap pointer; dropping g while h is returned = UAF\n"+
				"  FIX: CLet Val=CVar{k} arm in annotate (perceus.go) must set bodyOwned[k+1]=false\n"+
				"  (the one-line fix removes exactly this release; see 2f3c027 emit diff)",
			bugLine,
		)
	}
}

// perceusBareRebindReturnSrcSrc is the edge-case receiver for `let h = g in g`
// (return the SOURCE g, not the binding h). h is dead in the body; g is returned.
//
// The CLet shared-var-dup loop (independent of the FIX 1 bodyOwned arm) inserts
// CDup{g} because g appears in BOTH Val (CVar{0}) and x.Body (CVar{1} after shift):
//   CDup{CVar{0}, CLet{"h", CVar{0}, CDrop{CVar{0}, CVar{1}}}}
// Sequence: retain g (rc 1->2); set h=g; CDrop h (rc 2->1); return g (rc=1).
//
// Crucially, the WAT for this program is IDENTICAL pre-fix and post-fix (the
// shared-var-dup fires in both cases; in the pre-fix case bodyOwned[1]=true but
// g is still not dead because cirUsesArg(CVar{1}, 1)=true). So this is a
// CORRECTNESS receiver, not a regression guard. Steady-flat.
const perceusBareRebindReturnSrcSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
rebindSrcFn : (Nat -> Nat) -> (Nat -> Nat) is
  fn (g : Nat -> Nat) is
    let h : Nat -> Nat = g in g
  end
end
mainRebindSrc : Nat -> Nat is rebindSrcFn (fn (x : Nat) is x end) end
`

// TestPerceusBareRebindReturnSrc verifies the `let h = g in g` edge case (return
// source, dead h). The shared-var dup ensures g.rc=2 before the let; CDrop h
// reduces rc to 1; g returned at rc=1; harness releases (rc=0). Net: 0/run.
// Output "<function>" (the identity closure g). Steady-flat.
func TestPerceusBareRebindReturnSrc(t *testing.T) {
	assertSteadyFlat(t, perceusBareRebindReturnSrcSrc, "mainRebindSrc", "<function>")
}

// perceusBareRebindDeadHSrc is the edge-case receiver for `let h = g in <body not
// using h or g>` (dead h, dead g). The rebindDeadFn captures g in h but then builds
// a fresh result closure -- neither h nor g appears in the MkClosure body.
//
// Post-fix (FIX 1): bodyOwned[1]=false (g not owned in body); only h is dead-dropped
//   (CDrop{CVar{0}, MkClosure{...}}); g is freed exactly once via h (same pointer).
//   WAT: (call $rt_release (local.get $t1)) -- releases $t1 (= h = g); no release of $arg.
//
// Pre-fix: bodyOwned=[true,true]; both h (index 0) AND g (index 1) are dead-dropped:
//   CDrop{CVar{0}, CDrop{CVar{1}, MkClosure{...}}} in the body context where
//   CVar{1} = $arg -> WAT contains "(call $rt_release (local.get $arg))" (double-free!).
//   The same WAT guard string TestPerceusBareRebindWATGuard checks covers this shape;
//   this program has its own structural check below to pin the dead-h invariant.
const perceusBareRebindDeadHSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
rebindDeadFn : (Nat -> Nat) -> (Nat -> Nat) is
  fn (g : Nat -> Nat) is
    let h : Nat -> Nat = g in
    fn (z : Nat) is z end
  end
end
mainRebindDead : Nat -> Nat is rebindDeadFn (fn (x : Nat) is x end) end
`

// TestPerceusBareRebindDeadH verifies the `let h = g in <body not using h or g>`
// edge case. Post-fix: only h is dead-dropped (CDrop h frees g via the aliased
// pointer); the body's fresh MkClosure is returned. Output "<function>". Steady-flat.
//
// WAT structural check (the deterministic guard): must NOT contain
// "(call $rt_release (local.get $arg))". Pre-fix: CDrop g appears as a second CDrop
// on the same pointer -> that string IS present (double-free). Post-fix: absent.
// This exercises the same FIX 1 bodyOwned arm as TestPerceusBareRebindWATGuard but
// for the case where the body is a MkClosure (not CVar{0}), pinning the dead-h shape.
func TestPerceusBareRebindDeadH(t *testing.T) {
	// WAT structural guard: double-free of g must not appear.
	wat := emitWith(t, cg.Wasm{}, perceusBareRebindDeadHSrc, "mainRebindDead")
	const bugLine = "(call $rt_release (local.get $arg))"
	if strings.Contains(wat, bugLine) {
		t.Fatalf(
			"double-free regression (dead-h shape): rebindDeadFn releases $arg = g twice\n"+
				"  WAT contains: %s\n"+
				"  h=g aliased; CDrop h frees g (rc=1->0); CDrop g then double-frees\n"+
				"  FIX: bodyOwned[k+1]=false in perceus.go CLet Val=CVar{k} arm",
			bugLine,
		)
	}
	// Correctness + steady-flat: each run allocs two closures (arg + result);
	// CDrop h frees arg; harness releases result. Net: 0/run. Flat after run 1.
	assertSteadyFlat(t, perceusBareRebindDeadHSrc, "mainRebindDead", "<function>")
}

// perceusNatFoldSrc: a NatElim fold (doubling) over the literal 3. Three iterations,
// step = succ(succ(ih)). Correct output: 6. Used by TestPerceusNatFoldOwnership.
const perceusNatFoldSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
double : Nat -> Nat is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is succ (succ ih) end) n
  end
end
mainFold : Nat is double 3 end
`

const natFoldWant = "6"

// natFoldMaxResidual: upper bound on the per-run steady $live delta after Task 1.
// Post-fix measured delta for n=3 (succ succ ih step): 27.
// Pre-fix measured delta for n=3: 31.
// Any value in (27, 31) gives teeth: passes after fix, fails before.
// Set to 30 for a round bound with 3 units of headroom above the measured 27.
const natFoldMaxResidual = 30

// TestPerceusNatFoldOwnership: a NatElim fold over a small nat. BEFORE Task 1: each of
// the N iterations leaks the prior counter + step closure + acc, so the steady per-run
// delta grows with N. AFTER Task 1: the fold's per-iteration temps (counter/step) are
// released, reducing the per-run delta from 31 to 27 (for n=3). We assert the delta
// shrinks to natFoldMaxResidual. Output must be unchanged.
//
// Residual after Task 1: K_CONST intermediate from rt_big_succ (1 per iteration, frozen
// runtime) + CGlobal AppClosure limitation in the step body (Task 4 scope). The
// retain/release pairing for the counter awaits Task 2 (carve-out removal). The fix
// eliminates the step-closure leak and final-counter leak.
func TestPerceusNatFoldOwnership(t *testing.T) {
	src := perceusNatFoldSrc
	got := runWasm(t, emitWith(t, cg.Wasm{}, src, "mainFold"))
	if got != natFoldWant {
		t.Fatalf("output changed under Task 1: got %q want %q", got, natFoldWant)
	}
	p := mustProgram(t, src, "mainFold")
	counts := wasmSteadyLivePInts(t, p, 4)
	d := counts[3] - counts[2]
	if d > natFoldMaxResidual {
		t.Fatalf("nat-fold still leaks per-iteration: per-run delta=%d > %d", d, natFoldMaxResidual)
	}
}
