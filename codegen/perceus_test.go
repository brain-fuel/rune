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

// steadySrc is a minimal rune source with builtin-nat bignums. Each `succ`
// application invokes succ_code, which does rt_big_succ($arg) (a fresh K_BIG)
// and -- after Plan 6b-2 Task 4d -- RELEASES its owned $arg. So in a succ-chain
// every intermediate K_BIG is freed when the next succ consumes it, and the
// chain runs steady-flat.
const steadySrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
three : Nat is succ (succ (succ zero)) end
`

// TestPerceusSuccChainFlat: a builtin-nat succ-chain reaches steady-flat. Before
// Plan 6b-2 Task 4d this test asserted the OPPOSITE (succ-chains LEAK, counts
// GROW) because succ_code borrow-read its arg and never freed it, so each
// intermediate K_BIG in `succ (succ (succ zero))` leaked every run. Task 4d makes
// succ_code free its owned arg (the caller hands it an owned reference under PATH
// B), so each intermediate is released when the enclosing succ consumes it and
// $rt_live is flat after run 1. The receiver is INVERTED accordingly (the
// structural leak it documented is now closed).
func TestPerceusSuccChainFlat(t *testing.T) {
	p := mustProgram(t, steadySrc, "three")
	counts := wasmSteadyLivePInts(t, p, 4)
	if len(counts) != 4 {
		t.Fatalf("want 4 per-run live counts, got %d: %v", len(counts), counts)
	}
	// succ-chain intermediates are now freed each run -> counts FLAT after run 1.
	if !(counts[1] == counts[2] && counts[2] == counts[3]) {
		t.Fatalf("succ-chain should be steady-flat after Task 4d, got growing/uneven %v", counts)
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
	// Steady: the FIX-B guard keeps x balanced (delta <= +1/run). Before Task 3 the
	// residual was exactly +1/run (the arity-2 ctor intermediate K_CLO_mk1, kept BARE by
	// the recognize-then-skip rule). Task 3's satCtorDispatch builds `mk x x`'s K_CON
	// directly (no intermediate K_CLO), so the residual is now 0 -- but the gate stays an
	// UPPER bound (a delta > 1 would mean x itself leaks, i.e. FIX B regressed).
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
		t.Fatalf("mk x x steady: counts %v, deltas %d/%d exceed +1/run (FIX B shared-var dup broken?)", counts, d32, d43)
	}
	t.Logf("mk x x steady: %v (delta=%d/%d; FIX B balances x, Task 3 removed the K_CLO_mk1 intermediate -> flat)", counts, d32, d43)
}

// perceusSatCtorSrc is the Task 3 receiver: an INLINE SATURATED arity-2 constructor
// application `mk a b` (both args owned locals). Mirrors perceusCtorMkXXSrc but with two
// DISTINCT owned fields so the measurement is not confounded by FIX B's shared-var dup.
//
// BEFORE Task 3: `mk a b` lowers through emitCtorBlock's curry steps, so the first
// rt_apply builds an intermediate partial-application K_CLO (`mk a`). The recognize-then-
// skip rule keeps the ctor backbone BARE (releasing the intermediate would double-free the
// moved field a), so that K_CLO_mk1 container LEAKS +1/run.
//
// AFTER Task 3: the WASM emitter recognizes the saturated ctor spine (satCtorDispatch) and
// builds the K_CON DIRECTLY via rt_mkcon + rt_con_set per arg -- no intermediate K_CLO is
// ever allocated, so the +1/run container leak is gone. Output is unchanged ("<function>":
// prElim projects field[0] = a, the identity closure).
const perceusSatCtorSrc = `
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
mainCtor : Nat -> Nat is
  let a : Nat -> Nat = fn (y : Nat) is y end in
  let b : Nat -> Nat = fn (y : Nat) is y end in
  prElim (mk a b)
end
`

// satCtorWant is the output of perceusSatCtorSrc's mainCtor (a Nat -> Nat value: a
// function, shown as "<function>"). Byte-identical before and after Task 3 -- the K_CON's
// printed form (tag/fields/name) is unchanged; only the intermediate K_CLO disappears.
const satCtorWant = "<function>"

// satCtorResidualAfter is the per-run $rt_live delta for perceusSatCtorSrc AFTER Task 3.
// The K_CLO_mk1 container (+1/run) is removed by the direct rt_mkcon emit; the program
// reaches TRUE flat (delta 0). Measured post-fix: the two field closures a/b are moved
// into the K_CON and released when it is dropped, the K_CON is consumed by prElim, and no
// intermediate survives -- nothing leaks.
const satCtorResidualAfter = 0

// TestPerceusSaturatedCtorNoContainer: an inline saturated arity-2 ctor `mk a b`.
// BEFORE Task 3 the partial `mk a` K_CLO is allocated and leaked (+1/run, the K_CLO_mk1
// container). AFTER Task 3 the saturated application emits rt_mkcon directly, no
// intermediate K_CLO, so the +1/run container leak is gone. Asserts the per-run delta
// drops to <= satCtorResidualAfter (TRUE flat) and the output is unchanged.
func TestPerceusSaturatedCtorNoContainer(t *testing.T) {
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusSatCtorSrc, "mainCtor"))
	if got != satCtorWant {
		t.Fatalf("output changed: got %q want %q", got, satCtorWant)
	}
	p := mustProgram(t, perceusSatCtorSrc, "mainCtor")
	counts := wasmSteadyLivePInts(t, p, 4)
	d := counts[3] - counts[2]
	if d > satCtorResidualAfter { // residual without the K_CLO_mk1 container
		t.Fatalf("saturated ctor still leaks the container: delta=%d > %d (counts %v)", d, satCtorResidualAfter, counts)
	}
	t.Logf("saturated ctor steady: counts %v, per-run delta=%d (K_CLO_mk1 container removed)", counts, d)
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

// TestPerceusCorpusSteady asserts that every corpus listing in the Perceus FLAT
// fragment (PerceusBalanceable = true) reaches a TRUE steady state: after the first
// run, $rt_live is flat (no per-run leak). Listings outside the fragment
// (PerceusBalanceable = false) are SKIPPED -- they are the remaining 6b-2 frontier
// documented in R-PERCEUS.md. The test logs how many were balanced vs skipped.
//
// After the Plan 6b-2 re-opening (Task 5), the balanceable subset is NON-EMPTY: the
// recursive-datatype eliminators `three` (NatElim fold) and `two` (ListElim fold) and
// the arity-4 constructor `p` all reach flat. `product` (mulN 100 100) stays excluded
// (the builtin-nat fold-SETUP residual, +507/run); `big`/`bigger`/`sum`/`prod`/`diff`/
// `diffZero` are flat but conservatively excluded because their source modules carry
// dead addN/mulN defs whose NatElimSpine trips condition 2 (a one-sided miss, never a
// soundness break: PerceusBalanceable is never true for a non-flat program).
//
// The two REMAINING exclusion conditions are:
//  1. CBounce (partial trampoline) -- unsupported on WASM.
//  2. Inline builtin-nat-fold (NatElimSpine) -- the fold-SETUP residual.
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
			t.Fatalf("%s leaks under Perceus but PerceusBalanceable returned true "+
				"(predicate too loose -- narrow it): counts=%v", c.name, counts)
		}
	}
	if nBalanced == 0 {
		t.Fatalf("6b-2 should make real corpus listings balanceable; got 0")
	}
	t.Logf("corpus steady: %d balanced (steady flat), %d skipped (6b-2 frontier)", nBalanced, nSkipped)
}

// perceusRealisticSrc is the Plan 6b-2 Task 5 capstone receiver: a realistic INLINE
// program that combines, in its single per-run main path, every closed member of the
// flat fragment:
//
//	(a) a MULTI-DIGIT nat literal (`42`, the rt_big_parse path Task 4 made flat),
//	(b) a SATURATED arity>=2 constructor build (`mk 42 (succ 7)`, the K_CON Task 3
//	    emits directly via rt_mkcon -- no intermediate container),
//	(c) an inline NON-RECURSIVE user-eliminator match of it (`BoxElim` over the
//	    non-recursive record `Box`; the dead motive is dropped by Task 2, the K_CON
//	    scrutinee dropped after the arm, the kept field dup'd out),
//	(d) succ-chain arithmetic (`succ 7` building the discarded field, `succ (succ a)`
//	    on the kept field; succ_code frees its owned operand per Task 4d).
//
// It deliberately does NOT use a builtin NatElim FOLD (`NatElim mot z step n`): that is
// the deferred fold-SETUP residual (`product` leaks +507/run). Accel add/mul are also
// avoided -- not because they leak in isolation (the corpus `sum`/`prod`/`diff` are flat)
// but because binding them (`builtin natAdd addN`) pulls addN's NatElim fold def into the
// module, which both trips condition 2 AND, when an accel result is consumed inside an
// eliminator arm, leaks +1/run (empirically measured). The succ-chain form keeps the
// program inside the truly-flat, PerceusBalanceable fragment.
//
// The eliminator arm projects field a = 42 and computes succ (succ 42) = 44; the second
// field (succ 7 = 8) is discarded and freed when the scrutinee K_CON is dropped.
const perceusRealisticSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
data Box : U is mk : Nat -> Nat -> Box end
mainReal : Nat is
  BoxElim (fn (b : Box) is Nat end)
    (fn (a : Nat) (rest : Nat) is succ (succ a) end)
    (mk 42 (succ 7))
end
`

// TestPerceusRealisticFlat is the Task 5 capstone gate: a realistic inline program over
// the flat fragment reaches TRUE steady-flat (zero per-run $rt_live delta across runs
// 2..5) with a deterministic output. It is the integration proof that the 6b-2 residual
// closures (dead-motive Task 2, ctor-container Task 3, parse-temps Task 4, big_succ-temp
// Task 4b, accel/succ-operand Task 4d) compose: literal + saturated ctor + inline
// non-recursive eliminator + succ arithmetic together leak nothing.
func TestPerceusRealisticFlat(t *testing.T) {
	// Output-invariance: BoxElim selects field a = 42, computes succ (succ 42) = 44.
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusRealisticSrc, "mainReal"))
	if got != "44" {
		t.Fatalf("realistic flat-fragment output: got %q, want \"44\"", got)
	}
	// The program must lie in the balanceable fragment (it uses no builtin-nat fold,
	// no CBounce) -- a guard that the predicate actually admits this realistic shape.
	p := mustProgram(t, perceusRealisticSrc, "mainReal")
	if !cg.PerceusBalanceable(p) {
		t.Fatalf("realistic program should be PerceusBalanceable (literal + ctor + inline "+
			"non-recursive eliminator + succ arithmetic, no builtin-nat fold)")
	}
	// Steady-flat: zero per-run increment across runs 2..5.
	assertSteadyFlatInts(t, p, 5)
}

// perceusNestedElimSrc nests one INLINE non-recursive user-eliminator inside
// another's arm: an OptElim whose some-arm is itself an inline BoxElim over a
// freshly-built `mk n`. Task 5 removed the general-eliminator exclusion
// (condition 3) entirely; this is the shape the capstone review flagged as
// admitted-but-unmeasured (a user eliminator applied inside another eliminator's
// arm). It pins the predicate is SOUND for that shape: balanceable AND truly flat.
const perceusNestedElimSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
data Box : U is
  mk : Nat -> Box
end
data Opt : U is
  none : Opt
| some : Nat -> Opt
end
f : Opt -> Nat is
  fn (o : Opt) is
    OptElim (fn (x : Opt) is Nat end) zero
      (fn (n : Nat) is BoxElim (fn (y : Box) is Nat end) (fn (m : Nat) is m end) (mk n) end)
      o
  end
end
mainNest : Nat is f (some (succ (succ zero))) end
`

// TestPerceusNestedElimInArmFlat closes the capstone review's Important finding:
// removing the general-eliminator exclusion admits a nested inline
// eliminator-in-arm, a shape the corpus does not otherwise cover. Measured here:
// it is PerceusBalanceable AND steady-flat (output 2 = succ (succ zero)), so the
// removal of condition 3 is sound for this shape, not merely unproven.
func TestPerceusNestedElimInArmFlat(t *testing.T) {
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusNestedElimSrc, "mainNest"))
	if got != "2" {
		t.Fatalf("nested eliminator-in-arm output: got %q, want \"2\"", got)
	}
	p := mustProgram(t, perceusNestedElimSrc, "mainNest")
	if !cg.PerceusBalanceable(p) {
		t.Fatalf("nested inline eliminator-in-arm should be PerceusBalanceable")
	}
	assertSteadyFlatInts(t, p, 5)
}

// perceusSharedConsumeSrc exercises a SHARED owned local CONSUMED in two operand
// sub-spines of a recognized accel / nat-elim spine -- the exact shape the final
// whole-branch review caught as a double-free (C1). `addN (succ n) (succ n)`:
// each `succ n` operand is freshOwned (accelDispatch releases its result) AND
// succ_code frees its arg n, so n (rc=1) is freed TWICE without a dup -> UAF /
// double-free. The same for mulN, and for an INLINE NatElim whose base and bound
// both consume n. After the fix (annotateBareSpine inserts the shared-owned-local
// dup for accel/nat spines too, not only constructors), n is dup'd once so each
// consumption frees a live reference. Pre-fix these produce WRONG output (the
// runtime reads freed memory); post-fix they match the reference value.
const perceusSharedConsumeSrc = accelNatSrc + `
dblAdd : Nat -> Nat is fn (q : Nat) is addN (succ q) (succ q) end end
dblMul : Nat -> Nat is fn (q : Nat) is mulN (succ q) (succ q) end end
elimShared : Nat -> Nat is
  fn (q : Nat) is
    NatElim (fn (x : Nat) is Nat end) q (fn (k : Nat) (ih : Nat) is succ ih end) q
  end
end
mainDblAdd : Nat is dblAdd 3 end
mainDblMul : Nat is dblMul 3 end
mainElimShared : Nat is elimShared 4 end
`

// TestPerceusSharedConsumeNoDoubleFree is the regression guard for the final-review
// C1 double-free: a shared owned local consumed in two operand positions of a
// recognized accel / nat-elim spine. Output-invariance (WASM == the reference
// value) is the deterministic detector -- a double-free reads freed memory and
// diverges. addN (succ 3) (succ 3) = 8; mulN (succ 3) (succ 3) = 16; the inline
// NatElim folds q+q = 8 (base q=4, bound q=4, succ applied 4 times).
func TestPerceusSharedConsumeNoDoubleFree(t *testing.T) {
	cases := []struct {
		main, want string
	}{
		{"mainDblAdd", "8"},
		{"mainDblMul", "16"},
		{"mainElimShared", "8"},
	}
	for _, c := range cases {
		got := runWasm(t, emitWith(t, cg.Wasm{}, perceusSharedConsumeSrc, c.main))
		if got != c.want {
			t.Fatalf("%s: shared-local double-free regression: WASM got %q, want %q",
				c.main, got, c.want)
		}
	}
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
// userClosureWAT returns the concatenated WAT of the emitted USER-closure code
// blocks (the anonymous $code__codeN functions), excluding the builtin-nat
// runtime thunks (succ_code, the eliminator fold/curry blocks). The bare-rebind
// guards search for "(call $rt_release (local.get $arg))" -- a bug signature in
// the rebind closure's code block -- but as of Plan 6b-2 Task 4d succ_code
// legitimately uses $arg as its param and releases it, which would false-positive
// a whole-module string search. The rebind closure is always a user closure
// ($code__codeN), so scoping the search to those functions keeps the guard precise.
func userClosureWAT(wat string) string {
	var out strings.Builder
	for _, ch := range strings.Split(wat, "(func $") {
		if strings.HasPrefix(ch, "code__code") {
			out.WriteString("(func $")
			out.WriteString(ch)
		}
	}
	return out.String()
}

func TestPerceusBareRebindWATGuard(t *testing.T) {
	wat := userClosureWAT(emitWith(t, cg.Wasm{}, perceusBareRebindSrc, "mainRebind"))
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
	// WAT structural guard: double-free of g must not appear (scoped to user
	// closures so succ_code's legitimate $arg release does not false-positive).
	wat := userClosureWAT(emitWith(t, cg.Wasm{}, perceusBareRebindDeadHSrc, "mainRebindDead"))
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

// natFoldSrc builds a NatElim doubling fold over a literal of magnitude n
// (step = succ(succ(ih))). `double 3` outputs 6. Used by
// TestPerceusNatFoldOwnership to measure the per-iteration steady-leak slope at
// two fold sizes.
func natFoldSrc(n int) string {
	return `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
double : Nat -> Nat is
  fn (m : Nat) is
    NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is succ (succ ih) end) m
  end
end
mainFold : Nat is double ` + strconv.Itoa(n) + ` end
`
}

const natFoldWant = "6"

// natFoldDelta returns the per-run steady $live increment for `double n` (the
// per-iteration leak the fold contributes; counts[3]-counts[2] after warmup).
func natFoldDelta(t *testing.T, n int) int {
	t.Helper()
	p := mustProgram(t, natFoldSrc(n), "mainFold")
	c := wasmSteadyLivePInts(t, p, 4)
	return c[3] - c[2]
}

// natFoldPerIterMax: the post-Task-1 per-iteration steady-leak SLOPE is 6. The
// fold body still leaks, per iteration, the frozen rt_big_succ's K_BIG(1) (out
// of 6b-2 scope), the not-yet-dropped old counter (awaits the Task 2 carve-out
// removal), and the step-body intermediates. Pre-fix the slope was 7: Task 1's
// rt_retain(k) + rt_release(step) + final-counter release removed exactly the
// per-iteration STEP-CLOSURE unit, dropping the slope 7 -> 6. The test measures
// the slope between two fold sizes and asserts it is at most natFoldPerIterMax,
// which FAILS on the pre-fix emitNatFold (slope 7) and passes after. Full
// iteration-independence is a later property (Task 2 drops the counter; the
// frozen rt_big_succ residual is out of scope), so this task asserts its own
// contribution (the slope drop), not absolute flatness.
const natFoldPerIterMax = 6

// TestPerceusNatFoldOwnership pins Task 1: the emitNatFold ownership rewrite
// (a) preserves output (output-invariance) and (b) drops the per-iteration
// steady-leak slope from 7 to 6 by releasing the step closure each iteration.
// Teeth: the slope assertion FAILS on the pre-fix loop (slope 7) and passes
// after. Measured here: delta(n) = 6n + 5, slope 6.
func TestPerceusNatFoldOwnership(t *testing.T) {
	// (a) output-invariance: the fold result is unchanged by the ownership rewrite.
	got := runWasm(t, emitWith(t, cg.Wasm{}, natFoldSrc(3), "mainFold"))
	if got != natFoldWant {
		t.Fatalf("output changed under Task 1: got %q want %q", got, natFoldWant)
	}
	// (b) per-iteration slope: measure the per-run leak at two fold sizes and
	// take the slope in n. Pre-fix it is 7 (counter+step+acc all leak per
	// iteration); Task 1 releases the step closure, dropping it to 6.
	const n1, n2 = 3, 12
	d1 := natFoldDelta(t, n1)
	d2 := natFoldDelta(t, n2)
	slope := (d2 - d1) / (n2 - n1)
	if slope > natFoldPerIterMax {
		t.Fatalf("nat-fold per-iteration leak slope=%d > %d (delta(%d)=%d, delta(%d)=%d): "+
			"the step-closure leak is not removed", slope, natFoldPerIterMax, n1, d1, n2, d2)
	}
}

// natFoldSuccTempMax: after Task 4b (rt_big_succ releases its internal
// rt_big_from_long(1) temp), the per-iteration fold slope drops from 5 (post
// Task 2) to 2. The fold makes 3 rt_big_succ calls per iteration (the counter
// step plus the two succ in the `succ (succ ih)` body); each previously leaked
// one K_BIG(1), so releasing that temp removes 3 per iteration (5 -> 2). The
// remaining 2 are the CGlobal-succ intermediate RESULTS (the inner succ result
// consumed by the outer succ), which Task 4d closes structurally.
const natFoldSuccTempMax = 2

// TestPerceusBigSuccTempReleased pins Task 4b: rt_big_succ no longer leaks its
// internal "1" temporary, so the fold's per-iteration slope drops to at most 2.
// Teeth: FAILS on the pre-4b runtime (slope 5) and passes after. The residual 2
// is the structural CGlobal-succ intermediate, closed by Task 4d.
func TestPerceusBigSuccTempReleased(t *testing.T) {
	const n1, n2 = 3, 12
	d1 := natFoldDelta(t, n1)
	d2 := natFoldDelta(t, n2)
	slope := (d2 - d1) / (n2 - n1)
	if slope > natFoldSuccTempMax {
		t.Fatalf("rt_big_succ still leaks its temp: fold slope=%d > %d (delta(%d)=%d, delta(%d)=%d)",
			slope, natFoldSuccTempMax, n1, d1, n2, d2)
	}
}

// TestPerceusNatFoldFlat pins the satElimDispatch payoff: a saturated builtin-nat
// eliminator fold now reaches TRUE steady-flat (zero per-run leak), independent of
// the iteration count. satElimDispatch runs the fold via a single b3 closure (env
// {unit, base, step}) that it releases after the call, instead of currying through
// the cached eliminator thunk's b0->b1->b2 chain (three leaked partial K_CLOs +
// the erased motive); emitNatFold retains its base so it survives that release.
// Before satElim the fold leaked a constant per-run fold-SETUP cost (~6/run); this
// asserts the per-run delta is now exactly 0 at two different fold sizes.
func TestPerceusNatFoldFlat(t *testing.T) {
	for _, n := range []int{3, 12, 30} {
		p := mustProgram(t, natFoldSrc(n), "mainFold")
		c := wasmSteadyLivePInts(t, p, 5)
		for i := 2; i < len(c); i++ {
			if c[i] != c[i-1] {
				t.Fatalf("double %d: NatElim fold not steady-flat: counts=%v (run %d delta=%d)",
					n, c, i+1, c[i]-c[i-1])
			}
		}
	}
	// output-invariance (the satElim lowering must compute the same value as the
	// curried path): double doubles the input.
	for _, c := range []struct {
		n    int
		want string
	}{{3, "6"}, {12, "24"}} {
		if got := runWasm(t, emitWith(t, cg.Wasm{}, natFoldSrc(c.n), "mainFold")); got != c.want {
			t.Fatalf("double %d: satElim output %q, want %q", c.n, got, c.want)
		}
	}
}

// ---------------------------------------------------------------------------
// Plan 6b-2 Task 4: rt_big_parse releases its per-digit K_BIG temporaries.
// ---------------------------------------------------------------------------

// bignumLitSrc is the 6b-2 Task 4 receiver source: a program whose main body
// is a bare MULTI-digit bignum literal "137". Each run of WasmSteadyModule
// evaluates the literal by calling $rt_big_parse on the interned cstr "137",
// then releases the result. The literal is multi-digit on purpose: with a
// single digit, rt_nat_mul short-circuits on its zero-limb accumulator and the
// r[i+j] += a[i]*b[j] path that requires zero-initialized limbs never runs, so
// the big_alloc zeroing half of the fix would go unexercised. With "137", run 2
// onward draws freed (and therefore zeroed) K_BIG blocks from the free list and
// runs the real multiply, so the test covers BOTH halves of the fix.
//
// Without rt_big_parse releasing its per-digit temporaries, $rt_live grows each
// run (the initial zero K_BIG, the "ten" K_BIG, and the per-digit intermediate
// K_BIGs from rt_nat_mul and rt_big_from_long all leak). After the fix, all
// temporaries are released inside the loop and "ten" is released after the loop,
// so $rt_live is flat from run 2.
const bignumLitSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
main : Nat is 137 end
`

// TestPerceusBignumParseTemps is the Plan 6b-2 Task 4 gate. It asserts BOTH:
//
//   - OUTPUT-INVARIANCE: the literal still prints "137". This is the guard for
//     the big_alloc zeroing subfix; without zeroing, free-list reuse feeds dirty
//     limbs into rt_nat_mul and a multi-digit literal misprints (e.g. "137" came
//     out wrong) while $rt_live could still read flat.
//   - STEADY-FLAT: $rt_big_parse releases its per-digit temporaries, so $rt_live
//     reaches true steady-flat after run 1.
//
// Per call, $rt_big_parse allocates (per digit) $old (previous acc), $mid
// (rt_nat_mul result), $dig (rt_big_from_long), plus the one-shot $ten and the
// returned accumulator. With the fix, $old/$mid/$dig are released inside the
// loop after big_add, $ten after the loop, and the result by the harness, so the
// per-run net is zero. Without it, those temporaries leak each run.
func TestPerceusBignumParseTemps(t *testing.T) {
	// output-invariance: the multi-digit literal must still print 137 (the
	// big_alloc zeroing subfix; dirty free-list limbs would misprint it).
	got := runWasm(t, emitWith(t, cg.Wasm{}, bignumLitSrc, "main"))
	if got != "137" {
		t.Fatalf("output changed under Task 4: got %q want \"137\"", got)
	}
	// steady-flat: per-digit temporaries are released, zero per-run increment.
	p := mustProgram(t, bignumLitSrc, "main")
	assertSteadyFlatInts(t, p, 4)
}

// ---------------------------------------------------------------------------
// Plan 6b-2 Task 2: remove the curry-through carve-out (drop the dead motive).
// ---------------------------------------------------------------------------

// perceusDeadMotiveSrc is the Task 2 receiver source: an INLINE general
// eliminator application where the MOTIVE is an inline lambda (not a cached
// named def). The motive `fn (o : Opt) is Nat end` erases to
// `fn _ is () end` -- a K_CLO freshly allocated each run. Because the motive
// (IVar at index 3 in x's frame) does NOT appear in the ICase body for the
// non-recursive Opt datatype, the eliminator's first curry block (b0) has an
// EMPTY env: the motive CVar{0} is dead in b0's body.
//
// With the curry-through carve-out: b0 is a bare-MkClosure block, so
// Perceus annotates it without dropping its dead argument. The fresh motive
// K_CLO is allocated each run and NEVER freed; $rt_live grows by +1/run.
//
// After removing the carve-out: ownScope dead-drops CVar{0} in b0
// (CDrop{CVar{0}, MkClosure{...}}). The motive K_CLO is freed by b0 each
// run. $rt_live is flat (per-run delta = 0). Output unchanged: "7".
const perceusDeadMotiveSrc = `
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ
data Opt : U is
  none : Opt
| some : Nat -> Opt
end
mainElim : Nat is OptElim (fn (o : Opt) is Nat end) zero (fn (x : Nat) is x end) (some 7) end
`

// inlineElimResidualAfterMotive is the per-run $rt_live delta for
// perceusDeadMotiveSrc after the carve-out is REMOVED (the motive is dropped
// in b0). After Task 2, the only remaining contributions are eliminated by
// Tasks 1 and 4 already: the motive K_CLO is dropped, bignum temps are freed
// by rt_big_parse (Task 4), and the K_CON scrutinee + curry intermediates are
// released by the Perceus pass. The residual is 0 (fully flat). The
// pre-removal baseline is 1 (motive K_CLO not freed); removing the carve-out
// drops the delta by exactly 1.
const inlineElimResidualAfterMotive = 0

// TestPerceusDeadMotiveDropped is the Plan 6b-2 Task 2 gate: removing the
// curry-through carve-out so the dead motive in b0 is dropped.
//
// PRE-REMOVAL (with carve-out): b0 is isCurryThrough (bare-MkClosure body),
// so Perceus skips dead-dropping its owned arg. The inline motive K_CLO
// (freshly allocated each run) leaks: per-run delta = 1 > 0 = threshold.
// Test FAILS before the fix, catching the carve-out.
//
// POST-REMOVAL (without carve-out): ownScope inserts CDrop{CVar{0}, ...} in
// b0 because cirUsesArg(body, 0) = false (motive not in b0's empty env).
// Motive K_CLO is freed each run: per-run delta = 0 <= 0. Test PASSES.
//
// OUTPUT-INVARIANCE: the eliminator still prints "7" (some 7 -> 7). A
// use-after-free from a premature drop would corrupt this to a wrong value.
func TestPerceusDeadMotiveDropped(t *testing.T) {
	// Output-invariance: removing the carve-out must not change the computed value.
	got := runWasm(t, emitWith(t, cg.Wasm{}, perceusDeadMotiveSrc, "mainElim"))
	if got != "7" {
		t.Fatalf("output changed after carve-out removal: got %q want \"7\"", got)
	}
	// Delta gate: per-run $rt_live increment must drop to <= inlineElimResidualAfterMotive.
	p := mustProgram(t, perceusDeadMotiveSrc, "mainElim")
	counts := wasmSteadyLivePInts(t, p, 4)
	d := counts[3] - counts[2]
	if d > inlineElimResidualAfterMotive {
		t.Fatalf("dead motive still leaks: per-run delta=%d > %d (counts %v); "+
			"is the curry-through carve-out still present in Perceus?",
			d, inlineElimResidualAfterMotive, counts)
	}
	t.Logf("dead motive dropped: counts %v, per-run delta=%d (carve-out removed)", counts, d)
}
