package codegen

import (
	"fmt"

	"goforge.dev/rune/v3/core"
)

// perceus.go -- the Perceus ownership-insertion pass over the closure-converted
// CIr. It inserts CDup/CDrop so heap values are reference-counted by the Plan 6a
// ARC runtime (rt_retain/rt_release). Only the WASM backend runs it (Wasm.Emit);
// the C/LLVM and source backends consume the un-annotated CIr.
//
// Soundness rests on the erased IR being immutable, total, and acyclic (R-ARC):
// reference counting with no cycle collector is sound and complete. v1 inserts
// dup/drop only; the in-place reuse optimization is a later plan. See
// ref_docs/wootz/R-PERCEUS.md for the ownership rules and the receiver map.
//
// PATH B ownership convention (the 6a runtime stays FROZEN; store = MOVE):
//   - OWNED values (freshly allocated rc=1, or received as an owned argument) MOVE
//     on store/consume. Multi-used owned locals get N-1 dups for N consumes.
//   - BORROWED values (a CGlobal thunk-cached root the cache owns, a CEnv capture
//     the closure owns, a CField/CFst/CSnd projection aliasing its parent) require a
//     CDup WHEN CONSUMED in an owning position (passed as an argument, stored into a
//     constructor/pair/closure-env slot, or returned as the scope result). The owner
//     keeps its reference; the consumer takes a fresh one (consumeOwning).
//   - ARGS are OWNED by the callee: a code block OWNS CVar{0} and DROPS it when dead,
//     uniformly with dead CLet locals. Because the caller dups a borrowed value
//     before passing it, a cached root arrives as a FRESH owned reference, so the
//     callee dropping it leaves the cache's reference intact -- no corruption.
//
// TASK 3.5: reworks Task 3's argCount dead-drop SKIP (a non-generalizable workaround)
// into the principled PATH B model above. shiftCIr recurses through ALL nodes (the
// structural dual of cirUsesArg) so feeding a constructor or pair as a closure-application
// argument keeps de Bruijn indices correct. Task 4 extends annotate with CCase/CField;
// Task 5 extends it with CPair/CFst/CSnd (owned component positions + drop-after for
// pair projections).

// shiftCIr shifts all free CVar indices >= minDepth by amount. Used when inserting a
// CLet binder around an AppClosure (or a borrowed value) to keep de Bruijn indices
// correct in the continuation. It is the structural dual of cirUsesArg and must
// handle EVERY node symmetrically: a MkClosure's code-block body is NOT in the node
// (a separate closed scope) and is never shifted -- only its Env terms, evaluated in
// the enclosing frame, shift; a CLet body sits one binder deeper; CCase arms share
// the enclosing context (no shift offset).
func shiftCIr(t CIr, minDepth, amount int) CIr {
	switch x := t.(type) {
	case CVar:
		if x.Idx >= minDepth {
			return CVar{Idx: x.Idx + amount}
		}
		return x
	case CEnv, CGlobal, CForeign, CUnit, CLit:
		return x
	case CLet:
		return CLet{
			Name: x.Name,
			Val:  shiftCIr(x.Val, minDepth, amount),
			// CLet introduces a new binder, so the body's free indices are shifted
			// relative to the enclosing scope: increment minDepth by 1.
			Body: shiftCIr(x.Body, minDepth+1, amount),
		}
	case AppClosure:
		return AppClosure{
			Clo: shiftCIr(x.Clo, minDepth, amount),
			Arg: shiftCIr(x.Arg, minDepth, amount),
		}
	case MkClosure:
		env := make([]CIr, len(x.Env))
		for i, e := range x.Env {
			env[i] = shiftCIr(e, minDepth, amount)
		}
		// Code is a separate closed scope: its body is NOT shifted, only Env terms.
		return MkClosure{Code: x.Code, Env: env}
	case CPair:
		return CPair{A: shiftCIr(x.A, minDepth, amount), B: shiftCIr(x.B, minDepth, amount)}
	case CFst:
		return CFst{P: shiftCIr(x.P, minDepth, amount)}
	case CSnd:
		return CSnd{P: shiftCIr(x.P, minDepth, amount)}
	case CField:
		return CField{Scrut: shiftCIr(x.Scrut, minDepth, amount), Index: x.Index}
	case CCase:
		arms := make([]CCaseArm, len(x.Arms))
		for i, a := range x.Arms {
			// Arms share the enclosing context (no binder offset; see cirUsesArg).
			arms[i] = CCaseArm{Tag: a.Tag, Body: shiftCIr(a.Body, minDepth, amount)}
		}
		return CCase{Scrut: shiftCIr(x.Scrut, minDepth, amount), Arms: arms}
	case CBounce:
		return CBounce{Call: shiftCIr(x.Call, minDepth, amount)}
	case CDup:
		return CDup{V: shiftCIr(x.V, minDepth, amount), K: shiftCIr(x.K, minDepth, amount)}
	case CDrop:
		return CDrop{V: shiftCIr(x.V, minDepth, amount), K: shiftCIr(x.K, minDepth, amount)}
	default:
		panic(fmt.Sprintf("codegen(perceus): shiftCIr unknown CIr node %T", t))
	}
}

// consumeOwning returns a CIr whose RESULT value is OWNED -- safe to transfer into an
// owning position (a function argument, a constructor/closure-env/pair slot, or the
// scope result). This is PATH B's dup-on-consume-borrowed.
//
//   - An already-owned value passes through unchanged: an owned-local CVar (consuming
//     it MOVES it) or a fresh allocation (MkClosure / AppClosure / CPair result).
//   - A BORROWED value is retained first with a CDup so the owner keeps its reference
//     and the consumer takes a fresh one. A bare capture name (CEnv) is dup'd in place
//     (CDup.V/K both name the slot). A borrowed leaf that is NOT a bare variable
//     (CGlobal thunk-cached root, or a CField/CFst/CSnd projection aliasing its
//     parent) is let-bound first, then the binding is dup'd -- CDup.V must name a
//     CVar/CEnv, never an allocating or projecting expression.
//
// The result value of a CLet/CDup/CDrop is its continuation (Body/K), so consumeOwning
// reaches the TAIL through those wrappers: a let- or closure-release-wrapped borrowed
// result is still made owned. It deliberately does NOT descend into CCase arms or
// allocation sub-terms (those are owned at their own owning positions; Task 4
// annotates case-result ownership). It is idempotent (a borrowed leaf becomes an
// owned CVar/CDup tail, which then passes through unchanged).
func consumeOwning(t CIr) CIr {
	switch x := t.(type) {
	case CEnv:
		// A bare capture name: retain the slot, then yield it.
		return CDup{V: x, K: x}
	case CGlobal, CField, CFst, CSnd:
		// A borrowed leaf that is not a bare variable: let-bind it, dup the binding.
		// The CLet's Val is evaluated in the enclosing context (the borrowed leaf's
		// own free vars stay put); its Body is closed (only CVar{0}), so wrapping the
		// leaf in this CLet does not shift any enclosing de Bruijn index.
		return CLet{Name: "$own", Val: t, Body: CDup{V: CVar{Idx: 0}, K: CVar{Idx: 0}}}
	case CLet:
		return CLet{Name: x.Name, Val: x.Val, Body: consumeOwning(x.Body)}
	case CDup:
		return CDup{V: x.V, K: consumeOwning(x.K)}
	case CDrop:
		return CDrop{V: x.V, K: consumeOwning(x.K)}
	default:
		// CVar (owned local), MkClosure / AppClosure / CPair (fresh allocations),
		// CUnit / CLit / CForeign, and CCase / CBounce (out of the Task 3.5 transfer
		// fragment): already owned, or owned at their own owning positions.
		return t
	}
}

// pairProjSrc returns the de Bruijn index k if t is CFst{CVar{k}} or CSnd{CVar{k}}
// where owned[k] = true (the pair source is an owned local). Returns -1 otherwise.
// Used by the CLet case to detect a projection of an owned pair local and insert the
// pair-drop-after pattern (dup projection, then drop pair when dead in body).
func pairProjSrc(t CIr, owned []bool) int {
	var p CIr
	switch x := t.(type) {
	case CFst:
		p = x.P
	case CSnd:
		p = x.P
	default:
		return -1
	}
	cv, ok := p.(CVar)
	if !ok || cv.Idx < 0 || cv.Idx >= len(owned) || !owned[cv.Idx] {
		return -1
	}
	return cv.Idx
}

// perceusPass holds per-pass state. It carries the builtin-nat eliminator name and the
// accel-op table (both from the program's NatSpec) so the AppClosure case can RECOGNIZE
// a saturated accel / nat-elim spine -- the spine the WASM emitter pattern-matches
// (accelMatchC / NatElimSpine) and shortcuts -- and leave its AppClosure backbone BARE
// (the recognize-then-skip rule; see annotateBareSpine).
type perceusPass struct {
	natElim string
	accel   map[string]core.NatOp
	// bareSpineHead holds the emitted CONSTRUCTOR names whose SATURATED-OR-PARTIAL
	// application spine must stay BARE (no curry-intermediate release). A constructor's
	// curry steps MOVE each accumulated field forward into the final K_CON via the
	// hand-emitted emitCtorBlock WAT (store = MOVE), so releasing an intermediate would
	// free a field the K_CON now owns (use-after-free). Only ORDINARY function curry
	// intermediates (whose final block dup-on-escapes its result) are released.
	//
	// NOTE (FIX A, 6-pre-fix): general DATATYPE ELIMINATORS are NOT in this set.
	// Eliminator curry blocks consumeOwning their CEnv prefix captures (dup forward), so
	// releasing their intermediates is BALANCED, not a UAF. The over-broad original
	// inclusion of d.ElimName made every inline pattern-match leak its intermediates
	// (hollow 6-gate). Accel ops + the builtin-nat eliminator are recognized separately
	// (accelMatchC / NatElimSpine) per the recognize-then-skip rule; they are not in
	// bareSpineHead either.
	//
	// bareSpineHead ALSO governs the FIX B shared-owned-local dup in annotateBareSpine:
	// because constructors store=MOVE, a local appearing in two argument slots of the same
	// constructor spine is consumed twice and needs N-1 dups (annotateBareSpine isCtor=true
	// path). Accel/nat spines borrow their args (the emitter never builds the intermediate);
	// no dup is needed or correct there.
	bareSpineHead map[string]bool
}

// Perceus inserts CDup/CDrop into all code blocks and top-level defs of the closure
// program. Each code block OWNS its argument (CVar{0}); top-level defs own nothing
// (owned = nil). Returns the annotated program.
func Perceus(p ClosureProgram) ClosureProgram {
	pp := &perceusPass{bareSpineHead: map[string]bool{}}
	if p.Nat != nil {
		pp.natElim = p.Nat.ElimName
		pp.accel = p.Nat.Ops
	}
	// Only CONSTRUCTOR spines stay bare (their emitCtorBlock curry steps MOVE each
	// accumulated field into the final K_CON by store=MOVE; releasing an intermediate
	// would free a field the K_CON now owns -- use-after-free). General DATATYPE
	// ELIMINATORS are NOT added here (FIX A): their curry blocks consumeOwning their
	// CEnv prefix captures (dup forward), so releasing their intermediates is balanced.
	for _, d := range p.Datas {
		for _, c := range d.Ctors {
			pp.bareSpineHead[c.Name] = true
		}
	}
	for i := range p.Blocks {
		// Every code-block argument (CVar{0}) is an owned local, dropped when dead
		// (PATH B). This includes curry-through blocks (bare-MkClosure bodies) --
		// the former carve-out that left a dead curry argument borrowed was REMOVED
		// by Plan 6b-2 Task 2. That carve-out existed because the OLD emitNatFold
		// borrow-passed its loop counter; Task 1 made emitNatFold retain the counter
		// before the step call, so the step block receives an owned reference and
		// dropping a dead curry argument is safe. The carve-out was leaking the dead
		// motive argument of inline general-eliminator applications (+1/run per call).
		p.Blocks[i].Body = pp.ownScope(p.Blocks[i].Body, []bool{true})
	}
	for i := range p.Defs {
		// Defs have no owned code-block argument.
		p.Defs[i].Body = pp.ownScope(p.Defs[i].Body, nil)
	}
	return p
}

// ownScope is the entry point for a lexical scope. owned[i] = true means the CVar at
// de Bruijn index i is owned by the current scope. It:
//  1. Annotates t (inserting CDup for multiply-consumed owned locals and CLet+CDrop
//     for consumed closures), then ensures the scope's RESULT value is owned
//     (dup-on-consume-borrowed at the return position).
//  2. Wraps the result with CDrop for each DEAD owned local -- INCLUDING code-block
//     arguments (PATH B drops dead owned args uniformly). Traverses high-to-low so the
//     outermost CDrop corresponds to the innermost binder.
//
// The dead check runs on the ORIGINAL t (pre-annotation) so annotate never sees its
// own inserted nodes.
func (pp *perceusPass) ownScope(t CIr, owned []bool) CIr {
	out := consumeOwning(pp.annotate(t, owned))
	for i := len(owned) - 1; i >= 0; i-- {
		if owned[i] && !cirUsesArg(t, i) {
			out = CDrop{V: CVar{Idx: i}, K: out}
		}
	}
	return out
}

// annotate traverses t and inserts CDup/CDrop annotations. owned is unchanged here
// (only ownScope and the CLet case create extended slices for recursive calls). Each
// owning position (an AppClosure Arg, a MkClosure env slot) is routed through
// consumeOwning so a borrowed value flowing in is dup'd first.
func (pp *perceusPass) annotate(t CIr, owned []bool) CIr {
	switch x := t.(type) {

	case CVar:
		// A single CVar is a consuming use -- ownership MOVES to whoever receives the
		// result. No CDup (one use = one transfer); no CDrop (cirUsesArg is true so
		// ownScope does not dead-drop it).
		return x

	case CEnv, CGlobal, CForeign, CUnit, CLit:
		// Borrowed/global/constant: no dup/drop as a bare leaf here. consumeOwning at
		// the enclosing owning position inserts the dup when one of these is consumed.
		return x

	case CLet:
		// CLet introduces a new owned binder at de Bruijn index 0, shifting existing
		// indices by +1. Annotate the value in the current scope, open a new scope for
		// the body.
		val := pp.annotate(x.Val, owned)

		// Pair-projection ownership (Task 5): if Val is CFst/CSnd of an owned local
		// CVar{k}, the binding gets an ALIAS of the component (borrowed at runtime).
		// consumeOwning inserts a CDup so the binding carries its own reference
		// (component.rc+1). The pair is then dropped after the body if it is dead
		// in x.Body (the component outlives the pair drop because rc was pre-incremented).
		srcIdx := pairProjSrc(x.Val, owned)
		if srcIdx >= 0 {
			val = consumeOwning(val)
		}

		bodyOwned := make([]bool, len(owned)+1)
		bodyOwned[0] = true
		copy(bodyOwned[1:], owned)

		// FIX: bare-variable rebind (Task 6-fix). When Val is a bare owned local CVar{k},
		// the let-binding `let h = g in body` is a MOVE of g into h: h and g are the same
		// heap pointer. h (at body index 0) owns the value; g (at body index k+1) must be
		// treated as BORROWED so ownScope does not dead-drop it (which would release the
		// pointer that h will also drop, causing a use-after-free). This mirrors the CPair
		// component marking below: both cases suppress the source's dead-drop because the
		// value was MOVED, not copied. g's owning-position uses in the body get CDup'd by
		// consumeOwning (borrowed -> owned); g's non-owning uses are safe borrows (aliases).
		if cv, ok := x.Val.(CVar); ok && cv.Idx >= 0 && cv.Idx < len(owned) && owned[cv.Idx] {
			bodyOwned[cv.Idx+1] = false
		}

		// When Val is CFst/CSnd of owned local k and the pair is dead in x.Body,
		// set bodyOwned[k+1] = false so ownScope does not dead-drop the pair. The
		// drop-after below is the sole consumer of the pair's lifetime.
		pairDeadInBody := srcIdx >= 0 && !cirUsesArg(x.Body, srcIdx+1)
		if pairDeadInBody {
			bodyOwned[srcIdx+1] = false
		}

		// When Val is CPair and its components are owned local CVars, those locals
		// are MOVED into the pair (rt_mkpair stores them with store=MOVE). Mark them
		// not-owned in the body so ownScope does not dead-drop them -- the pair's
		// rt_free will release them when the pair itself is dropped.
		if cv, ok := x.Val.(CPair); ok {
			if cvA, ok2 := cv.A.(CVar); ok2 && cvA.Idx >= 0 && cvA.Idx < len(owned) && owned[cvA.Idx] {
				bodyOwned[cvA.Idx+1] = false
			}
			if cvB, ok2 := cv.B.(CVar); ok2 && cvB.Idx >= 0 && cvB.Idx < len(owned) && owned[cvB.Idx] {
				bodyOwned[cvB.Idx+1] = false
			}
		}

		body := pp.ownScope(x.Body, bodyOwned)

		if pairDeadInBody {
			// Drop-after: bind the body result ($pres), then release the pair.
			// In bodyOwned's scope the pair is at srcIdx+1 (new 'a' binding = 0).
			// After binding $pres (+1 shift), pair index becomes srcIdx+2.
			body = CLet{
				Name: "$pres",
				Val:  body,
				Body: CDrop{V: CVar{Idx: srcIdx + 2}, K: CVar{Idx: 0}},
			}
		}

		out := CIr(CLet{Name: x.Name, Val: val, Body: body})
		// Capture-then-use-later (and any Val/Body shared owned local): a local
		// consumed in BOTH the bound value and the continuation is consumed twice
		// (e.g. captured into a closure env in Val AND applied in Body), so it needs a
		// dup before the binding. In the body the local sits one binder deeper (i+1).
		// Guard: if Val is a pure pair projection of local i (a borrow, not a consume),
		// do NOT dup i -- its lifetime is managed by the drop-after above.
		for i := len(owned) - 1; i >= 0; i-- {
			if owned[i] && cirUsesArg(x.Val, i) && cirUsesArg(x.Body, i+1) {
				if srcIdx >= 0 && i == srcIdx {
					continue // pair projection is a borrow; drop-after handles its lifetime
				}
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out

	case AppClosure:
		// RECOGNIZE-THEN-SKIP (Phase 6-pre): a saturated accel (add/mul/monus) or
		// builtin-nat-elim spine is recognized + shortcut by the WASM emitter, which
		// pattern-matches the RAW AppClosure backbone (accelMatchC / NatElimSpine) ending
		// in a CGlobal. A recognized accel spine is lowered by accelDispatch DIRECTLY to
		// rt_nat_add/mul/monus(a, b) -- the intermediate CLOSURE `(add a)` is NEVER BUILT,
		// so there is no intermediate to leak; leaving the backbone bare is NECESSARY (so
		// the matcher fires). A recognized nat-elim spine is likewise left bare (the frozen
		// emitNatFold borrows its loop counter via non-retaining rt_apply, so releasing its
		// intermediates would corrupt it). annotateBareSpine keeps the whole backbone bare
		// and annotates the leaf args (owned + the shared-owned-local dup). NOTE: the leaf
		// OPERANDS are not borrows -- since Task 4d they CONSUME (accelDispatch frees a
		// freshOwned operand; succ_code frees its arg), which is exactly why
		// annotateBareSpine must dup a shared owned local (see its docstring).
		if pp.isRecognizedSpine(x) {
			// Keep the backbone bare for the matcher; annotateBareSpine makes the leaf
			// args owned and inserts the shared-owned-local dup. The dup is needed for
			// EVERY recognized spine: a constructor stores fields by MOVE, and (since
			// Task 4d) accel/nat operands CONSUME too (accelDispatch frees a freshOwned
			// operand; succ_code frees its arg), so a local consumed in two operand
			// sub-spines (e.g. `addN (succ n) (succ n)`) is freed twice without it.
			return pp.annotateBareSpine(x, owned)
		}

		clo := pp.annotate(x.Clo, owned)
		// The argument is consumed by the callee (which owns its CVar{0}); make it
		// owned so a borrowed arg is dup'd before the call.
		arg := consumeOwning(pp.annotate(x.Arg, owned))

		// Release the closure after the call when it is an owned local (CVar), a freshly
		// allocated inline closure (MkClosure), OR a CURRY INTERMEDIATE (an AppClosure-
		// headed Clo, e.g. the `(f a)` in `(f a) b`). Each is a K_CLO that nothing else
		// frees: an applied closure is consumed by rt_apply but rt_apply does NOT release
		// it (it would free closures at rc=1 in the no-Perceus runtime, which stays
		// FROZEN), so the pass releases it post-call. The curry-intermediate case is what
		// Phase 6-pre adds (Task 3 released only CVar/MkClosure): it is now safe because a
		// RECOGNIZED accel/nat spine was already routed to annotateBareSpine above, so the
		// only AppClosure-headed Clos reaching here are ORDINARY curry intermediates the
		// emitter does NOT pattern-match -- releasing them cannot break accelMatchC /
		// NatElimSpine. A CGlobal Clo (a cached root / constructor) stays borrowed (NOT
		// released): the cache owns it.
		_, isCloVar := x.Clo.(CVar)
		_, isCloMk := x.Clo.(MkClosure)
		_, isCloApp := x.Clo.(AppClosure)

		var out CIr
		if isCloVar || isCloMk || isCloApp {
			// CLet("$clo", clo, CLet("$res", AppClosure(CVar{0}, shiftedArg),
			//   CDrop{CVar{1}, CVar{0}})). The arg shifts by 1: the $clo binder
			// displaces existing indices.
			shiftedArg := shiftCIr(arg, 0, 1)
			out = CLet{
				Name: "$clo",
				Val:  clo,
				Body: CLet{
					Name: "$res",
					Val:  AppClosure{Clo: CVar{Idx: 0}, Arg: shiftedArg},
					Body: CDrop{V: CVar{Idx: 1}, K: CVar{Idx: 0}},
				},
			}
		} else {
			out = AppClosure{Clo: clo, Arg: arg}
		}

		// An owned local consumed in BOTH Clo and Arg is consumed twice: dup it once
		// before the pair. Check the ORIGINAL x.Clo/x.Arg for correct cirUsesArg.
		for i := len(owned) - 1; i >= 0; i-- {
			if owned[i] && cirUsesArg(x.Clo, i) && cirUsesArg(x.Arg, i) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out

	case MkClosure:
		// Each env term is stored into the closure (an owning reference): make it owned
		// so a borrowed capture is dup'd before the store.
		env := make([]CIr, len(x.Env))
		for i, e := range x.Env {
			env[i] = consumeOwning(pp.annotate(e, owned))
		}
		out := CIr(MkClosure{Code: x.Code, Env: env})
		// If an owned local is stored in N env slots it is consumed N times: N-1 dups.
		// (The cross-let capture+use-later dup is handled by the CLet case above.)
		for i := len(owned) - 1; i >= 0; i-- {
			if !owned[i] {
				continue
			}
			n := 0
			for _, e := range x.Env {
				if cirUsesArg(e, i) {
					n++
				}
			}
			for k := 1; k < n; k++ {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out

	case CField:
		// rt_con_get returns an ALIAS into the scrutinee (a borrowed read; it does not
		// transfer ownership). Annotate the scrutinee (the projection itself consumes
		// nothing). A projected value that ESCAPES into an owning position is dup'd
		// there by consumeOwning's CField arm -- not here.
		return CField{Scrut: pp.annotate(x.Scrut, owned), Index: x.Index}

	case CCase:
		return pp.annotateCase(x, owned)

	case CPair:
		// Both A and B are owning positions (rt_mkpair stores them with store=MOVE).
		// consumeOwning ensures a borrowed value (CEnv, CGlobal, projection) is dup'd
		// before the store so the pair becomes the sole owner. An owned local (CVar)
		// passes through (moves into the pair). If the same owned local appears in
		// both A and B (rare but valid), dup it once so each slot gets a live reference.
		a := consumeOwning(pp.annotate(x.A, owned))
		b := consumeOwning(pp.annotate(x.B, owned))
		out := CIr(CPair{A: a, B: b})
		for i := len(owned) - 1; i >= 0; i-- {
			if owned[i] && cirUsesArg(x.A, i) && cirUsesArg(x.B, i) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out

	case CFst:
		// rt_pair_fst returns an ALIAS (borrow); no retain here. When this CFst is
		// the Val of a CLet whose pair source is dead in the body, the CLet case
		// above inserts consumeOwning (dup) + drop-after (pair released after body).
		// When CFst escapes to an owning position (return slot, AppClosure arg),
		// consumeOwning's CFst arm dups it there.
		return CFst{P: pp.annotate(x.P, owned)}

	case CSnd:
		return CSnd{P: pp.annotate(x.P, owned)}

	default:
		// CBounce: outside the covered fragment. Carry through unchanged.
		return t
	}
}

// isRecognizedSpine reports whether the AppClosure x is a spine that must stay BARE
// (no CLet/CDup/CDrop on its Clo chain after the Perceus pass):
//   - A 2-argument accel-op application (accelMatchC): the WASM emitter shortcuts it
//     DIRECTLY to rt_nat_add/mul/monus(a,b) -- the intermediate closure is never built,
//     so there is nothing to release.
//   - A >=4-argument builtin-nat-elim application (NatElimSpine): the frozen emitNatFold
//     borrow-passes its loop counter (non-retaining rt_apply) and reuses it.
//   - A constructor spine (spineBaseGlobal in bareSpineHead): the hand-emitted
//     emitCtorBlock WAT moves each field into the K_CON by store=MOVE; releasing an
//     intermediate would free a field the K_CON now owns (use-after-free).
//
// General datatype ELIMINATORS are NOT recognized here (FIX A): their curry blocks
// consumeOwning their CEnv prefix captures (dup forward), so releasing their
// intermediates is balanced. The over-broad original inclusion made every inline
// pattern-match leak its intermediates (hollow 6-gate).
//
// Checked on the ORIGINAL x, before any annotation -- the matchers see no ownership
// wrappers in the C / other backends (which never run Perceus).
func (pp *perceusPass) isRecognizedSpine(x AppClosure) bool {
	if _, _, _, ok := accelMatchC(x, pp.accel); ok {
		return true
	}
	if _, ok := NatElimSpine(pp.natElim, x); ok {
		return true
	}
	// A constructor spine (recognized by its base head, at ANY arity:
	// even a partially applied constructor used as a value must keep its intermediates).
	if g, ok := spineBaseGlobal(x); ok && pp.bareSpineHead[g] {
		return true
	}
	return false
}

// spineBaseGlobal walks an AppClosure's Clo backbone to its base and returns the
// CGlobal name if the base is a CGlobal (the head of a constructor / eliminator /
// def / accel / nat-elim spine). Returns ok=false for a CVar / MkClosure base (an
// ordinary local or inline-lambda spine, whose curry intermediates are releasable).
func spineBaseGlobal(x AppClosure) (string, bool) {
	var t CIr = x
	for {
		app, ok := t.(AppClosure)
		if !ok {
			break
		}
		t = app.Clo
	}
	if g, ok := t.(CGlobal); ok {
		return g.Name, true
	}
	return "", false
}

// annotateBareSpine annotates a RECOGNIZED accel / nat-elim / constructor spine while
// keeping its AppClosure backbone BARE: no CLet/CDup/CDrop wrappers anywhere on the Clo
// chain, so the WASM emitter's accelMatchC / NatElimSpine still recognize it. Only the
// leaf ARGS are annotated and made owned (consumeOwning), and the SHARED-owned-local dup
// is applied at each level (see below). A CDup that wraps the WHOLE spine is fine -- the
// emitter sees CDup, retains, then recurses to the bare spine inside, so the matcher
// still fires.
//
// SHARED-OWNED-LOCAL DUP (all recognized spines). An owned local appearing in BOTH a
// deeper Clo level (x.Clo side) AND the current arg slot (x.Arg) is consumed twice, so it
// needs N-1 dups across N consuming slots (mirror the general AppClosure shared-var dup):
//   - Constructor spine: emitCtorBlock / satCtorDispatch store each field by MOVE into the
//     final K_CON, so the same pointer in two field slots at rc=1 would double-free on
//     the K_CON's release.
//   - Accel / nat-elim spine: the operands CONSUME now (Task 4d -- accelDispatch releases
//     a freshOwned operand, and succ_code frees its arg), so the SAME owned local consumed
//     in two operand sub-spines (e.g. `addN (succ n) (succ n)`) is freed twice -> UAF
//     without the dup. A spine where the local is a bare borrowed operand (`addN n n`,
//     accel borrow-reads a bare CVar and does NOT free it) gets a harmless extra retain
//     (a +1/run LEAK, never a UAF -- a CDup can only over-retain); such accel/nat
//     programs are already outside the steady-flat fragment (PerceusBalanceable condition
//     2 excludes every NatElim/accel program), so the leak is invisible to the flat gate
//     while output stays correct. Correctness (no double-free) is the binding requirement
//     here, so the dup is applied UNCONDITIONALLY rather than guessing borrow-vs-consume.
func (pp *perceusPass) annotateBareSpine(x AppClosure, owned []bool) CIr {
	var clo CIr
	if inner, ok := x.Clo.(AppClosure); ok {
		// More backbone: recurse, keeping it bare (do NOT re-enter the general
		// AppClosure case, which would release this AppClosure-headed intermediate).
		clo = pp.annotateBareSpine(inner, owned)
	} else {
		// The spine head: a CGlobal (constructor / accel op / nat eliminator).
		// annotate is a no-op on a bare CGlobal but keeps the code uniform.
		clo = pp.annotate(x.Clo, owned)
	}
	arg := consumeOwning(pp.annotate(x.Arg, owned))
	out := CIr(AppClosure{Clo: clo, Arg: arg})
	// Shared-owned-local dup: a local consumed in both the Clo sub-spine AND this Arg
	// level is consumed twice; dup it once so each consumption frees a live reference.
	for i := len(owned) - 1; i >= 0; i-- {
		if owned[i] && cirUsesArg(x.Clo, i) && cirUsesArg(x.Arg, i) {
			out = CDup{V: CVar{Idx: i}, K: out}
		}
	}
	return out
}

// annotateCase inserts ownership for an eliminator's tag dispatch (CCase). The
// scrutinee is BORROWED for the tag read (rt_con_tag does not consume) and for the
// arms' field reads (rt_con_get returns aliases); after the arms it is owned and must
// be DROPPED on EVERY arm path. The common shape -- the one a lowered eliminator's
// final lambda-x block produces -- is an OWNED-local scrutinee CVar{k} whose arms
// project its fields via CField{Scrut: CVar{k}} (the same index, no binder; arms share
// the enclosing context). For that shape:
//   - Each arm is annotated with the scrutinee marked NOT-owned, so the multi-use dup
//     logic and the per-arm dead-drop do NOT touch it -- its field reads are borrows,
//     not consumes. The scrutinee is consumed exactly once, by the drop below.
//   - Each arm RESULT is run through consumeOwning so a borrowed return (a returned
//     capture CEnv, or the kept field a `some x` arm projects) is dup'd -- the field
//     thus carries its own reference past the scrutinee's free (no use-after-free).
//   - PER-ARM LIVENESS: an owned local live across the case but dead in THIS arm is
//     dropped at the arm front (a local dead in EVERY arm is dropped once before the
//     case by ownScope). The scrutinee is excluded -- it has the drop-after below.
//   - The scrutinee is dropped AFTER the arm body computes (after its field reads):
//     bind the result, release the scrutinee, yield the result (drop-after).
//
// A scrutinee that is NOT an owned local (a borrowed CGlobal/CEnv, or -- never produced
// by a lowered eliminator -- a non-variable term) is borrowed by, not owned by, this
// scope: nothing to drop. The arms are still owned and per-arm-liveness handled.
func (pp *perceusPass) annotateCase(x CCase, owned []bool) CIr {
	sv, isVar := x.Scrut.(CVar)
	scrutOwned := isVar && sv.Idx >= 0 && sv.Idx < len(owned) && owned[sv.Idx]

	arms := make([]CCaseArm, len(x.Arms))
	for i, a := range x.Arms {
		armOwned := owned
		if scrutOwned {
			armOwned = append([]bool(nil), owned...)
			armOwned[sv.Idx] = false // borrowed within the arm (field reads only)
		}
		body := consumeOwning(pp.annotate(a.Body, armOwned))

		// Per-arm liveness for OTHER owned locals: drop one that is live across the
		// case (used in some arm or the scrutinee position) but dead in this arm.
		for j := len(owned) - 1; j >= 0; j-- {
			if !owned[j] || (scrutOwned && j == sv.Idx) {
				continue
			}
			if cirUsesArg(x, j) && !cirUsesArg(a.Body, j) {
				body = CDrop{V: CVar{Idx: j}, K: body}
			}
		}

		if scrutOwned {
			// Drop-after: `let $scrut = <arm> in drop scrutinee; $scrut`. Binding the
			// result shifts the scrutinee index by 1 in the CDrop continuation; the
			// result (CVar{0}) does not mention the scrutinee, so the CDrop is clean.
			body = CLet{
				Name: "$scrut",
				Val:  body,
				Body: CDrop{V: CVar{Idx: sv.Idx + 1}, K: CVar{Idx: 0}},
			}
		}
		arms[i] = CCaseArm{Tag: a.Tag, Body: body}
	}
	// The scrutinee position is a borrowed tag read: annotate it (a bare CVar passes
	// through), do NOT consumeOwning it.
	return CCase{Scrut: pp.annotate(x.Scrut, owned), Arms: arms}
}

// PerceusBalanceable reports whether a program lies in the Perceus FLAT fragment:
// programs outside the fragment are the named 6b-2 frontier (R-PERCEUS.md). It is
// CONSERVATIVE: when unsure, it returns false (exclude from the steady gate rather
// than wrongly asserting flat). The HARD INVARIANT is one-sided: it must NEVER return
// true for a program that is not actually steady-flat; a flat program it conservatively
// excludes is merely a missed opportunity, never a soundness break.
//
// Plan 6b-2 RE-OPENED this predicate. The earlier four-condition form excluded the
// whole flat fragment (0 balanceable). Tasks 2-4d closed the per-run residuals it was
// guarding against -- the dead datatype-eliminator MOTIVE (Task 2), the arity>=2
// constructor container K_CLO (Task 3), the rt_big_parse per-digit temps (Task 4), the
// rt_big_succ temp (Task 4b), and the accel/succ owned-operand frees (Task 4d) -- so
// the former exclusion conditions 3 (inline general eliminator) and 4 (inline arity>=2
// constructor) are REMOVED. Empirically every corpus listing those two conditions
// excluded now reaches TRUE steady-flat: the recursive datatype eliminators `three`
// (NatElim fold) and `two` (ListElim fold) and the arity-4 constructor `p` all measure
// [4 4 4 4]/[2 2 2 2] (wasmSteadyLivePInts, runs 2..5 delta 0). NOTE: the corpus's
// recursive eliminators recurse via the induction hypothesis `ih`, not by calling the
// eliminator head by NAME; both are flat. A pathological eliminator whose SOURCE step
// re-invokes the eliminator head is not in the corpus and is not separately verified
// flat -- but it is also caught by TestPerceusCorpusSteady (which asserts flat for every
// balanceable listing), so a future leaky balanceable program fails loudly rather than
// silently mis-asserting.
//
// SATURATED builtin-nat FOLDS are now in the fragment (satElimDispatch, the 6b-2
// follow-up). A saturated `NatElim mot z step n` lowers via satElimDispatch to a single
// b3 fold closure that is released after the call (no leaked b0/b1/b2 curry K_CLOs, no
// stored motive; emitNatFold retains its base), so an accel-free fold reaches true
// steady-flat -- verified for succ-step, ctor-step, and nested folds, and for the corpus
// `big`/`bigger`/`product` (plain NatElim multiplies, 100 iterations, [1..5]/[2..5] flat).
//
// The two REMAINING exclusion categories (both still leak, both deferred frontiers):
//
//  1. CBounce (partial/trampoline): UNSUPPORTED on WASM -- emitIn has no CBounce case,
//     so a partial-recursive program does not lower at all. This is a missing-feature
//     exclusion, NOT a leak: ownership across a deferred saturated tail call needs the
//     WASM-partial-support frontier (a separate future plan; see R-PERCEUS).
//
//  2. ACCEL-op programs (a program that REGISTERS natAdd/natMul/natMonus, i.e.
//     p.Nat.Ops is non-empty): an accel op BORROW-reads its operands but does not free a
//     bare-CVar operand (accelDispatch's freshOwned releases only freshly-produced
//     operands, never a CVar, to avoid freeing a borrowed root), so an accel op applied
//     to an OWNED-dead local -- `addN n n`, or an accel op inside a fold STEP like
//     `addN ih (succ n)` -- leaks that operand (+1/run, or +1/iteration in a step;
//     `armUse` measures +9/run). Statically separating a flat accel use (literal/fresh
//     operands, e.g. `mulN 100 100`) from a leaky one (owned-local operand) needs the
//     PERCEUS-LEVEL accel-operand ownership annotation (the deferred accel-operand
//     frontier; see R-PERCEUS). So any accel-registering program is conservatively
//     excluded WHOLESALE -- some are actually flat (`sum`/`prod`), but the predicate must
//     never wrongly assert flat on the leaky ones, and the distinction is not static.
func PerceusBalanceable(p Program) bool {
	// Accel-op programs are excluded wholesale (the accel-operand leak above is not
	// statically separable from flat accel uses).
	if p.Nat != nil && len(p.Nat.Ops) > 0 {
		return false
	}

	cp := ClosureConvert(p)
	for _, b := range cp.Blocks {
		if cirUnbalanceable(b.Body) {
			return false
		}
	}
	for _, d := range cp.Defs {
		if cirUnbalanceable(d.Body) {
			return false
		}
	}
	return true
}

// cirUnbalanceable returns true if t (or any sub-term) contains a construct that
// places the program outside the Perceus flat fragment (see PerceusBalanceable). It
// traverses every node, mirroring cirUsesArg's scope rules: a MkClosure's code-block
// body is a separate lifted scope (already scanned as a separate CodeBlock by
// PerceusBalanceable), so only its Env captures are scanned. After the satElimDispatch
// follow-up the ONLY construct it flags is CBounce (the accel-op exclusion is handled
// at the program level in PerceusBalanceable; NatElim folds are now flat).
func cirUnbalanceable(t CIr) bool {
	switch x := t.(type) {
	case CBounce:
		// CBounce: partial/trampoline tail call -- UNSUPPORTED on WASM (emitIn has no
		// CBounce case). Excluded as a missing feature, not a leak.
		return true

	case AppClosure:
		// NatElim folds are no longer flagged (satElimDispatch made them flat); accel
		// programs are excluded at the program level in PerceusBalanceable. Recurse to
		// find a buried CBounce.
		return cirUnbalanceable(x.Clo) || cirUnbalanceable(x.Arg)

	case CLet:
		return cirUnbalanceable(x.Val) || cirUnbalanceable(x.Body)

	case MkClosure:
		// Code is a lifted code block scanned separately; scan only Env (evaluated
		// in the current scope, not the closed block scope).
		for _, e := range x.Env {
			if cirUnbalanceable(e) {
				return true
			}
		}
		return false

	case CPair:
		return cirUnbalanceable(x.A) || cirUnbalanceable(x.B)
	case CFst:
		return cirUnbalanceable(x.P)
	case CSnd:
		return cirUnbalanceable(x.P)
	case CField:
		return cirUnbalanceable(x.Scrut)

	case CCase:
		if cirUnbalanceable(x.Scrut) {
			return true
		}
		for _, a := range x.Arms {
			if cirUnbalanceable(a.Body) {
				return true
			}
		}
		return false

	case CDup:
		return cirUnbalanceable(x.V) || cirUnbalanceable(x.K)
	case CDrop:
		return cirUnbalanceable(x.V) || cirUnbalanceable(x.K)

	default:
		// CVar, CEnv, CGlobal, CForeign, CUnit, CLit: leaves. None contain a CBounce.
		return false
	}
}
