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
		body := p.Blocks[i].Body
		if isCurryThrough(body) {
			// CURRY-THROUGH carve-out: a block whose entire body is a single MkClosure
			// is a currying step that returns a closure. Its argument may be applied by
			// the FROZEN 6a fold/curry machinery -- emitNatFold (the builtin-nat
			// eliminator loop) and emitCurryBlock -- which BORROWS the value (rt_apply
			// does not retain) and REUSES it afterward (emitNatFold succ's its loop
			// counter after passing it to the step). Conservatively BORROW such an
			// argument: annotate the body, dup-on-consume-borrowed any escaping capture,
			// but do NOT drop the argument. Dropping it would free a value the frozen
			// caller still uses (the "101" / "succ <function>" corruption). A real
			// consumer (a non-MkClosure body, e.g. an eliminator leaf or a match arm)
			// drops its dead argument normally -- which is what Task 4's owned-scrutinee
			// receivers need. This is the principled boundary between the pass's
			// move-discipline and the frozen runtime's borrow-discipline at the
			// eliminator/fold interface; a leak of an ignored curried argument is bounded
			// and refined once Task 4+ brings eliminator/fold ownership into the pass.
			p.Blocks[i].Body = consumeOwning(pp.annotate(body, []bool{true}))
			continue
		}
		// A real consumer: the one code-block argument (CVar{0}) is an owned local,
		// dropped when dead (PATH B; no argCount skip).
		p.Blocks[i].Body = pp.ownScope(body, []bool{true})
	}
	for i := range p.Defs {
		// Defs have no owned code-block argument.
		p.Defs[i].Body = pp.ownScope(p.Defs[i].Body, nil)
	}
	return p
}

// isCurryThrough reports whether a code block's body is a single MkClosure -- a
// currying step that returns a closure without consuming its argument inline. Such a
// block's argument is conservatively BORROWED (see the carve-out in Perceus).
func isCurryThrough(body CIr) bool {
	_, ok := body.(MkClosure)
	return ok
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
		// rt_nat_add/mul/monus(a, b) -- the intermediate closure `(add a)` is NEVER BUILT,
		// so there is nothing to leak; leaving the backbone bare is both NECESSARY (so the
		// matcher fires) and leak-free. A recognized nat-elim spine is likewise left bare
		// (the frozen emitNatFold borrows its loop counter via non-retaining rt_apply, so
		// releasing its intermediates would corrupt it). annotateBareSpine keeps the whole
		// backbone bare and only makes the leaf args owned.
		if pp.isRecognizedSpine(x) {
			// After FIX A, bareSpineHead contains only constructors. A constructor
			// spine needs the shared-owned-local dup (FIX B: store=MOVE, an owned
			// local appearing in N arg slots needs N-1 dups). Accel/nat spines
			// borrow their args; no dup is needed or correct for them.
			isCtor := false
			if g, ok := spineBaseGlobal(x); ok && pp.bareSpineHead[g] {
				isCtor = true
			}
			return pp.annotateBareSpine(x, owned, isCtor)
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
// leaf ARGS are annotated and made owned (consumeOwning).
//
// isCtor=true (a constructor spine, after FIX A the ONLY kind in bareSpineHead): a
// constructor's emitCtorBlock curry steps store each accumulated field by MOVE into the
// final K_CON. An owned local appearing in BOTH a deeper Clo level (x.Clo side) AND the
// current arg slot (x.Arg) is consumed twice -- the K_CON owns the same pointer in two
// slots at rc=1, so releasing the K_CON would double-free (rc 1->0 then again -> UAF).
// FIX B: mirror the general AppClosure shared-var dup: for each owned local[i] where
// cirUsesArg(x.Clo, i) && cirUsesArg(x.Arg, i), insert CDup{CVar{i}, out}. This is
// applied at EACH level of the spine recursion so the check sees each (Clo, Arg) pair.
//
// isCtor=false (accel / nat-elim spines): args are borrow-read by the emitter (accel
// shortcut reads each arg directly; nat-elim borrows its loop counter). No dup is needed
// or correct there (and a spine-head CDup would break the matcher anyway).
func (pp *perceusPass) annotateBareSpine(x AppClosure, owned []bool, isCtor bool) CIr {
	var clo CIr
	if inner, ok := x.Clo.(AppClosure); ok {
		// More backbone: recurse, keeping it bare (do NOT re-enter the general
		// AppClosure case, which would release this AppClosure-headed intermediate).
		clo = pp.annotateBareSpine(inner, owned, isCtor)
	} else {
		// The spine head: a CGlobal (constructor / accel op / nat eliminator).
		// annotate is a no-op on a bare CGlobal but keeps the code uniform.
		clo = pp.annotate(x.Clo, owned)
	}
	arg := consumeOwning(pp.annotate(x.Arg, owned))
	out := CIr(AppClosure{Clo: clo, Arg: arg})
	// FIX B: constructor shared-owned-local dup. A local appearing in both the Clo
	// sub-spine AND this Arg level is consumed twice by store=MOVE; dup it once so
	// the K_CON owns two live references instead of double-owning one at rc=1.
	// accel/nat spines skip this (isCtor=false; borrow-read, no dup needed).
	if isCtor {
		for i := len(owned) - 1; i >= 0; i-- {
			if owned[i] && cirUsesArg(x.Clo, i) && cirUsesArg(x.Arg, i) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
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

// PerceusBalanceable reports whether a program lies in the v1 Perceus fragment:
// programs outside the fragment are the named 6b-2 frontier (R-PERCEUS.md). It is
// CONSERVATIVE: when unsure, it returns false (exclude from the steady gate rather
// than wrongly asserting flat). The closure-converted CIr is scanned for four
// known-unbalanceable constructs. All are leak-only (no UAF/double-free), but their
// per-run delta is non-zero, so the steady-state gate cannot reach flat for them
// until the 6b-2 plan closes each one.
//
// Exclusion categories (in scan order):
//
//  1. CBounce (partial/trampoline): ownership across a deferred saturated tail call
//     is not handled by the v1 pass. The trampoline driver reuses args across bounces
//     in a way the current drop rules do not model.
//
//  2. Inline builtin-nat-fold application (NatElimSpine): the frozen emitNatFold
//     borrow-passes its loop counter via non-retaining rt_apply and creates bignum
//     temporaries (rt_big_parse) whose lifetime the pass does not track.
//
//  3. Inline GENERAL eliminator application (saturated spine head in p.Datas
//     ElimNames): the dead-motive carve-out leaks +1/run (the dup'd motive arg the
//     curry-through block leaves borrowed), and bignum-literal scrutinees create
//     rt_big_parse temps. A PARTIALLY applied eliminator (cached as a def, applied
//     per-call via a separate CGlobal) is not flagged: only a spine with at least
//     (1 + numCtors + 1) args at a single application site.
//
//  4. Inline arity>=2 constructor application: the hand-emitted emitCtorBlock WAT
//     keeps the K_CLO_mk1 intermediate BARE (releasing it mid-spine would UAF a
//     moved field). Each inline multi-arg constructor application leaks +1/run per
//     call-site from the unfree'd intermediate. Cached partial applications (the
//     intermediate built once as a top-level thunk) avoid this.
func PerceusBalanceable(p Program) bool {
	cp := ClosureConvert(p)

	natElim := ""
	if p.Nat != nil {
		natElim = p.Nat.ElimName
	}

	// elimArities: general datatype eliminator names -> required saturation depth.
	// depth = 1 (motive) + len(ctors) (one case per constructor) + 1 (scrutinee).
	// The builtin-nat eliminator is checked separately (condition 2, NatElimSpine).
	elimArities := map[string]int{}
	// multiArgCtors: constructor names with Arity >= 2. emitCtorBlock builds Arity-1
	// intermediate K_CLO containers for each inline multi-arg application.
	multiArgCtors := map[string]bool{}
	for _, d := range p.Datas {
		if p.Nat != nil && d.ElimName == p.Nat.ElimName {
			continue // handled by NatElimSpine (condition 2)
		}
		elimArities[d.ElimName] = 1 + len(d.Ctors) + 1
		for _, c := range d.Ctors {
			if c.Arity >= 2 {
				multiArgCtors[c.Name] = true
			}
		}
	}

	for _, b := range cp.Blocks {
		if cirUnbalanceable(b.Body, natElim, elimArities, multiArgCtors) {
			return false
		}
	}
	for _, d := range cp.Defs {
		if cirUnbalanceable(d.Body, natElim, elimArities, multiArgCtors) {
			return false
		}
	}
	return true
}

// cirUnbalanceable returns true if t (or any sub-term) contains a construct that
// places the program outside the v1 Perceus balanceable fragment (see
// PerceusBalanceable). It traverses every node, mirroring cirUsesArg's scope rules:
// a MkClosure's code-block body is a separate lifted scope (already scanned as a
// separate CodeBlock by PerceusBalanceable), so only its Env captures are scanned.
func cirUnbalanceable(t CIr, natElim string, elimArities map[string]int, multiArgCtors map[string]bool) bool {
	switch x := t.(type) {
	case CBounce:
		// Condition 1: partial/trampoline tail call. Ownership across CBounce is
		// deferred to 6b-2 (the pass has no model for the driver-loop reuse pattern).
		return true

	case AppClosure:
		// Conditions 2, 3, 4: inspect the spine rooted at this AppClosure.
		if _, ok := NatElimSpine(natElim, x); ok {
			// Condition 2: builtin-nat-fold. The frozen emitNatFold borrows its loop
			// counter via non-retaining rt_apply and leaks bignum temporaries.
			return true
		}
		if head, depth := perceusSpineInfo(x); head != "" {
			if req, isElim := elimArities[head]; isElim && depth >= req {
				// Condition 3: inline saturated general-eliminator application.
				// The dead-motive carve-out leaks +1/run; bignum-literal scrutinees
				// add rt_big_parse temps. Only a fully-saturated spine is flagged
				// (a partially-applied def cached at top level is not an inline spine).
				return true
			}
			if multiArgCtors[head] && depth >= 2 {
				// Condition 4: inline arity>=2 constructor application. The
				// recognize-then-skip rule keeps the K_CLO_mk1 intermediate BARE;
				// releasing it would UAF a field the K_CON just moved in. So each
				// inline multi-arg ctor application leaks the intermediate +1/run.
				return true
			}
		}
		// Recurse into sub-terms. The Clo chain is the spine backbone; scanning it
		// also re-checks each sub-spine at one fewer level of depth (harmless, since
		// inner-depth checks are strictly weaker than the outer-depth check already
		// performed). The Arg may contain independent constructs.
		return cirUnbalanceable(x.Clo, natElim, elimArities, multiArgCtors) ||
			cirUnbalanceable(x.Arg, natElim, elimArities, multiArgCtors)

	case CLet:
		return cirUnbalanceable(x.Val, natElim, elimArities, multiArgCtors) ||
			cirUnbalanceable(x.Body, natElim, elimArities, multiArgCtors)

	case MkClosure:
		// Code is a lifted code block scanned separately; scan only Env (evaluated
		// in the current scope, not the closed block scope).
		for _, e := range x.Env {
			if cirUnbalanceable(e, natElim, elimArities, multiArgCtors) {
				return true
			}
		}
		return false

	case CPair:
		return cirUnbalanceable(x.A, natElim, elimArities, multiArgCtors) ||
			cirUnbalanceable(x.B, natElim, elimArities, multiArgCtors)
	case CFst:
		return cirUnbalanceable(x.P, natElim, elimArities, multiArgCtors)
	case CSnd:
		return cirUnbalanceable(x.P, natElim, elimArities, multiArgCtors)
	case CField:
		return cirUnbalanceable(x.Scrut, natElim, elimArities, multiArgCtors)

	case CCase:
		if cirUnbalanceable(x.Scrut, natElim, elimArities, multiArgCtors) {
			return true
		}
		for _, a := range x.Arms {
			if cirUnbalanceable(a.Body, natElim, elimArities, multiArgCtors) {
				return true
			}
		}
		return false

	case CDup:
		return cirUnbalanceable(x.V, natElim, elimArities, multiArgCtors) ||
			cirUnbalanceable(x.K, natElim, elimArities, multiArgCtors)
	case CDrop:
		return cirUnbalanceable(x.V, natElim, elimArities, multiArgCtors) ||
			cirUnbalanceable(x.K, natElim, elimArities, multiArgCtors)

	default:
		// CVar, CEnv, CGlobal, CForeign, CUnit, CLit: leaves. None trigger any
		// exclusion condition on their own.
		return false
	}
}

// perceusSpineInfo walks an AppClosure spine down its Clo chain to its base and
// returns the CGlobal name of the head (empty string if the base is not a CGlobal)
// and the total depth (number of AppClosure levels). Used by cirUnbalanceable to
// check conditions 3 and 4.
func perceusSpineInfo(x AppClosure) (head string, depth int) {
	var t CIr = x
	for {
		app, ok := t.(AppClosure)
		if !ok {
			break
		}
		depth++
		t = app.Clo
	}
	if g, ok := t.(CGlobal); ok {
		head = g.Name
	}
	return
}
