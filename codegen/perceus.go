package codegen

import "fmt"

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
// into the principled PATH B model above. Constructor/case nodes (CCase, CPair,
// CField, CFst, CSnd, CBounce) are still carried through annotate unchanged -- Tasks
// 4-5 extend ownership coverage there -- but shiftCIr now recurses through ALL nodes
// (the structural dual of cirUsesArg) so feeding a constructor as a closure-application
// argument keeps de Bruijn indices correct.

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

// perceusPass holds per-pass state. Currently stateless -- the type exists to allow
// future fields (e.g. accel table lookup for smarter CDrop elision) without changing
// signatures.
type perceusPass struct{}

// Perceus inserts CDup/CDrop into all code blocks and top-level defs of the closure
// program. Each code block OWNS its argument (CVar{0}); top-level defs own nothing
// (owned = nil). Returns the annotated program.
func Perceus(p ClosureProgram) ClosureProgram {
	pp := &perceusPass{}
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
		bodyOwned := make([]bool, len(owned)+1)
		bodyOwned[0] = true
		copy(bodyOwned[1:], owned)
		body := pp.ownScope(x.Body, bodyOwned)
		out := CIr(CLet{Name: x.Name, Val: val, Body: body})
		// Capture-then-use-later (and any Val/Body shared owned local): a local
		// consumed in BOTH the bound value and the continuation is consumed twice
		// (e.g. captured into a closure env in Val AND applied in Body), so it needs a
		// dup before the binding. In the body the local sits one binder deeper (i+1).
		for i := len(owned) - 1; i >= 0; i-- {
			if owned[i] && cirUsesArg(x.Val, i) && cirUsesArg(x.Body, i+1) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out

	case AppClosure:
		clo := pp.annotate(x.Clo, owned)
		// The argument is consumed by the callee (which owns its CVar{0}); make it
		// owned so a borrowed arg is dup'd before the call.
		arg := consumeOwning(pp.annotate(x.Arg, owned))

		// Release the closure after the call ONLY when it is an owned local (CVar) or a
		// freshly allocated inline closure (MkClosure). A CGlobal/AppClosure closure
		// (a cached or intermediate K_CLO) is borrowed by rt_apply and NOT released
		// here -- releasing it would cascade-free thunk-cached values. rt_apply itself
		// is never modified (it would free closures at rc=1 in the no-Perceus runtime).
		_, isCloVar := x.Clo.(CVar)
		_, isCloMk := x.Clo.(MkClosure)

		var out CIr
		if isCloVar || isCloMk {
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

	default:
		// CCase/CPair/CField/CFst/CSnd/CBounce: outside the Task 3.5 fragment. Carry
		// through unchanged (Tasks 4-5 extend coverage); consumeOwning still owns a
		// borrowed RESULT of one of these when it reaches an owning position.
		return t
	}
}
