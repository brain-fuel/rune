package codegen

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
// TASK 3: pure-fragment implementation (CVar/CEnv/CGlobal/CUnit/CLit/CLet/
// AppClosure/MkClosure). Constructor/case nodes (CCase, CPair, CField, CFst,
// CSnd, CBounce) are carried through unchanged -- Tasks 4-5 extend coverage.
//
// SCOPE LIMITATION (Task 3): CLet+CDrop is inserted for AppClosure ONLY when
// the closure expression is a CVar (owned local) or MkClosure (freshly allocated
// inline closure). AppClosure results (intermediate partial applications) are NOT
// released here: the callee's code blocks may store arguments without retaining,
// so cascade-releasing an intermediate K_CLO would corrupt thunk-cached values
// such as K_CON(zero). Full release requires thunk retain-on-return (Task 4+).

// shiftCIr shifts all free CVar indices >= minDepth by amount. Used when
// inserting a CLet binder around an AppClosure to keep de Bruijn indices correct
// in the continuation.
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
		return MkClosure{Code: x.Code, Env: env}
	case CDup:
		return CDup{V: shiftCIr(x.V, minDepth, amount), K: shiftCIr(x.K, minDepth, amount)}
	case CDrop:
		return CDrop{V: shiftCIr(x.V, minDepth, amount), K: shiftCIr(x.K, minDepth, amount)}
	default:
		// CCase/CPair/CField/CFst/CSnd/CBounce: outside pure fragment.
		// Carry through unchanged; Tasks 4-5 extend coverage.
		return x
	}
}

// perceusPass holds per-pass state. Currently stateless -- the type exists to
// allow future fields (e.g. accel table lookup for smarter CDrop elision) without
// changing signatures.
type perceusPass struct{}

// Perceus inserts CDup/CDrop into all code blocks and top-level defs of the
// closure program. Each code block OWNS its argument (CVar{0}); top-level defs
// own nothing (owned = nil). Returns the annotated program.
func Perceus(p ClosureProgram) ClosureProgram {
	pp := &perceusPass{}
	for i := range p.Blocks {
		// argCount=1: the one code-block argument is at the outermost owned slot.
		// ownScope skips dead-drop for code-block args to avoid cascade-releasing
		// structural K_CON values that may be shared with thunk caches (Task 4+
		// will fix this by making rt_mkcon/rt_clo_set retain their arguments).
		p.Blocks[i].Body = pp.ownScope(p.Blocks[i].Body, []bool{true}, 1)
	}
	for i := range p.Defs {
		// Defs have no owned code-block argument; argCount=0.
		p.Defs[i].Body = pp.ownScope(p.Defs[i].Body, nil, 0)
	}
	return p
}

// ownScope is the entry point for a lexical scope. owned[i] = true means the
// CVar at de Bruijn index i is owned by the current scope. argCount is the
// number of "code block argument" slots at the HIGH end of the owned slice
// (i.e., indices len(owned)-argCount .. len(owned)-1). These are skipped for
// dead-drop CDrop because the runtime does NOT retain values when storing them
// in K_CON fields or K_CLO env slots, so code-block arguments may alias
// thunk-cached shared values -- cascade-releasing them would corrupt the cache.
// Only CLet-INTRODUCED variables (indices 0 .. len(owned)-argCount-1) are
// safe to dead-drop because they are always freshly computed at their binding
// site in the current analysis (e.g. a MkClosure or AppClosure that produces a
// new heap object). Full dead-drop for args requires Task 4+'s retain-on-store.
//
// The function:
//  1. Annotates t (inserting CDup for multiply-used owned locals and CLet+CDrop
//     for consumed closures).
//  2. Wraps the result with CDrop for each SAFE dead owned local, traversing
//     from the highest to the lowest index so the outermost CDrop corresponds
//     to the innermost binder.
//
// IMPORTANT: annotate runs on the ORIGINAL t (pre-CDup insertion) for the dead
// check, then CDrop wraps the ANNOTATED out. This ordering prevents annotate from
// seeing its own CDrop nodes.
func (pp *perceusPass) ownScope(t CIr, owned []bool, argCount int) CIr {
	out := pp.annotate(t, owned, argCount)
	// skipFrom is the first owned index that belongs to a code-block argument.
	// Indices [skipFrom, len(owned)-1] are not eligible for dead-drop.
	skipFrom := len(owned) - argCount
	// Wrap CDrop for safe dead owned locals. Iterate high-to-low so the generated
	// CDrop chain is: CDrop(i_max, CDrop(i_max-1, ... CDrop(i_min, out)...)).
	for i := len(owned) - 1; i >= 0; i-- {
		if i >= skipFrom {
			continue // code-block arg range: skip dead-drop (may alias cache)
		}
		if owned[i] && !cirUsesArg(t, i) {
			out = CDrop{V: CVar{Idx: i}, K: out}
		}
	}
	return out
}

// annotate traverses t and inserts CDup/CDrop annotations for the PURE FRAGMENT.
// owned is unchanged (we never extend or truncate it here); only ownScope and the
// CLet case create extended slices for recursive calls. argCount is threaded
// through to CLet's ownScope call so the dead-drop guard remains consistent.
func (pp *perceusPass) annotate(t CIr, owned []bool, argCount int) CIr {
	switch x := t.(type) {

	case CVar:
		// A single CVar is a consuming use -- ownership transfers to whoever
		// receives the result. No CDup needed (one use = one transfer). No CDrop
		// either (cirUsesArg is true so ownScope skips the dead-drop).
		return x

	case CEnv, CGlobal, CForeign, CUnit, CLit:
		// Borrowed/global/constant: no dup/drop in the pure fragment.
		return x

	case CLet:
		// CLet introduces a new owned binder at de Bruijn index 0, shifting all
		// existing indices by +1. Annotate the value in the current scope, then
		// open a new scope for the body.
		val := pp.annotate(x.Val, owned, argCount)
		// Body scope: index 0 = the new binder (owned=true), rest = shifted owned.
		bodyOwned := make([]bool, len(owned)+1)
		bodyOwned[0] = true
		copy(bodyOwned[1:], owned)
		// argCount is unchanged: the code-block arguments are now one slot higher
		// in the de Bruijn numbering but still at the high end of bodyOwned.
		body := pp.ownScope(x.Body, bodyOwned, argCount)
		return CLet{Name: x.Name, Val: val, Body: body}

	case AppClosure:
		// Annotate sub-expressions.
		clo := pp.annotate(x.Clo, owned, argCount)
		arg := pp.annotate(x.Arg, owned, argCount)

		// Decide whether to release the closure after the call. We only insert
		// CLet+CDrop when the closure expression is an owned local (CVar) or a
		// freshly allocated inline closure (MkClosure). Intermediate K_CLO objects
		// produced by evaluating AppClosure/CGlobal spines are NOT released here
		// to avoid cascade-freeing thunk-cached constructor values (Task 4+).
		_, isCloVar := x.Clo.(CVar)
		_, isCloMk := x.Clo.(MkClosure)

		var out CIr
		if isCloVar || isCloMk {
			// Release the closure after rt_apply:
			//   CLet("$clo", clo,
			//     CLet("$res", AppClosure(CVar{0}, shiftedArg),
			//       CDrop{CVar{1}, CVar{0}}))
			//
			// The arg must be shifted by 1 because the outer CLet("$clo",...) adds
			// a new binder (CVar{0}=$clo) that displaces existing indices.
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

		// Insert CDup for owned locals that appear in BOTH Clo and Arg. Each
		// such local is consumed twice (once to evaluate Clo, once to evaluate
		// Arg) so we need an extra reference. Check on the ORIGINAL x.Clo/x.Arg
		// (before annotation) for correct cirUsesArg results.
		for i := len(owned) - 1; i >= 0; i-- {
			if owned[i] && cirUsesArg(x.Clo, i) && cirUsesArg(x.Arg, i) {
				out = CDup{V: CVar{Idx: i}, K: out}
			}
		}
		return out

	case MkClosure:
		// Annotate each env term.
		env := make([]CIr, len(x.Env))
		for i, e := range x.Env {
			env[i] = pp.annotate(e, owned, argCount)
		}
		out := CIr(MkClosure{Code: x.Code, Env: env})
		// If an owned local is stored in N env slots, we need N-1 extra CDup's
		// (each slot takes one ownership reference; the last takes the original).
		// Iterate high-to-low to preserve de Bruijn stability.
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
		// CCase/CPair/CField/CFst/CSnd/CBounce: outside the pure fragment.
		// Carry through unchanged (Tasks 4-5 extend coverage).
		return t
	}
}
