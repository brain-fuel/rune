package codegen

import "fmt"

// closure.go — CLOSURE CONVERSION / lambda lifting over the erased IR.
//
// THE M4 NATIVE-BACKEND PREREQUISITE. The six source-emitter backends
// (js/py/go/rust/jvm/beam) realize an `ILam` as a HOST closure — the host
// runtime allocates the environment record and captures the surrounding scope
// for free (JS arrow, Erlang fun, Rust `Rc<dyn Fn>`, …). Native backends
// (LLVM / Cranelift) have no host closures and no host GC: a function is a bare
// code pointer with no implicit captured environment, so EVERY captured variable
// must be made EXPLICIT — a closure becomes a {code pointer, captured-env record}
// pair, and application becomes "load the code pointer, prepend the env, call."
//
// This pass performs exactly that lowering, entirely within the throwaway shadow
// IR (THE SHADOW RULE, CLAUDE.md): it consumes a `Program` of de-Bruijn IR and
// produces a `ClosureProgram` whose lambdas are LIFTED to top-level `CodeBlock`s
// taking an explicit environment parameter, and whose closures/applications are
// the explicit `MkClosure`/`AppClosure` nodes a native backend can lower to an
// allocation + an indirect call. It is SEMANTICS-PRESERVING and OPT-IN: the
// existing backends never see these nodes (they emit the un-converted IR), so no
// existing output changes. A native backend is the consumer that will walk a
// `ClosureProgram` (see ref_docs/wootz/R-NATIVE.md for the staged plan).
//
// The conversion follows the textbook closure-conversion recipe (Appel,
// "Compiling with Continuations"; Minamide–Morrisett–Harper, "Typed Closure
// Conversion", POPL'96): compute the free variables of each lambda, build an
// environment record from them, and rewrite the body so free-variable references
// read from that record. Because the IR is de Bruijn, the analysis is index
// arithmetic — no name capture, no freshening.

// CIr is a closure-converted IR term. Like Ir it is a sealed sum, but a CIr
// contains NO bare ILam: every function has been lifted, so the only function
// value is a MkClosure (a code pointer paired with its captured environment) and
// the only call is an AppClosure. This is the IR a native backend consumes.
type CIr interface {
	isCIr()
}

// CVar is a LOCAL variable of a lifted code block, addressed by a de Bruijn
// index into the block's OWN binders (the env parameter + the code block's
// argument). A code block has exactly two binders, innermost-first:
//
//	index 0 = the code block's argument   (the lambda's original parameter)
//	index 1 = the environment record      (the captured free variables)
//
// So within a converted lambda body the original parameter is CVar{0} and a
// captured free variable is read out of the environment via CEnv, never CVar.
type CVar struct {
	Idx int
}

// CEnv projects the captured-environment record of the ENCLOSING code block by
// 0-based slot. Slot k holds the k-th free variable the lambda captured, in the
// order ClosureConvert discovered them (ascending original de Bruijn index, made
// deterministic so a backend's layout is stable).
type CEnv struct {
	Idx int
}

// CGlobal is a reference to a top-level definition / constructor / eliminator,
// by emitted name — the same role as IGlobal, unaffected by conversion.
type CGlobal struct {
	Name string
}

// CForeign is the host-linked foreign accessor (R-FFI), carried through.
type CForeign struct {
	Name string
}

// CUnit is the erased token, carried through.
type CUnit struct{}

// CLit is a native host literal (the marshalling primitive), carried through
// unchanged — literals never capture.
type CLit struct {
	Kind LitKind
	Int  int64
	Str  string
	Nat  string
}

// MkClosure is the EXPLICIT closure: a code pointer (the name of a lifted
// CodeBlock) paired with the captured-environment record. A native backend
// allocates a heap object {Code, Env...} here; a source backend that wanted to
// validate the conversion rebuilds a host closure that, when applied, calls the
// code block with (arg, env). Env[k] is the term captured into env slot k.
type MkClosure struct {
	Code string
	Env  []CIr
}

// AppClosure applies a closure value to one argument. A native backend reads the
// closure's code pointer and env, then calls code(arg, env). Curried multi-arg
// functions are nested closures, exactly as the source IR curries ILam.
type AppClosure struct {
	Clo CIr
	Arg CIr
}

// CLet is a local binding, carried through (it introduces a binder, so indices
// shift — handled in conversion).
type CLet struct {
	Name string
	Val  CIr
	Body CIr
}

// CPair / CFst / CSnd carry the dependent-pair erasure through unchanged.
type CPair struct{ A, B CIr }
type CFst struct{ P CIr }
type CSnd struct{ P CIr }

// CField / CCase / CCaseArm carry the eliminator's tag dispatch through. Arms
// reference the scrutinee via CField (no binders) exactly as ICase does, so an
// arm body shares the enclosing context and conversion needs no index surgery
// across an arm boundary.
type CField struct {
	Scrut CIr
	Index int
}
type CCase struct {
	Scrut CIr
	Arms  []CCaseArm
}
type CCaseArm struct {
	Tag  int
	Body CIr
}

func (CVar) isCIr()       {}
func (CEnv) isCIr()       {}
func (CGlobal) isCIr()    {}
func (CForeign) isCIr()   {}
func (CUnit) isCIr()      {}
func (CLit) isCIr()       {}
func (MkClosure) isCIr()  {}
func (AppClosure) isCIr() {}
func (CLet) isCIr()       {}
func (CPair) isCIr()      {}
func (CFst) isCIr()       {}
func (CSnd) isCIr()       {}
func (CField) isCIr()     {}
func (CCase) isCIr()      {}

// CodeBlock is a lifted lambda body: a top-level code block with exactly two
// binders (the argument, then the environment record; see CVar). Its Body refers
// to the argument as CVar{0}, to the env record's slots via CEnv, and to nothing
// else from the enclosing scope (closure conversion's invariant: a code block is
// CLOSED but for its two binders). Captures records, for documentation and tests,
// the original de Bruijn indices (relative to the lambda's enclosing context)
// that populate the env slots in order.
type CodeBlock struct {
	Name     string
	Body     CIr
	Captures []int
}

// ClosureProgram is a closure-converted whole program: the lifted code blocks
// plus the converted top-level definitions (whose bodies are closed CIr — every
// lambda is now a MkClosure referencing a CodeBlock). The data/nat/main metadata
// is carried over verbatim; a native backend reads it the same way the source
// backends read Program.
type ClosureProgram struct {
	Datas  []DataSpec
	Blocks []CodeBlock
	Defs   []CDefSpec
	Nat    *NatSpec
	Main   string
	IOMain bool
}

// CDefSpec is a converted top-level definition.
type CDefSpec struct {
	Name string
	Body CIr
}

// NatElimSpine unwinds a closure-converted application spine and, if its head is
// the builtin-nat eliminator `natElim` saturated to at least 4 arguments
// (m, c0, c1, x, [extra...]), returns those arguments. This mirrors the source
// backends' nat-dispatch recognition (js.go natDispatch) for the NATIVE backends,
// which consume the closure-converted CIr rather than the raw Ir.
func NatElimSpine(natElim string, app AppClosure) ([]CIr, bool) {
	if natElim == "" {
		return nil, false
	}
	var args []CIr
	var t CIr = app
	for {
		a, ok := t.(AppClosure)
		if !ok {
			break
		}
		args = append([]CIr{a.Arg}, args...)
		t = a.Clo
	}
	g, ok := t.(CGlobal)
	if !ok || g.Name != natElim || len(args) < 4 {
		return nil, false
	}
	return args, true
}

// StepIgnoresIH reports whether a NatElim successor step `c1` provably ignores its
// induction hypothesis. After closure conversion `c1 = fn (k)(ihk) is BODY` is an
// outer MkClosure whose code block's body is the inner MkClosure (`fn (ihk) is
// BODY`); the IH is the inner block's ARGUMENT (CVar index 0). The step ignores the
// IH exactly when that argument never occurs in the inner block's body. Such an
// eliminator is a constant-time CASE on the nat (zero -> c0, succ k -> c1 k _), so
// the native backends can emit a one-peel dispatch instead of the eager fold —
// turning the super-exponential `beqNat`-shape into linear (matches js/go/rust).
func StepIgnoresIH(blocks map[string]CodeBlock, c1 CIr) bool {
	outerMk, ok := c1.(MkClosure)
	if !ok {
		return false
	}
	outer, ok := blocks[outerMk.Code]
	if !ok {
		return false
	}
	innerMk, ok := outer.Body.(MkClosure)
	if !ok {
		return false
	}
	inner, ok := blocks[innerMk.Code]
	if !ok {
		return false
	}
	return !cirUsesArg(inner.Body, 0)
}

// cirUsesArg reports whether the de Bruijn index idx is referenced in t, scanning
// WITHIN a single code block. A MkClosure's code block is a separate closed scope
// (closure conversion's invariant), so its body is not followed; only its Env
// captures — terms evaluated in the current scope — are scanned. CLet introduces a
// binder (idx shifts); CCase arms share the enclosing context (no shift).
func cirUsesArg(t CIr, idx int) bool {
	switch x := t.(type) {
	case CVar:
		return x.Idx == idx
	case MkClosure:
		for _, e := range x.Env {
			if cirUsesArg(e, idx) {
				return true
			}
		}
		return false
	case AppClosure:
		return cirUsesArg(x.Clo, idx) || cirUsesArg(x.Arg, idx)
	case CLet:
		return cirUsesArg(x.Val, idx) || cirUsesArg(x.Body, idx+1)
	case CPair:
		return cirUsesArg(x.A, idx) || cirUsesArg(x.B, idx)
	case CFst:
		return cirUsesArg(x.P, idx)
	case CSnd:
		return cirUsesArg(x.P, idx)
	case CField:
		return cirUsesArg(x.Scrut, idx)
	case CCase:
		if cirUsesArg(x.Scrut, idx) {
			return true
		}
		for _, a := range x.Arms {
			if cirUsesArg(a.Body, idx) {
				return true
			}
		}
		return false
	default:
		return false
	}
}

// freeVars returns the de Bruijn indices that occur FREE in an Ir term, as seen
// from the term's own top (i.e. an index that escapes all the binders inside the
// term). The result is the ascending, de-duplicated set — the canonical capture
// order. `depth` threads the number of binders entered so far; an IVar{i} with
// i >= depth is free, contributing the index i-depth in the term's frame.
func freeVars(t Ir) []int {
	seen := map[int]bool{}
	var walk func(Ir, int)
	add := func(i int) {
		if !seen[i] {
			seen[i] = true
		}
	}
	walk = func(t Ir, depth int) {
		switch x := t.(type) {
		case IVar:
			if x.Idx >= depth {
				add(x.Idx - depth)
			}
		case ILam:
			walk(x.Body, depth+1)
		case IApp:
			walk(x.Fn, depth)
			walk(x.Arg, depth)
		case ILet:
			walk(x.Val, depth)
			walk(x.Body, depth+1)
		case IPair:
			walk(x.A, depth)
			walk(x.B, depth)
		case IFst:
			walk(x.P, depth)
		case ISnd:
			walk(x.P, depth)
		case IField:
			walk(x.Scrut, depth)
		case IBounce:
			walk(x.Call, depth)
		case ICase:
			walk(x.Scrut, depth)
			for _, arm := range x.Arms {
				walk(arm.Body, depth)
			}
		case IGlobal, IForeign, IUnit, ILit:
			// no variables
		default:
			panic(fmt.Sprintf("codegen.freeVars: unknown IR node %T", t))
		}
	}
	walk(t, 0)
	// Deterministic ascending order — the env layout a backend can rely on.
	out := make([]int, 0, len(seen))
	for i := 0; i <= maxKey(seen); i++ {
		if seen[i] {
			out = append(out, i)
		}
	}
	return out
}

func maxKey(m map[int]bool) int {
	max := -1
	for k := range m {
		if k > max {
			max = k
		}
	}
	return max
}

// closureConverter carries the lifted code blocks accumulated during a pass and
// a counter for fresh code-block names.
type closureConverter struct {
	blocks  []CodeBlock
	counter int
}

// ClosureConvert lowers a Program to a ClosureProgram: it lifts every lambda
// (including the lambdas LowerElim produces for eliminators) to a top-level code
// block taking an explicit environment, and rewrites the program so closures and
// applications are the explicit MkClosure / AppClosure nodes. SEMANTICS-
// PRESERVING (verified by closure_test.go against a direct evaluator on the
// conformance corpus). The Datas/Nat/Main/IOMain metadata passes through.
func ClosureConvert(p Program) ClosureProgram {
	cc := &closureConverter{}
	out := ClosureProgram{
		Datas:  p.Datas,
		Nat:    p.Nat,
		Main:   p.Main,
		IOMain: p.IOMain,
	}
	// Lower each datatype eliminator the same way the source backends do, then
	// convert it — a native backend gets the eliminator as a closed code block.
	for _, d := range p.Datas {
		if p.Nat != nil && d.ElimName == p.Nat.ElimName {
			continue // the nat group is a backend primitive, not a lifted lambda
		}
		body := cc.convert(LowerElim(d), 0)
		out.Defs = append(out.Defs, CDefSpec{Name: d.ElimName, Body: body})
	}
	for _, def := range p.Defs {
		out.Defs = append(out.Defs, CDefSpec{Name: def.Name, Body: cc.convert(def.Body, 0)})
	}
	out.Blocks = cc.blocks
	return out
}

// convert rewrites an Ir term in a context of `depth` enclosing binders (binders
// the SURROUNDING converted code still keeps — CLet binders and, at the top of a
// lifted code block, the block's two binders). A bound IVar{i} with i < depth is
// a genuine local (a CLet binder or the code block's argument); ILam is the case
// that lifts.
func (cc *closureConverter) convert(t Ir, depth int) CIr {
	switch x := t.(type) {
	case IVar:
		return CVar{Idx: x.Idx}
	case IGlobal:
		return CGlobal{Name: x.Name}
	case IForeign:
		return CForeign{Name: x.Name}
	case IUnit:
		return CUnit{}
	case ILit:
		return CLit{Kind: x.Kind, Int: x.Int, Str: x.Str, Nat: x.Nat}
	case ILam:
		return cc.lift(x, depth)
	case IApp:
		return AppClosure{Clo: cc.convert(x.Fn, depth), Arg: cc.convert(x.Arg, depth)}
	case ILet:
		return CLet{
			Name: x.Name,
			Val:  cc.convert(x.Val, depth),
			Body: cc.convert(x.Body, depth+1),
		}
	case IPair:
		return CPair{A: cc.convert(x.A, depth), B: cc.convert(x.B, depth)}
	case IFst:
		return CFst{P: cc.convert(x.P, depth)}
	case ISnd:
		return CSnd{P: cc.convert(x.P, depth)}
	case IField:
		return CField{Scrut: cc.convert(x.Scrut, depth), Index: x.Index}
	case IBounce:
		return cc.convert(x.Call, depth)
	case ICase:
		arms := make([]CCaseArm, len(x.Arms))
		for i, arm := range x.Arms {
			arms[i] = CCaseArm{Tag: arm.Tag, Body: cc.convert(arm.Body, depth)}
		}
		return CCase{Scrut: cc.convert(x.Scrut, depth), Arms: arms}
	default:
		panic(fmt.Sprintf("codegen.ClosureConvert: unknown IR node %T", t))
	}
}

// lift converts a single ILam into a MkClosure over a freshly lifted CodeBlock.
//
// The lambda's body sees, as de Bruijn index 0, the lambda's PARAMETER, and at
// indices ≥ 1 the surrounding context. We compute the body's free variables (as
// indices into the lambda's own frame, where 0 is the parameter); index 0 (the
// parameter) is the code block's argument, every free index ≥ 1 is a CAPTURE.
//
// In the lifted code block the binders are (argument=CVar{0}, env=CVar{1}); a
// reference to the parameter (original index 0) becomes CVar{0}, and a captured
// variable (original index j ≥ 1, the c-th capture) becomes CEnv{c}. We rewrite
// the body with that index remapping, then convert it (so nested lambdas lift
// recursively) under depth 1 — the code block keeps exactly one local binder
// (the argument) visible to convert; the env is reached only via CEnv, which the
// remap already inserted.
func (cc *closureConverter) lift(lam ILam, outerDepth int) CIr {
	// Free vars of the lambda BODY, in the lambda's own frame (param = index 0,
	// captures at indices ≥ 1). NOTE we walk the body, not the lam: freeVars(lam)
	// would strip the lambda's own binder and report captures relative to the
	// ENCLOSING context, conflating the parameter with the first capture.
	fvs := freeVars(lam.Body)
	// Captures are the free vars OTHER than the parameter (index 0). Each
	// capture j (an index into the lambda's frame) corresponds to outer term
	// index j-1 (drop the lambda's binder) — that is the term we store in the
	// env slot, evaluated in the enclosing converted context.
	var captures []int // env slot -> index in the lambda's body frame (≥1)
	for _, j := range fvs {
		if j >= 1 {
			captures = append(captures, j)
		}
	}
	slotOf := map[int]int{} // body-frame index -> env slot
	for slot, j := range captures {
		slotOf[j] = slot
	}
	name := fmt.Sprintf("$code%d", cc.counter)
	cc.counter++
	// Reserve the slot so recursion through cc.lift for nested lambdas gets a
	// distinct counter; append the finished block after building its body.
	body := cc.liftBody(lam.Body, slotOf)
	cc.blocks = append(cc.blocks, CodeBlock{Name: name, Body: body, Captures: captures})

	// The MkClosure's env terms are the captured outer terms, evaluated in the
	// enclosing context: capture j (lambda frame) = IVar{j-1} (outer frame),
	// converted at outerDepth.
	env := make([]CIr, len(captures))
	for slot, j := range captures {
		env[slot] = cc.convert(IVar{Idx: j - 1}, outerDepth)
	}
	return MkClosure{Code: name, Env: env}
}

// liftBody converts the BODY of a lifted lambda. Inside the code block exactly
// one local binder is visible (the argument, CVar{0}); the captured variables
// are reached via CEnv using slotOf, keyed by the variable's index in the
// lambda's frame. `extra` (threaded as depth) counts binders entered WITHIN the
// body (CLet, and nested-lambda boundaries are handled by recursion into lift).
//
// We re-implement the convert walk here because the index semantics differ: a
// free IVar of the body addresses the lambda's frame and must route to CVar{0}
// (the parameter) or CEnv{slot} (a capture); a bound IVar (entered a CLet here)
// is a genuine CVar. depth0 separates the two: an index < depth0 is a local
// binder of the body, an index ≥ depth0 addresses the lambda frame at
// (index-depth0).
func (cc *closureConverter) liftBody(t Ir, slotOf map[int]int) CIr {
	var walk func(Ir, int) CIr
	walk = func(t Ir, depth int) CIr {
		switch x := t.(type) {
		case IVar:
			if x.Idx < depth {
				// A binder introduced inside the body (e.g. a CLet) — a real local.
				return CVar{Idx: x.Idx}
			}
			frame := x.Idx - depth // index into the lambda's frame
			if frame == 0 {
				// The lambda's parameter: CVar{0} shifted by the local binders.
				return CVar{Idx: depth}
			}
			slot, ok := slotOf[frame]
			if !ok {
				panic(fmt.Sprintf("codegen.liftBody: free index %d has no env slot", frame))
			}
			return CEnv{Idx: slot}
		case IGlobal:
			return CGlobal{Name: x.Name}
		case IForeign:
			return CForeign{Name: x.Name}
		case IUnit:
			return CUnit{}
		case ILit:
			return CLit{Kind: x.Kind, Int: x.Int, Str: x.Str, Nat: x.Nat}
		case ILam:
			// A nested lambda. Lift it through the SAME converter, but its capture
			// terms are expressed in the current body's frame, so we cannot reuse
			// cc.lift directly (which assumes the outer term frame). Instead we
			// reconstruct the nested lambda's converted closure here: compute its
			// free vars, route each to a CVar/CEnv of THIS body, and emit a fresh
			// code block whose body routes through the nested lambda's own slots.
			return cc.liftNested(x, slotOf, depth)
		case IApp:
			return AppClosure{Clo: walk(x.Fn, depth), Arg: walk(x.Arg, depth)}
		case ILet:
			return CLet{Name: x.Name, Val: walk(x.Val, depth), Body: walk(x.Body, depth+1)}
		case IPair:
			return CPair{A: walk(x.A, depth), B: walk(x.B, depth)}
		case IFst:
			return CFst{P: walk(x.P, depth)}
		case ISnd:
			return CSnd{P: walk(x.P, depth)}
		case IField:
			return CField{Scrut: walk(x.Scrut, depth), Index: x.Index}
		case IBounce:
			return walk(x.Call, depth)
		case ICase:
			arms := make([]CCaseArm, len(x.Arms))
			for i, arm := range x.Arms {
				arms[i] = CCaseArm{Tag: arm.Tag, Body: walk(arm.Body, depth)}
			}
			return CCase{Scrut: walk(x.Scrut, depth), Arms: arms}
		default:
			panic(fmt.Sprintf("codegen.liftBody: unknown IR node %T", t))
		}
	}
	return walk(t, 0)
}

// liftNested lifts a lambda that appears INSIDE another lifted code block's body.
// The nested lambda's free variables address the ENCLOSING body's frame (param =
// the nested lambda's own param at index 0, captures at ≥1 into the enclosing
// body where index `depth` is the enclosing parameter slot etc.). Its env terms
// must therefore be emitted as CIr in the enclosing body (routing each through
// CVar/CEnv), which is exactly liftBody's walk on an IVar. We build those env
// terms by reusing the enclosing slotOf + depth, and build the nested code
// block's body by recursing liftBody with the nested lambda's own slot map.
func (cc *closureConverter) liftNested(lam ILam, outerSlotOf map[int]int, outerDepth int) CIr {
	fvs := freeVars(lam.Body)
	var captures []int
	for _, j := range fvs {
		if j >= 1 {
			captures = append(captures, j)
		}
	}
	slotOf := map[int]int{}
	for slot, j := range captures {
		slotOf[j] = slot
	}
	name := fmt.Sprintf("$code%d", cc.counter)
	cc.counter++
	body := cc.liftBody(lam.Body, slotOf)
	cc.blocks = append(cc.blocks, CodeBlock{Name: name, Body: body, Captures: captures})

	// Each capture j (nested-lambda frame) is the enclosing term IVar{j-1}; emit
	// it as a CIr in the enclosing body via the enclosing slotOf/depth.
	env := make([]CIr, len(captures))
	enc := func(idx int) CIr {
		// Replicate liftBody's IVar routing for the enclosing body.
		if idx < outerDepth {
			return CVar{Idx: idx}
		}
		frame := idx - outerDepth
		if frame == 0 {
			return CVar{Idx: outerDepth}
		}
		slot, ok := outerSlotOf[frame]
		if !ok {
			panic(fmt.Sprintf("codegen.liftNested: enclosing free index %d has no env slot", frame))
		}
		return CEnv{Idx: slot}
	}
	for slot, j := range captures {
		env[slot] = enc(j - 1)
	}
	return MkClosure{Code: name, Env: env}
}
