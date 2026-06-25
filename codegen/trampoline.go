package codegen

// The trampoline pass. Deep tail (and, at stage 4, mutual-tail) recursion in
// `partial` defs blows the host call stack on the backends without TCO (the JVM
// especially). MarkTailBounces rewrites each partial def's body, wrapping a
// SATURATED tail-position call to a member of its recursion group as an IBounce —
// a 0-arg thunk the def's driver loop forces, flattening the recursion onto the
// heap (Clojure core/trampoline). Every stack-limited backend renders IBounce as a
// bounce value (js/py/go/rs/jvm + native c/ll); BEAM (native TCO) emits the wrapped
// call directly, so it is a byte-identical no-op.
//
// The recursion in real rune code threads through a NatElim ONE-PEEL: a partial's
// self-call sits in the succ-arm of a `case n of zero -> … | succ k -> P k`, which
// erases to `NatElim motive c0 (λk ih. P k) n` whose step IGNORES its IH. That step
// body is the genuine tail continuation, so the analysis descends into a one-peel
// eliminator's arms when the eliminator stands in tail position. A step that USES
// its IH is a real fold (the self-call is NOT tail) and is left alone — so the IH
// gate is load-bearing for correctness.
//
// v1 marks SELF-recursion only (group = {def}); the mutual SCC is stage 4. The
// analysis is conservative: a call is bounced only when it is in TAIL position AND
// saturated; the default everywhere is inTail=false (a missed tail call only costs
// stack, never correctness; a wrongly-bounced non-tail call would defer a value).

type tailCtx struct {
	group   map[string]bool
	arity   map[string]int
	natElim string // the builtin-nat eliminator's emitted name ("" if none)
}

// MarkTailBounces marks tail self-calls in every partial def. Mutates p.Defs.
func MarkTailBounces(p *Program) {
	if len(p.Partials) == 0 {
		return
	}
	tc := tailCtx{group: nil, arity: make(map[string]int, len(p.Defs))}
	for _, d := range p.Defs {
		tc.arity[d.Name] = d.Arity
	}
	if p.Nat != nil {
		tc.natElim = p.Nat.ElimName
	}
	for i := range p.Defs {
		if !p.Partials[p.Defs[i].Name] {
			continue
		}
		tc.group = map[string]bool{p.Defs[i].Name: true} // self-recursion (v1)
		p.Defs[i].Body = markTailLams(p.Defs[i].Body, tc)
	}
}

// markTailLams peels the leading curried-parameter lambdas of a partial def's body
// (not yet a tail context), then marks the body proper in tail position.
func markTailLams(t Ir, tc tailCtx) Ir {
	if lam, ok := t.(ILam); ok {
		return ILam{Name: lam.Name, Body: markTailLams(lam.Body, tc)}
	}
	return markTail(t, tc, true)
}

// markTail walks t. inTail is true only at the def-body root, an ILet body, each
// ICase arm body, and the arms of a one-peel NatElim that itself stands in tail
// position. A saturated IApp spine onto a group member, in tail position, becomes
// an IBounce.
func markTail(t Ir, tc tailCtx, inTail bool) Ir {
	switch x := t.(type) {
	case ILet:
		return ILet{Name: x.Name,
			Val:  markTail(x.Val, tc, false),
			Body: markTail(x.Body, tc, inTail)}
	case ICase:
		arms := make([]ICaseArm, len(x.Arms))
		for i, a := range x.Arms {
			arms[i] = ICaseArm{Tag: a.Tag, Body: markTail(a.Body, tc, inTail)}
		}
		return ICase{Scrut: markTail(x.Scrut, tc, false), Arms: arms}
	case IApp:
		if inTail {
			// A one-peel NatElim in tail position: its zero-arm and (IH-ignoring)
			// succ-arm body are tail contexts — descend, leaving the scrutinee alone.
			if out, ok := markNatElimOnePeel(x, tc); ok {
				return out
			}
			if name, n, ok := spineHead(x); ok && tc.group[name] && n == tc.arity[name] {
				return IBounce{Call: x} // saturated tail self/sibling call
			}
		}
		return IApp{Fn: markTail(x.Fn, tc, false), Arg: markTail(x.Arg, tc, false)}
	case ILam:
		return ILam{Name: x.Name, Body: markTail(x.Body, tc, false)}
	case IPair:
		return IPair{A: markTail(x.A, tc, false), B: markTail(x.B, tc, false)}
	case IFst:
		return IFst{P: markTail(x.P, tc, false)}
	case ISnd:
		return ISnd{P: markTail(x.P, tc, false)}
	case IField:
		return IField{Scrut: markTail(x.Scrut, tc, false), Index: x.Index}
	case IBounce:
		return IBounce{Call: markTail(x.Call, tc, false)}
	default:
		return t
	}
}

// markNatElimOnePeel recognises `NatElim motive c0 (λk ih. body) scrut` in tail
// position where the succ-step IGNORES its induction hypothesis (a one-peel CASE,
// not a fold). It re-marks c0 and the step's inner body as tail contexts (so a
// self-call there bounces) and leaves motive/scrutinee alone. Returns ok=false for
// any other spine (a real fold, a non-NatElim head, the wrong shape).
func markNatElimOnePeel(app IApp, tc tailCtx) (Ir, bool) {
	if tc.natElim == "" {
		return nil, false
	}
	// Unwind to head + args.
	var args []Ir
	t := Ir(app)
	for {
		a, ok := t.(IApp)
		if !ok {
			break
		}
		args = append([]Ir{a.Arg}, args...)
		t = a.Fn
	}
	g, ok := t.(IGlobal)
	if !ok || g.Name != tc.natElim || len(args) != 4 {
		return nil, false
	}
	motive, c0, c1, scrut := args[0], args[1], args[2], args[3]
	// c1 must be `λk. λih. body` with body NOT using ih (de Bruijn 0 in body).
	l1, ok := c1.(ILam)
	if !ok {
		return nil, false
	}
	l2, ok := l1.Body.(ILam)
	if !ok {
		return nil, false
	}
	if irUsesVar(l2.Body, 0) {
		return nil, false // step uses its IH: a real fold, not a one-peel
	}
	newC1 := ILam{Name: l1.Name, Body: ILam{Name: l2.Name, Body: markTail(l2.Body, tc, true)}}
	newC0 := markTail(c0, tc, true)
	// Rebuild the spine: NatElim motive newC0 newC1 scrut.
	out := Ir(g)
	for _, a := range []Ir{motive, newC0, newC1, scrut} {
		out = IApp{Fn: out, Arg: a}
	}
	return out, true
}

// irUsesVar reports whether de Bruijn variable `idx` (relative to t's binding
// depth) occurs free in t. ILam/ILet bodies introduce a binder (idx+1); ICase arms
// share the enclosing context (fields via IField, no per-arm binder).
func irUsesVar(t Ir, idx int) bool {
	switch x := t.(type) {
	case IVar:
		return x.Idx == idx
	case ILam:
		return irUsesVar(x.Body, idx+1)
	case ILet:
		return irUsesVar(x.Val, idx) || irUsesVar(x.Body, idx+1)
	case IApp:
		return irUsesVar(x.Fn, idx) || irUsesVar(x.Arg, idx)
	case IPair:
		return irUsesVar(x.A, idx) || irUsesVar(x.B, idx)
	case IFst:
		return irUsesVar(x.P, idx)
	case ISnd:
		return irUsesVar(x.P, idx)
	case IField:
		return irUsesVar(x.Scrut, idx)
	case ICase:
		if irUsesVar(x.Scrut, idx) {
			return true
		}
		for _, a := range x.Arms {
			if irUsesVar(a.Body, idx) {
				return true
			}
		}
		return false
	case IBounce:
		return irUsesVar(x.Call, idx)
	default:
		return false
	}
}

// spineHead unwinds an application spine; returns the head global's name and the
// argument count when the head is an IGlobal.
func spineHead(app IApp) (name string, nargs int, ok bool) {
	t := Ir(app)
	for {
		a, isApp := t.(IApp)
		if !isApp {
			break
		}
		nargs++
		t = a.Fn
	}
	if g, isG := t.(IGlobal); isG {
		return g.Name, nargs, true
	}
	return "", 0, false
}

// LeadingLamCount is the arity of an erased body: its count of leading curried
// parameter lambdas (0 for a non-lambda value). The saturation gate reads it.
func LeadingLamCount(t Ir) int {
	n := 0
	for {
		lam, ok := t.(ILam)
		if !ok {
			break
		}
		n++
		t = lam.Body
	}
	return n
}
