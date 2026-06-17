package core

// Phase 1: quotation, the second half of NbE. Quote reads a value back into a
// term at a de Bruijn level. It does NOT force glued unfoldings: a neutral quotes
// as its un-unfolded spine, so normal forms keep references the user wrote and
// errors print near source. QuoteUnfold is the δ-normalizing variant.

// RestrictIv is value face-restriction along a single atomic interval constraint
// (FACE-RESTRICTED-EVAL-DESIGN.md step 2): it returns v with the free interval
// variable at level `atLevel` substituted by the endpoint value `endpoint`
// (i0/i1), re-normalized. Implemented as Quote ▸ re-Eval with that one
// environment slot bound to the endpoint and the rest to fresh variables — so
// the substitution semantics, and the ι-rules it triggers (ieq0 i0 ~> ftop, the
// El(Glue A ⊤ T e) ~> El(T htop) boundary, …), are inherited from the trusted
// NbE Quote/Eval rather than a new value action. `lvl` is the number of free
// variables in scope (0 ≤ atLevel < lvl). Additive: nothing in the kernel calls
// this yet; it is the contained foundation the proper-face Glue Kan fixup will
// consume, verified in isolation first.
func (m *Machine) RestrictIv(lvl, atLevel int, endpoint, v Val) Val {
	t := m.Quote(lvl, v)
	env := make(Env, lvl)
	for idx := 0; idx < lvl; idx++ {
		level := lvl - 1 - idx
		if level == atLevel {
			env[idx] = endpoint
		} else {
			env[idx] = VVar(level)
		}
	}
	return m.Eval(env, t)
}

// Quote reads v back into a term. lvl is the number of binders in scope: a free
// variable at level l quotes to index lvl-1-l.
func (m *Machine) Quote(lvl int, v Val) Tm {
	switch x := v.(type) {
	case VU:
		return Univ{Lvl: x.Lvl}
	case VProp:
		return Prop{}
	case VEq:
		return Eq{Ty: m.Quote(lvl, x.Ty), L: m.Quote(lvl, x.L), R: m.Quote(lvl, x.R)}
	case VRefl:
		return Refl{Tm: m.Quote(lvl, x.V)}
	case VPi:
		dom := m.Quote(lvl, x.Dom)
		body := m.Quote(lvl+1, x.Cod(VVar(lvl)))
		return Pi{Icit: x.Icit, Qty: x.Qty, Dom: dom, Cod: Scope{Name: x.Name, Body: body}}
	case VLam:
		body := m.Quote(lvl+1, x.Body(VVar(lvl)))
		return Lam{Icit: x.Icit, Qty: x.Qty, Body: Scope{Name: x.Name, Body: body}}
	case VSig:
		dom := m.Quote(lvl, x.Dom)
		body := m.Quote(lvl+1, x.Cod(VVar(lvl)))
		return Sig{Qty: x.Qty, Dom: dom, Cod: Scope{Name: x.Name, Body: body}}
	case VPair:
		body := m.Quote(lvl+1, x.Cod(VVar(lvl)))
		return Pair{Dom: m.Quote(lvl, x.Dom), Cod: Scope{Name: x.Name, Body: body},
			A: m.Quote(lvl, x.A), B: m.Quote(lvl, x.B)}
	case VNeu:
		return m.quoteSpine(lvl, x.Spine, m.Quote)
	default:
		panic("core.Quote: unknown Val constructor")
	}
}

// QuoteUnfold is Quote after forcing every glued unfolding: full βδ-normal form.
// Each forced definition is logged, like any force. It terminates on the acyclic
// stores Phase 1 admits (recursive definitions arrive with Phase-4 totality).
func (m *Machine) QuoteUnfold(lvl int, v Val) Tm {
	// C7 / R-NUM QuoteUnfold short-circuit: a CANONICAL compressed numeral
	// (a VNeu whose head spine is an NNatLit) quotes back to a compact NatLit
	// WITHOUT forcing. Forcing would peel one succ layer to `succ (NatLit (n-1))`
	// and the recursive QuoteUnfold on the predecessor would peel again, n deep —
	// re-materialising the very succ^n chain the literal exists to avoid (the
	// stack blowup on a large closed result, e.g. NormalizeUnfold of `mul 4096
	// 4096`). The literal is already a δ-normal canonical form; keeping it compact
	// is sound (NatLit n ≡ succ^n zero) and is QuoteUnfold's correct output here.
	// QuoteUnfold's legitimate generalization (unfolding a GENERALIZED `builtin
	// nat` binding, where succ/zero are remapped to defs WITH bodies — e.g.
	// `builtin nat Nat one double`) is untouched: there the literal's Succ is NOT
	// a rigid constructor, so the short-circuit declines and the value forces
	// below, peeling the numeral through the generalized chain (the established
	// generalized-binding behavior). The short-circuit fires ONLY when Succ is a
	// rigid head — a genuine canonical literal of the ordinary data constructors.
	//
	// forceToNatLit forces glued layers (e.g. a `+`/`mul` application that REDUCES
	// to a literal) but STOPS the instant the head spine is a canonical NNatLit —
	// it does not peel the literal's own one-layer Unfold. Plain Force would over-
	// peel (NatLit n -> succ (NatLit (n-1))), defeating the compact form and, for
	// a large n, recursing n deep through QuoteUnfold (the stack blowup).
	if lit, ok := m.forceToNatLit(v); ok {
		return NatLit{N: lit.N, Zero: lit.Zero, Succ: lit.Succ}
	}
	v = m.Force(v)
	switch x := v.(type) {
	case VU:
		return Univ{Lvl: x.Lvl}
	case VProp:
		return Prop{}
	case VEq:
		return Eq{Ty: m.QuoteUnfold(lvl, x.Ty), L: m.QuoteUnfold(lvl, x.L), R: m.QuoteUnfold(lvl, x.R)}
	case VRefl:
		return Refl{Tm: m.QuoteUnfold(lvl, x.V)}
	case VPi:
		dom := m.QuoteUnfold(lvl, x.Dom)
		body := m.QuoteUnfold(lvl+1, x.Cod(VVar(lvl)))
		return Pi{Icit: x.Icit, Qty: x.Qty, Dom: dom, Cod: Scope{Name: x.Name, Body: body}}
	case VLam:
		body := m.QuoteUnfold(lvl+1, x.Body(VVar(lvl)))
		return Lam{Icit: x.Icit, Qty: x.Qty, Body: Scope{Name: x.Name, Body: body}}
	case VSig:
		dom := m.QuoteUnfold(lvl, x.Dom)
		body := m.QuoteUnfold(lvl+1, x.Cod(VVar(lvl)))
		return Sig{Qty: x.Qty, Dom: dom, Cod: Scope{Name: x.Name, Body: body}}
	case VPair:
		body := m.QuoteUnfold(lvl+1, x.Cod(VVar(lvl)))
		return Pair{Dom: m.QuoteUnfold(lvl, x.Dom), Cod: Scope{Name: x.Name, Body: body},
			A: m.QuoteUnfold(lvl, x.A), B: m.QuoteUnfold(lvl, x.B)}
	case VNeu:
		return m.quoteSpine(lvl, x.Spine, m.QuoteUnfold)
	default:
		panic("core.QuoteUnfold: unknown Val constructor")
	}
}

// quoteSpine reads a neutral spine back into a term, quoting argument values with
// quoteVal (Quote or QuoteUnfold, so the two modes stay in sync).
func (m *Machine) quoteSpine(lvl int, n Neutral, quoteVal func(int, Val) Tm) Tm {
	switch s := n.(type) {
	case NVar:
		return Var{Idx: lvl - 1 - s.Lvl}
	case NRef:
		return Ref{Hash: s.Hash}
	case NMeta:
		return Meta{ID: s.ID}
	case NCast:
		return Cast{A: quoteVal(lvl, s.A), B: quoteVal(lvl, s.B),
			P: quoteVal(lvl, s.P), X: quoteVal(lvl, s.X)}
	case NSubst:
		return Subst{A: quoteVal(lvl, s.A), X: quoteVal(lvl, s.X), Y: quoteVal(lvl, s.Y),
			Prf: quoteVal(lvl, s.Prf), P: quoteVal(lvl, s.P), Px: quoteVal(lvl, s.Px)}
	case NApp:
		return App{Fn: m.quoteSpine(lvl, s.Fn, quoteVal), Arg: quoteVal(lvl, s.Arg), Icit: s.Icit}
	case NFst:
		return Fst{P: m.quoteSpine(lvl, s.P, quoteVal)}
	case NSnd:
		return Snd{P: m.quoteSpine(lvl, s.P, quoteVal)}
	case NNatLit:
		// A compressed numeral quotes back to a NatLit term (not its peeled
		// chain): Quote keeps the un-forced spine, so a literal stays a literal.
		// Under QuoteUnfold the spine is reached only when the value was NOT
		// forced (Force unfolds NNatLit one layer); a top-level NormalizeUnfold
		// of a literal therefore folds it through the unary chain. Quoting the
		// spine itself preserves the compact form.
		return NatLit{N: s.N, Zero: s.Zero, Succ: s.Succ}
	default:
		panic("core.Quote: unknown Neutral constructor")
	}
}

// Normalize is quote ∘ eval on a closed term: β-normal, references kept folded.
func (m *Machine) Normalize(t Tm) Tm {
	return m.Quote(0, m.Eval(nil, t))
}

// NormalizeUnfold is Normalize with full δ-unfolding of definition references.
func (m *Machine) NormalizeUnfold(t Tm) Tm {
	return m.QuoteUnfold(0, m.Eval(nil, t))
}
