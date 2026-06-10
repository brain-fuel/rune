package core

// Phase 1: quotation, the second half of NbE. Quote reads a value back into a
// term at a de Bruijn level. It does NOT force glued unfoldings: a neutral quotes
// as its un-unfolded spine, so normal forms keep references the user wrote and
// errors print near source. QuoteUnfold is the δ-normalizing variant.

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
