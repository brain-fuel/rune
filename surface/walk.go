package surface

import "fmt"

// MapExpNames returns a copy of e with every EVar name and every ECase clause
// constructor head replaced by ren(name), except for occurrences that are bound
// at that position in the tree. Binding forms tracked: ELam (Param scopes over
// Body), EPi (Param scopes over Cod), ELet (Name scopes over Body), ESeqBind
// (Name scopes over Body), ECase clause (each clause's Binders and IHs scope
// over that clause's Body). The Dom/Ty/Val positions and constructor heads are
// never in a binding scope and are always passed through ren unchanged.
// Same exhaustive-with-panic discipline as WalkExp: an unknown Exp type panics
// loudly in tests rather than silently skipping names.
func MapExpNames(e Exp, ren func(string) string) Exp {
	return mapNamesScoped(e, ren, nil)
}

// mapNamesScoped is the internal helper that threads the set of bound names.
// bound maps a name to true when it has been introduced by an enclosing binder
// and must not be passed through ren. The nil map means "nothing bound" and is
// valid to pass directly; boundWith always allocates a new map rather than
// mutating the caller's copy.
func mapNamesScoped(e Exp, ren func(string) string, bound map[string]bool) Exp {
	switch x := e.(type) {
	case EVar:
		if bound[x.Name] {
			return x // bound occurrence: leave unchanged
		}
		return EVar{Name: ren(x.Name)}
	case EUniv, EHole, EProp, EEq, ERefl, ECast, ESubst, ESig, EPair, ENum:
		return x // leaf nodes: no names to rewrite
	case EFst:
		return EFst{P: mapNamesScoped(x.P, ren, bound)}
	case ESnd:
		return ESnd{P: mapNamesScoped(x.P, ren, bound)}
	case ELam:
		// Dom is the parameter's type annotation: NOT in the binder scope.
		// Body IS in the binder scope: Param shadows there.
		return ELam{Param: x.Param, Icit: x.Icit, Qty: x.Qty,
			Dom:  mapNamesScoped(x.Dom, ren, bound),
			Body: mapNamesScoped(x.Body, ren, boundWith(bound, x.Param)),
		}
	case EApp:
		return EApp{Icit: x.Icit,
			Fn:  mapNamesScoped(x.Fn, ren, bound),
			Arg: mapNamesScoped(x.Arg, ren, bound),
		}
	case EPi:
		// Dom is the parameter's type annotation: NOT in the binder scope.
		// Cod IS in the binder scope: Param shadows there.
		return EPi{Param: x.Param, Icit: x.Icit, Qty: x.Qty,
			Dom: mapNamesScoped(x.Dom, ren, bound),
			Cod: mapNamesScoped(x.Cod, ren, boundWith(bound, x.Param)),
		}
	case ELet:
		// Ty and Val are NOT in the binder scope (standard let semantics).
		// Body IS in the binder scope: Name shadows there.
		var ty Exp
		if x.Ty != nil {
			ty = mapNamesScoped(x.Ty, ren, bound)
		}
		return ELet{Name: x.Name, Ty: ty,
			Val:  mapNamesScoped(x.Val, ren, bound),
			Body: mapNamesScoped(x.Body, ren, boundWith(bound, x.Name)),
		}
	case ESeqBind:
		// Same scoping as ELet.
		var ty Exp
		if x.Ty != nil {
			ty = mapNamesScoped(x.Ty, ren, bound)
		}
		return ESeqBind{Name: x.Name, Ty: ty,
			Val:  mapNamesScoped(x.Val, ren, bound),
			Body: mapNamesScoped(x.Body, ren, boundWith(bound, x.Name)),
		}
	case EAnn:
		return EAnn{
			Term: mapNamesScoped(x.Term, ren, bound),
			Ty:   mapNamesScoped(x.Ty, ren, bound),
		}
	case ECase:
		cls := make([]CaseClause, len(x.Clauses))
		for i, cl := range x.Clauses {
			// Each clause's Binders and IHs scope over that clause's Body only.
			// The Ctor head is a constructor reference (always free): rename it.
			clBound := boundWith(bound, cl.Binders...)
			clBound = boundWith(clBound, cl.IHs...)
			cls[i] = CaseClause{
				Ctor:    ren(cl.Ctor),
				Binders: cl.Binders,
				IHs:     cl.IHs,
				Body:    mapNamesScoped(cl.Body, ren, clBound),
			}
		}
		return ECase{Scrut: mapNamesScoped(x.Scrut, ren, bound), Clauses: cls}
	default:
		panic(fmt.Sprintf("surface.MapExpNames: unknown Exp type %T; update walk.go", e))
	}
}

// boundWith returns a copy of bound extended with the given names. If every
// name is already in bound (or the names list is empty), bound itself is
// returned unchanged to avoid allocation.
func boundWith(bound map[string]bool, names ...string) map[string]bool {
	if len(names) == 0 {
		return bound
	}
	anyNew := false
	for _, n := range names {
		if n != "" && !bound[n] {
			anyNew = true
			break
		}
	}
	if !anyNew {
		return bound
	}
	nb := make(map[string]bool, len(bound)+len(names))
	for k := range bound {
		nb[k] = true
	}
	for _, n := range names {
		if n != "" {
			nb[n] = true
		}
	}
	return nb
}

// WalkExp calls fn on e and recursively on every sub-expression, depth-first,
// pre-order. It panics on an unknown Exp type so a future AST addition fails
// loudly in tests rather than silently missing references.
func WalkExp(e Exp, fn func(Exp)) {
	fn(e)
	switch x := e.(type) {
	case EVar, EUniv, EHole, EProp, EEq, ERefl, ECast, ESubst, ESig, EPair, ENum:
		// leaf nodes: no sub-expressions to recurse into
	case EFst:
		WalkExp(x.P, fn)
	case ESnd:
		WalkExp(x.P, fn)
	case ELam:
		WalkExp(x.Dom, fn)
		WalkExp(x.Body, fn)
	case EApp:
		WalkExp(x.Fn, fn)
		WalkExp(x.Arg, fn)
	case EPi:
		WalkExp(x.Dom, fn)
		WalkExp(x.Cod, fn)
	case ELet:
		if x.Ty != nil {
			WalkExp(x.Ty, fn)
		}
		WalkExp(x.Val, fn)
		WalkExp(x.Body, fn)
	case ESeqBind:
		if x.Ty != nil {
			WalkExp(x.Ty, fn)
		}
		WalkExp(x.Val, fn)
		WalkExp(x.Body, fn)
	case EAnn:
		WalkExp(x.Term, fn)
		WalkExp(x.Ty, fn)
	case ECase:
		WalkExp(x.Scrut, fn)
		for _, cl := range x.Clauses {
			WalkExp(cl.Body, fn)
		}
	default:
		panic(fmt.Sprintf("surface.WalkExp: unknown Exp type %T; update walk.go", e))
	}
}
