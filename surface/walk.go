package surface

import "fmt"

// MapExpNames returns a copy of e with every EVar name and every ECase clause
// constructor head replaced by ren(name). The returned expression is structurally
// identical to e except at name positions. Same exhaustive-with-panic discipline
// as WalkExp: an unknown Exp type panics loudly in tests rather than silently
// skipping names.
func MapExpNames(e Exp, ren func(string) string) Exp {
	switch x := e.(type) {
	case EVar:
		return EVar{Name: ren(x.Name)}
	case EUniv, EHole, EProp, EEq, ERefl, ECast, ESubst, ESig, EPair, ENum:
		return x // leaf nodes: no names to rewrite
	case EFst:
		return EFst{P: MapExpNames(x.P, ren)}
	case ESnd:
		return ESnd{P: MapExpNames(x.P, ren)}
	case ELam:
		return ELam{Param: x.Param, Icit: x.Icit, Qty: x.Qty,
			Dom:  MapExpNames(x.Dom, ren),
			Body: MapExpNames(x.Body, ren),
		}
	case EApp:
		return EApp{Icit: x.Icit,
			Fn:  MapExpNames(x.Fn, ren),
			Arg: MapExpNames(x.Arg, ren),
		}
	case EPi:
		return EPi{Param: x.Param, Icit: x.Icit, Qty: x.Qty,
			Dom: MapExpNames(x.Dom, ren),
			Cod: MapExpNames(x.Cod, ren),
		}
	case ELet:
		var ty Exp
		if x.Ty != nil {
			ty = MapExpNames(x.Ty, ren)
		}
		return ELet{Name: x.Name, Ty: ty,
			Val:  MapExpNames(x.Val, ren),
			Body: MapExpNames(x.Body, ren),
		}
	case ESeqBind:
		var ty Exp
		if x.Ty != nil {
			ty = MapExpNames(x.Ty, ren)
		}
		return ESeqBind{Name: x.Name, Ty: ty,
			Val:  MapExpNames(x.Val, ren),
			Body: MapExpNames(x.Body, ren),
		}
	case EAnn:
		return EAnn{
			Term: MapExpNames(x.Term, ren),
			Ty:   MapExpNames(x.Ty, ren),
		}
	case ECase:
		cls := make([]CaseClause, len(x.Clauses))
		for i, cl := range x.Clauses {
			cls[i] = CaseClause{
				Ctor:    ren(cl.Ctor),
				Binders: cl.Binders,
				IHs:     cl.IHs,
				Body:    MapExpNames(cl.Body, ren),
			}
		}
		return ECase{Scrut: MapExpNames(x.Scrut, ren), Clauses: cls}
	default:
		panic(fmt.Sprintf("surface.MapExpNames: unknown Exp type %T; update walk.go", e))
	}
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
