package surface

import "fmt"

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
