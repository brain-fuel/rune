package explain

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/surface"
)

// short returns the last dot-segment of a possibly module-qualified name,
// the same display-identity rule as codegen's primName: after an import the
// reader sees the short name on screen, so the explainer shows it too.
func short(name string) string {
	if i := strings.LastIndex(name, "."); i >= 0 {
		return name[i+1:]
	}
	return name
}

// isAtomExp reports whether e prints as a single token (no parens needed).
func isAtomExp(e surface.Exp) bool {
	switch e.(type) {
	case surface.EVar, surface.ENum, surface.EUniv, surface.EProp, surface.EHole,
		surface.ESig, surface.EPair, surface.EEq, surface.ERefl, surface.ECast,
		surface.ESubst, surface.EFst, surface.ESnd:
		return true
	}
	return false
}

func atom(e surface.Exp) string {
	if isAtomExp(e) {
		return printExp(e)
	}
	return "(" + printExp(e) + ")"
}

// printExp renders a surface expression compactly on one line, close to what
// the user wrote: applications as spines, compound arguments parenthesized,
// names as their last dot-segment. Quantity annotations are omitted (display
// only). The switch is exhaustive over surface.Exp; an unknown node panics so
// an AST addition fails loudly in tests (the WalkExp discipline).
func printExp(e surface.Exp) string {
	switch x := e.(type) {
	case surface.EVar:
		return short(x.Name)
	case surface.ENum:
		return x.Val.String()
	case surface.EUniv:
		if x.Lvl == 0 {
			return "U"
		}
		return fmt.Sprintf("U%d", x.Lvl)
	case surface.EHole:
		return "_"
	case surface.EProp:
		return "Prop"
	case surface.ESig:
		return "Sig"
	case surface.EPair:
		return "pair"
	case surface.EEq:
		return "Eq"
	case surface.ERefl:
		return "refl"
	case surface.ECast:
		return "cast"
	case surface.ESubst:
		return "subst"
	case surface.EFst:
		return atom(x.P) + ".1"
	case surface.ESnd:
		return atom(x.P) + ".2"
	case surface.EApp:
		if x.Icit == core.Impl {
			return printExp(x.Fn) + " {" + printExp(x.Arg) + "}"
		}
		return printExp(x.Fn) + " " + atom(x.Arg)
	case surface.ELam:
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return "fn " + lp + x.Param + " : " + printExp(x.Dom) + rp + " is " + printExp(x.Body) + " end"
	case surface.EPi:
		if x.Param == "_" && x.Icit == core.Expl {
			return atom(x.Dom) + " -> " + printExp(x.Cod)
		}
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return lp + x.Param + " : " + printExp(x.Dom) + rp + " -> " + printExp(x.Cod)
	case surface.ELet:
		return "let " + x.Name + " = " + printExp(x.Val) + "; " + printExp(x.Body)
	case surface.ESeqBind:
		return "let " + x.Name + " = " + printExp(x.Val) + "; " + printExp(x.Body)
	case surface.EAnn:
		return "(" + printExp(x.Term) + " : " + printExp(x.Ty) + ")"
	case surface.ECase:
		var sb strings.Builder
		sb.WriteString("case " + printExp(x.Scrut) + " of")
		for _, cl := range x.Clauses {
			sb.WriteString(" | " + cl.Ctor)
			for _, b := range cl.Binders {
				sb.WriteString(" " + b)
			}
			if len(cl.IHs) > 0 {
				sb.WriteString(" with " + strings.Join(cl.IHs, " "))
			}
			sb.WriteString(" -> " + printExp(cl.Body))
		}
		sb.WriteString(" end")
		return sb.String()
	default:
		panic(fmt.Sprintf("explain.printExp: unknown surface.Exp %T; update print.go", e))
	}
}
