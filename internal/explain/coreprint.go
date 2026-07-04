package explain

import (
	"fmt"

	"goforge.dev/rune/v3/core"
)

// printCore renders a possibly-OPEN core term compactly on one line, given
// the binder names in scope (env[len(env)-1] names Var{0}). surface.Pretty
// requires closed terms, so the explainer carries its own small open-term
// printer. Display only: binder hints are used as-is, without capture-
// avoidance freshening. Implicit applications print in braces (the depth=core
// "implicits visible" rule rides on this printer).
func printCore(t core.Tm, env []string, refNames map[core.Hash]string) string {
	switch x := t.(type) {
	case core.Var:
		i := len(env) - 1 - x.Idx
		if i < 0 || i >= len(env) {
			return fmt.Sprintf("?v%d", x.Idx)
		}
		return env[i]
	case core.Ref:
		if n, ok := refNames[x.Hash]; ok {
			return short(n)
		}
		return fmt.Sprintf("#%s", x.Hash)
	case core.Univ:
		if x.Lvl == 0 {
			return "U"
		}
		return fmt.Sprintf("U%d", x.Lvl)
	case core.Prop:
		return "Prop"
	case core.NatLit:
		return x.N.String()
	case core.Meta:
		return fmt.Sprintf("?m%d", x.ID)
	case core.App:
		arg := printCore(x.Arg, env, refNames)
		if x.Icit == core.Impl {
			return printCore(x.Fn, env, refNames) + " {" + arg + "}"
		}
		if !coreAtom(x.Arg) {
			arg = "(" + arg + ")"
		}
		return printCore(x.Fn, env, refNames) + " " + arg
	case core.Lam:
		n := scopeName(x.Body)
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return "fn " + lp + n + rp + " is " + printCore(x.Body.Body, append(env, n), refNames) + " end"
	case core.Pi:
		n := scopeName(x.Cod)
		dom := printCore(x.Dom, env, refNames)
		cod := printCore(x.Cod.Body, append(env, n), refNames)
		if n == "_" && x.Icit == core.Expl {
			if !coreAtom(x.Dom) {
				dom = "(" + dom + ")"
			}
			return dom + " -> " + cod
		}
		lp, rp := "(", ")"
		if x.Icit == core.Impl {
			lp, rp = "{", "}"
		}
		return lp + n + " : " + dom + rp + " -> " + cod
	case core.Let:
		n := scopeName(x.Body)
		return "let " + n + " = " + printCore(x.Val, env, refNames) + "; " +
			printCore(x.Body.Body, append(env, n), refNames)
	case core.Ann:
		return "(" + printCore(x.Term, env, refNames) + " : " + printCore(x.Ty, env, refNames) + ")"
	case core.Sig:
		n := scopeName(x.Cod)
		return "Sig (" + n + " : " + printCore(x.Dom, env, refNames) + ") " +
			printCore(x.Cod.Body, append(env, n), refNames)
	case core.Pair:
		return "pair " + coreAtomStr(x.A, env, refNames) + " " + coreAtomStr(x.B, env, refNames)
	case core.Fst:
		return coreAtomStr(x.P, env, refNames) + ".1"
	case core.Snd:
		return coreAtomStr(x.P, env, refNames) + ".2"
	case core.Eq:
		return "Eq " + coreAtomStr(x.Ty, env, refNames) + " " + coreAtomStr(x.L, env, refNames) +
			" " + coreAtomStr(x.R, env, refNames)
	case core.Refl:
		return "refl " + coreAtomStr(x.Tm, env, refNames)
	case core.Cast:
		return "cast " + coreAtomStr(x.A, env, refNames) + " " + coreAtomStr(x.B, env, refNames) +
			" " + coreAtomStr(x.P, env, refNames) + " " + coreAtomStr(x.X, env, refNames)
	case core.Subst:
		return "subst " + coreAtomStr(x.A, env, refNames) + " " + coreAtomStr(x.X, env, refNames) +
			" " + coreAtomStr(x.Y, env, refNames) + " " + coreAtomStr(x.Prf, env, refNames) +
			" " + coreAtomStr(x.P, env, refNames) + " " + coreAtomStr(x.Px, env, refNames)
	default:
		panic(fmt.Sprintf("explain.printCore: unknown core.Tm %T; update coreprint.go", t))
	}
}

func scopeName(sc core.Scope) string {
	if sc.Name == "" {
		return "_"
	}
	return sc.Name
}

func coreAtom(t core.Tm) bool {
	switch t.(type) {
	case core.Var, core.Ref, core.Univ, core.Prop, core.NatLit, core.Meta, core.Fst, core.Snd:
		return true
	}
	return false
}

func coreAtomStr(t core.Tm, env []string, refNames map[core.Hash]string) string {
	s := printCore(t, env, refNames)
	if coreAtom(t) {
		return s
	}
	return "(" + s + ")"
}
