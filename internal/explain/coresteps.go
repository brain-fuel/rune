package explain

import (
	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
)

// coreSteps renders the elaborated core walk (the nothing-hidden view):
// binders peel to Given steps (implicits marked, underscore names shown),
// lets show their value, bindIO chains still flatten (the step LIST is the
// shared shape across depths) with the bound value's type after the binder,
// and everything else prints whole via printCore. English prim templates do
// NOT apply here: the reader asked to see the program as the kernel sees it.
//
// The io flag is true when the current term is in a position that must produce
// an IO action (i.e. inside a bindIO continuation body). In that position the
// terminal step uses the verb "Do" instead of "Compute" so the reader sees the
// do-notation shape even for final, non-bind IO actions.
func coreSteps(s *session.Session, t core.Tm, env []string) []Step {
	return coreStepsIO(s, t, env, false)
}

func coreStepsIO(s *session.Session, t core.Tm, env []string, io bool) []Step {
	rn := s.RefNames()
	switch x := t.(type) {
	case core.Lam:
		n := scopeName(x.Body)
		label := "Given `" + n + "`:"
		if x.Icit == core.Impl {
			label = "Given implicit `" + n + "`:"
		}
		return []Step{{Text: label, Kids: coreStepsIO(s, x.Body.Body, append(env, n), io)}}
	case core.Let:
		n := scopeName(x.Body)
		st := Step{Text: "Let `" + n + "` be (" + printCore(x.Val, env, rn) + ")"}
		return append([]Step{st}, coreStepsIO(s, x.Body.Body, append(env, n), io)...)
	}
	head, args := coreSpine(t)
	if r, ok := head.(core.Ref); ok && len(args) == 4 {
		if n, named := rn[r.Hash]; named && short(n) == "bindIO" {
			aTy := printCore(args[0], env, rn)
			m, k := args[2], args[3]
			if lam, isLam := k.(core.Lam); isLam {
				b := scopeName(lam.Body)
				first := Step{Text: "Do (" + printCore(m, env, rn) + ") as `" + b + "` : " + aTy}
				return append([]Step{first}, coreStepsIO(s, lam.Body.Body, append(env, b), true)...)
			}
			return []Step{
				{Text: "Do (" + printCore(m, env, rn) + ")"},
				{Text: "Pass the Result to (" + printCore(k, env, rn) + ")"},
			}
		}
	}
	verb := "Compute"
	if io {
		verb = "Do"
	}
	return []Step{{Text: verb + " (" + printCore(t, env, rn) + ")"}}
}

// coreSpine flattens a left-associated core application into head + args
// (implicit args included: nothing hidden).
func coreSpine(t core.Tm) (core.Tm, []core.Tm) {
	var args []core.Tm
	for {
		a, ok := t.(core.App)
		if !ok {
			break
		}
		args = append([]core.Tm{a.Arg}, args...)
		t = a.Fn
	}
	return t, args
}
