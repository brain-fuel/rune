package elaborate

import (
	"fmt"
	"sort"
	"strconv"

	"goforge.dev/rune/v3/core"
)

// Metavariables (Phase 2). Metas are ELABORATION-SCOPED: created fresh per
// Elaborator run, solved by pattern unification, and eliminated by zonking
// before any term reaches the store or the hash boundary. The proof-cache layer
// never sees them — checking cached core is the deterministic, meta-free
// judgment (ref_docs/rune-proof-cache-semantics.md §0).

// metaEntry is one metavariable: its solution value and quoted solution term,
// nil/unset while unsolved.
type metaEntry struct {
	solution core.Val
	name     string // a display hint: where the meta came from
}

// metaState is the Elaborator's meta table; it satisfies core.MetaSolver so the
// shared Machine forces solved metas transparently.
type metaState struct {
	entries []metaEntry
}

// Solution implements core.MetaSolver.
func (ms *metaState) Solution(id int) (core.Val, bool) {
	if id < 0 || id >= len(ms.entries) || ms.entries[id].solution == nil {
		return nil, false
	}
	return ms.entries[id].solution, true
}

// freshMeta mints a new metavariable and returns its CONTEXTUAL term: the meta
// applied to every lambda/Pi-bound variable in scope (let-bound variables are
// definitionally transparent and excluded, keeping spines in the pattern
// fragment). The term is well-scoped in c.
func (e *Elaborator) freshMeta(c *Ctx, hint string) core.Tm {
	id := len(e.metas.entries)
	e.metas.entries = append(e.metas.entries, metaEntry{name: hint})
	var tm core.Tm = core.Meta{ID: id}
	// Apply outermost-first: walk indices from the outside in.
	for i := len(c.names) - 1; i >= 0; i-- {
		if c.bound[i] {
			tm = core.App{Fn: tm, Arg: core.Var{Idx: i}, Icit: core.Expl}
		}
	}
	return tm
}

// unsolved returns the ids of unsolved metas, sorted.
func (e *Elaborator) unsolved() []int {
	var out []int
	for i, m := range e.metas.entries {
		if m.solution == nil {
			out = append(out, i)
		}
	}
	sort.Ints(out)
	return out
}

// ErrUnsolved formats the unsolved-metas error for a definition or expression.
func (e *Elaborator) ErrUnsolved(what string) error {
	ids := e.unsolved()
	if len(ids) == 0 {
		return nil
	}
	holes := ""
	for i, id := range ids {
		if i > 0 {
			holes += ", "
		}
		holes += fmt.Sprintf("?%d", id)
		if n := e.metas.entries[id].name; n != "" {
			holes += " (" + n + ")"
		}
	}
	n := len(ids)
	thing := "one value"
	if n > 1 {
		thing = strconv.Itoa(n) + " values"
	}
	return &Diagnostic{
		Summary: "I couldn't work out every type in " + what + ".",
		Body: []string{
			"Type inference left " + thing + " undetermined: " + holes + ". These come " +
				"from a hole (`_`) or an implicit argument (`{…}`) that the surrounding code " +
				"does not pin down — there was not enough information to fill them in.",
		},
		Hints: []string{
			"Add an annotation where the type is ambiguous: give the hole an explicit type, " +
				"or pass the implicit argument directly with `{the type}`, so inference has " +
				"a value to work from.",
		},
	}
}

// Zonk replaces every solved metavariable in t by its solution, β-reducing only
// the meta-headed applications it rewrites (user redexes are left untouched, so
// plain programs keep resolution's exact core and hashes). t must be well-scoped
// at depth lvl. Unsolved metas survive zonking; callers reject them separately.
func (e *Elaborator) Zonk(lvl int, t core.Tm) core.Tm {
	switch tm := t.(type) {
	case core.Var, core.Ref, core.Univ, core.Prop:
		return t
	case core.Eq:
		return core.Eq{Ty: e.Zonk(lvl, tm.Ty), L: e.Zonk(lvl, tm.L), R: e.Zonk(lvl, tm.R)}
	case core.Refl:
		return core.Refl{Tm: e.Zonk(lvl, tm.Tm)}
	case core.Cast:
		return core.Cast{A: e.Zonk(lvl, tm.A), B: e.Zonk(lvl, tm.B),
			P: e.Zonk(lvl, tm.P), X: e.Zonk(lvl, tm.X)}
	case core.Subst:
		return core.Subst{A: e.Zonk(lvl, tm.A), X: e.Zonk(lvl, tm.X), Y: e.Zonk(lvl, tm.Y),
			Prf: e.Zonk(lvl, tm.Prf), P: e.Zonk(lvl, tm.P), Px: e.Zonk(lvl, tm.Px)}
	case core.Meta:
		if sol, ok := e.metas.Solution(tm.ID); ok {
			return e.Zonk(lvl, e.M.Quote(lvl, sol))
		}
		return t
	case core.Pi:
		return core.Pi{Icit: tm.Icit, Qty: tm.Qty, Dom: e.Zonk(lvl, tm.Dom),
			Cod: core.Scope{Name: tm.Cod.Name, Body: e.Zonk(lvl+1, tm.Cod.Body)}}
	case core.Lam:
		return core.Lam{Icit: tm.Icit, Qty: tm.Qty,
			Body: core.Scope{Name: tm.Body.Name, Body: e.Zonk(lvl+1, tm.Body.Body)}}
	case core.App:
		// A meta-headed application whose head is solved is evaluated away as a
		// unit, not rebuilt — the artificial redex must not reach the core.
		if id, ok := metaHead(tm); ok {
			if _, solved := e.metas.Solution(id); solved {
				// Evaluate the spine, force the flexible head away (Quote never
				// forces), and re-zonk the readback for metas in arguments.
				v := e.forceFlex(e.M.Eval(spineEnv(lvl), t))
				return e.Zonk(lvl, e.M.Quote(lvl, v))
			}
		}
		return core.App{Fn: e.Zonk(lvl, tm.Fn), Arg: e.Zonk(lvl, tm.Arg), Icit: tm.Icit}
	case core.Let:
		var ty core.Tm
		if tm.Ty != nil {
			ty = e.Zonk(lvl, tm.Ty)
		}
		return core.Let{Ty: ty, Val: e.Zonk(lvl, tm.Val),
			Body: core.Scope{Name: tm.Body.Name, Body: e.Zonk(lvl+1, tm.Body.Body)}}
	case core.Ann:
		return core.Ann{Term: e.Zonk(lvl, tm.Term), Ty: e.Zonk(lvl, tm.Ty)}
	case core.Sig:
		return core.Sig{Qty: tm.Qty, Dom: e.Zonk(lvl, tm.Dom),
			Cod: core.Scope{Name: tm.Cod.Name, Body: e.Zonk(lvl+1, tm.Cod.Body)}}
	case core.Pair:
		return core.Pair{Dom: e.Zonk(lvl, tm.Dom),
			Cod: core.Scope{Name: tm.Cod.Name, Body: e.Zonk(lvl+1, tm.Cod.Body)},
			A:   e.Zonk(lvl, tm.A), B: e.Zonk(lvl, tm.B)}
	case core.Fst:
		return core.Fst{P: e.Zonk(lvl, tm.P)}
	case core.Snd:
		return core.Snd{P: e.Zonk(lvl, tm.P)}
	case core.NatLit:
		// A compressed numeral carries no subterms and no metas: it is its own zonk.
		return tm
	default:
		panic(fmt.Sprintf("zonk: unknown core term %T", t))
	}
}

// metaHead reports the meta id heading an application spine, if any.
func metaHead(t core.Tm) (int, bool) {
	for {
		switch x := t.(type) {
		case core.App:
			t = x.Fn
		case core.Meta:
			return x.ID, true
		default:
			return 0, false
		}
	}
}

// spineEnv is the identity environment at depth lvl: Var i ↦ the free variable
// at level lvl-1-i, so evaluating an open term and quoting back at lvl is the
// identity on everything but the reduced meta application.
func spineEnv(lvl int) core.Env {
	env := make(core.Env, lvl)
	for i := 0; i < lvl; i++ {
		env[i] = core.VVar(lvl - 1 - i)
	}
	return env
}

// MetaFree reports whether t contains no metavariables (solved or not) — the
// invariant required at the store/hash boundary.
func MetaFree(t core.Tm) bool {
	switch tm := t.(type) {
	case core.Var, core.Ref, core.Univ, core.Prop:
		return true
	case core.Eq:
		return MetaFree(tm.Ty) && MetaFree(tm.L) && MetaFree(tm.R)
	case core.Refl:
		return MetaFree(tm.Tm)
	case core.Cast:
		return MetaFree(tm.A) && MetaFree(tm.B) && MetaFree(tm.P) && MetaFree(tm.X)
	case core.Subst:
		return MetaFree(tm.A) && MetaFree(tm.X) && MetaFree(tm.Y) &&
			MetaFree(tm.Prf) && MetaFree(tm.P) && MetaFree(tm.Px)
	case core.Meta:
		return false
	case core.Pi:
		return MetaFree(tm.Dom) && MetaFree(tm.Cod.Body)
	case core.Lam:
		return MetaFree(tm.Body.Body)
	case core.App:
		return MetaFree(tm.Fn) && MetaFree(tm.Arg)
	case core.Let:
		return (tm.Ty == nil || MetaFree(tm.Ty)) && MetaFree(tm.Val) && MetaFree(tm.Body.Body)
	case core.Ann:
		return MetaFree(tm.Term) && MetaFree(tm.Ty)
	case core.Sig:
		return MetaFree(tm.Dom) && MetaFree(tm.Cod.Body)
	case core.Pair:
		return MetaFree(tm.Dom) && MetaFree(tm.Cod.Body) && MetaFree(tm.A) && MetaFree(tm.B)
	case core.Fst:
		return MetaFree(tm.P)
	case core.Snd:
		return MetaFree(tm.P)
	case core.NatLit:
		// A compressed numeral mentions no metavariable.
		return true
	default:
		return false
	}
}
