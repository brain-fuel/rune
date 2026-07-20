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
	match t {
	case core.Var(_), core.Ref(_), core.Univ(_), core.Prop, core.NatLit(_, _, _):
		// Leaves (a compressed numeral carries no subterms and no metas).
		return t
	case core.Eq(ty, l, r):
		return core.Eq(e.Zonk(lvl, ty), e.Zonk(lvl, l), e.Zonk(lvl, r))
	case core.Refl(tm):
		return core.Refl(e.Zonk(lvl, tm))
	case core.Cast(a, b, p, x):
		return core.Cast(e.Zonk(lvl, a), e.Zonk(lvl, b), e.Zonk(lvl, p), e.Zonk(lvl, x))
	case core.Subst(a, x, y, prf, p, px):
		return core.Subst(e.Zonk(lvl, a), e.Zonk(lvl, x), e.Zonk(lvl, y),
			e.Zonk(lvl, prf), e.Zonk(lvl, p), e.Zonk(lvl, px))
	case core.Meta(id):
		if sol, ok := e.metas.Solution(id); ok {
			return e.Zonk(lvl, e.M.Quote(lvl, sol))
		}
		return t
	case core.Pi(icit, qty, dom, cod):
		return core.Pi(icit, qty, e.Zonk(lvl, dom),
			core.Scope{Name: cod.Name, Body: e.Zonk(lvl+1, cod.Body)})
	case core.Lam(icit, qty, body):
		return core.Lam(icit, qty,
			core.Scope{Name: body.Name, Body: e.Zonk(lvl+1, body.Body)})
	case core.App(fn, arg, icit):
		// A meta-headed application whose head is solved is evaluated away as a
		// unit, not rebuilt — the artificial redex must not reach the core.
		if id, ok := metaHead(t); ok {
			if _, solved := e.metas.Solution(id); solved {
				// Evaluate the spine, force the flexible head away (Quote never
				// forces), and re-zonk the readback for metas in arguments.
				v := e.forceFlex(e.M.Eval(spineEnv(lvl), t))
				return e.Zonk(lvl, e.M.Quote(lvl, v))
			}
		}
		return core.App(e.Zonk(lvl, fn), e.Zonk(lvl, arg), icit)
	case core.Let(ty, val, body):
		var zty core.Tm
		if ty != nil {
			zty = e.Zonk(lvl, ty)
		}
		return core.Let(zty, e.Zonk(lvl, val),
			core.Scope{Name: body.Name, Body: e.Zonk(lvl+1, body.Body)})
	case core.Ann(term, ty):
		return core.Ann(e.Zonk(lvl, term), e.Zonk(lvl, ty))
	case core.Sig(qty, dom, cod):
		return core.Sig(qty, e.Zonk(lvl, dom),
			core.Scope{Name: cod.Name, Body: e.Zonk(lvl+1, cod.Body)})
	case core.Pair(dom, cod, a, b):
		return core.Pair(e.Zonk(lvl, dom),
			core.Scope{Name: cod.Name, Body: e.Zonk(lvl+1, cod.Body)},
			e.Zonk(lvl, a), e.Zonk(lvl, b))
	case core.Fst(p):
		return core.Fst(e.Zonk(lvl, p))
	case core.Snd(p):
		return core.Snd(e.Zonk(lvl, p))
	}
}

// metaHead reports the meta id heading an application spine, if any.
func metaHead(t core.Tm) (int, bool) {
	for {
		match t {
		case core.App(fn, _, _):
			t = fn
		case core.Meta(id):
			return id, true
		case _:
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
	if t == nil {
		return false // matches the old walk: nil is not judged meta-free
	}
	// The derived traversal walks every subterm; one Meta anywhere sinks it.
	for sub := range core.TmUniverse(t) {
		match sub {
		case core.Meta(_):
			return false
		case _:
		}
	}
	return true
}
