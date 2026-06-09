package surface

import (
	"fmt"

	"goforge.dev/rune/core"
)

// Resolver carries the only context name resolution needs in Phase 0: a map from
// top-level definition names to their content hashes. Resolution is the sole
// "elaboration" in Phase 0 — it sends bound identifiers to de Bruijn indices and
// free identifiers to definition references. There is NO type elaboration here.
type Resolver struct {
	Refs map[string]core.Hash
}

// ResolveExp resolves a closed-or-reference-only surface expression into core. Bound
// variables become de Bruijn indices; free identifiers are looked up in Refs and
// become content-hash references.
func (r *Resolver) ResolveExp(e Exp) (core.Tm, error) {
	return r.resolve(e, nil)
}

// resolve walks e with ctx, the list of in-scope binder names innermost-first, so a
// name's de Bruijn index is its position in ctx.
func (r *Resolver) resolve(e Exp, ctx []string) (core.Tm, error) {
	switch x := e.(type) {
	case EVar:
		for i, n := range ctx {
			if n == x.Name {
				return core.Var{Idx: i}, nil
			}
		}
		if r.Refs != nil {
			if h, ok := r.Refs[x.Name]; ok {
				return core.Ref{Hash: h}, nil
			}
		}
		return nil, fmt.Errorf("unbound identifier %q", x.Name)
	case EUniv:
		return core.Univ{}, nil
	case ELam:
		// The binder's domain annotation is scope-checked in the enclosing context and
		// then discarded: the Phase-0 core lambda is un-annotated (GRAMMAR.md §6).
		if x.Dom != nil {
			if _, err := r.resolve(x.Dom, ctx); err != nil {
				return nil, err
			}
		}
		body, err := r.resolve(x.Body, push(x.Param, ctx))
		if err != nil {
			return nil, err
		}
		return core.Lam{Body: core.Scope{Name: x.Param, Body: body}}, nil
	case EApp:
		fn, err := r.resolve(x.Fn, ctx)
		if err != nil {
			return nil, err
		}
		arg, err := r.resolve(x.Arg, ctx)
		if err != nil {
			return nil, err
		}
		return core.App{Fn: fn, Arg: arg}, nil
	case EPi:
		dom, err := r.resolve(x.Dom, ctx)
		if err != nil {
			return nil, err
		}
		cod, err := r.resolve(x.Cod, push(x.Param, ctx))
		if err != nil {
			return nil, err
		}
		return core.Pi{Dom: dom, Cod: core.Scope{Name: x.Param, Body: cod}}, nil
	case ELet:
		var ty core.Tm
		if x.Ty != nil {
			t, err := r.resolve(x.Ty, ctx)
			if err != nil {
				return nil, err
			}
			ty = t
		}
		val, err := r.resolve(x.Val, ctx)
		if err != nil {
			return nil, err
		}
		body, err := r.resolve(x.Body, push(x.Name, ctx))
		if err != nil {
			return nil, err
		}
		return core.Let{Ty: ty, Val: val, Body: core.Scope{Name: x.Name, Body: body}}, nil
	case EAnn:
		term, err := r.resolve(x.Term, ctx)
		if err != nil {
			return nil, err
		}
		ty, err := r.resolve(x.Ty, ctx)
		if err != nil {
			return nil, err
		}
		return core.Ann{Term: term, Ty: ty}, nil
	default:
		return nil, fmt.Errorf("resolve: unknown surface expression %T", e)
	}
}

func push(name string, ctx []string) []string {
	out := make([]string, 0, len(ctx)+1)
	out = append(out, name)
	return append(out, ctx...)
}

// FreeIdents returns the free identifiers of e (those not bound by an enclosing
// lambda, Pi, or let), de-duplicated. The CLI uses this to build the definition
// dependency graph that drives SCC-ordered hashing.
func FreeIdents(e Exp) []string {
	seen := map[string]bool{}
	var out []string
	var walk func(e Exp, bound map[string]bool)
	walk = func(e Exp, bound map[string]bool) {
		switch x := e.(type) {
		case EVar:
			if !bound[x.Name] && !seen[x.Name] {
				seen[x.Name] = true
				out = append(out, x.Name)
			}
		case EUniv:
		case ELam:
			if x.Dom != nil {
				walk(x.Dom, bound)
			}
			walk(x.Body, with(bound, x.Param))
		case EApp:
			walk(x.Fn, bound)
			walk(x.Arg, bound)
		case EPi:
			walk(x.Dom, bound)
			walk(x.Cod, with(bound, x.Param))
		case ELet:
			if x.Ty != nil {
				walk(x.Ty, bound)
			}
			walk(x.Val, bound)
			walk(x.Body, with(bound, x.Name))
		case EAnn:
			walk(x.Term, bound)
			walk(x.Ty, bound)
		}
	}
	walk(e, map[string]bool{})
	return out
}

func with(bound map[string]bool, name string) map[string]bool {
	out := make(map[string]bool, len(bound)+1)
	for k := range bound {
		out[k] = true
	}
	out[name] = true
	return out
}
