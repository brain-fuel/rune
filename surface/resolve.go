package surface

import (
	"fmt"

	"goforge.dev/rune/v3/core"
)

// Resolver carries the only context name resolution needs in Phase 0: a map from
// top-level definition names to their content hashes. Resolution is the sole
// "elaboration" in Phase 0 — it sends bound identifiers to de Bruijn indices and
// free identifiers to definition references. There is NO type elaboration here.
type Resolver struct {
	Refs map[string]core.Hash
	// Num lowers numeral literals. Resolution is untyped, so it always picks the
	// unary `builtin nat` default — byte-identical to the old parse-time
	// expansion, keeping content hashes unchanged. A numeral whose real type is
	// binary lowers here to the (type-wrong) unary form, but that only feeds the
	// proof-cache fast path, where a miss is harmless: the typed elaborator is
	// the authority and lowers it to the binary spine.
	Num NumConfig
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
		return core.Univ{Lvl: x.Lvl}, nil
	case EHole:
		return nil, fmt.Errorf("a hole (_) needs the type checker to solve it; name resolution alone cannot")
	case EProp:
		return core.Prop{}, nil
	case ENum:
		// Untyped: lower to the unary default. An over-cap literal errors, which
		// only disables the cache fast path (the elaborator handles it typed).
		return r.Num.Nat(x.Val)
	case EEq, ERefl, ECast, ESubst:
		return nil, fmt.Errorf("an under-applied equality former; Eq takes 3 arguments, refl 1, cast 4, subst 6")
	case ESig, EPair:
		return nil, fmt.Errorf("an under-applied Σ former; Sig takes 2 (Sig A B), Pair 4 (Pair A B a b)")
	case EFst:
		pp, err := r.resolve(x.P, ctx)
		if err != nil {
			return nil, err
		}
		return core.Fst{P: pp}, nil
	case ESnd:
		pp, err := r.resolve(x.P, ctx)
		if err != nil {
			return nil, err
		}
		return core.Snd{P: pp}, nil
	case ECase:
		return nil, fmt.Errorf("a case expression needs the type checker (its motive comes from the expected type); name resolution alone cannot lower it")
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
		return core.Lam{Icit: x.Icit, Qty: x.Qty, Body: core.Scope{Name: x.Param, Body: body}}, nil
	case EApp:
		// A saturated equality-former spine resolves to its core node.
		if head, args := SpineOf(x); len(args) > 0 {
			switch head.(type) {
			case EEq:
				if len(args) != 3 {
					return nil, fmt.Errorf("Eq takes exactly 3 arguments, got %d", len(args))
				}
				ty, err := r.resolve(args[0], ctx)
				if err != nil {
					return nil, err
				}
				l, err := r.resolve(args[1], ctx)
				if err != nil {
					return nil, err
				}
				rr, err := r.resolve(args[2], ctx)
				if err != nil {
					return nil, err
				}
				return core.Eq{Ty: ty, L: l, R: rr}, nil
			case ERefl:
				if len(args) != 1 {
					return nil, fmt.Errorf("refl takes exactly 1 argument, got %d", len(args))
				}
				tm, err := r.resolve(args[0], ctx)
				if err != nil {
					return nil, err
				}
				return core.Refl{Tm: tm}, nil
			case ECast:
				if len(args) != 4 {
					return nil, fmt.Errorf("cast takes exactly 4 arguments, got %d", len(args))
				}
				var rs [4]core.Tm
				for i, a := range args {
					t, err := r.resolve(a, ctx)
					if err != nil {
						return nil, err
					}
					rs[i] = t
				}
				return core.Cast{A: rs[0], B: rs[1], P: rs[2], X: rs[3]}, nil
			case ESubst:
				if len(args) != 6 {
					return nil, fmt.Errorf("subst takes exactly 6 arguments, got %d", len(args))
				}
				var rs [6]core.Tm
				for i, a := range args {
					t, err := r.resolve(a, ctx)
					if err != nil {
						return nil, err
					}
					rs[i] = t
				}
				return core.Subst{A: rs[0], X: rs[1], Y: rs[2], Prf: rs[3], P: rs[4], Px: rs[5]}, nil
			case ESig:
				if len(args) != 2 {
					return nil, fmt.Errorf("Sig takes exactly 2 arguments (Sig A B), got %d", len(args))
				}
				aT, err := r.resolve(args[0], ctx)
				if err != nil {
					return nil, err
				}
				bT, err := r.resolve(args[1], ctx)
				if err != nil {
					return nil, err
				}
				lam, ok := bT.(core.Lam)
				if !ok {
					return nil, fmt.Errorf("Sig's family must be a function literal: Sig A (fn (x : A) is B end)")
				}
				return core.Sig{Dom: aT, Cod: lam.Body}, nil
			case EPair:
				if len(args) != 4 {
					return nil, fmt.Errorf("pair takes exactly 4 arguments (pair A B a b), got %d", len(args))
				}
				var rs [4]core.Tm
				for i, a := range args {
					t, err := r.resolve(a, ctx)
					if err != nil {
						return nil, err
					}
					rs[i] = t
				}
				lam, ok := rs[1].(core.Lam)
				if !ok {
					return nil, fmt.Errorf("pair's family must be a function literal: pair A (fn (x : A) is B end) a b")
				}
				return core.Pair{Dom: rs[0], Cod: lam.Body, A: rs[2], B: rs[3]}, nil
			}
		}
		fn, err := r.resolve(x.Fn, ctx)
		if err != nil {
			return nil, err
		}
		arg, err := r.resolve(x.Arg, ctx)
		if err != nil {
			return nil, err
		}
		return core.App{Fn: fn, Arg: arg, Icit: x.Icit}, nil
	case EPi:
		dom, err := r.resolve(x.Dom, ctx)
		if err != nil {
			return nil, err
		}
		cod, err := r.resolve(x.Cod, push(x.Param, ctx))
		if err != nil {
			return nil, err
		}
		return core.Pi{Icit: x.Icit, Qty: x.Qty, Dom: dom, Cod: core.Scope{Name: x.Param, Body: cod}}, nil
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

// SpineOf splits an application into its head and argument list (explicit
// arguments only; an implicit argument in a former spine disqualifies it).
func SpineOf(e Exp) (Exp, []Exp) {
	var args []Exp
	for {
		app, ok := e.(EApp)
		if !ok || app.Icit != core.Expl {
			return e, args
		}
		args = append([]Exp{app.Arg}, args...)
		e = app.Fn
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
		case EHole:
		case EProp, EEq, ERefl, ECast, ESubst:
		case ESig, EPair:
		case EFst:
			walk(x.P, bound)
		case ESnd:
			walk(x.P, bound)
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
		case ECase:
			walk(x.Scrut, bound)
			for _, cl := range x.Clauses {
				// The constructor name is a free reference (it drives the
				// dependency graph); the binders and IHs scope over the body.
				walk(EVar{Name: cl.Ctor}, bound)
				b := bound
				for _, n := range cl.Binders {
					b = with(b, n)
				}
				for _, n := range cl.IHs {
					b = with(b, n)
				}
				walk(cl.Body, b)
			}
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
