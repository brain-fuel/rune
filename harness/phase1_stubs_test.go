package harness

import (
	"testing"

	"pgregory.net/rapid"

	"goforge.dev/rune/core"
	"goforge.dev/rune/elaborate"
	"goforge.dev/rune/store"
	"goforge.dev/rune/surface"
)

// The Phase-1 invariants, formerly documented skips, now live properties. Each is
// a target the mutation-testing layer hunts against: a surviving mutant in the
// region one of these guards is a hole in the CHECKER, not the tests.

// elabClosed elaborates a generated closed expression against its generated type
// in an empty store, returning the core term, the type value, and the elaborator.
func elabClosed(t *rapid.T, e surface.Exp, ty Ty) (core.Tm, core.Val, *elaborate.Elaborator) {
	st := store.New()
	el := elaborate.New(st, nil, nil)
	tyTm, err := el.Check(&elaborate.Ctx{}, TyExp(ty), core.VU{})
	if err != nil {
		t.Fatalf("generated type does not check: %v", err)
	}
	want := el.M.Eval(nil, tyTm)
	tm, err := el.Check(&elaborate.Ctx{}, e, want)
	if err != nil {
		t.Fatalf("generated term does not check against its type: %v\nterm: %s",
			err, surface.Pretty(mustResolve(e)))
	}
	return tm, want, el
}

func mustResolve(e surface.Exp) core.Tm {
	r := &surface.Resolver{}
	c, err := r.ResolveExp(e)
	if err != nil {
		panic(err)
	}
	return c
}

// TestTypePreservation: normalization preserves well-typedness. If t checks at T,
// the βδ-normal form of t checks at T too.
func TestTypePreservation(t *testing.T) {
	rapid.Check(t, func(rt *rapid.T) {
		e, ty := GenTypedExp(rt)
		tm, want, el := elabClosed(rt, e, ty)
		nf := el.M.NormalizeUnfold(tm)
		// Re-check the normal form with a FRESH machine over the same store.
		el2 := elaborate.New(el.M.G, nil, nil)
		if err := el2.CheckCore(&elaborate.Ctx{}, nf, want); err != nil {
			rt.Fatalf("normal form lost its type: %v\nbefore: %s\nafter:  %s",
				err, surface.Pretty(tm), surface.Pretty(nf))
		}
	})
}

// TestConversionEquivalence: definitional conversion is reflexive, symmetric, and
// transitive on well-typed values. Transitivity is exercised through β-expansion
// chains: t ≡ (λx.x) t ≡ (λx.x) ((λx.x) t).
func TestConversionEquivalence(t *testing.T) {
	rapid.Check(t, func(rt *rapid.T) {
		e, ty := GenTypedExp(rt)
		tm, _, el := elabClosed(rt, e, ty)
		m := el.M

		idLam := core.Lam{Body: core.Scope{Name: "x", Body: core.Var{Idx: 0}}}
		a := m.Eval(nil, tm)
		wrap1 := core.App{Fn: idLam, Arg: tm}
		wrap2 := core.App{Fn: idLam, Arg: wrap1}
		b := m.Eval(nil, wrap1)
		c := m.Eval(nil, wrap2)

		if !m.Conv(0, a, a) {
			rt.Fatal("conversion not reflexive")
		}
		if !m.Conv(0, a, b) || !m.Conv(0, b, a) {
			rt.Fatal("conversion not symmetric across a β-expansion")
		}
		if !m.Conv(0, b, c) {
			rt.Fatal("β-expansion chain broke")
		}
		if !m.Conv(0, a, c) {
			rt.Fatal("conversion not transitive across β-expansions")
		}
	})
}

// TestConversionCongruence: conversion is preserved by the term formers — under
// application to the same neutral head, under Pi, and under a binder.
func TestConversionCongruence(t *testing.T) {
	rapid.Check(t, func(rt *rapid.T) {
		e, ty := GenTypedExp(rt)
		tm, _, el := elabClosed(rt, e, ty)
		m := el.M

		idLam := core.Lam{Body: core.Scope{Name: "x", Body: core.Var{Idx: 0}}}
		a := m.Eval(nil, tm)
		b := m.Eval(nil, core.App{Fn: idLam, Arg: tm})
		if !m.Conv(0, a, b) {
			rt.Fatal("precondition: a ≡ b")
		}

		// Congruence under application to a fresh neutral head f.
		f := core.VVar(0)
		if !m.Conv(1, m.Apply(f, a), m.Apply(f, b)) {
			rt.Fatal("conversion not a congruence under application")
		}
		// Congruence under Pi (when a, b : U so they can be a codomain).
		if _, isU := ty.(TyU); isU {
			pa := core.VPi{Name: "_", Dom: core.VU{}, Cod: func(core.Val) core.Val { return a }}
			pb := core.VPi{Name: "_", Dom: core.VU{}, Cod: func(core.Val) core.Val { return b }}
			if !m.Conv(0, pa, pb) {
				rt.Fatal("conversion not a congruence under Pi")
			}
		}
		// Congruence under a binder.
		la := core.VLam{Name: "_", Body: func(core.Val) core.Val { return a }}
		lb := core.VLam{Name: "_", Body: func(core.Val) core.Val { return b }}
		if !m.Conv(0, la, lb) {
			rt.Fatal("conversion not a congruence under a binder")
		}
	})
}

// TestProofCacheFrameLemma: a check's result — including its recorded dependency
// set U — is invariant under every store difference that preserves the bodies in
// U. In a content-addressed store the only admissible difference IS "other
// definitions": a body cannot change under a hash. Definitions whose bodies were
// never unfolded must not appear in U and must not affect the result.
func TestProofCacheFrameLemma(t *testing.T) {
	parse := func(src string) []surface.Def {
		defs, err := surface.ParseFile(src)
		if err != nil {
			t.Fatal(err)
		}
		return defs
	}

	// Checking d : A with body U forces A's body (conversion must see through
	// the Ref to U), so U_d = {A}. "unrelated" sits in the store, never touched.
	base := parse(`
A : U is U end
unrelated : U is U -> U end
d : A is U end
`)

	check := func(extra []surface.Def) (deps []core.Hash, defHash core.Hash, aHash core.Hash) {
		st := store.New()
		refs := map[string]core.Hash{}
		refNames := map[core.Hash]string{}
		addRaw := func(d surface.Def) core.Hash {
			r := &surface.Resolver{Refs: refs}
			ty, err := r.ResolveExp(d.Ty)
			if err != nil {
				t.Fatal(err)
			}
			body, err := r.ResolveExp(d.Body)
			if err != nil {
				t.Fatal(err)
			}
			h := st.Add(d.Name, ty, body)
			refs[d.Name] = h
			refNames[h] = d.Name
			return h
		}
		aHash = addRaw(base[0])
		addRaw(base[1])
		for _, d := range extra {
			addRaw(d)
		}
		dd := base[2]
		r := &surface.Resolver{Refs: refs}
		ty, _ := r.ResolveExp(dd.Ty)
		body, _ := r.ResolveExp(dd.Body)
		el := elaborate.New(st, refs, refNames)
		if err := el.CheckDef(ty, body); err != nil {
			t.Fatalf("d failed to check: %v", err)
		}
		return el.M.DepList(), store.HashDef(ty, body), aHash
	}

	depsS, hS, aS := check(nil)
	// S′ differs from S by extra definitions whose bodies d never consults.
	depsS2, hS2, _ := check(parse(`
extra1 : U is U -> U -> U end
extra2 : U -> U is fn (x : U) is x end end
`))

	if hS != hS2 {
		t.Fatal("d's content hash changed across stores")
	}
	if len(depsS) != 1 || len(depsS2) != 1 {
		t.Fatalf("expected U = {A} in both runs, got %v and %v", depsS, depsS2)
	}
	if depsS[0] != depsS2[0] || depsS[0] != aS {
		t.Fatalf("U not invariant or not exactly {A}: %v vs %v (A=%s)", depsS, depsS2, aS.Short())
	}
}
