// Package harness is the property-test scaffolding — the gate from day one. It holds
// generators of well-formed surface terms and the Phase-0 invariants (parse∘pretty
// round-trip, hash-invariance under alpha-renaming), plus documented, skipped stubs
// for the invariants Phase 1+ must satisfy. Those stubs are the targets the
// mutation-testing layer will hunt against.
package harness

import (
	"pgregory.net/rapid"

	"goforge.dev/rune/v3/surface"
)

// namePool is a deliberately small alphabet so that generated terms frequently
// shadow, exercising the de Bruijn machinery and the pretty-printer's capture-
// avoiding freshening.
var namePool = []string{"a", "b", "c", "x", "y"}

// GenClosedExp draws a closed, well-scoped surface expression: every variable refers
// to an enclosing binder, so resolution never produces a free reference. This is the
// generator the round-trip and alpha-invariance properties run over.
func GenClosedExp(t *rapid.T) surface.Exp {
	return genExp(t, nil, 4)
}

func genExp(t *rapid.T, scope []string, fuel int) surface.Exp {
	if fuel <= 0 {
		return genAtom(t, scope)
	}
	switch rapid.IntRange(0, 6).Draw(t, "form") {
	case 0:
		return genAtom(t, scope)
	case 1: // lambda: fn (n : Dom) is Body end; Dom is drawn in the outer scope
		n := pickName(t)
		dom := genExp(t, scope, fuel-1)
		return surface.ELam{Param: n, Dom: dom, Body: genExp(t, append(scope, n), fuel-1)}
	case 2: // application
		return surface.EApp{Fn: genExp(t, scope, fuel-1), Arg: genExp(t, scope, fuel-1)}
	case 3: // dependent function type
		n := pickName(t)
		return surface.EPi{
			Param: n,
			Dom:   genExp(t, scope, fuel-1),
			Cod:   genExp(t, append(scope, n), fuel-1),
		}
	case 4: // let
		n := pickName(t)
		let := surface.ELet{
			Name: n,
			Val:  genExp(t, scope, fuel-1),
			Body: genExp(t, append(scope, n), fuel-1),
		}
		if rapid.Bool().Draw(t, "let-ann") {
			let.Ty = genExp(t, scope, fuel-1)
		}
		return let
	case 5: // annotation
		return surface.EAnn{Term: genExp(t, scope, fuel-1), Ty: genExp(t, scope, fuel-1)}
	default: // non-dependent arrow: a Pi whose binder is unused
		n := pickName(t)
		return surface.EPi{
			Param: n,
			Dom:   genExp(t, scope, fuel-1),
			Cod:   genExp(t, scope, fuel-1), // codomain drawn without the binder in scope
		}
	}
}

func genAtom(t *rapid.T, scope []string) surface.Exp {
	if len(scope) > 0 && rapid.Bool().Draw(t, "use-var") {
		i := rapid.IntRange(0, len(scope)-1).Draw(t, "var")
		return surface.EVar{Name: scope[i]}
	}
	return surface.EUniv{}
}

func pickName(t *rapid.T) string {
	return rapid.SampledFrom(namePool).Draw(t, "name")
}

// AlphaRename returns an alpha-equivalent copy of a closed expression with every
// binder consistently renamed to a fresh unique name. Resolving the original and the
// renamed copy must yield identical core (hence identical content hashes), which is
// the hash-invariance-under-alpha property.
func AlphaRename(e surface.Exp) surface.Exp {
	c := &renamer{}
	return c.rename(e, map[string]string{})
}

type renamer struct{ n int }

func (c *renamer) fresh() string {
	c.n++
	return "g" + itoa(c.n)
}

func (c *renamer) rename(e surface.Exp, env map[string]string) surface.Exp {
	switch x := e.(type) {
	case surface.EVar:
		if nn, ok := env[x.Name]; ok {
			return surface.EVar{Name: nn}
		}
		return x
	case surface.EUniv:
		return x
	case surface.ELam:
		nn := c.fresh()
		out := surface.ELam{Param: nn, Body: c.rename(x.Body, extend(env, x.Param, nn))}
		if x.Dom != nil {
			out.Dom = c.rename(x.Dom, env)
		}
		return out
	case surface.EApp:
		return surface.EApp{Fn: c.rename(x.Fn, env), Arg: c.rename(x.Arg, env)}
	case surface.EPi:
		nn := c.fresh()
		return surface.EPi{
			Param: nn,
			Dom:   c.rename(x.Dom, env),
			Cod:   c.rename(x.Cod, extend(env, x.Param, nn)),
		}
	case surface.ELet:
		nn := c.fresh()
		out := surface.ELet{Name: nn, Val: c.rename(x.Val, env), Body: c.rename(x.Body, extend(env, x.Name, nn))}
		if x.Ty != nil {
			out.Ty = c.rename(x.Ty, env)
		}
		return out
	case surface.EAnn:
		return surface.EAnn{Term: c.rename(x.Term, env), Ty: c.rename(x.Ty, env)}
	default:
		return e
	}
}

func extend(env map[string]string, from, to string) map[string]string {
	out := make(map[string]string, len(env)+1)
	for k, v := range env {
		out[k] = v
	}
	out[from] = to
	return out
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	var buf []byte
	for n > 0 {
		buf = append([]byte{byte('0' + n%10)}, buf...)
		n /= 10
	}
	return string(buf)
}
