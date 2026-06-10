// Package elaborate is the typed pipeline: bidirectional type checking.
//
// Two judgments live here, deliberately separate (ref_docs/
// rune-proof-cache-semantics.md §0):
//
//   - The CORE checker (CheckCore / InferCore) — the judgment the proof cache
//     certifies. On fixed, metavariable-free core it is a deterministic total
//     function of its inputs and the bodies it unfolds; its Machine's dependency
//     log is the certificate's U.
//   - The SURFACE elaborator (Infer / Check) — surface Exp → core Tm, using the
//     binder annotations the grammar carries. This is where Phase-2 metavariables,
//     pattern unification, and implicit insertion will slot in; Phase 1 keeps it
//     deterministic (no metas), so its output equals name resolution's output on
//     well-typed programs and hashing is unaffected.
//
// Both drive the same glued NbE (core.Machine): eval, quote, conversion.
package elaborate

import (
	"fmt"

	"goforge.dev/rune/core"
	"goforge.dev/rune/equality"
	"goforge.dev/rune/surface"
)

// Elaborator holds one elaboration/checking run: the NbE machine (with its
// dependency log), the name→hash map for free identifiers, and the hash→name map
// for error messages.
type Elaborator struct {
	M        *core.Machine
	Refs     map[string]core.Hash
	RefNames map[core.Hash]string
	metas    metaState // Phase 2: this run's metavariables
}

// New returns an Elaborator over globals g with the given name maps.
func New(g core.Globals, refs map[string]core.Hash, refNames map[core.Hash]string) *Elaborator {
	e := &Elaborator{M: core.NewMachine(g), Refs: refs, RefNames: refNames}
	e.M.Metas = &e.metas
	e.M.EqS = equality.Default()
	return e
}

// Ctx is the typing context: parallel innermost-first lists of binder names,
// types (as values), and values (the eval environment). A lambda/Pi binder's
// value is a fresh variable; a let binder's value is the bound value, making lets
// definitionally transparent.
type Ctx struct {
	names []string
	types []core.Val
	env   core.Env
	bound []bool // true for lambda/Pi binders, false for let definitions
}

// Lvl is the number of binders in scope — the next fresh de Bruijn level.
func (c *Ctx) Lvl() int { return len(c.env) }

// bind extends the context with a fresh variable of type ty (lambda/Pi binders).
func (c *Ctx) bind(name string, ty core.Val) *Ctx {
	return c.push(name, ty, core.VVar(c.Lvl()), true)
}

// define extends the context with a binder bound to a known value (let binders).
func (c *Ctx) define(name string, ty, val core.Val) *Ctx {
	return c.push(name, ty, val, false)
}

func (c *Ctx) push(name string, ty, val core.Val, isBound bool) *Ctx {
	names := make([]string, 0, len(c.names)+1)
	names = append(names, name)
	names = append(names, c.names...)
	types := make([]core.Val, 0, len(c.types)+1)
	types = append(types, ty)
	types = append(types, c.types...)
	env := make(core.Env, 0, len(c.env)+1)
	env = append(env, val)
	env = append(env, c.env...)
	bound := make([]bool, 0, len(c.bound)+1)
	bound = append(bound, isBound)
	bound = append(bound, c.bound...)
	return &Ctx{names: names, types: types, env: env, bound: bound}
}

// lookup finds a bound name, returning its de Bruijn index and type.
func (c *Ctx) lookup(name string) (int, core.Val, bool) {
	for i, n := range c.names {
		if n == name {
			return i, c.types[i], true
		}
	}
	return 0, nil, false
}

// Eval evaluates a core term in this context's environment.
func (e *Elaborator) Eval(c *Ctx, t core.Tm) core.Val { return e.M.Eval(c.env, t) }

// pretty renders a type value for an error message.
func (e *Elaborator) pretty(c *Ctx, v core.Val) string {
	return surface.PrettyWith(e.M.Quote(c.Lvl(), v), e.RefNames)
}

// prettyTm renders a core term for an error message.
func (e *Elaborator) prettyTm(t core.Tm) string {
	return surface.PrettyWith(t, e.RefNames)
}

// refType returns the evaluated public type of the definition at h. Reading a
// type is pure (no dependency logged); evaluating it can only force inside the
// type, which is in-band.
func (e *Elaborator) refType(h core.Hash) (core.Val, error) {
	ty, ok := e.M.G.TypeOf(h)
	if !ok || ty == nil {
		name := e.RefNames[h]
		return nil, fmt.Errorf("definition %s has no stored type", name)
	}
	return e.M.Eval(nil, ty), nil
}
