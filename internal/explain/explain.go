// Package explain renders a checked definition (or a REPL expression) as a
// deterministic tree of English steps. One renderer, three thin frontends
// (the `rune explain` CLI, the REPL `:explain` command, and the --annotate
// views): each frontend only formats the Step tree this package builds.
//
// Structure comes from the retained, import-qualified SURFACE definition
// (session.SurfaceDef) so the output matches what the reader sees on screen;
// type facts come from the elaborated core (session.Defs). Depth controls
// expansion only: the same tree shape underlies every view. Output is
// compiler-derived and golden-tested; no LLM is involved.
package explain

import (
	"fmt"
	"strings"

	"goforge.dev/rune/v3/core"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/surface"
)

// Step is one English step. Text is the step's sentence WITHOUT the square
// brackets (renderers add them). Code is the source fragment the step came
// from (the --annotate views pair it with the English; empty for steps with
// no code counterpart). Kids are sub-steps: inlined callee bodies, case
// branches, lambda bodies.
type Step struct {
	Text string
	Code string
	Kids []Step
}

// Options selects the view depth.
type Options struct {
	// Depth is the user-definition inline budget: 0 (default) explains only
	// the target's top-level steps, with calls one line each; n > 0 inlines
	// called user definitions n levels deep. Prims, foreign axioms, and
	// builtin-accelerated definitions stay one-line at every depth.
	Depth int
	// Core renders the elaborated core walk instead of the surface tree: the
	// nothing-hidden view. Implicits visible (braces), sugar expanded,
	// underscore binders shown, numerals as literals, no English templates.
	Core bool
}

// Explain renders the named definition from the session as a Step tree.
func Explain(s *session.Session, name string, opts Options) (Step, error) {
	defs := defMap(s)
	d, ok := defs[name]
	if !ok {
		return Step{}, fmt.Errorf("explain: no definition named %q", name)
	}
	root := Step{
		Text: "Entrypoint: " + short(name),
		Code: short(name) + " : " + printCore(d.Ty, nil, s.RefNames()),
	}
	if opts.Core {
		if d.Body == nil {
			return Step{}, fmt.Errorf("explain: %q has no body to walk (a foreign axiom or postulate)", name)
		}
		root.Kids = coreSteps(s, d.Body, nil)
		return root, nil
	}
	sd, ok := s.SurfaceDef(name)
	if !ok {
		return Step{}, fmt.Errorf("explain: %q has no surface body to explain (a foreign axiom, builtin, or constructor)", name)
	}
	c := &ctx{s: s, defs: defs, depth: opts.Depth}
	root.Kids = c.steps(sd.Body, "")
	return root, nil
}

// ExplainExp renders a bare surface expression (the REPL $N path).
func ExplainExp(s *session.Session, e surface.Exp, opts Options) (Step, error) {
	if opts.Core {
		tm, _, err := s.ElabExpr(e)
		if err != nil {
			return Step{}, err
		}
		return Step{Text: "Expression", Kids: coreSteps(s, tm, nil)}, nil
	}
	c := &ctx{s: s, defs: defMap(s), depth: opts.Depth}
	return Step{Text: "Expression", Code: printExp(e), Kids: c.steps(e, "")}, nil
}

func defMap(s *session.Session) map[string]session.Def {
	m := make(map[string]session.Def)
	for _, d := range s.Defs() {
		m[d.Name] = d
	}
	return m
}

// ctx carries one Explain run: the session (types, retained surface defs),
// the name -> core def map, and the remaining inline budget.
type ctx struct {
	s     *session.Session
	defs  map[string]session.Def
	depth int
}

// steps renders expression e as a flat list of sibling steps. binder is the
// name the surrounding bindIO chain or let gives e's result; "" means
// unnamed, and names starting with "_" are treated as unnamed.
func (c *ctx) steps(e surface.Exp, binder string) []Step {
	if strings.HasPrefix(binder, "_") {
		binder = ""
	}
	head, args := spine(e)
	if v, ok := head.(surface.EVar); ok {
		n := short(v.Name)
		expl := explicitArgs(args)
		if n == "bindIO" && len(expl) == 4 {
			return c.bindSteps(e, expl, binder)
		}
		if t, ok := primTemplates[n]; ok && len(expl) >= t.args {
			return c.primSteps(t, e, expl, binder)
		}
		if _, ok := c.defs[v.Name]; ok {
			return []Step{c.callStep(v.Name, expl, e, binder)}
		}
	}
	switch x := e.(type) {
	case surface.ELam:
		return []Step{{
			Text: "Given `" + x.Param + "`:",
			Code: "fn (" + x.Param + " : " + printExp(x.Dom) + ")",
			Kids: c.steps(x.Body, ""),
		}}
	case surface.ECase:
		var out []Step
		for _, cl := range x.Clauses {
			pat := cl.Ctor
			if len(cl.Binders) > 0 {
				pat += " " + strings.Join(cl.Binders, " ")
			}
			out = append(out, Step{
				Text: "When (" + printExp(x.Scrut) + ") is (" + pat + "):",
				Code: "| " + pat + " -> " + printExp(cl.Body),
				Kids: c.steps(cl.Body, ""),
			})
		}
		return out
	case surface.ELet:
		return append(c.steps(x.Val, x.Name), c.steps(x.Body, binder)...)
	case surface.ESeqBind:
		return append(c.steps(x.Val, x.Name), c.steps(x.Body, binder)...)
	case surface.EAnn:
		return c.steps(x.Term, binder)
	}
	return []Step{fallbackStep(e, binder)}
}

// bindSteps flattens `bindIO A B m k`: m's step carries k's parameter as its
// binder, then k's body continues as SIBLING steps. The word bindIO never
// appears at any surface depth. A chain that is itself let-bound nests under
// one "Compute `x` by:" step so the name is not lost.
func (c *ctx) bindSteps(e surface.Exp, expl []surface.Exp, binder string) []Step {
	m, k := expl[2], expl[3]
	var out []Step
	if lam, ok := k.(surface.ELam); ok {
		out = append(c.steps(m, lam.Param), c.steps(lam.Body, "")...)
	} else {
		out = append(c.steps(m, ""), Step{
			Text: "Pass the Result to (" + printExp(k) + ")",
			Code: printExp(k),
		})
	}
	if binder != "" {
		return []Step{{Text: "Compute `" + binder + "` by:", Code: printExp(e), Kids: out}}
	}
	return out
}

// primSteps renders a host-op application through its English template
// (rendering rule 4: atomic argument inline, compound argument as a
// preceding Apply Function step whose slot reads Result).
func (c *ctx) primSteps(t primTemplate, e surface.Exp, expl []surface.Exp, binder string) []Step {
	take := expl[len(expl)-t.args:]
	var pre []Step
	slots := make([]any, 0, t.args)
	for _, a := range take {
		if isAtomExp(a) {
			slots = append(slots, "("+printExp(a)+")")
			continue
		}
		pre = append(pre, Step{Text: "Apply Function (" + printExp(a) + ")", Code: printExp(a)})
		slots = append(slots, "Result")
	}
	text := fmt.Sprintf(t.verb, slots...)
	if binder != "" {
		if t.bound != "" && t.args == 0 {
			text = fmt.Sprintf(t.bound, binder)
		} else {
			text += " as `" + binder + "`"
		}
	}
	return append(pre, Step{Text: text, Code: printExp(e)})
}

// callStep renders a call to a user definition (or a bodiless axiom without
// a template) as one line, named and typed via the elaborator.
// With a positive depth budget the callee's retained surface body inlines as
// Kids, one budget unit per level; prims, foreign axioms (no surface body),
// and builtin-accelerated defs stay one-line. The decrementing budget bounds
// recursion through partial/self-referential definitions.
func (c *ctx) callStep(full string, expl []surface.Exp, e surface.Exp, binder string) Step {
	d := c.defs[full]
	text := "Apply `" + short(full) + "`"
	if len(expl) > 0 {
		parts := make([]string, len(expl))
		for i, a := range expl {
			parts[i] = "(" + printExp(a) + ")"
		}
		text += " to " + strings.Join(parts, " and ")
	}
	text += " (" + typeSummary(c.s, d.Ty) + ")"
	if binder != "" {
		text += " as `" + binder + "`"
	}
	st := Step{Text: text, Code: printExp(e)}
	if c.depth > 0 && !c.s.Accelerated(full) {
		if sd, ok := c.s.SurfaceDef(full); ok {
			sub := &ctx{s: c.s, defs: c.defs, depth: c.depth - 1}
			st.Kids = sub.steps(sd.Body, "")
		}
	}
	return st
}

// fallbackStep is the bracket-code form for anything without a template: an
// application renders as Apply Function (...), an atom as Result: (...).
func fallbackStep(e surface.Exp, binder string) Step {
	text := "Result: (" + printExp(e) + ")"
	if _, ok := e.(surface.EApp); ok {
		text = "Apply Function (" + printExp(e) + ")"
	}
	if binder != "" {
		text += " as `" + binder + "`"
	}
	return Step{Text: text, Code: printExp(e)}
}

// spineArg is one application argument with its plicity.
type spineArg struct {
	e    surface.Exp
	icit core.Icit
}

// spine flattens a left-associated application into head + arguments.
func spine(e surface.Exp) (surface.Exp, []spineArg) {
	var args []spineArg
	for {
		a, ok := e.(surface.EApp)
		if !ok {
			break
		}
		args = append([]spineArg{{a.Arg, a.Icit}}, args...)
		e = a.Fn
	}
	return e, args
}

func explicitArgs(args []spineArg) []surface.Exp {
	var out []surface.Exp
	for _, a := range args {
		if a.icit == core.Expl {
			out = append(out, a.e)
		}
	}
	return out
}

// typeSummary phrases a definition's core type: "takes A and B, gives C".
// Explicit Pi domains only; implicit parameters are skipped. printCore
// carries the binder environment so dependent codomains print their names.
func typeSummary(s *session.Session, ty core.Tm) string {
	var doms []string
	var env []string
	rn := s.RefNames()
	for {
		pi, ok := ty.(core.Pi)
		if !ok {
			break
		}
		if pi.Icit == core.Expl {
			doms = append(doms, printCore(pi.Dom, env, rn))
		}
		env = append(env, scopeName(pi.Cod))
		ty = pi.Cod.Body
	}
	ret := printCore(ty, env, rn)
	if len(doms) == 0 {
		return "gives " + ret
	}
	return "takes " + joinAnd(doms) + ", gives " + ret
}

func joinAnd(xs []string) string {
	switch len(xs) {
	case 0:
		return ""
	case 1:
		return xs[0]
	}
	return strings.Join(xs[:len(xs)-1], ", ") + " and " + xs[len(xs)-1]
}
