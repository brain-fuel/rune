package core

// Phase 1: the glued NbE evaluator.
//
// Eval sends a core term to the Val domain. References to top-level definitions
// evaluate to GLUED neutrals: the spine keeps the un-unfolded Ref, and the lazy
// unfolding — when forced — goes through the Globals gateway (store.Unfold) and
// logs the unfolded definition's hash into the Machine's write-only dependency
// set. Force IS unfold; the proof-cache instrumentation rides on the laziness
// built for speed (CLAUDE.md, ref_docs/rune-proof-cache-semantics.md).

// Globals is eval/conversion's view of the definition store. store.Store satisfies
// it. TypeOf is pure — a referenced definition's type is part of the content hash
// any referencing term already carries, so reading it logs nothing. Unfold is the
// body gateway; every Machine access to it is logged.
type Globals interface {
	TypeOf(Hash) (Tm, bool)
	Unfold(Hash) (Tm, bool)
}

// Machine carries one checking/evaluation run: the store view and the write-only
// dependency log (the ConvM accumulator of the proof-cache semantics). A Machine
// is created per judgment run — its Deps, read after the run, are exactly the
// definitional dependency set U the certificate is keyed on. Machines and the
// values they produce must not be shared across runs, or a memoized forced thunk
// would swallow a later run's dependency.
type Machine struct {
	G    Globals
	Deps map[Hash]struct{}
	// Metas, when non-nil, resolves metavariable solutions (Phase 2). It is set
	// only during elaboration; pure core runs (the cached checker judgment, REPL
	// normalization) operate on zonked, meta-free terms and leave it nil.
	Metas MetaSolver
}

// MetaSolver resolves a metavariable to its solution value, if solved.
type MetaSolver interface {
	Solution(int) (Val, bool)
}

// NewMachine returns a Machine over g with an empty dependency log.
func NewMachine(g Globals) *Machine {
	return &Machine{G: g, Deps: map[Hash]struct{}{}}
}

// DepList returns the logged dependency set as an unordered slice.
func (m *Machine) DepList() []Hash {
	out := make([]Hash, 0, len(m.Deps))
	for h := range m.Deps {
		out = append(out, h)
	}
	return out
}

// Env is the evaluation environment: index 0 is the innermost binder's value,
// matching Var's de Bruijn indices.
type Env []Val

// Extend pushes v as the new innermost value.
func (e Env) Extend(v Val) Env {
	out := make(Env, 0, len(e)+1)
	out = append(out, v)
	return append(out, e...)
}

// VVar is the fresh-variable value at de Bruijn level l, used by quote and
// conversion to go under binders.
func VVar(l int) Val {
	return VNeu{Spine: NVar{Lvl: l}}
}

// Eval evaluates t in env. Precondition: t is well-scoped in env (every Var index
// is < len(env)) and well-typed; eval of ill-typed core may panic (the checker
// runs first).
func (m *Machine) Eval(env Env, t Tm) Val {
	switch tm := t.(type) {
	case Var:
		return env[tm.Idx]
	case Ref:
		return m.refVal(tm.Hash)
	case Univ:
		return VU{}
	case Pi:
		dom := m.Eval(env, tm.Dom)
		cod := tm.Cod
		return VPi{Name: cod.Name, Icit: tm.Icit, Dom: dom, Cod: func(v Val) Val {
			return m.Eval(env.Extend(v), cod.Body)
		}}
	case Lam:
		body := tm.Body
		return VLam{Name: body.Name, Icit: tm.Icit, Body: func(v Val) Val {
			return m.Eval(env.Extend(v), body.Body)
		}}
	case App:
		return m.apply(m.Eval(env, tm.Fn), m.Eval(env, tm.Arg), tm.Icit)
	case Meta:
		return m.metaVal(tm.ID)
	case Let:
		// The bound value is in-band (part of the term); the binder is
		// definitionally transparent, so the body sees the VALUE.
		return m.Eval(env.Extend(m.Eval(env, tm.Val)), tm.Body.Body)
	case Ann:
		return m.Eval(env, tm.Term)
	default:
		panic("core.Eval: unknown Tm constructor")
	}
}

// refVal builds the glued neutral for a definition reference: the spine is the
// un-unfolded Ref; the lazy unfolding logs the dependency and evaluates the body.
// The thunk memoizes within this Machine's run.
func (m *Machine) refVal(h Hash) Val {
	var memo Val
	return VNeu{Spine: NRef{Hash: h}, Unfold: func() Val {
		if memo == nil {
			m.logDep(h)
			body, ok := m.G.Unfold(h)
			if !ok {
				panic("core: unfolding unknown definition " + h.Short())
			}
			memo = m.Eval(nil, body)
		}
		return memo
	}}
}

func (m *Machine) logDep(h Hash) {
	if m.Deps != nil {
		m.Deps[h] = struct{}{}
	}
}

// Apply applies fn to an explicit arg (the common external entry point).
func (m *Machine) Apply(fn, arg Val) Val { return m.apply(fn, arg, Expl) }

// ApplyIcit applies fn to arg at the given plicity.
func (m *Machine) ApplyIcit(fn, arg Val, icit Icit) Val { return m.apply(fn, arg, icit) }

// apply applies fn to arg. A lambda β-reduces; a neutral grows its spine, and its
// glued unfolding (if any) is applied lazily so forcing the result forces the head.
func (m *Machine) apply(fn, arg Val, icit Icit) Val {
	switch f := fn.(type) {
	case VLam:
		return f.Body(arg)
	case VNeu:
		out := VNeu{Spine: NApp{Fn: f.Spine, Arg: arg, Icit: icit}}
		if f.Unfold != nil {
			u := f.Unfold
			var memo Val
			out.Unfold = func() Val {
				if memo == nil {
					memo = m.apply(u(), arg, icit)
				}
				return memo
			}
		}
		return out
	default:
		panic("core.Apply: applying a non-function (ill-typed core reached eval)")
	}
}

// metaVal builds the value of a metavariable: its solution if solved, otherwise
// a flexible neutral. The solution is consulted lazily via the glued unfolding,
// so a meta solved AFTER this value was built still forces correctly.
func (m *Machine) metaVal(id int) Val {
	return VNeu{Spine: NMeta{ID: id}, Unfold: func() Val {
		if m.Metas != nil {
			if sol, ok := m.Metas.Solution(id); ok {
				return sol
			}
		}
		return VNeu{Spine: NMeta{ID: id}}
	}}
}

// Force unfolds a glued neutral to its delta-reduced value, repeatedly, logging
// each unfolded definition (forcing the thunk is store.Unfold). A flexible
// neutral whose meta is unsolved is returned as-is (its Unfold yields itself).
// Non-neutrals and neutrals with nothing to unfold are returned as-is.
func (m *Machine) Force(v Val) Val {
	for {
		n, ok := v.(VNeu)
		if !ok || n.Unfold == nil {
			return v
		}
		next := n.Unfold()
		if nn, ok := next.(VNeu); ok && nn.Unfold == nil && sameSpine(nn.Spine, n.Spine) {
			return v // an unsolved meta forces to itself; stop
		}
		v = next
	}
}

// sameSpine is a shallow identity check used to detect a no-progress force.
func sameSpine(a, b Neutral) bool {
	x, ok1 := a.(NMeta)
	y, ok2 := b.(NMeta)
	return ok1 && ok2 && x.ID == y.ID
}

// force1 unfolds one glued layer if possible, reporting whether it did.
func (m *Machine) force1(v Val) (Val, bool) {
	if n, ok := v.(VNeu); ok && n.Unfold != nil {
		return n.Unfold(), true
	}
	return v, false
}
