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
	// EqS, when non-nil, supplies the equality stratum's reduction rules.
	EqS EqStratum
	// Data, when non-nil, supplies datatype roles for ι-reduction.
	Data DataInfo
}

// MetaSolver resolves a metavariable to its solution value, if solved.
type MetaSolver interface {
	Solution(int) (Val, bool)
}

// CtorSig describes one constructor's arguments: its arity (after the data
// type's parameters) and which of those arguments are recursive occurrences of
// the datatype (the positions that get induction hypotheses).
type CtorSig struct {
	Arity int
	Rec   []bool
}

// ElimSig describes an eliminator: the datatype it eliminates, the number of
// datatype parameters, and the constructor signatures in declaration order.
// The eliminator's argument layout is
//
//	elim p1..pk motive case1..casen scrutinee
//
// and the ι-rule fires when the scrutinee forces to a saturated constructor.
type ElimSig struct {
	Data      Hash
	NumParams int
	Ctors     []CtorSig
}

// DataInfo gives the evaluator the datatype roles of stored hashes (Phase 4).
// store.Store implements it. Nil means no datatypes (ι never fires).
type DataInfo interface {
	CtorOf(Hash) (data Hash, idx int, ok bool)
	ElimOf(Hash) (ElimSig, bool)
}

// EqStratum is the EQUALITY STRATUM's reduction hooks (Phase 3). The conversion
// checker and evaluator call these when they reach an equality former; the one
// v1 implementation is equality.Observational (Pujet–Tabareau). Nil means the
// formers are stuck (pre-Phase-3 behavior).
type EqStratum interface {
	// EvalEq computes the value of Eq ty l r, applying the stratum's
	// type-directed reductions (e.g. funext: Eq over a Pi unfolds pointwise).
	EvalEq(m *Machine, ty, l, r Val) Val
	// EvalCast computes cast a b p x, never inspecting p.
	EvalCast(m *Machine, a, b, p, x Val) Val
	// EvalSubst computes subst a x y prf P px, never inspecting prf.
	EvalSubst(m *Machine, a, x, y, prf, pmot, px Val) Val
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
	case Prop:
		return VProp{}
	case Eq:
		return m.EvalEq(m.Eval(env, tm.Ty), m.Eval(env, tm.L), m.Eval(env, tm.R))
	case Refl:
		return VRefl{V: m.Eval(env, tm.Tm)}
	case Cast:
		return m.EvalCast(m.Eval(env, tm.A), m.Eval(env, tm.B), m.Eval(env, tm.P), m.Eval(env, tm.X))
	case Subst:
		return m.EvalSubst(m.Eval(env, tm.A), m.Eval(env, tm.X), m.Eval(env, tm.Y),
			m.Eval(env, tm.Prf), m.Eval(env, tm.P), m.Eval(env, tm.Px))
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
			body, ok := m.G.Unfold(h)
			if !ok {
				// Bodiless (a datatype former, constructor, or eliminator) or
				// unknown: permanently rigid. No body was read, so no
				// dependency is logged.
				memo = VNeu{Spine: NRef{Hash: h}}
				return memo
			}
			m.logDep(h)
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
		// ι: a saturated eliminator applied to a constructor-headed scrutinee
		// computes. This is as much a computation rule as β; it fires eagerly.
		if red, ok := m.tryIota(out.Spine); ok {
			return red
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

// EvalEq applies the equality stratum's Eq reduction, or leaves the type stuck.
func (m *Machine) EvalEq(ty, l, r Val) Val {
	if m.EqS != nil {
		return m.EqS.EvalEq(m, ty, l, r)
	}
	return VEq{Ty: ty, L: l, R: r}
}

// EvalCast applies the equality stratum's cast reduction, or leaves it stuck.
func (m *Machine) EvalCast(a, b, p, x Val) Val {
	if m.EqS != nil {
		return m.EqS.EvalCast(m, a, b, p, x)
	}
	return VNeu{Spine: NCast{A: a, B: b, P: p, X: x}}
}

// EvalSubst applies the equality stratum's transport, or leaves it stuck.
func (m *Machine) EvalSubst(a, x, y, prf, pmot, px Val) Val {
	if m.EqS != nil {
		return m.EqS.EvalSubst(m, a, x, y, prf, pmot, px)
	}
	return VNeu{Spine: NSubst{A: a, X: x, Y: y, Prf: prf, P: pmot, Px: px}}
}

// spineParts flattens a neutral spine into its head and argument values.
func spineParts(n Neutral) (head Neutral, args []Val) {
	for {
		app, ok := n.(NApp)
		if !ok {
			return n, args
		}
		args = append([]Val{app.Arg}, args...)
		n = app.Fn
	}
}

// tryIota fires the eliminator computation rule on a saturated spine
//
//	elim p1..pk motive case1..casen scrutinee
//
// when the scrutinee forces to a saturated constructor C_i of the same
// datatype: the result is case_i applied to the constructor's arguments and
// then to one induction hypothesis (a recursive elimination) per recursive
// argument. Forcing the scrutinee may unfold definitions — logged, as always.
func (m *Machine) tryIota(spine Neutral) (Val, bool) {
	if m.Data == nil {
		return nil, false
	}
	head, args := spineParts(spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	sig, ok := m.Data.ElimOf(ref.Hash)
	if !ok {
		return nil, false
	}
	want := sig.NumParams + 1 + len(sig.Ctors) + 1
	if len(args) != want {
		return nil, false
	}
	scrut := m.Force(args[len(args)-1])
	sneu, ok := scrut.(VNeu)
	if !ok {
		return nil, false
	}
	chead, cargs := spineParts(sneu.Spine)
	cref, ok := chead.(NRef)
	if !ok {
		return nil, false
	}
	cdata, idx, ok := m.Data.CtorOf(cref.Hash)
	if !ok || cdata != sig.Data || idx >= len(sig.Ctors) {
		return nil, false
	}
	cs := sig.Ctors[idx]
	if len(cargs) != sig.NumParams+cs.Arity {
		return nil, false // under-applied constructor: stuck
	}
	ctorArgs := cargs[sig.NumParams:]

	// case_i applied to the constructor arguments...
	result := args[sig.NumParams+1+idx]
	for _, a := range ctorArgs {
		result = m.Apply(result, a)
	}
	// ...then to the induction hypotheses for recursive arguments.
	elimPrefix := args[:len(args)-1] // params, motive, cases
	for i, rec := range cs.Rec {
		if !rec {
			continue
		}
		var ih Val = m.refVal(ref.Hash)
		for _, a := range elimPrefix {
			ih = m.Apply(ih, a)
		}
		ih = m.Apply(ih, ctorArgs[i])
		result = m.Apply(result, ih)
	}
	return result, true
}
