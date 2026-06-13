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
	// Quot, when non-nil, supplies quotient-builtin roles for the quotient
	// ι-rules (v2): qlift/qind compute when their scrutinee is qin-headed.
	Quot QuotInfo
	// Fib, when non-nil, supplies fibrant-builtin roles for the two-level
	// ι-rules (v3): El decodes, pathJ computes on preflF, castU computes on
	// ureflU and through ua.
	Fib FibInfo
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

// QuotRole classifies a stored hash's role in the quotient builtin group (v2).
// The quotient formers are BUILTIN DEFINITIONS, not new core syntax: permanently
// neutral heads (like datatype constructors) whose eliminators compute by the
// quotient ι-rules below. See store/quot.go for the group and its types.
type QuotRole byte

const (
	// QRoleNone: not a quotient builtin.
	QRoleNone QuotRole = iota
	// QRoleQuot is the type former Quot : (A : U) -> (A -> A -> Prop) -> U.
	QRoleQuot
	// QRoleIn is the point constructor qin : … -> A -> Quot A R.
	QRoleIn
	// QRoleSound is the path introduction qsound : … R a b -> Eq (Quot A R) (qin … a) (qin … b).
	QRoleSound
	// QRoleLift is the non-dependent eliminator qlift : … (f : A -> B) -> (resp : …) -> Quot A R -> B.
	QRoleLift
	// QRoleInd is the Prop-motive induction principle qind : … -> (q : Quot A R) -> P q.
	QRoleInd
)

// QuotInfo gives the evaluator the quotient-builtin roles of stored hashes (v2).
// store.Store implements it. Nil means no quotient builtins (the ι-rules never
// fire).
type QuotInfo interface {
	QuotRoleOf(Hash) QuotRole
}

// FibRole classifies a stored hash's role in the fibrant builtin group (v3):
// the two-level layer's members, builtin definitions like the quotient kit.
// See store/fib.go for the group, its types, and the computation/postulate
// line.
type FibRole byte

const (
	// FRoleNone: not a fibrant builtin.
	FRoleNone FibRole = iota
	// FRoleUF is the fibrant universe UF : U1.
	FRoleUF
	// FRoleEl decodes a fibrant code to its outer type: El : UF -> U.
	FRoleEl
	// FRoleFib embeds a small outer type as fibrant: fib : U -> UF.
	FRoleFib
	// FRolePiF closes UF under Pi.
	FRolePiF
	// FRolePathF is the inner identity type former.
	FRolePathF
	// FRolePrefl is the inner reflexivity preflF.
	FRolePrefl
	// FRoleJ is inner path induction pathJ (computes on preflF).
	FRoleJ
	// FRolePathU is the type of inner paths between fibrant types.
	FRolePathU
	// FRoleUrefl is ureflU : (A : UF) -> pathU A A.
	FRoleUrefl
	// FRoleUa is POSTULATED univalence (an iso yields a pathU).
	FRoleUa
	// FRoleCastU is transport along a pathU (computes on ureflU and ua).
	FRoleCastU
)

// FibInfo gives the evaluator the fibrant-builtin roles of stored hashes (v3).
// store.Store implements it. Nil means no fibrant builtins.
type FibInfo interface {
	FibRoleOf(Hash) FibRole
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
		return VU{Lvl: tm.Lvl}
	case Pi:
		dom := m.Eval(env, tm.Dom)
		cod := tm.Cod
		return VPi{Name: cod.Name, Icit: tm.Icit, Qty: tm.Qty, Dom: dom,
			NonDep: !usesVar(cod.Body, 0),
			Cod: func(v Val) Val {
				return m.Eval(env.Extend(v), cod.Body)
			}}
	case Lam:
		body := tm.Body
		return VLam{Name: body.Name, Icit: tm.Icit, Qty: tm.Qty, Body: func(v Val) Val {
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
//
// A head KNOWN bodiless (a datatype constructor or eliminator, a quotient or
// fibrant builtin) is built RIGID up front — no unfolding thunk at all. Forcing
// such a head was always a no-op (no body is read, no dependency logged), but
// the thunk made every application over it chain a useless unfold closure of
// its own (apply glues lazily through f.Unfold). On constructor chains that
// closure-per-node was the dominant allocation: an n-deep numeral retained n
// thunks that forcing later re-applied one by one.
func (m *Machine) refVal(h Hash) Val {
	if m.rigidHead(h) {
		return VNeu{Spine: NRef{Hash: h}}
	}
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

// rigidHead reports whether h is a definition known bodiless WITHOUT reading
// through the Unfold gateway: datatype constructors and eliminators, and the
// quotient/fibrant builtin groups, are permanently neutral heads. Everything
// else stays on the lazy glued path (a data FORMER, for instance, is also
// bodiless but simply forces to itself there, exactly as before).
func (m *Machine) rigidHead(h Hash) bool {
	if m.Data != nil {
		if _, _, ok := m.Data.CtorOf(h); ok {
			return true
		}
		if _, ok := m.Data.ElimOf(h); ok {
			return true
		}
	}
	if m.Quot != nil && m.Quot.QuotRoleOf(h) != QRoleNone {
		return true
	}
	if m.Fib != nil && m.Fib.FibRoleOf(h) != FRoleNone {
		return true
	}
	return false
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
		if red, ok := m.tryRules(out.Spine); ok {
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

// spineParts flattens a neutral spine into its head and argument values. The
// argument slice is built in a single allocation, filled tail-first — the old
// prepend-and-copy built one slice per spine node, which was the dominant
// allocation in ι-heavy evaluation.
func spineParts(n Neutral) (head Neutral, args []Val) {
	ln := 0
	for s := n; ; {
		app, ok := s.(NApp)
		if !ok {
			break
		}
		ln++
		s = app.Fn
	}
	if ln == 0 {
		return n, nil
	}
	args = make([]Val, ln)
	for i := ln - 1; i >= 0; i-- {
		app := n.(NApp)
		args[i] = app.Arg
		n = app.Fn
	}
	return n, args
}

// usesVar reports whether the bound variable at de Bruijn index idx occurs in
// t. It powers VPi.NonDep: evaluation marks a Pi whose codomain never mentions
// its binder, so callers that only need the result TYPE of an application can
// skip evaluating the argument. The scan is structural and conservative-exact
// over the sealed Tm set.
func usesVar(t Tm, idx int) bool {
	switch tm := t.(type) {
	case Var:
		return tm.Idx == idx
	case Ref, Univ, Prop, Meta:
		return false
	case Pi:
		return usesVar(tm.Dom, idx) || usesVar(tm.Cod.Body, idx+1)
	case Lam:
		return usesVar(tm.Body.Body, idx+1)
	case App:
		return usesVar(tm.Fn, idx) || usesVar(tm.Arg, idx)
	case Let:
		return (tm.Ty != nil && usesVar(tm.Ty, idx)) || usesVar(tm.Val, idx) || usesVar(tm.Body.Body, idx+1)
	case Ann:
		return usesVar(tm.Term, idx) || usesVar(tm.Ty, idx)
	case Eq:
		return usesVar(tm.Ty, idx) || usesVar(tm.L, idx) || usesVar(tm.R, idx)
	case Refl:
		return usesVar(tm.Tm, idx)
	case Cast:
		return usesVar(tm.A, idx) || usesVar(tm.B, idx) || usesVar(tm.P, idx) || usesVar(tm.X, idx)
	case Subst:
		return usesVar(tm.A, idx) || usesVar(tm.X, idx) || usesVar(tm.Y, idx) ||
			usesVar(tm.Prf, idx) || usesVar(tm.P, idx) || usesVar(tm.Px, idx)
	default:
		panic("core.usesVar: unknown Tm constructor")
	}
}

// tryRules fires the ι computation-rule families on a spine just extended by
// apply. The head is found first WITHOUT flattening (an allocation-free walk);
// only when it names a head some rule family could fire on is the spine
// flattened — once, shared by the family — instead of three head-and-argument
// copies per application, which was the dominant constant in ι-heavy
// evaluation. A head cannot belong to two families, so dispatching to exactly
// one preserves the old try-each-in-order behavior.
func (m *Machine) tryRules(spine Neutral) (Val, bool) {
	if m.Data == nil && m.Quot == nil && m.Fib == nil {
		return nil, false
	}
	head := spine
	for {
		app, ok := head.(NApp)
		if !ok {
			break
		}
		head = app.Fn
	}
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	if m.Data != nil {
		if sig, ok := m.Data.ElimOf(ref.Hash); ok {
			_, args := spineParts(spine)
			return m.tryIota(sig, ref.Hash, args, spine)
		}
	}
	if m.Quot != nil {
		if role := m.Quot.QuotRoleOf(ref.Hash); role == QRoleLift || role == QRoleInd {
			_, args := spineParts(spine)
			return m.tryQuotIota(role, args)
		}
	}
	if m.Fib != nil {
		if role := m.Fib.FibRoleOf(ref.Hash); role == FRoleEl || role == FRoleJ || role == FRoleCastU {
			_, args := spineParts(spine)
			return m.tryFibIota(role, ref.Hash, args)
		}
	}
	return nil, false
}

// tryQuotIota fires the quotient computation rules (v2) on a saturated spine:
//
//	qlift A R B f resp (qin A' R' a)  ~>  f a
//	qind  A R P h      (qin A' R' a)  ~>  h a
//
// exactly parallel to tryIota: the scrutinee (the last argument) is forced —
// logged, as always — and the rule fires when it is a saturated qin. The
// respect premise (resp) and the path constructor (qsound) have no computation
// rule: qsound is a proof, definitionally irrelevant, and resp is consumed
// only by the typing judgment. qin and qsound applied to anything are
// permanently neutral (canonical), like datatype constructors.
func (m *Machine) tryQuotIota(role QuotRole, args []Val) (Val, bool) {
	var fnIdx int
	switch role {
	case QRoleLift:
		// qlift A R B f resp q — six arguments, f at index 3.
		if len(args) != 6 {
			return nil, false
		}
		fnIdx = 3
	case QRoleInd:
		// qind A R P h q — five arguments, h at index 3.
		if len(args) != 5 {
			return nil, false
		}
		fnIdx = 3
	default:
		return nil, false
	}
	scrut := m.Force(args[len(args)-1])
	sneu, ok := scrut.(VNeu)
	if !ok {
		return nil, false
	}
	chead, cargs := spineParts(sneu.Spine)
	cref, ok := chead.(NRef)
	if !ok || m.Quot.QuotRoleOf(cref.Hash) != QRoleIn || len(cargs) != 3 {
		return nil, false
	}
	// qin A R a: the carried point is the last argument.
	return m.Apply(args[fnIdx], cargs[2]), true
}

// tryFibIota fires the two-level computation rules (v3) on a saturated spine.
// What computes — and, as deliberately, what does not:
//
//	El (fib A)            ~>  A                                   (decoding)
//	El (piF A B)          ~>  (x : El A) -> El (B x)              (decoding)
//	pathJ A x P d y (preflF _ _)   ~>  d                          (J on refl)
//	castU A B (ureflU _) x         ~>  x                          (transport on refl)
//	castU A B (ua _ _ f _ _ _) x   ~>  f x                        (transport THROUGH ua)
//
// pathJ on a ua-path stays stuck: that is the postulated half of v3's
// univalence, the §F frontier. El (pathF …) stays neutral: the inner path
// type is abstract. Scrutinees are forced — logged, as always — so the
// proof-cache seam is the same one ι-reduction has always used.
func (m *Machine) tryFibIota(role FibRole, h Hash, args []Val) (Val, bool) {
	switch role {
	case FRoleEl:
		if len(args) != 1 {
			return nil, false
		}
		code := m.Force(args[0])
		cneu, ok := code.(VNeu)
		if !ok {
			return nil, false
		}
		chead, cargs := spineParts(cneu.Spine)
		cref, ok := chead.(NRef)
		if !ok {
			return nil, false
		}
		switch m.Fib.FibRoleOf(cref.Hash) {
		case FRoleFib:
			if len(cargs) == 1 {
				return cargs[0], true
			}
		case FRolePiF:
			if len(cargs) == 2 {
				dom := m.Apply(m.refVal(h), cargs[0])
				fam := cargs[1]
				elRef := h
				return VPi{Name: "x", Dom: dom, Cod: func(x Val) Val {
					return m.Apply(m.refVal(elRef), m.Apply(fam, x))
				}}, true
			}
		}
		return nil, false
	case FRoleJ:
		// pathJ A x P d y p — six arguments, d at index 3.
		if len(args) != 6 {
			return nil, false
		}
		if fibHeadIs(m, args[5], FRolePrefl, 2) {
			return args[3], true
		}
		return nil, false
	case FRoleCastU:
		// castU A B p x — four arguments.
		if len(args) != 4 {
			return nil, false
		}
		if fibHeadIs(m, args[2], FRoleUrefl, 1) {
			return args[3], true
		}
		p := m.Force(args[2])
		if pneu, ok := p.(VNeu); ok {
			phead, pargs := spineParts(pneu.Spine)
			if pref, ok := phead.(NRef); ok &&
				m.Fib.FibRoleOf(pref.Hash) == FRoleUa && len(pargs) == 6 {
				return m.Apply(pargs[2], args[3]), true
			}
		}
		return nil, false
	default:
		return nil, false
	}
}

// fibHeadIs reports whether v forces to a saturated spine headed by a fibrant
// builtin with the given role and arity.
func fibHeadIs(m *Machine, v Val, role FibRole, arity int) bool {
	f := m.Force(v)
	n, ok := f.(VNeu)
	if !ok {
		return false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return false
	}
	return m.Fib.FibRoleOf(ref.Hash) == role && len(args) == arity
}

// tryIota fires the eliminator computation rule on a saturated spine
//
//	elim p1..pk motive case1..casen scrutinee
//
// when the scrutinee forces to a saturated constructor C_i of the same
// datatype: the result is case_i applied to the constructor's arguments and
// then to one induction hypothesis (a recursive elimination) per recursive
// argument. Forcing the scrutinee may unfold definitions — logged, as always.
// spine is the un-flattened original; its prefix (everything under the
// scrutinee) is SHARED into each IH's spine rather than rebuilt per node.
func (m *Machine) tryIota(sig ElimSig, h Hash, args []Val, spine Neutral) (Val, bool) {
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
	// ...then to the induction hypotheses for recursive arguments. Each IH is
	// a GLUED NEUTRAL: its spine is the un-reduced recursive elimination, and
	// forcing it runs that elimination (the Apply chain re-enters ι). A case
	// that ignores its IH therefore never computes it — building IHs eagerly
	// made nested eliminations exponential, since the unused IH of one level
	// recursively constructed the unused IHs of every level below it.
	elimPrefix := args[:len(args)-1] // params, motive, cases
	// The applied-eliminator prefix (the spine minus its scrutinee) is shared
	// from the original spine: tryRules only fires on a just-extended NApp, so
	// spine.Fn IS `elim p1..pk motive case1..casen`. Rebuilding it per IH was
	// the per-node spine copy this Machine used to pay on every ι step.
	prefix := spine.(NApp).Fn
	for i, rec := range cs.Rec {
		if !rec {
			continue
		}
		ihSpine := Neutral(NApp{Fn: prefix, Arg: ctorArgs[i], Icit: Expl})
		recArg := ctorArgs[i]
		var memo Val
		ih := VNeu{Spine: ihSpine, Unfold: func() Val {
			if memo == nil {
				v := Val(m.refVal(h))
				for _, a := range elimPrefix {
					v = m.Apply(v, a)
				}
				memo = m.Apply(v, recArg)
			}
			return memo
		}}
		result = m.Apply(result, ih)
	}
	return result, true
}
