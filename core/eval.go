package core

import "math/big"

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
	// Iv, when non-nil, supplies interval-builtin roles for the De Morgan
	// ι-rules (§F phase 1): ineg/imin/imax compute on the endpoints i0/i1.
	Iv IntervalInfo
	// Pa, when non-nil, supplies path-builtin roles for the cubical path
	// ι-rules (§F phase 2): papp computes by β, refl, and endpoint boundary.
	Pa PathInfo
	// Fc, when non-nil, supplies face-builtin roles for the face-lattice
	// ι-rules (§F phase 3a): ieq0/ieq1/fand/for compute on endpoints and ⊤/⊥.
	Fc FaceInfo
	// Sy, when non-nil, supplies cofibration-validity roles (§F phase 3b):
	// holds and its canonical intros (rigid heads; no ι-rules of their own).
	Sy SysInfo
	// Kn, when non-nil, supplies Kan-builtin roles for the Kan ι-rules (§F
	// phase 3c+): transp computes the regularity (constant type-line) case.
	Kn KanInfo
	// Si, when non-nil, supplies inner-Sigma roles (§F / R-SIGMA / A5): fstF/sndF
	// compute by ι on a pairF intro; sigmaF/pairF are rigid heads.
	Si SigmaInfo
	// Cn, when non-nil, supplies coinductive roles (R-COIND / C5a): out computes
	// by ι on an unfold; Nu/unfold are rigid heads.
	Cn CoindInfo
	// Gd, when non-nil, supplies guarded-recursion roles (R-COIND / C5b): Later /
	// next / dfix. Heads are rigid in eval; dfix unfolds once in convDfix.
	Gd GuardInfo
	// Gl, when non-nil, supplies inner-Glue roles (§F / R-GLUE / A6): unglue
	// computes by ι (β on a glue intro, the equivFun boundary on ⊤); Glue/glue
	// are rigid heads, and El (Glue A ⊤ T e) decodes to El (T htop).
	Gl GlueInfo
	// Fs, when non-nil, supplies the face-split eliminator role (§F / R-BOX / A3):
	// fsplit computes by face-ι — `fsplit A φ ψ u v h ~> u htop` when φ ≡ ⊤,
	// `~> v htop` when ψ ≡ ⊤. The dispatch primitive the Kan rules build systems
	// from (a `holds (for φ ψ)` eliminator); rigid head otherwise.
	Fs FsplitInfo
	// SyU, when non-nil, supplies the UF-valued face-split role (§F / R-BOX / A8):
	// sysU computes by face-ι — `sysU φ ψ u v h ~> u htop` when φ ≡ ⊤, `~> v htop`
	// when ψ ≡ ⊤. The type-level counterpart of fsplit (a partial TYPE selector);
	// rigid head otherwise.
	SyU SysUInfo
	// FsD, when non-nil, supplies the dependent face-split role (§F / R-BOX / A8):
	// fsplitD computes by face-ι like fsplit, but carries a motive so its branch
	// (and result) types may depend on the face — the e-component of a univalence
	// Glue line; rigid head otherwise.
	FsD SplitDInfo
	// Fa, when non-nil, supplies the ∀-cofibration role (§F / R-GLUE / A6):
	// forallF computes `forallF (λ_. ⊤) ~> ⊤`; rigid head otherwise.
	Fa ForallInfo
	// Pu, when non-nil, supplies the type-level path-abstraction role (§F / R-UA /
	// A7): pabsU is a rigid pathU intro, and castU along it computes via transp.
	Pu PabsUInfo
	// PpU, when non-nil, supplies the type-level path-application role (§F / R-UA /
	// A7): pappU applies a pathU at an interval point, computing by β/refl/boundary
	// like papp; rigid head otherwise.
	PpU PappUInfo
	// Hi, when non-nil, supplies inner-HIT roles (§F / R-HIT / A9): the circle's
	// recursor circElim computes by ι on base and along loop; Circle/base/loop are
	// rigid heads.
	Hi HitInfo
	// Su, when non-nil, supplies suspension-HIT roles (§F / R-HIT / A9): suspElim
	// computes by ι on north/south and along merid; Susp/north/south/merid rigid.
	Su SuspInfo
	// Qh, when non-nil, supplies fibrant-quotient-HIT roles (§F / R-HIT / A9):
	// quotElim computes by ι on qinc and along qrel; Quotient/qinc/qrel rigid.
	Qh QuotHitInfo
	// Pp, when non-nil, supplies dependent-path roles (§F / R-HIT / A9): pappP
	// computes by β and boundary; pathP/pabsP are rigid heads.
	Pp PathPInfo
	// Ci, when non-nil, supplies the circle's dependent eliminator (§F / R-HIT /
	// A9): circInd computes by ι on base and along loop into a dependent motive.
	Ci CircIndInfo
	// SuI, when non-nil, supplies the suspension's dependent eliminator (A9):
	// suspInd computes by ι on north/south and along merid into a dependent motive.
	SuI SuspIndInfo
	// QuI, when non-nil, supplies the fibrant quotient's dependent eliminator (A9):
	// quotInd computes by ι on qinc and along qrel into a dependent motive.
	QuI QuotIndInfo
	// Tr, when non-nil, supplies set-truncation roles (§F / R-HIT / A9 dim-2):
	// trunc0Rec computes on inc; Trunc0/inc/squash are rigid heads.
	Tr TruncInfo
	// Na, when non-nil, supplies the opt-in kernel ACCELERATION table for nat
	// arithmetic (C7 / R-NUM, Decision 1): a `builtin natAdd add` (etc.)
	// registers the def's content hash with a NatOp. The fast-path ι fires ONLY
	// when every argument forces to a VNatLit, computing the result in one bigint
	// step (a single VNatLit). On any neutral argument it does NOT fire — the def
	// reduces by its ordinary recursive body (the NatLit-peeling eliminator), so
	// open-term reasoning is byte-identical and the normalizer cannot get stuck
	// differently. The bridge to the unfolded definition is refl (the accelerated
	// op IS the user's own def); agreement is the differential-tested soundness
	// gate, not a trust assumption.
	Na NatAccelInfo
	// Pt, when non-nil, reports which definition hashes are `partial` (C4): a
	// general-recursive definition whose head is PERMANENTLY NEUTRAL in
	// eval/conversion — the firewall that keeps the normalizer terminating. Its
	// body is reachable only through codegen's direct Unfold (it runs, it does
	// not reduce during type-checking). The total fragment is unaffected.
	Pt PartialInfo
}

// PartialInfo reports whether a stored hash is a `partial` (general-recursive)
// definition. store.Store implements it.
type PartialInfo interface {
	IsPartial(Hash) bool
}

// NatOp names a kernel-accelerated nat arithmetic operation (C7 / R-NUM). Each
// is the bigint short-circuit of the user's OWN binary nat function on closed
// literal inputs; the bridge to the recursive definition is refl.
type NatOp byte

const (
	// NatOpNone: not an accelerated op.
	NatOpNone NatOp = iota
	// NatOpAdd: addition (a + b).
	NatOpAdd
	// NatOpMul: multiplication (a * b).
	NatOpMul
	// NatOpMonus: truncated subtraction (max(a-b, 0)).
	NatOpMonus
	// NatOpDiv: flooring quotient (a // b), with a // 0 = 0 (matching the prelude's
	// fuel divF). The accel that makes packed-String uncons (n div 256) and the
	// tower's `//` fast on large literals.
	NatOpDiv
	// NatOpMod: remainder (a % b), with a % 0 = a (matching the prelude's fuel
	// modF). The companion for n mod 256.
	NatOpMod
)

// NatAccelInfo reports the accelerated nat operation a stored hash names, and the
// session's nat constructor hashes so the accel rule can build its VNatLit result
// (a literal is relative to a particular `builtin nat`). A nil NatAccelInfo (or
// NatOpNone) means no acceleration: arithmetic runs by the def's ordinary body.
type NatAccelInfo interface {
	NatOpOf(Hash) NatOp
	NatCtors() (zero, succ Hash, ok bool)
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
	// FRoleCastU is transport along a pathU. It computes on ureflU (identity) and,
	// through the derived `ua` (a pabsU-Glue line, ambient prelude), on a univalence
	// path via the genuine transp-over-Glue arm. There is no postulated `ua` head.
	FRoleCastU
)

// FibInfo gives the evaluator the fibrant-builtin roles of stored hashes (v3).
// store.Store implements it. Nil means no fibrant builtins.
type FibInfo interface {
	FibRoleOf(Hash) FibRole
	// FibHash is the reverse lookup: the hash of the member with the given role,
	// for the structural Kan rules to CONSTRUCT fibrant sub-terms (El, piF, …).
	FibHash(FibRole) (Hash, bool)
}

// IntervalRole classifies a stored hash's role in the interval builtin group
// (§F phase 1): the cubical interval as a De Morgan algebra, a builtin group
// like the quotient and fibrant kits. See store/interval.go.
type IntervalRole byte

const (
	// IRoleNone: not an interval builtin.
	IRoleNone IntervalRole = iota
	// IRoleI is the interval I : U (a neutral type former).
	IRoleI
	// IRoleI0 is the endpoint i0 : I.
	IRoleI0
	// IRoleI1 is the endpoint i1 : I.
	IRoleI1
	// IRoleNeg is reversal ineg : I -> I.
	IRoleNeg
	// IRoleMin is the ∧ connection imin : I -> I -> I.
	IRoleMin
	// IRoleMax is the ∨ connection imax : I -> I -> I.
	IRoleMax
)

// IntervalInfo gives the evaluator the interval-builtin roles of stored hashes
// (§F phase 1). store.Store implements it. Nil means no interval builtins (the
// De Morgan ι-rules never fire). IntervalHash is the reverse lookup the ι-rules
// need to PRODUCE an endpoint value (ineg i0 ~> i1 returns the i1 value).
type IntervalInfo interface {
	IntervalRoleOf(Hash) IntervalRole
	IntervalHash(IntervalRole) (Hash, bool)
}

// PathRole classifies a stored hash's role in the cubical path builtin group
// (§F phase 2): inner paths as interval functions. See store/path.go.
type PathRole byte

const (
	// PRoleNone: not a path builtin.
	PRoleNone PathRole = iota
	// PRoleAbs is path abstraction pabs (a canonical intro; neutral).
	PRoleAbs
	// PRoleApp is path application papp (computes by β, refl, and boundary).
	PRoleApp
)

// PathInfo gives the evaluator the path-builtin roles of stored hashes (§F
// phase 2). store.Store implements it. Nil means no path builtins (papp never
// computes).
type PathInfo interface {
	PathRoleOf(Hash) PathRole
	PathHash(PathRole) (Hash, bool)
}

// PathPRole classifies a stored hash's role in the DEPENDENT path builtin group
// (§F / R-HIT / A9 prerequisite): paths over a type-line `A : I -> UF`, with
// endpoints in different fibers. See store/pathp.go.
type PathPRole byte

const (
	// PPRoleNone: not a dependent-path builtin.
	PPRoleNone PathPRole = iota
	// PPRolePathP is the dependent path code former (a fibrant former; neutral).
	PPRolePathP
	// PPRoleAbs is dependent path abstraction pabsP (a canonical intro; neutral).
	PPRoleAbs
	// PPRoleApp is dependent path application pappP (computes by β and boundary).
	PPRoleApp
)

// PathPInfo gives the evaluator the dependent-path roles of stored hashes (A9).
// store.Store implements it. Nil means no dependent-path builtins.
type PathPInfo interface {
	PathPRoleOf(Hash) PathPRole
	PathPHash(PathPRole) (Hash, bool)
}

// FaceRole classifies a stored hash's role in the face-lattice builtin group
// (§F phase 3a): cofibrations as a distributive lattice. See store/face.go.
type FaceRole byte

const (
	// CRoleNone: not a face builtin.
	CRoleNone FaceRole = iota
	// CRoleF is the type of cofibrations F : U.
	CRoleF
	// CRoleEq0 is the atomic constraint (i = 0): ieq0 : I -> F.
	CRoleEq0
	// CRoleEq1 is the atomic constraint (i = 1): ieq1 : I -> F.
	CRoleEq1
	// CRoleAnd is conjunction fand : F -> F -> F.
	CRoleAnd
	// CRoleOr is disjunction for : F -> F -> F.
	CRoleOr
	// CRoleTop is ⊤ (ftop : F).
	CRoleTop
	// CRoleBot is ⊥ (fbot : F).
	CRoleBot
)

// FaceInfo gives the evaluator the face-builtin roles of stored hashes (§F
// phase 3a). store.Store implements it. Nil means no face builtins. FaceHash is
// the reverse lookup the ι-rules need to PRODUCE ftop/fbot.
type FaceInfo interface {
	FaceRoleOf(Hash) FaceRole
	FaceHash(FaceRole) (Hash, bool)
}

// SysRole classifies a stored hash's role in the cofibration-validity builtin
// group (§F phase 3b): holds and its intros. See store/sys.go. The members are
// canonical (no ι-rules of their own); they need only be recognized as rigid
// heads so the evaluator does not try to unfold their (absent) bodies.
type SysRole byte

const (
	// SRoleNone: not a systems builtin.
	SRoleNone SysRole = iota
	// SRoleHolds is holds : F -> Prop.
	SRoleHolds
	// SRoleTop is htop : holds ftop.
	SRoleTop
	// SRoleAnd is hand (validity of a conjunction).
	SRoleAnd
	// SRoleOrL is horl (left injection into a disjunction's validity).
	SRoleOrL
	// SRoleOrR is horr (right injection).
	SRoleOrR
)

// SysInfo gives the evaluator the cofibration-validity roles of stored hashes
// (§F phase 3b). store.Store implements it. Nil means no systems builtins.
type SysInfo interface {
	SysRoleOf(Hash) SysRole
	SysHash(SysRole) (Hash, bool)
}

// KanRole classifies a stored hash's role in the Kan-operation builtin group
// (§F phase 3c onward): transp (and later hcomp/comp). See store/kan.go.
type KanRole byte

const (
	// KRoleNone: not a Kan builtin.
	KRoleNone KanRole = iota
	// KRoleTransp is transport along a line of fibrant types.
	KRoleTransp
	// KRoleHcomp is homogeneous composition (fill a box, return the lid).
	KRoleHcomp
	// KRoleComp is heterogeneous composition (box-fill along a type-line).
	KRoleComp
	// KRoleTranspG is generalized transport carrying a constancy cofibration φ
	// (the CCHM primitive; plain transp is the φ = ⊥ case).
	KRoleTranspG
)

// KanInfo gives the evaluator the Kan-builtin roles of stored hashes (§F
// phase 3c). store.Store implements it. Nil means no Kan builtins.
type KanInfo interface {
	KanRoleOf(Hash) KanRole
	// KanHash is the reverse lookup: the hash of the member with the given role,
	// for the structural Kan rules to recurse (build `transp`/`hcomp` sub-terms).
	KanHash(KanRole) (Hash, bool)
}

// SigmaRole classifies a stored hash's role in the inner Sigma builtin group
// (§F / R-SIGMA / A5): the fibrant dependent-pair former, its intro, and its two
// computing projections. See store/sigma.go.
type SigmaRole byte

const (
	// GRoleNone: not an inner-Sigma builtin.
	GRoleNone SigmaRole = iota
	// GRoleSigma is sigmaF, the fibrant Σ former (El stays neutral).
	GRoleSigma
	// GRolePair is pairF, the canonical pair intro (permanently neutral).
	GRolePair
	// GRoleFst is fstF, the first projection (computes by ι).
	GRoleFst
	// GRoleSnd is sndF, the second projection (computes by ι).
	GRoleSnd
)

// SigmaInfo gives the evaluator the inner-Sigma roles of stored hashes (A5).
// store.Store implements it. Nil means no inner-Sigma builtins.
type SigmaInfo interface {
	SigmaRoleOf(Hash) SigmaRole
	SigmaHash(SigmaRole) (Hash, bool)
}

// GlueRole classifies a stored hash's role in the inner Glue builtin group
// (§F / R-GLUE / A6): the fibrant glue former, its intro, and the computing
// unglue elimination — the engine of computational univalence. See
// store/glue.go and ref_docs/wootz/GLUE-DESIGN.md.
type GlueRole byte

const (
	// URoleNone: not an inner-Glue builtin.
	URoleNone GlueRole = iota
	// URoleGlue is Glue, the fibrant former welding a partial type T onto a base
	// A along a partial equivalence. El (Glue A ⊤ T e) decodes to El (T htop);
	// on a proper/neutral face it stays a new neutral fibrant type.
	URoleGlue
	// URoleGlueIn is glue, the canonical pair intro (permanently neutral, like
	// pairF/pabs).
	URoleGlueIn
	// URoleUnglue is unglue, the elimination (computes by ι): β on a glue intro,
	// and the equivFun boundary when the face is ⊤.
	URoleUnglue
	// URoleUnglueT is unglueT, the T-component projection on φ (dual to unglue):
	// unglueT A φ T e (glue _ _ _ _ t a) h ~> t h. Used by hcomp-over-Glue to
	// extract glued-fibre components; stays neutral on a neutral scrutinee.
	URoleUnglueT
)

// PabsURole classifies a stored hash's role in the type-level path-abstraction
// builtin group (§F / R-UA / A7): pabsU builds a `pathU (A i0) (A i1)` from an
// interval line of fibrant codes `A : I -> UF`. castU along a pabsU-path computes
// as transport along the line — the engine of ua-from-Glue. See store/pathu.go.
type PabsURole byte

const (
	// PURoleNone: not a pabsU builtin.
	PURoleNone PabsURole = iota
	// PURolePabsU is pabsU, the type-level path abstraction (a canonical pathU
	// intro; rigid). castU computes on it via transp.
	PURolePabsU
)

// PabsUInfo gives the evaluator the pabsU role of stored hashes (A7).
// store.Store implements it. Nil means no pabsU builtin.
type PabsUInfo interface {
	PabsURoleOf(Hash) PabsURole
	PabsUHash() (Hash, bool)
}

// PappURole classifies a stored hash's role in the type-level path-application
// group (§F / R-UA / A7): pappU applies a `pathU` at an interval point to read the
// type there — the pathU analogue of papp. See store/pappu.go.
type PappURole byte

const (
	// PPURoleNone: not a pappU builtin.
	PPURoleNone PappURole = iota
	// PPURoleApp is pappU, the type-level path application (computes by β, refl,
	// and endpoint boundary, like papp).
	PPURoleApp
)

// PappUInfo gives the evaluator the pappU role of stored hashes (A7).
// store.Store implements it. Nil means no pappU builtin.
type PappUInfo interface {
	PappURoleOf(Hash) PappURole
	PappUHash() (Hash, bool)
}

// ForallRole classifies a stored hash's role in the ∀-cofibration builtin group
// (§F / R-GLUE / A6): the "φ holds on the whole interval line" face former that
// CCHM's transp-over-Glue reconciliation quantifies over. See store/forall.go.
type ForallRole byte

const (
	// ARoleNone: not a ∀-cofibration builtin.
	ARoleNone ForallRole = iota
	// ARoleForall is forallF : (I -> F) -> F, computing forallF (λ_. ⊤) ~> ⊤.
	ARoleForall
)

// ForallInfo gives the evaluator the ∀-cofibration role of stored hashes (A6).
// store.Store implements it. Nil means no ∀-cofibration builtin.
type ForallInfo interface {
	ForallRoleOf(Hash) ForallRole
	ForallHash() (Hash, bool)
}

// HitRole classifies a stored hash's role in the inner higher-inductive-type kit
// (§F / R-HIT / A9). The ready slice is the CIRCLE: a fibrant former `Circle`, a
// point constructor `base`, a path constructor `loop : pathF Circle base base`,
// and the non-dependent recursor `circElim`. The recursor COMPUTES by ι on the
// generators (core.HitInfo / tryHitIota): `circElim P b l base ~> b`, and along
// the loop `circElim P b l (papp Circle base base loop i) ~> papp P b b l i`. The
// path-ctor boundary (`papp … loop i0 ~> base`) is already given by tryPathIota,
// and constant-line transport (`transp (λ_. Circle) base ~> base`) by regularity —
// so the monomorphic circle lands on existing dispatch. The formal hcomp cell (a
// proper-face `hcomp Circle φ u u0`) is the canonical Kan generator: the recursor
// commutes with it (the hcomp branch of tryHitIota), and for the parameterised HITs
// `transp` commutes with it too (transpHitHcompCell) — Circle's is constant-line so
// regularity already returns it unchanged. See ref_docs/wootz/R-HIT.md, store/hit.go.
type HitRole byte

const (
	// HRoleNone: not a HIT builtin.
	HRoleNone HitRole = iota
	// HRoleCircle is the fibrant former Circle : UF (a rigid code).
	HRoleCircle
	// HRoleBase is the point constructor base : El Circle (rigid).
	HRoleBase
	// HRoleLoop is the path constructor loop : El (pathF Circle base base) (rigid).
	HRoleLoop
	// HRoleElim is the recursor circElim (computes by the HIT ι-rules).
	HRoleElim
)

// HitInfo gives the evaluator the inner-HIT roles of stored hashes (A9).
// store.Store implements it. Nil means no HIT builtins.
type HitInfo interface {
	HitRoleOf(Hash) HitRole
	HitHash(HitRole) (Hash, bool)
}

// CircIndInfo gives the evaluator the circle's DEPENDENT eliminator (§F / R-HIT /
// A9): the induction principle `circInd`, a separate one-member group (so the
// circle group's hashes stay byte-stable) whose ι-rules compute on base and along
// loop into a dependent motive `P : El Circle -> UF`, using pathP for the loop
// method. store.Store implements it. See store/circind.go.
type CircIndInfo interface {
	IsCircInd(Hash) bool
	CircIndHash() (Hash, bool)
}

// SuspIndInfo gives the evaluator the suspension's DEPENDENT eliminator (§F /
// R-HIT / A9): the induction principle `suspInd`, computing on north/south and
// along merid into a dependent motive. store.Store implements it. See
// store/suspind.go.
type SuspIndInfo interface {
	IsSuspInd(Hash) bool
	SuspIndHash() (Hash, bool)
}

// QuotIndInfo gives the evaluator the fibrant quotient's DEPENDENT eliminator
// (§F / R-HIT / A9): the induction principle `quotInd`, computing on qinc and
// along qrel into a dependent motive. store.Store implements it. See
// store/quotind.go.
type QuotIndInfo interface {
	IsQuotInd(Hash) bool
	QuotIndHash() (Hash, bool)
}

// SuspRole classifies a stored hash's role in the suspension HIT (§F / R-HIT /
// A9), the second inner higher inductive type and the S^n building block (S² =
// Susp S¹). A parameterised HIT: former `Susp : UF -> UF`, two point ctors
// `north`/`south`, an argument-carrying path ctor `merid : (a:El A) -> pathF
// (Susp A) north south`, and the recursor `suspElim`. The recursor COMPUTES by ι
// (core.SuspInfo / trySuspIota): `suspElim … north ~> n`, `… south ~> s`, along
// a meridian `… (papp … (merid A a) i) ~> papp P n s (m a) i`, and it commutes
// with hcomp. Constant-parameter transport is free (regularity); varying-A
// transport is the R-FILL remainder. See store/susp.go.
type SuspRole byte

const (
	// SuRoleNone: not a suspension builtin.
	SuRoleNone SuspRole = iota
	// SuRoleSusp is the fibrant former Susp : UF -> UF (a rigid code).
	SuRoleSusp
	// SuRoleNorth is the north-pole point constructor (rigid).
	SuRoleNorth
	// SuRoleSouth is the south-pole point constructor (rigid).
	SuRoleSouth
	// SuRoleMerid is the meridian path constructor merid (rigid).
	SuRoleMerid
	// SuRoleElim is the recursor suspElim (computes by the HIT ι-rules).
	SuRoleElim
)

// TruncRole classifies a stored hash's role in the set-truncation HIT (§F /
// R-HIT / A9, the dim-2 slice): the former Trunc0, the point ctor inc, the dim-2
// path ctor squash, and the recursor trunc0Rec. See store/trunc.go.
type TruncRole byte

const (
	// TRoleNone: not a set-truncation builtin.
	TRoleNone TruncRole = iota
	// TRoleTrunc is the former Trunc0 (a rigid code).
	TRoleTrunc
	// TRoleInc is the point constructor inc (rigid).
	TRoleInc
	// TRoleSquash is the dim-2 path constructor squash (rigid).
	TRoleSquash
	// TRoleElim is the recursor trunc0Rec (computes on inc by ι).
	TRoleElim
)

// TruncInfo gives the evaluator the set-truncation roles of stored hashes (A9).
// store.Store implements it. Nil means no set-truncation builtins.
type TruncInfo interface {
	TruncRoleOf(Hash) TruncRole
	TruncHash(TruncRole) (Hash, bool)
}

// SuspInfo gives the evaluator the suspension-HIT roles of stored hashes (A9).
// store.Store implements it. Nil means no suspension builtins.
type SuspInfo interface {
	SuspRoleOf(Hash) SuspRole
	SuspHash(SuspRole) (Hash, bool)
}

// QuotHitRole classifies a stored hash's role in the FIBRANT quotient HIT
// (§F / R-HIT / A9): the path-respecting, effective quotient the strict `Quot`
// (v2, store/quot.go) cannot give. A parameterised HIT: former `Quotient : (A:UF)
// -> (El A -> El A -> UF) -> UF`, point ctor `qinc`, an evidence-carrying path
// ctor `qrel : (a b)(r:El (R a b)) -> pathF (Quotient A R) (qinc a) (qinc b)`,
// and the recursor `quotElim`. The recursor COMPUTES by ι (core.QuotHitInfo /
// tryQuotHitIota): `quotElim … (qinc a) ~> f a`, along a relation path `… (papp …
// (qrel a b r) i) ~> papp P (f a) (f b) (rel a b r) i`, and commutes with hcomp.
// See store/quotient.go.
type QuotHitRole byte

const (
	// QHRoleNone: not a fibrant-quotient builtin.
	QHRoleNone QuotHitRole = iota
	// QHRoleQuot is the former Quotient (a rigid code).
	QHRoleQuot
	// QHRoleInc is the point constructor qinc (rigid).
	QHRoleInc
	// QHRoleRel is the relation path constructor qrel (rigid).
	QHRoleRel
	// QHRoleElim is the recursor quotElim (computes by the HIT ι-rules).
	QHRoleElim
)

// QuotHitInfo gives the evaluator the fibrant-quotient-HIT roles of stored
// hashes (A9). store.Store implements it. Nil means no fibrant-quotient builtins.
type QuotHitInfo interface {
	QuotHitRoleOf(Hash) QuotHitRole
	QuotHitHash(QuotHitRole) (Hash, bool)
}

// FsplitRole classifies a stored hash's role in the face-split builtin group
// (§F / R-BOX / A3): the eliminator of `holds (for φ ψ)`, which dispatches a
// system by which disjunct holds. See store/fsplit.go.
type FsplitRole byte

const (
	// SRoleNoneF: not a face-split builtin.
	SRoleNoneF FsplitRole = iota
	// SRoleSplit is fsplit, the face-disjunction eliminator (computes by face-ι).
	SRoleSplit
)

// FsplitInfo gives the evaluator the face-split role of stored hashes (A3).
// store.Store implements it. Nil means no face-split builtin.
type FsplitInfo interface {
	FsplitRoleOf(Hash) FsplitRole
	FsplitHash() (Hash, bool) // the fsplit member hash, for Kan rules to build systems
}

// SysURole classifies a stored hash's role in the UF-valued face-split group
// (§F / R-BOX / A8): the type-level counterpart of fsplit, dispatching a partial
// TYPE by which disjunct of `holds (for φ ψ)` holds. See store/sysu.go.
type SysURole byte

const (
	// SURoleNone: not a UF-valued face-split builtin.
	SURoleNone SysURole = iota
	// SURoleSysU is sysU, the type-level face-disjunction eliminator (face-ι).
	SURoleSysU
)

// SysUInfo gives the evaluator the UF-valued face-split role of stored hashes
// (A8). store.Store implements it. Nil means no sysU builtin.
type SysUInfo interface {
	SysURoleOf(Hash) SysURole
	SysUHash() (Hash, bool) // the sysU member hash, for callers to build type systems
}

// SplitDRole classifies a stored hash's role in the dependent face-split group
// (§F / R-BOX / A8): the motive-carrying counterpart of fsplit, dispatching a
// partial ELEMENT whose type depends on the face. See store/fsplitd.go.
type SplitDRole byte

const (
	// SDRoleNone: not a dependent face-split builtin.
	SDRoleNone SplitDRole = iota
	// SDRoleSplitD is fsplitD, the dependent face-disjunction eliminator (face-ι).
	SDRoleSplitD
)

// SplitDInfo gives the evaluator the dependent face-split role of stored hashes
// (A8). store.Store implements it. Nil means no fsplitD builtin.
type SplitDInfo interface {
	SplitDRoleOf(Hash) SplitDRole
	SplitDHash() (Hash, bool) // the fsplitD member hash, for callers to build systems
}

// GlueInfo gives the evaluator the inner-Glue roles of stored hashes (A6).
// store.Store implements it. Nil means no inner-Glue builtins. EquivFunHash
// exposes the equivalence group's forward-map accessor (registered before the
// Glue group), which the unglue ⊤ boundary rule needs to build its reduct;
// EquivProofHash exposes its contractible-fibre accessor (equivProof), which the
// general transp-over-Glue arm (G1) consumes to recover a T-component at a face.
type GlueInfo interface {
	GlueRoleOf(Hash) GlueRole
	GlueHash(GlueRole) (Hash, bool)
	EquivFunHash() (Hash, bool)
	EquivProofHash() (Hash, bool)
}

// CoindRole classifies a stored hash's role in the coinductive builtin group
// (R-COIND / C5a): the final-coalgebra former, its observation, and the
// corecursor. See store/coind.go.
type CoindRole byte

const (
	// NRoleNone: not a coinductive builtin.
	NRoleNone CoindRole = iota
	// NRoleNu is the final-coalgebra former Nu (a code).
	NRoleNu
	// NRoleOut is the single observation (computes by ι on unfold).
	NRoleOut
	// NRoleUnfold is the anamorphism / corecursor (permanently neutral).
	NRoleUnfold
	// NRoleNuCons is the ONE-LEVEL coinductive constructor (the dual of a
	// datatype's constructor): nuCons F : El (F (Nu F)) -> El (Nu F). It computes
	// by β `out F (nuCons F x) ~> x` and by the one-level η `nuCons F (out F s) ~> s`
	// (both single-step, confluent). Unlike `unfold` (a DEEP anamorphism whose
	// observations are rebuilt corecursively), `nuCons` reconstructs a coinductive
	// value from ONE layer of observation, so the coinductive η holds definitionally.
	NRoleNuCons
)

// CoindInfo gives the evaluator the coinductive roles of stored hashes (C5a).
// store.Store implements it. Nil means no coinductive builtins.
type CoindInfo interface {
	CoindRoleOf(Hash) CoindRole
	CoindHash(CoindRole) (Hash, bool)
}

// GuardRole classifies a stored hash's role in the guarded-recursion modality
// group (R-COIND / C5b): the ▹ "later" modality, its `next` intro, and the guarded
// fixpoint `dfix`. All heads are permanently neutral; dfix's unfolding is a bounded
// conversion rule (convDfix), not an eval ι. See store/guard.go.
type GuardRole byte

const (
	// LRoleNone: not a guarded-recursion builtin.
	LRoleNone GuardRole = iota
	// LRoleClock is the clock pretype former Clock (a neutral type, no eliminator).
	LRoleClock
	// LRoleK0 is the clock constant k0 (a neutral; ∀κ is inhabited).
	LRoleK0
	// LRoleLater is the ▹ ("later") modality former Later : Clock -> UF -> UF (a code).
	LRoleLater
	// LRoleNext is the `next` intro (a value now is available later; neutral).
	LRoleNext
	// LRoleDfix is the guarded fixpoint dfix (neutral in eval; unfolds once in conv).
	LRoleDfix
	// LRoleLmap is the ▹ functor map lmap (computes by ι on a `next` intro).
	LRoleLmap
	// LRoleLap is the ▹ applicative ⊛ lap (computes by ι on two `next` intros).
	LRoleLap
	// LRoleForce is the clock-quantified guard elimination force (computes by ι on a
	// clock-abstracted `next` intro: force A (λκ. next κ A x) ~> λκ. x).
	LRoleForce
	// LRoleGfix is the guarded-recursive TYPE fixpoint gfix (neutral in eval; the
	// equation gfix k F ≡ F (gfix k F) unfolds once in conv, convGfix, with a progress
	// guard so it terminates).
	LRoleGfix
	// LRoleForceD is the DEPENDENT clock-quantified guard elimination forceD (computes by
	// ι on a clock-abstracted `next` whose payload TYPE varies with the clock:
	// forceD A (λκ. next κ (A κ) x) ~> λκ. x, the payload x MAY mention κ).
	LRoleForceD
	// LRoleGfixF is the INDEXED guarded-recursive TYPE fixpoint gfixF (the `Bisim`
	// former; neutral in eval, the equation gfixF k D Φ d ≡ Φ (gfixF k D Φ) d unfolds
	// once in conv, convGfixF, with a progress guard so it terminates).
	LRoleGfixF
	// LRoleLaterApp is the universe-level ▹κ application laterApp (computes by ι on a
	// `next` intro: laterApp k A f (next k A x) ~> Later k (f x)) — the companion to lmap
	// that lifts a UF-valued family through a delayed value.
	LRoleLaterApp
	// LRoleLapD is the DEPENDENT guarded application lapD (computes by ι on two `next`
	// intros: lapD k A B (next k _ f) (next k A x) ~> next k (B x) (f x)) — its result type
	// is given by laterApp; the converse's path-assembly applies the delayed path-builder.
	LRoleLapD
)

// GuardInfo gives the conversion checker the guarded-recursion roles of stored
// hashes (C5b). store.Store implements it. Nil means no guarded builtins.
type GuardInfo interface {
	GuardRoleOf(Hash) GuardRole
	GuardHash(GuardRole) (Hash, bool)
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
	case Sig:
		cod := tm.Cod
		return VSig{Name: cod.Name, Qty: tm.Qty, Dom: m.Eval(env, tm.Dom),
			Cod: func(v Val) Val { return m.Eval(env.Extend(v), cod.Body) }}
	case Pair:
		cod := tm.Cod
		return VPair{Name: cod.Name, Dom: m.Eval(env, tm.Dom),
			Cod: func(v Val) Val { return m.Eval(env.Extend(v), cod.Body) },
			A:   m.Eval(env, tm.A), B: m.Eval(env, tm.B)}
	case Fst:
		return m.proj1(m.Eval(env, tm.P))
	case Snd:
		return m.proj2(m.Eval(env, tm.P))
	case NatLit:
		return m.natLitVal(tm.N, tm.Zero, tm.Succ)
	default:
		panic("core.Eval: unknown Tm constructor")
	}
}

// proj1 takes the first projection. A pair β-reduces to its first component; a
// neutral grows an NFst head, threading the glued unfolding so forcing the
// projection forces the head and logs the dependency (the same seam as apply).
func (m *Machine) proj1(v Val) Val {
	switch x := v.(type) {
	case VPair:
		return x.A
	case VNeu:
		out := VNeu{Spine: NFst{P: x.Spine}}
		if x.Unfold != nil {
			u := x.Unfold
			var memo Val
			out.Unfold = func() Val {
				if memo == nil {
					memo = m.proj1(u())
				}
				return memo
			}
		}
		return out
	default:
		panic("core.proj1: first projection of a non-pair")
	}
}

// Proj1/Proj2 are the exported projection entry points (for the elaborator's
// unifier, which compares Σ values componentwise for η).
func (m *Machine) Proj1(v Val) Val { return m.proj1(v) }
func (m *Machine) Proj2(v Val) Val { return m.proj2(v) }

// proj2 takes the second projection, symmetric to proj1.
func (m *Machine) proj2(v Val) Val {
	switch x := v.(type) {
	case VPair:
		return x.B
	case VNeu:
		out := VNeu{Spine: NSnd{P: x.Spine}}
		if x.Unfold != nil {
			u := x.Unfold
			var memo Val
			out.Unfold = func() Val {
				if memo == nil {
					memo = m.proj2(u())
				}
				return memo
			}
		}
		return out
	default:
		panic("core.proj2: second projection of a non-pair")
	}
}

// natLitVal builds the glued value of a compressed numeral (C7 / R-NUM). The
// spine is an NNatLit (the fast-path representative: two literals compare by
// big.Int.Cmp in convSpine without forcing). The Unfold thunk PEELS one succ
// layer: NatLit 0 unfolds to `zero`, NatLit (k>0) to `succ (NatLit (k-1))`,
// built from the session's nat constructor refs carried on the literal. This is
// what makes a literal definitionally the unary succ-chain — forcing it (in
// conversion against a hand-written chain, or in an eliminator's scrutinee
// Force) recovers the canonical constructor form, ONE layer at a time, so the
// chain is never materialised and the normalizer terminates (each peel strictly
// decreases N toward 0). The thunk memoizes within this Machine's run.
func (m *Machine) natLitVal(n *big.Int, zero, succ Hash) Val {
	var memo Val
	return VNeu{Spine: NNatLit{N: n, Zero: zero, Succ: succ}, Unfold: func() Val {
		if memo == nil {
			if n.Sign() == 0 {
				memo = m.refVal(zero)
			} else {
				pred := new(big.Int).Sub(n, big.NewInt(1))
				if m.rigidHead(succ) {
					// CANONICAL binding: succ is a rigid data constructor with no
					// body to unfold. Build `succ (NatLit (n-1))` DIRECTLY as the
					// application neutral, bypassing apply/tryRules — the Decision-0
					// folding ι (which fires only on a rigid succ) would otherwise
					// fold it straight back to NatLit n, a no-progress peel loop.
					// The peel is the literal's DEFINING unfolding (it IS succ^n
					// zero one layer at a time); folding is for fresh `succ (lit)`
					// redexes built elsewhere, not for the peel itself.
					memo = VNeu{Spine: NApp{Fn: NRef{Hash: succ},
						Arg: m.natLitVal(pred, zero, succ), Icit: Expl}}
				} else {
					// GENERALIZED binding (`builtin nat Nat one double`): succ/zero
					// are remapped to defs WITH bodies. Go through the normal apply
					// path so forcing the peel UNFOLDS those bodies (the established
					// generalized-numeral behavior). The folding ι declines here
					// (non-rigid head), so there is no peel loop.
					memo = m.apply(m.refVal(succ), m.natLitVal(pred, zero, succ), Expl)
				}
			}
		}
		return memo
	}}
}

// NatLitVal is the exported constructor for a compressed-numeral value, for tests
// and callers that build literals directly in the value domain.
func (m *Machine) NatLitVal(n *big.Int, zero, succ Hash) Val { return m.natLitVal(n, zero, succ) }

// asNatLit reports whether v is (or unfolds to) a compressed numeral, returning
// its payload. It does NOT use Force — Force would peel the literal's own Unfold
// into the succ-chain. Instead it unfolds glued layers ONE at a time and stops
// the instant the spine is an NNatLit (the literal itself), so a Ref bound to a
// literal is still recognised while the literal is never materialised. O(1) on a
// bare literal.
func (m *Machine) asNatLit(v Val) (*big.Int, bool) {
	for {
		n, ok := v.(VNeu)
		if !ok {
			return nil, false
		}
		if lit, ok := n.Spine.(NNatLit); ok {
			return lit.N, true
		}
		if n.Unfold == nil {
			return nil, false
		}
		next, progressed := m.force1(v)
		if !progressed {
			return nil, false
		}
		// Guard against a no-progress meta self-force.
		if nn, ok := next.(VNeu); ok && nn.Unfold == nil && sameSpine(nn.Spine, n.Spine) {
			return nil, false
		}
		v = next
	}
}

// asNatLitSpine is asNatLit returning the FULL spine head (N plus the nat-binding
// hashes), for callers that must rebuild a literal of the same binding (the
// Decision-0 folding ι needs Zero/Succ to construct NatLit (n+1)). Like asNatLit
// it unfolds glued layers one at a time and never materialises the chain.
func (m *Machine) asNatLitSpine(v Val) (NNatLit, bool) {
	for {
		n, ok := v.(VNeu)
		if !ok {
			return NNatLit{}, false
		}
		if lit, ok := n.Spine.(NNatLit); ok {
			return lit, true
		}
		if n.Unfold == nil {
			return NNatLit{}, false
		}
		next, progressed := m.force1(v)
		if !progressed {
			return NNatLit{}, false
		}
		if nn, ok := next.(VNeu); ok && nn.Unfold == nil && sameSpine(nn.Spine, n.Spine) {
			return NNatLit{}, false
		}
		v = next
	}
}

// forceToNatLit forces glued layers of v and reports the CANONICAL compressed
// numeral it reduces to, if any — a literal whose nat constructors are RIGID data
// constructors (not a generalized `builtin nat` remapping). It stops at the
// NNatLit spine WITHOUT peeling the literal's own one-layer Unfold, so the result
// stays compact (the QuoteUnfold short-circuit consumer must not re-materialise
// the succ-chain). A generalized binding (Succ non-rigid) returns ok=false so the
// numeral is peeled through the generalized chain as before.
func (m *Machine) forceToNatLit(v Val) (NNatLit, bool) {
	lit, ok := m.asNatLitSpine(v)
	if !ok {
		return NNatLit{}, false
	}
	if !m.rigidHead(lit.Succ) || !m.rigidHead(lit.Zero) {
		return NNatLit{}, false
	}
	return lit, true
}

// tryNatAccel fires the C7 accel ι-rule (Decision 1) on a saturated 2-argument
// arithmetic spine. It fires ONLY when BOTH arguments force to a VNatLit; the
// result is a single fresh VNatLit computed in one bigint step. On any neutral
// argument it returns (_, false), so the op falls back to its ordinary recursive
// body and open-term reasoning is unchanged. Monus is truncated at zero (Nat has
// no negatives). The agreement of each rule with the def's unfolded peeling is
// the differential-tested soundness gate.
func (m *Machine) tryNatAccel(op NatOp, spine Neutral) (Val, bool) {
	_, args := spineParts(spine)
	if len(args) != 2 {
		return nil, false
	}
	a, ok := m.asNatLit(args[0])
	if !ok {
		return nil, false
	}
	b, ok := m.asNatLit(args[1])
	if !ok {
		return nil, false
	}
	zero, succ, ok := m.Na.NatCtors()
	if !ok {
		return nil, false
	}
	r := new(big.Int)
	switch op {
	case NatOpAdd:
		r.Add(a, b)
	case NatOpMul:
		r.Mul(a, b)
	case NatOpMonus:
		r.Sub(a, b)
		if r.Sign() < 0 {
			r.SetInt64(0)
		}
	case NatOpDiv:
		if b.Sign() == 0 {
			r.SetInt64(0) // a // 0 = 0 (matches divF)
		} else {
			r.Quo(a, b) // non-negative operands: truncated == flooring
		}
	case NatOpMod:
		if b.Sign() == 0 {
			r.Set(a) // a % 0 = a (matches modF)
		} else {
			r.Rem(a, b) // non-negative operands: == Euclidean mod
		}
	default:
		return nil, false
	}
	return m.natLitVal(r, zero, succ), true
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
	if m.Iv != nil && m.Iv.IntervalRoleOf(h) != IRoleNone {
		return true
	}
	if m.Pa != nil && m.Pa.PathRoleOf(h) != PRoleNone {
		return true
	}
	if m.Fc != nil && m.Fc.FaceRoleOf(h) != CRoleNone {
		return true
	}
	if m.Sy != nil && m.Sy.SysRoleOf(h) != SRoleNone {
		return true
	}
	if m.Kn != nil && m.Kn.KanRoleOf(h) != KRoleNone {
		return true
	}
	if m.Si != nil && m.Si.SigmaRoleOf(h) != GRoleNone {
		return true
	}
	if m.Cn != nil && m.Cn.CoindRoleOf(h) != NRoleNone {
		return true
	}
	if m.Gl != nil && m.Gl.GlueRoleOf(h) != URoleNone {
		return true
	}
	if m.Fs != nil && m.Fs.FsplitRoleOf(h) != SRoleNoneF {
		return true
	}
	if m.SyU != nil && m.SyU.SysURoleOf(h) != SURoleNone {
		return true
	}
	if m.FsD != nil && m.FsD.SplitDRoleOf(h) != SDRoleNone {
		return true
	}
	if m.Fa != nil && m.Fa.ForallRoleOf(h) != ARoleNone {
		return true
	}
	if m.Pu != nil && m.Pu.PabsURoleOf(h) != PURoleNone {
		return true
	}
	if m.PpU != nil && m.PpU.PappURoleOf(h) != PPURoleNone {
		return true
	}
	if m.Hi != nil && m.Hi.HitRoleOf(h) != HRoleNone {
		return true
	}
	if m.Su != nil && m.Su.SuspRoleOf(h) != SuRoleNone {
		return true
	}
	if m.Qh != nil && m.Qh.QuotHitRoleOf(h) != QHRoleNone {
		return true
	}
	if m.Pp != nil && m.Pp.PathPRoleOf(h) != PPRoleNone {
		return true
	}
	if m.Ci != nil && m.Ci.IsCircInd(h) {
		return true
	}
	if m.Tr != nil && m.Tr.TruncRoleOf(h) != TRoleNone {
		return true
	}
	if m.SuI != nil && m.SuI.IsSuspInd(h) {
		return true
	}
	if m.QuI != nil && m.QuI.IsQuotInd(h) {
		return true
	}
	// A `partial` definition is a permanently-neutral head in eval/conversion
	// (the C4 firewall): its general-recursive body never unfolds here, so the
	// normalizer cannot diverge. Codegen reaches the body through its own direct
	// Unfold; type-checking treats the head as opaque.
	if m.Pt != nil && m.Pt.IsPartial(h) {
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
	case Ref, Univ, Prop, Meta, NatLit:
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
	case Sig:
		return usesVar(tm.Dom, idx) || usesVar(tm.Cod.Body, idx+1)
	case Pair:
		return usesVar(tm.Dom, idx) || usesVar(tm.Cod.Body, idx+1) ||
			usesVar(tm.A, idx) || usesVar(tm.B, idx)
	case Fst:
		return usesVar(tm.P, idx)
	case Snd:
		return usesVar(tm.P, idx)
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
	if m.Data == nil && m.Quot == nil && m.Fib == nil && m.Iv == nil && m.Pa == nil && m.Fc == nil && m.Kn == nil && m.Na == nil {
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
	// Decision-0 folding ι (C7 / R-NUM): `succ (NatLit n) ~> NatLit (n+1)`. When
	// the nat `succ` constructor is applied to a value that is (or unfolds to) a
	// compressed numeral of the SAME nat binding, fold it back into a single
	// canonical literal rather than leaving a `succ` wrapper around it. This keeps
	// closed nat normal forms CANONICAL — one NatLit, never a mixed
	// `succ (NatLit k)` chain — so quote/normalize of a closed result stays
	// compact. It is SOUND because `succ (NatLit n)` and `NatLit (n+1)` are the
	// same value (both succ^(n+1) zero — the literal's own peel unfolds to exactly
	// `succ (NatLit n)`), and BOUNDED because it removes one `succ` and the literal
	// strictly grows toward a fixed point with no `succ` to fold. The `succ` head
	// is recognised by the literal's self-carried Succ hash, so it fires whether or
	// not the accel table (m.Na) is configured.
	//
	// The fold fires ONLY when the head is a RIGID `succ` (a genuine data
	// constructor): a GENERALIZED `builtin nat` binding remaps succ to a def WITH
	// a body (e.g. `builtin nat Nat one double`), which must run that body, not
	// fold — so a non-rigid head declines here and falls through to its ordinary
	// reduction. (rigidHead also guarantees this fold cannot fight the peel, whose
	// own `succ (NatLit (n-1))` is built directly, bypassing tryRules.)
	if app, ok := spine.(NApp); ok {
		if _, isApp := app.Fn.(NRef); isApp && m.rigidHead(ref.Hash) {
			if lit, ok := m.asNatLitSpine(app.Arg); ok && lit.Succ == ref.Hash {
				r := new(big.Int).Add(lit.N, big.NewInt(1))
				return m.natLitVal(r, lit.Zero, lit.Succ), true
			}
		}
	}
	if m.Na != nil {
		if op := m.Na.NatOpOf(ref.Hash); op != NatOpNone {
			if red, ok := m.tryNatAccel(op, spine); ok {
				return red, true
			}
			// else fall through: an argument is not a literal, so the op reduces
			// by its ordinary body (it is not in any other ι family).
		}
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
	if m.Iv != nil {
		if role := m.Iv.IntervalRoleOf(ref.Hash); role == IRoleNeg || role == IRoleMin || role == IRoleMax {
			_, args := spineParts(spine)
			return m.tryIntervalIota(role, args)
		}
	}
	if m.Pa != nil {
		if m.Pa.PathRoleOf(ref.Hash) == PRoleApp {
			_, args := spineParts(spine)
			return m.tryPathIota(args)
		}
	}
	if m.Fc != nil {
		if role := m.Fc.FaceRoleOf(ref.Hash); role == CRoleEq0 || role == CRoleEq1 || role == CRoleAnd || role == CRoleOr {
			_, args := spineParts(spine)
			return m.tryFaceIota(role, args)
		}
	}
	if m.Kn != nil {
		switch m.Kn.KanRoleOf(ref.Hash) {
		case KRoleTransp:
			_, args := spineParts(spine)
			return m.tryTransp(args)
		case KRoleHcomp:
			_, args := spineParts(spine)
			return m.tryHcomp(args)
		case KRoleComp:
			_, args := spineParts(spine)
			return m.tryComp(args)
		case KRoleTranspG:
			_, args := spineParts(spine)
			if len(args) != 3 {
				return nil, false
			}
			return m.transpG(args[0], args[1], args[2])
		}
	}
	if m.Si != nil {
		if role := m.Si.SigmaRoleOf(ref.Hash); role == GRoleFst || role == GRoleSnd {
			_, args := spineParts(spine)
			return m.trySigmaIota(role, args)
		}
	}
	if m.Cn != nil && m.Cn.CoindRoleOf(ref.Hash) == NRoleOut {
		_, args := spineParts(spine)
		return m.tryCoindIota(args)
	}
	if m.Cn != nil && m.Cn.CoindRoleOf(ref.Hash) == NRoleUnfold {
		_, args := spineParts(spine)
		return m.tryUnfoldEta(args)
	}
	if m.Cn != nil && m.Cn.CoindRoleOf(ref.Hash) == NRoleNuCons {
		_, args := spineParts(spine)
		return m.tryNuConsEta(args)
	}
	if m.Gd != nil && m.Gd.GuardRoleOf(ref.Hash) == LRoleLmap {
		_, args := spineParts(spine)
		return m.tryGuardIota(args)
	}
	if m.Gd != nil && m.Gd.GuardRoleOf(ref.Hash) == LRoleLap {
		_, args := spineParts(spine)
		return m.tryGuardLapIota(args)
	}
	if m.Gd != nil && m.Gd.GuardRoleOf(ref.Hash) == LRoleForce {
		_, args := spineParts(spine)
		return m.tryForceIota(args)
	}
	if m.Gd != nil && m.Gd.GuardRoleOf(ref.Hash) == LRoleForceD {
		_, args := spineParts(spine)
		return m.tryForceDIota(args)
	}
	if m.Gd != nil && m.Gd.GuardRoleOf(ref.Hash) == LRoleLaterApp {
		_, args := spineParts(spine)
		return m.tryLaterAppIota(args)
	}
	if m.Gd != nil && m.Gd.GuardRoleOf(ref.Hash) == LRoleLapD {
		_, args := spineParts(spine)
		return m.tryLapDIota(args)
	}
	if m.Gl != nil && m.Gl.GlueRoleOf(ref.Hash) == URoleUnglue {
		_, args := spineParts(spine)
		return m.tryGlueIota(args)
	}
	if m.Gl != nil && m.Gl.GlueRoleOf(ref.Hash) == URoleUnglueT {
		_, args := spineParts(spine)
		return m.tryGlueTIota(args)
	}
	if m.Fs != nil && m.Fs.FsplitRoleOf(ref.Hash) == SRoleSplit {
		_, args := spineParts(spine)
		return m.tryFsplitIota(args)
	}
	if m.SyU != nil && m.SyU.SysURoleOf(ref.Hash) == SURoleSysU {
		_, args := spineParts(spine)
		return m.trySysUIota(args)
	}
	if m.FsD != nil && m.FsD.SplitDRoleOf(ref.Hash) == SDRoleSplitD {
		_, args := spineParts(spine)
		return m.trySplitDIota(args)
	}
	if m.Fa != nil && m.Fa.ForallRoleOf(ref.Hash) == ARoleForall {
		_, args := spineParts(spine)
		return m.tryForallIota(args)
	}
	if m.PpU != nil && m.PpU.PappURoleOf(ref.Hash) == PPURoleApp {
		_, args := spineParts(spine)
		return m.tryPappUIota(args)
	}
	if m.Hi != nil && m.Hi.HitRoleOf(ref.Hash) == HRoleElim {
		_, args := spineParts(spine)
		return m.tryHitIota(args)
	}
	if m.Su != nil && m.Su.SuspRoleOf(ref.Hash) == SuRoleElim {
		_, args := spineParts(spine)
		return m.trySuspIota(args)
	}
	if m.Qh != nil && m.Qh.QuotHitRoleOf(ref.Hash) == QHRoleElim {
		_, args := spineParts(spine)
		return m.tryQuotHitIota(args)
	}
	if m.Pp != nil && m.Pp.PathPRoleOf(ref.Hash) == PPRoleApp {
		_, args := spineParts(spine)
		return m.tryPathPIota(args)
	}
	if m.Ci != nil && m.Ci.IsCircInd(ref.Hash) {
		_, args := spineParts(spine)
		return m.tryCircIndIota(args)
	}
	if m.Tr != nil && m.Tr.TruncRoleOf(ref.Hash) == TRoleElim {
		_, args := spineParts(spine)
		return m.tryTruncIota(args)
	}
	if m.SuI != nil && m.SuI.IsSuspInd(ref.Hash) {
		_, args := spineParts(spine)
		return m.trySuspIndIota(args)
	}
	if m.QuI != nil && m.QuI.IsQuotInd(ref.Hash) {
		_, args := spineParts(spine)
		return m.tryQuotIndIota(args)
	}
	return nil, false
}

// tryHitIota fires the inner-HIT recursor rules (§F / R-HIT / A9) on a saturated
// `circElim P b l s` spine (four arguments, the scrutinee s at index 3):
//
//	circElim P b l base                              ~>  b
//	circElim P b l (papp Circle base base loop i)    ~>  papp P b b l i
//
// The first is the point branch (identical in shape to a `data` eliminator firing
// on a saturated constructor). The second runs the recursor along the loop: the
// scrutinee is a loop-point `papp Circle base base loop i`, and the reduct is the
// loop method `l` re-applied at the SAME interval coordinate `i`. This is
// boundary-coherent on the nose: at i0/i1, tryPathIota gives `papp … loop i0 ~>
// base` and `papp P b b l i0 ~> b`, so both rules agree with the point branch.
// On any other scrutinee (a neutral, or a formal hcomp cell — the A9 tail) the
// recursor stays stuck. Constant-line transport over Circle needs no rule here:
// the regularity rule already returns the argument for a constant former line.
func (m *Machine) tryHitIota(args []Val) (Val, bool) {
	if len(args) != 4 || m.Hi == nil {
		return nil, false
	}
	s := m.Force(args[3])
	sn, ok := s.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(sn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	// Point branch: circElim P b l base ~> b.
	if m.Hi.HitRoleOf(ref.Hash) == HRoleBase && len(sargs) == 0 {
		return args[1], true
	}
	// Loop branch: the scrutinee is papp Circle base base loop i. papp is a
	// five-argument spine [A, x, y, p, i]; fire when its path argument p (index 3)
	// is the loop constructor.
	if m.Pa != nil && m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		pn, ok := m.Force(sargs[3]).(VNeu)
		if ok {
			ph, pa := spineParts(pn.Spine)
			if pref, ok := ph.(NRef); ok && len(pa) == 0 &&
				m.Hi.HitRoleOf(pref.Hash) == HRoleLoop {
				pappH, ok := m.Pa.PathHash(PRoleApp)
				if ok {
					P, b, l, i := args[0], args[1], args[2], sargs[4]
					// papp P b b l i
					r := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(pappH), P), b), b), l), i)
					return r, true
				}
			}
		}
	}
	// hcomp branch (CHM hcomp-as-constructor — the circle is a genuine Kan HIT):
	// a proper-face `hcomp Circle φ u u0` is itself a canonical generator (the
	// formal cell, a stuck Kan spine), and the recursor COMMUTES with it,
	// transporting the composition into the motive type P:
	//   circElim P b l (hcomp Circle φ u u0)
	//     ~> hcomp P φ (λi h. circElim P b l (u i h)) (circElim P b l u0)
	// This is the one rule that makes a HIT a HIT: instead of a structural fill
	// under a former, the eliminator pushes the motive's composition over the
	// recursively-eliminated box. Closed `El Circle` terms thus reach a generator
	// (base, loop-point, or this hcomp cell), and the recursor consumes all three.
	if m.Kn != nil && m.Kn.KanRoleOf(ref.Hash) == KRoleHcomp && len(sargs) == 4 {
		// The scrutinee's type is El Circle, so its head former is Circle; guard
		// defensively before commuting.
		if cf, ok := m.Force(sargs[0]).(VNeu); ok {
			ch, ca := spineParts(cf.Spine)
			if cref, ok := ch.(NRef); ok && len(ca) == 0 &&
				m.Hi.HitRoleOf(cref.Hash) == HRoleCircle {
				if hcompH, ok := m.Kn.KanHash(KRoleHcomp); ok {
					if elimH, ok := m.Hi.HitHash(HRoleElim); ok {
						P, b, l := args[0], args[1], args[2]
						phi, u, u0 := sargs[1], sargs[2], sargs[3]
						ce := func(x Val) Val {
							return m.Apply(m.Apply(m.Apply(m.Apply(
								m.refVal(elimH), P), b), l), x)
						}
						innerU := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
							return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
								return ce(m.Apply(m.Apply(u, iv), h)) // circElim … (u i h)
							}}
						}}
						r := m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(hcompH), P), phi), innerU), ce(u0))
						return r, true
					}
				}
			}
		}
	}
	return nil, false
}

// tryTruncIota fires the set-truncation recursor (§F / R-HIT / A9 dim-2) on a
// saturated `trunc0Rec A B setB f x` spine (five arguments, scrutinee x at 4):
//
//	trunc0Rec A B setB f (inc A a)  ~>  f a                                (point)
//	trunc0Rec A B setB f (papp (Trunc0 A) x y
//	                        (papp (pathF (Trunc0 A) x y) p q
//	                              (squash A x y p q) i) j)
//	  ~>  papp B (rec x) (rec y)
//	            (papp (pathF B (rec x) (rec y)) (ap_rec p) (ap_rec q)
//	                  (setB (rec x) (rec y) (ap_rec p) (ap_rec q)) i) j    (dim-2)
//
// where rec z := trunc0Rec A B setB f z and ap_rec p := the recursor over the
// path p, pabs B (λk. rec (papp (Trunc0 A) x y p k)) : pathF B (rec x)(rec y).
//
// The POINT branch mirrors a data eliminator. The DIM-2 INTERIOR runs the
// recursor along the `squash` SQUARE: the scrutinee is a squash 2-cell observed
// at interval coordinates i (outer, between paths) and j (inner, along the path),
// and the reduct is the corresponding square in B built from the set-witness
// setB, observed at the SAME coordinates. This is the dim-2 analog of the dim-1
// loop rule (circElim … (papp … loop i) ~> papp P b b l i) — one dimension up.
//
// Boundary coherence (load-bearing, definitional on the nose):
//   - j=i0: inner papp ~> rec x; the scrutinee at j=i0 is x, and rec x matches.
//   - j=i1: ~> rec y; scrutinee at j=i1 is y. ✓
//   - i=i0: setB-square boundary ~> ap_rec p, so the reduct is rec (papp … p j);
//           the scrutinee at i=i0 is papp … p j (squash boundary), rec of it. ✓
//   - i=i1: ~> ap_rec q, dual. ✓
// All four agree with the point branch via pabs/papp β and the squash/setB
// boundaries — exactly the dim-1 coherence, lifted to a square.
func (m *Machine) tryTruncIota(args []Val) (Val, bool) {
	if len(args) != 5 || m.Tr == nil {
		return nil, false
	}
	A, B, setB, f := args[0], args[1], args[2], args[3]
	x := m.Force(args[4])
	xn, ok := x.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(xn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	// Point branch: inc A a (a two-argument spine [A, a]).
	if m.Tr.TruncRoleOf(ref.Hash) == TRoleInc && len(sargs) == 2 {
		return m.Apply(f, sargs[1]), true
	}
	// Dim-2 interior branch: the scrutinee is a squash 2-cell observed at two
	// interval coordinates. The OUTER observation is the inner papp on the
	// scrutinee itself; the INNER (the squash square's outer dimension) is its
	// path argument. Detect papp (Trunc0 A) x y OUTER j with OUTER itself
	// papp (pathF (Trunc0 A) x y) p q (squash A x y p q) i.
	if m.Pa != nil && m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		jOuter := sargs[4] // the inner (along-the-path) interval coordinate j
		outer := m.Force(sargs[3])
		on, ok := outer.(VNeu)
		if !ok {
			return nil, false
		}
		ohead, oargs := spineParts(on.Spine)
		oref, ok := ohead.(NRef)
		if !ok || m.Pa.PathRoleOf(oref.Hash) != PRoleApp || len(oargs) != 5 {
			return nil, false
		}
		iSquash := oargs[4] // the outer (between-paths) interval coordinate i
		sq := m.Force(oargs[3])
		sn, ok := sq.(VNeu)
		if !ok {
			return nil, false
		}
		shead, qargs := spineParts(sn.Spine)
		sqref, ok := shead.(NRef)
		if !ok || m.Tr.TruncRoleOf(sqref.Hash) != TRoleSquash || len(qargs) != 5 {
			return nil, false
		}
		// squash A x y p q — qargs = [A, x, y, p, q]; reuse the recursor's own A.
		px, py, pp, pq := qargs[1], qargs[2], qargs[3], qargs[4]
		pappH, ok1 := m.Pa.PathHash(PRoleApp)
		pabsH, ok2 := m.Pa.PathHash(PRoleAbs)
		elimH, ok3 := m.Tr.TruncHash(TRoleElim)
		truncH, ok4 := m.Tr.TruncHash(TRoleTrunc)
		var pathFH Hash
		ok5 := false
		if m.Fib != nil {
			pathFH, ok5 = m.Fib.FibHash(FRolePathF)
		}
		if !(ok1 && ok2 && ok3 && ok4 && ok5) {
			return nil, false
		}
		truncA := m.Apply(m.refVal(truncH), A) // Trunc0 A : the inner path type
		// rec z := trunc0Rec A B setB f z
		rec := func(z Val) Val {
			return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
				m.refVal(elimH), A), B), setB), f), z)
		}
		// ap_rec p := pabs B (λk. rec (papp (Trunc0 A) px py p k)) : pathF B (rec px)(rec py)
		apRec := func(p Val) Val {
			body := VLam{Name: "k", Icit: Expl, Body: func(k Val) Val {
				scrut := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
					m.refVal(pappH), truncA), px), py), p), k)
				return rec(scrut)
			}}
			return m.Apply(m.Apply(m.refVal(pabsH), B), body)
		}
		rx, ry := rec(px), rec(py)
		arp, arq := apRec(pp), apRec(pq)
		// pathF B rx ry : the fibrant code for the inner path type of the B-square.
		pathBxy := m.Apply(m.Apply(m.Apply(m.refVal(pathFH), B), rx), ry)
		// setB rx ry arp arq : the square in B (pathF (pathF B rx ry) arp arq).
		sb := m.Apply(m.Apply(m.Apply(m.Apply(setB, rx), ry), arp), arq)
		// OUTER' := papp (pathF B rx ry) arp arq (setB …) i   : pathF B rx ry
		outerB := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
			m.refVal(pappH), pathBxy), arp), arq), sb), iSquash)
		// papp B rx ry OUTER' j
		r := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
			m.refVal(pappH), B), rx), ry), outerB), jOuter)
		return r, true
	}
	return nil, false
}

// tryCircIndIota fires the circle's DEPENDENT eliminator (§F / R-HIT / A9) on a
// saturated `circInd P b l x` spine (four arguments, scrutinee x at index 3):
//
//	circInd P b l base                            ~>  b
//	circInd P b l (papp Circle base base loop i)  ~>  pappP (λi. P (loop@i)) b b l i
//
// The induction principle: the motive P : El Circle -> UF is dependent, so the
// loop method l is a DEPENDENT path (pathP) over the line λi. P (loop@i), and the
// loop branch applies it with pappP at the same coordinate. Boundary-coherent: at
// i0/i1 the line is P base and pappP l reads off b, agreeing with the base branch.
// The dependent hcomp branch (comp over the motive line) is the deferred tail.
func (m *Machine) tryCircIndIota(args []Val) (Val, bool) {
	if len(args) != 4 || m.Ci == nil || m.Hi == nil {
		return nil, false
	}
	P, b, l := args[0], args[1], args[2]
	x := m.Force(args[3])
	xn, ok := x.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(xn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	// Point branch.
	if m.Hi.HitRoleOf(ref.Hash) == HRoleBase && len(sargs) == 0 {
		return b, true
	}
	// Loop branch: scrutinee papp Circle base base loop i (sargs [C,base,base,loop,i]).
	if m.Pa != nil && m.Pp != nil &&
		m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		if ln, ok := m.Force(sargs[3]).(VNeu); ok {
			lh, la := spineParts(ln.Spine)
			if lref, ok := lh.(NRef); ok && len(la) == 0 &&
				m.Hi.HitRoleOf(lref.Hash) == HRoleLoop {
				pappH, ok1 := m.Pa.PathHash(PRoleApp)
				pappPH, ok2 := m.Pp.PathPHash(PPRoleApp)
				if ok1 && ok2 {
					circle, base, loop := sargs[0], sargs[1], sargs[3]
					// line = λi. P (papp Circle base base loop i)
					line := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						loopAt := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(pappH), circle), base), base), loop), iv)
						return m.Apply(P, loopAt)
					}}
					// pappP line b b l i
					r := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(pappPH), line), b), b), l), sargs[4])
					return r, true
				}
			}
		}
	}
	// hcomp branch (dependent): circInd over a formal hcomp cell commutes via comp
	// over the MOTIVE line λi. P (hfill Circle φ u u0 i) — the dependent analogue
	// of the recursor's hcomp commute (which used hcomp; the dependent one uses
	// comp because the motive type varies along the filler):
	//   circInd P b l (hcomp Circle φ u u0)
	//     ~> comp (λi. P (hfill Circle φ u u0 i)) φ
	//             (λi h. circInd P b l (u i h))
	//             (circInd P b l u0)
	// The floor circInd P b l u0 : El (P u0) = El (A i0) (hfill at i0 is u0), and on
	// φ the wall agrees (hfill on φ is u i). So closed dependent elimination of an
	// hcomp cell reduces — the induction principle is total on all three generators.
	if m.Kn != nil {
		if m.Kn.KanRoleOf(ref.Hash) == KRoleHcomp && len(sargs) == 4 {
			if cf, ok := m.Force(sargs[0]).(VNeu); ok {
				ch, ca := spineParts(cf.Spine)
				if cref, ok := ch.(NRef); ok && len(ca) == 0 &&
					m.Hi.HitRoleOf(cref.Hash) == HRoleCircle {
					compH, okc := m.Kn.KanHash(KRoleComp)
					elimH, oke := m.Ci.CircIndHash()
					filler, okf := m.hfill(sargs[0], sargs[1], sargs[2], sargs[3])
					if okc && oke && okf {
						phi, u, u0 := sargs[1], sargs[2], sargs[3]
						ci := func(v Val) Val {
							return m.Apply(m.Apply(m.Apply(m.Apply(
								m.refVal(elimH), P), b), l), v)
						}
						motive := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
							return m.Apply(P, m.Apply(filler, iv)) // P (hfill … i)
						}}
						walls := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
							return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
								return ci(m.Apply(m.Apply(u, iv), h))
							}}
						}}
						r := m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(compH), motive), phi), walls), ci(u0))
						return r, true
					}
				}
			}
		}
	}
	return nil, false
}

// trySuspIndIota fires the suspension's DEPENDENT eliminator (§F / R-HIT / A9) on
// a saturated `suspInd A P pn ps pm x` spine (six arguments, scrutinee x at 5):
//
//	suspInd A P pn ps pm north                                   ~>  pn
//	suspInd A P pn ps pm south                                   ~>  ps
//	suspInd A P pn ps pm (papp (Susp A) north south (merid A a) i)
//	                                  ~>  pappP (λi. P (merid@i)) pn ps (pm a) i
//
// The meridian method pm a is a DEPENDENT path over λi. P (merid@i) from pn to ps.
func (m *Machine) trySuspIndIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.SuI == nil || m.Su == nil {
		return nil, false
	}
	P, pn, ps, pm := args[1], args[2], args[3], args[4]
	x := m.Force(args[5])
	xn, ok := x.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(xn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	switch m.Su.SuspRoleOf(ref.Hash) {
	case SuRoleNorth:
		if len(sargs) == 1 {
			return pn, true
		}
	case SuRoleSouth:
		if len(sargs) == 1 {
			return ps, true
		}
	}
	// Meridian branch: papp (Susp A) north south (merid A a) i.
	if m.Pa != nil && m.Pp != nil &&
		m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		if mn, ok := m.Force(sargs[3]).(VNeu); ok {
			mh, ma := spineParts(mn.Spine)
			if mref, ok := mh.(NRef); ok &&
				m.Su.SuspRoleOf(mref.Hash) == SuRoleMerid && len(ma) == 2 {
				pappH, ok1 := m.Pa.PathHash(PRoleApp)
				pappPH, ok2 := m.Pp.PathPHash(PPRoleApp)
				if ok1 && ok2 {
					suspA, nor, sou, mer, a, i := sargs[0], sargs[1], sargs[2], sargs[3], ma[1], sargs[4]
					line := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						meridAt := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(pappH), suspA), nor), sou), mer), iv)
						return m.Apply(P, meridAt)
					}}
					r := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(pappPH), line), pn), ps), m.Apply(pm, a)), i)
					return r, true
				}
			}
		}
	}
	// hcomp branch (dependent): suspInd over a formal hcomp cell commutes via comp
	// over the motive line λi. P (hfill (Susp A) φ u u0 i).
	if m.Kn != nil && m.Su.SuspRoleOf(ref.Hash) == SuRoleNone &&
		m.Kn.KanRoleOf(ref.Hash) == KRoleHcomp && len(sargs) == 4 {
		if cf, ok := m.Force(sargs[0]).(VNeu); ok {
			ch, ca := spineParts(cf.Spine)
			if cref, ok := ch.(NRef); ok && len(ca) == 1 &&
				m.Su.SuspRoleOf(cref.Hash) == SuRoleSusp {
				compH, okc := m.Kn.KanHash(KRoleComp)
				elimH, oke := m.SuI.SuspIndHash()
				filler, okf := m.hfill(sargs[0], sargs[1], sargs[2], sargs[3])
				if okc && oke && okf {
					A, phi, u, u0 := args[0], sargs[1], sargs[2], sargs[3]
					se := func(v Val) Val {
						return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(elimH), A), P), pn), ps), pm), v)
					}
					motive := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return m.Apply(P, m.Apply(filler, iv))
					}}
					walls := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
							return se(m.Apply(m.Apply(u, iv), h))
						}}
					}}
					return m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(compH), motive), phi), walls), se(u0)), true
				}
			}
		}
	}
	return nil, false
}

// tryQuotIndIota fires the fibrant quotient's DEPENDENT eliminator (§F / R-HIT /
// A9) on a saturated `quotInd A R P f rel x` spine (six arguments, scrutinee at 5):
//
//	quotInd A R P f rel (qinc A R a)                          ~>  f a
//	quotInd … (papp (Quotient A R) (qinc a) (qinc b) (qrel A R a b r) i)
//	                          ~>  pappP (λi. P (qrel@i)) (f a) (f b) (rel a b r) i
//
// The relation method rel a b r is a DEPENDENT path over λi. P (qrel@i).
func (m *Machine) tryQuotIndIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.QuI == nil || m.Qh == nil {
		return nil, false
	}
	P, f, rel := args[2], args[3], args[4]
	x := m.Force(args[5])
	xn, ok := x.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(xn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	// Point branch: quotInd … (qinc A R a) ~> f a (qinc spine [A, R, a]).
	if m.Qh.QuotHitRoleOf(ref.Hash) == QHRoleInc && len(sargs) == 3 {
		return m.Apply(f, sargs[2]), true
	}
	// Relation branch: papp (Quotient A R) (qinc a) (qinc b) (qrel A R a b r) i.
	if m.Pa != nil && m.Pp != nil &&
		m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		if qn, ok := m.Force(sargs[3]).(VNeu); ok {
			qh, qa := spineParts(qn.Spine)
			if qref, ok := qh.(NRef); ok &&
				m.Qh.QuotHitRoleOf(qref.Hash) == QHRoleRel && len(qa) == 5 {
				pappH, ok1 := m.Pa.PathHash(PRoleApp)
				pappPH, ok2 := m.Pp.PathPHash(PPRoleApp)
				if ok1 && ok2 {
					quotAR, qiA, qiB, qr := sargs[0], sargs[1], sargs[2], sargs[3]
					a, b, rr, i := qa[2], qa[3], qa[4], sargs[4]
					line := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						qrelAt := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(pappH), quotAR), qiA), qiB), qr), iv)
						return m.Apply(P, qrelAt)
					}}
					relabr := m.Apply(m.Apply(m.Apply(rel, a), b), rr)
					r := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(pappPH), line), m.Apply(f, a)), m.Apply(f, b)), relabr), i)
					return r, true
				}
			}
		}
	}
	// hcomp branch (dependent): quotInd over a formal hcomp cell commutes via comp
	// over the motive line λi. P (hfill (Quotient A R) φ u u0 i).
	if m.Kn != nil && m.Qh.QuotHitRoleOf(ref.Hash) == QHRoleNone &&
		m.Kn.KanRoleOf(ref.Hash) == KRoleHcomp && len(sargs) == 4 {
		if cf, ok := m.Force(sargs[0]).(VNeu); ok {
			ch, ca := spineParts(cf.Spine)
			if cref, ok := ch.(NRef); ok && len(ca) == 2 &&
				m.Qh.QuotHitRoleOf(cref.Hash) == QHRoleQuot {
				compH, okc := m.Kn.KanHash(KRoleComp)
				elimH, oke := m.QuI.QuotIndHash()
				filler, okf := m.hfill(sargs[0], sargs[1], sargs[2], sargs[3])
				if okc && oke && okf {
					A, R, phi, u, u0 := args[0], args[1], sargs[1], sargs[2], sargs[3]
					qe := func(v Val) Val {
						return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(elimH), A), R), P), f), rel), v)
					}
					motive := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return m.Apply(P, m.Apply(filler, iv))
					}}
					walls := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
							return qe(m.Apply(m.Apply(u, iv), h))
						}}
					}}
					return m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(compH), motive), phi), walls), qe(u0)), true
				}
			}
		}
	}
	return nil, false
}

// trySuspIota fires the suspension recursor rules (§F / R-HIT / A9) on a saturated
// `suspElim A P n s m x` spine (six arguments, the scrutinee x at index 5):
//
//	suspElim A P n s m north                                  ~>  n
//	suspElim A P n s m south                                  ~>  s
//	suspElim A P n s m (papp (Susp A) north south (merid A a) i)  ~>  papp P n s (m a) i
//	suspElim A P n s m (hcomp (Susp A) φ u u0)               ~>  hcomp P φ
//	                                                              (λi h. suspElim … (u i h))
//	                                                              (suspElim … u0)
//
// The point branches mirror a data eliminator; the meridian branch runs the
// meridian method m at the same interval coordinate, boundary-coherent on the
// nose (papp (merid A a) i0 ~> north, suspElim north ~> n, papp (m a) i0 ~> n).
func (m *Machine) trySuspIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.Su == nil {
		return nil, false
	}
	A, P, n, s, mer := args[0], args[1], args[2], args[3], args[4]
	x := m.Force(args[5])
	xn, ok := x.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(xn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	switch m.Su.SuspRoleOf(ref.Hash) {
	case SuRoleNorth:
		if len(sargs) == 1 {
			return n, true
		}
	case SuRoleSouth:
		if len(sargs) == 1 {
			return s, true
		}
	}
	// Meridian branch: scrutinee papp (Susp A) north south (merid A a) i.
	if m.Pa != nil && m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		if mn, ok := m.Force(sargs[3]).(VNeu); ok {
			mh, ma := spineParts(mn.Spine)
			if mref, ok := mh.(NRef); ok &&
				m.Su.SuspRoleOf(mref.Hash) == SuRoleMerid && len(ma) == 2 {
				if pappH, ok := m.Pa.PathHash(PRoleApp); ok {
					a, i := ma[1], sargs[4]
					// papp P n s (m a) i
					r := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(pappH), P), n), s), m.Apply(mer, a)), i)
					return r, true
				}
			}
		}
	}
	// hcomp branch: the recursor commutes with composition at Susp A.
	if m.Kn != nil && m.Kn.KanRoleOf(ref.Hash) == KRoleHcomp && len(sargs) == 4 {
		if sf, ok := m.Force(sargs[0]).(VNeu); ok {
			sh, _ := spineParts(sf.Spine)
			if sref, ok := sh.(NRef); ok && m.Su.SuspRoleOf(sref.Hash) == SuRoleSusp {
				if hcompH, ok := m.Kn.KanHash(KRoleHcomp); ok {
					if elimH, ok := m.Su.SuspHash(SuRoleElim); ok {
						phi, u, u0 := sargs[1], sargs[2], sargs[3]
						se := func(v Val) Val {
							return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
								m.refVal(elimH), A), P), n), s), mer), v)
						}
						innerU := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
							return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
								return se(m.Apply(m.Apply(u, iv), h))
							}}
						}}
						return m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(hcompH), P), phi), innerU), se(u0)), true
					}
				}
			}
		}
	}
	return nil, false
}

// tryQuotHitIota fires the fibrant-quotient recursor rules (§F / R-HIT / A9) on a
// saturated `quotElim A R P f rel x` spine (six arguments, scrutinee x at index 5):
//
//	quotElim A R P f rel (qinc A R a)                          ~>  f a
//	quotElim … (papp (Quotient A R) (qinc a) (qinc b) (qrel A R a b r) i)
//	                                                           ~>  papp P (f a) (f b) (rel a b r) i
//	quotElim … (hcomp (Quotient A R) φ u u0)                  ~>  hcomp P φ (λi h. quotElim … (u i h)) (quotElim … u0)
//
// The relation branch sends a generator path `qrel a b r` to the chosen method
// path `rel a b r`, boundary-coherent (qrel boundary qinc a/qinc b, quotElim qinc
// ~> f, rel a b r boundary f a/f b). This is the path-respecting, EFFECTIVE
// quotient the strict Quot (v2) cannot give.
func (m *Machine) tryQuotHitIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.Qh == nil {
		return nil, false
	}
	A, R, P, f, rel := args[0], args[1], args[2], args[3], args[4]
	x := m.Force(args[5])
	xn, ok := x.(VNeu)
	if !ok {
		return nil, false
	}
	head, sargs := spineParts(xn.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	// Point branch: quotElim … (qinc A R a) ~> f a (qinc spine is [A, R, a]).
	if m.Qh.QuotHitRoleOf(ref.Hash) == QHRoleInc && len(sargs) == 3 {
		return m.Apply(f, sargs[2]), true
	}
	// Relation branch: scrutinee papp (Quotient A R) (qinc a) (qinc b) (qrel A R a b r) i.
	if m.Pa != nil && m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(sargs) == 5 {
		if qn, ok := m.Force(sargs[3]).(VNeu); ok {
			qh, qa := spineParts(qn.Spine)
			if qref, ok := qh.(NRef); ok &&
				m.Qh.QuotHitRoleOf(qref.Hash) == QHRoleRel && len(qa) == 5 {
				if pappH, ok := m.Pa.PathHash(PRoleApp); ok {
					a, b, r, i := qa[2], qa[3], qa[4], sargs[4]
					// papp P (f a) (f b) (rel a b r) i
					relabr := m.Apply(m.Apply(m.Apply(rel, a), b), r)
					res := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(pappH), P), m.Apply(f, a)), m.Apply(f, b)), relabr), i)
					return res, true
				}
			}
		}
	}
	// hcomp branch: the recursor commutes with composition at Quotient A R.
	if m.Kn != nil && m.Kn.KanRoleOf(ref.Hash) == KRoleHcomp && len(sargs) == 4 {
		if qf, ok := m.Force(sargs[0]).(VNeu); ok {
			qhh, _ := spineParts(qf.Spine)
			if qref, ok := qhh.(NRef); ok && m.Qh.QuotHitRoleOf(qref.Hash) == QHRoleQuot {
				if hcompH, ok := m.Kn.KanHash(KRoleHcomp); ok {
					if elimH, ok := m.Qh.QuotHitHash(QHRoleElim); ok {
						phi, u, u0 := sargs[1], sargs[2], sargs[3]
						qe := func(v Val) Val {
							return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
								m.refVal(elimH), A), R), P), f), rel), v)
						}
						innerU := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
							return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
								return qe(m.Apply(m.Apply(u, iv), h))
							}}
						}}
						return m.Apply(m.Apply(m.Apply(m.Apply(
							m.refVal(hcompH), P), phi), innerU), qe(u0)), true
					}
				}
			}
		}
	}
	return nil, false
}

// tryForallIota fires the ∀-cofibration rule on `forallF φ` (one argument): when
// the line φ is constantly ⊤ (its value at a fresh interval point forces to ftop,
// so φ does not depend on i and is ⊤), the quantification holds — `forallF (λ_. ⊤)
// ~> ⊤`. Otherwise it stays neutral (a genuine cofibration the Kan rules carry in
// a face). This is the slice the transp-over-Glue ⊤-degeneracy needs; the general
// ∀ on a proper line is honest-stuck.
func (m *Machine) tryForallIota(args []Val) (Val, bool) {
	if len(args) != 1 || m.Fc == nil {
		return nil, false
	}
	at := m.Apply(args[0], VNeu{Spine: NRef{Hash: kanFreshSentinel}})
	if c, ok := m.faceConst(at); ok && c == CRoleTop {
		if topH, ok := m.Fc.FaceHash(CRoleTop); ok {
			return m.refVal(topH), true
		}
	}
	return nil, false
}

// coindXSentinel marks the recursive position X when deriving a functor's
// action fmapF by recursion on the strictly-positive code F (R-COIND / C5a).
// Like kanFreshSentinel it is a reserved hash that is never a real content hash.
var coindXSentinel = Hash{
	0xc0, 0xda, 0x7a, 0x10, 0xc0, 0xda, 0x7a, 0x10, 0xc0, 0xda, 0x7a, 0x10, 0xc0, 0xda, 0x7a, 0x10,
	0xc0, 0xda, 0x7a, 0x10, 0xc0, 0xda, 0x7a, 0x10, 0xc0, 0xda, 0x7a, 0x10, 0xc0, 0xda, 0x7a, 0x10,
}

// tryCoindIota fires the coinductive observation rule on `out F s` (two args):
//
// tryGuardIota fires the ▹ functor-map rule on `lmap k A B g l` (five args, the
// clock-explicit form):
//
//	lmap k A B g (next k A x)  ~>  next k B (g x)              (functor on a `next` intro)
//	lmap k A B g l             ~>  next k B (g _)   if g const  (constant-collapse, ANY l)
//
// The first fires when l forces to a saturated `next` intro. The second — the ▹κ functor
// law that a CONSTANT g maps EVERY delayed value to the constant `next` — fires for ANY
// l (including a stuck neutral) once g is detected constant by probing it at a fresh
// sentinel argument (g sentinel does not mention the sentinel ⇒ g ignores its argument).
// Sound in the later/topos-of-trees model: at stage n+1, `lmap g` is `g_n` and a constant
// g yields `(next (g _))_{n+1}`; at stage 0, ▹ is terminal so both agree. Confluent with
// the `next`-intro case (when both fire, g x ≡ g _ since g is constant). This is the
// guard-group ι that closes the E2-converse's two delayed-tail correction paths: with a
// constant g the previously-stuck `lmap (const ra) NL` (NL the neutral delayed dfix-self)
// reduces to `next ra`, so the converter body's endpoints land on the nose. Bounded
// (fires once per lmap, the probe never forces a recursion), no conv.go change.
func (m *Machine) tryGuardIota(args []Val) (Val, bool) {
	if len(args) != 5 || m.Gd == nil {
		return nil, false
	}
	k, B, g := args[0], args[2], args[3]
	nextH, ok := m.Gd.GuardHash(LRoleNext)
	if !ok {
		return nil, false
	}
	l := m.Force(args[4])
	if ln, ok := l.(VNeu); ok {
		if head, nargs := spineParts(ln.Spine); len(nargs) == 3 {
			if nref, ok := head.(NRef); ok && m.Gd.GuardRoleOf(nref.Hash) == LRoleNext {
				// next k A x ; apply g to x and re-wrap under `next k B`.
				x := nargs[2]
				return m.Apply(m.Apply(m.Apply(m.refVal(nextH), k), B), m.Apply(g, x)), true
			}
		}
	}
	// Constant-collapse: g ignores its argument ⇒ lmap g l ~> next k B (g _) for ANY l.
	probe := m.Apply(g, VNeu{Spine: NRef{Hash: lmapConstSentinel}})
	if !mentionsRefVal(m, probe, lmapConstSentinel) {
		return m.Apply(m.Apply(m.Apply(m.refVal(nextH), k), B), probe), true
	}
	return nil, false
}

// tryGuardLapIota fires the ▹ applicative rule on `lap k A B lf lx` (five args):
//
//	lap k A B (next k _ f) (next k A x)  ~>  next k B (f x)
//
// It fires when both lf and lx force to saturated `next` intros; otherwise stuck.
// This makes ▹ an applicative functor (next is pure, lap is ⊛). The clock k is
// carried onto the result.
func (m *Machine) tryGuardLapIota(args []Val) (Val, bool) {
	if len(args) != 5 || m.Gd == nil {
		return nil, false
	}
	k, B := args[0], args[2]
	nextArg := func(v Val) (Val, bool) {
		n, ok := m.Force(v).(VNeu)
		if !ok {
			return nil, false
		}
		head, nargs := spineParts(n.Spine)
		nref, ok := head.(NRef)
		if !ok || m.Gd.GuardRoleOf(nref.Hash) != LRoleNext || len(nargs) != 3 {
			return nil, false
		}
		return nargs[2], true
	}
	f, ok1 := nextArg(args[3])
	x, ok2 := nextArg(args[4])
	if !ok1 || !ok2 {
		return nil, false
	}
	nextH, ok := m.Gd.GuardHash(LRoleNext)
	if !ok {
		return nil, false
	}
	return m.Apply(m.Apply(m.Apply(m.refVal(nextH), k), B), m.Apply(f, x)), true
}

// tryForceIota fires the clock-β / force-next coherence on `force A g` (two args) —
// the SOLE elimination of the ▹ modality, gated by clock quantification:
//
//	force A (λκ. next κ A x)  ~>  λκ. x
//
// It probes g at a FRESH clock sentinel κ. The rule fires only when g κ forces to a
// `next` intro whose clock is exactly κ (so the delayed value genuinely lives on the
// quantified clock) AND whose payload x is clock-closed (does not mention κ — so the
// result is independent of the clock). The result is the constant clock function
// `λκ. x`. On any other g (a neutral, a `next` on a different/free clock, or a payload
// that uses κ) it stays stuck — the normalizer cannot loop, and the unsound
// `force : ▹A -> A` is never reachable (the clock-indexing of Later makes its type
// uninhabitable). This is what turns guarded recursion into genuine coinduction.
func (m *Machine) tryForceIota(args []Val) (Val, bool) {
	if len(args) != 2 || m.Gd == nil {
		return nil, false
	}
	g := args[1]
	kappa := VNeu{Spine: NRef{Hash: clockFreshSentinel}}
	gk := m.Force(m.Apply(g, kappa))
	gn, ok := gk.(VNeu)
	if !ok {
		return nil, false
	}
	head, nargs := spineParts(gn.Spine)
	nref, ok := head.(NRef)
	if !ok || m.Gd.GuardRoleOf(nref.Hash) != LRoleNext || len(nargs) != 3 {
		return nil, false
	}
	// next κ A x : the delayed value must be on the BOUND clock κ (the sentinel).
	cn, ok := m.Force(nargs[0]).(VNeu)
	if !ok {
		return nil, false
	}
	ch, cas := spineParts(cn.Spine)
	chref, ok := ch.(NRef)
	if !ok || chref.Hash != clockFreshSentinel || len(cas) != 0 {
		return nil, false
	}
	// x must be clock-closed (parametricity): if it mentions κ the value genuinely
	// depends on the clock and force must NOT discharge the guard.
	x := nargs[2]
	if mentionsRefVal(m, x, clockFreshSentinel) {
		return nil, false
	}
	// force A (λκ. next κ A x) ~> λκ. x (constant in the clock).
	return VLam{Name: "k", Icit: Expl, Body: func(_ Val) Val { return x }}, true
}

// tryForceDIota fires the DEPENDENT clock-β / force-next coherence on `forceD A g` (two
// args), where the payload TYPE varies with the clock (A : Clock -> UF):
//
//	forceD A (λκ. next κ (A κ) x)  ~>  λκ. x          (x : El (A κ) MAY mention κ)
//
// Like `force` it probes g at a fresh clock sentinel and fires only when g produces a
// `next` on the BOUND clock — but it does NOT require the payload to be clock-closed,
// because the type `A κ` is clock-varying (e.g. a guarded stream's tail `▹κ (gStr κ A)`).
// Soundness is by CLOCK INERTNESS: `Clock` has no eliminator, so a payload can mention κ
// but can never inspect it — g is therefore uniform in the clock, and the probe at the
// sentinel is representative. The result re-applies g at the bound clock and strips the
// `next`, substituting the real clock for κ automatically. Stays neutral on any other g.
func (m *Machine) tryForceDIota(args []Val) (Val, bool) {
	if len(args) != 2 || m.Gd == nil {
		return nil, false
	}
	g := args[1]
	// Probe g at a fresh clock sentinel to decide whether the ι fires.
	kappa := VNeu{Spine: NRef{Hash: clockFreshSentinel}}
	gk := m.Force(m.Apply(g, kappa))
	gn, ok := gk.(VNeu)
	if !ok {
		return nil, false
	}
	head, nargs := spineParts(gn.Spine)
	nref, ok := head.(NRef)
	if !ok || m.Gd.GuardRoleOf(nref.Hash) != LRoleNext || len(nargs) != 3 {
		return nil, false
	}
	// next κ (A κ) x : the delayed value must be on the BOUND clock κ (the sentinel).
	cn, ok := m.Force(nargs[0]).(VNeu)
	if !ok {
		return nil, false
	}
	ch, cas := spineParts(cn.Spine)
	chref, ok := ch.(NRef)
	if !ok || chref.Hash != clockFreshSentinel || len(cas) != 0 {
		return nil, false
	}
	// forceD A (λκ. next κ (A κ) x) ~> λκ. x. The payload may mention κ, so re-apply g at
	// the bound clock and strip the `next` (substituting the real clock for κ). Sound by
	// clock inertness: g cannot branch on the clock, so this is uniform.
	return VLam{Name: "k", Icit: Expl, Body: func(kv Val) Val {
		body := m.Force(m.Apply(g, kv))
		if bn, ok := body.(VNeu); ok {
			bh, bargs := spineParts(bn.Spine)
			if br, ok := bh.(NRef); ok && m.Gd.GuardRoleOf(br.Hash) == LRoleNext && len(bargs) == 3 {
				return bargs[2]
			}
		}
		return body
	}}, true
}

// tryLaterAppIota fires the universe-level ▹κ application on `laterApp k A f la` (four
// args), the companion to lmap that lifts a UF-VALUED family through a delayed value:
//
//	laterApp k A f (next k A x)  ~>  Later k (f x)
//
// It fires when la forces to a `next` intro; otherwise stuck. Unlike lmap (which lifts an
// El→El function, producing a `next`), laterApp lifts an `El A -> UF` family, producing a
// `Later k` of the applied type — the piece a UF-valued recursive occurrence (bisimilarity)
// needs to sit under ▹κ. Sound: it only re-wraps under `Later`, never escapes it.
func (m *Machine) tryLaterAppIota(args []Val) (Val, bool) {
	if len(args) != 4 || m.Gd == nil {
		return nil, false
	}
	k, f := args[0], args[2]
	la := m.Force(args[3])
	ln, ok := la.(VNeu)
	if !ok {
		return nil, false
	}
	head, nargs := spineParts(ln.Spine)
	nref, ok := head.(NRef)
	if !ok || m.Gd.GuardRoleOf(nref.Hash) != LRoleNext || len(nargs) != 3 {
		return nil, false
	}
	laterH, ok := m.Gd.GuardHash(LRoleLater)
	if !ok {
		return nil, false
	}
	// next k A x ; apply f to x and wrap the resulting code under `Later k`.
	x := nargs[2]
	return m.Apply(m.Apply(m.refVal(laterH), k), m.Apply(f, x)), true
}

// tryLapDIota fires the DEPENDENT guarded application on `lapD k A B lf la` (five args):
//
//	lapD k A B (next k (piF A B) f) (next k A x)  ~>  next k (B x) (f x)
//
// It fires when both lf and lx force to `next` intros. Unlike lap (non-dependent codomain),
// the result `f x : El (B x)` lands in a clock-varying fibre, so the wrapper is
// `next k (B x) (…)` — whose type `El (Later k (B x))` is exactly `El (laterApp k A B la)`,
// lapD's declared result. The combinator the converse's path-assembly recursion uses to
// apply the delayed (dependent) recursive path-builder to the delayed tail index.
func (m *Machine) tryLapDIota(args []Val) (Val, bool) {
	if len(args) != 5 || m.Gd == nil {
		return nil, false
	}
	k, B := args[0], args[2]
	nextArg := func(v Val) (Val, bool) {
		n, ok := m.Force(v).(VNeu)
		if !ok {
			return nil, false
		}
		head, nargs := spineParts(n.Spine)
		nref, ok := head.(NRef)
		if !ok || m.Gd.GuardRoleOf(nref.Hash) != LRoleNext || len(nargs) != 3 {
			return nil, false
		}
		return nargs[2], true
	}
	f, ok1 := nextArg(args[3])
	x, ok2 := nextArg(args[4])
	if !ok1 || !ok2 {
		return nil, false
	}
	nextH, ok := m.Gd.GuardHash(LRoleNext)
	if !ok {
		return nil, false
	}
	// next k (B x) (f x): the fibre B x of the result, the value f x.
	return m.Apply(m.Apply(m.Apply(m.refVal(nextH), k), m.Apply(B, x)), m.Apply(f, x)), true
}

//	out F (unfold F S c s)  ~>  fmapF F (unfold F S c) (c s)
//
// It fires when s forces to a saturated unfold (four args F' S c s0); on any
// other s (a neutral) it stays stuck.
func (m *Machine) tryCoindIota(args []Val) (Val, bool) {
	if len(args) != 2 || m.Cn == nil || m.Fib == nil {
		return nil, false
	}
	F := args[0]
	s := m.Force(args[1])
	sn, ok := s.(VNeu)
	if !ok {
		return nil, false
	}
	head, uargs := spineParts(sn.Spine)
	uref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	// Kan-over-Nu (the global endpoint-repair enabler): `out` commutes with an
	// hcomp over a coinductive type. `Nu F` is NEGATIVE — not definitionally its
	// unfolding (unlike gfix), so the type-unfold trick the Σ/gStr repair uses does
	// not apply; instead the COMPOSITION is defined by its observation:
	//   out F (hcomp (Nu F) φ u u0)
	//     ~> hcomp (F (Nu F)) φ (λi h. out F (u i h)) (out F u0)
	// Sound: it is the defining equation of hcomp for a coinductive type (the
	// observation commutes with the composition). Bounded: the result's type
	// `F (Nu F)` is a structural former (a `sigmaF` for streams), so the landed Σ
	// Kan rule fires next and `out` is not re-introduced on the same hcomp. This
	// makes `head`/`tail (hcomp (Str A) …)` compute componentwise — opening the
	// global repair route for the E2 converse. (transp/comp over Nu are the duals,
	// deferred until a consumer needs them.)
	if m.Kn != nil && m.Kn.KanRoleOf(uref.Hash) == KRoleHcomp && len(uargs) == 4 && m.headIsNu(uargs[0]) {
		F := args[0]
		phi, u, u0 := uargs[1], uargs[2], uargs[3]
		hcompH, ok1 := m.Kn.KanHash(KRoleHcomp)
		outH, ok2 := m.Cn.CoindHash(NRoleOut)
		nuH, ok3 := m.Cn.CoindHash(NRoleNu)
		if ok1 && ok2 && ok3 {
			out := func(x Val) Val { return m.Apply(m.Apply(m.refVal(outH), F), x) }
			target := m.Apply(F, m.Apply(m.refVal(nuH), F)) // F (Nu F)
			wall := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
				return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
					return out(m.Apply(m.Apply(u, iv), h)) // out F (u i h)
				}}
			}}
			return m.Apply(m.Apply(m.Apply(m.Apply(
				m.refVal(hcompH), target), phi), wall), out(u0)), true
		}
	}
	// β for the one-level constructor: out F (nuCons F x) ~> x.
	if m.Cn.CoindRoleOf(uref.Hash) == NRoleNuCons && len(uargs) == 2 {
		return uargs[1], true // x
	}
	if m.Cn.CoindRoleOf(uref.Hash) != NRoleUnfold || len(uargs) != 4 {
		return nil, false
	}
	// unfold F' S c s0 ; the recursive map g = unfold F' S c (the first three
	// args re-applied), the seed-step value is c s0.
	uF, uS, uc, us := uargs[0], uargs[1], uargs[2], uargs[3]
	unfoldH, ok := m.Cn.CoindHash(NRoleUnfold)
	if !ok {
		return nil, false
	}
	nuH, ok := m.Cn.CoindHash(NRoleNu)
	if !ok {
		return nil, false
	}
	// g : El S -> El (Nu F), the corecursive call.
	g := VLam{Name: "x", Icit: Expl, Body: func(x Val) Val {
		return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(unfoldH), uF), uS), uc), x)
	}}
	cs := m.Apply(uc, us) // c s0 : El (F S)
	// target code F (Nu F): the shape of the result, used to rebuild codes.
	nuF := m.Apply(m.refVal(nuH), F)
	target := m.Apply(F, nuF)
	// probe code F X: where the recursive position X occurs.
	probe := m.Apply(F, VNeu{Spine: NRef{Hash: coindXSentinel}})
	if mapped, ok := m.fmapF(probe, target, g, cs); ok {
		return mapped, true
	}
	return nil, false
}

// fmapF applies the functorial action of a strictly-positive code over its
// recursive position. probe is F applied to the X-sentinel (so an occurrence of
// the sentinel marks where X — the recursive argument — sits); target is the
// result code F (Nu F) (used to rebuild constructor codes); g is the map to
// apply at X-positions; val inhabits F S. Returns ok=false on an unsupported
// (non-strictly-positive / unknown) shape, leaving `out` stuck.
func (m *Machine) fmapF(probe, target, g, val Val) (Val, bool) {
	// X position: the probe IS the sentinel ⇒ val : El S, map it.
	if pn, ok := m.Force(probe).(VNeu); ok {
		if ref, ok := pn.Spine.(NRef); ok && ref.Hash == coindXSentinel {
			return m.Apply(g, val), true
		}
	}
	// No recursive position anywhere in this sub-code ⇒ identity (covers fib T
	// and every X-free shape).
	if !mentionsRefVal(m, probe, coindXSentinel) {
		return val, true
	}
	// piF A Pfam (A X-free by strict positivity): map under the binder.
	if role, pargs, ok := m.fibFormer(probe); ok && role == FRolePiF && len(pargs) == 2 {
		trole, targs, tok := m.fibFormer(target)
		if !tok || trole != FRolePiF || len(targs) != 2 {
			return nil, false
		}
		pf, tf := pargs[1], targs[1]
		return VLam{Name: "x", Icit: Expl, Body: func(x Val) Val {
			r, _ := m.fmapF(m.Apply(pf, x), m.Apply(tf, x), g, m.Apply(val, x))
			return r
		}}, true
	}
	// sigmaF A Pfam (A X-free): keep the first component, recurse on the second,
	// rebuilding the pair at the target (F (Nu F)) codes.
	if m.Si != nil {
		if role, pargs, ok := m.sigmaFormer(probe); ok && role == GRoleSigma && len(pargs) == 2 {
			trole, targs, tok := m.sigmaFormer(target)
			if !tok || trole != GRoleSigma || len(targs) != 2 {
				return nil, false
			}
			vn, ok := m.Force(val).(VNeu)
			if !ok {
				return nil, false
			}
			vh, vargs := spineParts(vn.Spine)
			vref, ok := vh.(NRef)
			if !ok || m.Si.SigmaRoleOf(vref.Hash) != GRolePair || len(vargs) != 4 {
				return nil, false
			}
			a, b := vargs[2], vargs[3]
			pairH, ok := m.Si.SigmaHash(GRolePair)
			if !ok {
				return nil, false
			}
			At, Bt := targs[0], targs[1]
			bMapped, ok := m.fmapF(m.Apply(pargs[1], a), m.Apply(Bt, a), g, b)
			if !ok {
				return nil, false
			}
			return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pairH), At), Bt), a), bMapped), true
		}
	}
	return nil, false
}

// sigmaFormer decomposes an inner-Sigma former value (sigmaF A B), returning its
// role and argument values — the SigmaInfo analogue of fibFormer.
func (m *Machine) sigmaFormer(v Val) (SigmaRole, []Val, bool) {
	if m.Si == nil {
		return GRoleNone, nil, false
	}
	n, ok := m.Force(v).(VNeu)
	if !ok {
		return GRoleNone, nil, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return GRoleNone, nil, false
	}
	role := m.Si.SigmaRoleOf(ref.Hash)
	if role == GRoleNone {
		return GRoleNone, nil, false
	}
	return role, args, true
}

// trySigmaIota fires the inner-Sigma projection rules (§F / R-SIGMA / A5):
//
//	fstF A B (pairF A' B' a b)  ~>  a
//	sndF A B (pairF A' B' a b)  ~>  b
//
// fstF/sndF take three args (A, B, p); they fire when p forces to a saturated
// pairF (four args A' B' a b). On any other p (a neutral pair) they stay stuck —
// the projection of a variable is canonical neutral data (NFst/NSnd's inner-layer
// analogue), exactly as papp on a neutral path stays stuck.
func (m *Machine) trySigmaIota(role SigmaRole, args []Val) (Val, bool) {
	if len(args) != 3 || m.Si == nil {
		return nil, false
	}
	p := m.Force(args[2])
	pn, ok := p.(VNeu)
	if !ok {
		return nil, false
	}
	head, pargs := spineParts(pn.Spine)
	pref, ok := head.(NRef)
	if !ok || m.Si.SigmaRoleOf(pref.Hash) != GRolePair || len(pargs) != 4 {
		return nil, false
	}
	if role == GRoleFst {
		return pargs[2], true // a
	}
	return pargs[3], true // b
}

// tryGlueIota fires the inner Glue elimination rule (§F / R-GLUE / A6) on a
// saturated `unglue A φ T e g` spine (six arguments, the scrutinee g at index 5):
//
//	unglue A φ T e (glue A' φ' T' e' t a)  ~>  a        (β)
//
// The scrutinee is forced — logged, as always — and the rule fires when it is a
// saturated glue intro (six args, the A-component a at index 5). On any other g
// (a neutral) unglue stays stuck.
//
// The φ≡⊤ unglue boundary fires on a non-glue scrutinee:
//
//	unglue A ⊤ T e g  ~>  equivFun (T htop) A (e htop) g
//
// On a total face the glued type IS T (the type boundary), so g : El (T htop) and
// unglue is the equivalence's forward map. Because `equivFun` is its OWN builtin
// (the equiv group, registered before Glue), the reduct is a clean spine on a
// stable hash — no fstF/isEquiv reconstruction, so no containment break. See
// ref_docs/wootz/GLUE-DESIGN.md §4.3.
func (m *Machine) tryGlueIota(args []Val) (Val, bool) {
	// unglue A φ T e g — five arguments, the scrutinee g at index 4.
	if len(args) != 5 || m.Gl == nil {
		return nil, false
	}
	g := m.Force(args[4])
	// β: unglue of a glued pair is its A-component (fires for any face).
	// glue A φ T e t a is a six-argument intro; a is at index 5.
	if gn, ok := g.(VNeu); ok {
		head, gargs := spineParts(gn.Spine)
		if gref, ok := head.(NRef); ok &&
			m.Gl.GlueRoleOf(gref.Hash) == URoleGlueIn && len(gargs) == 6 {
			return gargs[5], true // the A-component a
		}
	}
	// ⊤ boundary: on a total face, unglue is equivFun (e htop) applied to g.
	if m.Sy != nil {
		if c, ok := m.faceConst(args[1]); ok && c == CRoleTop {
			efH, ok1 := m.Gl.EquivFunHash()
			hth, ok2 := m.Sy.SysHash(SRoleTop)
			if ok1 && ok2 {
				htop := m.refVal(hth)
				A, T, e := args[0], args[2], args[3]
				thtop := m.Apply(T, htop) // T htop : UF
				ehtop := m.Apply(e, htop) // e htop : El (Equiv (T htop) A)
				// equivFun (T htop) A (e htop) g
				r := m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(efH), thtop), A), ehtop), g)
				return r, true
			}
		}
	}
	return nil, false
}

// tryFsplitIota fires the face-split eliminator rule (§F / R-BOX / A3) on a
// saturated `fsplit A φ ψ u v h` spine (six arguments). It dispatches by which
// disjunct of `for φ ψ` has become ⊤:
//
//	fsplit A φ ψ u v h  ~>  u htop   when φ ≡ ⊤
//	fsplit A φ ψ u v h  ~>  v htop   when ψ ≡ ⊤
//
// and stays neutral when both faces are proper. This is the dispatch primitive
// the Kan rules build systems from: a system `[φ ↦ u; ψ ↦ v]` is `λh. fsplit A φ
// ψ u v h`, a genuine `holds (for φ ψ) -> El A` function that *selects* by face
// — which `holds φ -> El A` functions cannot do (no `holds` eliminator). Overlap
// (both ⊤) resolves to the φ-branch; soundness on overlap is the caller's
// agreement obligation (for disjoint faces like ieq0/ieq1 it never arises). See
// ref_docs/wootz/RBOX-DESIGN.md §1.
func (m *Machine) tryFsplitIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.Fc == nil || m.Sy == nil {
		return nil, false
	}
	hth, ok := m.Sy.SysHash(SRoleTop)
	if !ok {
		return nil, false
	}
	htop := m.refVal(hth)
	if c, ok := m.faceConst(args[1]); ok && c == CRoleTop {
		return m.Apply(args[3], htop), true // u htop
	}
	if c, ok := m.faceConst(args[2]); ok && c == CRoleTop {
		return m.Apply(args[4], htop), true // v htop
	}
	// Proof-directed dispatch: the validity proof itself may witness which disjunct
	// holds (a `horl`/`horr` intro), even when the FACE is still proper. Selecting
	// the matching branch applied to the carried sub-proof is the same reduct the
	// ⊤-face rule gives, and is what makes a system's branch types line up under a
	// bound face proof (`fsplit A φ ψ u v (horl φ ψ h) ~> u h`).
	if left, hp, ok := m.holdsInjection(args[5]); ok {
		if left {
			return m.Apply(args[3], hp), true // u h
		}
		return m.Apply(args[4], hp), true // v h
	}
	return nil, false
}

// holdsInjection reports whether v forces to a `holds`-disjunction injection — a
// saturated `horl φ ψ h` (left=true) or `horr φ ψ h` (left=false) intro — and if so
// returns the carried sub-proof h (the third argument). This is the proof witness a
// face-split eliminator dispatches on when the face itself is still a proper (non-⊤)
// cofibration but the validity proof already names its disjunct.
func (m *Machine) holdsInjection(v Val) (left bool, inner Val, ok bool) {
	if m.Sy == nil {
		return false, nil, false
	}
	n, isNeu := m.Force(v).(VNeu)
	if !isNeu {
		return false, nil, false
	}
	head, as := spineParts(n.Spine)
	ref, isRef := head.(NRef)
	if !isRef || len(as) != 3 {
		return false, nil, false
	}
	switch m.Sy.SysRoleOf(ref.Hash) {
	case SRoleOrL:
		return true, as[2], true
	case SRoleOrR:
		return false, as[2], true
	}
	return false, nil, false
}

// trySysUIota fires the UF-valued face-split rule (§F / R-BOX / A8) on a saturated
// `sysU φ ψ u v h` spine (five arguments — there is no `A` argument, the result is
// UF). It is the type-level counterpart of tryFsplitIota, dispatching by which
// disjunct of `for φ ψ` has become ⊤:
//
//	sysU φ ψ u v h  ~>  u htop   when φ ≡ ⊤
//	sysU φ ψ u v h  ~>  v htop   when ψ ≡ ⊤
//
// and stays neutral when both faces are proper. This is the primitive a type-level
// system (e.g. the T-component of a univalence Glue line) is built from; a plain
// `holds φ -> UF` cannot dispatch (no `holds` eliminator). Overlap takes the
// φ-branch; agreement is the caller's obligation (vacuous for disjoint faces).
func (m *Machine) trySysUIota(args []Val) (Val, bool) {
	if len(args) != 5 || m.Fc == nil || m.Sy == nil {
		return nil, false
	}
	hth, ok := m.Sy.SysHash(SRoleTop)
	if !ok {
		return nil, false
	}
	htop := m.refVal(hth)
	if c, ok := m.faceConst(args[0]); ok && c == CRoleTop {
		return m.Apply(args[2], htop), true // u htop
	}
	if c, ok := m.faceConst(args[1]); ok && c == CRoleTop {
		return m.Apply(args[3], htop), true // v htop
	}
	// Proof-directed dispatch (see tryFsplitIota): a horl/horr validity proof picks
	// its disjunct's branch even on a proper face — this is what lets the T-component
	// `sysU … (horl … h) ~> A` reduce inside a system branch so its type lines up.
	if left, hp, ok := m.holdsInjection(args[4]); ok {
		if left {
			return m.Apply(args[2], hp), true // u h
		}
		return m.Apply(args[3], hp), true // v h
	}
	return nil, false
}

// trySplitDIota fires the dependent face-split rule (§F / R-BOX / A8) on a
// saturated `fsplitD φ ψ C u v h` spine (six arguments — the motive C is at index 2,
// the branches u/v at 3/4, the proof h at 5). It dispatches exactly like
// tryFsplitIota / trySysUIota by which disjunct of `for φ ψ` has become ⊤:
//
//	fsplitD φ ψ C u v h  ~>  u htop   when φ ≡ ⊤
//	fsplitD φ ψ C u v h  ~>  v htop   when ψ ≡ ⊤
//
// and stays neutral when both faces are proper. The motive is ι-irrelevant to the
// reduct (it only types the branches). This is the primitive a dependent
// element-system — e.g. the e-component of a univalence Glue line, whose type
// varies per face — is built from. Overlap takes the φ-branch.
func (m *Machine) trySplitDIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.Fc == nil || m.Sy == nil {
		return nil, false
	}
	hth, ok := m.Sy.SysHash(SRoleTop)
	if !ok {
		return nil, false
	}
	htop := m.refVal(hth)
	if c, ok := m.faceConst(args[0]); ok && c == CRoleTop {
		return m.Apply(args[3], htop), true // u htop
	}
	if c, ok := m.faceConst(args[1]); ok && c == CRoleTop {
		return m.Apply(args[4], htop), true // v htop
	}
	// Proof-directed dispatch (see tryFsplitIota): a horl/horr validity proof picks
	// its disjunct's branch even on a proper face, so the dependent e-component
	// `fsplitD … (horl … h) ~> u h` reduces inside a system branch.
	if left, hp, ok := m.holdsInjection(args[5]); ok {
		if left {
			return m.Apply(args[3], hp), true // u h
		}
		return m.Apply(args[4], hp), true // v h
	}
	return nil, false
}

// tryGlueTIota fires the T-component projection rule (dual to unglue) on a
// saturated `unglueT A φ T e g h` spine (six arguments, scrutinee g at index 4,
// proof h at index 5):
//
//	unglueT A φ T e (glue A' φ' T' e' t a) h  ~>  t h
//
// On φ the glued type IS its T-component, so unglueT projects it. Stays neutral
// when g is not a glue intro. Used by hcomp-over-Glue to extract glued-fibre
// components of the walls/floor (staying symbolic on neutral walls).
func (m *Machine) tryGlueTIota(args []Val) (Val, bool) {
	if len(args) != 6 || m.Gl == nil {
		return nil, false
	}
	g := m.Force(args[4])
	gn, ok := g.(VNeu)
	if !ok {
		return nil, false
	}
	head, gargs := spineParts(gn.Spine)
	gref, ok := head.(NRef)
	if !ok || m.Gl.GlueRoleOf(gref.Hash) != URoleGlueIn || len(gargs) != 6 {
		return nil, false
	}
	// glue _ _ _ _ t a: the T-component t is at index 4; apply it to the proof h.
	return m.Apply(gargs[4], args[5]), true
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
		// El (Glue A φ T e) ~> El (T htop)  when φ ≡ ⊤ (R-GLUE / A6 type boundary):
		// on a total face the glued type IS its partial T-component. On a proper or
		// neutral φ, Glue is a genuinely new fibrant type and El stays neutral.
		if m.Gl != nil && m.Sy != nil && m.Gl.GlueRoleOf(cref.Hash) == URoleGlue && len(cargs) == 4 {
			if c, ok := m.faceConst(cargs[1]); ok && c == CRoleTop {
				if hth, ok := m.Sy.SysHash(SRoleTop); ok {
					// El (T htop): decode the partial code at the canonical proof.
					return m.Apply(m.refVal(h), m.Apply(cargs[2], m.refVal(hth))), true
				}
			}
		}
		return nil, false
	case FRoleJ:
		// pathJ A x P d y p — six arguments: A, x, P, d, y, p.
		if len(args) != 6 {
			return nil, false
		}
		// Fast path: J on reflexivity is the base datum.
		if fibHeadIs(m, args[5], FRolePrefl, 2) {
			return args[3], true
		}
		// A4 — pathJ on a GENERAL path computes by transport along the line of
		// "contracting singletons" (CCHM):
		//   pathJ A x P d y p
		//     ~> transp (λi. P (p@i) (⟨j⟩ p@(i∧j))) ⊥ d
		// where ⟨j⟩ p@(i∧j) = pabs A (λj. papp A x y p (imin i j)) is the path from
		// x to p@i. At i0 the inner path is pabs A (λ_. x) ≡ preflF A x (constant
		// path-η) so the line there is P x (preflF A x) — the type of d; at i1 it is
		// pabs A (λj. p@j) ≡ p (general path-η) so the line lands at P y p, the
		// result type. Both coherences are the path-η just added to conversion.
		// When p reduces to preflF the line is constant in i and transp collapses to
		// d by regularity — consistent with the fast path above.
		if m.Pa != nil && m.Kn != nil && m.Iv != nil && m.Fc != nil {
			tgH, ok1 := m.Kn.KanHash(KRoleTranspG)
			pappH, ok2 := m.Pa.PathHash(PRoleApp)
			pabsH, ok3 := m.Pa.PathHash(PRoleAbs)
			iminH, ok4 := m.Iv.IntervalHash(IRoleMin)
			fbotH, ok5 := m.Fc.FaceHash(CRoleBot)
			if ok1 && ok2 && ok3 && ok4 && ok5 {
				A, x, P, d, y, p := args[0], args[1], args[2], args[3], args[4], args[5]
				papp := func(k Val) Val {
					return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pappH), A), x), y), p), k)
				}
				line := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
					inner := VLam{Name: "j", Icit: Expl, Body: func(jv Val) Val {
						return papp(m.Apply(m.Apply(m.refVal(iminH), iv), jv)) // p @ (i∧j)
					}}
					path := m.Apply(m.Apply(m.refVal(pabsH), A), inner) // ⟨j⟩ p@(i∧j)
					return m.Apply(m.Apply(P, papp(iv)), path)          // P (p@i) path
				}}
				_ = y
				return m.vTranspG(tgH, line, m.refVal(fbotH), d), true
			}
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
			// A7 — castU along a pabsU-path is transport along its line:
			//   castU A B (pabsU line) x ~> transp line x
			// The derived `ua A B f g s t` (ambient prelude) unfolds to such a
			// pabsU-Glue line, so castU over a univalence path reduces here, through
			// the genuine transp-over-Glue arm — there is no postulated-`ua` fiat.
			if m.Pu != nil && m.Kn != nil {
				if pref, ok := phead.(NRef); ok &&
					m.Pu.PabsURoleOf(pref.Hash) == PURolePabsU && len(pargs) == 1 {
					if tgH, ok := m.Kn.KanHash(KRoleTranspG); ok {
						return m.vTranspG(tgH, pargs[0], m.vFbot(), args[3]), true
					}
				}
			}
			// R-UA Decision 2 — castU IS transp over the decoded pathU line, for ANY
			// path: castU A B p x ~> transp (λi. pappU A B p i) ⊥ x. ureflU and pabsU
			// take the fast paths above; a genuinely NEUTRAL path p still reduces here
			// to a stuck `transpG` over its `pappU` decoding (not a stuck `castU`), so
			// castU is uniformly "transport along the line" — this is what makes
			// `transportF` in the identity family equal to `castU` definitionally.
			if m.Pu != nil && m.PpU != nil && m.Kn != nil {
				pappH, ok1 := m.PpU.PappUHash()
				tgH, ok2 := m.Kn.KanHash(KRoleTranspG)
				if ok1 && ok2 {
					A, B, pth := args[0], args[1], args[2]
					line := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pappH), A), B), pth), iv)
					}}
					return m.vTranspG(tgH, line, m.vFbot(), args[3]), true
				}
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

// intervalEndpoint forces v and reports whether it is the bare endpoint i0 or
// i1 (its interval role), or stuck.
func (m *Machine) intervalEndpoint(v Val) (IntervalRole, bool) {
	f := m.Force(v)
	n, ok := f.(VNeu)
	if !ok {
		return IRoleNone, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok || len(args) != 0 {
		return IRoleNone, false
	}
	if role := m.Iv.IntervalRoleOf(ref.Hash); role == IRoleI0 || role == IRoleI1 {
		return role, true
	}
	return IRoleNone, false
}

// tryIntervalIota fires the De Morgan algebra rules (§F phase 1) on a saturated
// interval connective. The connectives compute when an argument forces to an
// endpoint; a neutral interval term stays stuck (its eventual meaning is a Kan
// operation, phase 3). The endpoint-producing rules (ineg i0 ~> i1) read the
// other endpoint's value back through IntervalHash. Scrutinees are forced —
// logged, as always — so the proof-cache seam is the one ι-reduction uses.
//
//	ineg i0 ~> i1     ineg i1 ~> i0
//	imin i0 _ ~> i0   imin i1 j ~> j   imin _ i0 ~> i0   imin i i1 ~> i
//	imax i0 j ~> j    imax i1 _ ~> i1  imax _ i0 ~> i    imax _ i1 ~> i1
func (m *Machine) tryIntervalIota(role IntervalRole, args []Val) (Val, bool) {
	endpoint := func(r IntervalRole) Val {
		h, ok := m.Iv.IntervalHash(r)
		if !ok {
			return nil
		}
		return m.refVal(h)
	}
	switch role {
	case IRoleNeg:
		if len(args) != 1 {
			return nil, false
		}
		switch e, ok := m.intervalEndpoint(args[0]); {
		case ok && e == IRoleI0:
			return endpoint(IRoleI1), true
		case ok && e == IRoleI1:
			return endpoint(IRoleI0), true
		}
		return nil, false
	case IRoleMin:
		if len(args) != 2 {
			return nil, false
		}
		if e, ok := m.intervalEndpoint(args[0]); ok {
			if e == IRoleI0 {
				return args[0], true // i0 ∧ _ = i0
			}
			return args[1], true // i1 ∧ j = j
		}
		if e, ok := m.intervalEndpoint(args[1]); ok {
			if e == IRoleI0 {
				return args[1], true // i ∧ i0 = i0
			}
			return args[0], true // i ∧ i1 = i
		}
		return nil, false
	case IRoleMax:
		if len(args) != 2 {
			return nil, false
		}
		if e, ok := m.intervalEndpoint(args[0]); ok {
			if e == IRoleI0 {
				return args[1], true // i0 ∨ j = j
			}
			return args[0], true // i1 ∨ _ = i1
		}
		if e, ok := m.intervalEndpoint(args[1]); ok {
			if e == IRoleI0 {
				return args[0], true // i ∨ i0 = i
			}
			return args[1], true // i ∨ i1 = i1
		}
		return nil, false
	}
	return nil, false
}

// tryPathIota fires the cubical path rules (§F phase 2) on a saturated
// application `papp A x y p i` (five arguments):
//
//	papp A _ _ (pabs A' f)    i   ~>  f i     β
//	papp A x x (preflF A' x') i   ~>  x'      reflexivity is the constant path
//	papp A x y  p             i0  ~>  x       boundary at the left endpoint
//	papp A x y  p             i1  ~>  y       boundary at the right endpoint
//
// The intro rules (β, refl) are tried first — they fire for any interval point,
// including a neutral one. The boundary rules fire for ANY path, including a
// neutral variable: a path's value at an endpoint IS that endpoint, definitionally.
func (m *Machine) tryPathIota(args []Val) (Val, bool) {
	if len(args) != 5 {
		return nil, false
	}
	path, pt := args[3], args[4]
	// Intro rules: inspect the path head.
	if pneu, ok := m.Force(path).(VNeu); ok {
		phead, pargs := spineParts(pneu.Spine)
		if pref, ok := phead.(NRef); ok {
			if m.Pa != nil && m.Pa.PathRoleOf(pref.Hash) == PRoleAbs && len(pargs) == 2 {
				return m.Apply(pargs[1], pt), true // β: f i
			}
			if m.Fib != nil && m.Fib.FibRoleOf(pref.Hash) == FRolePrefl && len(pargs) == 2 {
				return pargs[1], true // refl is the constant path at its point
			}
		}
	}
	// Boundary rules: inspect the interval point.
	if m.Iv != nil {
		switch e, ok := m.intervalEndpoint(pt); {
		case ok && e == IRoleI0:
			return args[1], true // left endpoint x
		case ok && e == IRoleI1:
			return args[2], true // right endpoint y
		}
	}
	return nil, false
}

// tryPappUIota fires the type-level path-application rules (§F / R-UA / A7) on a
// saturated `pappU A B p i` (four arguments — A,B at 0/1, the pathU p at 2, the
// interval point i at 3). It is the pathU analogue of tryPathIota, parallel β / refl
// / boundary:
//
//	pappU A B (pabsU line) i   ~>  line i     β
//	pappU A A (ureflU A')  i   ~>  A          reflexivity is the constant type-path
//	pappU A B  p           i0  ~>  A          boundary at the left endpoint
//	pappU A B  p           i1  ~>  B          boundary at the right endpoint
func (m *Machine) tryPappUIota(args []Val) (Val, bool) {
	if len(args) != 4 {
		return nil, false
	}
	path, pt := args[2], args[3]
	// Intro rules: inspect the path head.
	if pneu, ok := m.Force(path).(VNeu); ok {
		phead, pargs := spineParts(pneu.Spine)
		if pref, ok := phead.(NRef); ok {
			if m.Pu != nil && m.Pu.PabsURoleOf(pref.Hash) == PURolePabsU && len(pargs) == 1 {
				return m.Apply(pargs[0], pt), true // β: line i
			}
			if m.Fib != nil && m.Fib.FibRoleOf(pref.Hash) == FRoleUrefl && len(pargs) == 1 {
				return args[0], true // ureflU: the constant type-path is A
			}
		}
	}
	// Boundary rules: inspect the interval point.
	if m.Iv != nil {
		switch e, ok := m.intervalEndpoint(pt); {
		case ok && e == IRoleI0:
			return args[0], true // left endpoint A
		case ok && e == IRoleI1:
			return args[1], true // right endpoint B
		}
	}
	return nil, false
}

// tryPathPIota fires the DEPENDENT path rules (§F / R-HIT / A9) on a saturated
// `pappP A x y p i` (five arguments), generalising tryPathIota to a type-line:
//
//	pappP A x y (pabsP A' f) i   ~>  f i     β
//	pappP A x y  p           i0  ~>  x       boundary at the left endpoint
//	pappP A x y  p           i1  ~>  y       boundary at the right endpoint
//
// There is no refl rule (preflF is non-dependent); the β and boundary rules suffice
// for the dependent HIT eliminators' path methods.
func (m *Machine) tryPathPIota(args []Val) (Val, bool) {
	if len(args) != 5 || m.Pp == nil {
		return nil, false
	}
	path, pt := args[3], args[4]
	// β: applying a dependent abstraction.
	if pneu, ok := m.Force(path).(VNeu); ok {
		phead, pargs := spineParts(pneu.Spine)
		if pref, ok := phead.(NRef); ok &&
			m.Pp.PathPRoleOf(pref.Hash) == PPRoleAbs && len(pargs) == 2 {
			return m.Apply(pargs[1], pt), true // f i
		}
	}
	// Boundary rules: a dependent path's value at an endpoint is that endpoint.
	if m.Iv != nil {
		switch e, ok := m.intervalEndpoint(pt); {
		case ok && e == IRoleI0:
			return args[1], true // left endpoint x
		case ok && e == IRoleI1:
			return args[2], true // right endpoint y
		}
	}
	return nil, false
}

// faceConst forces v and reports whether it is the bare cofibration ⊤ or ⊥
// (ftop/fbot), or stuck.
func (m *Machine) faceConst(v Val) (FaceRole, bool) {
	f := m.Force(v)
	n, ok := f.(VNeu)
	if !ok {
		return CRoleNone, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok || len(args) != 0 {
		return CRoleNone, false
	}
	if role := m.Fc.FaceRoleOf(ref.Hash); role == CRoleTop || role == CRoleBot {
		return role, true
	}
	return CRoleNone, false
}

// tryFaceIota fires the face-lattice rules (§F phase 3a) on a saturated
// cofibration connective. The atomic constraints decide on interval endpoints,
// and the lattice connectives absorb/unit on ⊤/⊥; a neutral face stays stuck.
//
//	ieq0 i0 ~> ⊤   ieq0 i1 ~> ⊥      ieq1 i0 ~> ⊥   ieq1 i1 ~> ⊤
//	fand ⊤ φ ~> φ  fand ⊥ _ ~> ⊥     (symmetric)
//	for  ⊥ φ ~> φ  for  ⊤ _ ~> ⊤     (symmetric)
func (m *Machine) tryFaceIota(role FaceRole, args []Val) (Val, bool) {
	face := func(r FaceRole) Val {
		h, ok := m.Fc.FaceHash(r)
		if !ok {
			return nil
		}
		return m.refVal(h)
	}
	switch role {
	case CRoleEq0, CRoleEq1:
		if len(args) != 1 || m.Iv == nil {
			return nil, false
		}
		e, ok := m.intervalEndpoint(args[0])
		if !ok {
			return nil, false
		}
		// ieq0: i0->⊤, i1->⊥.  ieq1: i0->⊥, i1->⊤.
		hit := IRoleI0
		if role == CRoleEq1 {
			hit = IRoleI1
		}
		if e == hit {
			return face(CRoleTop), true
		}
		return face(CRoleBot), true
	case CRoleAnd:
		if len(args) != 2 {
			return nil, false
		}
		if c, ok := m.faceConst(args[0]); ok {
			if c == CRoleTop {
				return args[1], true // ⊤ ∧ φ = φ
			}
			return args[0], true // ⊥ ∧ _ = ⊥
		}
		if c, ok := m.faceConst(args[1]); ok {
			if c == CRoleTop {
				return args[0], true // φ ∧ ⊤ = φ
			}
			return args[1], true // _ ∧ ⊥ = ⊥
		}
		return nil, false
	case CRoleOr:
		if len(args) != 2 {
			return nil, false
		}
		if c, ok := m.faceConst(args[0]); ok {
			if c == CRoleBot {
				return args[1], true // ⊥ ∨ φ = φ
			}
			return args[0], true // ⊤ ∨ _ = ⊤
		}
		if c, ok := m.faceConst(args[1]); ok {
			if c == CRoleBot {
				return args[0], true // φ ∨ ⊥ = φ
			}
			return args[1], true // _ ∨ ⊤ = ⊤
		}
		return nil, false
	}
	return nil, false
}

// kanFreshSentinel is a reserved hash used as a FRESH interval point when
// transp tests a type-line for constancy. It is never a real content hash (real
// hashes are BLAKE3 digests of terms; this fixed pattern is not), so scanning a
// value for it detects exactly the dependence introduced by the test.
var kanFreshSentinel = Hash{
	0xca, 0xfe, 0xf0, 0x07, 0xca, 0xfe, 0xf0, 0x07, 0xca, 0xfe, 0xf0, 0x07, 0xca, 0xfe, 0xf0, 0x07,
	0xca, 0xfe, 0xf0, 0x07, 0xca, 0xfe, 0xf0, 0x07, 0xca, 0xfe, 0xf0, 0x07, 0xca, 0xfe, 0xf0, 0x07,
}

// clockFreshSentinel is a reserved hash used as a FRESH clock κ when `force` probes a
// clock-abstracted delayed value (tryForceIota). Like kanFreshSentinel it is never a
// real content hash, so scanning the payload for it detects exactly the clock
// dependence force must refuse to discharge.
var clockFreshSentinel = Hash{
	0xc1, 0x0c, 0x4f, 0x00, 0xc1, 0x0c, 0x4f, 0x00, 0xc1, 0x0c, 0x4f, 0x00, 0xc1, 0x0c, 0x4f, 0x00,
	0xc1, 0x0c, 0x4f, 0x00, 0xc1, 0x0c, 0x4f, 0x00, 0xc1, 0x0c, 0x4f, 0x00, 0xc1, 0x0c, 0x4f, 0x00,
}

// lmapConstSentinel is a reserved hash used as a FRESH argument when tryGuardIota
// tests whether the mapped function `g` of an `lmap k A B g la` is CONSTANT (ignores
// its argument). If `g sentinel` does not mention the sentinel, `g` is constant, so
// `lmap g la ~> next (g _)` for ANY `la` (even a neutral) — the ▹κ functor law that a
// constant function maps to the constant delayed value, sound in the topos-of-trees /
// later model at every stage (at stage n+1 `lmap g` is `g_n`, and a constant `g` gives
// `(next x)_{n+1}`; at stage 0 ▹ is terminal so both agree). Never a real content hash.
var lmapConstSentinel = Hash{
	0x1a, 0xc0, 0x57, 0x2e, 0x1a, 0xc0, 0x57, 0x2e, 0x1a, 0xc0, 0x57, 0x2e, 0x1a, 0xc0, 0x57, 0x2e,
	0x1a, 0xc0, 0x57, 0x2e, 0x1a, 0xc0, 0x57, 0x2e, 0x1a, 0xc0, 0x57, 0x2e, 0x1a, 0xc0, 0x57, 0x2e,
}

// gfixRecSentinel is a reserved hash standing for the RECURSIVE occurrence when
// unfoldGfixType probes a gfix/gfixF code body for guardedness (the unfold is sound
// to give the Kan rules only if the recursive occurrence is guarded by `Later`, so the
// Σ Kan descent lands on a `Later`-typed component that never re-unfolds — guaranteeing
// termination). Like the other sentinels it is never a real content hash.
var gfixRecSentinel = Hash{
	0x9f, 0x18, 0x5e, 0xc7, 0x9f, 0x18, 0x5e, 0xc7, 0x9f, 0x18, 0x5e, 0xc7, 0x9f, 0x18, 0x5e, 0xc7,
	0x9f, 0x18, 0x5e, 0xc7, 0x9f, 0x18, 0x5e, 0xc7, 0x9f, 0x18, 0x5e, 0xc7, 0x9f, 0x18, 0x5e, 0xc7,
}

// mentionsRefVal reports whether v contains a neutral headed by the ref h,
// without forcing glued unfoldings (a purely structural occurrence check).
// Closures are entered by applying them to a throwaway variable — the check is
// for the REF h, so the throwaway's identity is irrelevant. Terminates: values
// are finite and unfold thunks are never forced.
func mentionsRefVal(m *Machine, v Val, h Hash) bool {
	switch x := v.(type) {
	case VU, VProp:
		return false
	case VEq:
		return mentionsRefVal(m, x.Ty, h) || mentionsRefVal(m, x.L, h) || mentionsRefVal(m, x.R, h)
	case VRefl:
		return mentionsRefVal(m, x.V, h)
	case VPi:
		return mentionsRefVal(m, x.Dom, h) || mentionsRefVal(m, x.Cod(VVar(0)), h)
	case VLam:
		return mentionsRefVal(m, x.Body(VVar(0)), h)
	case VNeu:
		return mentionsRefNeu(m, x.Spine, h)
	}
	return false
}

func mentionsRefNeu(m *Machine, n Neutral, h Hash) bool {
	switch x := n.(type) {
	case NRef:
		return x.Hash == h
	case NApp:
		return mentionsRefNeu(m, x.Fn, h) || mentionsRefVal(m, x.Arg, h)
	case NCast:
		return mentionsRefVal(m, x.A, h) || mentionsRefVal(m, x.B, h) ||
			mentionsRefVal(m, x.P, h) || mentionsRefVal(m, x.X, h)
	case NSubst:
		return mentionsRefVal(m, x.A, h) || mentionsRefVal(m, x.X, h) || mentionsRefVal(m, x.Y, h) ||
			mentionsRefVal(m, x.Prf, h) || mentionsRefVal(m, x.P, h) || mentionsRefVal(m, x.Px, h)
	}
	// NVar, NMeta: no ref.
	return false
}

// tryTransp fires on `transp A a0` (two args). Plain transport is DEFINITIONALLY
// the generalized transport with the empty constancy cofibration:
// `transp A a0 ≡ transpG A ⊥ a0`. So tryTransp simply rewrites to the transpG
// member and lets its ι-rules (regularity, the φ-identity, the structural piF
// fill) do the work — keeping one code path for both forms and letting a stuck
// `transp` and a stuck `transpG ⊥` share a normal form.
func (m *Machine) tryTransp(args []Val) (Val, bool) {
	if len(args) != 2 || m.Fc == nil || m.Kn == nil {
		return nil, false
	}
	tgH, ok := m.Kn.KanHash(KRoleTranspG)
	if !ok {
		return nil, false
	}
	return m.vTranspG(tgH, args[0], m.vFbot(), args[1]), true
}

// glueFormer probes a value for a saturated Glue former, returning its four args
// [A, φ, T, e] (Glue is a separate builtin group, so fibFormer does not see it).
func (m *Machine) glueFormer(v Val) ([]Val, bool) {
	if m.Gl == nil {
		return nil, false
	}
	n, ok := m.Force(v).(VNeu)
	if !ok {
		return nil, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok || m.Gl.GlueRoleOf(ref.Hash) != URoleGlue || len(args) != 4 {
		return nil, false
	}
	return args, true
}

// suspFormer probes a value for a saturated Susp former, returning its one
// argument [Afam] (the suspension HIT is a separate group; fibFormer does not
// see it).
func (m *Machine) suspFormer(v Val) ([]Val, bool) {
	if m.Su == nil {
		return nil, false
	}
	n, ok := m.Force(v).(VNeu)
	if !ok {
		return nil, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok || m.Su.SuspRoleOf(ref.Hash) != SuRoleSusp || len(args) != 1 {
		return nil, false
	}
	return args, true
}

// quotFormer probes a value for a saturated Quotient former, returning its two
// arguments [Afam, Rfam].
func (m *Machine) quotFormer(v Val) ([]Val, bool) {
	if m.Qh == nil {
		return nil, false
	}
	n, ok := m.Force(v).(VNeu)
	if !ok {
		return nil, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok || m.Qh.QuotHitRoleOf(ref.Hash) != QHRoleQuot || len(args) != 2 {
		return nil, false
	}
	return args, true
}

// extend is the contractibility-based filler (cubicaltt `extend`, the reconciler
// inside compGlue): given a contractibility proof `contr : El (isContr B)` and a
// partial element `u : holds φ -> El B`, it produces an element of `El B` that
// agrees with `u` on φ — by composing from the centre of contraction along the
// contraction path to `u`:
//
//	extend B contr φ u
//	  = hcomp B φ (λj h. papp B center (u h) (contraction (u h)) j) center
//	  where center       = fstF B isContrFam contr        : El B
//	        contraction  = sndF B isContrFam contr        : (x:El B) -> El (pathF B center x)
//	        isContrFam   = λc. piF B (λx. pathF B c x)     (the isContr family)
//
// On φ=⊤ the wall at i1 is `papp (contraction (u htop)) i1 = u htop`, so extend
// reduces to `u` there; off φ it is the contractible fill. This is the second of
// the two helpers (with RestrictIv) the proper-face transp-over-Glue assembly
// consumes; verified standalone, no kernel caller yet. See
// FACE-RESTRICTED-EVAL-DESIGN.md §4 and cubicaltt Eval.hs `extend`.
func (m *Machine) extend(B, contr, phi, u Val) (Val, bool) {
	if m.Fib == nil || m.Si == nil || m.Kn == nil || m.Pa == nil {
		return nil, false
	}
	piFH, ok1 := m.Fib.FibHash(FRolePiF)
	pathFH, ok2 := m.Fib.FibHash(FRolePathF)
	fstFH, ok3 := m.Si.SigmaHash(GRoleFst)
	sndFH, ok4 := m.Si.SigmaHash(GRoleSnd)
	hcompH, ok5 := m.Kn.KanHash(KRoleHcomp)
	pappH, ok6 := m.Pa.PathHash(PRoleApp)
	if !(ok1 && ok2 && ok3 && ok4 && ok5 && ok6) {
		return nil, false
	}
	pathFc := func(ty, x, y Val) Val {
		return m.Apply(m.Apply(m.Apply(m.refVal(pathFH), ty), x), y)
	}
	// isContrFam = λc. piF B (λx. pathF B c x)
	isContrFam := VLam{Name: "c", Icit: Expl, Body: func(c Val) Val {
		fam := VLam{Name: "x", Icit: Expl, Body: func(x Val) Val { return pathFc(B, c, x) }}
		return m.Apply(m.Apply(m.refVal(piFH), B), fam)
	}}
	center := m.Apply(m.Apply(m.Apply(m.refVal(fstFH), B), isContrFam), contr)
	contraction := m.Apply(m.Apply(m.Apply(m.refVal(sndFH), B), isContrFam), contr)
	wall := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
		return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
			uh := m.Apply(u, h)
			pth := m.Apply(contraction, uh) // : pathF B center uh
			// papp B center uh pth j
			return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pappH), B), center), uh), pth), j)
		}}
	}}
	return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), B), phi), wall), center), true
}

// transpGlueIntro fires transp-over-Glue when the input is a `glue` INTRO (so its
// T-component `t` and base `a` are directly available — no value face-restriction
// needed), over a Glue line whose face φ is CONSTANT in the transport direction.
// CCHM compGlue specialized to transport (ws=∅) on a canonical input:
//
//	transp (λi. Glue (A i) φ (T i) (e i)) (glue _ φ _ _ t a)
//	  ~> glue (A i1) φ (T i1) (e i1)
//	          (λh. transp (λi. T i h) (t h))                 -- t1: transport the T-part
//	          (hcomp (A i1) φ                                 -- a1: reconcile the base
//	             (λj h. papp (pathComp_h) j) (transp (λi. A i) a))
//
// where pathComp_h is the naturality path `vi1' ⟷ equivFun (e i1 h) (t1 h)` built
// (cubicaltt `pathComp`) as a comp over the A-line with a (j=1) wall feeding
// `equivFun` of the T-FILL. On φ the base reconciles to `equivFun (e i1) (t1)`
// (glue validity); off φ the result is a genuine glued value. Requires φ constant
// in i and a glue-intro input; otherwise stuck (neutral input / varying φ need
// the face-restricted route — RestrictIv/extend — a later increment).
func (m *Machine) transpGlueIntro(ALine, a0 Val) (Val, bool) {
	if m.Gl == nil || m.Kn == nil || m.Pa == nil || m.Fc == nil || m.Iv == nil {
		return nil, false
	}
	g := m.Force(a0)
	gn, ok := g.(VNeu)
	if !ok {
		return nil, false
	}
	gh, gargs := spineParts(gn.Spine)
	gref, ok := gh.(NRef)
	if !ok || m.Gl.GlueRoleOf(gref.Hash) != URoleGlueIn || len(gargs) != 6 {
		return nil, false
	}
	tComp, aComp := gargs[4], gargs[5] // the intro's T-component and base
	// Glue-line part accessor: part(i, k) = the k-th Glue arg [A,φ,T,e] at point i.
	part := func(iv Val, k int) (Val, bool) {
		ga, ok := m.glueFormer(m.Apply(ALine, iv))
		if !ok {
			return nil, false
		}
		return ga[k], true
	}
	sent := VNeu{Spine: NRef{Hash: kanFreshSentinel}}
	phiS, ok := part(sent, 1)
	if !ok || mentionsRefVal(m, phiS, kanFreshSentinel) {
		return nil, false // φ varies in i: needs the face-restricted route
	}
	transpGH, ok1 := m.Kn.KanHash(KRoleTranspG)
	compH, ok2 := m.Kn.KanHash(KRoleComp)
	hcompH, ok3 := m.Kn.KanHash(KRoleHcomp)
	eq1H, ok6 := m.Fc.FaceHash(CRoleEq1)
	efH, ok7 := m.Gl.EquivFunHash()
	i1h, ok8 := m.Iv.IntervalHash(IRoleI1)
	if !(ok1 && ok2 && ok3 && ok6 && ok7 && ok8) {
		return nil, false
	}
	i1v := m.refVal(i1h)
	phi := phiS
	aLineOnly := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { a, _ := part(iv, 0); return a }}
	Ai1, _ := part(i1v, 0)
	Ti1, _ := part(i1v, 2)
	ei1, _ := part(i1v, 3)
	fbot := m.vFbot()
	// vi1' = transp (λi. A i) a
	vi1p := m.vTranspG(transpGH, aLineOnly, fbot, aComp)
	equivFun := func(X, Y, e, arg Val) Val {
		return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(efH), X), Y), e), arg)
	}
	// t1 h = transp (λi. T i h) (t h); the T-fill at point i is transpFillF.
	t1 := VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
		tLineH := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { t, _ := part(iv, 2); return m.Apply(t, h) }}
		return m.vTranspG(transpGH, tLineH, fbot, m.Apply(tComp, h))
	}}
	body := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
		return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
			// pathComp_h @ j : path vi1' ⟷ equivFun (e i1 h) (t1 h), built as
			// comp (λi. A i) (ieq1 j) (λi _. equivFun (e i h) (Tfill_h i)) a.
			tLineH := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { t, _ := part(iv, 2); return m.Apply(t, h) }}
			wall := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
				return VLam{Name: "hj", Icit: Expl, Body: func(Val) Val {
					Ti, _ := part(iv, 2)
					Ai, _ := part(iv, 0)
					ei, _ := part(iv, 3)
					fillI := m.transpFillF(tLineH, fbot, m.Apply(tComp, h), iv) // T-fill at i
					return equivFun(m.Apply(Ti, h), Ai, m.Apply(ei, h), fillI)
				}}
			}}
			e1j := m.Apply(m.refVal(eq1H), j) // ieq1 j
			// pcj is the naturality path's value at j: comp over the A-line forcing
			// equivFun(e i h)(T-fill i) at j=1 and giving vi1' elsewhere. Fed directly
			// as the reconciling hcomp's j-th wall value (no pabs needed).
			return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(compH), aLineOnly), e1j), wall), aComp)
		}}
	}}
	// a1 = hcomp (A i1) φ (λj h. pathComp_h @ j) vi1'
	a1v := m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), Ai1), phi), body), vi1p)
	glueInH, ok := m.Gl.GlueHash(URoleGlueIn)
	if !ok {
		return nil, false
	}
	// glue (A i1) φ (T i1) (e i1) (λh. t1 h) a1
	res := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
		m.refVal(glueInH), Ai1), phi), Ti1), ei1), t1), a1v)
	return res, true
}

// transpGlueEndpointTotal fires the general transp-over-Glue arm (G1) for a Glue
// line whose BASE is CONSTANT in the transport direction and whose face φ is
// DEFINITE (⊤ or ⊥) at BOTH endpoints while varying in between — the univalence
// (`ua`) shape φ i = (ieq0 i) ∨ (ieq1 i) is the (⊤,⊤) case; the single-atom faces
// ieq0 i / ieq1 i are the (⊤,⊥) / (⊥,⊤) cases. Unlike transpGlueIntro this accepts
// a NEUTRAL input and a VARYING φ. The T-component cannot be transported (T exists
// only where φ holds), so the result is assembled at the two ENDPOINTS:
//
//	transp (λi. Glue B (φ i) (T i) (e i)) g0  ~>  out
//	  b1  = (φ i0 ≡ ⊤) ? unglue g0 (= equivFun (e i0 htop) g0)  :  g0     -- base at i0
//	  out = (φ i1 ≡ ⊤) ? fst (fst (equivProof (T i1 htop) B (e i1 htop) b1))  :  b1
//
// On φ i0 ≡ ⊤ the input is a T-element so unglue pulls it to the base; on ⊥ it IS
// the base. On φ i1 ≡ ⊤ the target is El (T i1 htop), recovered from the
// contractible fibre (CCHM §6.3 / cubicaltt `compGlue`, constant base ⇒ identity
// base transport); on ⊥ the target is the base, so b1 is the result. For the ua
// line (e i1 = idEquiv) the centre is b1 = equivFun (e i0 htop) g0 — transport along
// a univalence path applies the equivalence, matching the retired `ua` reduct. A
// VARYING base, or a face that stays a PROPER cofibration at an endpoint, needs the
// fully-general compGlue with multi-atom face restriction (RestrictIv) — stuck, the
// remaining tail (FACE-RESTRICTED-EVAL-DESIGN.md §7).
func (m *Machine) transpGlueEndpointTotal(ALine, g0 Val) (Val, bool) {
	if m.Gl == nil || m.Iv == nil || m.Sy == nil || m.Fc == nil || m.Si == nil || m.Kn == nil {
		return nil, false
	}
	part := func(iv Val, k int) (Val, bool) {
		ga, ok := m.glueFormer(m.Apply(ALine, iv))
		if !ok || len(ga) != 4 {
			return nil, false
		}
		return ga[k], true
	}
	// The line must be a Glue former at the generic point (it is — the caller probed
	// it). The BASE may now VARY in i: it is transported along its own line below
	// (transpG, the identity when the base is constant, so the constant-base cases are
	// recovered exactly). Only a face that stays PROPER at an endpoint is still stuck.
	sent := VNeu{Spine: NRef{Hash: kanFreshSentinel}}
	if _, ok := part(sent, 0); !ok {
		return nil, false
	}
	i0h, ok0 := m.Iv.IntervalHash(IRoleI0)
	i1h, ok1 := m.Iv.IntervalHash(IRoleI1)
	hth, okh := m.Sy.SysHash(SRoleTop)
	unglueH, oku := m.Gl.GlueHash(URoleUnglue)
	epH, okep := m.Gl.EquivProofHash()
	fstH, okf := m.Si.SigmaHash(GRoleFst)
	transpGH, okt := m.Kn.KanHash(KRoleTranspG)
	if !(ok0 && ok1 && okh && oku && okep && okf && okt) {
		return nil, false
	}
	i0v, i1v, htop := m.refVal(i0h), m.refVal(i1h), m.refVal(hth)
	// Both endpoint faces must be DEFINITE (reduce to ⊤ or ⊥). A face that stays a
	// proper cofibration AT an endpoint needs the fully-general face-restricted route
	// (RestrictIv) — stuck. The four definite combinations are all handled here:
	// (⊤,⊤) the univalence shape, (⊤,⊥)/(⊥,⊤) the single-atom faces, (⊥,⊥) the
	// base-degenerate line.
	phi0, ok := part(i0v, 1)
	if !ok {
		return nil, false
	}
	phi1, ok := part(i1v, 1)
	if !ok {
		return nil, false
	}
	c0, ok := m.faceConst(phi0)
	if !ok {
		return nil, false
	}
	c1, ok := m.faceConst(phi1)
	if !ok {
		return nil, false
	}
	base0, _ := part(i0v, 0)
	// b0 — the BASE part of the input at i0. On φ i0 ≡ ⊤ the input is a T-element, so
	// unglue it to the base (`equivFun (e i0 htop) g0`); on φ i0 ≡ ⊥ the glued type is
	// already the base, so the input IS the base (unglue would stay stuck — don't call it).
	var b0 Val
	if c0 == CRoleTop {
		T0, _ := part(i0v, 2)
		e0, _ := part(i0v, 3)
		b0 = m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(unglueH), base0), phi0), T0), e0), g0)
	} else {
		b0 = g0
	}
	// Transport the base part from i0 to i1 along the BASE line λi. (Glue base of i).
	// When the base is constant this is the identity (regularity); when it varies this
	// is the genuine compGlue base transport.
	baseLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { b, _ := part(iv, 0); return b }}
	b1 := m.vTranspG(transpGH, baseLine, m.vFbot(), b0)
	base1, _ := part(i1v, 0)
	// Result at i1. On φ i1 ≡ ⊥ the target is the base, so the transported base b1 IS
	// the result. On φ i1 ≡ ⊤ the target is El (T i1 htop), recovered from the
	// contractible fibre: fst (fst (equivProof (T i1 htop) (base i1) (e i1 htop) b1)).
	if c1 == CRoleBot {
		return b1, true
	}
	Ti1, _ := part(i1v, 2)
	ei1, _ := part(i1v, 3)
	Ti1htop := m.Apply(Ti1, htop)
	ei1htop := m.Apply(ei1, htop)
	isc := m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(epH), Ti1htop), base1), ei1htop), b1)
	fst := func(p Val) Val { return m.Apply(m.Apply(m.Apply(m.refVal(fstH), Ti1htop), Ti1htop), p) }
	return fst(fst(isc)), true
}

// TranspGlueEndpointTotalForTest exposes the G1 transp-over-Glue arm to tests in
// internal/session (which owns the full evaluator wiring). Not used in production.
func (m *Machine) TranspGlueEndpointTotalForTest(line, g0 Val) (Val, bool) {
	return m.transpGlueEndpointTotal(line, g0)
}

// transpG is generalized transport `transpG A φ a0` (§F phase 3, R-FILL): the
// CCHM primitive carrying a constancy cofibration φ on which the type-line A is
// already constant. It reduces by, in order: (1) φ = ⊤ ⇒ a0 (constant on ⊤);
// (2) A constant in i (the sentinel probe) ⇒ a0 (regularity); (3) structural by
// the head former — the varying-domain piF rule (A1) below; (4) otherwise stuck.
func (m *Machine) transpG(A, phi, a0 Val) (Val, bool) {
	if m.Fib == nil || m.Kn == nil || m.Fc == nil || m.Iv == nil {
		return nil, false
	}
	// (1) φ = ⊤: A is constant everywhere it matters; transport is the identity.
	if c, ok := m.faceConst(phi); ok && c == CRoleTop {
		return a0, true
	}
	// (2) A constant in i (regularity), regardless of φ.
	line := m.Apply(A, VNeu{Spine: NRef{Hash: kanFreshSentinel}})
	if !mentionsRefVal(m, line, kanFreshSentinel) {
		return a0, true
	}
	tgH, okk := m.Kn.KanHash(KRoleTranspG)
	if !okk {
		return nil, false
	}
	// (3b) structural over an inner Sigma line (A5a): a NON-DEPENDENT product
	// transports componentwise. sigmaF lives in its own group, so fibFormer does
	// not see it — probe with sigmaFormer.
	if r, sa, ok := m.sigmaFormer(line); ok && r == GRoleSigma && len(sa) == 2 && m.Si != nil {
		if v, done := m.transpSigma(A, phi, a0, tgH); done {
			return v, true
		}
		return nil, false
	}
	// (3c) structural over a Glue LINE (A6). Glue lives in its own group, so
	// fibFormer does not see it — probe with glueFormer.
	if gargs, ok := m.glueFormer(line); ok {
		// ⊤-degeneracy (READY): if the Glue's φ is constantly ⊤ the glued type IS
		// its T-component (the type boundary), so transport reduces to transport
		// over the T-line λi. (T i) htop.
		if m.Sy != nil {
			if c, ok := m.faceConst(gargs[1]); ok && c == CRoleTop {
				if hth, ok := m.Sy.SysHash(SRoleTop); ok {
					htop := m.refVal(hth)
					tLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						ga, ok := m.glueFormer(m.Apply(A, iv))
						if !ok || len(ga) != 4 {
							return m.Apply(A, iv)
						}
						return m.Apply(ga[2], htop) // (T i) htop
					}}
					return m.vTranspG(tgH, tLine, phi, a0), true
				}
			}
		}
		// transp over a Glue line whose input is a `glue` INTRO and whose φ is
		// constant in i: CCHM compGlue specialized to a canonical input (no value
		// face-restriction needed). See transpGlueIntro.
		if v, done := m.transpGlueIntro(A, a0); done {
			return v, true
		}
		// General arm (G1) for the univalence shape: constant base, φ total at both
		// endpoints (the `ua` line). Accepts a neutral input / varying φ, recovering
		// the T-component at i1 from the contractible fibre. See transpGlueEndpointTotal.
		if v, done := m.transpGlueEndpointTotal(A, a0); done {
			return v, true
		}
		// Remaining proper-face transp-over-Glue (neutral input with a non-endpoint
		// proper φ, or a varying base) needs the fully-general compGlue with multi-atom
		// value face-restriction — FACE-RESTRICTED-EVAL-DESIGN.md §7. Stuck until then.
		return nil, false
	}
	// (3d) varying-parameter transport over a SUSPENSION line (R-HIT / R-FILL).
	// The POINT constructors re-index the parameter (they carry no fiber element):
	//   transp (λi. Susp (Afam i)) north(Afam i0) ~> north (Afam i1)   (south too)
	// The MERIDIAN point transports its argument along the carrier line and rebuilds
	// the meridian — no comp reconciliation, because north/south are parameter-only
	// (their boundary already agrees via the point rule). See RFILL-MERIDIAN-DESIGN.
	//   transp (λi. Susp (Afam i)) (merid (Afam i0) a @ k)
	//     ~> merid (Afam i1) (transp (λi. Afam i) φ a) @ k
	// The formal hcomp CELL is handled by transpHitHcompCell (transp commutes with
	// the cell — its system is transport-dimension-constant), so all three generator
	// shapes (poles, meridian, hcomp cell) transport: the Susp Kan structure is TOTAL.
	if _, ok := m.suspFormer(line); ok {
		i1h, oki1 := m.Iv.IntervalHash(IRoleI1)
		if !oki1 {
			return nil, false
		}
		sa1, oksa1 := m.suspFormer(m.Apply(A, m.refVal(i1h)))
		if !oksa1 {
			return nil, false
		}
		// transp of a formal hcomp CELL commutes with the HIT line (A9 last filler).
		if v, done := m.transpHitHcompCell(A, phi, a0, m.Apply(A, m.refVal(i1h)), tgH); done {
			return v, true
		}
		afam1 := sa1[0] // Afam i1
		if an, ok := m.Force(a0).(VNeu); ok {
			head, aargs := spineParts(an.Spine)
			if ref, ok := head.(NRef); ok {
				// point ctor: re-index.
				role := m.Su.SuspRoleOf(ref.Hash)
				if role == SuRoleNorth || role == SuRoleSouth {
					if ctorH, ok := m.Su.SuspHash(role); ok {
						return m.Apply(m.refVal(ctorH), afam1), true
					}
				}
				// meridian point: papp (Susp(A i0)) north south (merid (A i0) a) k.
				if m.Pa != nil && m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(aargs) == 5 {
					if mn, ok := m.Force(aargs[3]).(VNeu); ok {
						mh, ma := spineParts(mn.Spine)
						if mref, ok := mh.(NRef); ok &&
							m.Su.SuspRoleOf(mref.Hash) == SuRoleMerid && len(ma) == 2 {
							northH, ok1 := m.Su.SuspHash(SuRoleNorth)
							southH, ok2 := m.Su.SuspHash(SuRoleSouth)
							meridH, ok3 := m.Su.SuspHash(SuRoleMerid)
							suspH, ok4 := m.Su.SuspHash(SuRoleSusp)
							pappH, ok5 := m.Pa.PathHash(PRoleApp)
							if ok1 && ok2 && ok3 && ok4 && ok5 {
								carrierLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
									ca, ok := m.suspFormer(m.Apply(A, iv))
									if !ok {
										return m.Apply(A, iv)
									}
									return ca[0] // Afam i
								}}
								aT := m.vTranspG(tgH, carrierLine, phi, ma[1])
								merid1 := m.Apply(m.Apply(m.refVal(meridH), afam1), aT)
								n1 := m.Apply(m.refVal(northH), afam1)
								s1 := m.Apply(m.refVal(southH), afam1)
								suspA1 := m.Apply(m.refVal(suspH), afam1)
								// papp (Susp(A i1)) (north(A i1)) (south(A i1)) merid1 k
								return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
									m.refVal(pappH), suspA1), n1), s1), merid1), aargs[4]), true
							}
						}
					}
				}
			}
		}
		return nil, false
	}
	// (3e) varying-parameter transport over a fibrant-QUOTIENT line (R-HIT /
	// R-FILL). The point ctor transports its carried element along the carrier line
	// and re-indexes; the relation path ctor transports both endpoints' carriers
	// AND the witness along the relation line, then rebuilds qrel — its endpoints
	// reconcile via the qinc point rule (no comp needed). See RFILL-MERIDIAN-DESIGN.
	//   transp (λi. Quotient (A i) (R i)) (qinc (A i0) (R i0) a)
	//     ~> qinc (A i1) (R i1) (transp (λi. A i) φ a)
	//   transp (λi. Quotient (A i) (R i)) (qrel (A i0) (R i0) a b r @ k)
	//     ~> qrel (A i1) (R i1) (transp a) (transp b) (transp_R r) @ k
	// The formal hcomp CELL is handled by transpHitHcompCell (transp commutes with the
	// cell), so the fibrant-quotient Kan structure is TOTAL on all generator shapes.
	if _, ok := m.quotFormer(line); ok {
		i1h, oki1 := m.Iv.IntervalHash(IRoleI1)
		if !oki1 {
			return nil, false
		}
		q1, okq1 := m.quotFormer(m.Apply(A, m.refVal(i1h)))
		if !okq1 {
			return nil, false
		}
		// transp of a formal hcomp CELL commutes with the HIT line (A9 last filler).
		if v, done := m.transpHitHcompCell(A, phi, a0, m.Apply(A, m.refVal(i1h)), tgH); done {
			return v, true
		}
		afam1, rfam1 := q1[0], q1[1] // Afam i1, Rfam i1
		carrierA := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			qa, ok := m.quotFormer(m.Apply(A, iv))
			if !ok {
				return m.Apply(A, iv)
			}
			return qa[0] // Afam i
		}}
		if an, ok := m.Force(a0).(VNeu); ok {
			head, aargs := spineParts(an.Spine)
			if ref, ok := head.(NRef); ok {
				// point ctor: transport carrier + re-index.
				if m.Qh.QuotHitRoleOf(ref.Hash) == QHRoleInc && len(aargs) == 3 {
					if qincH, ok := m.Qh.QuotHitHash(QHRoleInc); ok {
						aT := m.vTranspG(tgH, carrierA, phi, aargs[2])
						return m.Apply(m.Apply(m.Apply(
							m.refVal(qincH), afam1), rfam1), aT), true
					}
				}
				// relation point: papp (Quotient(A i0)(R i0)) (qinc a)(qinc b)
				//                       (qrel (A i0)(R i0) a b r) k.
				if m.Pa != nil && m.Pa.PathRoleOf(ref.Hash) == PRoleApp && len(aargs) == 5 {
					if qn, ok := m.Force(aargs[3]).(VNeu); ok {
						qh, qa := spineParts(qn.Spine)
						if qref, ok := qh.(NRef); ok &&
							m.Qh.QuotHitRoleOf(qref.Hash) == QHRoleRel && len(qa) == 5 {
							qincH, ok1 := m.Qh.QuotHitHash(QHRoleInc)
							qrelH, ok2 := m.Qh.QuotHitHash(QHRoleRel)
							quotH, ok3 := m.Qh.QuotHitHash(QHRoleQuot)
							pappH, ok4 := m.Pa.PathHash(PRoleApp)
							if ok1 && ok2 && ok3 && ok4 {
								a, b, r := qa[2], qa[3], qa[4]
								aT := m.vTranspG(tgH, carrierA, phi, a)
								bT := m.vTranspG(tgH, carrierA, phi, b)
								// relation line λi. Rfam i (ã i) (b̃ i), over the fills.
								rLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
									qai, ok := m.quotFormer(m.Apply(A, iv))
									if !ok {
										return m.Apply(A, iv)
									}
									aFill := m.transpFillF(carrierA, phi, a, iv)
									bFill := m.transpFillF(carrierA, phi, b, iv)
									return m.Apply(m.Apply(qai[1], aFill), bFill) // Rfam i (ã i)(b̃ i)
								}}
								rT := m.vTranspG(tgH, rLine, phi, r)
								qrel1 := m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
									m.refVal(qrelH), afam1), rfam1), aT), bT), rT)
								qiA1 := m.Apply(m.Apply(m.Apply(m.refVal(qincH), afam1), rfam1), aT)
								qiB1 := m.Apply(m.Apply(m.Apply(m.refVal(qincH), afam1), rfam1), bT)
								quotAR1 := m.Apply(m.Apply(m.refVal(quotH), afam1), rfam1)
								return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
									m.refVal(pappH), quotAR1), qiA1), qiB1), qrel1), aargs[4]), true
							}
						}
					}
				}
			}
		}
		return nil, false
	}
	// (3) structural, by the probed head former.
	role, cargs, ok := m.fibFormer(line)
	if !ok {
		return nil, false
	}
	switch role {
	case FRolePiF:
		// A1 — transport over a function-type line (ANY domain, incl. varying):
		//   transpG (λi. piF (Dom i) (Fam i)) φ f
		//     ~> λx. transpG (λi. Fam i (x̄ i)) φ (f (x̄ i0))
		//   where  x̄ j = transpFillB (λj. Dom j) φ x j   (pull x from i1 to j).
		// When Dom is constant in i, x̄ j ~> x (the backward fill of a constant
		// line is the identity), so this collapses to the constant-domain push.
		if len(cargs) != 2 {
			return nil, false
		}
		i0h, ok1 := m.Iv.IntervalHash(IRoleI0)
		if !ok1 {
			return nil, false
		}
		A, phi, f := A, phi, a0
		return VLam{Name: "x", Icit: Expl, Body: func(x Val) Val {
			// the domain line λj. Dom j, by decomposing the piF former at each j.
			domLine := VLam{Name: "j", Icit: Expl, Body: func(jv Val) Val {
				r, ca, ok := m.fibFormer(m.Apply(A, jv))
				if !ok || r != FRolePiF || len(ca) != 2 {
					return m.Apply(A, jv) // unreachable: piF head is i-stable
				}
				return ca[0] // Dom j
			}}
			xbar := func(jv Val) Val { return m.transpFillB(domLine, phi, x, jv) }
			// the codomain line λi. Fam_i (x̄ i).
			codLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
				r, ca, ok := m.fibFormer(m.Apply(A, iv))
				if !ok || r != FRolePiF || len(ca) != 2 {
					return m.Apply(A, iv)
				}
				return m.Apply(ca[1], xbar(iv)) // Fam_i (x̄ i)
			}}
			fArg := m.Apply(f, xbar(m.refVal(i0h))) // f (x̄ i0)
			return m.vTranspG(tgH, codLine, phi, fArg)
		}}, true
	case FRolePathF:
		// A3 — transport over a PATH line. CCHM (φ = ⊥, plain transport):
		//   transp (λi. pathF (A i) (a i) (b i)) p
		//     ~> pabs (A i1) (λj. comp (λi. A i) (for (ieq0 j) (ieq1 j))
		//                            (λi h. fsplit (A i) (ieq0 j) (ieq1 j)
		//                                          (λ_. a i) (λ_. b i) h)
		//                            (papp (A i0) (a i0) (b i0) p j))
		// The endpoint system [(j=0)↦a i, (j=1)↦b i] is a genuine face-dispatching
		// system via fsplit (the disjuncts ieq0 j / ieq1 j are disjoint, so no
		// overlap). At j=i0/i1 the comp face becomes ⊤ and the total rule reads off
		// a i1 / b i1 — so the result inhabits pathF (A i1) (a i1) (b i1) on the
		// nose. The interior (generic j) is a symbolic comp over the carrier line.
		// Only the plain transport (φ ≡ ⊥) is handled here; a proper φ stays stuck
		// (it adds a third, constant-on-φ branch — a later increment).
		if len(cargs) != 3 {
			return nil, false
		}
		if c, ok := m.faceConst(phi); !ok || c != CRoleBot {
			return nil, false
		}
		compH, okc := m.Kn.KanHash(KRoleComp)
		i0h, ok0 := m.Iv.IntervalHash(IRoleI0)
		i1h, ok1 := m.Iv.IntervalHash(IRoleI1)
		var pabsH, pappH, orH, eq0H, eq1H, fsH Hash
		var okp1, okp2, oko, oke0, oke1, okf bool
		if m.Pa != nil {
			pabsH, okp1 = m.Pa.PathHash(PRoleAbs)
			pappH, okp2 = m.Pa.PathHash(PRoleApp)
		}
		oko = false
		if m.Fc != nil {
			orH, oko = m.Fc.FaceHash(CRoleOr)
			eq0H, oke0 = m.Fc.FaceHash(CRoleEq0)
			eq1H, oke1 = m.Fc.FaceHash(CRoleEq1)
		}
		if m.Fs != nil {
			fsH, okf = m.Fs.FsplitHash()
		}
		if !(okc && ok0 && ok1 && okp1 && okp2 && oko && oke0 && oke1 && okf) {
			return nil, false
		}
		// per-i decomposition of the path line into (carrier, a, b).
		part := func(iv Val, idx int) Val {
			r, ca, ok := m.fibFormer(m.Apply(A, iv))
			if !ok || r != FRolePathF || len(ca) != 3 {
				return m.Apply(A, iv)
			}
			return ca[idx]
		}
		carrierLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { return part(iv, 0) }}
		i0v, i1v := m.refVal(i0h), m.refVal(i1h)
		ci0, ai0, bi0 := part(i0v, 0), part(i0v, 1), part(i0v, 2)
		papp := func(carrier, a, b, p, j Val) Val {
			return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pappH), carrier), a), b), p), j)
		}
		body := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
			e0 := m.Apply(m.refVal(eq0H), j)
			e1 := m.Apply(m.refVal(eq1H), j)
			ext := m.Apply(m.Apply(m.refVal(orH), e0), e1) // for (ieq0 j) (ieq1 j)
			sys := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
				return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
					ai := part(iv, 1)
					bi := part(iv, 2)
					ub := VLam{Name: "_", Icit: Expl, Body: func(Val) Val { return ai }}
					vb := VLam{Name: "_", Icit: Expl, Body: func(Val) Val { return bi }}
					// fsplit (A i) (ieq0 j) (ieq1 j) (λ_. a i) (λ_. b i) h
					return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(fsH), part(iv, 0)), e0), e1), ub), vb), h)
				}}
			}}
			floor := papp(ci0, ai0, bi0, a0, j) // papp (A i0) (a i0) (b i0) p j
			// comp (λi. A i) ext sys floor
			return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(compH), carrierLine), ext), sys), floor)
		}}
		return m.Apply(m.Apply(m.refVal(pabsH), part(i1v, 0)), body), true
	}
	return nil, false
}

// transpSigma transports over an inner-Sigma type-line (A5a). It handles the
// NON-DEPENDENT product slice — `λi. sigmaF (A i) (λ_. B i)` where the second
// family ignores its argument — componentwise:
//
//	transpG (λi. sigmaF (A i) (λ_. B i)) φ p
//	  ~> pairF (A i1) (Bfam i1) (transpG (λi. A i) φ p.1) (transpG (λi. B i) φ p.2)
//
// p.1/p.2 are built with fstF/sndF (they compute on a pairF intro, stay neutral
// otherwise). A dependent family is the A5b remainder (needs transpFill of the
// first component into the second's line); it returns done=false → stuck.
func (m *Machine) transpSigma(A, phi, a0 Val, tgH Hash) (Val, bool) {
	decomp := func(iv Val) (Val, Val, bool) {
		r, ca, ok := m.sigmaFormer(m.Apply(A, iv))
		if !ok || r != GRoleSigma || len(ca) != 2 {
			return nil, nil, false
		}
		return ca[0], ca[1], true
	}
	// Probe the family at the sentinel point for NON-DEPENDENCE on its argument.
	_, bfamS, ok := decomp(VNeu{Spine: NRef{Hash: kanFreshSentinel}})
	if !ok {
		return nil, false
	}
	argSent := VNeu{Spine: NRef{Hash: coindXSentinel}}
	dependent := mentionsRefVal(m, m.Apply(bfamS, argSent), coindXSentinel)
	i0h, ok1 := m.Iv.IntervalHash(IRoleI0)
	i1h, ok2 := m.Iv.IntervalHash(IRoleI1)
	fstH, ok3 := m.Si.SigmaHash(GRoleFst)
	sndH, ok4 := m.Si.SigmaHash(GRoleSnd)
	pairH, ok5 := m.Si.SigmaHash(GRolePair)
	if !ok1 || !ok2 || !ok3 || !ok4 || !ok5 {
		return nil, false
	}
	aI0, bI0, ok6 := decomp(m.refVal(i0h))
	aI1, bI1, ok7 := decomp(m.refVal(i1h))
	if !ok6 || !ok7 {
		return nil, false
	}
	aLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
		a, _, _ := decomp(iv)
		return a
	}}
	app := func(f Val, xs ...Val) Val {
		for _, x := range xs {
			f = m.Apply(f, x)
		}
		return f
	}
	p1 := app(m.refVal(fstH), aI0, bI0, a0)
	p2 := app(m.refVal(sndH), aI0, bI0, a0)
	r1 := m.vTranspG(tgH, aLine, phi, p1) // a1 : El (A i1)
	var bLine Val
	if dependent {
		// A5b — the B-fibre line over the FIRST component's forward FILLER:
		//   a*(i) = transpFillF (λi. A i) φ p.1 i      (a* i1 = a1)
		//   b1 = transpG (λi. B i (a* i)) φ p.2  :  El (B i1 a1)
		bLine = VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			_, bf, _ := decomp(iv)
			aStar := m.transpFillF(aLine, phi, p1, iv) // a* i
			return m.Apply(bf, aStar)                  // B i (a* i)
		}}
	} else {
		// A5a — non-dependent: the family ignores its argument.
		dummy := VNeu{Spine: NVar{Lvl: 0}}
		bLine = VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			_, bf, _ := decomp(iv)
			return m.Apply(bf, dummy)
		}}
	}
	r2 := m.vTranspG(tgH, bLine, phi, p2)
	return app(m.refVal(pairH), aI1, bI1, r1, r2), true
}

// transpHitHcompCell fires the transp-of-formal-hcomp-cell rule for a varying HIT
// line (R-HIT / A9 — the last cubical filler the HIT kit left honest-stuck). A
// formal `hcomp` cell at a HIT type is the canonical Kan generator (CHM
// "hcomp-as-constructor"), and `transp` over a HIT line COMMUTES with it: the
// cell's system is indexed by the hcomp's OWN filling dimension, not the transport
// dimension, so every wall slice and the cap transport along the full line
// independently:
//
//	transp (λi. D(A i)) φ (hcomp (D(A i0)) ψ u u0)
//	  ~> hcomp (D(A i1)) ψ (λj h. transp (λi. D(A i)) φ (u j h))
//	                       (transp (λi. D(A i)) φ u0)
//
// Soundness (the reason "transp commutes with hcomp" is exact HERE, though false
// for an i-varying system): the formal cell's system `u : I -> holds ψ -> El (D(A
// i0))` does not mention the transport dimension, so no forward fill of the walls
// is required. Boundary holds (ψ=⊤: both sides reduce to `transp … (u i1 htop)`);
// regularity holds (on φ the line is constant, so D(A i1)=D(A i0) and the walls/cap
// transport by the identity, returning the cell unchanged); type-preservation holds
// (the reduct is a cell at D(A i1) = El (A i1)). The reduct is a canonical hcomp
// cell, which the HIT recursors/inductors already commute with, so closed-term
// canonicity is preserved with NO new elimination branch. i1Type is `D(A i1)` (the
// line applied at i1). Matches the standard cubical transp-on-hcomp reduction.
func (m *Machine) transpHitHcompCell(A, phi, a0, i1Type Val, tgH Hash) (Val, bool) {
	if m.Kn == nil {
		return nil, false
	}
	an, ok := m.Force(a0).(VNeu)
	if !ok {
		return nil, false
	}
	head, hargs := spineParts(an.Spine)
	ref, ok := head.(NRef)
	if !ok || m.Kn.KanRoleOf(ref.Hash) != KRoleHcomp || len(hargs) != 4 {
		return nil, false
	}
	hcompH, ok := m.Kn.KanHash(KRoleHcomp)
	if !ok {
		return nil, false
	}
	psi, u, u0 := hargs[1], hargs[2], hargs[3]
	walls := VLam{Name: "j", Icit: Expl, Body: func(jv Val) Val {
		return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
			return m.vTranspG(tgH, A, phi, m.Apply(m.Apply(u, jv), h))
		}}
	}}
	floor := m.vTranspG(tgH, A, phi, u0)
	return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), i1Type), psi), walls), floor), true
}

// transpFillF forward-fills: transport u0 from i0 up to the interval point s.
//
//	transpFillF A φ u0 s := transpG (λj. A (imin s j)) (for (ieq0 s) φ) u0
//
// At s = i0 the restricted line A(s∧j) is constant (ieq0 i0 ~> ftop), so the
// fill is the identity there; at s = i1 it is the full transport.
func (m *Machine) transpFillF(A, phi, u0, s Val) Val {
	tgH, ok := m.Kn.KanHash(KRoleTranspG)
	if !ok {
		return nil
	}
	restricted := VLam{Name: "j", Icit: Expl, Body: func(jv Val) Val {
		return m.Apply(A, m.vImin(s, jv))
	}}
	cof := m.vFor(m.vIeq0(s), phi)
	return m.vTranspG(tgH, restricted, cof, u0)
}

// transpFillB backward-fills: transport u1 from i1 down to the interval point r.
//
//	transpFillB A φ u1 r := transpG (λj. A (imax r (ineg j))) (for (ieq1 r) φ) u1
//
// At r = i1 the reversed restricted line is constant (ieq1 i1 ~> ftop), so the
// fill is the identity; at r = i0 it is the full backward transport.
func (m *Machine) transpFillB(A, phi, u1, r Val) Val {
	tgH, ok := m.Kn.KanHash(KRoleTranspG)
	if !ok {
		return nil
	}
	reversed := VLam{Name: "j", Icit: Expl, Body: func(jv Val) Val {
		return m.Apply(A, m.vImax(r, m.vIneg(jv)))
	}}
	cof := m.vFor(m.vIeq1(r), phi)
	return m.vTranspG(tgH, reversed, cof, u1)
}

// vTranspG builds (and reduces, via Apply→ι) the spine `transpG A φ a0`.
func (m *Machine) vTranspG(tgH Hash, A, phi, a0 Val) Val {
	return m.Apply(m.Apply(m.Apply(m.refVal(tgH), A), phi), a0)
}

// Interval/face value constructors over the existing members, so the Kan rules
// read like the CCHM formulas. Each is m.Apply over m.refVal(memberHash).
func (m *Machine) vIntervalOp(role IntervalRole, args ...Val) Val {
	h, ok := m.Iv.IntervalHash(role)
	if !ok {
		return nil
	}
	v := m.refVal(h)
	for _, a := range args {
		v = m.Apply(v, a)
	}
	return v
}

func (m *Machine) vImin(a, b Val) Val { return m.vIntervalOp(IRoleMin, a, b) }
func (m *Machine) vImax(a, b Val) Val { return m.vIntervalOp(IRoleMax, a, b) }
func (m *Machine) vIneg(a Val) Val    { return m.vIntervalOp(IRoleNeg, a) }

func (m *Machine) vFaceOp(role FaceRole, args ...Val) Val {
	h, ok := m.Fc.FaceHash(role)
	if !ok {
		return nil
	}
	v := m.refVal(h)
	for _, a := range args {
		v = m.Apply(v, a)
	}
	return v
}

func (m *Machine) vIeq0(a Val) Val   { return m.vFaceOp(CRoleEq0, a) }
func (m *Machine) vIeq1(a Val) Val   { return m.vFaceOp(CRoleEq1, a) }
func (m *Machine) vFor(a, b Val) Val { return m.vFaceOp(CRoleOr, a, b) }
func (m *Machine) vFbot() Val        { return m.vFaceOp(CRoleBot) }

// fibFormer decomposes a fibrant CODE value: it forces v and, when v is a
// saturated fibrant former (piF/pathF/fib/…), returns that former's role and
// argument values. Used by the structural Kan rules to recurse on the head of a
// type-line. Returns ok=false on a neutral that is not a fibrant former head.
func (m *Machine) fibFormer(v Val) (FibRole, []Val, bool) {
	if m.Fib == nil {
		return FRoleNone, nil, false
	}
	f := m.Force(v)
	n, ok := f.(VNeu)
	if !ok {
		return FRoleNone, nil, false
	}
	head, args := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return FRoleNone, nil, false
	}
	role := m.Fib.FibRoleOf(ref.Hash)
	if role == FRoleNone {
		return FRoleNone, nil, false
	}
	return role, args, true
}

// tryHcomp fires the homogeneous-composition rules (§F phase 3d) on
// `hcomp A φ u u0` (four arguments). Phase 3d ships the TOTAL-system rule: when
// φ forces to ⊤ the partial path u is defined everywhere, so the lid is the
// system at i1 — `hcomp A ⊤ u u0 ~> u i1 htop`. A proper face (φ neutral) leaves
// hcomp stuck: the genuine homogeneous filling by recursion on A's former is the
// labelled remainder of the frontier.
func (m *Machine) tryHcomp(args []Val) (Val, bool) {
	if len(args) != 4 || m.Fc == nil || m.Iv == nil || m.Sy == nil {
		return nil, false
	}
	if c, ok := m.faceConst(args[1]); ok {
		switch c {
		case CRoleTop:
			// total system: the lid is the system at i1 — u i1 htop.
			i1h, ok1 := m.Iv.IntervalHash(IRoleI1)
			hth, ok2 := m.Sy.SysHash(SRoleTop)
			if ok1 && ok2 {
				return m.Apply(m.Apply(args[2], m.refVal(i1h)), m.refVal(hth)), true
			}
		case CRoleBot:
			// empty system: no walls, just the floor — hcomp A ⊥ u u0 ~> u0.
			return args[3], true
		}
	}
	// Structural (§F phase 3, the interior): homogeneous composition over a
	// FUNCTION type pushes under the binder, for ANY face:
	//   hcomp (piF P Fam) φ u u0 ~> λx. hcomp (Fam x) φ (λi h. u i h x) (u0 x)
	// Sound and transpFill-free: the base type is fixed (homogeneous), only the
	// codomain family is indexed by x. (The ⊤ rule above has already fired on a
	// total face, so here φ is proper or ⊥.)
	if m.Fib != nil && m.Kn != nil {
		if role, cargs, ok := m.fibFormer(args[0]); ok && role == FRolePiF && len(cargs) == 2 {
			if hcompH, ok1 := m.Kn.KanHash(KRoleHcomp); ok1 {
				phi, u, u0, fam := args[1], args[2], args[3], cargs[1]
				return VLam{Name: "x", Icit: Expl, Body: func(x Val) Val {
					innerU := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
							return m.Apply(m.Apply(m.Apply(u, iv), h), x) // u i h x
						}}
					}}
					return m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(hcompH), m.Apply(fam, x)), phi), innerU), m.Apply(u0, x))
				}}, true
			}
		}
	}
	// Structural over a PATH type (R-BOX, homogeneous, ANY face):
	//   hcomp (pathF A a b) φ u u0
	//     ~> pabs A (λj. hcomp A (for φ (for (ieq0 j) (ieq1 j)))
	//                          (λi h. papp A a b (u i h) j)
	//                          (papp A a b u0 j))
	// The walls u i h are themselves paths a→b, so papp (u i h) i0 ~> a and
	// papp (u i h) i1 ~> b by the path boundary rule. The extended face's endpoint
	// atoms make the inner face ⊤ at j=i0/i1, where the total-system rule fires and
	// reads off a/b through those boundaries — so the result inhabits pathF A a b
	// on the nose for ANY φ, with NO per-face dispatch (overlap-free). The interior
	// (generic j, proper φ) is a symbolic inner hcomp A. See RBOX-DESIGN.md §2.
	if m.Fib != nil && m.Kn != nil && m.Pa != nil && m.Fc != nil {
		if role, cargs, ok := m.fibFormer(args[0]); ok && role == FRolePathF && len(cargs) == 3 {
			pabsH, ok1 := m.Pa.PathHash(PRoleAbs)
			pappH, ok2 := m.Pa.PathHash(PRoleApp)
			hcompH, ok3 := m.Kn.KanHash(KRoleHcomp)
			orH, ok4 := m.Fc.FaceHash(CRoleOr)
			eq0H, ok5 := m.Fc.FaceHash(CRoleEq0)
			eq1H, ok6 := m.Fc.FaceHash(CRoleEq1)
			if ok1 && ok2 && ok3 && ok4 && ok5 && ok6 {
				A, a, b := cargs[0], cargs[1], cargs[2]
				phi, u, u0 := args[1], args[2], args[3]
				papp := func(p, j Val) Val { // papp A a b p j
					return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pappH), A), a), b), p), j)
				}
				body := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
					e0 := m.Apply(m.refVal(eq0H), j)
					e1 := m.Apply(m.refVal(eq1H), j)
					ext := m.Apply(m.Apply(m.refVal(orH), phi),
						m.Apply(m.Apply(m.refVal(orH), e0), e1)) // for φ (for (ieq0 j) (ieq1 j))
					sys := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
							return papp(m.Apply(m.Apply(u, iv), h), j) // papp A a b (u i h) j
						}}
					}}
					return m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(hcompH), A), ext), sys), papp(u0, j))
				}}
				return m.Apply(m.Apply(m.refVal(pabsH), A), body), true
			}
		}
	}
	// Structural over a GLUE type (A6, homogeneous, CCHM compGlue homogeneous
	// case): compose the base through unglue and the glued fibre through unglueT,
	// then re-glue:
	//   hcomp (Glue A φ T e) ψ u u0
	//     ~> glue A φ T e
	//          (λh. hcomp (T h) ψ (λj h'. unglueT A φ T e (u j h') h)
	//                             (unglueT A φ T e u0 h))
	//          (hcomp A ψ (λj h'. unglue A φ T e (u j h')) (unglue A φ T e u0))
	// The type is fixed (homogeneous), so no transpFill; the a-part composes the
	// unglued walls/floor in A, the t-part composes the T-projected walls/floor in
	// each fibre, and `glue` re-assembles — a glue constructor (canonicity). The
	// ⊤/⊥ short-circuits above have already handled total/empty ψ.
	if m.Gl != nil && m.Kn != nil {
		if gargs, ok := m.glueFormer(args[0]); ok && len(gargs) == 4 {
			A, phi, T, e := gargs[0], gargs[1], gargs[2], gargs[3]
			psi, u, u0 := args[1], args[2], args[3]
			hcompH, ok1 := m.Kn.KanHash(KRoleHcomp)
			unglueH, ok2 := m.Gl.GlueHash(URoleUnglue)
			unglueTH, ok3 := m.Gl.GlueHash(URoleUnglueT)
			glueInH, ok4 := m.Gl.GlueHash(URoleGlueIn)
			if ok1 && ok2 && ok3 && ok4 {
				unglue := func(x Val) Val {
					return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(unglueH), A), phi), T), e), x)
				}
				ungT := func(x, h Val) Val {
					return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(unglueTH), A), phi), T), e), x), h)
				}
				// a-part: hcomp A ψ (λj h'. unglue (u j h')) (unglue u0)
				aWall := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
					return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val { return unglue(m.Apply(m.Apply(u, j), h)) }}
				}}
				aPart := m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), A), psi), aWall), unglue(u0))
				// t-part: λh. hcomp (T h) ψ (λj h'. unglueT (u j h') h) (unglueT u0 h)
				tPart := VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
					tWall := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
						return VLam{Name: "h2", Icit: Expl, Body: func(h2 Val) Val { return ungT(m.Apply(m.Apply(u, j), h2), h) }}
					}}
					return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), m.Apply(T, h)), psi), tWall), ungT(u0, h))
				}}
				return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
					m.refVal(glueInH), A), phi), T), e), tPart), aPart), true
			}
		}
	}
	// Structural over an inner-Σ NON-DEPENDENT product (A5a, homogeneous):
	//   hcomp (sigmaF A B) φ u u0
	//     ~> pairF A B (hcomp A φ (λi h. (u i h).1) u0.1)
	//                  (hcomp B φ (λi h. (u i h).2) u0.2)
	// where B is the (argument-ignoring) family. fstF/sndF compute on a pairF.
	if m.Si != nil && m.Kn != nil {
		if role, ca, ok := m.sigmaFormer(args[0]); ok && role == GRoleSigma && len(ca) == 2 {
			if v, done := m.hcompSigma(args, ca[0], ca[1]); done {
				return v, true
			}
		}
	}
	// Structural over a GUARDED-RECURSIVE type (the converse's endpoint-repair): a
	// guarded stream `gStr k A = gfix k F` has a gfix-neutral head, which the formers
	// above do not match — so unfold the type ONE step (`gfix k F ≡ F (gfix k F)`,
	// exposing its `sigmaF`) and recurse, letting the landed Σ Kan rule (A5a/A5b) repair
	// endpoints over the guarded stream. Sound: El(gfix k F) ≡ El(F (gfix k F)). Bounded:
	// the unfolded body's former is sigmaF (not gfix), so this fires at most once.
	if un, ok := m.unfoldGfixType(args[0]); ok {
		return m.tryHcomp([]Val{un, args[1], args[2], args[3]})
	}
	return nil, false
}

// tryUnfoldEta is the final-coalgebra η (coinductive uniqueness), the dual of the
// `out`-on-unfold ι: the anamorphism into the FINAL coalgebra at the carrier `Nu F`
// with the coalgebra `out F` is the identity —
//
//	unfold F (Nu F) (out F) s  ~>  s
//
// Sound: both `id` and `unfold F (Nu F) (out F)` are coalgebra morphisms
// (Nu F, out) -> (Nu F, out), and the anamorphism into a final coalgebra is unique, so
// they coincide. Confluent with the `out`-ι: `out F (unfold F (Nu F) (out F) s)` reduces
// either way to `out F s` (the other path gives `fmapF F (unfold F (Nu F)(out F)) (out F
// s)`, which collapses by this η on the recursive position + Σ-η `pairF (fst p)(snd p) ≡
// p`). Fires only on the exact shape (carrier Nu-headed, coalgebra out-headed); a
// well-typed `unfold` forces the codes to agree, so no conversion re-check is needed.
// This lets streams reconstructed from their observations bottom out — the law the
// global E2 converse's endpoints rely on.
func (m *Machine) tryUnfoldEta(args []Val) (Val, bool) {
	if len(args) != 4 || m.Cn == nil {
		return nil, false
	}
	// carrier S = Nu F'
	sn, ok := m.Force(args[1]).(VNeu)
	if !ok {
		return nil, false
	}
	sHead, _ := spineParts(sn.Spine)
	sRef, ok := sHead.(NRef)
	if !ok || m.Cn.CoindRoleOf(sRef.Hash) != NRoleNu {
		return nil, false
	}
	// coalgebra c = out F'' (a partial application: out-headed neutral with one arg)
	cn, ok := m.Force(args[2]).(VNeu)
	if !ok {
		return nil, false
	}
	cHead, cArgs := spineParts(cn.Spine)
	cRef, ok := cHead.(NRef)
	if !ok || m.Cn.CoindRoleOf(cRef.Hash) != NRoleOut || len(cArgs) != 1 {
		return nil, false
	}
	return args[3], true // s
}

// tryNuConsEta is the ONE-LEVEL coinductive η: reconstructing a coinductive value
// from one layer of its own observation is the identity —
//
//	nuCons F (out F s)  ~>  s
//
// Sound: it is the η-law for the final coalgebra's constructor (the dual of a
// datatype's `subst`/constructor η). Confluent with the β `out (nuCons x) ~> x`
// (both `out (nuCons (out s))` and `nuCons (out (nuCons x))` reduce either way to
// the same normal form). BOUNDED: a SINGLE step, no recursion (unlike the deep
// `unfold`), so conversion terminates — this is what `unfold (out) s ~> s`
// (`tryUnfoldEta`) could not give for a constructor-rebuilt stream. With it the
// coinductive extensionality `consStr (head s)(tail s) ≡ s` is DEFINITIONAL when
// `consStr := nuCons`, which discharges the E2 converse's cons-η.
func (m *Machine) tryNuConsEta(args []Val) (Val, bool) {
	if len(args) != 2 || m.Cn == nil {
		return nil, false
	}
	// arg = out F' s : an out-headed neutral with two args [F', s].
	on, ok := m.Force(args[1]).(VNeu)
	if !ok {
		return nil, false
	}
	oHead, oArgs := spineParts(on.Spine)
	oRef, ok := oHead.(NRef)
	if !ok || m.Cn.CoindRoleOf(oRef.Hash) != NRoleOut || len(oArgs) != 2 {
		return nil, false
	}
	return oArgs[1], true // s
}

// headIsNu reports whether a type value is a coinductive `Nu F` (its head forces to
// the Nu former). Used by the Kan-over-Nu rule to confirm an hcomp's type before
// commuting `out` through it.
func (m *Machine) headIsNu(ty Val) bool {
	if m.Cn == nil {
		return false
	}
	n, ok := m.Force(ty).(VNeu)
	if !ok {
		return false
	}
	head, _ := spineParts(n.Spine)
	ref, ok := head.(NRef)
	return ok && m.Cn.CoindRoleOf(ref.Hash) == NRoleNu
}

// unfoldGfixType unfolds a guarded-recursive TYPE value one step so the Kan operations
// can see the type's structural former (e.g. a `sigmaF`) and fire the structural rules.
// Two shapes are recognised, sharing the same defining equation the conversion stratum
// already uses (convGfix / convGfixF), so the unfold is the SAME sound step, now made
// available to eval's Kan rules:
//
//   - `gfix k F` (2-arg neutral, the head of a guarded stream `gStr k A`) becomes
//     `F (gfix k F)` (`convGfix`). This is the original endpoint-repair enabler.
//   - `gfixF k D Φ d` (4-arg neutral, the INDEXED fixpoint — the `Bisim` former, the
//     head of `gBisim k A d`) becomes `Φ (gfixF k D Φ) d` (`convGfixF`). This is the
//     cubical-route enabler for the E2 converse: `hcomp` over a `gBisim d` relation
//     value unfolds to its `sigmaF` body so the Σ Kan rule (A5a/b) fires componentwise.
//
// TERMINATION GUARD (stronger than the conversion stratum's one-step progress guard):
// the eval-side unfold feeds the structural-Σ Kan rule, which recurses into the body's
// components — so an UNGUARDED recursive occurrence (one not under `Later`) would make
// the Kan rule re-unfold forever. unfoldGfixType therefore fires ONLY when the recursive
// occurrence is GUARDED by `Later` (probed with the gfixRecSentinel). When guarded, the
// Σ descent lands on a `Later`-typed component, which is NOT a gfix/gfixF former, so the
// unfold fires at most once per structural layer and NbE terminates. When the occurrence
// is unguarded (a non-productive code, e.g. `λX. sigmaF Y X`), the step is refused and
// the Kan operation stays soundly STUCK. The real converse codes (`gStr` via `Later k X`,
// `gBisim` via `laterApp ▸ Later`) are guarded, so they unfold and repair. Returns false
// on any non-gfix/gfixF type, or on an unguarded one.
func (m *Machine) unfoldGfixType(ty Val) (Val, bool) {
	if m.Gd == nil {
		return nil, false
	}
	n, ok := m.Force(ty).(VNeu)
	if !ok {
		return nil, false
	}
	head, as := spineParts(n.Spine)
	ref, ok := head.(NRef)
	if !ok {
		return nil, false
	}
	role := m.Gd.GuardRoleOf(ref.Hash)
	switch {
	case role == LRoleGfix && len(as) == 2:
		gfixH, ok := m.Gd.GuardHash(LRoleGfix)
		if !ok {
			return nil, false
		}
		k, F := as[0], as[1]
		// Productivity probe: unfold with the recursive occurrence marked by the
		// sentinel; require it to be guarded by `Later`.
		probe := m.Apply(F, VNeu{Spine: NRef{Hash: gfixRecSentinel}})
		if !m.recOccGuarded(probe, gfixRecSentinel) {
			return nil, false
		}
		gfixKF := m.Apply(m.Apply(m.refVal(gfixH), k), F) // rigid gfix k F
		return m.Apply(F, gfixKF), true                   // F (gfix k F)
	case role == LRoleGfixF && len(as) == 4:
		gfixFH, ok := m.Gd.GuardHash(LRoleGfixF)
		if !ok {
			return nil, false
		}
		k, D, Phi, d := as[0], as[1], as[2], as[3]
		// Productivity probe: replace the recursive family by a sentinel-headed family
		// and unfold once; require the sentinel to occur only under `Later`.
		recSent := VLam{Name: "_d", Icit: Expl, Body: func(Val) Val {
			return VNeu{Spine: NRef{Hash: gfixRecSentinel}}
		}}
		probe := m.Apply(m.Apply(Phi, recSent), d)
		if !m.recOccGuarded(probe, gfixRecSentinel) {
			return nil, false
		}
		gfixKDPhi := m.Apply(m.Apply(m.Apply(m.refVal(gfixFH), k), D), Phi) // rigid gfixF k D Φ
		return m.Apply(m.Apply(Phi, gfixKDPhi), d), true                    // Φ (gfixF k D Φ) d
	}
	return nil, false
}

// recOccGuarded reports whether every occurrence of the sentinel ref `h` in `v` lies
// UNDER a `Later` guard (or there is no occurrence at all) — i.e. the recursive
// occurrence of a gfix/gfixF code is productive. It walks the value structurally; on a
// `Later k X` neutral it stops descending into X (that subterm is guarded). The walk is
// finite (values are finite trees; thunks are never forced). Used by unfoldGfixType to
// fire the eval-side Kan unfold only on productive codes, guaranteeing NbE termination.
func (m *Machine) recOccGuarded(v Val, h Hash) bool {
	switch x := m.Force(v).(type) {
	case VU, VProp:
		return true
	case VEq:
		return m.recOccGuarded(x.Ty, h) && m.recOccGuarded(x.L, h) && m.recOccGuarded(x.R, h)
	case VRefl:
		return m.recOccGuarded(x.V, h)
	case VPi:
		return m.recOccGuarded(x.Dom, h) && m.recOccGuarded(x.Cod(VVar(0)), h)
	case VLam:
		return m.recOccGuarded(x.Body(VVar(0)), h)
	case VNeu:
		return m.recOccGuardedNeu(x.Spine, h)
	}
	return true
}

func (m *Machine) recOccGuardedNeu(n Neutral, h Hash) bool {
	switch x := n.(type) {
	case NRef:
		// A bare occurrence of the sentinel is UNGUARDED (we only reach here if not
		// stopped at a Later above).
		return x.Hash != h
	case NApp:
		// If this spine is `Later k X` (or `laterApp …`), the recursive position is
		// GUARDED — stop descending into the guarded payload. Detect by walking to the
		// head ref and checking its guard role.
		if m.Gd != nil {
			head, _ := spineParts(x)
			if ref, ok := head.(NRef); ok {
				role := m.Gd.GuardRoleOf(ref.Hash)
				if role == LRoleLater || role == LRoleLaterApp {
					return true // guarded: the payload sits under ▹κ
				}
			}
		}
		return m.recOccGuardedNeu(x.Fn, h) && m.recOccGuarded(x.Arg, h)
	case NCast:
		return m.recOccGuarded(x.A, h) && m.recOccGuarded(x.B, h) &&
			m.recOccGuarded(x.P, h) && m.recOccGuarded(x.X, h)
	case NSubst:
		return m.recOccGuarded(x.A, h) && m.recOccGuarded(x.X, h) && m.recOccGuarded(x.Y, h) &&
			m.recOccGuarded(x.Prf, h) && m.recOccGuarded(x.P, h) && m.recOccGuarded(x.Px, h)
	}
	return true
}

// hcompSigma is the homogeneous A5a rule: hcomp over a non-dependent inner-Σ
// product reduces to a pairF of componentwise hcomps. A dependent family is the
// remainder (the second hcomp's type would slide with the first projection) and
// returns done=false → stuck.
func (m *Machine) hcompSigma(args []Val, aCode, bFam Val) (Val, bool) {
	hcompH, ok1 := m.Kn.KanHash(KRoleHcomp)
	fstH, ok2 := m.Si.SigmaHash(GRoleFst)
	sndH, ok3 := m.Si.SigmaHash(GRoleSnd)
	pairH, ok4 := m.Si.SigmaHash(GRolePair)
	if !ok1 || !ok2 || !ok3 || !ok4 {
		return nil, false
	}
	phi, u, u0 := args[1], args[2], args[3]
	app := func(f Val, xs ...Val) Val {
		for _, x := range xs {
			f = m.Apply(f, x)
		}
		return f
	}
	proj := func(projH Val) Val {
		return VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
				return app(projH, aCode, bFam, app(u, iv, h)) // (u i h).k
			}}
		}}
	}
	uFst := proj(m.refVal(fstH))
	u0Fst := app(m.refVal(fstH), aCode, bFam, u0)
	uSnd := proj(m.refVal(sndH))
	u0Snd := app(m.refVal(sndH), aCode, bFam, u0)
	// First component over A (homogeneous, any face).
	a1 := app(m.refVal(hcompH), aCode, phi, uFst, u0Fst)

	argSent := VNeu{Spine: NRef{Hash: coindXSentinel}}
	if !mentionsRefVal(m, m.Apply(bFam, argSent), coindXSentinel) {
		// NON-DEPENDENT family (A5a): the second component is a plain hcomp over B.
		dummy := VNeu{Spine: NVar{Lvl: 0}}
		bCode := m.Apply(bFam, dummy)
		b1 := app(m.refVal(hcompH), bCode, phi, uSnd, u0Snd)
		return app(m.refVal(pairH), aCode, bFam, a1, b1), true
	}
	// DEPENDENT family (A5b): the second component is a comp along the B-fibre
	// line over the FIRST component's filler a* = hfill A φ (fst∘u) (fst u0).
	//   b1 = comp (λj. B (a* j)) φ (snd∘u) (snd u0)   :  El (B a1)   (a* i1 = a1)
	compH, okc := m.Kn.KanHash(KRoleComp)
	if !okc {
		return nil, false
	}
	aStar, ok := m.hfill(aCode, phi, uFst, u0Fst)
	if !ok {
		return nil, false
	}
	bLine := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val { return m.Apply(bFam, m.Apply(aStar, j)) }}
	b1 := app(m.refVal(compH), bLine, phi, uSnd, u0Snd)
	return app(m.refVal(pairH), aCode, bFam, a1, b1), true
}

// hfill is the filler form of hcomp (the i0→r interpolation), built from hcomp +
// fsplit (CCHM / Cubical Agda):
//
//	hfill A φ u u0 r = hcomp A (for φ (ieq0 r))
//	                     (λj h. fsplit A φ (ieq0 r)
//	                              (λhφ. u (imin j r) hφ)   -- the wall, capped at r
//	                              (λh0. u0)                 -- the floor on (r=0)
//	                              h)
//	                     u0
//
// hfill … i0 = u0, hfill … i1 = hcomp A φ u u0, and on φ it tracks the wall. The
// φ ∧ (r=0) overlap is u i0 vs u0, which agree by the standing open-box input
// condition (the floor extends the walls' bottom on φ) — so the fsplit dispatch
// is sound. Returns a `λr. …` Val. See ref_docs/wootz/RBOX-DESIGN.md.
func (m *Machine) hfill(A, phi, u, u0 Val) (Val, bool) {
	if m.Kn == nil || m.Fc == nil || m.Iv == nil || m.Fs == nil {
		return nil, false
	}
	hcompH, ok1 := m.Kn.KanHash(KRoleHcomp)
	orH, ok2 := m.Fc.FaceHash(CRoleOr)
	eq0H, ok3 := m.Fc.FaceHash(CRoleEq0)
	iminH, ok4 := m.Iv.IntervalHash(IRoleMin)
	fsH, ok5 := m.Fs.FsplitHash()
	if !(ok1 && ok2 && ok3 && ok4 && ok5) {
		return nil, false
	}
	return VLam{Name: "r", Icit: Expl, Body: func(r Val) Val {
		e0r := m.Apply(m.refVal(eq0H), r) // ieq0 r
		face := m.Apply(m.Apply(m.refVal(orH), phi), e0r)
		sys := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
			return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
				uPhi := VLam{Name: "hp", Icit: Expl, Body: func(hp Val) Val {
					jr := m.Apply(m.Apply(m.refVal(iminH), j), r) // imin j r
					return m.Apply(m.Apply(u, jr), hp)            // u (imin j r) hp
				}}
				uZero := VLam{Name: "h0", Icit: Expl, Body: func(Val) Val { return u0 }}
				return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
					m.refVal(fsH), A), phi), e0r), uPhi), uZero), h)
			}}
		}}
		return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), A), face), sys), u0)
	}}, true
}

// tryComp fires the heterogeneous-composition rules (§F phase 3e) on
// `comp A φ u u0` (four arguments — A is now a type-LINE, not a single type).
// Phase 3e ships two honest endpoints. The TOTAL-system rule: φ = ⊤ makes the
// partial path total, so the lid is the system at i1 — `comp A ⊤ u u0 ~> u i1
// htop` (independent of A, exactly as hcomp). The DEGENERATE rule: φ = ⊥ leaves
// an empty system, so comp collapses to transp; if additionally the type-line A
// is constant in i (the freshness machinery), transport is the identity and
// `comp A ⊥ u u0 ~> u0`. A proper face (φ neutral) or a genuinely varying line
// under ⊥ leaves comp stuck: the structural box-fill (classically transp ∘
// hcomp, by recursion on A's former) is the labelled remainder of the frontier.
func (m *Machine) tryComp(args []Val) (Val, bool) {
	if len(args) != 4 || m.Fc == nil || m.Iv == nil || m.Sy == nil {
		return nil, false
	}
	if c, ok := m.faceConst(args[1]); ok {
		switch c {
		case CRoleTop:
			// total system: the lid is the system at i1.
			i1h, ok1 := m.Iv.IntervalHash(IRoleI1)
			hth, ok2 := m.Sy.SysHash(SRoleTop)
			if ok1 && ok2 {
				return m.Apply(m.Apply(args[2], m.refVal(i1h)), m.refVal(hth)), true
			}
		case CRoleBot:
			// empty system: comp is transport; a constant type-line transports by
			// the identity (the regularity rule, via the freshness sentinel).
			line := m.Apply(args[0], VNeu{Spine: NRef{Hash: kanFreshSentinel}})
			if !mentionsRefVal(m, line, kanFreshSentinel) {
				return args[3], true
			}
		}
	}
	// Structural (§F phase 3, the interior): composition over a function-type
	// LINE with CONSTANT DOMAIN pushes under the binder, for ANY face — the
	// heterogeneous analogue of the transp/hcomp piF rules:
	//   comp (λi. piF P (Fam i)) φ u u0 ~> λx. comp (λi. Fam i x) φ (λi h. u i h x) (u0 x)
	// Constant domain ⇒ no argument fill (transpFill); a varying domain is the
	// labelled deep remainder.
	if m.Fib != nil && m.Kn != nil {
		sent := VNeu{Spine: NRef{Hash: kanFreshSentinel}}
		role, cargs, ok := m.fibFormer(m.Apply(args[0], sent))
		if ok && role == FRolePiF && len(cargs) == 2 && !mentionsRefVal(m, cargs[0], kanFreshSentinel) {
			if compH, ok1 := m.Kn.KanHash(KRoleComp); ok1 {
				A, phi, u, u0 := args[0], args[1], args[2], args[3]
				return VLam{Name: "x", Icit: Expl, Body: func(x Val) Val {
					codLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						r, ca, ok := m.fibFormer(m.Apply(A, iv))
						if !ok || r != FRolePiF || len(ca) != 2 {
							return m.Apply(A, iv) // unreachable: piF head is i-stable
						}
						return m.Apply(ca[1], x) // Fam_i x : UF
					}}
					innerU := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
						return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
							return m.Apply(m.Apply(m.Apply(u, iv), h), x) // u i h x
						}}
					}}
					return m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(compH), codLine), phi), innerU), m.Apply(u0, x))
				}}, true
			}
		}
	}
	// Structural over a GLUE LINE (A6, heterogeneous) — the standard CCHM seam
	// comp = hcomp(filled walls)(transp floor), reusing the landed transp-over-Glue
	// and hcomp-over-Glue arms (so its correctness is inherited from theirs, no new
	// reconciliation):
	//   comp (λi. Glue_i) ψ u u0
	//     ~> hcomp (Glue_i1) ψ
	//          (λj h. transp (λk. Glue_(imax j k)) (ieq1 j) (u j h))  -- wall j→i1
	//          (transp (λi. Glue_i) ⊥ u0)                              -- floor i0→i1
	if m.Gl != nil && m.Kn != nil && m.Iv != nil && m.Fc != nil {
		sent := VNeu{Spine: NRef{Hash: kanFreshSentinel}}
		if _, ok := m.glueFormer(m.Apply(args[0], sent)); ok {
			transpGH, ok1 := m.Kn.KanHash(KRoleTranspG)
			hcompH, ok2 := m.Kn.KanHash(KRoleHcomp)
			imaxH, ok3 := m.Iv.IntervalHash(IRoleMax)
			eq1H, ok4 := m.Fc.FaceHash(CRoleEq1)
			i1h, ok5 := m.Iv.IntervalHash(IRoleI1)
			if ok1 && ok2 && ok3 && ok4 && ok5 {
				A, psi, u, u0 := args[0], args[1], args[2], args[3]
				glueI1 := m.Apply(A, m.refVal(i1h))
				floor1 := m.vTranspG(transpGH, A, m.vFbot(), u0)
				walls1 := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
					return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
						// line λk. Glue_(imax j k); cofibration (ieq1 j) (constant at j=i1)
						upLine := VLam{Name: "k", Icit: Expl, Body: func(k Val) Val {
							return m.Apply(A, m.Apply(m.Apply(m.refVal(imaxH), j), k))
						}}
						return m.vTranspG(transpGH, upLine, m.Apply(m.refVal(eq1H), j), m.Apply(m.Apply(u, j), h))
					}}
				}}
				return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(hcompH), glueI1), psi), walls1), floor1), true
			}
		}
	}
	// Structural over a PATH LINE (A3, heterogeneous, ANY face) — the CCHM
	// comp-Path formula (the transp-Path rule plus the φ-walls branch):
	//   comp (λi. pathF (A i) (a i) (b i)) φ u u0
	//     ~> pabs (A i1) (λj. comp (λi. A i) (for φ (for (ieq0 j) (ieq1 j)))
	//                            (λi h. fsplit (A i) φ (for (ieq0 j) (ieq1 j))
	//                                     (λhφ. papp (A i) (a i) (b i) (u i hφ) j)
	//                                     (λhψ. fsplit (A i) (ieq0 j) (ieq1 j)
	//                                              (λ_. a i) (λ_. b i) hψ)
	//                                     h)
	//                            (papp (A i0) (a i0) (b i0) u0 j))
	// The three-branch endpoint+walls system dispatches via nested fsplit; the
	// endpoint/φ overlaps agree (papp of a wall at i0/i1 is a/b). Endpoints compute
	// to a i1 / b i1; the interior is the symbolic carrier comp.
	if m.Fib != nil && m.Kn != nil && m.Pa != nil && m.Fc != nil && m.Fs != nil {
		sent := VNeu{Spine: NRef{Hash: kanFreshSentinel}}
		if role, ca, ok := m.fibFormer(m.Apply(args[0], sent)); ok && role == FRolePathF && len(ca) == 3 {
			if v, done := m.compPath(args); done {
				return v, true
			}
		}
	}
	// Structural over an inner-Σ NON-DEPENDENT product LINE (A5a, heterogeneous):
	//   comp (λi. sigmaF (A i) (λ_. B i)) φ u u0
	//     ~> pairF (A i1) (B i1) (comp (λi. A i) φ (λi h. (u i h).1) u0.1)
	//                            (comp (λi. B i) φ (λi h. (u i h).2) u0.2)
	if m.Si != nil && m.Kn != nil {
		sent := VNeu{Spine: NRef{Hash: kanFreshSentinel}}
		if role, sa, ok := m.sigmaFormer(m.Apply(args[0], sent)); ok && role == GRoleSigma && len(sa) == 2 {
			if v, done := m.compSigma(args); done {
				return v, true
			}
		}
	}
	return nil, false
}

// compPath is the heterogeneous A3 rule: comp over a path LINE reduces to a comp
// over the carrier line with a three-branch (φ-walls + two endpoints) system
// built from nested fsplit. See tryComp and ref_docs/wootz/RBOX-DESIGN.md.
func (m *Machine) compPath(args []Val) (Val, bool) {
	A, phi, u, u0 := args[0], args[1], args[2], args[3]
	compH, okc := m.Kn.KanHash(KRoleComp)
	i0h, ok0 := m.Iv.IntervalHash(IRoleI0)
	i1h, ok1 := m.Iv.IntervalHash(IRoleI1)
	pabsH, okp1 := m.Pa.PathHash(PRoleAbs)
	pappH, okp2 := m.Pa.PathHash(PRoleApp)
	orH, oko := m.Fc.FaceHash(CRoleOr)
	eq0H, oke0 := m.Fc.FaceHash(CRoleEq0)
	eq1H, oke1 := m.Fc.FaceHash(CRoleEq1)
	fsH, okf := m.Fs.FsplitHash()
	if !(okc && ok0 && ok1 && okp1 && okp2 && oko && oke0 && oke1 && okf) {
		return nil, false
	}
	part := func(iv Val, idx int) Val {
		r, ca, ok := m.fibFormer(m.Apply(A, iv))
		if !ok || r != FRolePathF || len(ca) != 3 {
			return m.Apply(A, iv)
		}
		return ca[idx]
	}
	carrierLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { return part(iv, 0) }}
	i0v, i1v := m.refVal(i0h), m.refVal(i1h)
	papp := func(carrier, a, b, p, j Val) Val {
		return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(pappH), carrier), a), b), p), j)
	}
	body := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
		e0 := m.Apply(m.refVal(eq0H), j)
		e1 := m.Apply(m.refVal(eq1H), j)
		ends := m.Apply(m.Apply(m.refVal(orH), e0), e1)   // for (ieq0 j) (ieq1 j)
		ext := m.Apply(m.Apply(m.refVal(orH), phi), ends) // for φ (for …)
		sys := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			ci, ai, bi := part(iv, 0), part(iv, 1), part(iv, 2)
			return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
				// φ-branch: λhφ. papp (A i) (a i) (b i) (u i hφ) j
				uPhi := VLam{Name: "hp", Icit: Expl, Body: func(hp Val) Val {
					return papp(ci, ai, bi, m.Apply(m.Apply(u, iv), hp), j)
				}}
				// ψ-branch: λhψ. fsplit (A i) (ieq0 j) (ieq1 j) (λ_. a i) (λ_. b i) hψ
				uPsi := VLam{Name: "hq", Icit: Expl, Body: func(hq Val) Val {
					ub := VLam{Name: "_", Icit: Expl, Body: func(Val) Val { return ai }}
					vb := VLam{Name: "_", Icit: Expl, Body: func(Val) Val { return bi }}
					return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
						m.refVal(fsH), ci), e0), e1), ub), vb), hq)
				}}
				return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
					m.refVal(fsH), ci), phi), ends), uPhi), uPsi), h)
			}}
		}}
		floor := papp(part(i0v, 0), part(i0v, 1), part(i0v, 2), u0, j)
		return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(compH), carrierLine), ext), sys), floor)
	}}
	return m.Apply(m.Apply(m.refVal(pabsH), part(i1v, 0)), body), true
}

// compSigma is the heterogeneous A5a rule: comp over a non-dependent inner-Σ
// product LINE reduces to a pairF of componentwise comps. A dependent family is
// the remainder and returns done=false → stuck.
func (m *Machine) compSigma(args []Val) (Val, bool) {
	A, phi, u, u0 := args[0], args[1], args[2], args[3]
	decomp := func(iv Val) (Val, Val, bool) {
		r, ca, ok := m.sigmaFormer(m.Apply(A, iv))
		if !ok || r != GRoleSigma || len(ca) != 2 {
			return nil, nil, false
		}
		return ca[0], ca[1], true
	}
	_, bfamS, ok := decomp(VNeu{Spine: NRef{Hash: kanFreshSentinel}})
	if !ok {
		return nil, false
	}
	argSent := VNeu{Spine: NRef{Hash: coindXSentinel}}
	dependent := mentionsRefVal(m, m.Apply(bfamS, argSent), coindXSentinel)
	compH, ok1 := m.Kn.KanHash(KRoleComp)
	fstH, ok2 := m.Si.SigmaHash(GRoleFst)
	sndH, ok3 := m.Si.SigmaHash(GRoleSnd)
	pairH, ok4 := m.Si.SigmaHash(GRolePair)
	i0h, ok5 := m.Iv.IntervalHash(IRoleI0)
	i1h, ok6 := m.Iv.IntervalHash(IRoleI1)
	if !ok1 || !ok2 || !ok3 || !ok4 || !ok5 || !ok6 {
		return nil, false
	}
	aI0, bI0, _ := decomp(m.refVal(i0h))
	aI1, bI1, ok7 := decomp(m.refVal(i1h))
	if !ok7 {
		return nil, false
	}
	app := func(f Val, xs ...Val) Val {
		for _, x := range xs {
			f = m.Apply(f, x)
		}
		return f
	}
	aLine := VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { a, _, _ := decomp(iv); return a }}
	projSys := func(projH Val) Val {
		return VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
				return app(projH, aI0, bI0, app(u, iv, h)) // (u i h).k (projection is code-agnostic on a pairF)
			}}
		}}
	}
	uFst := projSys(m.refVal(fstH))
	u0Fst := app(m.refVal(fstH), aI0, bI0, u0)
	uSnd := projSys(m.refVal(sndH))
	u0Snd := app(m.refVal(sndH), aI0, bI0, u0)
	r1 := app(m.refVal(compH), aLine, phi, uFst, u0Fst) // a1 : El (A i1)

	var bLine Val
	if dependent {
		// A5b — the B-fibre line over the FIRST component's comp-FILLER:
		//   a*(i) = compFill (λi. A i) φ (fst∘u) (fst u0) i   (a* i1 = a1)
		//   b1 = comp (λi. B i (a* i)) φ (snd∘u) (snd u0)
		aStar, okf := m.compFill(aLine, phi, uFst, u0Fst)
		if !okf {
			return nil, false
		}
		bLine = VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val {
			_, bf, _ := decomp(iv)
			return m.Apply(bf, m.Apply(aStar, iv)) // B i (a* i)
		}}
	} else {
		dummy := VNeu{Spine: NVar{Lvl: 0}}
		bLine = VLam{Name: "i", Icit: Expl, Body: func(iv Val) Val { _, bf, _ := decomp(iv); return m.Apply(bf, dummy) }}
	}
	r2 := app(m.refVal(compH), bLine, phi, uSnd, u0Snd)
	return app(m.refVal(pairH), aI1, bI1, r1, r2), true
}

// compFill is the heterogeneous filler form of comp (the i0→r interpolation over
// a type-LINE), the comp analogue of hfill (CCHM):
//
//	compFill A φ u u0 r = comp (λj. A (imin r j)) (for φ (ieq0 r))
//	                        (λj h. fsplit (A (imin r j)) φ (ieq0 r)
//	                                 (λhφ. u (imin r j) hφ) (λh0. u0) h)
//	                        u0
//
// compFill … i0 = u0, compFill … i1 = comp A φ u u0. The restricted line A(r∧j)
// is the heterogeneous counterpart of hfill's constant carrier. Returns a `λr. …`
// Val. See hfill and ref_docs/wootz/RBOX-DESIGN.md.
func (m *Machine) compFill(A, phi, u, u0 Val) (Val, bool) {
	if m.Kn == nil || m.Fc == nil || m.Iv == nil || m.Fs == nil {
		return nil, false
	}
	compH, ok1 := m.Kn.KanHash(KRoleComp)
	orH, ok2 := m.Fc.FaceHash(CRoleOr)
	eq0H, ok3 := m.Fc.FaceHash(CRoleEq0)
	iminH, ok4 := m.Iv.IntervalHash(IRoleMin)
	fsH, ok5 := m.Fs.FsplitHash()
	if !(ok1 && ok2 && ok3 && ok4 && ok5) {
		return nil, false
	}
	imin := func(x, y Val) Val { return m.Apply(m.Apply(m.refVal(iminH), x), y) }
	return VLam{Name: "r", Icit: Expl, Body: func(r Val) Val {
		e0r := m.Apply(m.refVal(eq0H), r)
		face := m.Apply(m.Apply(m.refVal(orH), phi), e0r)
		line := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val { return m.Apply(A, imin(r, j)) }}
		sys := VLam{Name: "j", Icit: Expl, Body: func(j Val) Val {
			return VLam{Name: "h", Icit: Expl, Body: func(h Val) Val {
				rj := imin(r, j)
				uPhi := VLam{Name: "hp", Icit: Expl, Body: func(hp Val) Val { return m.Apply(m.Apply(u, rj), hp) }}
				uZero := VLam{Name: "h0", Icit: Expl, Body: func(Val) Val { return u0 }}
				return m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(m.Apply(
					m.refVal(fsH), m.Apply(A, rj)), phi), e0r), uPhi), uZero), h)
			}}
		}}
		return m.Apply(m.Apply(m.Apply(m.Apply(m.refVal(compH), line), face), sys), u0)
	}}, true
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
