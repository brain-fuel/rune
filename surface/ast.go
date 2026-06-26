// Package surface is Rune's named layer: the lexer, parser, named AST, the
// pretty-printer (core -> named surface), and name resolution (named surface ->
// locally-nameless core).
//
// Names live here and only here. Resolution is the only "elaboration" in Phase 0:
// it sends bound identifiers to de Bruijn indices and free identifiers to
// definition references (content hashes). There is no type elaboration in Phase 0.
package surface

import (
	"math/big"

	"goforge.dev/rune/v3/core"
)

// Exp is a named surface expression. Like core.Tm it is a sealed interface matched
// by type switch; the marker keeps the constructor set closed.
type Exp interface {
	isExp()
}

// EVar is an identifier occurrence: either a bound variable or a free reference to a
// top-level definition. Resolution decides which.
type EVar struct {
	Name string
}

// EUniv is a universe: `U` is U_0, `U1`…`U9` the higher levels (Phase 6).
type EUniv struct {
	Lvl int
}

// EHole is `_`: a hole for elaboration to solve with a fresh metavariable
// (Phase 2). Holes never survive elaboration; the untyped resolver rejects them.
type EHole struct{}

// EProp is the universe of propositions, Prop (Phase 3).
type EProp struct{}

// ESig is the dependent-pair type former head, `Sig`, saturated as `Sig A B`
// where B is the family (a function `A -> U`): Σ (x : A), B x.
type ESig struct{}

// EPair is the pair intro head, `pair`, saturated as `pair A B a b` at Σ A B.
type EPair struct{}

// EFst is the first projection of a Σ pair. The operand is embedded (not applied
// via the spine), so `Fst p` / `p.1` is one node and `p.1 x` applies x to the
// projection rather than over-saturating a keyword head.
type EFst struct{ P Exp }

// ESnd is the second projection of a Σ pair; operand embedded like EFst.
type ESnd struct{ P Exp }

// EEq is the equality former head, `Eq`. It is applied like a function and must
// be saturated with three arguments: Eq T l r.
type EEq struct{}

// ERefl is the reflexivity proof head, `refl`. Used bare (in checking position)
// or applied to one argument.
type ERefl struct{}

// ECast is the cast head, `cast`, saturated with four arguments: cast A B p x.
type ECast struct{}

// ESubst is the transport head, `subst`, saturated with six arguments:
// subst A x y p P px — from p : Eq A x y and px : P x, conclude P y.
type ESubst struct{}

// ELam is one parameter of a lambda: fn (Param : Dom) is Body end. The parser
// desugars a curried fn (x : A) (y : B) is e end into nested ELam. Dom is the binder's
// domain annotation; resolution scope-checks it and then discards it, since the
// Phase-0 core lambda is un-annotated (see GRAMMAR.md §6). Dom is never nil.
type ELam struct {
	Param string
	Icit  core.Icit // Impl when written fn {x : A}
	Qty   core.Qty  // 0 or 1 when annotated (fn (0 x : A) …); ω otherwise
	Dom   Exp
	Body  Exp
}

// EApp is application Fn Arg, left-associative.
type EApp struct {
	Fn   Exp
	Arg  Exp
	Icit core.Icit // Impl when written f {e}
}

// EPi is a dependent function type (Param : Dom) -> Cod. A non-dependent arrow
// A -> B parses to EPi with Param "_".
type EPi struct {
	Param string
	Icit  core.Icit // Impl when written {x : A} -> B
	Qty   core.Qty  // 0 or 1 when annotated ((0 x : A) -> B); ω otherwise
	Dom   Exp
	Cod   Exp
}

// ELet is let Name (: Ty)? = Val in Body. Ty is nil when the annotation is absent.
type ELet struct {
	Name string
	Ty   Exp
	Val  Exp
	Body Exp
}

// ESeqBind is a seq-origin binding: `let Name (: Ty)? = Val` inside a seq block.
// It is distinct from ELet so the elaborator can lower it TYPE-AWARELY: if Val's
// type is IO A, the binding becomes a bindIO application (a monadic, ordered
// effect), otherwise it lowers exactly like an ordinary let. The parser stays
// type-agnostic and only tags the node as seq-origin; the IO decision is the
// elaborator's. Ty is nil when the annotation is absent.
type ESeqBind struct {
	Name string
	Ty   Exp
	Val  Exp
	Body Exp
}

// EAnn is a type annotation (Term : Ty).
type EAnn struct {
	Term Exp
	Ty   Exp
}

// CaseClause is one clause of a case expression:
//
//	| ctor b1 b2 with ih1 -> body
//
// Binders match the constructor's argument positions in order; IHs names the
// induction hypotheses the eliminator provides for the recursive arguments
// (in argument order), and may be shorter than the recursive-argument count —
// missing ones are bound fresh and unused.
type CaseClause struct {
	Ctor    string
	Binders []string
	IHs     []string
	Body    Exp
}

// ECase is `case Scrut of (| Clause)+ end` (GRAMMAR §5.6): sugar for one
// saturated application of the scrutinee type's eliminator. It elaborates in
// checking position only — the expected type supplies the motive — and leaves
// no trace in the core.
type ECase struct {
	Scrut   Exp
	Clauses []CaseClause
}

func (EVar) isExp()  {}
func (EHole) isExp() {}
func (EProp) isExp() {}
func (EEq) isExp()   {}
func (ERefl) isExp() {}
func (ECast) isExp() {}
func (EUniv) isExp() {}
func (ELam) isExp()  {}
func (EApp) isExp()  {}
func (EPi) isExp()   {}
func (ELet) isExp()     {}
func (ESeqBind) isExp() {}
func (EAnn) isExp()     {}

// Def is one top-level definition: Name (: Ty)? = Body. The file is a flat list of
// these (no modules in Phase 0). Ty is nil when omitted.
type Def struct {
	Name string
	Ty   Exp
	Body Exp
	// IsInstance marks a typeclass instance (C2): the def is additionally
	// registered in the session instance table and resolved by implicit search.
	IsInstance bool
	// IsPartial marks a general-recursive `partial` definition (C4): its own name
	// is in scope in its body, and its head stays neutral in type-checking.
	IsPartial bool
	// IsForeign marks a `foreign` axiom (R-FFI / B4): a bodiless typed constant
	// whose TYPE is its contract; tracked as an assumption. Body is nil.
	IsForeign bool
}

// Ctor is one constructor of a datatype declaration: Name : Ty.
type Ctor struct {
	Name string
	Ty   Exp
}

// DataDef is a datatype declaration (Phase 4):
//
//	data Name : Ty is C1 : T1 | C2 : T2 | … end
type DataDef struct {
	Name  string
	Ty    Exp
	Ctors []Ctor
}

// BuiltinNat is a builtin-binding declaration (ergonomics rung 2):
//
//	builtin nat Nat zero succ
//
// It registers which data type numeric literals mean: from its position to the
// end of the file, a numeral n parses as the n-fold application of Succ to
// Zero. The declaration is surface state only — it desugars every literal at
// parse time and leaves no trace of its own in the core or the store.
type BuiltinNat struct {
	TyName string
	Zero   string
	Succ   string
}

// BuiltinNatOp is a kernel-acceleration declaration (C7 / R-NUM, Decision 1):
//
//	builtin natAdd add
//	builtin natMul mul
//	builtin natMonus monus
//	builtin natDiv //
//	builtin natMod %
//
// It tags an already-defined binary nat function as a kernel-accelerated
// arithmetic op. From its position the def's content hash is registered with the
// corresponding NatOp in the session acceleration table, so a call on two
// compressed literals (NatLit) short-circuits to one bigint step instead of
// O(a·b) eliminator peeling. The accelerated op IS the user's own def — the
// bridge to its recursive body is refl. Like `builtin nat`, the declaration is
// session state only; nothing enters the store. Kind is one of "natAdd",
// "natMul", "natMonus", "natDiv", "natMod"; DefName is the function accelerated.
type BuiltinNatOp struct {
	Kind    string // "natAdd" | "natMul" | "natMonus" | "natDiv" | "natMod"
	DefName string
}

// BuiltinNumInj is a typed numeral-injection declaration (numeric-tower rung C4):
//
//	builtin int Z intOf
//	builtin rat Rat ratOf
//
// It registers a `Nat -> T` injection so a numeral CHECKED AT T (the integers Z,
// the rationals Rat) lowers to `inj (NatLit n)` — the inner literal stays a real
// Nat (bignum + accel), and the injection carries it into the tower type. The
// base `builtin nat` is unaffected; multiple injections coexist, dispatched by
// the expected type. Negative/fractional literal syntax is NOT added (write
// `zneg`/`/`). Session state only — nothing enters the store. Kind is one of
// "int" | "rat"; TyName is the codomain type (validated against the injection);
// InjName is the injection function.
type BuiltinNumInj struct {
	Kind    string // "int" | "rat"
	TyName  string
	InjName string
}

// ENum is a numeral literal, carried unexpanded (a parser cannot know which
// type it means). Pos is its source offset, for error messages. It is lowered
// by NumConfig: the resolver to the unary default, the elaborator by the
// expected type (see numeral.go). Val is arbitrary-precision: a numeral has no
// size cap (it lowers to one big.Int NatLit node), so the surface must not cap it.
type ENum struct {
	Val *big.Int
	Pos int
}

// DefGroup is a mutually-recursive `partial` group (mutual general recursion):
//
//	partial f : T is e and g : U is d end
//
// Every member is a `partial` def whose name is in scope in EVERY member's body
// (so f may call g and g may call f). A single `partial … end` with no `and`
// parses to a plain Def (IsPartial), not a DefGroup — so existing listings are
// unchanged. The members are stored as one content-addressed SCC group; their
// heads stay neutral (the C4 firewall), so mutual recursion cannot diverge the
// checker.
type DefGroup struct {
	Members []Def // each IsPartial; len >= 2
}

// DataGroup is a mutually-recursive datatype group:
//
//	data D : U is … and E : U is … end
//
// Each member's former is in scope in every member's constructor types (so D's
// constructors may reference E and vice versa). A single `data … end` with no
// `and` parses to a plain DataDef, so existing listings are unchanged.
type DataGroup struct {
	Members []DataDef // len >= 2
}

// Item is one top-level program item: a Def, a DataDef, or a builtin binding.
type Item interface {
	isItem()
}

func (Def) isItem()          {}
func (DataDef) isItem()      {}
func (DefGroup) isItem()      {}
func (DataGroup) isItem()     {}
func (BuiltinNat) isItem()    {}
func (BuiltinNatOp) isItem()  {}
func (BuiltinNumInj) isItem() {}

func (ESubst) isExp() {}
func (ECase) isExp()  {}
func (ENum) isExp()   {}
func (ESig) isExp()   {}
func (EPair) isExp()  {}
func (EFst) isExp()   {}
func (ESnd) isExp()   {}
