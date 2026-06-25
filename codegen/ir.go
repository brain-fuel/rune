package codegen

import "goforge.dev/rune/v3/core"

// The erased intermediate representation: an untyped lambda calculus with
// globals, constructors, and eliminators. Types, proofs, and 0-quantity
// content erase to the unit token — positions are preserved (no arity
// surgery), so an erased argument is simply a unit at the call site and an
// erased binder receives one. THE SHADOW RULE (CLAUDE.md): this tree is
// throwaway codegen shadow; the core it came from is the immutable truth.

// Ir is an erased term. Sealed like core.Tm.
type Ir interface {
	isIr()
}

// IVar is a bound variable (de Bruijn index, as in core).
type IVar struct {
	Idx int
}

// IGlobal is a reference to a top-level definition, constructor, or
// eliminator, by the name the emitted program binds it to.
type IGlobal struct {
	Name string
}

// IUnit is the erased token: every type, proof, and 0-quantity value.
type IUnit struct{}

// LitKind tags an ILit's native payload at the C-ABI boundary (R-FFI / B4).
type LitKind int

const (
	// LitInt is a native machine integer scalar.
	LitInt LitKind = iota
	// LitStr is a native UTF-8 string.
	LitStr
	// LitPtr is an OPAQUE host pointer (R-FFI / B4): a value that crosses the
	// C-ABI boundary AS the host's own handle and whose structure Rune never
	// inspects. Unlike LitInt/LitStr it is not a transparent scalar — `$show`
	// renders it as a fixed opaque marker (`<ptr>`) on every backend rather than
	// printing its payload, and it is boxed in a dedicated runtime variant so it
	// can never alias an int/string payload. It is the marshalling case for a
	// foreign that hands back a host object (a file handle, a NumPy array, a
	// socket) the Rune program only passes along — the un-`show`-able dual of the
	// transparent scalars. The Int field carries an opaque handle token (the
	// host links the real pointer); its VALUE is never observed structurally,
	// only its opaque identity is preserved across the round trip.
	LitPtr
	// LitNat is a COMPRESSED builtin-nat numeral (C7 / R-NUM Decision 4): the O(1)
	// native-integer deployment of a `core.NatLit{N}`. The builtin-nat group is
	// already compiled to the host's native integer by NatSpec (`emitNat*`: JS
	// `0n`/`x+1n`, Py `0`/`x+1`, Go `int`, Rust/JVM `V::Int`/`VInt`, BEAM
	// integers). Before C7 a numeral erased to an O(n) `succ^N zero` IGlobal chain
	// that only evaluated to that native integer at runtime; LitNat emits the
	// native integer DIRECTLY in O(1) source, observationally identical (same
	// `$show`, same behavior under succ/NatElim/accelerated arithmetic) to the
	// chain. The magnitude rides the `Nat` field as a canonical decimal string so
	// arbitrary-precision literals deploy where the backend's integer is
	// arbitrary-precision (JS BigInt, Python, BEAM); machine-int backends
	// (Go/Rust/JVM) keep the same bound the succ-chain-to-machine-int path had.
	// CRITICAL: LitNat must match the NatSpec representation, NOT LitInt — on JS
	// a nat is a BigInt (`5000n`) and `x + 1n` rejects a plain `Number`.
	LitNat
	// LitBytes is a REAL byte string (Phase 0 / Go-stdlib parity): a
	// length-prefixed, immutable sequence of arbitrary bytes — the honest
	// counterpart to the packed-Nat `String` (which packs UTF-8 into a single
	// bignum and so cannot index by byte in O(1) nor carry the full breadth of
	// bytes/strings/encoding/crypto a real stdlib needs). The Str field carries the
	// RAW bytes verbatim (Go `string` is a byte vector and may hold NUL and invalid
	// UTF-8); NO encoding is assumed. The canonical wire form is exactly this byte
	// sequence — bytes are ordered, not numeric, so there is NO endianness hazard
	// (the bignum's little-endian byte order was the old trap, gone here). Each
	// backend renders it to its native immutable byte container (JS Uint8Array — NOT
	// a JS string, whose UTF-16 reindexes astral codepoints; Python `bytes`; Go
	// `[]byte`; Rust `V::Bytes(Vec<u8>)`; BEAM binary; JVM `VBytes(byte[])`; C/LLVM
	// a `K_BYTES` length-prefixed GC object with the payload inline in slots, since
	// the mark-sweep has no per-kind finalizer). The canonical `$show` (the
	// conformance gate) is PURE BYTE-LEVEL — no UTF-8 decoding, so it is trivially
	// identical on all 8: print a double-quoted string where each byte in printable
	// ASCII (0x20..0x7e) other than `"` (0x22) and `\` (0x5c) is emitted literally,
	// and EVERY other byte (control, NUL, and all bytes >= 0x7f — i.e. every
	// multibyte-UTF-8 lead/continuation byte and every invalid byte) is emitted as
	// `\xNN` with two lowercase hex digits. One algorithm mirrored by every emitter.
	// Indexing is BY BYTE at the
	// Bytes level (O(1) `byteAt`, `len` = byte count); rune indexing is a derived
	// O(n) UTF-8 view in the prelude, never the carrier's job.
	LitBytes
)

// ILit is a NATIVE host literal — the marshalling primitive at the C-ABI / FFI
// boundary (R-FFI / B4). The erased IR is otherwise untyped lambda calculus over
// tagged records; a foreign function that produces or consumes a host scalar or
// string needs that value to cross the boundary AS the host's own representation,
// not as a unary Peano record. ILit is that bridge: each backend renders it to its
// native int/string and `$show` prints it natively, so the SAME marshalled value is
// observably equal on every backend (the conformance gate). Int is universal; the
// string variant lands a dedicated value-domain case where a backend's dynamic value
// lacked one (Rust V::Str, JVM VStr, BEAM binaries) so strings never collide with
// int payloads. THE SHADOW RULE holds: ILit lives only in this throwaway IR.
type ILit struct {
	Kind LitKind
	// Int is the payload when Kind == LitInt.
	Int int64
	// Str is the payload when Kind == LitStr, and also the RAW bytes when
	// Kind == LitBytes (a Go string used as an arbitrary byte vector — may hold
	// NUL and non-UTF-8 bytes).
	Str string
	// Nat is the canonical decimal magnitude when Kind == LitNat (a string so an
	// arbitrary-precision numeral survives to the arbitrary-precision backends).
	Nat string
	// Int doubles as the opaque handle token when Kind == LitPtr (never shown
	// structurally — only its opaque identity survives the boundary).
}

// ILam is a one-parameter function.
type ILam struct {
	Name string
	Body Ir
}

// IApp is application.
type IApp struct {
	Fn  Ir
	Arg Ir
}

// ILet is a local binding.
type ILet struct {
	Name string
	Val  Ir
	Body Ir
}

// IPair is a dependent-pair value, erased to a 2-element tuple (R-SUM / C1).
type IPair struct {
	A Ir
	B Ir
}

// IFst / ISnd are the tuple projections.
type IFst struct{ P Ir }
type ISnd struct{ P Ir }

// IForeign is a reference to a host-provided `foreign` axiom (R-FFI / B4). Unlike
// IGlobal, it does NOT resolve to an emitted definition — the host links the
// implementation. Every backend emits it as a uniform zero-argument accessor
// `<name>()` so the FFI ABI is the same symbol on each target (the host defines
// that accessor to return the foreign value).
type IForeign struct {
	Name string
}

// IField projects a constructor record's argument by 0-based index (datatype
// parameters occupy the low indices; a constructor's own arguments follow). It is
// the eliminator's view of its scrutinee — `scrut.args[Index]`.
type IField struct {
	Scrut Ir
	Index int
}

// ICase is the eliminator's tag dispatch: force Scrut to a constructor record and
// take the arm whose Tag matches. Arms reference the scrutinee's fields through
// IField (no field binders), so an arm body shares the enclosing de Bruijn context
// — this is what lets the eliminator be LOWERED ONCE into the IR (LowerElim) rather
// than re-emitted per backend. An unmatched tag is impossible by coverage.
type ICase struct {
	Scrut Ir
	Arms  []ICaseArm
}

// ICaseArm is one constructor branch of an ICase.
type ICaseArm struct {
	Tag  int
	Body Ir
}

// IBounce marks a tail-position recursive call to a partial-group member (the
// trampoline). The backend renders it as a 0-arg thunk (a "bounce") that the
// group's driver loop forces, instead of a direct call — flattening tail and
// mutual-tail recursion onto the heap so deep recursion runs in O(1) host stack
// (the JVM has no TCO). Call is the original SATURATED application spine whose head
// names a member of the current partial recursion group. Semantically identical to
// Call (the driver forces it exactly once before the loop continues); it changes
// only WHEN the frame is created, never the value. Every stack-limited backend now
// renders it as a real bounce (js/py/go/rs/jvm + native c/ll, the T2 driver); BEAM
// has native TCO, so it emits the wrapped Call directly (a byte-identical passthrough).
type IBounce struct{ Call Ir }

func (IVar) isIr()     {}
func (IGlobal) isIr()  {}
func (IUnit) isIr()    {}
func (ILit) isIr()     {}
func (ILam) isIr()     {}
func (IApp) isIr()     {}
func (ILet) isIr()     {}
func (IPair) isIr()    {}
func (IFst) isIr()     {}
func (ISnd) isIr()     {}
func (IForeign) isIr() {}
func (IField) isIr()   {}
func (ICase) isIr()    {}
func (IBounce) isIr()  {}

// CtorSpec is one constructor of an emitted datatype: its bound name, its
// 0-based tag, and the total number of curried parameters it takes (datatype
// parameters + its own arguments; parameters arrive erased).
type CtorSpec struct {
	Name  string
	Tag   int
	Arity int
}

// DataSpec is one datatype: its eliminator's bound name, the number of
// datatype parameters, and its constructors in declaration order. Rec mirrors
// core.CtorSig.Rec for building recursive calls in the eliminator.
type DataSpec struct {
	ElimName  string
	NumParams int
	Ctors     []CtorSpec
	Rec       [][]bool
}

// DefSpec is one emitted definition. Arity is the number of leading curried
// lambdas of Body (0 for a non-function value); the trampoline's saturation gate
// reads it to know when a partial-member call is a tail call vs a partial value.
type DefSpec struct {
	Name  string
	Body  Ir
	Arity int
}

// NatSpec marks a datatype as the `builtin nat` binding (ergonomics rung 6):
// the backend may then compile it to native machine integers — zero/succ/the
// eliminator become arithmetic and a loop — instead of unary records. The
// core stays Peano and provable; only the shadow gets fast.
//
// Ops is the C7 / R-NUM Decision-4 acceleration table threaded from the
// session's `builtin natAdd|natMul|natMonus` registrations (natAccel): it maps
// an emitted def NAME (the registered op's def, e.g. `add`) to its core.NatOp
// kind. A backend emits an application of such a def to two arguments as the
// host's NATIVE arithmetic (`a + b` / `a * b` / saturating monus) on the
// NatSpec representation, O(1), instead of unfolding the def's NatElim loop.
// The accel's soundness contract (registration-time differential gate, see
// internal/session.AddBuiltinNatOp) guarantees the native op AGREES with the
// eliminator-peeled result on the NatSpec representation, so this is a pure
// speedup, observationally identical. Empty/nil means no acceleration.
type NatSpec struct {
	Zero, Succ, ElimName string
	Ops                  map[string]core.NatOp
}

// accelMatch recognizes a saturated 2-argument application of a registered
// accel-op def (C7 / R-NUM Decision 4): `IApp(IApp(IGlobal name, a), b)` where
// `name` is in the accel table. It returns the op kind and the two argument
// terms. A partial application (fewer than 2 args) or a call to an unregistered
// global does NOT match — the backend then falls back to the def's ordinary
// emission (the eliminator loop), so partial application is sound. Over-applied
// spines (3+ args) also fall through: an accel op is `Nat -> Nat -> Nat`, so a
// third argument cannot be a nat arg and is handled by the normal application
// path. Shared across every backend so the native-emit recognition is identical.
func accelMatch(app IApp, accel map[string]core.NatOp) (op core.NatOp, a, b Ir, ok bool) {
	if len(accel) == 0 {
		return core.NatOpNone, nil, nil, false
	}
	// Unwind the application spine into head + args (left-to-right).
	var args []Ir
	t := Ir(app)
	for {
		ap, isApp := t.(IApp)
		if !isApp {
			break
		}
		args = append([]Ir{ap.Arg}, args...)
		t = ap.Fn
	}
	g, isGlobal := t.(IGlobal)
	if !isGlobal || len(args) != 2 {
		return core.NatOpNone, nil, nil, false
	}
	kind, registered := accel[g.Name]
	if !registered || kind == core.NatOpNone {
		return core.NatOpNone, nil, nil, false
	}
	return kind, args[0], args[1], true
}

// Program is a whole erased program in definition order (acyclic, so the
// order is also the emission order).
type Program struct {
	Datas []DataSpec
	Defs  []DefSpec
	// Nat, when non-nil, names the builtin-nat data group for fast-path
	// compilation.
	Nat *NatSpec
	// Main, when non-empty, names the definition whose value the emitted
	// program prints on run.
	Main string
	// IOMain is set when Main has type `IO A` (R-EFFECT / C3): the emitted
	// program RUNS the IO action (forces the world thunk) before showing it,
	// rather than printing the action itself.
	IOMain bool
	// Partials is the set of emitted def names that are `partial` (general
	// recursion). The trampoline pass marks tail-position calls to these as
	// IBounce; the backends wrap each partial def's body in a driver loop. Nil/
	// empty means no trampolining (every def emitted as today).
	Partials map[string]bool
}

// Erase lowers elaborated, meta-free core to the erased IR. names maps a
// definition hash to its emitted global name; typeRefs holds the hashes that
// denote types at runtime (datatype formers), which erase to the unit token.
//
// This is the SYNTACTIC erasure: it sees only the term, so it units the
// boundaries it can recognize structurally (type formers, the Refl proof).
// The session emit path drives elaborate.TypedEraser instead, which has the
// types and so honors proof irrelevance (any Prop-typed subterm, not only a
// literal refl) and 0-quantity argument positions — without that, proofs built
// from ordinary ω helpers leak the deep numerals their endpoints mention into
// the shadow (see ref_docs/rune-verified-implementations.md). Erase remains the
// type-free primitive for contexts without a checker (and its own tests).
func Erase(t core.Tm, names map[core.Hash]string, typeRefs map[core.Hash]bool) Ir {
	switch tm := t.(type) {
	case core.Var:
		return IVar{Idx: tm.Idx}
	case core.Ref:
		if typeRefs[tm.Hash] {
			return IUnit{}
		}
		n, ok := names[tm.Hash]
		if !ok {
			n = "$" + tm.Hash.Short()
		}
		return IGlobal{Name: n}
	case core.Univ, core.Prop, core.Pi, core.Eq, core.Refl:
		// Types and proofs are build-time discipline; the shadow keeps a unit.
		return IUnit{}
	case core.Lam:
		return ILam{Name: tm.Body.Name, Body: Erase(tm.Body.Body, names, typeRefs)}
	case core.App:
		fn := Erase(tm.Fn, names, typeRefs)
		if _, isUnit := fn.(IUnit); isUnit {
			// A unit head is an erased type former (List, Quot, …): the whole
			// application denotes a type and erases with it.
			return IUnit{}
		}
		return IApp{Fn: fn, Arg: Erase(tm.Arg, names, typeRefs)}
	case core.Let:
		return ILet{Name: tm.Body.Name, Val: Erase(tm.Val, names, typeRefs),
			Body: Erase(tm.Body.Body, names, typeRefs)}
	case core.Ann:
		return Erase(tm.Term, names, typeRefs)
	case core.Cast:
		// cast computes on types, which are gone: the shadow is the subject.
		return Erase(tm.X, names, typeRefs)
	case core.Subst:
		// Transport is the identity on its computational payload.
		return Erase(tm.Px, names, typeRefs)
	default:
		panic("codegen.Erase: unexpected core term (metavariable or unknown constructor)")
	}
}
