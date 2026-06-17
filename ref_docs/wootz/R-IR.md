# R-IR — Portable erased IR + runtime ABI

> **DELIVERED (2026-06-16) — B1/B2 done.** One shared erased IR (`codegen/ir.go`) is
> rendered by all 8 backends; a datatype's eliminator is lowered to that IR ONCE via
> `LowerElim` (`codegen/lower.go`, ICase/IField), so no backend carries eliminator-
> specific code (verified: js/py/go/rust/beam/jvm + closure(c/ll) all call it). The
> B2 gate — byte-identical full-corpus output across 8 backends — holds. Sole hand-
> rolled eliminator: the builtin Nat's accel path (justified compressed-`NatLit` ABI
> exception, C7/R-NUM). Track B complete; see PARKING-LOT.md for parked tails.

> Roadmap node B1 (`B1 [I] portable erased IR + runtime ABI ⇐ R-IR`). The hinge of
> Track B: it unblocks B2 (refactor the Backend stratum to the IR, JS as the
> reference), B3 (BEAM to genuine working FIRST), and the B3+ fan-out (Go, JVM,
> WASM, LLVM, Cranelift, Swift). It is also the substrate R-EFFECT's `IForeign`
> node, R-FFI (B4), and R-ERASE2 (B5) extend. M0 (the vertical slice, deployed on
> BEAM) cannot happen without it.

## Problem (what's stuck/absent today, with file:line)

There is already an erased IR and one backend. The IR is **adequate for one
target by accident, not portable by design**, and the Backend interface assumes a
source-string output model that the systems backends (LLVM/Cranelift/WASM-machine)
cannot satisfy. Concretely:

1. **The IR is too small to lower without re-discovering structure.** `codegen/ir.go`
   defines six nodes — `IVar` (de Bruijn), `IGlobal`, `IUnit`, `ILam`, `IApp`,
   `ILet` (`codegen/ir.go:18-55`). Constructors and eliminators are **not IR
   nodes**: they ride in `IGlobal` names plus side tables `DataSpec`/`CtorSpec`
   (`codegen/ir.go:60-101`). So a constructor application is just nested `IApp`
   over an `IGlobal{Name:"cons"}`, and the *only* place that knows `cons` builds a
   tagged record is the JS emitter (`codegen/js.go:54` `emitCtor`). Every new
   backend re-derives "this global is a constructor of tag N, arity K" from the
   `Program.Datas` table by string-matching names. Pattern matching does not exist
   as an IR node at all: the eliminator is emitted as a `switch` *inside the JS
   backend* (`codegen/js.go:84` `emitElim`), with the recursive-IH wiring
   (`codegen/js.go:108` `d.Rec[i][j]`) hand-written in JavaScript. A BEAM backend
   would have to re-implement that fold-with-IH logic in Erlang from the same side
   table. **The shape that should be lowered once is lowered per-backend.**

2. **The "calling convention" is implicit and JS-specific.** Everything is
   one-argument curried arrows (`codegen/js.go:11` doc, `:55` the `a%d =>` loop).
   That is a *fine source-IR convention* (it mirrors the core: `core.VLam`/`core.VPi`
   are single-binder closures, `core/val.go:63,74`), but it is also the *only*
   convention — there is no notion of a known-arity call, no eval/apply boundary,
   no distinction between a saturated call and a partial application. On BEAM that
   is catastrophic: Erlang funs are arity-fixed and there is no implicit currying,
   so naive one-arg arrows become a tower of `fun(X) -> ... end` allocations and
   every application is an indirect `apply/2`. The push/enter vs eval/apply
   decision (Marlow & Peyton Jones, *How to make a fast curry*) has to be made in
   the IR, once, not improvised by each backend.

3. **Data representation is hard-coded three times, inconsistently.** A constructor
   is `{tag, name, args:[...]}` (`codegen/js.go:63`); the unit token is JS `null`
   (`codegen/js.go:25` `const $unit = null`); `builtin nat` is BigInt
   (`codegen/js.go:70` `emitNat`); quotients compile to their carrier with a
   bespoke `quotRuntime` string blob (`codegen/js.go:297`). Each of these is a
   *representation decision* that every backend must agree on to be conformance-
   equal, yet none is written down in a backend-neutral place. `$show`
   (`codegen/js.go:332`) bakes the representation into a printer. On BEAM `null`
   is wrong (it should be a fixed atom), tagged records should be tuples
   `{Tag, A1, ..}`, and BigInt is the native integer.

4. **The Backend interface only models "emit a source string."**
   `codegen/codegen.go:19` `Backend.Emit(p Program) (TargetSource, error)` returns
   `TargetSource string`. JS, Erlang, Go, Swift source all fit. **WASM (machine),
   LLVM bitcode, and Cranelift do not** — they emit modules/objects, need a target
   triple, a runtime library to link, and a build step. There is no place for a
   per-backend runtime shim, a foreign-op table, or a link manifest. The interface
   is the wrong shape for half the eight targets.

5. **The inner-taint deploy ban is enforced by name-walking the IR.**
   `internal/session/session.go:621` `innerTaint` walks the erased IR looking for
   `IGlobal` names in a hard-coded set (`transp`, `hcomp`, `ua`, `i0`, …,
   `session.go:622-627`). This is the only thing standing between "checks" and
   "deploys" for the cubical layer. It is brittle (string set, easy to drift from
   the builtin groups) and it lives in `session`, not `codegen`. A portable IR
   must carry this provenance structurally so B5 (R-ERASE2) can later *lower* these
   ops instead of banning them.

6. **`builtin nat`/`bin` fast paths are JS-only and ad hoc.** `NatSpec`
   (`codegen/ir.go:86`) and the `natDispatch` peephole (`codegen/js.go:170`) are
   real optimizations (Peano → machine int; case-on-nat → branch) but they are
   expressed as a JS-specific side channel plus a JS-AST pattern match. A portable
   IR needs to express "this datatype has a machine-integer representation" once,
   so BEAM/LLVM get it for free.

The net: today's IR is a *thin JS-shaped lambda calculus with side tables*. R-IR's
job is to make it a **backend-neutral core IR + a written-down ABI + a shim
boundary**, so B3 (BEAM) is the first *port*, not a second implementation of
codegen.

## Prior art (what the literature/other systems do; cite)

- **Idris2's tiered backend IR** is the closest precedent and the right shape to
  steal. A custom backend transforms one of `NamedCExp` / `Lifted` / `CExp` /
  `ANF` / `VMCode` — progressively lower forms — into the target. The backend
  author chooses the tier; the cookbook enumerates exactly the obligations a
  backend has: *represent tagged data and (possibly partial) function application,
  handle `let`, implement and invoke primitive operations, handle Erased
  arguments, and do runtime crashes*
  ([Idris2 backend cookbook](https://idris2.readthedocs.io/en/latest/backends/backend-cookbook.html)).
  Constructors carry a **tag and arity**; matching is on the tag (`ConCase`),
  constants on the value (`ConstCase`); erased arguments are an explicit IR notion,
  not "a unit by convention." This is the contract R-IR should make explicit.
- **Idris2-Erlang (chrrasmussen)** is a *working* dependently-typed-language → BEAM
  backend and the direct B3 reference. It lowers the Idris ANF/Lifted IR to Core
  Erlang, represents ADTs as tagged tuples, and bridges Idris's curried functions
  to Erlang's fixed-arity funs with explicit adapters (`MkFunN` families that wrap
  a curried value as an N-ary Erlang fun)
  ([Idris2-Erlang IR notes](https://github.com/chrrasmussen/Idris2-Erlang/blob/main/docs/intermediaterepresentations.md)).
  The lesson: **keep a curried source IR, but carry arity so the backend can build
  known-arity wrappers** — exactly the eval/apply story below.
- **GHC's STG + eval/apply** is the calling-convention canon. *Making a fast
  curry: push/enter vs eval/apply for higher-order languages* (Marlow & Peyton
  Jones, JFP 2006) shows eval/apply wins for typed compilation: the caller inspects
  the closure's arity and either does a saturated call, builds a PAP (partial
  application), or over-applies and re-enters. STG closures are `{ code, free vars
  }`; constructors are heap objects with an info table carrying the tag.
  ([Making a fast curry, JFP](https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/);
  push/enter↔eval/apply transformation: [arXiv:1606.06380](https://arxiv.org/pdf/1606.06380)).
- **Core Erlang as a backend target** is plain text, a tiny language with the full
  power of Erlang, the standard compile target for non-Erlang BEAM languages
  ([The Core of Erlang, 8th Light](https://8thlight.com/insights/the-core-of-erlang)).
  Its formal semantics is settled (*A Frame Stack Semantics for Sequential Core
  Erlang*, [arXiv:2308.12403](https://arxiv.org/pdf/2308.12403)), which matters for
  the M7 distributed-algebra track that will eventually want to *prove* against the
  BEAM operational model. ADTs → tagged tuples, ANF before emission, and a curried
  arity-1 adapter for partial/over-application is the established recipe
  (Yhc/Erlang, [HaskellWiki](https://wiki.haskell.org/Yhc/Erlang/Proof_of_concept)).
- **Gleam** ships its *own* type-safe OTP on BEAM and compiles the *same* program
  to Erlang or JS ([Gleam FAQ](https://gleam.run/frequently-asked-questions/)) —
  evidence that one IR → {BEAM, JS} with shared data representation is practical,
  and that OTP belongs in a library over the runtime, not in the IR (this is the
  D5/R-OTP seam, kept out of R-IR).
- **WebAssembly component model / WASI** and **LLVM** establish the *other* shape:
  some backends emit modules/objects, not source, and need a target triple + a
  linked runtime. The Backend interface must accommodate both "emit source" and
  "emit module + link a runtime shim."

## Chosen approach for THIS substrate (concrete; respects containment)

**Thesis: grow the *shadow* IR, never the core.** The Shadow Rule (CLAUDE.md
standing rule 4) already licenses this: the IR is throwaway codegen output built
from erased core. R-IR adds nodes and an ABI to the *shadow*; `core/` and `store/`
do not change, there is no hash-format bump (CLAUDE.md: a bump is only for a new
*core* constructor), and the equality/cubical machinery stays behind its
interface. The work is entirely in `codegen/` + the emit driver in
`internal/session/`.

Four pieces: (A) a portable core IR; (B) a written calling convention; (C) a
written data ABI with a per-backend representation table; (D) a reshaped Backend
boundary with a runtime shim and a foreign-op/inner-op table.

### (A) Portable core IR — make the implicit structure explicit

Keep the existing six nodes (they are the right untyped-λ core) and **promote the
side tables into IR nodes** so every backend lowers the *same* tree:

- `ICon{Tag int; Name string; Args []Ir}` — a **saturated** constructor
  application. Erasure builds this directly (it knows arity from `CtorSig`), so the
  backend never reconstructs "is this global a ctor." Partial constructor
  application erases to an `ILam` over an `ICon` (η-expand to saturation at erase
  time — constructors have known arity, so this is always possible and removes a
  whole class of backend special-casing).
- `ICase{Scrut Ir; Alts []IAlt; Default Ir}` with
  `IAlt{Tag int; Binds []string; Body Ir}` — pattern match on a constructor tag,
  binding the constructor's (non-erased) fields. The eliminator's ι-rule — fold
  with induction hypotheses — is lowered to `ICase` **plus** an explicit
  self-recursive `IFix`/named def, *once*, in a shared `codegen` lowering pass, not
  per backend. (See "eliminator lowering" below.)
- `ILit{Kind LitKind; Val any}` — a primitive literal (machine integer for
  `builtin nat`/`bin`, later string/float). Subsumes the `NatSpec` BigInt channel:
  `zero`→`ILit{Int,0}`, `succ n`→`IPrim{Add1,[n]}`, the eliminator→`IPrim{NatFold}`
  / a loop, chosen by a portable peephole, not a JS-AST match.
- `IPrim{Op PrimOp; Args []Ir}` — a built-in primitive operation (arithmetic on
  the machine-int representation; the case-on-nat dispatch). Backend-neutral; each
  backend maps `PrimOp` to its instruction/BIF.
- `IForeign{Op string; Args []Ir}` — a foreign/runtime call (the R-EFFECT node,
  promoted here so it is part of the portable core from day one). This is the seam
  IO, FFI (B4), and the inner-op lowering (B5) all flow through.
- `IInner{Op InnerOp; Args []Ir}` — a **typed marker for an inner-cubical
  operation** (transp/hcomp/comp/ua/papp/…). Today the backend's contract is "if
  you see one, refuse" (the deploy ban). This replaces the brittle name-set walk
  (`session.go:621`) with a structural node carrying which op it is. B5 (R-ERASE2)
  later teaches backends to *lower* `IInner` (transport → identity-ish runtime
  meaning); until then `Backend.Emit` returns an error on any `IInner`, and the
  taint check is `any IInner reachable` — exact, not string-matched.

`IUnit` stays as the single erased token (types, proofs, 0-quantity args), exactly
as the TypedEraser produces it (`elaborate/erase.go:64,80,113`).

**Why promote rather than keep side tables?** Containment + teachability. With
`ICon`/`ICase`/`IPrim`/`IForeign`/`IInner` the IR is *self-describing*: a backend
is a total function on a sealed sum (the same `isIr()` discipline as `core.Tm`,
CLAUDE.md architecture), and a reviewer can read one tree and know what every
target must do. The eight backends become structurally identical recursions; only
the leaf emission differs.

### Eliminator lowering (the one shared pass)

The keystone simplification. Today each backend re-implements the fold-with-IH
(`codegen/js.go:84-122`). Instead, lower each generated eliminator `DElim` **once**
in `codegen` to a named, self-recursive IR definition built from `ICase`:

```
DElim params motive c_0 .. c_n  =  fix elim. λx.
  case x of
    Ctor_i b_0 .. b_k -> c_i b_0 .. b_k  (elim <rec-arg>)*    -- IH only where Rec[i][j]
```

`Rec[][]` (`codegen/ir.go:74`, already computed) drives where the recursive call
is inserted, here, in the shared pass. Backends then only need: build a closure,
build a constructor, match a tag, recurse. BEAM gets the eliminator as an ordinary
Core Erlang recursive function for free; LLVM gets a loop after the standard
tail/self-call recognition. This is the single biggest portability win and it
deletes `emitElim` from every backend.

### (B) Calling convention — eval/apply, arity-annotated

Adopt **eval/apply** (Marlow & Peyton Jones): the source IR stays curried
(matching the core's single-binder closures, so erasure is unchanged), but every
function value and every call site carries **known arity** so backends can do
saturated calls and build PAPs explicitly.

- Annotate top-level defs and `ILam` chains with arity (a derived attribute, not a
  new representational choice): `n` nested `ILam`s = a function of arity `n`.
- The ABI specifies three closure-calling cases a backend MUST implement, in terms
  the backend can dispatch locally:
  - **saturated** (`#args == arity`): direct call.
  - **partial** (`#args < arity`): allocate a PAP (a closure capturing the
    supplied args).
  - **over-applied** (`#args > arity`): saturated call, then apply the rest to the
    result.
- **Source-string backends** (JS, Erlang, Go, Swift) may *implement* eval/apply
  trivially by keeping curried arity-1 functions (JS already does;
  BEAM uses the Yhc/Erlang arity-1 `_c` adapter for partial/over-application and
  a fixed-arity fun for the common saturated case). The ABI does not force a heap
  layout on them; it forces the *observable* currying semantics.
- **Machine backends** (LLVM/Cranelift/WASM-machine) implement it as STG-style
  `{code, freevars}` closures + an `apply` runtime routine. This is the shim's
  job (D), not the IR's.

The IR therefore commits to *curried semantics with arity metadata*; the heap
realization is a per-backend shim concern. That is the containment line: the IR is
the contract, the shim is the realization.

### (C) Data ABI — one written representation, a per-backend table

Write the canonical runtime representation down (a doc table + a Go `Repr`
descriptor the shim consumes), so conformance-equality is a spec, not an accident:

| IR notion        | Canonical meaning            | JS                       | BEAM                          | Machine (LLVM/…)              |
|------------------|------------------------------|--------------------------|-------------------------------|------------------------------|
| `IUnit`          | the erased token             | `null` (`$unit`)         | atom `unit`                   | a fixed boxed singleton / i0 |
| `ICon{tag,args}` | tagged constructor           | `{tag,name,args:[..]}`   | tuple `{Tag, A1, .., Ak}`     | boxed `{header(tag,arity),fields}` |
| `ILit Int`       | machine integer (`builtin nat/bin`) | BigInt            | integer (bignum-native)       | i64 / GMP for big            |
| closure          | curried fn + arity           | arity-1 arrows           | fun + `_c` adapter            | `{code,env}` + apply         |
| `IForeign`       | runtime/foreign call         | host fn in shim          | BIF / module fn               | C-ABI call into runtime      |
| `IInner`         | inner-cubical op             | **error (deploy ban)**   | **error**                     | **error** (until B5)         |

Key fixes vs today: the unit token becomes a **named atom on BEAM** (not `null`),
constructor records become **tuples on BEAM**, and the representation is *declared*
so `$show`-style printers (`codegen/js.go:332`) are derived from the same `Repr`,
guaranteeing the conformance corpus (M4 gate) sees equal observable results.

`builtin nat`/`bin` fold into `ILit`/`IPrim`: the `NatSpec` channel
(`codegen/ir.go:86`) is replaced by a `Repr` flag "this datatype is the machine-int
type," set once, honored by every backend.

### (D) Reshaped Backend boundary + runtime shim

Generalize `codegen.Backend` so it spans source-emitters and module-emitters and
carries a runtime shim + foreign-op registry:

```go
type Backend interface {
    Target() string
    // Emit lowers an IR program to one or more artifacts (source files,
    // modules, a link manifest). Replaces (TargetSource, error).
    Emit(p Program, opts Options) (Artifacts, error)
    // Runtime returns the backend's runtime shim: the prelude that realizes the
    // ABI (closure/apply, ctor/case, the unit token, $show) + its foreign-op
    // implementations. For source backends this is a source blob; for machine
    // backends, a library to link.
    Runtime() Shim
    // Foreign reports which IForeign ops this backend implements (the FFI/IO
    // surface). An unimplemented op used by the program is a clean error.
    Foreign() map[string]ForeignSig
}
```

- `Artifacts` = a set of named blobs + an optional `Manifest` (entry point, link
  inputs, target triple). JS returns one `.js`; BEAM returns one or more
  `.core`/`.erl` modules + an app entry; LLVM returns bitcode + a link line.
- `Shim` is where today's `quotRuntime`/`showRuntime`/`natDispatchRuntime` string
  blobs (`codegen/js.go:137,297,332`) move — out of the emitter logic, into a
  declared, per-backend runtime that *realizes the ABI*. The quotient runtime
  becomes part of the BEAM/JS shim (quotient → carrier, the v2 erasure), included
  on demand exactly like `usesQuot` does now (`codegen/js.go:305`).
- The **inner-taint ban moves into `codegen`**: `Emit` errors on any reachable
  `IInner` and reports its `InnerOp`. `internal/session/session.go:621` `innerTaint`
  is deleted; `EmitProgram`/`EmitExpr` (`session.go:428,454`) just propagate the
  backend's error. The brittle string set is gone.

**Containment audit.** Nothing here touches `core/`, `store/`, `equality/`, or the
hash preimage. The cubical builtin groups are unchanged; `IInner` is produced by
*erasing* their value members (the eraser already special-cases them via taint),
just structurally now. The IR remains the disposable shadow. This is the same
move v2/v3 made (a contained group + a runtime), applied to codegen itself.

## Interfaces & signatures to add (Go + Rune surface as relevant)

Go, in `codegen/`:

```go
// ir.go — new sealed IR nodes (added to the isIr() family)
type ICon     struct { Tag int; Name string; Args []Ir }      // saturated ctor
type ICase    struct { Scrut Ir; Alts []IAlt; Default Ir }
type IAlt     struct { Tag int; Binds []string; Body Ir }
type ILit     struct { Kind LitKind; Val any }                // Int (BigInt), later Str/Float
type IPrim    struct { Op PrimOp; Args []Ir }                 // machine-int ops, nat dispatch
type IForeign struct { Op string; Args []Ir }                 // runtime/foreign call (R-EFFECT)
type IInner   struct { Op InnerOp; Args []Ir }                // cubical op marker (deploy ban / B5)
func (ICon) isIr(); func (ICase) isIr(); /* ...all... */

type LitKind int    // LInt, (LStr, LFloat later)
type PrimOp  int    // PNatSucc, PNatPred, PNatFold, PNatCase, PIntAdd, ...
type InnerOp int    // ITransp, IHcomp, IComp, IUa, ICastU, IPapp, IPabs, ...

// codegen.go — reshaped Backend boundary
type Options   struct { TargetTriple string; /* link flags, opt level */ }
type Artifacts struct { Files map[string][]byte; Manifest *Manifest }
type Manifest  struct { Entry string; Link []string; Triple string }
type Shim      struct { Prelude []byte; Foreign map[string][]byte } // realizes the ABI
type ForeignSig struct { Arity int /* later: Rune contract for B4 */ }

type Backend interface {
    Target() string
    Emit(p Program, opts Options) (Artifacts, error)
    Runtime() Shim
    Foreign() map[string]ForeignSig
}

// One shared lowering pass (deletes per-backend emitElim):
func LowerElim(d DataSpec) DefSpec   // DElim -> fix/ICase named def, IH-wired via d.Rec
func SaturateCtors(p Program) Program // η-expand partial ctor apps to ICon
```

Erasure changes (`elaborate/erase.go`): emit `ICon` for saturated constructor
applications (it has the `CtorSig` arity), `IInner` for cubical value members
(replacing the taint), and `ILit`/`IPrim` for the `builtin nat`/`bin` group.
`Program.Nat`/`NatSpec` (`codegen/ir.go:86,97`) retire into a `Repr` flag.

Driver changes (`internal/session/session.go`): `emitDefs` builds `ICon`-aware IR;
`innerTaint` (`session.go:621`) is **removed**; `EmitProgram`/`EmitExpr` surface
the backend's `IInner` error. No Rune *surface* change — R-IR is entirely below the
language.

## Worked micro-example (the teachable artifact)

The Savage artifact: **the same `length` program (already the codegen test,
`codegen/codegen_test.go:73`) compiled to JS and to BEAM, observably equal.**

Source (unchanged):

```
data List : U -> U is nil : (A:U) -> List A | cons : (A:U) -> A -> List A -> List A end
length : (A:U) -> List A -> Nat is fn (A:U)(xs:List A) is
  ListElim A (fn _ is Nat end) zero (fn _ _ ih is succ ih end) xs end end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
```

Portable IR (post-erasure, `A` erased to `IUnit`, ctors saturated):

```
two = length unit (ICon cons [unit, ILit 0, ICon cons [unit, ILit 1, ICon nil [unit]]])
length = λa. λxs. ListElim     -- LowerElim expands ListElim to:
ListElim = fix e. λp.λm.λc0.λc1.λx. case x of
  nil  _        -> c0
  cons _ hd tl  -> c1 unit hd (e p m c0 c1 tl)   -- IH because Rec[cons][2]
```

BEAM emission (Core Erlang sketch), driven by the **same** IR + the BEAM `Repr`:

```erlang
'two'/0 = fun () -> 'length'('unit', {'cons','unit',0,{'cons','unit',1,{'nil','unit'}}}) end
'ListElim'/6 = fun (P,M,C0,C1,X) ->
  case X of
    {'nil', _}      -> C0
    {'cons',_,H,T}  -> apply C1 ['unit',H, 'ListElim'(P,M,C0,C1,T)]   %% _c adapter for currying
  end end
```

JS emission (the existing shape, now derived from the JS `Repr`): tuples→`{tag,..}`,
`unit`→`null`, `ILit 0`→`0n`. **Both print `2`** (`succ (succ zero)` folds to the
machine-int `2` via the `builtin nat` `Repr`). One IR, two backends, equal result —
the M4 conformance gate in miniature, and the M0 slice's BEAM half.

The teaching point a newcomer holds: *the dependent types and proofs are gone
(units), the program is plain tagged data and curried functions, and the same tree
runs everywhere.*

## Risks / open sub-questions

- **Eval/apply realization on machine backends is research, not build.** The
  IR/ABI (curried + arity) is ready-to-build; the LLVM/Cranelift/WASM-machine
  *closure heap layout + apply routine + GC interaction* is genuinely open and
  belongs to the B3+ fan-out, not B1. **Label: B1 ships the IR + ABI spec + JS &
  BEAM shims (ready-to-build); the machine-shim heap design is research deferred to
  B3+.**
- **BEAM currying cost.** Arity-1 adapters allocate; the Yhc/Erlang `_c` recipe
  works but a naive translation is slow. Open: how aggressively to detect saturated
  calls at erase time and emit fixed-arity BEAM funs. Mitigation: arity metadata in
  the IR makes the saturated-call detection a local backend decision; start simple
  (correct), optimize under benchmark (Lambert).
- **`IInner` vs full erasure of paths (R-ERASE2/B5).** R-IR only *marks* inner ops
  and bans them. Whether a transport has *any* erased runtime meaning is R-ERASE2's
  question; if it does, `IInner` lowers, if not it stays banned. R-IR must not
  prejudge — it just makes the marker structural. Cross-check: A7 (ua computes)
  must land before B5 can give `IInner` meaning (DAG: `B5 ⇐ R-ERASE2, A7, B2`).
- **Primitive vocabulary scope creep (Thompson).** `PrimOp`/`IForeign`/`ILit`
  could metastasize. Cap: ship `LInt` + the nat/bin `PrimOp`s + the R-EFFECT IO
  ops, nothing speculative; new ops arrive with a consumer (standing rule 1).
- **Conformance oracle.** "Observably equal across backends" needs a canonical
  observation. Decision: the `$show` representation (derived from `Repr`) is the
  oracle; floats/IO ordering are out of scope for the M4 first corpus.
- **Source-string `Artifacts` vs module backends.** Returning `map[string][]byte`
  + `Manifest` is a wider interface than today's `TargetSource`. Risk it
  over-fits BEAM's multi-module shape. Mitigation: JS/Go/Swift return a single
  file + nil manifest; the wider shape is opt-in.

## Test/gate plan

- **IR well-formedness (harness):** every `ICon` is saturated (arity matches the
  `DataSpec`); every `ICase` covers all tags or has a `Default`; `IInner` never
  appears in a deployable program. Property-tested over generated programs.
- **Refactor parity (B2 gate, but starts here):** the existing JS backend ported
  to the new IR must produce **byte-identical or observ-identical** output to
  today's emitter on the whole listings corpus (`harness/listings_test.go`) and
  `codegen/codegen_test.go`. This is the safety net that the IR change is
  meaning-preserving.
- **Cross-backend conformance corpus (M4 gate, seeded now):** a fixed set of pure
  programs (nat arithmetic, list ops, the `length` example, a quotient program,
  the erasure-drops-proofs test `codegen_test.go:97`) must produce equal `$show`
  output on JS and BEAM. Run BEAM via `erl`/`escript` exactly as JS runs via
  `node` (`codegen_test.go:33` `runNode` pattern → a sibling `runBeam`).
- **Inner-taint preservation:** the v3 criterion (cubical checks, doesn't deploy)
  must hold under the new structure — a program using `transp`/`ua` must error at
  `Emit` with the op named, and a pure program must not (regression for
  `session.go:621`'s behavior, now in `codegen`). Reuse `internal/session`
  taint-style tests.
- **`builtin nat` fast path:** machine-int representation gives the same results as
  Peano on both backends (port `TestEmitAndRunNat`, `codegen_test.go:65`).
- **Shadow Rule audit (review gate):** no `core/`, `store/`, `equality/`,
  hash-preimage diff; no hash-format bump. Mechanical check in CI.

## Unblocks (which implement nodes, and what they still need)

- **B2 (refactor Backend stratum to IR, JS = ref)** — **ready-to-build** the moment
  R-IR's IR + reshaped `Backend` land. B2 *is* the parity refactor above; its gate
  is byte/observ-identical JS output on the corpus. Needs nothing external.
- **B3 (BEAM to genuine working FIRST)** — **mostly unblocked**: the IR, the data
  ABI (tuples + `unit` atom), eval/apply with the `_c` currying adapter, and
  `LowerElim` give a Core-Erlang backend a straight path (Idris2-Erlang is the
  living reference). *Still needs:* the BEAM `Shim` (closure/apply realization,
  `$show`, quotient carrier runtime) and the `IForeign` IO ops mapped to BIFs —
  which is R-EFFECT's vocabulary (already ready-to-build) plus a build/run harness
  (`escript`). OTP/actors are **not** B3 — they are D5/R-OTP over the runtime.
- **B3+ fan-out (Go, JVM, WASM, LLVM, Cranelift, Swift)** — **partially unblocked**:
  Go/JVM/Swift are source-emitter ports off the same IR (close to B3 difficulty).
  *Still research:* the **machine-backend closure heap + apply routine + GC** for
  LLVM/Cranelift/WASM-machine (flagged above) — ready-to-port for the IR shape,
  research for the runtime realization.
- **B4 (C-ABI FFI + contracts, R-FFI)** — **seam ready**: `IForeign` + `Foreign()
  map[string]ForeignSig` is the attach point; `ForeignSig` grows a Rune-contract
  field. Still needs R-FFI proper (the contract attach/discharge semantics).
- **B5 (erased meaning of inner paths/Kan; lift the deploy ban, R-ERASE2)** —
  **seam ready**: `IInner{Op,Args}` is the structural hook B5 lowers instead of
  bans. Still needs R-ERASE2 + A7 (ua computes) to define what `IInner` lowers to.
- **M0 (vertical slice on BEAM)** — its codegen spine is exactly B1+B2+B3; with
  R-EFFECT's IO this is the deployable artifact. Still needs the one E-track
  protocol and the hand-sized D1, which are separate nodes.

---

**Status: ready-to-build** for the IR (the six new nodes + `LowerElim` +
`SaturateCtors`), the data ABI table, the reshaped `Backend`/`Shim`/`Artifacts`
boundary, the JS port (B2), and a first BEAM Core-Erlang backend (B3) over pure
programs. **Research-deferred** (clearly labelled, to B3+/B5): the machine-backend
closure heap + apply + GC realization, and the erased runtime *meaning* of
`IInner` (R-ERASE2). The containment line holds: this is all shadow, no core
growth, no hash bump.

Sources:
[Idris2 backend cookbook](https://idris2.readthedocs.io/en/latest/backends/backend-cookbook.html) ·
[Idris2-Erlang IR notes](https://github.com/chrrasmussen/Idris2-Erlang/blob/main/docs/intermediaterepresentations.md) ·
[Making a fast curry (Marlow & Peyton Jones)](https://www.microsoft.com/en-us/research/publication/make-fast-curry-pushenter-vs-evalapply/) ·
[Push/Enter↔Eval/Apply transformation (arXiv:1606.06380)](https://arxiv.org/pdf/1606.06380) ·
[The Core of Erlang (8th Light)](https://8thlight.com/insights/the-core-of-erlang) ·
[A Frame Stack Semantics for Sequential Core Erlang (arXiv:2308.12403)](https://arxiv.org/pdf/2308.12403) ·
[Yhc/Erlang proof of concept (HaskellWiki)](https://wiki.haskell.org/Yhc/Erlang/Proof_of_concept) ·
[Gleam FAQ](https://gleam.run/frequently-asked-questions/)
