# R-FFI — C-ABI FFI with proof obligations

> **DELIVERED at consumer scope (2026-06-16) — B4 core done.** `foreign name : T end`
> axioms carry the assume-tier (the type IS the contract, tracked in the proof-cache
> `A`-set); scalar/String/Ptr marshalling (`ILit{LitInt|LitStr|LitPtr}`) + IForeign
> accessors are byte-identical across all 8 backends (incl C + LLVM native); MULTI-
> FOREIGN / NON-IDENTITY calls conform (ch197 `hostId` + binary first-projection
> `hostConst`, `TestForeignMultiConformance` 8/8). PARKED (Standing Rule 1): the
> runtime contract-GUARD tier (consumer = D3/D4 BLAS interop, unbuilt) and arbitrary-
> precision native LitNat past i64. See PARKING-LOT.md.

> Roadmap node B4 (`B4 [I] C-ABI FFI + contract checking ⇐ R-FFI, B2`). The edge
> from telos 2 ("a C-ABI FFI whose foreign calls carry Rune assertions/specs you
> can assume or discharge at the boundary"). Builds directly on R-EFFECT's
> `IForeign` IR node and `store/io.go` primitive pattern, and on R-IR's
> `Foreign() map[string]ForeignSig` registry (whose `ForeignSig` was explicitly
> reserved "later: Rune contract for B4", `ref_docs/wootz/R-IR.md:327`). Unblocks
> the interop track (R-INTEROP / D4 — bind NumPy/BLAS/plotting behind contracts)
> and D3 (linear-algebra interfaces over BLAS) and D6 (richer fs/net surface).

## Problem (what's stuck/absent today, with file:line)

There is no foreign-call mechanism in the kernel at all, and the one
runtime-reaching channel that exists is representation-only and contract-free.

1. **No foreign call exists.** The erased IR is six nodes
   (`codegen/ir.go:13-55`: `IVar/IGlobal/IUnit/ILam/IApp/ILet`); there is no
   primitive-call node. The JS backend (`codegen/js.go`) can only
   `console.log($show(main))` — print one pure value. R-EFFECT adds the first
   foreign node (`IForeign{Op,Args}`, `ref_docs/wootz/R-EFFECT.md:206`) and a
   `store/io.go` builtin group of *bodiless, permanently-neutral* primitives
   (`primPutStr`, `primReadFile`, …). But those primitives are **trusted blindly**:
   their Rune type is the *only* contract, the runtime extern is assumed correct,
   and nothing records that the program now rests on an unverified foreign axiom.

2. **The only "reach the runtime" mechanism is `builtin nat`/`bin`, which carries
   no obligation.** `AddBuiltinNat` / `AddBuiltinBin`
   (`internal/session/session.go:230,256`) swap a *pure* datatype's
   representation (Peano → BigInt). They validate that the named terms *fit a Rune
   type* (`session.go:241-244`), then emit faster code. There is no place to say
   "this foreign thing satisfies property P," no pre/post, and no marshalling
   spec beyond the implicit BigInt one baked into `codegen/js.go:70` `emitNat`.

3. **Representation/marshalling is hard-coded and JS-only.** A constructor is a
   tagged JS record (`codegen/js.go:63`), `unit` is `null` (`js.go:25`), nat is
   BigInt. R-IR writes these down as a per-backend `Repr` table
   (`ref_docs/wootz/R-IR.md:244-252`) — but only for *Rune-internal* data. There
   is **no ABI for crossing to C**: no notion of how `Nat`, `String`, a `Result`,
   or a struct maps to a C `int64_t`, `char*`, tagged union, or `struct`, and no
   statement of who owns the memory. The eight backends
   (`humble-humming-elephant.md:22`: JS, BEAM, Go, JVM, WASM-DOM, WASM-machine,
   LLVM, Cranelift) each reach C through a *different* native FFI, so a single
   foreign declaration must compile to eight different marshalling shims.

4. **The proof cache has no concept of an assumption.** A certificate is
   `Δ ⊢ (h ✓)` — well-typed under the *bodies it unfolded*
   (`store/cert.go:9-23`, `ref_docs/rune-proof-cache-semantics.md:23-37`). The dep
   set `Δ` is logged through the one `Unfold` gateway. A foreign primitive is a
   **permanently neutral head** (`core/eval.go:427-462` `rigidHead`: every builtin
   group's head is rigid, never unfolds) — so it is *never* added to `Δ`, and a
   proof that *relies on a foreign contract* records **no trace of that reliance**.
   There is currently no analogue of Coq's `Print Assumptions` or Agda's `--safe`:
   nothing tells you "this theorem is only true if `c_sqrt` actually returns a
   non-negative root."

5. **The deploy ban is the wrong shape for FFI.** `innerTaint`
   (`session.go:621-654`) refuses to deploy anything mentioning a *cubical* value
   member because transport-along-ua has no erased meaning. A foreign call is the
   **opposite**: it has a perfectly good erased meaning (the C call) but an
   *unverified specification*. We need a parallel, orthogonal tracking — not
   "won't run" but "runs on an assumption" — that R-IR's `IForeign` seam can carry.

The net: Wootz can state pure proofs and (with R-EFFECT) perform effects, but it
cannot **bind a foreign function with a Rune specification, discharge or assume
that spec, marshal data across the C ABI on eight backends, and have the proof
cache remember exactly which foreign axioms a proof leans on.** That last clause
is the user's ask — "write assertions or proofs around FFI" — and it is the whole
of R-FFI.

## Prior art (what the literature/other systems do; cite)

- **F\* `assume val` + refinement specs, erased at compile time.** The direct
  model for "a spec you may assume." An external function is declared
  `assume val getCurrent : unit -> Ifc l (ensures …)`: F\* trusts the
  signature, generates **no** proof obligation for the body it doesn't have, but
  *callers* still discharge the precondition and *get* to assume the
  postcondition. Refinements/`requires`/`ensures` live in the `GTot` (ghost,
  total) effect and are **erased at compile time** — exactly the QTT-0 / proof
  layer this substrate already has. The lesson: a foreign declaration is a
  **typed axiom**; its spec is proof-only (erased); the obligation moves to the
  call site ([LIO\* in F\*](https://arxiv.org/pdf/2004.12885);
  [F\* tutorial](https://fstar-lang.org/tutorial/book/part1/part1_getting_off_the_ground.html)).
- **Coq `Print Assumptions` / Agda `--safe` / "Multiverse" modular axiom
  tracking.** The model for *tracking* which assumptions a result rests on. Coq
  walks a term's dependency graph and reports every `Axiom`/`admit`/parameter it
  transitively uses; Agda's `--safe` *forbids* postulates in a checked module;
  the Multiverse gives a "type-theoretic, local and modular alternative to the
  `--safe` pragma of Agda, or the Print Assumptions checker of Coq"
  ([The Multiverse, arXiv:2108.10259](https://arxiv.org/pdf/2108.10259)). This
  substrate already *has* the machine for it — the certificate dep set `Δ`
  (`store/cert.go`) is a content-addressed dependency graph — so "Print
  Assumptions" is a query over a `Δ` that has been *extended to log foreign
  axioms*. That is the central insight of the chosen approach.
- **Idris 2 `%foreign` with FFI descriptors + marshallable-type predicate.** The
  model for the *declaration surface* and *marshalling*. `%foreign "C:sqrt,libm"`
  attaches a backend-tagged target string to a typed declaration; a `C_Types`
  predicate enumerates exactly which types may cross (`Int`, `Double`, `String`,
  `Ptr`, `unit`, …), and each backend reads the descriptor to build its shim
  ([Idris FFI](https://docs.idris-lang.org/en/v0.10/reference/ffi.html);
  [Brady, *Idris — Systems Programming Meets Full Dependent
  Types*](https://www.type-driven.org.uk/edwinb/papers/plpv11.pdf)). The lesson:
  a *backend-keyed target table* + a *closed set of marshallable representations*
  is the portable FFI shape, and it is the natural growth of R-IR's `Repr` table
  to a C-facing column.
- **VeriFFI: A Verified Foreign Function Interface between Coq and C
  (POPL/PACMPL 2025).** The deepest precedent and the marshalling design to
  steal. It pairs each Coq function with a **functional model** and a **C
  specification**, uses a "novel hybrid deep/shallow description of Coq dependent
  types" to bridge the representations, and *proves* the C code meets the
  Coq-derived spec (in VST), guaranteeing type-safety and correctness across the
  boundary ([VeriFFI, ACM](https://dl.acm.org/doi/full/10.1145/3704860)). The
  lesson: **a foreign declaration = (Rune spec) × (representation map) × (a
  discharge obligation)**; when the obligation is met by an external proof you get
  VeriFFI-grade assurance, when it is only *assumed* you get F\*-grade trust — and
  R-FFI should make that a *tier*, not a binary.
- **Soft contract verification / higher-order contracts + blame (Nguyen,
  Tobin-Hochstadt et al.).** The model for the *runtime* fallback when a contract
  is neither proven nor blindly assumed. Higher-order contracts with **blame**
  attribute a violation to the component that broke its invariant; soft contract
  verification *statically discharges* the checks it can and leaves runtime
  checks only where it can't ([Soft contract verification, arXiv:1507.04817](https://arxiv.org/pdf/1507.04817);
  [Corpse Reviver, arXiv:2007.12630](https://arxiv.org/pdf/2007.12630)). The
  lesson: an *un*proven post-condition that is a **decidable Rune predicate** can
  be compiled to a **runtime guard with blame at the boundary** — the furnace
  on-ramp: test/guard first, prove later (Savage), and Lambert's "the C library
  lies sometimes, catch it at the door."
- **Idris `%World` / Lean `IO.RealWorld` erased token** (already R-EFFECT's base,
  `ref_docs/wootz/R-EFFECT.md:50-69`). Foreign *effects* thread the erased world;
  R-FFI's contract layer is orthogonal and rides on top — a contract is a
  proof-layer (QTT-0) attachment, the call itself is the R-EFFECT `IForeign`.

Cross-cutting, three smiths: F\*/Idris show the **declaration + assume** surface
is library-and-pragma, never core growth (Thompson). VeriFFI shows the
**discharge** tier is real and is "model + spec + proof" (Lambert). Soft
contracts + blame show the **runtime-guard** tier is the teachable middle
(Savage). Coq/Agda show **assumption-tracking is a dependency-graph query** —
which this substrate's content-addressed `Δ` already is.

## Chosen approach for THIS substrate (concrete; respects containment)

**Thesis: a foreign declaration is a *bodiless, permanently-neutral builtin* (the
quotient/fibrant/IO pattern) whose *type is a Rune contract*, and discharging-or-
assuming that contract is *tracked in the existing certificate dep graph* by
making the foreign axiom a logged dependency.** No new outer-core constructor, no
hash-format bump (CLAUDE.md: a bump is only for a new *core* constructor). The
genuinely new surface is in three contained places R-EFFECT/R-IR already opened:
a builtin group (`store/foreign.go`), a contract-aware `ForeignSig`/`Repr` in
`codegen/`, and a `markAssumed` extension to the cache's `Δ` (the X-track / R-FRAME
gateway, reused).

Five pieces: (A) the foreign-declaration surface; (B) contract attachment as Rune
props; (C) the three-tier discharge/assume/guard policy; (D) assumption tracking
in the dep graph; (E) C-ABI marshalling across the eight backends.

### (A) Foreign-declaration surface — a typed, backend-keyed axiom

A foreign function is declared in surface Rune and lowered to a **bodiless
content-addressed definition** whose head is **permanently neutral**
(`rigidHead`, `core/eval.go:427-462`) — it never unfolds, exactly like
`primPutStr`. Its *type* is its contract carrier; its *target* (the C symbol per
backend) is **session/codegen metadata**, never hashed (the same discipline as
`builtin nat`'s binding, which "is session state only — nothing enters the store",
`session.go:228`). So two foreign decls with the same Rune contract but different
C targets are *distinct definitions* only if their contracts differ — the identity
is the contract, not the symbol, which is correct: the proof depends on the spec,
not on which `.so` provides it.

```
foreign c_sqrt : (x : Double) -> { y : Double | leq 0.0 y }
  is "C:sqrt,libm"            -- target table: keyed per backend below
end
```

The surface keyword `foreign … is <target> end` parses to a new
`surface.ForeignDef` (a `surface/` change, contained, like `BuiltinNat`). It
resolves to a `core` term that is **just the type** (no body), stored via a new
`store/foreign.go` builtin-style entry. The *target string* and its
**per-backend overrides** are a side table (below, piece E), not part of the hash.

### (B) Contract attachment — pre/post as Rune props (QTT-0, erased)

The contract is **not a new syntax**; it is the foreign decl's *Rune type*, using
the type system already present:

- **Postcondition** is a refinement on the result. Refinements are **R-SUM
  Sigma** of value-and-proof: `{ y : Double | P y }` desugars to
  `Σ (y : Double). El (P y)` once R-SUM lands (C1). The proof component is
  **QTT-0** (erased, `quantity/zeroone.go`) — exactly F\*'s `GTot`-erased
  refinement. *Before R-SUM*, R-FFI ships the **non-dependent** form: the post is
  a separate, named **lemma the caller may invoke**, `c_sqrt_post : (x:Double) ->
  El (leq 0.0 (c_sqrt x))`, itself a foreign/assumed fact (a bodiless prop). This
  is the honest M0-era slice; refinements-in-the-type are the C1-gated upgrade.
- **Precondition** is a 0-quantity argument the caller must supply a proof for:
  `foreign c_div : (x : Double) -> (y : Double) -> (0 _ : El (not (eq 0.0 y)))
  -> Double`. The proof is erased; supplying it is the caller's obligation,
  exactly F\*'s `requires`. This needs **no new feature** — it is an ordinary
  QTT-0 Pi (Phase 5 is done, `CLAUDE.md` Phase 5).
- **Pure foreign functions** (no effect) get a pure arrow type; **effectful**
  foreign functions return `IO T` (R-EFFECT) and their call lowers through
  `IForeign`. A pure foreign function is a foreign *axiom* whose call also lowers
  to `IForeign` (it touches C), but whose *type* claims purity — that claim is
  itself part of the contract the discharge tier must respect.

The contract is therefore **the type, in the existing theory**: refinements via
R-SUM-Sigma+QTT-0, preconditions via QTT-0 Pi, effects via R-EFFECT `IO`. R-FFI
adds *no* logic to conversion or checking — the kernel checks a foreign call
exactly as it checks any application (the proof obligations are ordinary
arguments/refinement-eliminations).

### (C) Three tiers: discharge / assume / guard (the policy axis)

This is the heart of the user's "assume-or-discharge." Every foreign contract
clause sits in exactly one tier, recorded per-decl:

1. **Discharge (VeriFFI-grade).** The clause is *proven* by an external
   artifact: a Rune proof that the C realizer meets the spec (long-term: a VST-
   style C proof, near-term: a Rune-side model + a proof the model matches). When
   discharged, the clause contributes **nothing** to the assumption set — it is as
   trusted as any kernel-checked def. Status: the *mechanism* (a discharged clause
   is a normal certified lemma) is ready-to-build; *producing* C-side proofs is
   the interop research tail (R-INTEROP), labelled research.
2. **Assume (F\*-grade).** The clause is *postulated*: the foreign axiom is taken
   on trust, its spec is available to callers, and it is **recorded as an
   assumption in the dep graph** (piece D). This is the default and the M0/M4
   tier. Sound *relative to* the assumption, and the assumption is *visible*
   (`rune assumptions <def>` lists them, the `Print Assumptions` analogue).
3. **Guard (soft-contract-grade).** The clause's post-condition is a **decidable
   Rune predicate**; instead of proving or blindly trusting, the boundary
   **checks it at runtime** and **blames the foreign side** on failure. The guard
   is generated by erasing the predicate to an IR `decide` and wrapping the
   `IForeign` call: `assert-post (decide (P result)) "c_sqrt" result`. This is the
   furnace on-ramp (Savage): a learner *guards* a foreign call, runs it, sees
   blame when libm misbehaves, and *later* promotes the guard to an assume (drop
   the check, take the axiom) or a discharge (prove it). The guard tier needs a
   `decide : (P : Prop) -> Dec P`-style bridge for the predicates that have one
   (D1 decidability), and is **ready-to-build** for the decidable fragment.

The tier is **per clause, per decl**, defaulting to **assume**, declared with a
surface attribute:

```
foreign c_sqrt : (x : Double) -> { y : Double | leq 0.0 y }
  is "C:sqrt,libm"
  with post guard       -- check leq 0.0 y at the boundary, blame "c_sqrt"
end
```

`with post assume` (default), `with post guard`, `with post discharge by
<proofName>`. Pre-conditions are always *discharge* (the caller must supply the
proof) — there is no "assume the precondition" for the callee; F\*'s asymmetry.

### (D) Assumption tracking in the dep graph (the load-bearing novelty)

The certificate is `Δ ⊢ (h ✓)` where `Δ` is the set of **bodies unfolded**
(`store/cert.go`, `rune-proof-cache-semantics.md:29`). A foreign axiom is a
*rigid head* and is never unfolded, so today it would vanish from `Δ`. R-FFI
**extends `Δ` with an assumption tag**: when conversion/checking *consults a
foreign axiom's contract* (i.e. uses its type as a fact — which happens whenever a
proof eliminates a refinement or supplies a precondition proof), the machine logs
the axiom's hash into a **second, write-only set `A` (the assumed set)**, through
the *same gateway discipline* the Frame Lemma already mandates (R-FRAME's
`markImprecise`/logging seam, `ref_docs/wootz/r-frame.md`). The certificate key
becomes `hash(h, ‖U‖, ‖A‖)` — a backward-compatible superset (an empty `A`
reproduces today's key bit-for-bit, so **no existing certificate changes and there
is no cache nuke**).

Why this is exactly right for this substrate:

- **Soundness is unchanged and the Frame Lemma still holds verbatim.** `A` names
  immutable content-hashes (the foreign axioms), so `Δ,A ⊢ (h ✓)` has a fixed
  truth value forever (`rune-proof-cache-semantics.md:33`). A discharged clause's
  proof enters `Δ` normally; an *assumed* clause's axiom enters `A`. The cache
  stays append-only, no invalidation (`§1` of the cache doc).
- **`Print Assumptions` is free.** `rune assumptions <def>` = the transitive
  union of `A` over the def's certificate closure — a graph walk over the same
  content-addressed store (Coq/Agda parity, by reusing `Δ`'s machinery).
- **`--safe` is a one-line policy.** "This module uses no assumed foreign
  contracts" = "`A` is empty across its closure." A *tiered* assurance bar
  (telos 3) is then literally a predicate on `A`: foundational modules require
  `A = ∅` (fully discharged), leaves may have non-empty `A` (assumed/guarded).
- **Guards leave a runtime trace, not a cache trace.** A *guarded* clause is
  **not** an assumption (it is checked at runtime), so it does **not** enter `A`;
  instead it marks the def as carrying a runtime obligation (a codegen flag), and
  the deploy artifact includes the blame check. This keeps the static assumption
  set honest: `A` is exactly the *unchecked* trust.

This is the central design decision and it is **ready-to-build**: it is a strict,
backward-compatible superset of the cert key, riding the R-FRAME logging gateway,
producing the Coq-grade `Print Assumptions` the user's "proofs around FFI" needs.

### (E) C-ABI marshalling across the eight backends

Marshalling is an extension of R-IR's `Repr` table (`R-IR.md:244-252`) with a
**C-facing column** and a closed set of **marshallable representations** (the
Idris `C_Types` discipline; the VeriFFI deep/shallow split). A foreign decl's
argument and result types must each have a `CRepr`; a type with none cannot cross
(a clean compile error, like Idris's `C_Types` failure).

```
| Rune type        | CRepr (C ABI)        | JS         | BEAM (NIF/port)   | WASM/LLVM/Cranelift | JVM        | Swift   |
|------------------|----------------------|------------|-------------------|---------------------|------------|---------|
| Nat (builtin)    | int64_t / GMP*       | BigInt↔num | integer (bignum)  | i64 / GMP ptr       | long/BigInt| Int     |
| Double           | double               | number     | float             | f64                 | double     | Double  |
| Bool             | int (0/1)            | bool       | atom true/false   | i32                 | boolean    | Bool    |
| String           | char* (UTF-8, len)   | string     | binary            | (ptr,len) in memory | String     | String  |
| Unit / IUnit     | void / ignored arg   | null       | atom unit         | (nothing)           | null/Unit  | Void    |
| Ptr A (opaque)   | void* (owned/borrow) | extern obj | resource term     | i32 table index     | long handle| OpaquePtr |
| Result E A       | tagged: int tag+union| {tag,..}   | {ok,_}/{error,_}  | tag word + payload  | sealed cls | enum    |
```

`*` big `Nat` uses the same GMP/bignum bridge the `builtin nat` Repr already
implies. **Memory ownership** is part of the `CRepr` for `Ptr`/`String`: a
`borrow` arg is not freed by Rune; an `owned` result is freed by a backend-
provided finalizer (BEAM resource GC, WASM table + explicit free, JVM
cleaner) — ownership is a *contract clause* (an affine obligation), and on
backends without it the deploy is a guard-tier runtime leak check or a hard
error, not a silent UAF (Lambert: "no misaligned FFI struct").

The eight backends split into **three FFI realization classes**, mirroring R-IR's
source/module split:

1. **Native-C backends (LLVM, Cranelift, WASM-machine):** the `IForeign{Op}`
   lowers to a direct C-ABI call; `CRepr` drives the calling-convention
   marshalling (System V / wasm ABI). This is the *reference* C-ABI — the others
   bridge *to* it.
2. **Hosted-runtime backends (BEAM via NIF/port, JVM via JNI/Panama, JS via
   N-API/WASM, Swift via its C interop):** the `IForeign` lowers to the host's
   *own* native-call mechanism; `CRepr` plus a per-backend `Shim` (R-IR's `Shim`,
   `R-IR.md:326`) generate the glue. BEAM additionally chooses NIF (in-process,
   fast, can crash the VM — guard tier matters) vs port (isolated process,
   Lambert-safe) per a decl attribute.
3. **No-native backends / sandbox (WASM-DOM, pure JS):** a foreign C call may be
   *unavailable*; the decl's per-backend target table can provide a **JS/DOM
   shim** instead of a C symbol, or the decl is **unsupported on that target** (a
   clean `Emit` error, surfaced like R-IR's `IInner` ban). The per-backend target
   table (piece A) is exactly where "C:sqrt,libm" for native and
   "js:Math.sqrt" for the browser coexist (Idris's multi-backend descriptors,
   Gleam's BEAM/JS dual targeting).

`codegen.Backend.Foreign()` (`R-IR.md:280`) returns, per op, the `ForeignSig`
grown with the contract and `CRepr` data; an op a backend cannot realize is a
clean error (R-IR's conformance discipline). The M4 conformance corpus checks
that a foreign call's *observable* result and its *guard/blame behavior* agree
across the backends that support it.

**Containment audit.** No `core/` term, no `store/` hashing, no `equality/`, no
hash-format bump. New: `surface.ForeignDef` (surface), `store/foreign.go` (a
builtin-style group + a non-hashed target table), a contract-and-`CRepr` field on
R-IR's `ForeignSig`/`Repr` (codegen), and the `A` (assumed) set on the cert key
(store/cert + the R-FRAME gateway). The foreign axiom is a *permanently neutral
head* (`rigidHead`), the same kind of object as `ua`/`primPutStr` — Reality lives
in codegen + a contained group, where the strata thesis puts it.

## Interfaces & signatures to add (Go + Rune surface as relevant)

Go:

```go
// surface/ — a new top-level item, like BuiltinNat (surface state, parses
// `foreign NAME : TYPE is "TARGET" [with CLAUSE...] end`).
type ForeignDef struct {
    Name    string
    Ty      Exp                       // the contract carrier (refinements/pre = QTT-0)
    Targets map[string]string         // backend -> symbol, e.g. {"c":"sqrt,libm","js":"Math.sqrt"}
    Clauses []ContractClause          // per-clause tier
}
type Tier int               // TAssume (default), TGuard, TDischarge
type ContractClause struct {
    Kind  ClauseKind        // CPre, CPost, COwnership
    Tier  Tier
    Proof string            // for TDischarge: the proof def name
}

// store/foreign.go — the foreign builtin group (pattern of store/io.go / fib.go).
// Each foreign decl is a bodiless content-addressed def; the head is rigid.
func (s *Store) AddForeign(name string, ty core.Tm) core.Hash   // returns the axiom hash
func (s *Store) ForeignRoleOf(h core.Hash) bool                 // rigidHead consults this
func (s *Store) ForeignTargets(h core.Hash) (map[string]string, bool) // NON-hashed side table
func (s *Store) ForeignClauses(h core.Hash) []core.ForeignClause

// core/eval.go — rigidHead gains the foreign group (mirrors Fib/Io checks,
// core/eval.go:441-460). A foreign axiom never reduces.
//   if m.Fo != nil && m.Fo.ForeignRoleOf(h) { return true }
// Machine gains:  Fo ForeignInfo

// store/cert.go — the assumed set A, a backward-compatible key superset.
type Cert struct {
    Deps    []core.Hash   // unchanged: bodies unfolded (Δ)
    Assumed []core.Hash   // NEW: foreign axioms consulted as facts (A); nil = today's key
}
func certKey(h core.Hash, deps, assumed []core.Hash) core.Hash  // empty assumed == old key
func (s *Store) Assumptions(h core.Hash) []core.Hash            // transitive A over the closure

// The R-FRAME logging gateway gains a sibling to Unfold for foreign facts:
//   markAssumed(h core.Hash)   // write-only into A, like the dep log into Δ

// codegen/ — grow R-IR's reserved ForeignSig + the Repr table.
type CRepr int        // CInt64, CDouble, CBool, CString, CPtr{Owned|Borrow}, CTagged, CVoid
type ForeignSig struct {
    Arity   int
    ArgRepr []CRepr
    ResRepr CRepr
    Clauses []ContractClause   // post-guard predicates the backend must emit checks for
    Targets map[string]string  // per-backend symbol/shim
}
// codegen.Backend.Foreign() map[string]ForeignSig  (R-IR.md:280) carries these.
// IForeign{Op,Args} (R-EFFECT/R-IR) is the call node; a guarded clause wraps it
// in an IR `assert-post` (decide + blame), an assumed clause does not.
```

Rune surface (a `surface/` change + a guard runtime in each `Shim`; NOT core):

```
-- Pure, assumed (default): an F*-grade axiom; appears in `rune assumptions`.
foreign c_sqrt : (x : Double) -> { y : Double | leq 0.0 y } is "C:sqrt,libm" end

-- Precondition discharged by the caller (QTT-0 erased proof), post guarded:
foreign c_div : (x y : Double) -> (0 _ : El (not (eq 0.0 y))) -> Double
  is "C:c_div,libm"
  with post guard               -- runtime check + blame "c_div"
end

-- Effectful foreign call over R-EFFECT IO, post discharged by a Rune proof:
foreign c_read_u32 : Ptr U8 -> IO Nat
  is "C:read_u32,libmine"
  with post discharge by c_read_u32_spec
end

-- Multi-backend target (native C + browser fallback), Idris-style:
foreign sqrt : (x : Double) -> Double
  is { c = "sqrt,libm", js = "Math.sqrt", beam = "math:sqrt/1" }
end
```

CLI: `rune assumptions <def>` (the `Print Assumptions` analogue, walks `A`);
`rune check --safe FILE` (fails if any deployed def's closure has non-empty `A`);
`rune emit`/`run` emit guard checks for `with … guard` clauses.

## Worked micro-example (the teachable artifact)

`sqrt.rune` — the furnace's first *contract-carrying foreign call*, taught across
all three tiers so the on-ramp from "I guarded it" to "I proved it" is visible.

```
-- Tier 1, the on-ramp (Savage): guard the post, run it, watch blame.
foreign c_sqrt : (x : Double) -> { y : Double | leq 0.0 y }
  is "C:sqrt,libm"
  with post guard
end

main : IO Unit is
  seq
    let r = c_sqrt 2.0
    primPutStr (showDouble r)      -- prints 1.414…; the boundary checked leq 0.0 r
  end
```

What the learner sees and can verify:

1. `c_sqrt` type-checks: its contract is just a Rune type (a refinement). The
   kernel records it as a **foreign axiom** in `store/foreign.go`; `rune
   assumptions main` lists `c_sqrt` — *because the post is guarded, it is NOT in
   `A`*, it shows under "runtime-guarded," teaching the tier distinction.
2. `rune run sqrt.rune main` calls libm's `sqrt` via `IForeign`, **checks
   `leq 0.0 r` at the boundary**, and prints. If a buggy `sqrt` returned `-1.0`,
   the program halts with `contract violation: c_sqrt post (leq 0.0 y) — blame
   c_sqrt` — Lambert's "the C library lied, caught at the door," with blame.
3. Promote to **assume** (`with post assume`): the runtime check vanishes (faster,
   trusted), and now `rune assumptions main` lists `c_sqrt` under **assumed
   axioms** — the F\* tier. `rune check --safe` now *fails* on `main`: you took an
   unproven foreign axiom. The cache key gains `c_sqrt`'s hash in `A`.
4. Promote to **discharge** (`with post discharge by c_sqrt_correct`, supplying a
   Rune proof that the model of `sqrt` meets the spec): `A` is empty again,
   `--safe` passes, and the proof enters `Δ` like any lemma. The same program,
   three assurance levels, one surface.

The teaching beat: *a foreign call is an axiom with a spec; you may guard it
(check at runtime, blame on failure), assume it (trust, and it shows in `Print
Assumptions`), or discharge it (prove, and it disappears from your trust set) —
and the proof cache always tells you exactly which foreign axioms your program
rests on.*

Cross-backend artifact (Lambert/M4): the same `sqrt.rune` compiles to LLVM (direct
C call to libm), BEAM (NIF or `math:sqrt/1`), and JS (`Math.sqrt`); the guard and
blame behave identically; `Double`/`leq` marshal per the `CRepr` table.

## Risks / open sub-questions

- **Refinements-in-the-type need R-SUM (C1).** The clean `{ y | P y }`
  postcondition is Σ-of-value-and-erased-proof, which needs outer Sigma. *Until
  R-SUM lands*, R-FFI ships the **non-dependent** form (a separate
  assumed/discharged post-lemma per foreign decl). **Status: ready-to-build for
  the non-dependent form; refinement sugar is C1-gated.**
- **Guard tier needs decidability (D1).** A post-condition can only be *guarded*
  if it has a `Dec P` instance (a decision procedure). Non-decidable posts can
  only be assumed or discharged. *Mitigation:* the surface rejects `with post
  guard` on a non-decidable predicate at elaboration. **Status: ready-to-build for
  the decidable fragment; depends on D1's decidability typeclass (C-ABS/R-SUM).**
- **Discharge tier (VeriFFI-grade C proofs) is research.** Proving the *C
  realizer* meets the spec (VST-style) is the interop research tail and is **not**
  in scope for B4's first cut. B4 ships discharge-*by-Rune-proof* (prove a Rune
  model matches the spec, *assume* the C↔model bridge) — honest partial discharge.
  Full C-side proof is **research, parked under R-INTEROP/D4**.
- **C-ABI struct/union layout fidelity (Lambert's "misaligned FFI struct").**
  `CRepr` for `Result`/tagged types must match the C compiler's struct layout
  exactly per target ABI (System V, wasm, JVM Panama). *Mitigation:* start with the
  scalar + `String` + opaque-`Ptr` fragment (Idris's `C_Types` floor); structs are
  a later `CRepr` extension with a per-ABI layout oracle. **Status: scalars/ptr
  ready-to-build; struct layout is research per-ABI.**
- **Memory ownership across GC'd backends.** `owned`/`borrow` is an affine
  obligation; backends without affine runtime support (JS, naive WASM) need either
  a finalizer hook or a guard-tier leak check. *Mitigation:* ownership defaults to
  `borrow` (Rune never frees); `owned` results require a backend that provides a
  finalizer or the decl is unsupported there. **Status: borrow ready-to-build;
  owned is per-backend, partly research.**
- **`A`-set soundness interaction with R-FRAME.** Logging foreign-axiom
  *consultation* must go through the same disciplined gateway as `Unfold`, or `A`
  could be incomplete (a proof leans on an axiom not recorded → unsound `--safe`).
  *Mitigation:* `markAssumed` is the only way a foreign contract becomes a usable
  fact (mirrors the sealed-`Body`/`unfold` barrier, `cache-semantics §3`);
  property-test that every proof using a foreign fact has it in `A`. **This is the
  one soundness-critical seam; ready-to-build but must be property/mutation-tested
  exactly like the Frame Lemma.**
- **Blame for higher-order foreign values.** If a foreign function takes or
  returns a *Rune function* (callback), contracts become higher-order (Nguyen's
  setting) and blame must wrap both directions. *Mitigation:* first cut is
  first-order foreign signatures only; higher-order is a labelled extension.
  **Status: first-order ready-to-build; higher-order is research.**

## Test/gate plan

- **Containment gates (Thompson, must pass):** no diff to `core/term.go`,
  `core/hash.go`, `defFormatVersion`/`hashFormatVersion`; a test asserts an
  existing pure def's hash AND its certificate key are byte-identical
  before/after the foreign group is registered (empty `A` reproduces the old key —
  the fib/interval groups already prove own-hash-space is achievable).
  `store/foreign_test.go` mirrors `store/io_test.go`/`fib_test.go`.
- **Assumption-tracking correctness (the soundness property):** for any def `d`
  that uses a foreign fact, `Assumptions(‖d‖)` contains exactly the assumed
  foreign axioms in its closure (no more, no fewer). Mutation target: a check that
  uses a foreign contract *without* logging it into `A` must be killed (parallels
  the Frame-Lemma barrier mutants, `cache-semantics §7`).
- **`--safe` firewall:** a module declared `--safe` with any assumed (`A ≠ ∅`)
  foreign decl in a deployed def's closure fails; the same module with all foreign
  decls discharged passes. Property-tested.
- **Guard/blame runtime (Lambert):** a foreign decl with `with post guard` whose
  C realizer violates the post halts with a blame error naming the decl; a
  conforming realizer passes. Run on JS (M0) and, when B3 lands, BEAM.
- **Marshalling round-trip:** each `CRepr` round-trips a value Rune→C→Rune
  observably unchanged (scalar + String + opaque Ptr first); a type with no
  `CRepr` is a clean elaboration error.
- **Cross-backend conformance (M4):** the same contract-carrying program (`sqrt`,
  a `Result`-returning read) produces equal observable results and identical
  guard/blame behavior on every backend that supports the call; an unsupported
  target yields a clean `Emit` error (R-IR's `IInner`-ban shape).
- **Proof-cache regression:** a def using an *assumed* foreign decl cache-hits on
  reload with the same `(h,‖U‖,‖A‖)` key; promoting a clause assume→discharge
  changes the key and re-checks (correctly, a miss), assume→guard removes it from
  `A` (key changes, runtime flag set). Extend `store/cert_test.go`.

## Unblocks (which implement nodes, and what they still need)

- **B4 (this node, C-ABI FFI + contract checking):** **ready-to-build** for the
  *non-dependent contract + assume/guard tiers + assumption-tracking + scalar/
  String/Ptr marshalling + JS backend*. The surface `foreign` decl, the
  `store/foreign.go` group, the `A`-set cert key, and the guard runtime are all
  contained repeats of the v2/v3/R-EFFECT pattern. *Still needs:* R-SUM (C1) for
  refinement-typed posts, D1 for the guard tier's decidability, and B2 (the IR
  refactor, R-IR) for the `IForeign`/`ForeignSig`/`Repr` substrate this rides.
- **The interop track (R-INTEROP / D4):** **seam ready** — binding NumPy/BLAS/
  plotting is exactly "foreign decls with contracts," and the assume/guard tiers
  are the "contract-guard" the roadmap names (`D4: Math/Stats/AI/ML/Plotting
  INTEROP (bind, contract-guard)`). *Still needs:* R-INTEROP proper (the Python/C
  bridge mechanism and the numeric `CRepr`s — arrays, BLAS handles) and the
  VeriFFI-grade discharge research for full proof (vs assume).
- **D3 (reals/floats + linear-algebra interfaces over BLAS):** **unblocked at the
  seam** — BLAS routines are foreign decls with numeric contracts; *still needs*
  D1 (the numeric tower) and the array `CRepr` (research).
- **D6 (IO/OS/net/time/fs richer surface):** **directly aided** — fs/net beyond
  R-EFFECT's primitive set becomes contract-carrying foreign decls; *still needs*
  D1 (`Result`/`IOError`) and the per-backend syscall `CRepr`s.
- **M0 (vertical slice on BEAM):** **aided, not blocked** — M0's pure/IO half is
  R-EFFECT; R-FFI adds the *contract-guarded* foreign call when M0 wants a real C
  dependency. *Needs* B3 (BEAM) for the BEAM NIF/port realization of `IForeign`.

---

**Status: ready-to-build** for the contract-carrying foreign-declaration surface,
the `store/foreign.go` bodiless-axiom group, the **assume** and **guard** tiers,
the **assumption-tracking `A`-set** (the backward-compatible cert-key superset and
its `Print Assumptions`/`--safe` queries — the user's "proofs around FFI"), and
scalar/String/opaque-`Ptr` marshalling on the JS backend, all riding R-EFFECT's
`IForeign` and R-IR's `ForeignSig`/`Repr`/`Shim` seams with **no outer-core
growth and no hash-format bump**. **Research/parked, clearly labelled:**
refinement-typed postconditions (needs R-SUM/C1), the guard tier's full
decidability (D1), VeriFFI-grade C-side discharge proofs (R-INTEROP), C struct/
union layout per ABI, `owned`-memory across GC'd backends, and higher-order
foreign contracts with blame.

Sources:
[LIO\* / F\* assume val (arXiv:2004.12885)](https://arxiv.org/pdf/2004.12885) ·
[F\* tutorial — refinements & assume](https://fstar-lang.org/tutorial/book/part1/part1_getting_off_the_ground.html) ·
[The Multiverse: modular axiom tracking (arXiv:2108.10259)](https://arxiv.org/pdf/2108.10259) ·
[Idris FFI / %foreign + C_Types](https://docs.idris-lang.org/en/v0.10/reference/ffi.html) ·
[Brady, Idris — Systems Programming Meets Full Dependent Types](https://www.type-driven.org.uk/edwinb/papers/plpv11.pdf) ·
[VeriFFI: A Verified FFI between Coq and C (ACM)](https://dl.acm.org/doi/full/10.1145/3704860) ·
[Soft contract verification (arXiv:1507.04817)](https://arxiv.org/pdf/1507.04817) ·
[Corpse Reviver: gradual typing via contract verification (arXiv:2007.12630)](https://arxiv.org/pdf/2007.12630)
