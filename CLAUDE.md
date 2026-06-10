# Rune — Working Discipline

`goforge.dev/rune` — the kernel of a small, content-addressed, dependently typed
language. The end goal is provably correct infrastructure code; the v1 release
criterion is that every listing in the book *Specify & Verify* elaborates, checks,
and runs against this core, and the core contains nothing the book does not use.

This file is authoritative for how work proceeds. The reference designs live in
`ref_docs/` (`rune-v1-design.md`, `rune-v2-design.md`, `rune-v3-design.md`,
`rune-proof-cache-semantics.md`).

## Architecture

Three representations, one rule. The surface is **named**. The core is **locally
nameless**: bound variables are de Bruijn indices; references to top-level
definitions are **content hashes**, never names. The pretty-printer turns core back
into named surface for display. The only elaboration in Phase 0 is **name
resolution** (surface → core): binders to de Bruijn indices, free identifiers to
definition refs. There is no type elaboration in Phase 0.

```
surface/          lexer, parser, named AST, pretty-printer, name resolution  (names live here)
elaborate/        bidirectional checking: the surface elaborator (Phase-2 meta
                  insertion site) and the core checker (the cached judgment)
core/             locally-nameless Tm; glued Val domain; NbE eval/quote; conversion
                  with unfolding-tracking (the Machine and its dependency log)
store/            sealed bodies, the Unfold gateway, content-addressed map, SCC
                  hashing, and the proof cache (append-only certificate table)
equality/         the EQUALITY stratum interface (+ Phase-3 stub)
quantity/         the QUANTITY stratum interface (+ default 0/1/ω semiring)
codegen/          the CODEGEN stratum interface (Backend; erased IR -> target source)
harness/          property-test scaffolding — the gate from day one
internal/session  the shared parse -> resolve -> hash pipeline (file commands + REPL)
internal/repl     the `rune repl` read -> resolve -> show loop
cmd/rune/         the CLI: `rune fmt`, `rune hash`, `rune repl`
```

**`ref_docs/GRAMMAR.md` is authoritative for the surface language.** The lexer, parser,
named AST, name resolution, and pretty-printer conform to it; resolve any gap there
before touching code. As of v0.2.0 the surface is Elixir-style block syntax —
`fn (x : A) is e end` lambdas, `name : T is e end` definitions, and `seq … end`
sequencing that desugars to nested `let` — over the same unchanged core.

The core term encoding is the conventional Go AST encoding: a **sealed interface
with an unexported marker method, one small struct per constructor, matched by type
switch** (`core.Tm`, `core.Val`, `surface.Exp`). Committed to everywhere; do not mix
encodings.

### The strata are interfaces, not hardcoded choices

The three parts people fight type-theory wars over each sit behind a Go interface so
the right one lives in the right layer. v1 ships one implementation of each.

- **equality/** — equality type formers + future eval/quote/conversion hooks. The
  load-bearing interface of the roadmap: the store, hashing, semiring, codegen, and
  surface/nameless split are all orthogonal to it. v2 *extends* it (quotients); v3
  *swaps* it (two-level type theory).
- **quantity/** — the usage semiring; default instance is 0/1/ω. The 0-fragment is
  the erasure boundary. QTT is wired into binders in Phase 5, not before.
- **codegen/** — `Backend`: erased IR → target source. One plugin per target.

### Glued values

`core.Val` is a glued NbE domain: a neutral carries both its un-unfolded spine and a
lazy unfolding (a thunk). Forcing that thunk IS `store.Unfold`, and logs the unfolded
definition's hash into the Machine's write-only dependency set — the proof-cache
instrumentation rides on the laziness built for speed. Conversion compares spines
first and forces only on mismatch, so the fast path logs nothing.

## Phase map

- **Phase 0 (done):** lock the irreversibles + walking skeleton — term/value/hashing
  shapes, the strata interfaces, the body barrier, name resolution, pretty-printer,
  `rune fmt`/`rune hash`, the property harness.
- **v0.2.0 (done):** surface conforms to `ref_docs/GRAMMAR.md` (`fn … is … end`,
  `seq … end`); `rune repl` front-end over the existing pipeline. Still no eval,
  quote, conversion, or type checking — the core is unchanged.
- **Phase 1 (done):** the MLTT core with glued NbE — eval, quote (folded and
  δ-unfolding), conversion with the dependency log; the bidirectional core checker
  and surface elaborator (`elaborate/`); the append-only certificate table keyed
  `(defHash, ‖U‖)`; every definition type checked and cached on entry; the REPL's
  `runExpr` upgraded to elaborate+normalize+print. The four Phase-1 harness
  properties (preservation, conversion equivalence + congruence, Frame Lemma) are
  live.
- **Phase 2 (done):** metavariables (contextual, elaboration-scoped), Miller
  pattern unification (no pruning — parked), implicit Pi/lambda/application
  (`{x : A}`, `{e}`) and holes (`_`) in the surface; plicity (Icit) joined the
  core and its hash preimage (hashFormatVersion 0x02). Definitions are zonked,
  meta-free, before the store; the certificate layer is unchanged.
- **Phase 3 (done):** the observational equality stratum (Pujet–Tabareau) —
  proof-irrelevant `Prop`, `Eq` computing on type structure (funext is a
  REDUCTION: an equality of functions unfolds to the pointwise equality Pi, and
  `refl f` eta-expands), `cast` computing on its endpoint types and never
  inspecting its proof (conversion skips cast proofs and equates refls — UIP at
  the canonical level). equality.Observational implements core.EqStratum, wired
  into every Machine; full Eq-U decomposition (needs Sigma) is parked.
- **Phase 4 (done):** datatypes by ELIMINATORS — `data D : (params) -> U is C : … end`
  declares a former, constructors, and a generated eliminator (`DElim`), all
  bodiless (permanently neutral heads; the eliminator computes by the ι-rule in
  the evaluator, firing when the scrutinee forces to a saturated constructor,
  with induction hypotheses for recursive arguments). Strict positivity is
  checked at declaration; uniform parameters only (indexed families parked);
  coverage is by construction and TOTALITY IS BY CONSTRUCTION — the eliminator
  is the only recursion principle, so no termination checker exists or is
  needed. `subst` (Leibniz transport) joins the equality stratum so induction
  proofs over `Eq` go through; `Prop <: U` cumulativity admits Prop-valued
  motives. Declaration groups are content-addressed as a unit.
- **Phase 5 (done):** QTT is ON — binders carry a quantity from the 0/1/ω
  semiring (`(0 x : A)`, `(1 x : A)`; unannotated is ω; the annotation domain
  core.Qty lives in core and is hashed — format 0x03 — while the RULES live in
  quantity.Semiring). The elaborator does usage accounting: occurrences are
  recorded scaled by the current multiplicity (0 inside types and proofs),
  argument positions multiply by the Pi's quantity, and each binder exit
  compares usage against mult·declared. Quantities are part of a Pi's
  identity (conversion and unification check them). Lambda binders adopt the
  expected quantity; explicit annotations must match. Let binders are ω;
  metavariable spines are not usage-counted (recorded in PARKING-LOT.md).
  The 0-fragment is the erasure boundary Phase 7 reads.
- **Phase 6 (done):** the predicative universe hierarchy — `U` is U_0, `U1`…
  surface higher levels; U_i : U_{i+1}, Pi lands at the max of its parts'
  levels (Prop stays impredicative: any Pi into Prop is a Prop), cumulativity
  U_i <: U_j via Sub. Type-in-type is gone; levels are part of identity and
  the hash preimage (0x04). The one-time cache nuke the design priced in.
- **Phase 7 (done):** codegen — Erase lowers checked, meta-free core to the
  erased IR (untyped lambda calculus + globals + the unit token; types,
  proofs, casts, and transports become their computational payload or unit;
  no arity surgery — erased positions receive units), and the JS backend
  emits self-contained, dependency-free JavaScript: curried arrows, tagged
  constructor records, switch-dispatch eliminators with recursive IHs.
  `rune emit FILE [NAME]` prints the shadow; `rune run FILE NAME` executes it
  under node. The shadow rule holds: codegen reads bodies through the store
  and mutates only its own output.
- **Phase 8 / v1.0.0 (done):** the listings corpus (`listings/`, gated by
  `harness/listings_test.go`) — every chapter elaborates, checks, and runs.
- The equality stratum is then **extended** (v2 quotients) and a second equality
  stratum is **added** (v3 two-level type theory).

## Standing rules

1. **Parking lot.** Any improvement not required by the current goal goes to
   `PARKING-LOT.md` with a one-line rationale — not into code. Scope is capped by
   demonstrated need: add no feature with no consumer. Eventually the cap is the
   listings in *Specify & Verify*.
2. **Never hash modulo conversion.** A definition's identity is the Merkle hash of
   its elaborated core, computed structurally. Hashing MUST NEVER call eval,
   normalize, or a future conversion routine. Because the core is de Bruijn,
   alpha-equivalent terms are literally equal and hash equal.
3. **The body barrier.** A definition's type is public; its body is sealed inside
   `store/`. Bodies are reachable only through `store.Unfold`, the sole gateway and
   the proof-cache instrumentation point. This is a compile-time fact, not vigilance.
4. **Mutate the shadow, not the source.** Any future optimization IR is built on
   erased, throwaway codegen output, never on the immutable core/store.

## Engineering conventions

- Go standard library plus two recorded direct dependencies: the property-testing
  library `pgregory.net/rapid`, and `goforge.dev/blake3sum` for BLAKE3 content
  hashing (behind `core.Hash`; pulls `klauspost/cpuid` and `golang.org/x/sys` as
  indirect deps for its SIMD dispatch). No further dependency without recording why.
- The harness is the gate from day one: `parse ∘ pretty = id` and hash-invariance
  under alpha-renaming hold now; the Phase-1+ invariants (type preservation,
  conversion as equivalence + congruence, the proof-cache Frame Lemma) exist as
  documented, skipped property stubs that the mutation-testing layer will hunt.
- Conventional Commits for every commit.
