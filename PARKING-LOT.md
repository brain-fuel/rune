# Parking Lot

Improvements that were tempting during Phase 0 but are **not required** by the
current goal. Each is parked with a one-line rationale. Nothing here may be built
until a current listing (eventually a *Specify & Verify* listing) needs it.

## Deferred by phase plan (well-understood swaps)

- **Eq-of-types decomposition.** `Eq U (Pi …) (Pi …)` stays stuck rather than
  unfolding to a telescope of domain/codomain equalities — that unfolding needs
  Sigma types, which have no other consumer yet. Convertible endpoints are still
  provable by `refl`. (equality/)
- **Full definitional proof irrelevance.** Conversion equates refls and skips
  cast proofs; equating an arbitrary NEUTRAL proof with refl needs type-directed
  conversion. Parked until a listing needs it. (core/conv.go)
- **A second backend (Scheme/Go).** The Backend interface is exercised by one
  target (JS); a second lands when a deployment needs it.
- **Erased-argument elision.** Erasure keeps positions (units at call sites)
  instead of arity surgery; dropping 0-quantity arguments entirely is an
  optimization on the shadow with no current consumer.
- **Universe polymorphism / level variables.** Levels are concrete (U, U1…U9);
  level-polymorphic definitions (and large eliminations — eliminator motives
  target U_0/Prop) arrive only if a listing needs them.

## Parked in Phase 4

- **Indexed families (Vec, Fin).** Datatypes take uniform parameters only;
  indices need unification-based coverage machinery with no current listing.
- **Pattern-matching sugar.** Listings use eliminators directly; compiling
  match to eliminators is ergonomics with no consumer yet.
- **General recursive definitions + termination checking.** The eliminator is
  the only recursion principle, which makes totality structural. A termination
  checker arrives only if a listing cannot be written eliminator-style.
- **The empty type.** Declarations require at least one constructor; absurdity
  arrives when a listing needs `Empty`/`absurd`.

## Parked in Phase 5

- **Linearity through let.** Let binders are unrestricted; threading the
  bound value's usage by the binder's count is bookkeeping with no listing.
- **Usage counting of metavariable spines.** Contextual metas apply to bound
  variables without recording uses; QTT×unification interaction is parked
  until a listing exercises it.
- **Sub-usaging (1 ≤ ω).** Quantities compare exactly; admitting a linear
  function where an unrestricted one is expected is a one-line lattice change
  behind quantity.Semiring when wanted.

## Tempted in Phase 0, not built

- **Recursive-definition resolution.** `store.HashSCC` and positional `Placeholder`
  lay down SCC-as-unit hashing, but the resolver wires only the acyclic case; the
  CLI rejects recursive groups. Cyclic resolution lands WITH Phase-4 totality —
  unchecked recursion would let conversion diverge. Rationale: no recursive
  listing to exercise it yet, and no totality checker to make it safe.
- **Multiset / incremental dependency-set hashing.** The proof cache will
  canonicalize the dependency set by sort-then-hash; a commutative incremental
  combiner is a tempting optimization with cancellation/collision hazards. Parked
  behind the sign reading *here be invalidation*.
- **Conditional-fact cache keying.** Keying certificates on "`e` reduces to `v`"
  rather than on `e`'s full content would recover reuse across harmless body edits,
  but reintroduces a validation procedure (invalidation logic). Explicitly out of
  scope; the cache asserts only "this exact configuration of bodies checks."
- **Richer surface beyond the current calculus.** The surface is: variables, lambda
  (explicit and implicit binders), application (with `{e}` implicit override), Pi
  (explicit and implicit), holes (`_`), inline `let`, `seq`, `U`, and parenthesized
  ascription (see `ref_docs/GRAMMAR.md`). No data types or modules beyond a flat
  list of definitions until a phase needs them.
- **Pruning in pattern unification.** A meta solved against a term whose metas
  carry out-of-scope spine variables fails with a scope error instead of pruning
  the offending dependency. Add pruning only when a listing needs it.
- **Sigma types and pairs.** The v1 design's `Tm` includes `Sig`/`Pair`; they
  still have no consumer (their main customer is the parked Eq-of-types
  decomposition). They enter when a listing needs them.
- **Persistence of the store.** The content-addressed map is in-memory; on-disk
  persistence has no Phase-0 consumer.
- **Readable error spans / source positions in core.** Tokens carry offsets; the
  core does not thread positions yet. Deferred until the checker needs them.

## Tempted in v0.2.0 (surface grammar + REPL), not built

These are named in `ref_docs/GRAMMAR.md §9` and the v0.2.0 prompt. Each is ergonomics
or a later-phase feature with no current consumer.

- **Inline arrow lambda** (`fn x => body`). One lambda form (`fn … is … end`) is
  enough; the `=>` shorthand is parked.
- **Unannotated lambda parameters** (`fn x is …`). v0.2.0 binders are `(name : Type)`.
  Note the annotation is scope-checked then discarded — the Phase-0 core lambda is
  un-annotated — so it constrains nothing yet; a core home arrives with Phase-1 types.
- **Multi-binder Pi telescopes** (`(x : A) (y : B) -> C`). Chain with `->` for now.
- **Operators / infix, multi-clause definitions, pattern matching, literals.** Later
  phases; the surface has no notion of any of them.
- **REPL line editing: readline, history, completion.** The REPL reads with `bufio`
  only. No `golang.org/x/term` or readline dependency — the one-dependency posture
  holds; ergonomics wait.
- **REPL evaluation.** `:type`/`:t` is an honest forward-compat stub
  (`type checking arrives in Phase 1`); the default expression action is resolve +
  pretty-print, and the single dispatch point in `internal/repl` is marked as the
  Phase-1 insertion site. No fake eval before NbE exists.

## Parked in v2.0.0 (quotients)

- **Quotient effectiveness.** `Eq (Quot A R) (qin a) (qin b) -> R a b` is not
  provable: Eq stays STUCK at quotient types and identification is introduced
  by `qsound` (the Lean-style presentation). Making Eq COMPUTE to `R a b` at
  point-constructors (the full observational presentation) requires R to be an
  equivalence relation — premises on the former, or an equivalence-closure
  reduction — with no listing demanding it. Parked, not lost: it is an
  EvalEq/EvalCast extension behind the existing stratum interface.
- **Universe-polymorphic Quot.** The builtin telescope fixes the carrier at
  `U` (level 0), exactly as data formers do. Lifts to higher-level carriers
  arrive with universe polymorphism, if a listing needs them.
- **Dependent quotient elimination into U.** `qind` targets Prop (respect is
  free by irrelevance); `qlift` targets a non-dependent `B : U`. The full
  dependent eliminator into U owes a transported-respect premise whose
  statement needs heavier Eq machinery than any current chapter uses.
- **A truncation FORMER.** `‖A‖` is the Church encoding in impredicative Prop
  (listings ch08); a primitive `Trunc` with a definitional squash adds nothing
  a listing uses. If a chapter ever needs `Eq (Squash A) x y` to compute,
  revisit.
- **Quotient-inductive(-inductive) types and a user-facing HIT schema.**
  Named in the v2 design as the standing temptation; chapter-gated, and no
  chapter teaches them.
- **Quantity annotations on the builtin telescopes.** Quot/qin/qlift/qind
  binders are all ω; marking type/proof positions 0 would document erasure in
  the types at the cost of quantity-polymorphism questions with no consumer.

## Parked in v3.0.0 (the two-level layer)

- **§F: computational inner univalence.** The labelled frontier
  (ref_docs/rune-v3-design.md): a cubical INNER stratum — interval, Kan
  operations, Glue — isolated behind the fibrant universe, with the outer
  OTT core untouched. v3 ships `ua` postulated and `castU` computing through
  it; path induction OVER a ua-path does not compute, and making it compute
  is research, not a checkbox. Unexplored on a content-addressed substrate;
  plausibly the technical heart of a thesis.
- **A fibrant universe hierarchy.** `UF : U1` is the single inner universe;
  codes for UF itself (paths between universes, univalence one level up)
  need `UF1` and a lifting story. No chapter uses them.
- **Sigma codes in UF (`sigmaF`).** With Sigma absent from the outer core,
  the inner layer has no pair codes either; `Equiv` as a first-class fibrant
  type (rather than ua's four-component telescope) waits on it.
- **Inner higher inductive types.** The circle, suspensions, higher
  truncations — expressible in principle as postulated code groups in UF,
  shipped only when a chapter teaches one.
- **Erasure for the inner layer.** Tainted definitions are skipped at
  emission and a tainted main refuses (§F honesty: castU along ua would
  erase to the wrong function). A real erased meaning for inner transport —
  paths as data at runtime — arrives with computational univalence or not
  at all.
- **funext for inner paths (`pathF` over `piF`).** The outer Eq computes
  funext; the inner path type has no such rule yet. Chapter-gated.

## Parked in the ergonomics ladder (2026-06)

- **Compressed core numerals.** A literal expands to its unary succ-chain at
  parse time (GRAMMAR §5.5), capped at 4096. Literals beyond that need a core
  numeral representation (and a hash-format bump); no listing embeds one yet.
- **Deep-application evaluation is superlinear.** Machine.Eval on an n-deep
  constructor chain copies neutral spines per node (core.spineParts), making a
  4000-deep literal cost ~6s to check. Fix is spine sharing in the Machine;
  exposed by literals, not caused by them. Profile: Eval 70%, mallocgc 41%.
- **Recognizing arithmetic shapes in codegen.** The builtin-nat shadow (rung 6)
  compiles zero/succ/NatElim to BigInt and a loop, so user-defined + is O(m)
  per call, not O(1). Mapping canonical add/mul definitions to native BigInt
  +/* would need shape recognition or a `builtin natadd +` extension; wait for
  a listing (gcd on large inputs) to demand it.
- **Printer folding for `case`.** case/calc are input-only sugar; folding
  recognizable eliminator applications back into case on output is cosmetic.
- **Lazy IHs for record (non-nat) eliminators in the shadow.** emitElim still
  computes recursive eliminations eagerly in the switch; a case-with-unused-IH
  on a recursive RECORD type pays the same exponential the nat path used to.
  The nat path got $natD dispatch; generalize when a listing hits it.
- **`//` and `%` at Rat (numeric tower).** The flooring quotient on ℚ is
  well-defined on the quotient only by Euclidean uniqueness — the same
  general division-algorithm theorem ch11 deferred to the course-of-values
  chapter. ch13 ships `+ - * /` with full respect proofs; the floor pair's
  Rat semantics are pinned in ref_docs/rune-numeric-tower.md §2 and land
  with that chapter. Rationale: no sound qlift without the uniqueness proof.
