# Rune — Working Discipline

`goforge.dev/rune/v3` — the kernel of a small, content-addressed, dependently typed
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
- **v2.0.0 (done):** QUOTIENTS — the equality stratum extended without new core
  syntax. `Quot`, `qin`, `qsound`, `qlift`, `qind` are a BUILTIN GROUP of
  bodiless, content-addressed definitions (store/quot.go, registered ambiently
  by every session), exactly the data-declaration pattern: hashes derive from a
  placeholder-rewritten group digest, the heads are permanently neutral, and
  qlift/qind compute by quotient ι-rules in the evaluator
  (`qlift … (qin … a) ~> f a`, `qind … (qin … a) ~> h a` — core.QuotInfo, wired
  like core.DataInfo). Eq stays STUCK at quotient types; identification is
  introduced by `qsound` (Lean-style, UIP-safe; effectiveness parked). Prop's
  impredicativity already contains propositional truncation (listings ch08), so
  no Trunc former ships. Codegen: a quotient compiles to its carrier — qin is
  the identity, qlift applies, the proofs are units (codegen/js.go quotRuntime).
  No hash-format bump: no new core constructors exist. Listings ch06–ch08 are
  the gate. See ref_docs/rune-v2-implementation.md for the deltas from
  rune-v2-design.md and why.
- **v3.0.0 (done):** TWO-LEVEL TYPE THEORY — the second equality stratum,
  shipped as the third builtin group (store/fib.go, now TEN members): a
  Tarski-style fibrant universe (`UF : U1`, `El : UF -> U` decoding, `fib`
  embedding small outer types, `piF` closure) and TWO inner identities — the
  point-level `pathF`/`preflF`/`pathJ` (J computes on refl by ι,
  core.FibInfo/tryFibIota) and the type-level `pathU`/`ureflU`/`castU`. `ua` is
  NO LONGER a member here and NO LONGER postulated: univalence is DERIVED — `ua`
  is an ambient prelude definition (`internal/session/uaprelude.rune`,
  `ua A B f g s t := uaGlue …`, a Glue line), and `castU` along it computes the
  forward map through the genuine transp-over-Glue arm. There is no
  permanently-neutral `ua` head and no baked-in `castU`-over-`ua` fiat rule
  (both retired; the §F cubical machinery that supersedes them is below).
  Coexistence is structural: `pathU A B : U1` is data, not Prop, so UIP never
  applies to inner paths — the strict Eq can neither identify `ua not …` with
  `ureflU …` (distinct neutrals) nor refute it. EmitProgram skips inner-tainted
  definitions and refuses a tainted main; a `castU`-through-`ua` that COMPUTES
  to an outer value deploys via the B5 erase-the-normal-form rule (ch10
  `flipped` runs to `false`). `castU` is honest (R-UA Decision 2): `castU A B p x
  ~> transp (λi. pappU A B p i) ⊥ x` for ANY path, so `transportF` in the
  identity family `= castU` (`transportFId`). Path induction OVER a ua-path now
  COMPUTES too: `pathUJ` (ch58), the DERIVED dependent eliminator for `pathU`,
  reduces on `ureflU` and transports the motive along a genuine ua-path to a value
  (`pathUJOverUaComputes`), enabled by type-path η (`convPathUEta`). No
  ua-specific J rule, no postulate (ref_docs/rune-v3-implementation.md).
- **§F phase 1 (done):** the CUBICAL INTERVAL — the first brick of the frontier
  (make inner univalence COMPUTE by making the inner stratum cubical). Shipped
  as a fourth builtin group (store/interval.go, six members) on its own hash
  space (fib/ch09–ch10 hashes untouched): `I : U`, endpoints `i0`/`i1`, and the
  De Morgan algebra `ineg`/`imin`/`imax`. The connectives COMPUTE on endpoints
  by ι-rule (core.IntervalInfo/tryIntervalIota: `ineg i0 ~> i1`, `imin i1 j ~> j`,
  `imax i i1 ~> i1`, …) and stay stuck on a neutral interval term — there is no
  eliminator (you cannot case on I) and i0/i1 are distinct neutrals the strict
  Eq neither identifies nor refutes. `I` is morally a PRETYPE (no transport over
  it); the fibrancy/pretype split is a phase-3 concern (Kan). Interval value
  members are inner-tainted: they elaborate and check but do not deploy (paths
  get a runtime shadow at phase 5). Idempotence/De Morgan LAWS on neutrals are
  not definitional — they become paths once paths exist (phase 2). Listing ch17;
  ref_docs/rune-cubical-phase1.md.
- **§F phase 2 (done):** PATHS AS INTERVAL FUNCTIONS — inner paths get
  computational content. A fifth builtin group (store/path.go, two members) on
  its own hash space, built against the fibrant and interval groups (so their
  hashes stay fixed): `pabs : (A : UF) -> (f : I -> El A) -> El (pathF A (f i0)
  (f i1))` (path abstraction, a canonical neutral intro) and `papp : (A : UF) ->
  (x y : El A) -> El (pathF A x y) -> I -> El A` (application). `papp` COMPUTES
  (core.PathInfo/tryPathIota): β `papp … (pabs A f) i ~> f i`, refl-as-constant
  `papp … (preflF A x) i ~> x`, and the BOUNDARY `papp A x y p i0 ~> x` /
  `… p i1 ~> y` for ANY path p (even a neutral variable — the path type's
  defining property, definitional). This makes `sym` (reverse the interval),
  `ap`/cong (functoriality), and connection paths (compose `imin`/`imax` inside
  a `pabs`) all type-check on the nose — the endpoint reductions line the types
  up. Still NOT computing: `pathJ` over a non-refl path (needs Kan transport —
  phase 3), and the path LAWS as definitional equalities (`sym (sym p) = p`
  needs `ineg (ineg i) = i` on neutrals, which is a path, not a reduction).
  pabs/papp are inner-tainted (check, don't deploy; shadow at phase 5). Listing
  ch18; ref_docs/rune-cubical-phase1.md. Next: Kan operations (phase 3 — comp/
  hcomp/transp + the face lattice, where the research is).
- **§F phase 3 (done):** KAN OPERATIONS — the computational filling of fibrant
  types, landed as five builtin groups on their own hash spaces (fib/interval/
  path/ch09–ch18 hashes untouched), each gated in sequence:
  - **3a face lattice** (store/face.go, seven members): `F : U` (cofibrations),
    atomic constraints `ieq0`/`ieq1 : I -> F`, the lattice `fand`/`for`/`ftop`/
    `fbot`. Computes on endpoints by ι (core.FaceInfo/tryFaceIota): `ieq0 i0 ~>
    ftop`, `ieq0 i1 ~> fbot` (ieq1 dual), `fand ftop φ ~> φ`, `fand fbot _ ~>
    fbot`, `for fbot φ ~> φ`, `for ftop _ ~> ftop` (+symmetric); stuck on neutral
    faces. Listing ch19.
  - **3b partial elements & systems** (store/sys.go, five members): proof-
    irrelevant `holds : F -> Prop` with canonical intros `htop`/`hand`/`horl`/
    `horr` (permanently neutral, no ι of their own). A partial element is
    `holds φ -> El A`; proof-irrelevance discharges system overlap-agreement
    definitionally. Computation happens AT the Kan operations, which feed the
    system `htop` when the face is ⊤. Listing ch20.
  - **3c transp** (store/kan.go): `transp : (A : I -> UF) -> El (A i0) ->
    El (A i1)`. Ships REGULARITY (core.KanInfo/tryTransp): a type-line constant
    in i transports by the identity. Constancy is the genuine new NbE machinery —
    A is applied to a fresh sentinel interval point (kanFreshSentinel) and the
    result occurrence-scanned (mentionsRefVal, no level threading); absent ⇒
    constant ⇒ `transp A a0 ~> a0`. A varying line stays stuck. Listing ch21.
  - **3d hcomp** (store/kan.go): `hcomp : (A : UF) -> (φ : F) -> (I -> holds φ ->
    El A) -> El A -> El A`. Ships the TOTAL-system rule (tryHcomp): `hcomp A ⊤ u
    u0 ~> u i1 htop`. A proper face stays stuck. Listing ch22.
  - **3e comp** (store/kan.go): `comp : (A : I -> UF) -> (φ : F) -> ((i:I) ->
    holds φ -> El (A i)) -> El (A i0) -> El (A i1)`. Ships two honest endpoints
    (tryComp): TOTAL `comp A ⊤ u u0 ~> u i1 htop`, and DEGENERATE `comp (const A)
    ⊥ u u0 ~> u0` (empty system on a constant line ⇒ identity transport). Listing
    ch22.
  All Kan members are inner-tainted (check, don't deploy). No hash-format bump
  (no new core constructor). ref_docs/rune-cubical-phase1.md.
- **§F phase 3 — structural interior (done, the `piF` slice):** the Kan
  operations now recurse on a type's HEAD FORMER, not just the endpoints. The
  enabler: reverse lookups `FibHash(role)` / `KanHash(role)` (core.FibInfo /
  core.KanInfo, mirroring IntervalHash/SysHash) so an ι-rule can CONSTRUCT
  `El`/`transp`/`hcomp` sub-terms. Helper `fibFormer` probes a type-line at the
  freshness sentinel and reads its former. Three `transpFill`-free rules ship
  (core/eval.go; listing ch23):
  - `transp (λi. piF D (Fam i)) f ~> λx. transp (λi. Fam i x) (f x)` — CONSTANT
    domain (the constancy of D is what removes the argument fill).
  - `hcomp (piF P Fam) φ u u0 ~> λx. hcomp (Fam x) φ (λi h. u i h x) (u0 x)` —
    homogeneous, any face.
  - `comp (λi. piF P (Fam i)) φ u u0 ~> λx. comp (λi. Fam i x) φ (λi h. u i h x)
    (u0 x)` — constant domain, any face.
  Each fires only AFTER the existing endpoint rules short-circuit, and is pinned
  by an η-sensitive refl fact (a stuck top-level transport η-differs from a
  lambda). Substrate hardened: rapid properties that every CLOSED interval /
  face term normalizes to an endpoint / ⊤·⊥ (harness/cubical_props_test.go), and
  transp constancy-boundary pins — sees through a β-redex, stays stuck on a
  varying `pathF` line (internal/session/structural_test.go).
  STILL the labelled remainder (honest-stuck): `transpFill` (transport i→j) and
  with it varying-domain `piF`, `transp`/`comp` over `pathF` (base composition),
  `hcomp` on a proper face for non-`piF` formers; pathJ-via-comp on a general
  path; transport through `ua` (phase 4). FINDING: system overlap-agreement is
  NOT definitional here — this engine's proof irrelevance covers Eq/refl/cast
  proofs (UIP at the canonical level), not arbitrary `Prop` inhabitants in
  conversion, so `part h1 ≡ part h2` for `holds φ` proofs does not hold by
  conversion (PARKING-LOT.md). Next: `transpFill`, then phase 4 (ua / Glue).
- **Wootz roadmap (in progress) — see `ref_docs/wootz/` + the plan.** The book is
  no longer the scope cap; the telos is a mature verified language (full cubical
  inner univalence; portable multi-backend codegen + proof-carrying FFI;
  tiered-verified stdlib + OTP; verified process-calculus distributed algebra).
  19 design docs in `ref_docs/wootz/` (`00-INDEX.md`). Landed so far:
  - **A1 / R-FILL** — `transpG` generalized transport (4th Kan member) +
    `transpFill` + the varying-domain `piF` transport rule (core/eval.go,
    store/kan.go; ch23). The cubical-interior keystone.
  - **C1 / R-SUM (core)** — dependent pairs Σ in the OUTER core: Sig/Pair/Fst/Snd
    with β + **definitional η** (the one Thompson-sensitive conv.go edit),
    hash-format bump 0x04→0x05 (core/*; core/sigma_test.go). Surface sugar +
    elaboration + typeclasses (C1b/C2) are the follow-up; Σ is OUTER (runs).
  - **A5 / R-SIGMA** — inner `sigmaF`/`pairF`/`fstF`/`sndF` builtin group with
    computing projections, `El (sigmaF …)` neutral (store/sigma.go; ch24). The
    Equiv→Glue prerequisite.
  - **A5a** — transp/hcomp/comp over a NON-DEPENDENT inner-Σ product reduce
    componentwise (m.transpSigma/hcompSigma/compSigma; dependent families are the
    A5b remainder, stuck). Postfix `.1`/`.2` projection sugar for outer Σ.
  - **C1b / R-SUM (surface)** — surface Σ syntax `Sig`/`Pair`/`Fst`/`Snd` (the
    family is a function; collision-free capitalised keywords) + full elaborate
    pipeline (resolve/Infer/InferCore/Unify with VSig + VPair-η/Zonk/MetaFree/
    shift/pretty); ch26. Makes C1 user-facing.
  - **C5a / R-COIND** — final coalgebras `Nu`/`out`/`unfold`; `out` computes via
    structural `fmapF` (functor action by code-recursion over fib/piF/sigmaF +
    the recursive position, a coindXSentinel probe); store/coind.go, ch25.
  - **C3 / R-EFFECT** — IO monad as a bodiless group `World`/`IO`/`pureIO`/
    `bindIO` (types check; runtime IForeign is the follow-up); store/io.go, ch28.
  - **A2 (increment)** — `hcomp A ⊥ u u0 ~> u0` (empty-system rule).
  - **C2 / C-ABS** — typeclasses as type families; `instance` defs register in a
    session table keyed (class-head, arg-head); a class-constrained `{d : C T}`
    implicit is resolved by instance search at the use site (zero new core; the
    inserted dictionary is kernel-re-validated). ch31. Explicit `{d}` is the floor.
  - **C2b** — PARAMETRIC instance search: an instance may be `{A} -> {Self A} ->
    Self (List A)`; resolution recurses — unify the instance codomain to fix the
    parameters, then discharge each constraint premise by nested search, building
    the dictionary bottom-up (elaborate/resolveClass; classKeyOfType peels leading
    Pis to key off the codomain). Zero core, elaborator-only; overlap/priority +
    cycle detection stay parked. ch200. The Rule-5 REWRITE MANDATE sweep is a no-op
    on the current corpus (the only instance is ch31's base `selfBool` — nothing
    derivable to migrate). This CLOSES Track C (C1–C7 all landed).
  - **C3 / R-EFFECT** — IO monad group (types).
  - **C4 / R-PART** — general recursion via `partial` definitions: the head is
    permanently neutral in eval/conversion (the firewall — the normalizer cannot
    diverge, decidable checking preserved), the body runs only through codegen.
    Self-reference hashes via Placeholder(0)→content-hash (the datatype-group
    pattern); store.AddPartial + Machine.Pt + Elaborator.SelfHash/ElabPartialDef.
    Segregated: the total fragment stays sound. ch39 RUNS (countdown 3 = zero).
  - **C5a / R-COIND** — final coalgebras Nu/out/unfold; `out` computes via
    structural `fmapF`. ch25.
  - **C5b / R-COIND** — the GUARDED-RECURSION modality `Later`/`next`/`dfix`
    (store/guard.go, own hash space). Heads permanently neutral (no eval
    divergence); the guarded-fixpoint equation `dfix A f ≡ f (next A (dfix A f))`
    is a bounded one-step CONVERSION (`convDfix`); productivity by typing (the ▹
    guard), no checker, no sizes. Unblocked by M2 (paths compute). `lmap`/`lap`
    (the ▹ functor map + applicative ⊛) make ▹ a VERIFIED LAWFUL applicative
    (ch62 `lmapAsLap`/`lapInterchange`, refl on `next` intros). **CLOCKS +
    `force` LANDED** (CLOCKS-DESIGN steps 1–3): the guard group is now
    CLOCK-EXPLICIT (`Later : Clock -> UF -> UF`, eight members) — `Clock : U` a
    pretype (no eliminator, like `I`), `k0 : Clock`, `∀κ. A` an ordinary Pi over
    `Clock`, and the keystone `force : (∀κ. ▹κ A) -> ∀κ. A` the SOLE sound
    `▹`-elimination, computing by `force A (λκ. next κ A x) ~> λκ. x`
    (tryForceIota; fires only on a `next` on the bound clock with a clock-closed
    payload, neutral otherwise — C4 firewall intact). Clock-indexing makes the
    inconsistent `Later A -> A` UNTYPABLE, so soundness needs no freshness
    checker (the `Λκ` pass is a completeness-only tail). ch68; ch62 re-elaborates
    clock-indexed. **GUARDED-RECURSIVE TYPES `gfix` LANDED** (9th guard member,
    ch69): `gfix : (k:Clock) -> (UF -> UF) -> UF`, the TYPE-level dual of `dfix`,
    builds self-referential codes like the guarded stream `Str^g κ A = gfix κ
    (λX. A × ▹κ X)`; head neutral in eval, the equation `gfix k F ≡ F (gfix k F)`
    a bounded `convGfix` with a progress guard (unguarded `λX. X` cannot loop the
    checker). ch69 builds the guarded stream + `consG`/`headG`/`tailG` (compute
    through the fold) + fmapF-over-Later (`sfmap` lifts under ▹κ via `lmap`). The
    CONVERSE bisimilar⟹path (ch66 is the forward half) substrate is complete; its
    assembly is in `ref_docs/wootz/CLOCK-IRRELEVANCE-DESIGN.md`. **The DEPENDENT
    force `forceD` LANDED** (10th guard member, ch69): `forceD : (A:Clock->UF) ->
    (∀κ.▹κ (A κ)) -> ∀κ.(A κ)` cashes a clock-VARYING delayed value (a guarded
    stream's tail `▹κ (gStr κ A)` mentions κ, so plain `force` can't); sound by
    clock-inertness (`Clock` has no eliminator). ch69 `gtailG`/`gtailCons` cash a
    guarded stream's delayed tail — the E2-converse observation. **The INDEXED
    fixpoint `gfixF` LANDED** (11th guard member, ch69): `gfixF : (k:Clock) ->
    (D:UF) -> ((El D -> UF) -> (El D -> UF)) -> (El D -> UF)`, the `Bisim` former —
    recurses a FAMILY so the recursive occurrence can sit at a DIFFERENT index
    (bisimilarity recurses on the tails). `gfixF k D Φ d ≡ Φ (gfixF k D Φ) d`, a
    bounded convGfixF with a progress guard. **The universe-level ▹κ application
    `laterApp` LANDED** (12th guard member, ch69): `laterApp : (k:Clock) ->
    (A:UF) -> (El A -> UF) -> El (Later k A) -> UF` — the companion to `lmap` for
    a UF-VALUED family (bisimilarity's recursive occurrence is a relation, so it
    needs `laterApp k A f (next k A x) ~> Later k (f x)` to sit under ▹κ); ch69
    `gRelStep` is the abstract Bisim shape, unfolding by refl. **The CONCRETE
    stream `gBisim` is now BUILT** (ch69): `gfixF` over `Dpair = gStr × gStr`,
    `gBisim d = (headG (fst d) = headG (snd d)) × ▹κ (gBisim (tail-pair d))` (head-
    equality a `pathF`, recursive occurrence under ▹κ via `laterApp`, `step` zips
    the two ▹κ-tails via `lap`∘`lmap`); `gBisimUnfold` proves its coinductive
    characterisation by refl. **The DEPENDENT guarded application `lapD` LANDED**
    (13th guard member, ch69): `lapD : … -> El (Later k (piF A B)) -> (la : El
    (Later k A)) -> El (laterApp k A B la)`, ι `lapD k A B (next k (piF A B) f)
    (next k A x) ~> next k (B x) (f x)` — applies a delayed DEPENDENT function (its
    result type via `laterApp`), the path-assembly's recursion combinator. The
    coinductive destructors `bisimHead`/`bisimTail` are landed (ch69). FINDING:
    the path-assembly endpoint coherence is NOT definitional (the lmap-projected
    delayed tail-path stays neutral, so `consG (headG s)(…) ≢ s`), so the converse
    needs Kan endpoint-REPAIR (`hcomp` over `gStr`) — a cubical construction. The
    REPAIR ENABLER is landed (core/eval.go): `hcomp` over a `gfix`-typed value now
    reduces — `unfoldGfixType` unfolds `gfix k F ≡ F (gfix k F)` (a sigmaF) so the
    structural-Σ Kan rule fires (`hcomp (gStr) φ u u0 ~> pairF …`,
    TestHcompOverGfixUnfolds); ch69 `hcompHeadG`/`hcompTailG` verify the repair is
    componentwise (head/tail commute with hcomp, refl). The SLOT-COHERENCE (the
    `laterApp k Dpair gBisim (gstepPair d)` tail slot vs the `lapD`-over-gStr
    recursive call, two `laterApp` neutrals agreeing only on a `next`) is now
    DISCHARGED ON THE PRODUCTIVE CASE (`bisimTailSlotCons`, ch69): on a pair of
    CONS-built streams with `next`-guarded tails — the shape productive corecursion
    emits — four ι-rules chain (tailGCons ▸ lmapNext/lapNext ▸ laterAppNext) so the
    slot `≡ Later k (gBisim (pairF ra rb))` BY REFL, the delayed bisimilarity of the
    literal tail pair. So the mismatch is a pure neutral-vs-neutral standoff resolved
    wherever corecursion is productive; the general neutral case is the propositional
    residue the repair path carries. The PRODUCTIVE-CASE PATH-ASSEMBLY STEP is also
    landed (`congConsG` + `lmapPappNextI0`, ch69): the converse's corecursion BODY —
    a `consG`-congruence building `pathF (gStr) (consG a (next ra)) (consG b (next rb))`
    from a head-path + a `next`-delayed tail-path, one `pabs` consing `papp hp i` onto
    `lmap (λq. papp q i) (next k _ p)`; endpoints DEFINITIONAL (no Kan repair) because
    `lmapPappNextI0` pins `lmap (λq. papp q i0) (next k _ p) ~> next k _ ra` by refl, so
    pabs's forced endpoints match the declared type on the nose. The STREAM-η
    coherence is landed too (`streamEta`, ch69): `s ≡ consG (headG s)(tailG s)` BY
    REFL via inner-Σ η (convSigmaEta) — gStr unfolds to `sigmaF A (λ_. ▹κ gStr)`, the
    η-expansion law the converse rewrites a general endpoint `Sfst d`/`Ssnd d` along
    to reach cons-form (the bridge to `congConsG`). The bridge half `split : Str A ->
    ∀κ. gStr` is CONSTRUCTED (`splitFn`, ch69): a `dfix` at the function type
    `piF (Str A)(λ_. gStr k A)` (El(piF) = real Pi) consing `head s` onto
    `lap self (next (tail s))`, productive by the ▹κ guard. FINDING — the
    DFIX-OBSERVATION WALL: no computing coherence about a dfix-corecursor is provable,
    because `Eq` at a Pi funext-reduces to pointwise `splitFn s`, a dfix buried under
    APPLICATION, which convDfix won't unfold (fires only at comparison top — the
    `repeatG` wall). So the path AND bridge coherences must be obtained PRODUCTIVELY
    (observe via global `gheadG`/`gtailG`, the forceD-cash, which DOES compute, cf.
    `gtailCons`), not by dfix-observation. Remaining: wire `congConsG` into the guarded
    corecursion (`bisimHead`/`bisimTail` ▸ streamEta-rewrite) + `forceD`-cash to the
    global `pathF (Str A)` presented productively + hcomp-repair the neutral residue.
    **The COMPUTING bridge half `glue : (∀κ. gStr κ A) -> Str A` is LANDED** (ch69):
    built Nu-side via the anamorphism `unfold` (carrier `GStrAll A = piF (fib Clock)(λc.
    gStr c A)`, coalgebra = the productive `gheadG`/`gtailG` observations), so its head/
    tail observations COMPUTE — `glueHead`/`glueTail` hold by refl, dodging the dfix wall
    that made `split` opaque. **The step-2 ROUND-TRIP on cons-form is LANDED** (ch69):
    `glueConsHead`/`glueConsTail` (refl) prove `glue` is a computing stream homomorphism
    on cons-form global streams (preserves head, steps tail; forceD cashes the delayed
    rest under the binder). EMPIRICALLY CONFIRMED the through-`split` round-trip stays
    dfix-opaque (`head (glue (λκ. split κ s)) ≢ head s`), so the full `Str ≃ ∀κ. gStr`
    must assemble from the cons-form round-trip via streamEta + productive corecursion,
    never by observing `split`. **The CONVERSE recursion BODY is REALIZED on the
    productive case** (ch69 `bisimStepPath`): for a cons-built pair it consumes a real
    `gBisim` witness, pulls the head-path via `bisimHead` (computes to `pathF A a b` by
    headGCons), and conses it onto a present tail-path via `congConsG` — output `pathF
    (gStr)(Sfst d)(Ssnd d)` on the nose. SHARP FINDING: the body is definitional only
    for a PRESENT tail-path; the closed recursion supplies it via `dfix`'s self (a
    NEUTRAL ▹κ, not a `next`), so `lmap (λq.papp q i)` stays stuck and endpoints need
    `hcomp`-repair even on cons-form — the precise reason the repair is unavoidable.
    **The ENDPOINT-REPAIR TOOL is LANDED** (ch69 `repairEndpointsG` via `ptransG`/
    `pathJ`): slides a gStr path's neutral approximate endpoints to the real ones by
    double-composition `sx · q · sy`. REDUCTION: the converse closes iff the two
    correction paths exist; each decomposes (consG-cong + streamEta) into a head-
    correction (= `bisimHead`, in hand) and a DELAYED TAIL-CORRECTION `pathF (Later k
    gStr) (tailG (Sfst d)) (neutral lmap-self)` — the entire remaining E2 residue.
    **▹κ preserves paths** (ch69 `laterPathNext`, refl): `next` of a path is a path of
    `next`s. PROBED: `head (hcomp (Str A) φ u u0)` does NOT reduce — `Nu` has no
    structural Kan rule (unlike `gStr` via `unfoldGfixType`); the coinductive `Nu F` is
    not definitionally its unfolding (`out` is an observation, not a reduction). So the
    global endpoint-repair route needs a NEW KERNEL ENABLER — **Kan-over-Nu** (hcomp/
    transp over a coinductive type computing componentwise through `out`/`unfold`, the
    global-side dual of hcomp-over-gfix); the most tractable unblock for the closed
    converse. **KAN-OVER-Nu is now LANDED** (core/eval.go `tryCoindIota`; ch69
    `hcompHeadStr`/`hcompTailStr` refl; session `TestOutCommutesWithHcompOverNu`):
    `out` commutes with hcomp over a coinductive type — `out F (hcomp (Nu F) φ u u0)
    ~> hcomp (F (Nu F)) φ (out∘u)(out u0)`. Since `Nu` is negative (not its unfolding;
    `out` is an observation), the rule rides the observation: the composed stream stays
    neutral but observing it fires the rule, lands in the structural `F (Nu F)` (sigmaF),
    and the Σ Kan rule splits it — so `head (hcomp (Str A) …) ≡ hcomp A φ (heads)(head
    u0)` and the tail dual hold BY REFL. Sound (defining eqn of coinductive hcomp),
    bounded (fires once per `out`). The global endpoint-repair route is OPEN; next is
    assembling the global converse (approximate Str-path + `hcomp (Str A)` endpoint
    repair). **The GLOBAL repair tool + productive global converse are LANDED** (ch69):
    `ptransStr`/`repairEndpointsStr` (Str-level path-composition + endpoint-slide via
    `pathJ`, duals of `ptransG`/`repairEndpointsG`) and `unfoldSeedPath` — path-connected
    SEEDS give path-connected `unfold`-streams (`pabs (λi. unfold StreamF S c (papp ps
    i))`, endpoints definitional, no repair), the Str dual of `congConsG`. transp over a
    constant Str line = id (regularity). REMAINING (both routes converge): the
    interpolant for arbitrary `s`/`t` — a StreamF coalgebra driven by the bisimilarity —
    then `repairEndpointsStr`; the repair + observation machinery beneath it is all
    landed. **FINAL-COALGEBRA η is now LANDED** (core/eval.go `tryUnfoldEta`; ch69
    `nuEta` refl; session `TestUnfoldEtaFinalCoalgebra`): `unfold F (Nu F) (out F) s ~>
    s` — the coinductive uniqueness law (anamorphism into the final coalgebra at carrier
    `Nu F` with coalgebra `out` = id), dual to Kan-over-Nu, the negative-type η for
    coinductives. Sound + confluent with the `out`-ι (via this η on the recursive
    position + Σ-η). With Kan-over-Nu + Nu-η + the repair tools all landed, the
    coinductive cubical substrate for the E2 converse is COMPLETE; the remaining work is
    the interpolant-coalgebra construction itself. **STREAM PREPEND `consStr` LANDED**
    (ch69): prepend on `Str A` via `unfold` over carrier `Σ A (Str A)` (coalgebra
    `(a,s) ↦ (a, out s)`); head observations compute (`headConsStr`/`headTailConsStr`,
    refl). CRUCIAL FINDING: the cons-η `tail (consStr h ts) ≡ ts` (= rebuild-from-
    observations) is NOT definitional — the tail is `ts` rebuilt from its heads (carrier
    ≠ Nu, coalg ≠ out, Nu-η does not fire), only BISIMILAR. This IS the bisimilarity-is-
    path coherence the converse needs, so the converse cannot close by any FINITE
    definitional construction; it needs a COINDUCTIVE FILLING — the per-clock guarded
    route (∀κ + `forceD`-cash, the original design, where the guard makes corecursion
    productive) is the indicated path. The global substrate (Kan-over-Nu, Nu-η, repair
    tools, consStr) is complete and independently valuable. **CONVERSE CLOSED MODULO
    cons-η** (ch69 `converseFromConsEta` + `symStr`): a verified conditional theorem —
    given the head-path + tail-path (the converse of headPath/tailPath) AND the cons-η
    path `consStr (head s)(tail s) ≡ s`, the path `s ≡ t` is assembled (cons the
    observations, `repairEndpointsStr` re-aims endpoints). So bisimilarity-is-path holds
    the moment cons-η is supplied — every other piece is landed; cons-η is the single
    missing coinductive principle. Investigated relaxing convDfix to fire on applied
    dfix (sound + termination-safe) but it's intercepted by sigmaF-η projecting the dfix
    under fstF/sndF before convDfix sees it — reverted (no consumer). CONCLUSION: closing
    E2 is a DESIGN DECISION (adopt cons-η/bisimilarity-is-path as a contained derived-
    axiom — substrate makes the rest compute — or build comp/transp-over-Nu coinductive
    filling), not an incremental definitional lemma. **E2 CONVERSE NOW CLOSED** (computing,
    no postulate): added the one-level coinductive constructor `nuCons : (F) -> El (F (Nu
    F)) -> El (Nu F)` as the 4th coind builtin (store/coind.go; coind group 3→4, contained
    rehash, no hash-format bump) with β `out F (nuCons F x) ~> x` (eval) + the coinductive
    η `nuCons F y ≡ t ⟺ y ≡ out F t` (conversion `convNuConsEta`, dual of convSigmaEta,
    bounded). `Nu` lacked a one-level constructor — the deep `unfold` rebuilds observations
    (tail only bisimilar), which is why cons-η failed; `nuCons` reconstructs from ONE layer.
    With `consStr := nuCons (pairF h ts)`: head + FULL tail compute (headConsStr/tailConsStr
    refl) and the coinductive η `consStr (head s)(tail s) ≡ s` is REFL (consEtaStr). So
    `bisimToPathStr : (head s ≡ head t) -> (tail s ≡ tail t) -> pathF (Str A) s t` — the
    E2 converse — typechecks: bisimilarity ⟹ path, COMPUTING. With ch66's forward
    headPath/tailPath the correspondence is COMPLETE. Pinned: ch69 bisimToPathStr/
    consEtaStr/tailConsStr; session TestNuConsBetaEta. **E3 trace machinery LANDED**
    (ch69): `traceOf` (an LTS coalgebra `c : S -> A × S` corecurses a state's infinite
    observation stream via `unfold`) + `bisimTrace` — two states whose traces have a
    head-observation path and a tail-trace path are TRACE-EQUAL (`pathF (Str A)`), from
    `bisimToPathStr`. The bisimulation principle for any LTS; a process calculus
    instantiates A := `fib (Option Proc)`, S := `fib Proc`, c := the `reduce`-coalgebra
    (outer Proc embedded via `fib`). **E3 GROUNDED CONCRETELY** (ch70): `traceProc` wires
    `traceOf` onto the real `reduce` coalgebra — a process's behaviour is the stream of its
    reduction observations, and they COMPUTE: `head (traceProc (act tau halt)) ≡ some halt`
    and `head (tail …) ≡ none` (the behaviour `some halt, none, none, …` matches the
    operational semantics) — `traceHeadAct`/`traceHeadTailAct`, refl. The outer `Proc`
    crosses into the fibrant world via `fib`. ch69 `traceOutId` (canonical `out` coalgebra
    = identity trace, via Nu-η) grounds the abstract case too. **PROCESS BISIMULATION
    CLOSED** (ch70 `procBisim`): two DISTINCT processes `act tau halt` / `act recv halt`
    (differ in action label, but `reduce` observes the continuation not the label, so both
    behave `some halt, none, …`) are proven PATH-EQUAL as behaviours via `bisimTrace`
    (head/tail agreements supplied as `preflF` since each side computes to the common
    value). The E2/E3 currency on a real process calculus: observationally-equivalent
    processes are propositionally equal, computing, no postulate (NOT refl — distinct
    seeds, genuinely coinductive). **Parametric + structural process bisimulations**
    (ch70): `procActLabelIrrelevant` — for ALL `a a' p`, `act a p ≃ act a' p` (the action
    LABEL is unobservable; `reduce` drops it), a universally-quantified equivalence; and
    `procParPrefix` — `act tau (par halt halt) ≃ par halt (act tau halt)` (parallel
    composition with a stuck peer is behaviourally a prefix), a process-algebra law on
    structurally dissimilar terms. Both via `bisimTrace`, computing. ch62/ch66/ch68/ch69/
    ch70. (R-NUM/C7 is LANDED, not deferred — `NatLit{*big.Int}` compressed core numeral
    + opt-in kernel-accel arithmetic + hash bump 0x05→0x06 + `builtin bin` retired, all green.
    The NatLit↔succ-chain defeq is handled because `nat` is a user `builtin` binding: NatLit
    carries the active Zero/Succ ctor hashes and is DEFINITIONALLY `succ^n zero` — Force peels
    one layer on demand, QuoteUnfold short-circuits the canonical literal so a closed result
    stays compact (`mul 1000000 1000000` reads back without materialising a succ-chain), and
    the accel ι fires only when both args are literals so open-term reasoning is unchanged.
    Accel registration is gated by a differential soundness check (the def's unfolded peeling
    must agree with the bigint op). Only the n<2^63 int64 fast-lane is parked, no consumer.)
  **M0 VERTICAL SLICE — a DEPLOYED distributed protocol** (ch71): a sender ‖ receiver
  rendezvous (`protocol = par (out halt)(inp halt)`) that type-checks, CARRIES its
  certificate (`answerCorrect`/`finalCorrect`, refl), and DEPLOYS + RUNS on a real
  backend — gated in the emit-and-execute suite under node: `answer = reduce protocol`
  → `some (par halt halt)` (one rendezvous), and `final = run3 protocol` → `par halt
  halt` (run to completion, then quiescent). The spirit gate (Lambert's deployed
  artifact + Savage's worked example) satisfied for the distributed track on a real
  backend, not just proven. **Cross-backend distributed parity** (ch71 in
  backend_conformance): the protocol's `answer`/`final` observe byte-identical results on
  ALL backends — py, go, rust, BEAM (erl, the natural distributed target), AND JVM 25
  (an asdf temurin-25 is present, so `findJava25` resolves it). **Replicated counter
  (G-Counter CRDT)** (ch72): two replicas count locally, `merge` = pointwise `max` (the
  CRDT join), `value` = sum; CONVERGENCE proven — `maxComm` (double induction) ⊢
  `mergeComm` (merge is commutative → replicas converge regardless of gossip order). The
  scenario `converged = value (merge replicaA replicaB)` (counts 2 and 1) DEPLOYS + RUNS
  to 3 on every backend, certified by `convergedCorrect` (refl). Verified eventual
  consistency, deployed cross-backend — the M0 spec's named "replicated counter with a
  consensus core." **CvRDT correctness PROVEN** (ch72): merge forms a JOIN-SEMILATTICE —
  `mergeComm` (commutative) + `mergeIdem` (idempotent) + `mergeAssoc` (associative), each
  from the corresponding `max` law (`maxComm`/`maxIdem`/`maxAssoc`, the last by triple
  induction) via GC-congruence. That is exactly the state-based CRDT (CvRDT) convergence
  criterion — the replicated counter is a VERIFIED convergent replicated data type, not
  just a deployed one.
  - **D1 numeric tower + order + semiring** — `addAssoc` + `addComm` of Nat
    addition proven by induction (ch63, the CommutativeMonoid laws for (Nat,+,0));
    decidable order `lte` with `lteRefl`/`lteZero`/`lteSucc` + `min` (ch65); and
    the SEMIRING step `mul` + `mulZeroR` + right-distributivity `mulDistR` (ch63),
    certifying (Nat,+,*,0,1) a commutative semiring.
  - **C6** — modules/namespaces: `module M is … end` qualifies inner names as
    `M.name` (lexer absorbs `.segment`; `.1`/`.2` stay projections); sugar over
    qualified-named defs, zero core. ch37.
  - **D1/D2 stdlib (proven tier)** — algebra-records (ch27), Option (ch29),
    Result (ch30), induction proofs `add n zero = n` (ch35), verified Lists with
    the length-homomorphism theorem (ch36), and a **verified list ALGEBRA (ch73)**:
    append is a monoid (`appendAssoc` + `appendNilR`), reverse is an involutive
    anti-homomorphism (`revAppend`: reverse distributes contravariantly over append;
    `revInvol`: `reverse (reverse xs) ≡ xs`) — all by induction over the ambient `Eq`.
  - **§F suspension β COMPLETE (ch53)** — the HIT path-constructor computation rule
    `suspRecMerid` (`suspRec` maps `merid a` to the chosen path `m a`, pointwise along
    the interval, refl) + the pole rules; the dim-1 path constructor genuinely reduces,
    the substrate for spheres Sⁿ⁺¹ = Susp Sⁿ and dim-2 HITs.
  - **DIM-2 HIT — S² = Susp S¹ (ch74)** — a genuine 2-dimensional higher inductive type.
    A verified map `mapS2 : S² → S¹` sends both poles to `base` and each meridian to the
    circle's loop via `loopFam` (a CIRCLE elimination into the path type `base = base`,
    so the loop becomes a 2-cell). The dim-2 recursor β computes: `mapS2 north ≡ base`
    and `mapS2 (papp (merid c) i) ≡ papp (loopFam c) i` (refl, parametric in the circle
    point c — holds along S¹'s loop too). Spheres build by iterated suspension and
    eliminate two-dimensionally.
  - **Verified finite Map (ch75)** — an association-list Map over Nat keys with the
    correctness law `lookup k (insert k v m) ≡ some v` (M6 stdlib). Decidable key
    equality `eqNat` + its reflexivity `eqNatRefl` carry the proof (subst on the BoolElim
    guard).
  - **`ap` is a FUNCTOR (ch76)** — the action of a function on a path preserves identity
    (`ap id p ≡ p`), composition (`ap g (ap f p) ≡ ap (g∘f) p`), and constants (`ap (λ_.c)
    p ≡ refl c`) — all by `refl` (papp/pabs β + path η `convPathEta`). The groupoid action
    of functions on paths, computing (telos-1).
  - **List is a FUNCTOR (ch77)** — `map` preserves identity (`map id xs ≡ xs`) and
    composition (`map g (map f xs) ≡ map (g∘f) xs`, the map/map fusion law), by induction.
    Completes the verified list algebra (length ch36 + append-monoid/reverse ch73 +
    functor ch77).
  - **Option is a MONAD (ch78)** — the three monad laws: left identity `bind (ret a) f ≡
    f a`, right identity `bind m ret ≡ m`, associativity `bind (bind m f) g ≡ bind m (λx.
    bind (f x) g)` — by case analysis (OptionElim), refl branches.
  - **Path groupoid (ch79, §F/telos-1)** — `pcomp` (path composition by `pathJ`) + `symP`
    (reversal by `ineg`); the RIGHT-unit law `pcomp p refl ≡ p` computes by refl (pathJ on
    preflF), with `pcompReflRefl`/`symReflP`. The left-unit + inverse + associativity laws
    are genuine 2-cells (hcomp fillers) — verified non-definitional, recorded as the next
    §F frontier (the path-composition Kan filler).
  - **E1 distributed** — a process calculus: parallel composition + interleaving
    reduction + channel rendezvous, all by a total functional small-step. ch33,
    ch38. Bisimulation (needs C5b guarded recursion) is the R-CALC remainder.
  - **R-CALC FAULT LTS `{CRASH, LOSS, DETECT}` (ch206) — the keystone.** The
    calculus is now a LABELLED, fault-aware LTS. rune has no indexed families, so
    (per the R-CALC design) the LTS is a COMPUTING step FUNCTION `lstep : Fault ->
    Proc -> Option Trans` — the environment injects a fault (`ok`/`crashIt`/
    `loseIt`), a `Trans` carries the action label beside the next process, and the
    three fault rules are just more cases: CRASH (any live process fails to `crash`,
    label `lfail`), LOSS (an in-flight `out` is dropped, sender proceeds silently),
    DETECT (a `mon`itor beside a crashed peer observes the failure and runs its
    handler). Each rule computes, certified by refl. Atop it the BOUNDED restart-
    liveness guarantee (D5 Layer R2) the static ch115 `restart` could not state:
    `eventuallyRestarted : Σ (k : Nat). runN k supervised ≡ restarted` (the Σ-of-a-
    step-count `Eventually` — a monitored crashed worker recovers, witness k=1), plus
    safety certs that a monitor is NECESSARY (an unmonitored crash never spuriously
    recovers). This unblocks, as the SPEC layer, D5-R2 (supervisor liveness against a
    real crash-detect transition) and E3 adequacy (the LTS the projection refines now
    has faults). Zero core change. Still open: the live wiring (primMonitor/primExit,
    parked), the full coinductive `Eventually` over an unbounded fault stream, and the
    per-protocol bisimulation/adequacy proofs (E2/E3).
  - **E3 ADEQUACY — the projection refines the fault LTS (ch207).** The correctness
    theorem tying spec to runtime: the projected actor's OBSERVABLE behaviour refines
    `lstep`. The live runtime is foreign, so adequacy is stated between two rune-level
    semantics — the operational trace `visibleRun` (the observable labels `lstep ok`
    emits, τ erased) and the projection denotation `project` (the observable action
    sequence the projected actor emits, structural over Proc per the R-CALC table).
    Communication is internal (τ) on both sides; the observable alphabet is FAILURE
    (`lfail`), the events a supervisor reacts to. For well-supervised protocols (every
    crash beside a monitor) `visibleRun k P ≡ project P` by refl: a single supervised
    crash, a two-fault sequence (left-biased order matches), a survivor (failure
    isolation), a quiet no-fault protocol (empty trace, no spurious failure), and
    fuel-stability (a trace statement, not a step-count artifact). So the projected
    actor does nothing the spec forbids — verified OTP closes at the spec level
    (proven model ch114/115 + fault spec ch206 + projection-refines-spec ch207).
  - **General LTS bisimulation library (ch208).** The ch69/ch70 coinductive-trace
    machinery, packaged backend-agnostic: an LTS is a coalgebra `c : El S -> El
    (A × S)`, its behaviour is `traceOf c`, and `traceBisim` proves two states'
    behaviours path-equal from a head + tail agreement. Trace-bisimilarity is an
    EQUIVALENCE (the three laws `traceBisimRefl`/`Sym`/`Trans` are exported; it is
    path equality on `Str A`). The computing-bisimulation shape every protocol uses
    is `traceBisim … (preflF …) (preflF …)` — conversion discharges it when the
    states' observations definitionally coincide.
  - **Coinductive adequacy (ch209).** ch207's finite-list adequacy, lifted to the
    unbounded behaviour STREAM via the ch208 library: a process's fault behaviour is
    the infinite observation stream the fault LTS emits, and two mirror-image
    supervised systems (crash left vs right of the monitor, symmetric DETECT) are
    proven path-equal as behaviour streams by `traceBisim` — no fuel bound, the whole
    behaviour at once, genuinely coinductive (distinct seeds, discharged by
    `bisimToPathStr`). The observation certs (`behHeadA/B`, refl) show the fault
    appears in the stream. So adequacy holds for unbounded time, not just bounded
    prefixes.
  - **The adequacy tie + trust boundary (ref_docs/wootz/ADEQUACY-TIE.md).** The
    three-layer correspondence: SPEC (fault LTS ch206) → PROOF (adequacy ch207/ch209,
    kernel, no backend appeal) → LIVE (BEAM runtime ch205, runs the happy-path
    projection to its predicted value). Proven end-to-end with NO trust: `project P`
    refines `lstep`. Confirmed running for the implemented primitive subset
    (spawn/send/receive, ch205). TRUSTED at the foreign edge: that BEAM faithfully
    implements the projection table — the minimal, auditable "backend is correct"
    obligation, a few lines of Erlang per named primitive. The remaining gap is
    explicit: the FAULT primitives (`primMonitor`/`primExit`) are not yet in
    `beamOTPRuntime` (parked, D5-faults-live), so the CRASH/DETECT projection is
    proven + specified against the BEAM model but not yet executed live — closing it
    is two `beamOTPRuntime` entries + a fault listing, mechanical.
  - **Runtime** — Σ erases to tuples and DEPLOYS (ch32 runs); `partial` defs run.
  - **Kernel fix** — eliminator generation for ≥2-recursive-argument constructors
    (a de Bruijn miscount; unblocks all branching datatypes). ch34.
  - **B6 multi-backend source emitters** — beyond the v1 JS backend, FIVE more
    source emitters over the SAME Phase-7 erased IR: **Python, Go, Rust, BEAM, and
    JVM (Java 25+)** (`codegen/{py,go,rust,beam,jvm}.go`). Selectable via
    `codegen.ByTarget` + the CLI `rune emit|run <file> [name] --target
    js|py|go|rs|erl|jvm` (aliases python/rust/golang/javascript/java). The JVM
    backend targets **JVM 25+** — a sealed `V` interface + record variants,
    pattern-matching switch for apply/show/eliminators, virtual-thread entrypoint;
    `javac --release 25` then `java`. Cross-backend conformance
    (`harness/backend_conformance_test.go`) is the portability gate — the same
    checked program observes byte-identical results on every backend incl JVM (pure
    + partial + IO + FFI parity). B3+ fan-out, ahead of the B1/B2 IR refactor.
  **M5 (matured core) is COMPLETE: C1–C6 all landed.** **TRACK A (cubical inner) is
  COMPLETE — M1/M2/M3 closed (2026-06-16):** A1–A9 total — the cubical interior
  (A1–A4), computational univalence (A5–A8: `ua`-as-Glue, J/castU over `ua`), and
  the inner HIT kit (A9: `Circle`/`Susp`/`Quotient`/`Trunc0`, each with recursor +
  dependent inductor, dim-2 `squash`, and **total transport on all three generator
  shapes** — points, paths, and the formal `hcomp` cell via `transpHitHcompCell`,
  ch196). A10/R-UFH (fibrant universe hierarchy) is the DAG's one CONDITIONAL node
  ("if needed"); condition unmet (a single `UF : U1` carries every HIT/`ua`), parked
  by Standing Rule 1 (PARKING-LOT.md). **TRACK B (portable codegen + FFI) is COMPLETE
  — M4 closed (2026-06-16):** B1/B2 done (one shared erased IR; all 8 backends render
  it; eliminators lowered once via `LowerElim` — the sole exception is the builtin
  Nat's hand-rolled accel path, a justified compressed-literal ABI exception); B3+
  ships 6 source emitters (JS/Py/Go/Rust/BEAM/JVM-25) + 2 native backends (C and
  LLVM-IR, closure-converted IR + mark-sweep GC + shared C runtime); B4 C-ABI FFI =
  assume-tier + scalar/String/Ptr marshalling + multi-foreign/non-identity calls,
  byte-identical 8/8 (ch197); B5 deploys the compute-away inner fragment. Parked
  no-consumer/no-toolchain tails (Cranelift/WASM/Swift native; native bignum;
  contract-guard tier; bare-path runtime meaning) in PARKING-LOT.md. Still reachable
  (large/research): the fully-general proper-face transp-over-Glue (FACE-RESTRICTED-
  EVAL-DESIGN §7, no-consumer tail); **D3–D6** (reals/ML-interop/OTP, need the
  contract-guard tier + BLAS); **E3/E4** distributed (the M7 tail). No hash-format
  bump beyond 0x06 unless a new core constructor is genuinely required.
  - **D5 / R-OTP — LIVE BEAM RUNTIME (Layer R0+R1) LANDED.** The proven OTP tier
    (gen_server ch114, supervisor ch115, restart strategies ch118, io_actor ch120)
    now has a LIVE runtime, not just a functional model. The BEAM backend ships
    `beamOTPRuntime` (codegen/beam.go): the `foreign` process primitives map onto
    real Erlang — `primSpawn`→`spawn`, `primSelf`→`self()`, `primSend`→`P ! X`,
    `primReceive`→`receive`; `Pid : U -> U` is the typed mailbox (Gleam `Subject(M)`
    safety, erases to a bare pid). ch205 spawns a stateful worker, the client fires
    three `bump`s + a `report`, the worker's `partial` receive loop (R-PART/C4) replies
    the count, and the whole thing RUNS on escript to 3 (FIFO-deterministic), gated by
    `TestListingsOTPLiveBeam`. The runtime ships WITH the compiler (Lambert's deployed
    artifact; BEAM's own scheduler is the "near-free" gift — no hand-rolled scheduler).
    **LIVE FAULTS LANDED (Layer R2, ch214):** `primExit`→`exit(p, crashed)` injects a
    CRASH, and `primMonitor`→`erlang:monitor(process, p)` + a blocking `receive {'DOWN',
    …}` IS the DETECT rule (robust to the crash-before-monitor race — monitoring a dead
    pid delivers an immediate DOWN/noproc, so the failure is observed exactly once). ch214
    spawns a worker that crashes itself, the supervisor detects the DOWN and RESTARTS a
    fresh worker that takes one `bump` and replies — `eventuallyRestarted` (ch206, witness
    k=1) RUNS on escript to `succ zero` (`TestListingsOTPFaultLiveBeam`). This is the live
    half of the ch206 fault LTS and the executable tie ch207/ch209 adequacy was proven
    against (the CRASH/DETECT projection now runs, not just specified). PARKED: the
    non-BEAM JS/Go cooperative scheduler shim, the FULL coinductive Eventually over an
    unbounded fault stream, and a fully-general all-P (vs per-shape/refl) adequacy lemma.
  - **D6 / R-EFFECT — STANDARD OS/IO VOCABULARY (in + out + time) LANDED.** The IO
    monad (ch43) sequences effects; ch59 proved one host op runs cross-backend. D6
    grows the STANDARD host-op vocabulary and ships it WITH the compiler (like D5's
    beamOTPRuntime), NOT test-injected: `printNat : Nat -> IO Nat` (write a number to
    stdout, return it), `getNat : IO Nat` (read a decimal from stdin), and
    `timeNanos : IO Nat` (read the OS clock in ns). The host bodies are baked into
    each backend runtime gated by `usesForeign` against the `ioPrims` set
    (codegen/ioprims.go; js/py/go/rust/beam — the Go preamble gains `"time"` only
    when timeNanos is used). ch210 reads the clock (discarded for determinism) then
    prints 1 and 2 — byte-identical `1\n2\n2`; ch211 echoes a stdin number (feed "7"
    → `7\n7`); both on all 5 source backends (harness/io_os_test.go), RUN by
    `rune run` unaided. The compressed `builtin nat` (C7) is load-bearing: a clock
    reading is ~1e18 ns, a native int, never a succ-chain. No core change, no hash bump.
    **FILES + ENV LANDED (v3.20.0, ch215):** the net/fs vocabulary now spends D1's
    IOError + B4's packed String — `getEnvCode`/`readFileCode`/`writeFileCode`/
    `printStrCode`, all over the bare packed-String CODE (a `Nat`; the Rune side wraps
    `bytes`/`codeOf`, so the host body never touches the constructor encoding). Each
    backend emits a shared codec (`__s2h`/`__h2s` decode/encode the bignum; beam's are
    top-level `d6unpack`/`d6pack`) gated by `usesFileEnv`, plus the four bodies (JS uses
    `globalThis.String` — a Rune `String : U` def shadows the bare builtin; Go gains the
    `"os"` import). ch215 writes a String to a file, reads it back and prints it, then
    prints `$RUNE_D6` — byte-identical `hello, wootz\nok\nunit` on js/py/go/erl with cwd
    set + the env var (`TestIOFileEnvConformance`); `readFileR` lifts an unreadable file
    into `Result String IOError` (`emptyInput`, via `monus code 1`), the ch212/ch213
    totality discipline at the fs boundary. **ARGV + PROCESS-EXIT LANDED (v3.21.0,
    ch216):** `argCountCode`/`argAtCode` (argv[i] code, 1 if oob) + `exitWith` (process
    exit status); ch216 prints argc + argv[0]/argv[1] then exits with status = argc —
    "2\nalpha\nbeta" + exit 2 on js/py/go/erl (`TestIOArgvExitConformance`; go is BUILT
    to a binary since `go run` masks the child status; erl drops the script-name head of
    `init:get_plain_arguments`). **D6 is now COMPLETE** for the demonstrated-need scope
    (OS/time/fs/env/argv/process, all baked + cross-backend). PARKED: raw network
    SOCKETS — async accept/connect has no uniform sequential-`IO` shape across backends
    and no consumer; needs its own effect-shape design doc (PARKING-LOT). Distribution
    rides the proven process calculus (E1–E3), not raw sockets. (rust excluded from the
    String-marshalling prims — no packed-String host body yet, parked like ch213.)
  - **D3 / R-FFI — MACHINE FLOATS (f64) + a contract-GUARDED BLAS kernel LANDED
    (v3.22.0, ch217).** The proven-exact reals (ch121–149) gain a FAST native
    counterpart: a foreign `Float : U` (host f64) + `fromNat`/`fadd`/`fsub`/`fmul`/
    `fdiv`/`floatToNat`/`fleqN`, baked per backend (js Number / py float / go float64 /
    erl float; pure host bodies, not IO — gated by `usesForeign`). A comparison returns
    a `Nat` (1/0) the Rune side cases into `Bool`, so the host never builds a
    constructor; `Float` itself is a foreign TYPE but survives erasure as `ok`/`err`'s
    type argument, so it gets a trivial unit body (like ch205's `Pid`). The BLAS slice:
    `dot2` (a fixed-width native dot product `a0*b0+a1*b1`) is the FIRST consumer of
    R-FFI's contract-GUARD tier — `dotGuarded` assumes the kernel but CHECKS its
    postcondition (≤ budget) at the boundary and BLAMES it (`Result Float Blame`, the
    blame carrying the offending value — which also makes `Blame` structurally distinct
    from `Unit`, dodging the one-nullary-ctor hash collision). ch217 →
    `11\n13\n11\n0\nunit` byte-identical js/py/go/erl (`TestIOFloatBlasConformance`).
    Unblocks D4 (the float element type was its prerequisite). REMAINING: arbitrary-
    length ddot/gemm (array marshalling) + a real OpenBLAS bind (native backends) + the
    `with post guard` surface sugar. (rust excluded — value domain has no float variant
    yet, parked.)
  - **D3 / R-FFI / R-NATIVE — the OpenBLAS SWAP on the NATIVE backends LANDED (v3.23.0,
    ch218).** On the source backends `dot2` is a hand loop; on the NATIVE backends (C +
    LLVM) it is SWAPPED for `cblas_ddot` — real OpenBLAS, linked `-lopenblas`. Both native
    runtimes gained a `K_FLOAT` boxed-double kind (`mkfloat`/`float_val`/`big_to_double`,
    in cRuntime AND ll_runtime); the float/BLAS host bodies are baked per native backend
    (emitFloatPrimsC inline + `static`; emitFloatPrimsLL in `LL.EmitRuntimeFor(p)` with
    EXTERNAL linkage + the rt_* helpers, since the .ll calls `@dot2`/`@fromNat` that
    `foreignNames` auto-declares). The base `EmitRuntime()` omits them so the generic
    corpus links without -lopenblas; only a float program uses `EmitRuntimeFor` + the
    flag. The swap is bound to the portable reference by a CONFIGURABLE TOLERANCE contract
    (`dotChecked eps`, default `defaultEps` = 1e-9): the OpenBLAS result must be within ε
    of the in-language reference dot, else blame — parity defined at the CONTRACT, not the
    bits. ch218 → `11\n11\n0\nunit` byte-identical across js/py/go/erl AND C/LLVM-via-
    OpenBLAS (`TestD3OpenBLASTolerance`). The C backend also gained a baked `printNat`
    (the native backends lacked the ioPrims). **ARRAY MARSHALLING LANDED (v3.24.0,
    ch219):** `dotList` walks a Rune `FList` of floats (fnil tag 0 | fcons tag 1, the
    Float in slot 0) into a malloc'd C `double[]` and runs `cblas_ddot` at ARBITRARY
    length on C/LLVM (the FFI array boundary D4 interop needs), tolerance-bound to an
    in-language reference fold `refDotList` (`TestD3BLASVector`). REMAINING D3:
    gemm/matrix BLAS + the `with post guard` surface sugar + rust/jvm float bodies.
  - **REPL feature parity with the file loader (v3.24.0).** `runForm` now routes EVERY
    top-level declaration form through the session's Add* methods (the same pipeline as
    `LoadSource`): not just `name :` defs and `data`, but `foreign`/`partial`/`instance`/
    `builtin`/`module` too (`looksLikeDecl` dispatches on the leading keyword + the
    `name :` shape). REDEFINITION is editing — `AddDef` overwrites `s.refs[name]`, so a
    re-entered def is latest-wins. Multi-line works for all forms (the parser's
    `ErrIncomplete` drives the continuation loop). So the REPL can make/edit types,
    functions, foreign axioms, instances, and multi-line blocks — `TestREPLDeclParity`.
  - **STATE SINCE v3.24 (v3.25.0 → v3.290.0, ~200 listings).** This phase-map detail
    above stops at v3.24; the authoritative CURRENT build state + the live frontier is the
    DAG plan (`~/.claude/plans/humble-humming-elephant.md`, with the `ref_docs/wootz/`
    design records). Summary of what landed since, by track:
    - **D3 reals/LA + R-FFI** — f64 `Float` + a contract-GUARDED BLAS kernel across all
      backends (ch217), the OpenBLAS SWAP on the native C/LLVM backends behind a tolerance
      contract (ch218), arbitrary-length array marshalling (ch219), `gemm` matrix BLAS
      (ch220). The `with post guard` SUGAR LANDED (v3.317.0, ch440 — a pure-parser
      postfix desugaring to a let-bound single-eval check-and-blame; GRAMMAR §5.8;
      REPL-tested). Done bar JVM float bodies (java-25 emitter, untestable on local java-17).
    - **D4 ML interop** — the NumPy suite (npDot/npMean/npMatSum/npVar/npMax/npNorm,
      ch221–227), each REAL numpy on py / OpenBLAS on native / hand floor elsewhere, all
      contract-guarded + 7-backend identical; a capstone mean→center→norm pipeline (ch228);
      SHAPES-PROVEN safeDot (a ragged call is a compile error, ch233). Remaining: plotting
      (matplotlib absent), the `Array dt sh` handle + CPython embed.
    - **E3/E4 distributed** — the big one. General all-P adequacy CLOSED (soundness ch409 +
      completeness ch421 over the full four-label calculus); a VERIFIED CRDT corpus over two
      lattices carrying convergence+safety+stability (G-Counter/G-Set/PN-Counter
      ch410–419), refutations matching the simulator's law linter (ch416/417); the
      `internal/sim` + `rune simulate` better-than-Winglang SIMULATOR (v3.230–v3.248,
      gates by observed behaviour — safety/robustness/durability — with a `Diagnose` CvRDT
      law linter + `Stabilize` liveness check); expanded algebra (2P-Set ch422, the generic
      CvRDT SEC theorem ch423, VECTOR CLOCKS ch424–427, causal delivery ch428, MV-Register
      ch429, OR-Set ch430, inflation ch431, gossip-converges ch432, N-replica VCs ch437);
      and the LIVE-ACTOR projection — the verified replicated counter RUNS as gossiping BEAM
      processes, the fault-tolerance trilogy live (converge/durability/recovery ch433–435)
      + the GENERIC protocol→actors library `serveG` over any CvRDT (ch436). Remaining E4:
      only the `protocol … end` surface sugar + `deploy` verb (design-gated, needs user
      input; the projection mechanism is done as a library).
    - **REPL/tower DX (Savage, cross-cutting, branch `feat/tower-arithmetic-ops`)** —
      ergonomic verified STRINGS (`++` Semigroup/Monoid, `runes` view, Show, slicing,
      `{expr}` interpolation; algebra via a `toCodes/fromCodes` view, packed repr untouched,
      deployed all backends, ch438_string_algebra/ch439_string_deploy); DECIMAL literal
      input (`1.3`/`1.{3}` → exact Frac); overloaded tower `+ - *` via one `Num` Σ-record;
      and prefix NEGATION promotion (v3.290.0) — `negate` is result-indexed `NegR R A`
      (`Whole→Int`/`Int→Int`/`Frac→Frac`), so `-3 : Int` (was `: Frac`), `-1/3 : Frac`
      preserved via a new `Div Int`. All surface/elaborator/prelude-only, outer kernel fixed.
    - **E4 / WAVELET cloud-abstraction layer (v3.291.0 → v3.295.0, branch
      feat/wavelet-infra, ref_docs/wootz/R-INFRA.md).** E4's deploy half opened as a
      provider-agnostic infra layer — a new `infra/` package (the deploy-side dual of
      `codegen/`: Resource model → Emitter plugins, zero core). LANDED: a 22-ROW MATRIX
      (queue/kv/object/compute/database/secret/nosql/dns/disk/kms/file/stream/cdn/lb/
      metrics/iam/k8s/network/firewall/logs/registry/paas) each across AWS/Azure/GCP
      (OpenTofu/Terraform HCL, `fmt -check` gated, canonical hcl writer) + 15 self-hosted
      FOSS-under-Podman backends (RabbitMQ/NATS, Valkey, Garage, Podman, Postgres, Dotenv,
      DynamoLocal, CoreDNS, registry:2, Redpanda, Loki, Prometheus, k3s, NFS, Vault +
      Vault-transit) covering every row with a sane self-hosted form — exercisable with NO
      cloud account; the equal-config→equivalent-deployment equivalence gate (app-level,
      incl cdn+lb); data-plane `.rune` interfaces (queue/kv/object) that type-check + RUN
      cross-backend; the `protocol … end` block (CONTEXTUAL keyword — checked CvRDT
      grouping that REJECTS a missing convergence proof); `infra.Kinds()` single-sourcing
      the kind list + CLI-completeness gates (every kind × every cloud, single + manifest
      modes); examples/app.wav a 9-resource web app; and `rune deploy` in two modes — infra
      (`--resource …`/`--manifest`) and workload (`FILE --target beam` RUNS a verified
      protocol's actors live on BEAM to convergence, the Lambert gate). REMAINING: runtime
      data-plane foreign-op binding + live Podman round-trip; the dependency-heavy matrix
      tail (serverless/warehouse/archival/devops/ai-ml — each has one messy provider);
      real cloud apply.
    - **D5 distributed liveness** — UNBOUNDED restart-liveness LANDED (v3.317.x, ch206:
      `runStuck`/`stableRestart` prove recovery persists for ALL step counts; `unmonitoredStuck`
      the safety dual), the ∀-strengthening of the bounded k=1 witness, by conversion.
    - **Frontier (open nodes):** the dependency-heavy wavelet matrix tail + live Podman
      round-trip (above), D4 tail (plotting + CPython embed), D5 tail (non-BEAM scheduler
      shim + COINDUCTIVE-stream Eventually — the inductive always-property is done; the
      stream version hits the documented dfix wall), D7 live hot-reload (`code_change` on
      BEAM). Everything else landed or
      parked by demonstrated-need (PARKING-LOT.md). See the DAG for deps + implications.

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
5. **Delete the superseded; do not hoard it.** When a thing X is subsumed by
   something more fundamental Y (Y derives what X postulated, Y proves what X
   asserted), DELETE X — migrate its consumers to Y, take the hash event. "X is
   redundant, so deletion buys nothing" is BACKWARDS: redundancy is the REASON to
   delete, and keeping a less-fundamental redundant artifact is dead weight that
   misleads every future reader into thinking it is load-bearing. A real hash
   event or a consumer migration is a fair price for removing a postulate / fiat
   rule / dead code; do not defer it as "not required." (Worked example: the
   postulated `ua` head + its `castU`-over-`ua` fiat rule were retired once the
   derived `uaGlue` superseded them — fib group 11→10, `ua` moved to the ambient
   derived-univalence prelude.)

## Engineering conventions

- Go standard library plus three recorded direct dependencies: the property-testing
  library `pgregory.net/rapid`; `goforge.dev/blake3sum` for BLAKE3 content hashing
  (behind `core.Hash`; pulls `klauspost/cpuid` as an indirect dep for its SIMD
  dispatch); and `golang.org/x/sys` (was already indirect via blake3sum) promoted to
  DIRECT for the REPL's hand-rolled raw-mode line editor (termios via `unix.Ioctl*Termios`
  in `internal/repl/rawmode_linux.go`). The line editor — history, Ctrl-R reverse
  search, persistent `~/.rune_history` — is hand-rolled rather than pulled from a
  readline library, so no third-party module enters the graph (the MIT `chzyer/readline`
  was a structural reference only). No further dependency without recording why.
- The harness is the gate from day one: `parse ∘ pretty = id` and hash-invariance
  under alpha-renaming hold now; the Phase-1+ invariants (type preservation,
  conversion as equivalence + congruence, the proof-cache Frame Lemma) exist as
  documented, skipped property stubs that the mutation-testing layer will hunt.
- Conventional Commits for every commit.
