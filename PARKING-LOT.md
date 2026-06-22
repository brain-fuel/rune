# Parking Lot

Improvements that were tempting during Phase 0 but are **not required** by the
current goal. Each is parked with a one-line rationale. Nothing here may be built
until a current listing (eventually a *Specify & Verify* listing) needs it.

## Fixed bugs

- **Eliminator generation for constructors with ≥2 recursive arguments — FIXED
  (2026-06).** `data Tree is leaf | node : Tree -> Tree -> Tree end` and its
  `TreeElim` on `node` used to panic (de Bruijn miscount in `elaborate/data.go`
  `caseType`: an outer IH's domain `m a_j` was counting the IHs wrapped *inside*
  it rather than the *outer* recursive args that enclose it — correct only at
  nIH=1). Now the IH telescope uses the outer-recursive-arg count; verified by
  the leaf-count regression (listings/ch34_trees.rune, `leaves balanced = 4`).
  Branching datatypes (trees, a process calculus with parallel/choice) are now
  available.

## Deferred by phase plan (well-understood swaps)

- **Eq-of-types decomposition.** `Eq U (Pi …) (Pi …)` stays stuck rather than
  unfolding to a telescope of domain/codomain equalities — that unfolding needs
  Sigma types, which have no other consumer yet. Convertible endpoints are still
  provable by `refl`. (equality/)
- **Full definitional proof irrelevance.** Conversion equates refls and skips
  cast proofs; equating an arbitrary NEUTRAL proof with refl needs type-directed
  conversion. Parked until a listing needs it. (core/conv.go)
- **A10 / R-UFH — fibrant universe hierarchy `UF_i`.** The DAG's one CONDITIONAL
  Track-A node (`A10 [I] fibrant universe hierarchy UF_i (if needed) ⇐ R-UFH, A6`):
  a tower of fibrant universes `UF_0 : UF_1 : …` so a HIT/`ua` can be transported
  one level up (codes-for-`UF`). No design doc was ever written (no `R-UFH.md`) —
  by deliberate priority. Its condition ("if needed") is UNMET: nothing in the
  corpus or stdlib needs codes-for-`UF` or univalence one level up; a single
  fibrant `UF : U1` carries every HIT, `ua`, and the whole cubical interior. Per
  Standing Rule 1 (no feature without a consumer), parked until a stdlib math
  module genuinely requires a higher fibrant universe. With A10's condition unmet
  and A1–A9 total (A9's last cubical filler, transp-over-a-formal-`hcomp`-cell,
  closed 2026-06-16; ch196), **Track A is complete at its consumer-driven scope.**
- **Track B (portable codegen + FFI) parked tails — Track B COMPLETE at its
  consumer-driven scope (2026-06-16).** B1/B2 done (one shared erased IR, all 8
  backends render it, eliminators lowered once via `LowerElim`); B3 (BEAM) + B3+
  fan-out shipped JS/Py/Go/Rust/BEAM/JVM-25 source emitters + C and LLVM-IR native
  backends (closure-converted IR, mark-sweep GC, shared C runtime); B4 C-ABI FFI
  carries the `foreign`-axiom assume-tier + scalar/String/Ptr marshalling + multi-
  foreign / non-identity calls, all byte-identical 8/8 (ch197); B5 deploys the
  compute-away inner fragment (ch10 `flipped` runs). Parked, each no-consumer or
  no-toolchain (Standing Rule 1):
  - **B3+ Cranelift / WASM-machine / Swift native fan-out.** No toolchain present in
    this environment and no deployment needs them; the 6 source backends + 2 native
    backends cover every current target. The closure-converted IR + shared-runtime
    pattern is ready when one of these gets a real consumer.
  - **B4 arbitrary-precision `LitNat` past i64 on the native backends.** The core
    keeps `big.Int`; the C/LLVM runtime boxes nat as a machine `i64`. No listing
    deploys a nat literal exceeding i64 to a native target, so the bignum runtime is
    deferred (source backends already carry arbitrary precision via host bigint).
  - **B4 contract-DISCHARGE / runtime-guard tier.** The assume-tier (a `foreign`'s
    type IS its contract, tracked in the proof-cache `A`-set) ships; the runtime
    guard tier (checking a foreign's spec at the boundary) waits on its consumer —
    D3/D4 (BLAS/NumPy interop), not yet built.
  - **B5 full inner-Kan runtime meaning + op-precise deploy ban (R-ERASE2).** The
    Class-1/2 compute-away fragment deploys; a bare inner path (`pabs`/a `ua` value
    with no erased meaning) stays banned. The coarse `session.innerTaint` over-bans
    (sound — never under-bans); the op-precise structural ban + a runtime meaning
    for bare paths are research with no consumer (no deployed program runs a bare
    path). See R-ERASE2.md.
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
- **Sigma types and pairs.** LANDED (C1/R-SUM, 2026-06): `Sig`/`Pair`/`Fst`/`Snd`
  are in the outer core with definitional η (hash bump 0x05). Remaining (C1b/C2):
  surface `record`/`struct`/`class`/`instance` sugar + elaborator infer/check, the
  OTT Σ-Eq value rule + the Eq-U type-former telescope (retires the Eq-of-types
  entry below), typeclasses-as-records with implicit instance search, and Σ
  covariance in `Sub` / linear-Σ in QTT (no consumer yet).
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
- **REPL line editing: ~~readline, history~~, completion.** DONE for history +
  line editing + Ctrl-R reverse search (hand-rolled on `golang.org/x/sys`, promoted
  indirect→direct; no third-party readline dep — `chzyer/readline` was a reference,
  not an import). Persistent `~/.rune_history`; numbered referenceable results (`$N`).
  Still parked: TAB completion of names/commands; multi-row redraw for wrapped lines.
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

- **§F: computational inner univalence — IN PROGRESS (phases 1–3 done, 2026-06).**
  The labelled frontier (ref_docs/rune-v3-design.md): a cubical INNER stratum —
  interval, Kan operations, Glue — isolated behind the fibrant universe, with
  the outer OTT core untouched. v3 ships `ua` postulated and `castU` computing
  through it; path induction OVER a ua-path does not compute, and making it
  compute is research, not a checkbox. PHASE 1 (the interval as a De Morgan
  algebra) is now built: store/interval.go, a fourth builtin group on its own
  hash space, computing on endpoints; listing ch17. PHASE 2 (paths as interval
  functions) is built too: store/path.go, a fifth builtin group (pabs/papp) on
  its own hash space — papp computes by β, refl, and the endpoint boundary, so
  sym/ap/connections type-check; listing ch18. PHASE 3 (Kan operations) is built:
  the face lattice (store/face.go, ι on endpoints, ch19), partial elements via a
  proof-irrelevant `holds : F -> Prop` (store/sys.go, ch20), and `transp`/`hcomp`/
  `comp` (store/kan.go) computing their ENDPOINT rules — transp regularity (a
  constant type-line via a freshness sentinel), hcomp/comp total-system (φ = ⊤ ⇒
  `u i1 htop`), comp degenerate (φ = ⊥ on a constant line ⇒ `u0`); ch21–ch22.
  The STRUCTURAL interior is partly in: the `transpFill`-free `piF` slice —
  transp over a constant-domain function line, hcomp over a function type,
  comp over a constant-domain function line — all push UNDER the binder by
  recursion on the head former (`fibFormer` + `FibHash`/`KanHash` reverse
  lookups); ch23, ref_docs/rune-cubical-phase1.md. Remaining (still parked): the
  DEEP Kan fills — `transpFill` (transport i→j) and with it varying-domain piF,
  transp/comp over `pathF` (base composition), `hcomp` on a proper face for
  non-piF formers — and pathJ-via-comp on a general path; phase 4 (`Glue` +
  computational `ua`, J-over-ua computes); phase 5 (erased runtime for inner
  paths, lift the deploy ban); phase 6 (inner HITs). Unexplored on a
  content-addressed substrate; plausibly the technical heart of a thesis.
- **System overlap-agreement is not definitional.** A partial element on φ is
  `holds φ -> El A` and `holds φ : Prop`, but this engine's proof irrelevance
  only equates Eq/refl/cast proofs at the canonical level (UIP) — it does NOT
  equate arbitrary Prop inhabitants in conversion, so `part h1 ≡ part h2` for two
  proofs of `holds φ` fails to check. Consequence: a system's overlap-agreement
  obligation is not discharged definitionally (the 3b design aspiration). To make
  it so would need either an irrelevant-Prop conversion rule (compare any two
  neutrals at a Prop type as equal) or explicit Eq evidence threaded through
  systems. No consumer yet (the Kan ops only feed `htop` on a ⊤ face); revisit
  when a real multi-branch system needs the overlap.
- **A fibrant universe hierarchy.** `UF : U1` is the single inner universe;
  codes for UF itself (paths between universes, univalence one level up)
  need `UF1` and a lifting story. No chapter uses them.
- **Sigma codes in UF (`sigmaF`).** LANDED (A5/R-SIGMA, 2026-06): `sigmaF`/`pairF`/
  `fstF`/`sndF` are a builtin group with computing projections (store/sigma.go,
  ch24), so `Equiv = Σ f, isEquiv f` is now stateable. Remaining: Kan over sigmaF
  (A5a non-dependent componentwise now; A5b dependent fill needs R-FILL).
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

- **Compressed core numerals — ADDRESSED for binary (2026-06).** Numerals no
  longer expand at parse time: a literal survives as ENum and is lowered during
  elaboration BY ITS EXPECTED TYPE (surface/numeral.go, NumConfig). A `builtin
  bin BN bn0 bnP Pos pH pO pI` binding lets a numeral checked at BN/Pos lower to
  an O(log n) bit-spine with NO 4096 cap; the unary `builtin nat` default (and
  its cap) is unchanged, and the two coexist — the type chooses per literal. The
  untyped resolver still lowers to the unary default, so existing content hashes
  are unchanged (verified: ch14's hand-written Pos constants became `35`/`41`/…
  literals with byte-identical core, full gate green). A TRUE compressed core
  numeral (a core Tm node carrying a bigint, with a hash-format bump) is **no
  longer parked — promoted to DAG node C7 / R-NUM** (ref_docs/wootz/R-NUM.md):
  binary carries big *constants* compactly but does nothing for the unary 4096 cap
  (still the default) or for *arithmetic* (mul 4096 4096 = O(a·b) succ nodes), so
  `NatLit{*big.Int}` + an opt-in kernel accel table is the real fix, and it retires
  `builtin bin` (Rule 5). Pretty-folding a literal to a digit is part of C7.
  Guarded by internal/session/binlit_test.go (until the Rule-5 migration).
- **Deep-application evaluation was superlinear — FIXED (2026-06).** The
  profile (Eval 70%, mallocgc 41%) had been misread: the quadratic was not in
  Machine.Eval but in the CHECKERS — Infer/InferCore evaluated each
  application's argument term from scratch at every node just to feed pi.Cod,
  O(n²) over an n-deep chain (a 4000 literal cost ~7s to check; now ~25ms).
  Eval now marks non-dependent Pis (VPi.NonDep, the strict-language stand-in
  for a lazy argument thunk) and the checkers skip the eval when the codomain
  ignores it. The Machine's ι path also got the predicted spine sharing as
  constant-factor work: known-bodiless heads (constructors, eliminators,
  builtins) are built rigid with no glue thunk, IH spines share the
  applied-eliminator prefix instead of rebuilding it per node, and apply
  classifies the head before flattening (one single-allocation spineParts
  instead of three prepend-copies). 80*100 through the REPL: 2.35s → 0.87s.
  What remains is intrinsic, not a bug: unary `a * b` via `ih + n` costs
  ~b·a²/2 ι-steps (each addition re-walks the accumulator), so big products
  want the erased shadow or a compressed numeral core, not more Machine
  tuning. Guarded by internal/session/deepchain_test.go.
- **Call-site erasure of the 0-fragment — FIXED (2026-06).** Proofs built from
  ordinary ω helpers (`cong`, `trans`) leaked the deep numerals their equation
  endpoints mention into the JS shadow (node's parser died ~1600 deep). The
  parking-lot framing ("drop 0-quantity arguments at application sites") was
  insufficient — those helpers take UNANNOTATED (ω) proof parameters, so
  quantity is the wrong boundary; the numerals ride in their implicit ω
  endpoint positions. The real boundary is PROOF IRRELEVANCE: any Prop-typed
  subterm erases to the unit token however it was built. Fix is a type-directed
  pass (elaborate.TypedEraser) that mirrors CheckCore and drives the emit path;
  syntactic codegen.Erase stays as the type-free primitive. 0-quantity argument
  positions are still unit'd with their position kept (no arity surgery). See
  ref_docs/rune-verified-implementations.md, guarded by
  internal/session/erase_test.go.
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
- **`//` and `%` at Rat (numeric tower) — DONE (2026-06).** The flooring
  quotient on ℚ is well-defined on the quotient only by Euclidean uniqueness;
  that theorem (the general division-algorithm result ch11 deferred) is proved
  in ch16 and, specialised to ch13's conventions, drives `floorQ`'s qlift
  respect proof (`floorUnique`). The signed Int floor needed no sign-case split:
  `-b ≡ d·b (mod d+1)` makes the floor numerator `(a + d·b) // (d+1) - b`, one
  division, so the characterization falls out of divLaw/remBound. ch13 now ships
  `// = floorQ ∘ (/)` and `% = a - (a//b)·b` with live certificates (floorPos,
  floorNeg rounding toward −∞, fracHalf). Semantics pinned in
  rune-numeric-tower.md §2.

- **pabsU-η (type-path η)** — LANDED (cont.21). `convPathUEta` (conv.go, mirrors the pathF `convPathEta`) adds `pabsU(λi. pappU p i) ≡ p`. No perturbation (full suite green). It unblocked BOTH `transportFId` (with honest castU, below) and `pathUJ` (path induction over a ua-path), which need it for the endpoint coherences of the transport line.

- **D5 / R-OTP live runtime — the non-keystone tails (2026-06-19).** The live
  BEAM runtime LANDED (Layer R0+R1: Pid + spawn/send/receive/self as real Erlang,
  ch205 runs a looping actor on escript; codegen `beamOTPRuntime`). Parked, each
  for a stated reason:
  - **`primMonitor`/`primExit` (fault detection: DOWN/Reason).** LANDED 2026-06-19
    (ch214, v3.19.0) — a worker crashes, the supervisor detects the DOWN and restarts;
    no longer parked.
  - **Non-BEAM cooperative scheduler shim (JS single-thread, Go goroutines+chan).**
    Concurrency cannot give byte-identical cross-backend output without a
    deterministic scheduler, so the cross-backend conformance corpus gains nothing;
    BEAM is concurrency's natural home (Lambert's near-free gift). Build a shim only
    when a non-BEAM deployment target demands live actors.
  - **Layer-R2 guarantee-transfer (live processes ⊨ the ch114/115 models).** The
    verified-OTP liveness guarantee (a crashed permanent child is eventually
    restarted) needs the R-CALC fault LTS `{CRASH,LOSS,DETECT}` + E2 weak
    bisimulation + E3 per-protocol adequacy — none of which exist yet. The proven
    MODELS and the LIVE runtime both exist; the bisimulation BRIDGE between them is
    the open research (R-OTP.md "needs-more-research").

- **D6 / R-EFFECT — raw network SOCKETS (2026-06-19).** The standard OS/IO vocabulary
  is otherwise complete and cross-backend: console+clock (ch210/211), the IOError
  layer + packed String + CLI parsing (ch212/213), files+env (ch215, v3.20.0), and
  argv+process-exit (ch216, v3.20.x). Raw TCP/UDP sockets are PARKED:
  - No consumer — no listing or stdlib module needs a socket yet.
  - The async accept/connect/select model does NOT fit the sequential `IO` monad
    uniformly across backends (js callbacks/promises, py blocking, go goroutines,
    erl gen_tcp+processes), so there is no byte-identical cross-backend conformance
    shape — the property every other D6 op is gated by. A socket op needs its own
    effect shape (a continuation/async extension of R-EFFECT), i.e. a design doc, not
    a baked one-liner. Build when a networked consumer (an HTTP client, a distributed
    transport for the E-track) actually demands it; BEAM's `gen_tcp` + the OTP runtime
    is the natural first target. Until then, distribution rides the proven process
    calculus (E1–E3), not raw sockets.

- **C / LLVM backend — curried function-returning `NatElim` is EXPONENTIAL (2026-06-22).**
  Discovered while building the furnace↔CPython-embed bridge (ch465). An eliminator whose
  MOTIVE is a function type and which is then applied — the classic decidable-equality shape
  `beqNat : Nat -> Nat -> Bool` defined as `NatElim (… : Nat -> Bool) base step a` applied to
  `b` — runs in EXPONENTIAL time on the native C/LLVM backends, while the source backends
  (go/js/rust) evaluate it in linear time. Measured on C: `beqNat n n` takes n=8 → 4ms,
  n=16 → 191ms, n=32 → >5s (timeout). So ~2^n. CONSEQUENCE: a program that compares moderately
  large nats with `beqNat` works on go/js but appears to HANG on C/LLVM — a real cross-backend
  CONFORMANCE GAP (telos-2's "same observable on every backend" holds only for SMALL inputs of
  this pattern). ROOT CAUSE (suspected): the native `NatElim` lowering rebuilds / re-forces the
  inductive hypothesis closure `ihk` per inner-eliminator step instead of memoizing it, so the
  nested NatElim in `beqNat` compounds multiplicatively. WORKAROUND: keep `beqNat`-style
  comparisons to small nats on the native backends (ch465 uses 2^4=16, not 2^10=1024). PARKED:
  the fix is a native-`NatElim` codegen change (thunk/memoize the per-level IH closure) —
  high-caution core-codegen work, property+perf-tested, no current consumer beyond this furnace
  nicety. The proven LISTINGS that use `beqNat` are proof-only (elaborate-check) or run on the
  source backends, so none regress; only the native RUN of a large-nat `beqNat` is affected.
