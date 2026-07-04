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

- **C / LLVM backend — curried function-returning `NatElim` exponential — RESOLVED (2026-06-22,
  v3.328.38).** Was: an eliminator whose MOTIVE is a function type whose inner step ignores its
  IH (the decidable-equality shape `beqNat : Nat -> Nat -> Bool`) ran super-exponentially on the
  native C/LLVM backends (`beqNat n n`: n=16 → 0.2s, n=20 → 8.7s, n=24 → hung) while go/js/rust
  were linear. ROOT CAUSE (confirmed, NOT the suspected IH-closure re-forcing): the native nat
  eliminator `_b3` is a BOTTOM-UP eager fold that evaluates the step at every index 0..x-1; when
  the step ignores its IH, all but the last result are discarded yet still computed, and nested
  IH-ignoring elims compound (T(a,b)=Σ_{k<b}T(a-1,k), super-exponential). The source backends
  avoid this via a `$natD` dispatch (js.go natDispatch): when the step provably ignores the IH,
  emit the constant-time one-peel CASE `x==0 ? c0 : c1(x-1) unit` instead of the fold. FIX: ported
  that dispatch to the native backends — shared `StepIgnoresIH`/`NatElimSpine`/`cirUsesArg` on the
  closure-converted CIr (closure.go), a `<elim>_case` one-peel helper in c.go + ll.go, and a
  `natDispatch` hook at the AppClosure site. IH-USING elims still use the stack-safe bottom-up
  fold (only the IH-ignoring case is rewritten). Now linear on both native backends (`beqNat 1000
  1000` instant). Gated by harness/nat_dispatch_test.go (n=64, perf + correctness, both backends);
  full `go test ./...` green. Telos-2 "same observable on every backend" now holds at scale for
  this pattern.

## elemTy : DType -> U (D4 rung 3) — needs a universe-polymorphic eliminator
The DType kit (ch471) ships `dtypeStr : DType -> String` (the __array_interface__ typestr) and
`dtypeMatch` (the guard-tier dtype check) — both SMALL eliminations the fixed kernel handles. The
design's third piece, `elemTy : DType -> U` (the Rune scalar a cell crosses as — f64↦Float,
i64↦Nat, bool_↦Bool), is a LARGE elimination: a motive into `U`. The generated data eliminator
hardcodes its motive at U0 (`elaborate/data.go:254`, `m : D p* -> U0`), so a type-VALUED motive
(`fn w is U end : DType -> U1`) fails with a universe-level mismatch. Supporting it requires a
universe-POLYMORPHIC eliminator generator — a core change to EVERY datatype's eliminator preimage
(hash bump + cache nuke), Thompson-sensitive. The ONLY consumer is elemTy, and it folds naturally
into rung 4 (the CArray CRepr already knows each dtype's element representation at codegen, where
the marshaller lives). Parked per Standing Rule 1 until rung 4 (or another large-elim consumer)
forces the question. The typestr — not elemTy — is the boundary's actual dtype mechanism: the
guard compares a foreign buffer's reported typestr to `dtypeStr dt`.

## D4 rung 4: the opaque Array dt sh CRepr (zero-copy handle) — no consumer
The in-language typed array handle is CLOSED on the flat substrate (ch470 shape discharge, ch471
dtype kit, ch472 the bundled TypedArr) — shape-discharged, dtype-tagged numpy interop that RUNS
real numpy via the embed (→ 56) across the C/LLVM native backends. Rung 4 (R-INTEROP.md §A/C) would
replace TypedArr's flat `FList` payload with an opaque `Array dt sh` handle whose runtime repr
mirrors numpy's __array_interface__ (data ptr + dtype + shape + strides), so an array crosses the
FFI boundary as O(1) handle passing instead of an element-wise FList walk. That is a PERFORMANCE
optimization (O(1) vs O(n) marshalling), NOT a correctness or capability gap — ch472 already
computes the right answer. It requires a new Value kind (an opaque array handle, like the existing
K_FLOAT box) threaded through the erase pipeline AND every backend runtime (c.go/ll.go + the source
emitters), plus a `store/foreign.go` abstract-axiom entry — a multi-backend runtime change. NO
current consumer demands zero-copy (no shipped pipeline pushes arrays large enough for the FList
walk to bottleneck). Parked per Standing Rule 1 until such a consumer exists; ch472 makes the swap
mechanical when it does (replace the `buf : FList` field, indices + by-construction discharge
unchanged). Sibling of the parked int64 fast-lane / native bignum / raw-sockets no-consumer tails.

## Would-be consumers for the parked frontier nodes (2026-06-23)
Every parked node is parked for "no consumer." The 2026-06-23 sweep built the three GENUINE
consumers (fit.rune ch473 → D4 ML-interop; serving.rune → E4 inference/archive rows; ch474
perpetual service → live non-settling shape) and RECORDED the would-be consumer for the rest —
building those would be MANUFACTURING need (a Standing Rule 1 violation), so they stay parked:
- **rung-4 zero-copy `Array dt sh` CRepr** ← needs a LARGE-array workload where the O(n) FList
  marshalling bottlenecks; fit.rune (ch473) computes correctly on the flat substrate at demo
  scale, so no consumer demands zero-copy yet.
- **`elemTy:DType->U`** (universe-poly eliminator) ← needs a mixed-dtype array whose cells are
  genuinely `Int` (i64) vs `Float` (f64); every shipped array is f64.
- **E4 `serverless` / `devops` rows** ← need a NEW resource shape the current model lacks
  (serverless: handler code + runtime + packaging; devops: a `pipeline` kind + a `repository`
  relationship). The FOSS forms (OpenFaaS, Woodpecker/Gitea) exist but the cloud side is
  dependency-heavy and the abstraction is new.
- **full non-settling coinductive adequacy / Always-Eventually fairness** ← ch474 gives the live
  perpetual SHAPE; the all-P theorem over an unbounded fault stream (and the GF infinitely-often
  fairness) is itself the research result, against the dfix wall (ADEQUACY-TIE.md).
- **R-UFH / R-GLUE G1 / C-REG / cubical-coind / dim-2 HIT interior** ← each consumer is a
  HoTT/cubical research result (universe-poly stdlib math; a transp-over-Glue with neutral input;
  the X2 canonicity stress suite; the optional isEquiv-glue bridge; a dim-2 HIT computation).

## SECOND-WAVE UN-PARKINGS (2026-06-23 — the `/goal consumers for…` push)
The would-be consumers recorded above were BUILT, un-parking these nodes (each now has a genuine
consumer, no longer parked):
- **rung-4 zero-copy CRepr → BUILT (ch475):** opaque numpy-registry handle; consumer = a 1000-elem
  array, npAlloc once + O(1) ops. NO LONGER PARKED.
- **`elemTy:DType->U` → BUILT (ch476) + the eliminator motive bumped to U1 (large elim LIFTED,
  elaborate/data.go):** consumer = mixed i64/f64/bool_ cells. The "no large elimination into U"
  limit is RETIRED. NO LONGER PARKED.
- **E4 serverless/devops → BUILT (Fn/Woodpecker):** consumer = serving.rune; matrix complete (27).
- **non-settling adequacy → BUILT for the perpetual-mirror class (ch477):** consumer = ch474.
- **dim-2 HIT interior → was ALREADY CLOSED (ch98 + ch478 consumer):** the parking was stale.
Still genuinely parked (capability provided otherwise / research): R-UFH (single UF suffices),
R-GLUE G1 (derived-ua computes), cubical-coind (nuCons closed E2), the greatest-fixpoint
Always-Eventually fairness + fully-general non-settling adequacy (the dfix wall, research).

## Go foldLines is the 1-of-5 outlier on CRLF / >1MB lines -- FIXED (2026-06-30, bible cross-backend tier 2)
FIXED in Tier 2 (commit 1393d96): Go foldLines now `os.ReadFile` + `strings.Split(data,"\n")` (keeps `\r`,
no cap), matching js/py/rust/beam. Locked by ch560 CRLF fixture (`TestBibleConformanceCRLF`, all backends
sum to 16). All 6 backends (js/go/py/rust/erl/jvm) now agree 6/6 on `\n`-split foldLines. Original report:
`codegen/golang.go` foldLines used `bufio.Scanner`/`ScanLines`, which (a) stripped a trailing `\r` and
(b) caps a line at 1MB (an over-long line silently stops the scan, dropping it + everything after).
The js/py/rust/beam foldLines split on `\n` only (keep `\r`, no cap) -- so 4 of 5 agree and GO is the
lone outlier on CRLF input or any line >1MB. LATENT: no manifestation on the Unix/LF bible corpus, and
Go IS in the byte-identity divergence-lock so any ACTIVE manifestation fails the build. Frozen this tier
(golang.go untouched). FIX in the cross-backend tier that may touch golang.go: switch Go's foldLines to
`split on \n` semantics (keep `\r`, raise/remove the line cap) so all 5 backends agree 5/5; optionally add
a CRLF + a >1MB-line fixture to the conformance gate to make the divergence visible. Tracks the "no
divergence" telos to its last latent corner.

## JS printStrCode double-encodes non-ASCII on stdout (2026-06-30, bible Milestone C)
The JS `printStrCode` body (codegen/js.go) does `console.log(__s2h(c))`, which re-encodes the
latin1 byte-string as utf8 on stdout -- diverging from Go's raw-byte `fmt.Println` for any byte
> 127. The Milestone-C write/read vocabulary (writeFileCode/writeChunk/sortFile/readFileCode) was
made latin1-symmetric, but the STDOUT path was not (no non-ASCII stdout consumer in the bible port
-- the builders write to files, not stdout). FIX before a non-ASCII stdout consumer: a stdout-
encoding pass writing raw bytes (e.g. `process.stdout.write(Buffer.from(__s2h(c),'latin1'))`).
Park (Standing Rule 1, no consumer).

## JVM dbApply does not drain the sqlite3 child's stdout/stderr (2026-06-30, bible cross-backend tier 2)
JVM `dbApply` (codegen/jvm.go) is `new ProcessBuilder("sqlite3", db, ".read "+sql).start().waitFor()`
without redirecting/draining the child's stdout+stderr. Go's `exec.Command(...).Run()` discards them
(nil Stdout -> /dev/null). If a future bible SQL script ever wrote >~64KB to stdout, JVM's `waitFor()`
could DEADLOCK on a full pipe buffer where Go would not. LATENT: current build scripts are pure silent
DDL/DML, so the real-data gate passes and there is no divergence today. FIX if touched again:
`.redirectOutput(DISCARD).redirectError(DISCARD)` on the ProcessBuilder. Park (Standing Rule 1, no consumer).

## JVM foldDir/sortFile FILENAME order is UTF-16 code-unit, not byte order (2026-06-30, bible cross-backend tier 2)
JVM `_foldwalk`/`sortFile` order ENTRIES by `String.compareTo` (UTF-16 code-unit order); Go uses byte
order (filepath.WalkDir / sort.Strings). Diverges only for non-ASCII FILENAMES. LATENT: bible filenames
are ASCII (book/chapter ids), and file CONTENT ordering is byte-exact (latin1 codec, proven 6-way). FIX
if a non-ASCII-filename consumer appears: sort filenames by their latin1/byte encoding. Park (no consumer).

## JS write vocabulary emits Node default utf8, not raw bytes (2026-06-29, bible Milestone B)
The JS host bodies for the write ops -- `writeFileCode` (codegen/js.go), and the Milestone-B
`writeChunk`/`sortFile` output -- pass the latin1 byte-string straight to `fs.writeSync`/
`writeFileSync`, which re-encodes it as utf8. Go writes the raw bytes. For any written byte > 127
the two backends DIVERGE (the same utf8-vs-latin1 class that bit Milestone A on the READ side).
NOT triggered today: every byte the shared-root builder writes (`root\tstrong` + the ASCII edge
template) is ASCII, where utf8 == latin1 == raw, so the Go byte-identical gate and the js+go
fixture gate are both honest; the new ops match the existing `writeFileCode` precedent rather than
introducing a one-off. FIX when a non-ASCII WRITE consumer appears: emit `Buffer.from(str,'latin1')`
across the WHOLE write vocabulary at once (writeFileCode + writeChunk + sortFile), so they stay
consistent. No consumer now -> parked (Standing Rule 1).

## Native GC gc_find_obj is O(N_live) -- codec-over-corpus impractically slow (2026-07-01, bible cross-backend tier 3)
The native C/LLVM runtimes' conservative stack scan calls `gc_find_obj` (codegen/c.go), an O(N_live)
LINEAR scan per stack word. The bible builders allocate ~8000 bignums per file (the packed-String codec
+ splitOn/jsonStrField cells); over the real-data lexicon (~1500 sampled files) GC dominates -- native runs
at ~30s/entry (the source backends finish in seconds), so `TestBibleConformanceRealData` at N=1500 times out
on c/ll. NOT a correctness bug: native byte-identity on real Greek+Hebrew is PROVEN by the synthetic 8-way
`TestBibleConformanceBuilders` gate (lexdbfix fixture holds Hebrew `אב` + Greek `Α` routed through
jsonStrField/sqlQuote/sortFile -> lexicon.sql, sha256 c4246e3 identical across all 8 backends) + by
construction (raw-byte codec, no charset re-encode possible). So the RealData SCALE gate skips c/ll with a
logged reason (harness/bible_conformance_test.go). FIX (a real perf item, already flagged in c.go's GC
comment as "a sorted-bounds / bitmap speedup is a later perf item"): replace gc_find_obj's linear scan with
a sorted-interval tree or an allocation bitmap so conservative marking is sub-linear. Park (Standing Rule 1,
no perf consumer -- the corpus ETL runs go-only in production; native is a conformance target, not the ETL).

## WASM foldDir suffix window aliased the shared codec scratch -- FIXED (2026-07-02, bible cross-backend tier 4)
`foldDir`'s WAT emission (`emitFoldDirWasm`, codegen/wasm.go) decoded the walk's dir path into `$D6BUF`
and its suffix into `$D6BUF2` -- the SAME scratch windows `jsonStrField`/`splitOn`/`sqlQuote`/`byteLen`
use as their own throwaway decode targets. Because the user's step closure runs INSIDE the recursive walk
and (in ch555/ch559) calls `jsonStrField` per file, the first matched file's step call clobbered
`$D6BUF2`, so `$d6_foldwalk`'s per-entry suffix comparison (`d6_memeq(..., sfxPtr=$D6BUF2, ...)`) silently
failed for every entry after the first -- ONLY the first file in sort order was ever folded. Caught live
by the new 9-way `TestBibleConformanceBuilders` divergence-lock (joining WASM to `bibleBackends()`):
ch555's shared-root.out came back empty (a lone unpaired entry) and ch559's lexicon.sql held only the
first INSERT, both diverging from the other 8 backends' byte-identical output. FIXED by giving foldDir
its own dedicated, never-shared decode windows `$D6FDDIR`/`$D6FDSUF` (codegen/wasm_runtime.go, at the top
of the reserved low region, `$hp` bumped from 1837056 to 1968128) that stay stable for the whole walk
regardless of what the applied step decodes elsewhere. Verified: `TestBibleConformanceBuilders` now passes
9-way byte-identical (sha256 cff27bc shared-root / c4246e3 lexicon.sql) with WASM included.

## `rune run` does not forward stdin to the child (2026-07-02)
`echo 7 | rune run listings/ch211_io_stdin.rune main --target js` prints `0\n0` (the child's
getNat reads EMPTY stdin), while emitting + running the same program directly under node prints
`7\n7`. cmd/rune's run path does not wire os.Stdin into the child process, so INTERACTIVE
programs must be `rune emit`ed and executed directly (which is how the conformance gates do it
-- they were never affected). FIX when an interactive consumer appears: set cmd.Stdin = os.Stdin
in the run child-process setup. Park (Standing Rule 1, no consumer -- all current stdin listings
are gate-driven via direct execution).

## WASM show buffer is fixed-size (4096 bytes) -- oversized values TRUNCATE (2026-07-02, 6c final review)
`$show`'s scratch buffer occupies the fixed window [4096,8192); the freelist bucket heads sit
immediately past it at [8192,8484) (see the $freelist layout comment in codegen/wasm_runtime.go).
Before this review `$emit_byte` had no ceiling, so a value whose rendering exceeded the 4096-byte
window (e.g. a 2000-byte all-0xFF `Bin` shown via `printBin`, whose `\xNN` expansion is ~8000
emitted bytes) walked the cursor straight through the freelist heads, corrupting them; the next
allocation then popped a garbage bucket head and wasmtime trapped. FIXED by clamping `$emit_byte`
(the shared bottleneck every show/emit_* helper writes through) to stop writing -- and stop
advancing the cursor -- once it reaches the 8192 ceiling, so an oversized show TRUNCATES instead
of corrupting the heap (a deliberate divergence from the other backends, which render the value in
full: truncation beats heap corruption). Regression: codegen/wasm_arc_test.go
`TestARCBinShowClampNoCorruption` (a 2000-byte 0xFF Bin, show, release, then allocate + round-trip
a fresh Bin -- traps without the clamp, passes with it). REMAINING (honest limitation, not fixed
here): the window is still fixed-size, so a show that needs more than 4096 bytes on WASM will not
match the other backends' full-length output. FIX if a large-show consumer appears: a growable or
heap-allocated show buffer (bump-allocate a scratch block sized to the value instead of a fixed
low-memory window). Park (Standing Rule 1, no consumer today -- the 8-way conformance corpus, incl.
ch483, fits the window).

## Real-WASM-state-through-initial-sync is untested in a real browser (2026-07-03, 6f Task-3 review follow-up)
The puppeteer gate (`examples/twotab/e2e.mjs`) waits for both pages to report "connected" before it
clicks bump on either page, so the DOM is never asserted at the moment the initial G-Counter state
(the WASM-encoded bytes from `gcToBin`) actually crosses the wire -- only post-bump convergence is
checked. The initial send happens in the data channel's real `onopen` handler
(`examples/twotab/sync.js:30`: `dc.onopen = () => { onStatus("connected"); sendState(getState()); };`).
`sync_test.mjs` scenario B (`examples/twotab/sync_test.mjs`, `scenarioB`) does cover the PROTOCOL path
-- each side's `getState` is called once and delivered to the peer's `onPeerState` on open -- but it
runs entirely under node with fake string state (`"state-A"`/`"state-B"`), not real wasm-encoded `Bin`
bytes through a real `RTCDataChannel`. So a regression that corrupted the pre-connect state bump (e.g.
an ownership bug in `app.js`'s retain-before-consume discipline specific to the very first send) could
slip past both gates. FIX if initial-sync ever regresses: add a puppeteer scenario that asserts each
page's `#count` immediately after "connected" (before either page's bump button is clicked), so the
real `onopen` send is exercised end-to-end. Park (Standing Rule 1, no manifested bug -- e2e's post-bump
assertions pass and sync_test's scenario B covers the protocol shape).
Two adjacent gaps from the whole-branch review, same disposition: (1) the RUN.md kill-reopen re-sync
claim is pinned only by the fakes (`sync_test.mjs` scenario F fires `onclose` manually); a real
browser tab-kill's disconnect timing is untested -- fold a reopen step into the same future puppeteer
scenario. (2) equal-session-id deadlock (`examples/twotab/sync.js:14`: two tabs drawing the same
`Math.random().toString(36).slice(2)` id drop each other's hello as self-echo and neither wins the
offerer election); probability ~1e-17 per pair, inherent to single-random-id negotiation, cosmetic
for a demo.

- **WASM off-corpus float rounding.** The WASM backend's printFloat uses a
  hand-built WAT precision-search formatter in `codegen/wasm_float.go` that
  matches ECMAScript's Number::toString on the divergence-lock corpus
  (ch566-ch568). Parked residue: `p >= 16` formatting paths and `|k| > 22`
  parsing are not correctly rounded on edge-case inputs outside the lock corpus;
  parked until a WASM consumer exposes a concrete mismatch.

- **Native show %g vs __fmtf divergence.** The C/LLVM backends use printf %g
  for float display which can differ from the ECMAScript canonical form on
  some values (subnormals, trailing zeros); parked, no native-backend consumer
  for float IO display parity.

- **Go backend getFloat (bufio __stdinRdr) vs getNat (fmt.Scan) stdin mixing.**
  The Go backend uses a `bufio.Reader` (__stdinRdr) for `getFloat` and `fmt.Scan`
  for `getNat`; mixing the two in one program can drop buffered bytes already read
  by the bufio reader before `fmt.Scan` sees them. No current consumer mixes them
  (ch566 and the double demo use only `getFloat`); parked until a consumer that
  calls both in one program appears.

- **Prelude Std namespacing.** The always-on prelude exports Float ops as
  Std.Float.fromNat etc.; a future cleanup would shorten these to Float.fromNat
  without breaking existing listings; parked until a user-facing DX complaint
  arises.

- **Builtin-accel cross-registration via the always-on prelude (FIXED, two
  gates: declaring-name provenance + data-group drift).** Structurally
  identical datatypes and defs hash equal in de Bruijn core, which opened two
  hash-equal-shadow variants that both used to emit native arithmetic onto a
  constructor-record data group (BEAM badarith, JS `[object Object]` concat,
  Go interface-conversion panic). Variant 1, op-def shadow: a user
  eliminator-shaped `add` identical to the prelude's `addW` inherited the
  `builtin natAdd` export by hash. Fixed by the PROVENANCE GATE:
  `AddBuiltinNatOp` records the declaring def name (`natAccelDecl`) and
  `emitDefs` exports a `NatSpec.Ops` entry only when the hash's emitted name
  equals it. Variant 2, data-group shadow: a user file redeclaring only
  `data Nat is zero | succ end` (hash equal to the prelude's `Whole`) while
  calling the UNSHADOWED `addW` directly; the shared group hash emits under
  the latest name (`NatElim`), mismatching `p.Nat.ElimName` (`WholeElim`), so
  the group compiles to records, yet `addW`'s Ops export legitimately passed
  the provenance gate and its call sites emitted native `+` on records. Fixed
  by the DRIFT GATE: `emitDefs` (`natGroupEmitsNatively`) checks that the
  binding's former, constructors, and eliminator all still emit under the
  binding's own names; on drift the whole Ops export is suppressed and the
  program compiles through the ordinary eliminator loop. Numeral decision:
  numeral literals (codegen `LitNat`) always emit the backend's NATIVE
  integer, which is meaningless among records, so a drifted program that
  still contains a numeral after tree-shaking is REFUSED with a clear
  emit-time error (`checkNatLitDrift` in EmitProgram and EmitExpr) rather
  than silently miscompiled; `p.Nat` itself stays set so undrifted numeral
  emission is untouched. Tested: nataccel_test.go locks the provenance gate
  (positive export, hash-equal shadow drop, decl-name rebind drop, BEAM
  eliminator-call shape) and the drift gate (Ops suppressed + eliminator-call
  shape for the pure-constructor variant, emit-time refusal for the numeral
  variant); CLI conformance was verified by hand on erl/go/js (`rune run`,
  `rune emit`, `rune build`): the pure-constructor repro prints
  `succ (succ (succ zero))` and the normal prelude case still emits `(2 + 3)`
  and runs to 5. The kernel/normalizer accel stays hash-keyed, sound by the
  registration-time differential gate. History: the delivery path
  (`rune build`/`rune deploy`) had already been closed by demand-driven
  prelude loading (`sourcesNeedPrelude`); that mechanism stays, and the two
  emit-time gates close the `rune run`/`rune emit` reach (and `rune build`
  under an explicit `import Std`).

- **`sourcesNeedPrelude` substring conservatism + run-vs-build asymmetry.**
  `import StdX` (any module name starting with `Std`) loads the prelude for
  build/deploy unnecessarily (harmless, mirrors the `builtin nat` substring
  rule). Qualified `Std.Float.fmul` WITHOUT an `import Std.Float` directive
  works under `rune run` (always-on) but not `rune build` (demand-driven); the
  documented pattern (`import Std.Float`) works everywhere. Parked with this
  note as the record.

- explain --annotate runtime TTY width detection (x/term.GetSize): parked to keep the dependency graph closed; --width flag (default 80) is the deterministic override and goldens fix both layouts.
