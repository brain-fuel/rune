# R-PROTO — Verified distributed protocols

> **AS BUILT (E4 first slices, v3.230 to v3.240).** The better-than-Winglang
> simulator landed as `internal/sim` (shadow tooling, a consumer of the kernel,
> never core): `Simulate(sess, init, merge, value, n, rounds, FaultPolicy)` folds a
> protocol's OWN verified Rune operations forward under a deterministic FaultPolicy
> (`Partitioned`, `Duplicate`, `Crashed`), and `Render` prints the per-step trace.
> The gates hold by observed behaviour: SAFETY (a G-Counter diverges under a
> partition and reconverges to the correct total), ROBUSTNESS (drop+dup leave the
> outcome unchanged, idempotence), DURABILITY (a crashed replica recovers without
> losing its pre-crash increment), while a last-writer-wins register stays
> divergent. `Diagnose` is the observational CvRDT LAW LINTER (samples states,
> checks merge commutativity/idempotence/associativity, names the violated law with
> a witness); `Stabilize` is the LIVENESS check (reaches a global fixpoint under
> fair ring gossip within n-1 rounds for a join, never for a non-join). Surfaced as
> `rune simulate <file> [replicas]` (convention init/merge/value/op0..opN, prints
> the trace + law report + a LINTER-AUTHORITATIVE verdict). Examples: gcounter
> (PROVES convergence AND simulates AND deploys `converged`=3 on every backend incl
> BEAM/erl, the triad from one source), gcounter3 (N-way), gset (OR-join), pncounter
> (compound P/N, inc+dec), lww + badcounter (non-CvRDTs the linter catches). The E3
> all-P/all-fuel invariant landed as listing ch229 (projectVisible/visibleRunVisible
> + projectOnlyFail: the projection alphabet is exactly {lfail}). REMAINING:
> live-actor projection onto spawn/send/receive (the D5/B3 OTP runtime tie),
> `protocol ... end` surface sugar + a `deploy` verb.

> Roadmap node **E3** (`E3 [I] verified protocols (consensus/repl/CRDT) +
> projection to actors ⇐ E2, D5`), the **M7/M0 demonstrator**. Telos 4:
> "verified algebra over distributed systems… used to specify and *prove*
> distributed protocols, then project them to runnable actors on the backends.
> 'Better than Winglang': infra-as-code that is provably correct, simulated and
> deployed from the same verified source" (`humble-humming-elephant.md:29-34`).
> Conceptually depends on **R-CALC** (E1: the `Proc` calculus, `Step` LTS,
> structural congruence, the `project` table — `ref_docs/wootz/R-CALC.md`),
> **R-COIND** (C5: `Nu`/`out`/`unfold` to *state* bisimulation —
> `ref_docs/wootz/R-COIND.md`), **R-SUM** (C1: outer Σ for protocol state,
> existentials in `Step`, refinement records — `ref_docs/wootz/R-SUM.md`), and
> the projection runtime **R-EFFECT/R-OTP/B3(BEAM)** (the actor `IForeign` seam —
> `ref_docs/wootz/R-EFFECT.md`). R-CALC delivers the *statement* of adequacy and
> the projection *shape*; **R-PROTO is where a concrete protocol's spec, its
> safety/liveness properties, its proof obligations, and its projection are
> worked out end to end.** This is the node R-CALC.md:528-531 explicitly defers
> to ("the *proof* is E3, per-protocol").

## Problem (what's stuck/absent today, with file:line)

R-CALC laid the calculus substrate but, by its own statement, stopped at the
*statement* of correctness and the *shape* of projection. Everything that makes a
*specific protocol* "verified" is still absent:

- **No protocol spec language.** R-CALC's `Proc` (`ref_docs/wootz/R-CALC.md:177`)
  is raw π/actor syntax — `send`/`recv`/`par`/`nu`/`bang`/`crash`. A protocol is
  not a raw process; it is a *replicated state machine*: per-node local state, a
  set of operations, a merge/transition discipline, and an environment that
  delivers, drops, reorders, and crashes. There is no datatype for "a network of
  `n` replicas each running the same handler over local state `S`", no
  configuration/world type, no scheduler. The R-CALC artifact is a single
  two-process handshake (`ref_docs/wootz/R-CALC.md:456-465`); a *protocol* is a
  parametric family of `n` nodes.

- **No safety/liveness vocabulary.** Today the kernel can state a `Prop`-valued
  relation over data (Phase 4 datatypes + the Eq stratum). But the *correctness
  properties* of a protocol — *agreement* ("no two replicas decide differently"),
  *validity* ("a decided value was proposed"), *convergence* ("all replicas that
  receive the same updates reach the same state"), *eventual delivery* ("under
  fair scheduling every update is eventually applied") — have no stated form.
  Liveness in particular needs a **fairness predicate over infinite executions**,
  which is *coinductive* (an execution is a `Stream Config`), and the outer core
  is inductive-only (`ref_docs/wootz/R-COIND.md:13`, `core/eval.go:82` `ElimSig`,
  ι at `:1317`).

- **No refinement / adequacy proof, only its statement.** R-CALC declares
  `adequacy : (P : Proc) -> Simulates (traceOf (project P)) (Step P)`
  (`ref_docs/wootz/R-CALC.md:447`) and says outright the *proof* is "genuinely
  hard… the heart of E3" (`ref_docs/wootz/R-CALC.md:526-531`). There is no
  runtime-trace semantics for the `IForeign` actor ops, and no proof that the
  projected BEAM actors refine the spec's LTS for *any* concrete protocol.

- **No safety-preserved-by-projection bridge.** Even granting adequacy as a
  schema, nothing connects "the spec satisfies agreement" to "the deployed
  system satisfies agreement". That bridge — *a safety property proved on the
  `Step` LTS transfers along the projection refinement* — is the load-bearing
  theorem of the whole telos-4 claim and does not exist.

- **The projection target is a seam, not a runtime.** R-CALC's Layer-4 table
  (`ref_docs/wootz/R-CALC.md:343-351`) maps `Proc` constructors to
  `primSpawn`/`primSend`/`primReceive`/`primNewChan`/`primSpawnLoop`/`primExit`,
  which are R-EFFECT `IForeign` ops (`ref_docs/wootz/R-EFFECT.md:391`,
  `ref_docs/wootz/R-IR.md:314`). None of B1/B2/B3 (the IR + BEAM backend) exists,
  so today a protocol can be *reasoned about* but not *run* — the M0 "deploys and
  runs on BEAM" bar (`humble-humming-elephant.md:284`) is unmet.

The Thompson constraint is unchanged and total: **a protocol is data and proofs
in Rune.** No `Protocol` core constructor, no consensus baked into the kernel, no
hash-format bump ("No hash-format bump unless new core constructor", CLAUDE.md).
A protocol ships as ordinary datatype declarations + relations + proofs (a
*library*, `lib/proto/`), exactly as quotients (v2), the fibrant layer (v3), and
the §F groups shipped contained — plus a thin codegen-stratum extension (the
actor runtime), where Lambert's reality is allowed to live.

## Prior art (what the literature/other systems do; cite)

**Verified consensus — the two reference points.**
- **Verdi** (Wilcox et al., PLDI 2015) verifies Raft in Coq with *verified
  system transformers* (VSTs): prove the protocol against an idealized network
  semantics, then a *transformer* carries the proof across to a lossy/reordering
  network, and an extracted OCaml shim runs it.
  ([Verdi](https://verdi.uwplse.org/),
  [Verifying Raft in Verdi](https://verdi.uwplse.org/verdi-raft.pdf)). **The VST
  idea is exactly R-PROTO's refinement bridge**: prove on the clean LTS, transfer
  along a semantics-changing transformer to the real (failure) model, then to the
  runtime. Verdi's lesson and warning: the Raft linearizability proof was ~50k
  lines — *pick a small protocol first* (Savage/Thompson).
- **IronFleet** (Hawblitzel et al., SOSP 2015) proves a Paxos-based replicated
  KV in Dafny by *refinement*: a high-level state-machine spec, a distributed
  protocol layer, and an implementation layer, each a refinement of the one
  above, with **TLA-style always/eventually** for liveness.
  ([IronFleet](https://www.microsoft.com/en-us/research/publication/ironfleet-proving-practical-distributed-systems-correct/)).
  **The three-layer refinement stack is the skeleton R-PROTO adopts**: spec SM →
  protocol LTS (`Step`) → projected actors, each refining the last.

**CRDTs — the *right first protocol* for this substrate.**
- Shapiro et al., *Conflict-free Replicated Data Types*
  ([INRIA RR-7687](https://inria.hal.science/inria-00609399v1/document)) prove the
  **CvRDT/CmRDT convergence theorem**: a state-based CRDT whose states form a
  *join-semilattice* and whose merge is the *least upper bound* is **Strong
  Eventually Consistent** — replicas that have delivered the same set of updates
  have equal state, *regardless of order, duplication, or delay*. This is a
  **purely algebraic safety property** (commutativity + associativity +
  idempotence of merge ⇒ convergence) — no liveness, no consensus, no coinduction
  needed for *safety*. **That is why a CRDT counter is the M0 demonstrator**: its
  convergence proof is a one-screen algebra proof over a `data`/eliminator
  semilattice, provable in *today's* kernel modulo R-SUM, with failure
  (reorder/dup/drop) as the *hypothesis the theorem is robust to* rather than an
  obstacle.
- Mechanized CRDT proofs exist: **Gomes et al., *Verifying Strong Eventual
  Consistency in Distributed Systems*** (Isabelle/HOL, OOPSLA 2017,
  [paper](https://www.cl.cam.ac.uk/~vb358/strong-eventual-consistency.pdf))
  factor SEC into a network model + a per-CRDT commutativity obligation — the
  exact *proof-obligation decomposition* R-PROTO reuses.

**Process-calculus / refinement substrate.**
- **Bisimulation/weak-simulation as refinement.** "Projected actor refines the
  spec LTS" is a *weak simulation* (R-CALC.md:362-368), the coinductive relation
  R-COIND makes statable (`Nu`/`out`/`unfold`,
  `ref_docs/wootz/R-COIND.md:230`). Safety transfers *backward* along a
  simulation: if every concrete trace is matched by an abstract trace, an
  abstract safety invariant restricts the concrete behavior. This is the standard
  *trace-inclusion = refinement* result (Lynch–Vaandrager, *Forward and Backward
  Simulations*) — adopt it.
- **Fail-stop, asynchronous, reordering** is the model R-CALC already adopted
  (`ref_docs/wootz/R-CALC.md:113-116`) from Hennessy–Riely distributed π and
  crash-stop MPST — and it is the BEAM/OTP model. R-PROTO inherits it verbatim.

**Infra-as-code (the C-INFRA tie).** Winglang's preflight/inflight + simulator
(`ref_docs/wootz/R-CALC.md:126-136`) maps to **preflight = the proved spec + its
invariant certificate; inflight = the projected `IForeign` actors; simulator = a
kernel `Step`-scheduler**. "Better than Winglang" = the inflight carries a
machine-checked safety proof, and the *same source* simulates and deploys.

Cross-cutting lessons:
- **Refinement stack (spec SM → protocol LTS → actors), each layer a
  simulation** (IronFleet/Verdi) — the R-PROTO skeleton.
- **Safety-first, liveness-later.** CRDT *convergence is pure safety algebra* (no
  coinduction); consensus *agreement is a safety invariant*; only *liveness*
  (eventual delivery, leader election terminates) needs the coinductive
  fairness/execution machinery. Stage accordingly.
- **Pick the small protocol.** Verdi's 50k-line Raft is the cautionary tale; the
  G-Counter convergence proof is one screen. M0 = CRDT counter; the Raft-like
  single-decree consensus is the *labelled-harder* second artifact.

## Chosen approach for THIS substrate (concrete; respects containment)

**Recommendation: a three-layer refinement stack, entirely as a Rune library
(`lib/proto/`) over R-CALC's calculus, with the CRDT G-Counter as the M0
demonstrator (safety = pure semilattice algebra, provable now modulo R-SUM) and a
single-decree Raft-like consensus as the labelled-harder M7 artifact (safety =
agreement invariant; liveness = fairness over coinductive executions, gated on
R-COIND C5b). Zero outer-core growth: the spec is a record (R-SUM Σ), the network
is a datatype, the LTS is a relation, the properties are `Prop`s, the proofs are
ordinary Rune terms, and the projection is R-CALC's `project` over the
R-EFFECT/R-OTP `IForeign` seam.**

The stack, top to bottom — each layer refines the one above:

```
  L0  Spec state machine     SpecSM    :  a record (R-SUM Σ): state, step, decided  [pure]
        │  abstraction relation α + simulation proof  (safety lives here)
  L1  Protocol LTS           Step      :  R-CALC's network-of-replicas relation      [pure]
        │  adequacy: project P weak-simulates Step P  (R-CALC's statement; proved here)
  L2  Projected actors       project P :  IForeign spawn/send/receive on BEAM        [runs]
```

This is the IronFleet/Verdi skeleton, contained.

### Layer 0 — the protocol spec as a replicated-state-machine record

A protocol is parametric over a per-replica **local state** `S`, a set of
**operations** `Op`, and (for convergence-style protocols) a **merge**. R-SUM's Σ
gives the bundling; nothing here touches the kernel.

```
-- A state-based (Cv) replicated data type spec — the CRDT shape.
-- This is a RECORD (R-SUM Σ); `lib/proto/crdt.rune`.
CvRDT : U is
  Sig (S    : U)              -- replica-local state
    ( Sig (init  : S)
    ( Sig (merge : S -> S -> S)     -- the join
    ( Sig (Op    : U)
    ( Sig (apply : Op -> S -> S)    -- local effect of an operation
          -- the LATTICE LAWS that make convergence a theorem (Shapiro):
    ( Sig (mergeComm  : (a b   : S) -> Eq S (merge a b) (merge b a))
    ( Sig (mergeAssoc : (a b c : S) -> Eq S (merge a (merge b c)) (merge (merge a b) c))
    (       mergeIdem  : (a     : S) -> Eq S (merge a a) a) )))))))
```

The G-Counter instance: `S = Vec n Nat` (per-replica tallies), `merge =
zipWith max`, `apply (inc i) v = v[i]++`, `init = replicate n 0`. The three
lattice laws are `max`-comm/assoc/idem lifted pointwise — *one-screen proofs by
the `Nat`/`Vec` eliminators* (D1/D2 stdlib). **This is the M0 artifact's safety
core and it is provable in today's kernel the moment R-SUM lands** — no
coinduction, no Kan, no `Proc` even: convergence is an algebra theorem about the
record's fields (below).

### Layer 1 — the protocol LTS (a network of replicas) as a relation

A **configuration** is the global state of `n` replicas plus the in-flight
message bag (R-CALC's async model: a message in flight is a free-floating term).
This is a datatype + a `Step` relation, both ordinary Rune, no core change:

```
-- lib/proto/net.rune  — built over R-CALC's Proc/Step idioms.
data Config : U is                       -- a snapshot of the whole system
  mkConfig : (states : Vec n S)          -- each replica's local state
          -> (inflight : Bag (Fin n × Msg))   -- messages in transit (Bag = unordered)
          -> (down : Fin n -> Bool)      -- which replicas have crashed (fail-stop)
          -> Config

-- The LTS over configurations. A Prop/U-valued relation (R-SUM Σ for existentials).
-- Each constructor is a network event; the FAILURE rules are the Lambert model,
-- inherited verbatim from R-CALC (CRASH/LOSS/REORDER/DETECT, R-CALC.md:246-249).
Step : Config -> Config -> U
  -- LOCAL   : replica i applies op o          states[i] := apply o states[i], emit gossip
  -- DELIVER : an in-flight (j, m) reaches i    states[i] := merge states[i] (payload m)
  -- DROP    : an in-flight message is lost      (Lambert LOSS)
  -- DUP     : an in-flight message is duplicated (Lambert: reorder/dup are free here)
  -- CRASH   : replica i goes down               down[i] := true (fail-stop)
  -- (REORDER is structural: inflight is a Bag, so order is quotiented away.)
```

Two substrate-specific moves:
- **`inflight` is a `Bag` (multiset).** Wootz *has* multisets as a first-class
  quotient (CLAUDE.md v2.0.0 `Quot`; listings ch06-08). Modelling the network as
  a `Bag` makes **reordering definitionally free** — two configurations differing
  only in message order are *equal*, not merely bisimilar. This is a genuine
  substrate advantage over the Coq/Isabelle formalizations, which carry an
  explicit "network is a set/multiset" axiom; here it is the v2 quotient. `DUP`
  and `DROP` are `Bag`-insert/remove. (Honest note: `Eq` is *stuck* at a quotient
  type unless introduced by `qsound` — so reasoning that uses bag-equality threads
  `qsound`/`qind` evidence, exactly the v2 discipline.)
- **`Step` as a relation, encoded the R-CALC.md:498-506 way:** a `Prop`-valued
  recursive predicate over the data, using R-SUM's Σ/× for the per-rule
  existentials ("there exist `i,o` such that …"). Indexed inductive families would
  be cleaner but are parked (PARKING-LOT.md); the Σ-encoding is the
  *ready-to-build* path and is why **C1/R-SUM is a true blocker even to state the
  protocol**.

An **execution** is a (possibly infinite) sequence of `Step`s. For *safety* it is
an inductive `Reachable` (finite prefix); for *liveness* it is a coinductive
`Exec : Config -> U` = `Nu (\X. Σ c' (Step c c' × X))` (R-COIND's `Nu`,
`ref_docs/wootz/R-COIND.md:230`). **Safety needs only `Reachable` (inductive,
ready now); liveness needs `Exec` (coinductive, gated on R-COIND C5a to state,
C5b for path-fairness ergonomics).**

### Layer 1 — the properties (safety & liveness) and their proof obligations

**Safety (an invariant on all reachable configs):**

```
-- CONVERGENCE (CRDT SEC, the M0 safety theorem): two replicas that have merged
-- the same set of deliveries have equal state. Stated as: state is a function of
-- the delivered-op SET (order-independent), which by the lattice laws is a fold
-- of merge/apply that is invariant under permutation & duplication.
convergence :
  (cfg : Config) -> Reachable cfg
  -> (i j : Fin n)
  -> SameDeliveries cfg i j           -- i and j have applied the same op multiset
  -> Eq S (states cfg ! i) (states cfg ! j)

-- AGREEMENT (consensus safety, the M7 artifact): no two replicas decide
-- differently.
agreement :
  (cfg : Config) -> Reachable cfg
  -> (i j : Fin n) -> (vi vj : V)
  -> Decided cfg i vi -> Decided cfg j vj
  -> Eq V vi vj

-- VALIDITY (consensus): a decided value was proposed by someone.
validity :
  (cfg : Config) -> Reachable cfg -> (i : Fin n) -> (v : V)
  -> Decided cfg i v -> Exists (\j. Proposed cfg j v)
```

The **proof-obligation decomposition** (Gomes et al. / Shapiro): safety factors
into (a) a *network-independent* obligation discharged once in `lib/proto/`
(`Reachable`-induction: the invariant holds at `init` and is preserved by every
`Step` constructor) and (b) a *per-protocol algebra* obligation (the lattice laws
for CRDT; the quorum-intersection lemma for consensus). For the G-Counter, (b) is
the three `merge` laws *already in the `CvRDT` record* — so `convergence` is
`Reachable`-induction whose preservation step is *exactly* `mergeComm`/`Assoc`/
`Idem` + the `Bag`-permutation invariance the v2 quotient gives for free.

**Liveness (eventual delivery / progress, gated on R-COIND):**

```
-- under FAIR scheduling, every in-flight message is eventually delivered.
Fair : Exec cfg -> U                    -- coinductive: every enabled DELIVER fires eventually
eventualDelivery :
  (cfg : Config) -> (e : Exec cfg) -> Fair e
  -> (m : Fin n × Msg) -> InFlight cfg m
  -> Eventually (\c. Delivered c m) e   -- Eventually = an inductive "within finitely many steps"
                                        -- over the coinductive Exec (mixed μ/ν)
```

`Eventually` is **inductive** (finite-step), threaded over the **coinductive**
`Exec` (`Nu`) — a *mixed inductive/coinductive* property, the canonical liveness
shape. This needs R-COIND C5a to *state* `Exec`/`Fair`, and the up-to /
path-fairness ergonomics want C5b (gated on M2). **Convergence/agreement (safety)
do not need any coinduction** — that is the staging that makes M0 reachable.

### Layer 2 — projection to actors and the adequacy bridge

A spec/`Proc` is *run* by R-CALC's `project : Proc -> IO Unit` over the
R-EFFECT/R-OTP `IForeign` seam (`ref_docs/wootz/R-CALC.md:343-351`). R-PROTO adds
two things R-CALC left as statement-only:

1. **A *protocol-shaped* projection** `runReplica : CvRDT -> Fin n -> Proc`
   compiling one replica to a `bang (recv c (\m. ... merge ... gossip))` loop —
   the standard "receive, merge, re-gossip" actor — and `runSystem : CvRDT ->
   Proc` = the `par`/`nu` composition of `n` replicas. Each constructor maps
   through R-CALC's table; on BEAM each replica is a real OTP process, `crash` is
   process death + supervisor restart, `merge`-on-receive is the message handler.

2. **The adequacy + safety-transfer theorem** (the heart R-CALC deferred):

```
-- runtime-trace semantics of the projected actors (over IForeign ops):
RunTrace : Proc -> Stream Event -> U          -- coinductive (R-COIND): observable runtime

-- ADEQUACY: every runtime trace of the projection is matched by a Step-execution.
-- This IS R-CALC's `adequacy` (R-CALC.md:447), here PROVED for the CRDT shape.
adequacy :
  (proto : CvRDT)
  -> WeakSimulates (RunTrace (runSystem proto)) (Step (configOf proto))

-- SAFETY TRANSFER: a Reachable-invariant of the spec restricts every runtime trace.
-- This is the load-bearing telos-4 theorem: "proved safe ⇒ deployed safe."
safetyTransfer :
  (proto : CvRDT) (I : Config -> Prop)
  -> ((cfg : Config) -> Reachable cfg -> I cfg)        -- I is a Step-invariant (safety proof)
  -> adequacy proto
  -> (ev : Stream Event) -> RunTrace (runSystem proto) ev
  -> AlwaysHolds I ev                                    -- I holds along the real run
```

`safetyTransfer` is *backward simulation transfer* (Lynch–Vaandrager): adequacy
makes each runtime step shadowed by a `Step`, and `Reachable`-closure of `I`
restricts the shadow, hence the run. **This is where "verified algebra over
distributed systems" becomes true**: the CRDT convergence proof, established on
the clean LTS, is *carried to the deployed BEAM system* by `safetyTransfer ∘
adequacy`. `WeakSimulates`/`RunTrace` are coinductive ⇒ this layer is **gated on
R-COIND** (C5a to state, C5b for the proof ergonomics); the *spec-level safety*
(convergence/agreement) is not.

### Why this respects all three smiths

- **Thompson:** zero outer-core constructors. The spec is an R-SUM record; the
  network is a `data Config`; the LTS, `Reachable`, the invariants, and the
  properties are `Prop`s; the proofs are ordinary terms; `Exec`/`RunTrace`/
  `WeakSimulates` ride R-COIND's contained `Nu` group; the runtime is R-CALC's
  `project` over R-EFFECT `IForeign` codegen ops. No hash-format bump (no new
  `core.Tm`). The whole protocol library is content-addressed exactly like the
  listings corpus.
- **Savage:** the on-ramp is the furnace path made literal — **simulate, then
  prove**: run the G-Counter in the kernel `Step`-scheduler and *watch* two
  replicas diverge then re-converge after gossip; *then* prove `convergence` by
  the three one-line lattice laws; *then* `project` and watch it on BEAM. The
  CRDT is chosen precisely because its safety proof is one screen of algebra a
  learner can hold in their head (Verdi's 50k-line Raft is the contrast artifact).
- **Lambert:** failure is the model, not an idealization — `DROP`/`DUP`/`CRASH`
  are the spec's *hypotheses*, and convergence is a theorem *robust to all of
  them* (the CRDT point). `safetyTransfer` carries the proof to a real BEAM
  deployment with real OTP supervision; the *same source* simulates and deploys.

## Interfaces & signatures to add (Go + Rune surface as relevant)

**Go — strikingly little, all reused.** R-PROTO adds **no new Go to core/store**:
the spec is R-SUM Σ, the network is `DataDeclOf` datatypes, the coinductive parts
are R-COIND's `Nu` group, the runtime ops are R-CALC/R-EFFECT `IForeign`. The
only Go is *registering protocol-runtime op names* in the codegen vocabulary
(already R-CALC's list) and a **kernel `Step`-scheduler** for the simulator —
which is shadow tooling, not core:

```go
// internal/sim/scheduler.go  (NEW; shadow tooling, NOT core/store) — the
// "better-than-Winglang simulator". Folds the Step relation under a pluggable
// scheduling/fault policy to drive a Config forward, surfacing each event for the
// REPL/CLI to print. Reuses the kernel evaluator to DECIDE Step membership; it is
// a consumer of core, never a part of it.
type FaultPolicy struct {
    DropRate, DupRate float64
    Crash             func(step int) (replica int, ok bool)
    Partition         func(step int) (cut [][]int, ok bool) // derived: LOSS across a cut
}
func Simulate(sess *session.Session, cfg core.Tm, pol FaultPolicy, steps int) []Event

// codegen — the actor-runtime IForeign op names R-CALC named (R-CALC.md:418),
// reused, no new IR node:
//   "spawn","send","receive","newChan","spawnLoop","exit","monitor","restart"
// ("monitor"/"restart" are the OTP supervision ops R-OTP/D5 implements; R-PROTO
//  only names them in the projection vocabulary.)
```

**Rune surface — a *library* `lib/proto/` (NOT core), layered:**

```
-- lib/proto/crdt.rune     (L0 spec; needs R-SUM Σ)
CvRDT  : U                                            -- the record above
gcounter : (n : Nat) -> CvRDT                          -- the G-Counter instance
convergence : (proto : CvRDT) -> (cfg : Config)
            -> Reachable proto cfg -> SameDeliveries cfg i j
            -> Eq (stateOf proto) (states cfg ! i) (states cfg ! j)   -- M0 safety theorem

-- lib/proto/net.rune      (L1 LTS; needs R-SUM Σ, v2 Bag/Quot)
data Config : U is mkConfig ... end
Step        : CvRDT -> Config -> Config -> U
Reachable   : CvRDT -> Config -> U                     -- inductive (finite prefix) — SAFETY
Exec        : CvRDT -> Config -> U                     -- coinductive Nu — LIVENESS (R-COIND)
Fair        : Exec proto cfg -> U
Eventually  : (Config -> U) -> Exec proto cfg -> U     -- mixed μ over ν

-- lib/proto/consensus.rune (L1 the harder artifact; agreement/validity)
singleDecree : (n : Nat) -> (quorum : Nat) -> CvRDT-like  -- Raft-like single value
agreement    : ... -> Eq V vi vj
validity     : ... -> Exists (\j. Proposed cfg j v)

-- lib/proto/project.rune   (L2 projection + adequacy; needs R-CALC project, R-COIND)
runReplica   : CvRDT -> Fin n -> Proc
runSystem    : CvRDT -> Proc
project      : Proc -> IO Unit                          -- R-CALC's table (reused)
RunTrace     : Proc -> Stream Event -> U                 -- coinductive
adequacy     : (proto : CvRDT) -> WeakSimulates (RunTrace (runSystem proto)) (Step proto ...)
safetyTransfer : (proto : CvRDT) -> (I : Config -> Prop)
              -> ((cfg : Config) -> Reachable proto cfg -> I cfg)
              -> adequacy proto -> ... -> AlwaysHolds I ev
```

## Worked micro-example (the teachable artifact)

`gcounter.rune` — a replicated grow-only counter across `n` nodes, **proved
convergent under drop/dup/reorder/crash**, simulated, then deployed on BEAM. The
furnace beat: *simulate, then prove, then deploy.*

```
-- the instance (lattice = (Vec n Nat, pointwise max))
gc : CvRDT is gcounter 3        -- 3 replicas
-- value of a counter = sum of the per-replica tallies
value : Vec 3 Nat -> Nat is fn (v) is sum v end
```

What the learner sees and can verify, in forgeable order:

1. **It's just data + a record.** `gc : CvRDT` type-checks; `gcounter 3` builds
   the record whose three law fields are discharged by `maxComm`/`maxAssoc`/
   `maxIdem` (one-line `Nat`-eliminator proofs). The kernel certifies it like any
   pure def (`AddDef`, cache entry).

2. **Simulate (the better-than-Winglang simulator).** The kernel `Step`-scheduler
   runs three replicas: replica 0 does `inc 0` twice, replica 1 does `inc 1`,
   gossip is **dropped on a cut** (`FaultPolicy.Partition`), so for a while
   `value(states!0)=2, value(states!1)=1` — *the learner watches them diverge*.
   Healing the partition replays gossip; both `merge` to `value = 3`. **Divergence
   then convergence is observable, not abstract** (Lambert's teachable moment) —
   and crucially it *still converges* after arbitrary drop/dup/reorder, because
   `merge` is a join.

3. **Prove convergence.** `convergence gc cfg reach sameDel : Eq (Vec 3 Nat)
   (states cfg ! i) (states cfg ! j)` — a `Reachable`-induction whose preservation
   case is `mergeComm`/`Assoc`/`Idem` plus the `Bag`-permutation invariance the v2
   quotient gives for free. **The proof does not mention drop/dup/reorder at all**
   — they are quotiented or absorbed by idempotence. That is the CRDT magic the
   learner now *owns*: the failure model is a hypothesis the theorem is robust to.

4. **Project & deploy (M0/B3 BEAM).** `rune run gcounter.rune (project (runSystem
   gc))` spawns three BEAM processes exchanging gossip; killing replica 1 (`crash`
   → OTP process death) lets a supervisor restart it, and after re-gossip the
   restarted replica re-converges. `safetyTransfer` certifies that the BEAM run
   *cannot violate convergence* — the proof from step 3, carried to the deployment.
   **The same source that was proved correct now runs.**

Contrast/harder artifact (labelled): `consensus.rune` — single-decree Raft-like
`agreement`. Its safety (`agreement`/`validity`) is a *quorum-intersection*
invariant (still inductive `Reachable`, still ready once R-SUM lands), but its
*liveness* ("a leader is eventually elected and a value decided under fair
scheduling, modulo FLP's failure-detector caveat") needs `Exec`/`Fair`/
`Eventually` — the coinductive machinery — and is the **R-COIND-gated** half. The
contrast teaches the boundary: *CRDT safety is pure algebra; consensus safety is
a quorum invariant; consensus liveness needs coinduction and a fairness/failure-
detector assumption (FLP).*

## Risks / open sub-questions

- **R-COIND is the gating prerequisite for everything coinductive (liveness +
  adequacy).** Convergence/agreement (**safety**) are inductive and ready once
  R-SUM lands. But `Exec`/`Fair`/`Eventually` (liveness), and
  `RunTrace`/`WeakSimulates`/`adequacy`/`safetyTransfer` (the projection bridge),
  are coinductive ⇒ they need R-COIND C5a to *state* and C5b (gated on M2) for
  usable equational/path reasoning. **Status: the CRDT *safety* artifact is
  ready-to-build on R-SUM (+ v2 Bag); the projection bridge and all liveness are
  research-blocked on R-COIND.**

- **R-SUM (C1) is a hard blocker even to *state* a protocol.** The spec record,
  `Step`'s existentials, and `SameDeliveries`/`Decided` all need Σ/×
  (`ref_docs/wootz/R-CALC.md:506` makes the same point for the calculus). No
  protocol — not even the safety half — can be written before C1.

- **Indexed families vs Σ-encoded relations.** `Step`/`Reachable` are *indexed*
  relations; indexed inductive families are parked (PARKING-LOT.md). The
  ready-to-build path is the `Prop`-predicate-over-data encoding (R-CALC.md:498),
  which leans hard on R-SUM. Indexed families (a C-track unpark) would make the
  proofs much cleaner; flag as a long-term ergonomics improvement, not a blocker.

- **The adequacy proof is genuinely hard (R-CALC.md:526 says so).** It needs a
  runtime-trace semantics for the `IForeign` actor ops and a weak-simulation
  proof, and it depends on R-OTP's runtime guarantees + the BEAM scheduler's
  delivery semantics. *Mitigation:* prove adequacy for the **CRDT shape only**
  first (the `bang(recv→merge→gossip)` loop is structurally simple and its
  runtime trace is a stream of merges — the simulation is nearly diagonal), and
  *assume* the BEAM scheduler's per-mailbox FIFO + at-least-once delivery as a
  *stated R-OTP axiom* (R-FFI `assume` tier, `ref_docs/wootz/R-FFI.md`) tracked in
  the certificate's assumption set. General adequacy for consensus is deeper
  research.

- **Liveness needs a fairness *and* a failure-detector assumption (FLP).**
  Consensus liveness is impossible in pure asynchrony (FLP); the spec must
  *assume* an eventually-perfect failure detector / partial synchrony as a
  hypothesis (R-CALC's `DETECT` rule, R-CALC.md:249). This is honest and standard,
  but it means consensus liveness is *conditional* — label it as such in the
  listing. CRDT eventual delivery similarly assumes *fair scheduling* (every
  enabled `DELIVER` eventually fires).

- **`Bag`/`Quot` and stuck `Eq`.** Modelling the network as a v2 `Bag` makes
  reorder free but `Eq` is *stuck* at the quotient type — proofs that rely on
  bag-equality must introduce identifications via `qsound` and eliminate via
  `qind` (the v2 discipline). For the G-Counter the convergence proof is robust
  to this (it goes through `value = sum`, which is a `qlift` respecting the
  permutation relation), but a protocol whose proof needs *definitional*
  bag-equality would hit the parked quotient-effectiveness limit. Flag for
  per-protocol checking.

- **Strict positivity of `Config`/`Exec`.** `Config` is first-order (no problem);
  `Exec = Nu (\X. Σ c' (Step c c' × X))` requires R-COIND's positivity-variant
  check (`ref_docs/wootz/R-COIND.md:365`) — small but not free; inherited from
  R-COIND.

- **C-INFRA surface (open clarify).** The `Simulate` scheduler + `FaultPolicy` is
  the library-first realization of "better than Winglang" (resolves C-INFRA toward
  library-first, matching R-CALC.md:536). Surface sugar (a `protocol … end` block,
  a `deploy`/`simulate` CLI verb) is a later contained `surface/` + `cmd/rune/`
  change, not this node.

## Test/gate plan

- **Containment gates (Thompson, must pass):** no change to `core/term.go`,
  `core/hash.go`, `defFormatVersion`/`hashFormatVersion`; a test asserts an
  existing pure def's hash is byte-identical after `lib/proto/` + R-COIND register
  (the §F groups already prove this is achievable). `CvRDT`/`Config` are ordinary
  `DataDeclOf`/R-SUM-record declarations — a `structural_test.go`-style test that
  they declare, check, and that `gcounter 3` discharges its law fields.

- **Safety proof gate (M0, the ready-to-build core):** `convergence (gcounter 3)`
  type-checks and is *certified* — its preservation step reduces to the three
  lattice laws + `Bag` permutation invariance. Mutation target: corrupting
  `merge` to a non-idempotent op (e.g. `+` instead of `max`) must break
  `mergeIdem` and hence `convergence` (the proof catches the non-CRDT).

- **Simulator gate (Lambert, better-than-Winglang):** `Simulate` under a
  `Partition` policy shows two replicas diverge while partitioned and re-converge
  after healing; under `DropRate=1.0` on a cut, a *non-CRDT* (a last-writer-wins
  register without a proper join) is shown to *stay* divergent — i.e. the
  simulator distinguishes convergent from non-convergent protocols, matching the
  proof.

- **Failure-robustness property (harness/, rapid):** for the G-Counter, *for all*
  drop/dup/reorder schedules over a fixed set of ops, all replicas that delivered
  the same op-multiset have equal `value` — a randomized check that *empirically*
  corroborates `convergence` (the furnace "test it" rung beneath the proof).

- **Liveness gate (M7, R-COIND-gated):** `eventualDelivery` under `Fair`
  type-checks (needs `Exec`/`Eventually`); `agreement`/`validity` for
  `singleDecree` type-check (safety, inductive). Listing ch26 (CRDT) is the M0
  acceptance gate; ch27 (consensus) the M7 gate — paralleling the `listings/`
  discipline.

- **Projection/run gate (M0, Lambert):** `project (runSystem (gcounter 3))` runs
  under the JS actor shim (single-threaded scheduler) and on BEAM (real
  `spawn`/`!`/`receive`); killing a replica triggers an OTP-supervisor restart and
  re-convergence. Cross-backend conformance (M4): same protocol, same observable
  convergence on JS-sim and BEAM.

- **Adequacy/safety-transfer gate (research):** for the CRDT shape,
  `adequacy (gcounter 3)` and `safetyTransfer` type-check against R-OTP's
  assumed-axiom runtime semantics; the assumed BEAM-delivery axioms appear in the
  certificate's assumption set (`rune assumptions`, R-FFI's `assume` tier).

- **Proof-cache regression:** a `CvRDT`/`Step`/`convergence` def is certified and
  cache-hits on reload exactly like a pure def; R-COIND's `Nu` ι-rule logs deps
  through the same `Unfold` gateway (R-FRAME's seam untouched).

## Unblocks (which implement nodes, and what they still need)

- **E3 (this node's home — verified protocols + projection):** **the CRDT G-Counter
  *safety* artifact (spec record + LTS + `convergence` proof + simulator) is
  ready-to-build the moment R-SUM (C1) lands** — it needs Σ, the v2 `Bag`, and the
  D1/D2 `Nat`/`Vec` stdlib, and **zero coinduction**. The *projection bridge*
  (`adequacy`/`safetyTransfer`) and **all liveness** (`Exec`/`Fair`/
  `eventualDelivery`, consensus liveness) are **research-blocked on R-COIND** (C5a
  to state, C5b/M2 for ergonomics) and on **D5/R-OTP + B3(BEAM)** for the runtime
  the projection targets. Consensus *safety* (`agreement`/`validity`) is
  ready-to-build alongside the CRDT (inductive `Reachable`), but is the
  labelled-harder artifact.

- **M0 (vertical slice):** R-PROTO *is* the M0 distributed demonstrator
  (`humble-humming-elephant.md:236-238`, `:284` "a replicated counter with a tiny
  consensus core"). The **CRDT counter** is the honest M0 protocol: its safety is
  provable now (modulo R-SUM), and its deploy needs B1/B2/B3(BEAM) — none of which
  has a research node yet, so M0's *deploy* half is gated on the B-track, while its
  *proof* half is ready.

- **E4 / C-INFRA (infra-as-code surface):** R-PROTO delivers the simulator
  (`internal/sim` `Step`-scheduler + `FaultPolicy`) and the preflight(proved
  spec)/inflight(projection) split. Needs the projection runtime (D5/B3) for cloud
  deploy and a contained `surface/`+`cmd/rune/` sugar pass (`protocol … end`,
  `simulate`/`deploy` verbs). **Resolves C-INFRA toward library-first.**

- **D5 / R-OTP:** R-PROTO is the *forcing consumer* — it names the supervision ops
  (`monitor`/`restart`) the verified actor library must provide and the runtime
  axioms (per-mailbox FIFO, at-least-once delivery) `adequacy` assumes. R-OTP
  supplies the verified library; R-PROTO supplies the protocols that justify it.

- **R-COIND (the gating research):** R-PROTO is, alongside E2, R-COIND's forcing
  consumer for *liveness and adequacy* (the coinductive `Exec`/`RunTrace`/
  `WeakSimulates`). It confirms R-COIND.md's staging: C5a (`Nu`) is enough to
  *state* liveness/adequacy; C5b (paths) buys the *up-to/transport* ergonomics the
  refinement proofs want.

---

**Status: needs-more-research** overall, because the projection bridge
(`adequacy`/`safetyTransfer`) and *all liveness* are gated on **R-COIND**
(undelivered) and on **D5/R-OTP + B3(BEAM)** for the runtime — and the deep
adequacy proof is, by R-CALC's own admission, genuinely hard. **But the node is
substantially ready-to-build at its M0 core:** the **CRDT G-Counter spec (R-SUM
record), the network LTS (a `data Config` + `Step` relation with the Lambert
failure rules), the `Reachable`-invariant safety property, and `convergence`
proved by three one-screen lattice laws + the v2 `Bag` quotient's free
reordering** are all ready-to-build the moment **R-SUM (C1)** lands, with **zero
outer-core growth** (a protocol is a record, a datatype, a relation, and ordinary
proofs). The genuinely-open research is concentrated honestly in three places:
**(1) the coinductive liveness/adequacy layer** (gated on R-COIND), **(2) the
projection-refines-spec adequacy proof** (per-protocol, atop R-OTP's runtime
semantics, mitigated by proving the simple CRDT shape first and *assuming* BEAM
delivery axioms via R-FFI's tracked `assume` tier), and **(3) the consensus
liveness half** (conditional on a fairness + failure-detector / partial-synchrony
assumption, per FLP). C-INFRA is resolved toward **library-first** (an
`internal/sim` `Step`-scheduler + `FaultPolicy` as the better-than-Winglang
simulator), with the Wing preflight/inflight split mapping cleanly to
proved-spec / projected-actors.
