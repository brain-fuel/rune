# R-CALC — Process calculus for distributed algebra

> Roadmap node E1 (`E1 [I] the calculus: terms, semantics, typed chans ⇐ R-CALC,
> C1`), with E2 (`bisimulation/equivalence proof library ⇐ R-CALC, C5, E1`)
> hanging off the same research. Telos 4: "verified algebra over distributed
> systems… a process calculus embedded in Rune (operational semantics,
> behavioral equivalence/bisimulation, algebraic laws), used to specify and
> *prove* distributed protocols, then project them to runnable actors." The user
> chose **process calculus** over choreographies / TLA+. This node also sketches
> **C-INFRA** (the better-than-Winglang surface). It depends on **R-COIND**
> (bisimulation is coinductive; the outer core is inductive-only) and ties its
> projection to **R-OTP** / R-EFFECT (`primSpawn`/`primSend`/`primReceive`).

## Problem (what's stuck/absent today, with file:line)

There is no process calculus, no notion of concurrency, and no behavioral
equivalence anywhere in the kernel. Concretely:

- The core term language is sequential, pure, and **total by construction**. The
  evaluator (`core/eval.go:347` `Eval`) normalizes every closed term; the only
  recursion principle is the datatype eliminator (CLAUDE.md Phase 4: "the
  eliminator is the only recursion principle, so no termination checker exists or
  is needed"). A process that loops forever (`server : Proc` that never stops
  receiving) has **no home** in a language where every closed term has a normal
  form. This is the same wall R-EFFECT hit, and it routes to **R-PART** for
  genuine non-termination.
- There is no concurrency primitive. The only "reach into the runtime" mechanisms
  are `builtin nat`/`builtin bin` (`internal/session/session.go:500`), which only
  swap a *pure* datatype's representation, and (proposed by R-EFFECT) the
  `IForeign` IO node. There is no channel, no message send, no parallel
  composition, no nondeterminism.
- **Bisimulation is coinductive and the core is inductive-only.** A bisimulation
  is the *greatest* relation closed under a transition-matching condition; it is a
  coinductive predicate (a coalgebra / `νX. F(X)`). The outer core has datatypes
  by eliminators (`core/eval.go:82` `ElimSig`, the ι-rule `tryIota` at `:1317`) —
  i.e. **initial algebras only**. There is no `codata`, no coinductive type, no
  guarded fixpoint, no productivity checker. You literally cannot *state* "R is a
  bisimulation and therefore P ~ Q" in today's core. This is **R-COIND**, the
  hard prerequisite, and the reason E2 lists `C5` (`C5 [I] coinductive types /
  guarded recursion ⇐ R-COIND`) as a dep.
- **Real failure has no model.** Lambert's bar (CLAUDE.md / roadmap line 62: "the
  distributed algebra models *real* failure (crashes, partitions, reordering),
  not an idealized world") is unmet because there is *nothing*: no transition
  system at all, let alone one with crash/partition/reorder transitions.
- The projection target exists only as a seam: R-EFFECT names
  `primSpawn`/`primSend`/`primReceive` as future IO primitives
  (`ref_docs/wootz/R-EFFECT.md:391`), and R-IR's `IForeign` node
  (`ref_docs/wootz/R-IR.md:170`, `:314`) is the contract-free precursor. But no
  actor library, no runtime, and no BEAM backend (B3) exist yet.

So the entire telos-4 algebra is greenfield. The constraint that makes it a
*clean* fit for this substrate is the usual one: **Thompson — the outer core must
not grow.** No `Proc` core constructor, no transition relation baked into the
kernel, no hash-format bump. The calculus must ship the way quotients (v2),
the fibrant layer (v3), the interval/path/Kan groups (§F), and the proposed IO
group (R-EFFECT) did: as a **contained builtin group of bodiless,
content-addressed definitions** plus, for the parts that genuinely run, a
**codegen-stratum extension** (the `IForeign` actor primitives). The calculus is
*data and proofs in Rune*, exactly as Brady's `Effects` is data and functions in
Idris (`ref_docs/wootz/R-EFFECT.md:80`).

## Prior art (what the literature/other systems do; cite)

**Mechanized process calculi in type theory.** The π-calculus has been formalized
many times: Hirschkoff in Coq with de Bruijn indices (congruence + bisimulation
laws), Despeyroux with weak HOAS, Bengtson–Parrow in Nominal Isabelle
([Bengtson, *Formalising the π-calculus using nominal logic*](https://arxiv.org/pdf/0809.3960)),
and the *Concurrent Calculi Formalisation Benchmark*
([Springer 2024](https://link.springer.com/chapter/10.1007/978-3-031-62697-5_9))
which makes binder + scope-extrusion handling the standard stress test. The
lesson for *this* substrate: **the binder discipline is the hard part of
mechanizing a calculus, and Rune already solved it** — the core is locally
nameless with de Bruijn indices (CLAUDE.md Architecture; the Pitts theorem
that "binding is orthogonal to equality", `rune-v3-design.md:30`). We reuse it
verbatim for channel/name binding.

**Bisimulation, coinductively.** Strong/weak bisimilarity is the canonical
coinductive relation. Coq encodes strong late bisimilarity for replicated
π-calculus coinductively, and "corecursive proofs are in some sense easier than
traditional bisimulation proofs" (the relevant survey:
[Sangiorgi & Rutten, *Introduction to Bisimulation and Coinduction*](https://www.researchgate.net/publication/265113211_Introduction_to_Bisimulation_and_Coinduction)).
The proof technology that makes bisimulation *usable* is **up-to techniques**
(Pous–Sangiorgi,
[*Up-to Techniques for Weak Bisimulation*](https://perso.ens-lyon.fr/damien.pous/upto/);
[Bonchi et al., *A general account of coinduction up-to*](https://link.springer.com/article/10.1007/s00236-016-0271-4)) —
they shrink the relation you must exhibit. Crucially for weak bisimulation, "the
up-to techniques require **termination of internal computations** for soundness"
(Pous–Sangiorgi) — i.e. the soundness of `up-to` interacts with productivity,
which is exactly the R-COIND machinery.

**Bisimulation *as a path type* under guarded/cubical recursion.** The most
substrate-relevant prior art: CCS and π have been formalized in **Guarded
Cubical Agda**
([Veltri–Vezzosi, *Formalizing CCS and π-calculus in Guarded Cubical Agda*](https://www.sciencedirect.com/science/article/abs/pii/S2352220822000992)),
and
[Møgelberg–Veltri, *Bisimulation as path type for guarded recursive types*](https://arxiv.org/pdf/1810.13261)
proves bisimilarity **coincides with equality** when you have guarded recursion +
univalence for the relevant type. This is a striking fit: Wootz is *building* a
cubical inner stratum (§F) and *will* have inner paths. The honest read, though
(below): that coincidence theorem is a *future luxury*, not the E1/E2 build path —
guarded recursion as a TYPE-LEVEL modality (`▷`) is a heavier R-COIND than E2
needs to ship laws.

**Real failure: distributed π and crash-stop session types.** Hennessy–Riely's
**distributed π-calculus** ([*A Distributed Pi-Calculus*](https://www.researchgate.net/publication/220694917_A_distributed_Pi-Calculus))
adds *locations* to π and "accommodates the widest range of non-Byzantine faults:
message loss, delays and reordering, crash and link failures, and network
partitioning." Multiparty session types with **crash-stop failures**
([Barwell et al., *Generalised MPST with Crash-Stop Failures*](https://arxiv.org/pdf/2207.02015);
[*Crash-Stop Failures in Asynchronous MPST*](https://arxiv.org/pdf/2311.11851))
formalize the fail-stop model: "processes may crash and never recover; failures
are detected by failure detectors when attempting to receive." This is the
**Lambert specification we adopt**: fail-stop (not Byzantine), asynchronous
message passing with reordering and loss, partition as a derived dynamic
property. It is also precisely the **BEAM/OTP** model — "let it crash",
supervision, monitors — which is why R-OTP/B3(BEAM) is the projection target.

**Session types & linear logic.** Honda's session types, and the
Caires–Pfenning "session types = linear logic propositions" correspondence,
give a typed-channel discipline that makes well-typed processes deadlock-free /
communication-safe. Rune's **QTT** (`quantity/`, the 0/1/ω semiring, CLAUDE.md
Phase 5) is the linearity substrate: a linear channel endpoint is a `(1 c :
Chan S)` binder. This is the one place Wootz has a *structural* advantage over
the Coq/Agda formalizations, which bolt linearity on by hand.

**Infra-as-code (C-INFRA).** Winglang
([winglang/wing](https://github.com/winglang/wing),
[Preflight and Inflight](https://www.winglang.io/docs/concepts/inflights))
splits **preflight** (compile-time, emits Terraform/CloudFormation IaC) from
**inflight** (runtime, compiled to JS on cloud compute), and ships a **local
simulator** that "mimics cloud behavior… test locally before deploying." Its
weakness, and the gap Wootz fills: Wing's correctness story is *testing in the
simulator*, not *proof*. "Better than Winglang" (roadmap line 32) = the same
preflight/inflight/simulator ergonomics, but the inflight behavior is a process
term with a **machine-checked** safety/liveness proof, and the *same source*
both simulates and deploys.

Cross-cutting lessons:
- The binder problem is solved here (de Bruijn core); reuse it.
- Bisimulation is coinductive ⇒ this node is *gated on R-COIND*; the choice of
  R-COIND mechanism (codata-by-eliminators vs guarded `▷` vs sized types) sets a
  ceiling on how cleanly E2 can state up-to techniques.
- Fail-stop + asynchronous-reordering is the right Lambert model and the BEAM
  model; adopt it, do not invent.
- QTT gives typed/linear channels for free; lean on it.

## Chosen approach for THIS substrate (concrete; respects containment)

**Recommendation: a *shallow-embedded, reflexive* asynchronous π/actor calculus.
The process *language* is an ordinary Rune datatype (`Proc`), declared by the
existing data-by-eliminators machinery — no builtin group needed for the syntax
at all. The *operational semantics* is a Rune-level transition relation (an
indexed family / `Prop`-valued relation). *Bisimulation* is a coinductive
relation, supplied by R-COIND. Real failure is *extra transition rules*, not a
separate calculus. Projection to actors reuses R-EFFECT's `IForeign`
spawn/send/receive primitives. The only genuinely-new contained surface is in
codegen (the actor runtime), exactly where the strata thesis says reality lives.**

This is the single most important design decision and it is **Thompson-forced**:
the calculus must add *zero* outer-core constructors. Everything below is "data
and proofs in Rune", with reality quarantined in the codegen menu.

### Layer 0 — the term language is a datatype, not core syntax

`Proc` is declared with the *existing* `data … is … end` mechanism
(`core/eval.go:82` `ElimSig`, ι at `:1317`). No builtin group, no
`store/calc.go` formers, because the process syntax is genuinely *inductive data*
the user writes and the eliminator folds — it is not a new equality or a new
universe. An **asynchronous, name-passing** core (asynchronous because that is
what BEAM/actors actually are, and it sidesteps the synchronous-output binder
pain):

```
data Name : U is                 -- channels/actor addresses; an abstract atom type
  ...                            -- (uniform-parameter datatype; or `builtin name`)

data Proc : U is
  nil    :                                   Proc            -- 0, the dead process
  par    : Proc -> Proc -> Proc                              -- P | Q
  send   : Name -> Msg -> Proc                               -- async output: c!⟨m⟩  (no continuation)
  recv   : Name -> (Msg -> Proc) -> Proc                     -- input prefix: c?(x).P
  nu     : (Name -> Proc) -> Proc                            -- (νc) P : restriction = name binder (HOAS-via-Pi)
  bang   : Proc -> Proc                                      -- !P replication  (the only "infinite" source)
  crash  : Proc                                              -- ↯ : a crashed/halted location (Lambert)
end
```

Two substrate-specific moves:
- **Restriction uses HOAS-through-Pi**: `nu : (Name -> Proc) -> Proc`. The
  *function* `Name -> Proc` is an ordinary core lambda, so name binding and
  α-equivalence are **the core's de Bruijn binders, reused verbatim** — this is
  the dodge that makes scope extrusion (the Concurrent Calculi Benchmark's nemesis)
  free, because substitution of a name into `P` is just core application
  (`m.Apply`, `core/eval.go:472`). Strict positivity (CLAUDE.md Phase 4) **must
  be checked**: `nu`'s argument is `Name -> Proc` and `recv`'s is `Msg -> Proc` —
  `Proc` occurs strictly positively (to the right of `->`), so the existing
  positivity checker accepts it. This is the one subtlety to verify at
  declaration; see Risks.
- **Asynchronous output** (`send` has no continuation, a *message in flight* is a
  process) makes the reduction relation simple and matches the actor/BEAM model
  where a send is fire-and-forget into a mailbox. Reordering/loss is then natural:
  an in-flight `send c m` is a free-floating term in a `par`.

`Msg` is a parameter (often `Name` for pure name-passing π, or a payload
datatype). For typed channels we index: see Layer 3.

**Where this is honest-stuck (R-PART / R-COIND):** `bang` (replication) is the
only source of unbounded behavior. Today's evaluator would *not* be asked to
normalize a `bang` — `Proc` is **data being analyzed by the semantics**, never
*run* by the kernel evaluator. The kernel only ever folds finite `Proc` trees
with the eliminator (e.g. to define structural congruence). Unbounded *execution*
is the projected actor's job (Layer 4), not the kernel's. So `Proc` containing
`bang` type-checks and is reasoned about totally; it never asks the total core to
diverge. This is the same firewall R-EFFECT draws: the core builds the *recipe*;
the runtime *performs* it.

### Layer 1 — operational semantics as a Rune relation

The reduction/labelled-transition relation is an **inductively-defined family**,
which today means a `Prop`-valued (or `U`-valued) relation built from the
equality stratum and datatypes — *no core change*. Two presentations, both
expressible now:

A **labelled transition system** (LTS) for bisimulation, plus a **reduction**
relation for the runtime story. The LTS `Step : Proc -> Label -> Proc -> U`:

```
data Label : U is
  tau   :                 Label         -- τ, internal communication
  inp   : Name -> Msg ->  Label         -- c?m
  out   : Name -> Msg ->  Label         -- c!m
  fail  : Name ->         Label         -- ↯c : the crash of a location (Lambert)
end

-- Step P ℓ Q  ::  "P can do ℓ and become Q".  An indexed family; declared either
-- as an inductive relation (needs R-SUM-style indexed families, see Risks) or
-- encoded as a Prop predicate over the data.
Step : Proc -> Label -> Proc -> U
```

The rules (the teachable core): `COMM` (a `send c m | recv c k ~τ~> k m`), `PAR`
(congruence), `RES` (transition under `nu`, with scope extrusion handled by core
application), `REP` (`bang P ~> P | bang P`), and the **Lambert failure rules**:

```
CRASH   :  Step P (fail c) crash                 -- any located process may fail-stop
LOSS    :  Step (send c m) tau nil               -- an in-flight message may be dropped
REORDER :  par is commutative/assoc up to ≡      -- reordering is structural congruence (Layer 2)
DETECT  :  Step (recv c k) (fail c) (k_timeout)  -- a receiver detects a crash (failure detector)
```

`CRASH`/`LOSS`/`DETECT` are **just more constructors of the `Step` relation**.
This is the cleanest possible realization of "model real failure": a partition is
not a new calculus, it is the dynamic configuration where `LOSS` fires on every
message crossing a cut, and `CRASH` makes a location's sends/receives unavailable.
Lambert's bar is met *by adding rules to an inductive relation*, requiring no new
machinery. (A partition is **derived**, not primitive: define `Partitioned ps qs`
as "every cross-cut `send` reduces by `LOSS`"; healing is removing that
predicate. This is honest about partitions being a *runtime condition*, not a
syntactic construct.)

### Layer 2 — algebraic laws & structural congruence

Structural congruence `≡` (`par` commutative/associative with `nil` unit,
`nu` scope laws, `bang P ≡ P | bang P`) is a `Prop`-valued relation. The π/CCS
**algebraic laws** (the "algebra" of telos-4) — e.g. `P | Q ~ Q | P`,
`(νc)(νd)P ~ (νd)(νc)P`, `P | nil ~ P`, expansion law — are **theorems**, proved
once in the E2 library against the bisimulation of Layer 3 and reused. These are
ordinary Rune proofs over the eliminator; the only ones that need coinduction are
the ones quantifying over *behavior* (anything stated with `~`), which is why
they live in E2 (`⇐ C5`), not E1.

### Layer 3 — bisimulation (the coinductive heart, gated on R-COIND)

A bisimulation is the greatest relation `R` with: if `P R Q` and `P ~ℓ~> P'`
then `∃Q'. Q ~ℓ~> Q' ∧ P' R Q'`, and symmetrically. This is **coinductive** and
**cannot be stated in today's inductive-only core** (`core/eval.go` has
`ElimSig`/`tryIota` — initial algebras only). R-COIND must land first. The
design *commits to an interface* for R-COIND so E2 can be written against it,
and flags the choice as the gating research:

```
-- The bisimulation functor (a Rune function, no coinduction needed to STATE it):
SimF : (Proc -> Proc -> U) -> (Proc -> Proc -> U)
SimF R P Q =
  ( (ℓ : Label) -> (P' : Proc) -> Step P ℓ P' -> Σ Q' (Step Q ℓ Q' × R P' Q') )  -- forward
  × ( (ℓ : Label) -> (Q' : Proc) -> Step Q ℓ Q' -> Σ P' (Step P ℓ P' × R P' Q') )  -- backward

-- Strong bisimilarity = νR. SimF R     -- THE coinductive type; needs R-COIND.
Bisim : Proc -> Proc -> U
-- intro:  a coinductive/guarded constructor (the R-COIND primitive)
-- elim:   one-step "unfold"  Bisim P Q -> SimF Bisim P Q
```

`SimF` uses `Σ` and `×` — i.e. **R-SUM / C1 is a hard prerequisite** (the E1 dep
`⇐ R-CALC, C1` is real: you cannot even state the matching condition without
dependent pairs). The `νR.SimF R` is the coinductive type from R-COIND.

**Weak bisimulation** (`≈`, ignoring `τ`) is `SimF` with `Step` replaced by the
weak transition `=ℓ=>` (`τ*ℓτ*`). The weak transition's reflexive-transitive
closure of `τ` is *itself* a place where R-PART/productivity bites: the up-to
techniques' soundness "requires termination of internal computations"
(Pous–Sangiorgi) — so the E2 library's `up-to` lemmas inherit whatever
productivity guarantee R-COIND provides. **Design stance:** ship **strong**
bisimulation + algebraic laws in the first E2 increment (no `τ`-closure subtlety),
add weak bisimulation + a small set of up-to techniques (up-to-≡,
up-to-bisimilarity) in a second increment once R-COIND's productivity story is
pinned.

**The R-COIND choice, ranked for this substrate (this is the gating sub-question,
labelled research):**
1. **Codata by *co-eliminators* (mirror of Phase 4 data-by-eliminators).** A
   `codata` declaration generating a *generator/unfold* principle (the dual of
   the eliminator), with **productivity by construction** the dual of "totality
   by construction" — the corecursor is the only way to build, so guardedness is
   structural, no productivity checker. This is the *most in-keeping with the
   existing kernel* (it is literally the Phase-4 move dualized) and the
   recommended R-COIND target. `Bisim` is then `codata Bisim is unfold : Bisim P
   Q -> SimF Bisim P Q end` with a `coiter`/`corec` builder. **Best fit; lowest
   new surface.**
2. **Guarded recursion (`▷` modality, Nakano/clock-indexed).** Matches the
   "bisimulation as path type" prior art and meshes with §F's cubical interval,
   but adds a type-level modality to the core (or a contained group) — heavier,
   and only pays off if you *want* the bisimilarity-is-equality coincidence (a
   luxury, not E2's requirement). **Defer; revisit if synthetic-homotopy
   payoff is wanted.**
3. **Sized types.** Productivity via size indices; powerful for up-to but adds
   size polymorphism to the core. **Against Thompson; not recommended.**

**Recommendation to R-COIND: option 1 (codata by co-eliminators), as a contained
generalization of the Phase-4 eliminator machinery** — same `ElimSig`-shaped
metadata, a dual ι-rule (`unfold (corec h s) ~> h s`, computing when the
*observer* is applied rather than when the *scrutinee* is a constructor). This is
the dual of `tryIota` and would live in the evaluator the same way, contained.

### Layer 4 — projection to runnable actors (ties to R-OTP / R-EFFECT)

A `Proc` is *reasoned about* in the kernel and *run* on a backend. The bridge is
**not** running `Proc` in the kernel (it can't — `bang`/`crash`). Instead, a
**projection function** `project : Proc -> IO Unit` defined in the R-EFFECT
monad, compiling each constructor to an actor primitive:

| `Proc` constructor | projects to (R-EFFECT `IForeign` op / R-OTP)        |
|--------------------|-----------------------------------------------------|
| `nil`              | `pureIO unit`                                       |
| `send c m`         | `primSend c m`     (async; into mailbox)            |
| `recv c k`         | `bindIO primReceive (λm. project (k m))`            |
| `par P Q`          | `bindIO (primSpawn (project P)) (λ_. project Q)`    |
| `nu k`             | `bindIO primNewChan (λc. project (k c))`            |
| `bang P`           | `primSpawnLoop (project P)`  (the one unbounded op) |
| `crash`            | `primExit`  (fail-stop; BEAM: the process dies)     |

These are exactly R-EFFECT's named seam (`primSpawn`/`primSend`/`primReceive`,
`ref_docs/wootz/R-EFFECT.md:391`) plus `primNewChan`/`primSpawnLoop`/`primExit`,
realized as `IForeign{Op,Args}` IR nodes (`ref_docs/wootz/R-IR.md:170`,`:314`).
On **BEAM** (B3, the M0 target) they map to real OTP: `primSpawn`→`spawn`,
`primSend`→`!`, `primReceive`→`receive`, `crash`→process death + supervisor.
**This is why fail-stop is the right model: it is the BEAM model.** The verified
*actor/mailbox/supervision library* over these primitives is **R-OTP/D5**; R-CALC
only commits to the projection *shape* and the primitive *seam*.

**The correctness theorem that ties it together** (the thing that makes this
"verified algebra over distributed systems"): an *adequacy/soundness* statement —
the projected actor's observable behavior **refines** (is a weak-simulation of)
the `Proc`'s LTS. Stated in Rune as a simulation between `Step` and a
runtime-trace semantics; *proved* for the protocols in E3 (`E3 [I] verified
protocols + projection ⇐ E2, D5`). R-CALC delivers the *statement* and the
projection; E3 delivers the proof for concrete protocols.

### Why this respects all three smiths

- **Thompson:** zero outer-core constructors. The syntax is a *datatype*; the
  semantics is a *relation*; the laws are *theorems*; failure is *more relation
  constructors*; the only new machinery is (a) R-COIND (a contained dual of the
  existing eliminator — recommended option 1) and (b) the actor primitives, which
  are R-EFFECT/R-IR `IForeign` nodes living in the codegen menu where reality
  belongs. No hash-format bump (no new core constructor — same rule the §F groups
  obeyed, CLAUDE.md "No hash-format bump unless new core constructor").
- **Savage:** the calculus is a *textbook π/CCS* a learner already has a mental
  model for, and it is *just a datatype* — they can `case` on a `Proc` from day
  one. The furnace on-ramp: simulate a protocol (test it), *then* prove a law,
  *then* prove a protocol correct. The teachable artifact (below) is a
  two-process handshake whose commutativity law is proved by exhibiting a
  three-element bisimulation.
- **Lambert:** failure is first-class and is *the actual fail-stop/async-reorder
  model of BEAM*, not an idealization. The same `Proc` source simulates (kernel +
  a scheduler) and deploys (projection to OTP). Partition is a real dynamic
  condition (`LOSS` across a cut), not a fiction.

## Interfaces & signatures to add (Go + Rune surface as relevant)

**Go — and the surprise is how little there is.** The syntax/semantics/laws are
*all Rune*; the only Go is the R-COIND co-eliminator machinery (which is R-COIND's
node, sketched here as the interface E2 needs) and the actor `IForeign` ops
(R-EFFECT/R-IR's nodes, reused). R-CALC itself adds **no new Go to core/store**.

```go
// core/eval.go — the R-COIND co-eliminator metadata (dual of ElimSig). This is
// R-COIND's deliverable; R-CALC consumes it. Mirrors ElimSig/tryIota exactly.
type CoElimSig struct {
    Codata    Hash      // the codatatype being observed
    NumParams int
    Dtors     []DtorSig // observations (destructors), dual of Ctors
}
type DtorSig struct{ Arity int }            // an observation's result arity
// dual ι: `obs_i (corec h s) ~> h_i s`, fires when an OBSERVER is applied to a
// corec-built value (the dual of "scrutinee is a constructor"). Lives in
// tryRules alongside tryIota, contained, no hash-format bump.

// store — NO new builtin group for the calculus. `Proc`/`Step`/`Label` are
// user/stdlib datatype declarations through the EXISTING DataDeclOf path
// (internal/session/session.go:507). codata `Bisim` rides R-COIND's declaration
// path.

// codegen — the actor primitives are R-EFFECT/R-IR IForeign ops; R-CALC adds the
// op NAMES to the vocabulary, no new IR node:
//   "spawn", "send", "receive", "newChan", "spawnLoop", "exit"
// (codegen/ir.go IForeign{Op,Args}; per-backend in Backend.Foreign()).
```

**Rune surface — a *library* module `lib/calc.rune` (NOT core):**

```
-- the calculus (ordinary datatypes; strict-positivity-checked)
data Name : U is ... end
data Msg  : U is ... end                 -- or = Name for pure π
data Proc : U is nil | par | send | recv | nu | bang | crash ... end
data Label: U is tau | inp | out | fail end

-- semantics (a Prop/U-valued relation; indexed family — needs C1/indexed data)
Step  : Proc -> Label -> Proc -> U
StrCong : Proc -> Proc -> U              -- structural congruence ≡

-- bisimulation (E2; needs R-COIND codata)
SimF  : (Proc -> Proc -> U) -> (Proc -> Proc -> U)
codata Bisim (P Q : Proc) : U is unfold : SimF Bisim P Q end
WeakStep : Proc -> Label -> Proc -> U    -- =ℓ=>  (τ-closure; second increment)
WeakBisim : Proc -> Proc -> U

-- algebraic laws (E2; theorems, proved by exhibiting bisimulations)
parComm : (P Q : Proc) -> Bisim (par P Q) (par Q P)
parNil  : (P : Proc)   -> Bisim (par P nil) P
nuSwap  : ... ; expansion : ...

-- projection (E1 seam; runs on R-EFFECT/R-OTP)
project : Proc -> IO Unit                -- the table in Layer 4
adequacy : (P : Proc) -> Simulates (traceOf (project P)) (Step P)   -- statement; proof in E3
```

## Worked micro-example (the teachable artifact)

`handshake.rune` — two processes, a one-message handshake, and a **machine-checked
commutativity law** proved by a finite bisimulation. The furnace beat: *simulate,
then prove.*

```
-- a server that echoes one message back on a reply channel, and a client.
server : Name -> Proc is fn (c : Name) is
  recv c (fn (m : Msg) is send (replyOf m) ack end) end

client : Name -> Proc is fn (c : Name) is
  send c (msg myReply ping) end

system : Proc is nu (fn (c : Name) is par (server c) (client c) end)
```

What the learner sees and can verify:
1. `system` type-checks as `Proc` — it is *just data*. The kernel certifies it
   (`AddDef`), cache entry like any pure def.
2. **Simulate** (M0/C-INFRA simulator): a kernel-level scheduler folds the `Step`
   relation, showing `system ~τ~> (νc)(send (replyOf ping) ack) ~τ~> nil` —
   *the handshake happens*. With the `LOSS` rule enabled on a cut, the learner
   *watches the message drop* and the system deadlock: **failure is observable,
   not abstract** (Lambert's teachable moment).
3. **Prove a law**: `parComm (server c) (client c) : Bisim (par (server c)
   (client c)) (par (client c) (server c))` — proved by `corec` exhibiting the
   3-state bisimulation `{(initial,initial'), (sent,sent'), (done,done')}`. The
   learner *writes a coinductive proof* and the kernel checks it. This is the
   first time a Wootz user proves two concurrent systems *equivalent*.
4. **Project & deploy** (B3/BEAM): `rune run handshake.rune (project system)`
   spawns two BEAM processes that actually exchange a message; `crash` in the
   server projects to a dying OTP process a supervisor restarts. The *same
   source* that was proved correct now runs.

Contrast artifact (containment visible): `bad.rune` with `n : Nat is procElim
... bang ...` trying to *run* a replicated process in the kernel — a type error
or non-terminating-by-eliminator rejection, showing the firewall: the kernel
*reasons about* `Proc`, the runtime *executes* it.

## Risks / open sub-questions

- **R-COIND is a hard, undelivered prerequisite (the gate).** E2 (bisimulation +
  laws) cannot start until R-COIND lands. *Recommendation:* R-COIND = codata by
  co-eliminators (the Phase-4 dual), the lowest-surface option; this is itself a
  research node, here only *interface-sketched*. **Status: E1 (syntax + semantics
  + projection) is ready-to-build on R-SUM/C1; E2 (bisimulation/laws) is
  research-blocked on R-COIND.**
- **Indexed families for `Step`/`Bisim`.** `Step P ℓ Q` is an *indexed* relation;
  CLAUDE.md Phase 4 / PARKING-LOT.md park indexed families ("indices need
  unification-based coverage machinery with no current listing"). Either (a) unpark
  indexed families (a C-track item), or (b) encode `Step` as a `Prop`-valued
  *recursive predicate* over the `Proc`/`Label` data (works today with R-SUM's Σ
  for the existentials, no indexed data) — **(b) is the ready-to-build path**; (a)
  is cleaner long-term. Flag: the encoding (b) leans hard on R-SUM (`Σ`, `×`) so
  **C1 is a true blocker for even stating the semantics**, consistent with the
  DAG's `E1 ⇐ R-CALC, C1`.
- **Strict positivity of `Proc`.** `recv : Name -> (Msg -> Proc) -> Proc` and
  `nu : (Name -> Proc) -> Proc` put `Proc` to the right of `->` — strictly
  positive, *should* pass the existing checker (CLAUDE.md Phase 4 checks strict
  positivity at declaration). **Must verify** the checker accepts a *parameter
  function returning the datatype* (`Msg -> Proc`), not just first-order recursive
  args; if not, that is a small checker extension (research-flag, low risk).
- **HOAS-via-Pi vs first-order names.** Using `nu : (Name -> Proc) -> Proc` makes
  binding free but means you cannot `case`-analyze a binder structurally (no
  "exotic terms" issue here since `Proc` is first-order *except* the function
  args, which is fine for the LTS). Alternative: de-Bruijn `Name` as `Nat` indices
  — more faithful to the Coq formalizations, more substitution boilerplate.
  *Recommendation:* HOAS-via-Pi (reuses the core's strength); revisit if a law
  needs structural binder analysis.
- **Weak bisimulation productivity.** Up-to techniques' soundness "requires
  termination of internal computations" (Pous–Sangiorgi). Weak `≈` with
  `bang`-induced infinite `τ` needs R-COIND's productivity guarantee to keep
  up-to sound. *Mitigation:* ship strong bisimulation + laws first (no `τ`
  subtlety); weak + up-to in a second E2 increment. **Status: weak/up-to is
  research, gated on R-COIND's productivity story.**
- **Adequacy of projection (the deep theorem).** "Projected actor refines the
  `Proc` LTS" requires a runtime-trace semantics for the `IForeign` ops and a
  simulation proof — this is genuinely hard (it is the heart of E3) and depends on
  R-OTP's runtime semantics and the BEAM scheduler's guarantees. R-CALC delivers
  the *statement* and *projection*; the *proof* is E3, per-protocol. **Status:
  statement ready; general proof is research (E3).**
- **Bisimilarity = equality (the cubical luxury).** Møgelberg–Veltri's coincidence
  needs guarded recursion + univalence — i.e. §F + a `▷` modality. Tempting (it
  would let you `subst` along a bisimulation!) but *far* off the critical path.
  **Park; not E1/E2.**
- **C-INFRA surface shape (open clarify).** Library-first vs DSL. *Recommendation
  (matches the roadmap's C-INFRA bracket, line 160):* library-first — `Proc` +
  `project` + a simulator are a library over E-track; surface sugar (a
  `process … end` block desugaring to `Proc` constructors, à la `seq`) is a
  later, contained `surface/` change. The Wing preflight/inflight split maps to
  Wootz as: **preflight = the proved `Proc` + its laws (compile-time, the
  certificate)**; **inflight = `project`'s `IForeign` ops (runtime, BEAM)**; the
  **simulator = a kernel-level `Step`-scheduler** (no deploy needed) — "better
  than Winglang" because the inflight behavior carries a *proof*, not just a
  simulator pass.

## Test/gate plan

- **Containment gates (Thompson, must pass):** no change to `core/term.go`,
  `core/hash.go`, `defFormatVersion`/`hashFormatVersion`; a test asserts an
  existing pure def's hash is byte-identical after the calculus library + R-COIND
  machinery register (the §F groups already prove this is achievable). `Proc` is
  an ordinary `DataDeclOf` datatype — `internal/session/structural_test.go`-style
  test that it declares, checks, and folds with its eliminator.
- **Semantics property tests (harness/):** `Step` is deterministic-up-to-labels
  where it should be; `COMM`/`PAR`/`RES` fire as specified; the failure rules
  (`CRASH`/`LOSS`/`DETECT`) are reachable; structural congruence `≡` is an
  equivalence + a congruence (mirrors the Phase-1 conversion-equivalence
  property, CLAUDE.md). Mutation target: a dropped `PAR`-congruence rule must
  break a reachability property.
- **Bisimulation laws (E2 gate):** `parComm`/`parNil`/`nuSwap`/expansion
  type-check and their `corec` proofs are accepted (these *are* the algebraic-laws
  deliverable). A reference furnace listing (e.g. ch25) is the acceptance gate,
  paralleling the `listings/` discipline.
- **Failure-model gate (Lambert):** a property that under a `Partitioned`
  predicate, a cross-cut handshake *cannot* complete (the deadlock is provable),
  and that removing the partition restores completion — i.e. the model genuinely
  distinguishes partitioned from healthy.
- **Projection/run gate (M0, Lambert):** `project handshake` runs under the JS
  backend's actor shim (a single-threaded scheduler) and on BEAM (real `spawn`/
  `!`/`receive`); a crashed process is restarted by a supervisor. Cross-backend
  conformance (M4): same `Proc` produces the same observable trace on JS-sim and
  BEAM.
- **Proof-cache regression:** a `Proc`/`Step`/`Bisim` def is certified and
  cache-hits on reload exactly like a pure def (extend `store/cert_test.go`); the
  R-COIND co-eliminator's ι-rule logs dependencies through the same `Unfold`
  gateway (R-FRAME's seam is untouched — coinduction is *more* of the same ι
  instrumentation).

## Unblocks (which implement nodes, and what they still need)

- **E1 (the calculus: terms, semantics, typed chans):** **ready-to-build for the
  term language (`Proc` datatype), the `Step`/`≡` semantics-as-relation, the
  failure rules, and the `project` projection**, *the moment C1/R-SUM lands* (Σ/×
  are needed even to state `Step`'s existentials and `SimF`'s matching). Typed
  channels = QTT-linear `(1 c : Chan S)` binders + a session-type index on `Name`
  (a refinement; the un-indexed `Name` ships first). Still needs: C1 (Σ), and the
  R-EFFECT actor-op vocabulary for `project` to *run* (E1 can be *stated and
  proved-about* without running).
- **E2 (bisimulation/equivalence proof library):** **research-blocked on
  R-COIND** (codata/coinduction — bisimulation cannot be stated without it). Once
  R-COIND lands as codata-by-co-eliminators, E2 ships strong bisimulation + the
  algebraic laws first, then weak bisimulation + up-to techniques (the latter
  gated on R-COIND's productivity guarantee). Needs: R-COIND, C5, E1.
- **E3 (verified protocols + projection to actors):** needs E2 (to *prove*
  equivalences/refinements) + D5/R-OTP (the verified actor runtime the projection
  targets) + the adequacy theorem's runtime-trace semantics. R-CALC delivers the
  adequacy *statement* and projection *shape*; E3 supplies per-protocol proofs.
- **E4 / C-INFRA (infra-as-code surface):** R-CALC delivers the simulator concept
  (kernel `Step`-scheduler) and the preflight(proof)/inflight(projection) split.
  Needs E3 + B3(BEAM) for cloud deploy, and a contained `surface/` sugar pass for
  the `process … end` block. **Recommendation: library-first** (resolves C-INFRA's
  open clarify toward the library option).
- **R-COIND (the gating research this node depends on):** R-CALC *specifies the
  interface* R-COIND must satisfy (a `νR.F(R)` greatest-fixpoint type with a
  one-step `unfold` and a `corec` builder) and *recommends the mechanism*
  (codata-by-co-eliminators, the Phase-4 dual, lowest new surface, productivity-
  by-construction). R-COIND itself remains undelivered/open.
- **R-OTP / D5 (the projection runtime):** R-CALC names the primitive seam
  (`spawn`/`send`/`receive`/`newChan`/`spawnLoop`/`exit` as `IForeign` ops over
  R-EFFECT/R-IR) and the projection table; the *verified* actor/mailbox/
  supervision library is R-OTP/D5 over B3(BEAM).

---

**Status: needs-more-research** overall, because the behavioral-equivalence core
(E2) is gated on **R-COIND**, which is undelivered. But the node is *partially
ready-to-build*: **E1's term language, operational semantics (including the
Lambert failure rules), structural congruence, algebraic-law *statements*, and
the actor projection are ready-to-build on C1/R-SUM + the R-EFFECT actor-op
seam** — and they require **zero outer-core growth** (the calculus is a datatype,
the semantics a relation, failure more relation-constructors, the runtime an
`IForeign`/codegen extension). The genuinely-open research is concentrated in two
honest places: **(1) R-COIND** (the coinductive substrate for bisimulation — with
a concrete recommendation: codata-by-co-eliminators, the dual of the existing
Phase-4 machinery), and **(2) the adequacy proof** of projection-refines-LTS
(E3, per-protocol, atop R-OTP's runtime semantics). C-INFRA is resolved toward
**library-first**, with the Wing preflight/inflight split mapping cleanly to
proof/projection and a kernel `Step`-scheduler as the simulator.
