# The adequacy tie: spec → proof → live runtime (E3)

This note records the **three-layer correspondence** that makes "verified OTP" a
whole claim, and the exact **trust boundary** at the foreign edge. It is the
honest accounting for the `project Proc ⊨ lstep` story: what is proven in the
kernel, what runs on a real backend, and what is trusted across the boundary
between them.

## The three layers

1. **Spec — the fault LTS (`ch206`).** A labelled transition system
   `lstep : Fault -> Proc -> Option Trans` with the fault rules `{CRASH, LOSS,
   DETECT}`. This is the *meaning* of a distributed system: how it may behave,
   including failure. Each rule computes and is certified by `refl`.

2. **Proof — adequacy (`ch207`, `ch209`).** The projected actor's *observable
   behaviour refines* the LTS — it does nothing the spec forbids.
   - `ch207` proves it as **finite-trace** equality for concrete well-supervised
     protocols: `visibleRun k P ≡ project P` (the runtime's observable fault trace
     is exactly a trace the spec allows), by `refl`.
   - `ch209` lifts it to the **coinductive / unbounded** level via the general LTS
     bisimulation library (`ch208`): a process's fault behaviour is the infinite
     observation stream the LTS emits, and two (mirror-image) supervised systems
     are proven path-equal as behaviour streams by `traceBisim` — no fuel bound,
     the whole behaviour at once.
   Both are kernel proofs: they hold by the core's own conversion/`refl`, with no
   appeal to any backend.

3. **Live — the BEAM runtime (`ch205`).** The projection table
   (`send -> primSend`, `recv -> primReceive`, `par -> primSpawn`, …) is realised
   on the BEAM: `codegen.Beam` ships `beamOTPRuntime` mapping each primitive onto a
   real Erlang construct (`spawn`/`!`/`receive`/`self`). `ch205` spawns a stateful
   worker, drives it, and **runs on escript** to the FIFO-deterministic answer the
   projection predicts. The happy-path projection is therefore not merely specified
   — it executes, and its observable result is exactly its denotation.

## The tie, and the trust boundary

The kernel proof (layer 2) connects layer 1 to layer 3 **as far as the foreign
boundary allows**:

- **Proven, end to end (no trust):** `project P` (the projection's observable
  behaviour) refines `lstep` (the spec). This is layers 1↔2, entirely in the
  kernel.

- **Confirmed, running (no trust beyond the toolchain):** for the *implemented*
  primitive subset (`spawn`/`send`/`receive`/`self`), the live BEAM observable
  equals the projection's prediction (`ch205` runs to the projected value; the
  cross-backend conformance suite gates it). This is layers 2↔3 for the happy
  path.

- **Trusted (the foreign edge):** that the **BEAM runtime faithfully implements the
  projection table** — i.e. that Erlang's `spawn`/`!`/`receive` (and, once wired,
  `monitor`/`exit`) are the operational reality the `IForeign` accessors name. rune
  cannot prove properties of foreign Erlang; this is the irreducible trust
  assumption, and it is the *minimal* one: it is exactly the standard "the compiler
  backend is correct" obligation, scoped to a handful of named primitives whose
  Erlang bodies are a few lines each (`beamOTPRuntime`), auditable by inspection.

## The remaining gap (explicit, not hidden)

The **fault** primitives `primMonitor`/`primExit` are not yet wired into
`beamOTPRuntime` (they are parked as **D5-faults-live**, PARKING-LOT.md). So the
running confirmation (layer 2↔3) currently covers the happy-path subset only; the
CRASH/DETECT projection is proven (layers 1↔2, `ch206`/`ch207`/`ch209`) and
*specified* against the BEAM model, but not yet *executed* live. Closing that gap
is mechanical — two more `beamOTPRuntime` entries (`primExit -> exit/1`,
`primMonitor -> erlang:monitor` + a `{'DOWN',…}` receive) and a fault listing that
runs a crash → detect → recover scenario on escript, confirming its live observable
matches the `ch207`/`ch209` prediction. Until then the fault tie is a documented
trust boundary, not a running one — recorded here so it reads as what it is.

## Why this is the right shape (the three smiths)

- **Thompson:** zero new core. The spec is a function, the proof is `refl`/
  `traceBisim`, the runtime is a few lines of Erlang behind named accessors. The
  trust boundary is small and auditable, not a hole in the metatheory.
- **Lambert:** a deployed artifact at every step — `ch205` runs on the BEAM today;
  the fault tie closes by *adding runtime*, never by weakening a proof.
- **Savage:** the correspondence is teachable as three readable listings
  (`ch206` spec, `ch207`/`ch209` proof, `ch205` run) plus this one-page tie.

## Toward the general all-P refinement (the open research item — scoping, 2026-06-22)

`ch207` proves the projection refines the fault LTS **per protocol** (single/multi-
fault/survivor/quiet/fuel-stable). `ch409` + `ch421` close the *alphabet* halves for
ALL P (soundness: both semantics stay observable; completeness: the runtime emits only
`{ltau, lfail}`). What stays open is the **general refinement for every well-supervised
P at once**: `visibleRun k P ≡ project P`. It is blocked on **par-interleave fuel-
threading** — for `par a b`, the `k` steps interleave `a` and `b` unpredictably, so the
fixed-fuel induction cannot recurse on a "fuel for `a`" + "fuel for `b`" split; the fuel
does not decompose over `par`.

**What the 2026-06-22 lemmas add.** Two structural facts about `visibleRun` now hold for
ALL fuel and ALL processes:
- `prefixMono` (`ch452`): `visibleRun k P` is a PREFIX of `visibleRun (k+1) P`.
- `lenBound` (`ch456`): `len (visibleRun n P) ≤ n` (each step emits ≤ 1 observable).

Together the finite runs form a **monotone, length-bounded ω-chain** that grows by 0 or 1
per step. That is exactly the data an order-theoretic limit argument needs.

**Three candidate attacks (in decreasing promise):**
1. **State the refinement at the LIMIT, not at fixed fuel.** The ω-chain's supremum is the
   coinductive observation stream `ch209` already builds (`fib (Option Label)` over
   `fib Proc`). Comparing the LIMIT traces of runtime vs spec turns the par problem from
   fuel-arithmetic into a **bisimulation** — and `ch208`'s `traceBisim` already handles
   interleaving coinductively (no fuel bound). `prefixMono`/`lenBound` are the bridge lemmas
   justifying that the finite runs converge to that limit, so the fixed-`k` obligation is
   discharged by "every finite prefix agrees" rather than a `k`-indexed induction. This
   reuses the machinery that ALREADY closed `ch209` and sidesteps fuel decomposition
   entirely. **Most promising; consistent with the coinductive route that worked.**
2. **A simulation relation closed under `par`.** Define `R` between runtime and spec states,
   prove it a weak simulation, and — the crux — prove `R` is closed under parallel
   composition (`a R a'`, `b R b'` ⟹ `par a b R par a' b'`). Par-closure of a (bi)simulation
   is the standard CCS technique and needs no fuel; the work is the closure lemma, which the
   `interleaveT`/`commT`/`parOk` structure of `okStep` should support directly.
3. **Replace fuel with a structural measure on `Proc`** that decomposes over `par`
   (e.g. total observable-emitting depth). Least promising — inventing a well-founded
   measure that both decomposes over `par` and bounds the run is itself the hard part the
   fuel was standing in for.

**LEFT-BIAS active-step lemma LANDED (`ch466`, 2026-06-22).** The decisive structural fact
this fault fragment hands us: `par` is **LEFT-BIASED**. `okStep (par a b)` runs `a` first
(`interleaveT` does `OptionElim` on `a`'s step before `b`'s), so `b` steps only once `a` is
quiescent. The fuel therefore DOES decompose over `par` after all — not as an arithmetic
split, but as a SEQUENCING: `a` runs to quiescence emitting its trace, then the quiescent
residue lets `b` run (`parQuietNeutral`, `ch464`). `ch466` lands the missing piece of that
sequencing:
- `okStepActive` — when `a` is non-crash and ACTIVE (`okStep a = some ta`), `okStep (par a
  b) = some (tr (labelOf ta) (par (procOf ta) b))`: `a`'s step verbatim, `b` frozen on the
  right (the active-left dual of `ch464`'s `okStepParQuiet`). Proved by `subst` on the two
  hypotheses into `parOk`.
- `parActiveStep` — its `visibleRun` form: `visibleRun (succ k) (par a b) = consVis (labelOf
  ta) (visibleRun k (par (procOf ta) b))`. One fuelled step emits `a`'s label and recurses
  on the residue — the par threading made definitional.
- `closureSup` — closure under prepending an ACTIVE supervised unit `sup = par crash (mon
  halt)` to ANY adequate `b`, proved THROUGH the new machinery (`parActiveStep` emits `lfail`
  and leaves `par halt b`, `parHaltNeutral` strips the `halt`, the hypothesis closes `b`).
- `nestedLAdequate` — `par (par sup sup) halt` (a LEFT-NESTED tree) is adequate by `refl`.
  This is the headline: `ch459`'s tower induction recurses on the RIGHT and cannot reach a
  left-nested par-tree; the active-step decomposition does. Adequacy is no longer confined to
  the right spine.

**RUNNING-LENGTH lemma LANDED (`ch468`, 2026-06-22).** The gap `ch466` left — thread the
running length so the per-step rule composes into "a runs for exactly `ra` steps, then b" —
is now CLOSED, in full generality:

```
runLen : settlesB ra a = true ->
         visibleRun (natAdd ra k) (par a b) = append (visibleRun ra a) (visibleRun k b)
```

For ANY process `a` that settles in exactly `ra` active non-crash steps, running `par a b`
over `ra + k` fuel is `a`'s WHOLE trace followed by `b`'s `k`-step trace. **`par` IS sequential
composition at the observable level, left to right** — proven, not conjectured. Induction on
`ra`: base = `parQuietNeutral` (a already quiescent), step = `parActiveStep` (a's one step) +
the IH + `consVisAppend` (push the emitted label past the concatenation) + `selfStep` (a's own
step on the right).

The substrate forced a workaround worth recording: a `settles : Nat -> Proc -> U` predicate is
BLOCKED (no large elimination — `NatElim` into `U` is a universe error, the same wall as
`exFalso`/`ch247`). So settling is a BOOL predicate `settlesB` (`NatElim` into `Bool`, allowed),
and the per-step transition WITNESS — which `settlesB`'s Bool loses — is recovered by total step
functions (`stepProc`) + Boolean-algebra extraction (`andTrueL`/`andTrueR`/`notTrue`) + `Option`
inversion (`activeFalseNone`/`isActiveTrue`), with `exFalso` into the goal type via `cong` over
the `false = true` contradiction. This is the reusable recipe for "recursive predicate over an
inductive type without large elimination."

**PAYOFF — adequacy COMPOSES over `par` (`parSeqAdequate`).** `project (par a b) = append
(project a) (project b)` definitionally, so `runLen` + the two operand adequacies give adequacy
of the whole: `settlesB ra a = true → (visibleRun ra a = project a) → (visibleRun k b = project
b) → visibleRun (ra+k) (par a b) = project (par a b)`. `nestedGeneral` proves `par (par sup sup)
sup` adequate from this ONE general lemma — not a bespoke induction. Adequacy is now a
COMPOSITIONAL property: prove it for the pieces, get it for the `par`.

**SETTLING COMPOSES — the obligation discharged (`ch469`, 2026-06-22).** The single remaining
obligation (prove the well-supervised class settles) is now closed COMPOSITIONALLY:

```
parSettles : settlesB ra a = true -> settlesB rb b = true
          -> settlesB (natAdd ra rb) (par a b) = true
```

Settling COMPOSES over `par` (a runs its `ra` steps by left-bias, then the quiescent `a` lets `b`
run its `rb`). Base = `settlesQuiet` (a quiescent non-crash peer preserves settling, induction on
`rb` via `ch464`'s `okStepParQuiet`); step = the `parActive` consequences of `ch466`'s
`okStepActive`. With `ch468`'s `parSeqAdequate` (adequacy composes), the TWO compositions give the
all-P refinement for the WHOLE supervised-tree class with no per-shape induction. The capstone
proves `par (par sup sup) (par sup sup)` — a BALANCED tree, neither `ch459`'s right spine nor
`ch466`'s left-nest-to-halt — settling AND adequate purely by composing `parSettles` +
`parSeqAdequate` over its sub-trees (runs to its certified 4-failure trace).

**Status: the interleaving wall is CLOSED for the supervised-tree class.** The progression
`ch466` (left-bias per-step) → `ch468` (running-length / `runLen` / `parSeqAdequate`) → `ch469`
(`settlesQuiet`/`parSettles` + the balanced-tree capstone) takes the E3 all-P refinement from
"open research, the fuel does not decompose over par" to "a proven compositional theorem: any
finite par-tree of supervised units is adequate, by structural composition of two lemmas." Both
compositions (settling and adequacy) and the per-step transition rule are machine-checked, the
outer kernel untouched (Thompson).

**The only residue** is the genuinely-coinductive / NON-settling tail — unbounded fault streams
where no finite `ra` settles the process. That case does not use `runLen`; it rides
`ch208`/`ch209`'s `traceBisim` (interleaving already solved coinductively) with `ch466`'s
active-step rule as the per-step transition law. It is a different proof technique, not the
interleaving obstacle, and has no current consumer (every shipped protocol settles).
