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

**What stays open.** The fully-general arbitrary-`par` case — two SIMULTANEOUSLY-active
non-trivial branches where `a`'s quiescence TIMING (how many steps before `b` gets to run)
must be threaded through the fuel — is not closed; `parActiveStep` gives the per-step rule
but composing it to "a runs for exactly `j` steps then b runs" still needs a running-length
lemma. `ch466` is the structural bridge toward that, not the closure of it.

**Recommendation (still open at the limit).** Pursue (1): restate the general refinement as
trace-equality of the LIMIT streams via `traceBisim`, using `prefixMono`/`lenBound` as the
finite-approximation bridge and `ch466`'s active-step rule as the per-step transition law.
This keeps the proof coinductive (where `par` interleaving is already solved in
`ch208`/`ch209`) and the outer kernel fixed (Thompson). The open item remains open, but
`ch466` converts it from "the fuel does not decompose over par" to "the fuel decomposes as a
left-biased SEQUENCING; thread the running length" — a narrower, more concrete gap. This
leverages landed
machinery rather than waiting on a fuel-decomposition lemma that does not exist.
