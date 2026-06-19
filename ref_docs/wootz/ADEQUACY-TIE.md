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
