# Wavelet Beta: Design

Date: 2026-06-25
Status: Approved (brainstorming), pending spec review
Seed state: `goforge.dev/rune` @ v3.329.0 (kernel + 9 backends + distributed layer + Wavelet infra + ~537 stdlib listings)

## Premise

Wavelet is a content-addressed, dependently-typed language whose killer application is
**verified Infrastructure as Code**: an architecture model (FINOS CALM) whose controls
("encrypted in transit", "least-privilege IAM", "data stays in region X") are not documented
assertions but machine-checked claims, traceable 1:1 from the macro architecture down to the
running micro code, deployed across any cloud from a single source.

The public noun for the beta is **Wavelet, a verified IaC tool**: lead with the outcome a buyer
pays for. The language is the engine under the hood, revealed to builders who want the substrate.
"Becomes GA" means Wavelet GA.

The differentiator must always route through proof. If a direction does not cash out as a theorem
(or an honestly-labeled non-theorem) that someone cares about, it is just another IaC tool and is
named as such.

## The beta in one artifact

The entire beta reverse-engineers from a single artifact whose three "oh" moments are facets of one
modeled system, not three separate demos:

A shared collaborative counter/note (a CRDT) open in two browser tabs syncing over WebRTC, modeled
in CALM, where one source erases to five outputs:

1. **Proof.** The control catalog discharged: flagship controls proven, the tail contract-guarded,
   every control labeled in the Assurance Ledger.
2. **Fault sim.** The existing simulator runs the CRDT under adversarial schedules and partitions,
   judging convergence by behavior.
3. **Multi-cloud Terraform.** Relay service, persistence store, region pin, minimal IAM, emitted for
   AWS / Azure / GCP.
4. **Running app.** WASM (pure merge logic) plus JS (DOM and WebRTC glue), two tabs converging live.
5. **Re-validating CALM doc.** Emitted CALM JSON carrying per-control assurance plus git provenance,
   validated 1:1 against the source model (round-trip).

The 1:1 binding: the CALM node "store" IS the Wavelet service value whose IAM proof is about the
exact access set its erased code performs. Macro and micro are one value.

## Section 1: The Assurance Ledger (the differentiator)

A control instance is `(proposition, witness)`.

- The **proposition is a content-hashed type** (a stable identity for a claim, e.g. "this service is
  in-region"). The **witness** carries a tier: `proven` (a real proof term), `guarded` (a runtime
  contract), `assumed` / `postulate` (a bodiless axiom).
- **Honest about limitations.** The tier is part of the emitted artifact, not a footnote. The CALM
  doc annotates each control with `{tier, proposition-hash, proof-hash?, why}`, so "in-region:
  POSTULATED, cloud API not yet modeled" is on the page.
- **Attributable via version control.** A `postulate` / `assume` is a surface keyword on a real
  source line, so `git blame` already names the committer, commit, and date. The ledger reads blame
  keyed to each witness; no new VC mechanism is invented.
- **Upgradeable, and the upgrade is verifiable.** Because the proposition is content-hashed,
  replacing a postulate witness with a proof of the same proposition-hash is a tracked transition:
  the ledger shows `postulated@alice -> proven@bob`, same claim, stronger witness. CI can gate: no
  new postulates without sign-off; named flagship controls must stay `proven` or the build fails.
- **Kernel stays sacred.** A postulate is a bodiless content-addressed definition (the kernel already
  has these for constructors and builtins). Provenance is git tooling over the existing store. The
  upgrade detector is content-addressing doing what it already does. Zero kernel change.

This makes the assurance LADDER the product, which turns the "verified vs merely runnable" tension
into a feature: proof where it counts, honest and attributable labels everywhere, postulates visibly
a debt that later commits pay down.

## Section 2: The evaluation model (lazy pure core, concurrent-effect frontier)

- **Pure layer.** Call-by-need. Forcing order is unobservable (confluence), so values stay
  deterministic. Thunks are forced in dependency order.
- **Effect layer.** Effectful actions are nodes in a data-flow DAG. An action is on the **frontier**
  when its dependencies are satisfied; the scheduler dispatches the M frontier actions across N
  executors, so independent effects (print Hello, print World) interleave nondeterministically from
  one run to the next. The DAG is implicit from data flow (B consumes A's result implies B after A).
- **`seq ... end`.** A linearization barrier. Actions inside take a total program order (World always
  after Hello). This is the only explicit ordering.
- **`do` (inside `seq`).** Re-opens a concurrent block: its internal effects race on the frontier,
  but the `do` block as a whole is one ordered step of its enclosing `seq`. Layers alternate:
  `async` contains `seq` contains `do(async)`.
- **Correctness under every interleaving is a first-class proof target.** Because effects interleave
  nondeterministically by default, invariants must hold under all schedules. This is exactly what the
  CRDT and distributed proofs establish (convergence is order-independent), so the eval model and the
  verification brand become one story: async by default, proven safe under all schedules, `seq` only
  where order is genuinely required. Order-independence proofs reuse the existing CRDT and
  commutativity machinery as the obligation discharger.
- **Failure and cancellation.** Structured-concurrency cancellation under the existing supervisor
  model: a failed frontier action cancels its siblings up to the enclosing structured region.
- **Kernel stays sacred.** Laziness and the frontier scheduler are codegen and runtime concerns. The
  kernel's NbE conversion and definitional equality are untouched (eval order cannot change results
  in a total language). The N-executor scheduler is a per-backend runtime component (goroutines,
  promises and workers, BEAM processes, threads), constrained by the byte-identical conformance gate.

## Section 3: The artifact end to end

The language is authoritative; CALM is a projection that round-trips.

- App: shared counter/note CRDT, two browser tabs, WebRTC sync, merge is the CRDT join (proven
  convergent under all schedules).
- CALM model: nodes {web app, signaling/relay service, persistence store}; relationships carry
  controls {encrypted-in-transit, least-priv-IAM, in-region, CRDT-convergence-under-all-schedules}.
- One source erases to the five outputs above.
- Eval model made visible: merge logic is lazy-pure; WebRTC sync, persistence, and deploy are
  concurrent frontier effects; a `seq` block orders deploy steps (store provisioned before the relay
  references it).

## Section 4: Scope (v0.1 vs honest roadmap, 6 months)

### In v0.1 (the beta)

- **Language core.** The lazy plus `seq` / `do` frontier eval model (the headline new language work),
  all 9 backends emit (they already do), human-grade errors, REPL.
- **Assurance Ledger.** Tiers plus git provenance plus content-hash upgrade detection plus a CI gate.
- **Control catalog.** A small blessed set fully proven (least-priv-IAM, in-region,
  encrypted-in-transit, CRDT-convergence-under-all-schedules) plus the guarded-tail framework.
- **CALM.** Language-to-CALM emit plus round-trip validate on the demo's controls.
- **Wavelet infra.** One cloud live (deploys for real) plus the other two emit-HCL-only.
- **Browser.** Scoped: WASM merge plus JS/WebRTC glue plus two `<div>`s. Not a platform.
- **The artifact.** The two-tab CRDT demo, runnable, plus a short script or book chapter that is the
  go-to-market.

### Honest roadmap (post-beta)

Multi-cloud live; arbitrary CALM ingest (author-in-CALM for FINOS shops); full control-catalog
breadth; explicit concurrency primitives beyond the scheduler; browser-platform breadth (on demand
only); deep per-host FFI (which must never leak into the proven core).

### The uniformity invariant (resolves tension #3, trade owned)

No privileged host. Uniform 9-target parity is a v0.1 invariant, enforced by the byte-identical
conformance gate. The non-leaky abstraction is the product. The "lives beside" story becomes "beside
whatever you run, same verified source, byte-identical to all nine, no second-class target."

The trade being accepted: forgo Kotlin-style deep single-host intimacy (idiomatic per-host FFI) in
exchange for no leaks. The counter, which holds specifically for a verification product: a leaky
abstraction would falsify the proofs (a control proven on the IR must hold on every target, or the
brand is a lie). Uniformity is therefore load-bearing for correctness, not merely tidy. Deep per-host
FFI is explicitly roadmap.

## Section 5: Naming and CLI

- Language and product unified under **Wavelet**. Domains held: `wavelet-lang.{com,net,org}`.
- Public framing: a verified IaC tool.
- CLI: `wvli` (interpreter / REPL), `wvlc` (compiler), `wvl <subcommand>` for everything else
  (deploy, sim, prove, calm emit/validate, ledger).
- Rename from Rune is a mechanical pass over the existing surface, CLI, module path, and docs; the
  content-addressed identities are unaffected by names.

## Constraints and invariants

- The kernel is finished and load-bearing. No kernel changes. Content-addressing and erasure are
  non-negotiable substrate.
- The differentiator routes through proof. Anything that does not is named as plain IaC.
- Consumer-driven is the house style. Two conscious bends are documented and owned: (1) the beta is
  scoped from a target artifact rather than purely consumer-pulled, because "the book / the demo" is
  the consumer-generator; (2) uniform 9-target parity is held as an invariant rather than pulled by a
  single host consumer, because leaks would falsify proofs.
- Byte-identical cross-backend conformance is the gate that prevents leaky abstractions.

## Success criteria

The beta is worth shipping if, on the one artifact:

1. A skeptical staff engineer changes posture on contact with the proven-minimal-IAM diff plus the
   Assurance Ledger view (proven, labeled, blamed, upgradeable).
2. One source produces all five outputs, and the emitted CALM doc re-validates 1:1.
3. The CRDT converges live in two browser tabs and is proven convergent under all schedules, with the
   same source running byte-identical across the nine backends.
4. The Assurance Ledger honestly shows every control's tier, provenance, and (for postulates) the
   reason and the upgrade path; CI can gate on it.

## Decomposition into implementation plans

This is a 6-month, multi-pillar beta and is too large for a single implementation plan. It decomposes
into these sub-plans, each its own spec-to-plan-to-implementation cycle, roughly in dependency order:

1. **Eval model**: lazy core plus `seq` / `do` plus the N-executor frontier scheduler, per backend,
   under the conformance gate; the order-independence proof obligation.
2. **Assurance Ledger**: `postulate` / `assume` / `guard` / `proven` surface, the content-hash
   proposition identity, git-provenance reader, upgrade detector, CI gate.
3. **Control catalog**: the blessed proven set plus the guarded-tail framework.
4. **CALM round-trip**: language-to-CALM emit, ingest for validation, the 1:1 traceability binding.
5. **Wavelet infra one-cloud-live**: the demo's relay plus store plus region plus IAM, live on one
   cloud, HCL-only on the other two.
6. **The artifact**: the two-tab CRDT app (WASM plus JS plus WebRTC), wired to the above, plus the
   go-to-market script / book chapter.
7. **Rename pass**: Rune to Wavelet, CLI `wvl` / `wvli` / `wvlc`.

## Open questions and risks

- Beachhead and first design partner (FINOS-member bank vs cloud-cost startup vs eBPF vendor) remain
  founder calls; they inform but do not block the build.
- Build-in-public vs stealth-until-the-book is unresolved and is a GTM call.
- The N-executor scheduler across nine backends under a byte-identical gate is the largest technical
  risk in the eval-model sub-plan; nondeterministic effect ordering must remain reproducible enough
  to test (a seeded scheduler for the conformance suite).
- CALM schema coverage is deliberately partial in v0.1 (demo controls only); arbitrary ingest is
  roadmap, which may slow FINOS author-in-CALM adoption until built.
