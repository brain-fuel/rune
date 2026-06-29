# Rune

`goforge.dev/rune/v3`, the kernel of a small, **content-addressed, dependently typed**
language. A finished proof is a value, and its identity is the hash of its content,
so verification becomes a cache that never recomputes. Dependent types are a
build-time discipline; what deploys is the erasure, the shadow. The parts people
fight type-theory wars over, the codegen target, the resource semiring, the notion
of equality, each sit behind a clean interface so the right one lives in the right
layer.

## v3.0.0

v3 puts a **second equality** behind the interface the first one defined -
two-level type theory, shipped the way v2 shipped quotients: a builtin
content-addressed group, zero new core syntax, zero hash-format change. The
strict, proof-irrelevant `Eq` (UIP and all) stays exactly as it was and keeps
doing the metatheoretic work; alongside it, a **fibrant universe à la
Tarski** houses the inner layer:

- **`UF`, `El`, `fib`, `piF`**, fibrant types as codes, with decoding that
  COMPUTES: `El (fib A)` is `A`, `El (piF A B)` is the genuine function
  type, so inner functions are plain lambdas.
- **Inner identity, twice.** Point-level `pathF`/`preflF`/`pathJ`, J with
  its β-rule on refl (the v1 equality chapter replays one level in). And
  type-level `pathU`/`ureflU`/**`ua`**, univalence, **postulated**: the
  inhabitant UIP forbids globally now lives locally, as a permanently
  neutral head the strict layer can quantify over but never collapse.
  `pathU A B` is data, not Prop, UIP never applies to inner paths, so the
  two equalities never name the same relation and never contradict.
- **Transport computes.** `castU` reduces on `ureflU` AND through `ua`:
  `castU … (ua … not …) b` IS `not b`, provable by bare `refl`. Transport
  across an equivalence as a programming idiom, literally. What does NOT
  compute, path induction over a ua-path, the higher coherences, is the
  labelled §F frontier (cubical inner layer), postulated, not sold.
- **Check, not run, honestly enforced.** The v3 criterion is that the inner
  chapters elaborate and CHECK (`listings/ch09`-`ch10`). Erasure has no
  meaning for ua-paths yet, so `EmitProgram` skips inner-tainted definitions
  and refuses a tainted main, rather than silently emitting the identity
  where the checker computed `not`.

The thesis, end to end: names where you read, quantities as a parameter,
targets as a menu, and equality as a stratum, even the notion everyone said
you'd have to leave home for. See `ref_docs/rune-v3-implementation.md` for
why the feared two-layer machinery (fibrancy judgments, layer-tracking
elaboration, a second cache seam) collapsed into ordinary typing.

## Distributed: prove, simulate, deploy from one source

The "better than Winglang" thesis: a distributed protocol is **proved correct,
simulated, and deployed from the same verified source**. `rune simulate FILE
[replicas]` runs the protocol (defined by convention: `init` / `merge` / `value`
/ `op0`, `op1`, …) under a deterministic fault policy, a network partition that
divides the replicas, then heals, and prints the per-step trace plus a verdict:

```
rune simulate examples/gcounter.rune 2
  step 0 [r0:op0 r1:op1] => r0=1 r1=1   [converged]
  step 1 [r0:op0] DROP{...} => r0=2 r1=1   [diverged]
  step 2 gossip => r0=3 r1=3   [converged]
  merge laws (CvRDT join): commutative: ok  idempotent: ok  associative: ok
  verdict: CONVERGED ... (and the join laws hold, so under any schedule).
```

The simulator distinguishes a real CvRDT from a broken one **by observed
behaviour, with no appeal to a proof**: a G-Counter (`merge` = pointwise max)
diverges under the partition and re-converges to the correct total, is robust to
dropped/duplicated gossip (idempotence) and to a crashed-then-recovered replica
(durability); a last-writer-wins register stays divergent. The built-in **law
linter** is authoritative, `examples/badcounter.rune` (merge naively adds, so it
double-counts) passes a happy-path run by luck but the linter proves it is not
idempotent and the verdict flags it. The linter also checks that every update is
**inflationary** (only grows the state in the merge order), the op-side condition a
join merge alone cannot guarantee, `examples/resetcounter.rune` has a flawless
pointwise-max merge but an op that resets a tally, and the linter flags it not
convergent even when a run converges by schedule luck. `examples/gcounter.rune` is
all three at once: it loads (so its convergence proof checks), `rune simulate`
converges on it, and `rune run examples/gcounter.rune converged` executes the
scenario on a backend. The simulator lives in `internal/sim` (shadow tooling, never the
kernel); see `ref_docs/wootz/R-PROTO.md`.

Behind the simulator is a **verified distributed algebra** (the `listings/`
corpus). The CRDTs span grow-only and remove-capable shapes (G-Counter, G-Set,
PN-Counter, and a 2P-Set whose tombstones make a concurrent remove win), each
proved convergent; and the convergence criterion itself is proved **once,
generically**: any join-semilattice (a `merge` that is commutative, idempotent,
and associative) enjoys strong eventual consistency, so every CRDT inherits
order-independence, gossip-robustness, and duplicate-safety from one theorem.
Causality is verified too: **vector clocks** with a happens-before partial order
(reflexive, transitive, antisymmetric) that **detects concurrency** (the thing a
scalar Lamport clock cannot), the theorem that their merge is exactly the *join*
of that order, and a **causal-delivery** protocol whose deliverability rule is
proved to respect causal dependencies and exactly-once FIFO ordering. The
adequacy story is closed end to end: over the fault calculus the runtime step
provably emits no communication label, so every observation is a failure (the
spec and runtime observable alphabets coincide).

And it deploys to **live distributed actors**, not just a model: the verified
replicated counter runs as two genuine BEAM processes that hold replica state,
gossip it over `spawn`/`send`/`receive`, and merge to convergence (both replicas
reach the same value), executed on `escript` from the same source that carries the
convergence proof. That is the deploy half of the thesis on real distributed
actors.

## Wavelet: the cloud-abstraction layer

`rune deploy` carries the thesis to real infrastructure. A `protocol … end` block is
a checked CvRDT grouping: it is *rejected* unless it proves its merge is a
join-semilattice (`mergeComm`/`mergeIdem`/`mergeAssoc`), so convergence is structural.
`rune deploy FILE --target beam` then takes a verified protocol and **runs it** as live
gossiping actors on a real backend — proven, deployed, running, from one source.

The infra half is a provider-agnostic resource layer (`infra/`, the deploy-side dual of
`codegen/`): one agnostic configuration → an **equivalent deployment on every cloud**.
Eighteen resource kinds (queue, kv, object, compute, database, secret, nosql, dns, disk,
kms, file, stream, iam, k8s, network, firewall, logs, registry) each lower to AWS /
Azure / GCP as OpenTofu/Terraform HCL (canonical, `terraform fmt -check`-clean) and, for
the data-plane kinds, to a self-hosted backend that runs locally under Podman
(RabbitMQ/NATS, Valkey, Garage, Postgres). `rune deploy --manifest app.wav --backend
aws` emits a whole app's graph as one `main.tf`, each provider's shared scaffolding
de-duplicated; the same manifest lowers to the same *logical* resource set on every
cloud (the equivalence gate). The three data-plane abstractions (queue/kv/object) also
**run**: their foreign ops bind to in-process host bodies, so `rune run
examples/kv_demo.rune --target js|py|go|erl` does a real get-after-put. Managed Redis /
SQS / S3 are ports of the same ops over a live client. See `ref_docs/wootz/R-INFRA.md`.

## v2.0.0

v2 extends the equality stratum with **quotients**, and proves the extension
thesis: the core grew a new type former without one new core constructor, one
new token of syntax, or a hash-format bump. The release criterion is the same
shape as v1: the chapters that use a quotient or a set-level higher inductive
(`listings/ch06`-`ch08`) elaborate, check, and run, and the core gains nothing
they don't use.

- **`A / R` as a first-class type**, `Quot A R`, with point introduction
  `qin`, path introduction `qsound : R a b -> Eq (Quot A R) (qin … a) (qin … b)`,
  the eliminator `qlift` (one respect proof, paid once, then forgotten), and
  Prop-motive induction `qind` (respect free, by proof irrelevance). The five
  are a **builtin group of bodiless, content-addressed definitions**, the
  Phase-4 datatype mechanism, reused whole, and `qlift`/`qind` compute by
  quotient ι-rules in the evaluator: `qlift … (qin … a)` IS `f a`.
- **Eq stays uniform.** Equality at a quotient type is stuck; identification
  is introduced by `qsound` and consumed by the irrelevance machinery v1
  already has. UIP holds untouched; quotient effectiveness is parked with its
  upgrade path (`ref_docs/rune-v2-implementation.md`).
- **Truncation costs nothing.** `‖A‖` is the Church encoding in impredicative
  `Prop` (`listings/ch08`), introduction, elimination into Prop, and
  functoriality as plain definitions. No former ships because no chapter
  needs one.
- **The shadow stays boring.** A quotient compiles to its carrier: `qin` is
  the identity at runtime, a lift is a plain call, and every respect proof and
  path is a unit. `rune run listings/ch07_integers.rune zresult` computes
  2 + (−1) on ℤ under node with the proofs verifiably absent.
- **The chapters**: a parity quotient (ch06); ℤ as pairs-mod-equal-differences
  with `zadd` by double lift, where the outer respect obligation is an
  equality of *functions*, so funext-as-reduction hands it to a pointwise
  `qind`, plus `znegInvol` and `zaddComm` by quotient induction (ch07);
  truncation and the propositional toolkit (ch08).

## v1.0.0

The release criterion was that every listing in `listings/` (the book's code)
elaborates, checks, and runs against this core, and the core contains nothing the
listings do not use. `harness/listings_test.go` enforces it. What v1 ships:

- **The MLTT core with glued NbE**, eval/quote over a glued value domain
  (neutrals carry an un-unfolded spine plus a lazy unfolding), βδηι-conversion
  with a fast syntactic path that forces only on mismatch.
- **The proof cache**, checking is wrapped, not phased: forcing a glued neutral
  IS `store.Unfold`, the sole body gateway, and logs the dependency. Certificates
  are keyed `(content-hash, ‖unfolded-set‖)`; the table is append-only and there
  is **no invalidation logic anywhere** (see `ref_docs/rune-proof-cache-semantics.md`;
  the Frame Lemma is a live property test).
- **Elaboration**, bidirectional checking with contextual metavariables, Miller
  pattern unification, implicit arguments (`{x : A}`, `f {e}`) and holes (`_`).
- **Observational equality** (Pujet-Tabareau), proof-irrelevant `Prop`, `Eq`
  computing on type structure (**funext is a reduction**), `cast` and `subst`
  computing on types and never inspecting proofs; UIP holds canonically.
- **Data by eliminators**, `data … is … end` declares a content-addressed group
  (former, constructors, generated eliminator) with strict positivity; the
  eliminator is the only recursion principle, so **coverage and totality are by
  construction** and proofs by induction compute.
- **QTT**, binders carry 0/1/ω quantities; the elaborator counts usage; the
  0-fragment is the erasure boundary.
- **The predicative universe hierarchy**, `U` (= U_0), `U1`…; `U_i : U_{i+1}`,
  cumulativity, impredicative `Prop` at the bottom. No type-in-type.
- **Codegen**, erasure to a target-independent IR and a JavaScript backend:
  `rune emit FILE [NAME]`, `rune run FILE NAME` (needs node). Proofs, types,
  and the 0-fragment are verifiably absent from the output.

Three representations, one rule: the **surface** is named; the **core** is locally
nameless (de Bruijn indices; top-level references are content hashes); the
pretty-printer is the way back. Hashing is structural over elaborated core -
**never modulo conversion**, so alpha-equivalent definitions are literally equal.

## Build, test, run

```sh
go install goforge.dev/rune/v3/cmd/rune@latest

go test ./...        # the whole gate, including the listings corpus
rune fmt  FILE       # parse -> elaborate -> check -> pretty-print
rune hash FILE       # per definition: the Merkle hash of its checked core
rune repl            # type check + normalize, definitions cached; Nat
                     # arithmetic preloaded (--no-prelude for a bare session)
rune emit FILE NAME  # the erased JavaScript shadow, to stdout
rune run  FILE NAME  # emit and execute under node
rune build FILE [NAME] [--target T] [--kind app|library] [--module M] [--export R[:H]] [--out dir]
#   checked core -> host artifacts (app for any backend; library = Go package today)
rune simulate FILE [n]  # run a distributed protocol under a fault policy (see above)

rune deploy FILE [NAME] --target beam        # deploy + RUN a verified protocol live
rune deploy --resource <kind> --name N --backend <b>   # one resource -> HCL / Compose
rune deploy --manifest app.wav --backend aws # a whole app's graph -> one main.tf
#   kinds:    queue kv object compute database secret nosql dns disk kms file
#             stream iam k8s network firewall logs registry paas
#   backends: aws azure gcp | rabbitmq nats valkey garage podman postgres ...
```

Everything `rune` touches is TYPE CHECKED first; ill-typed files are rejected with
the mismatch, not silently processed.

## The listings

`listings/` is the book-in-progress (*Specify & Verify*) as runnable code:
functions and universes, implicits, observational equality (funext computes,
sym/trans by transport), data and induction (a proof of `add n zero = n` that
reduces to `refl` at numerals), quantities, and, as of v2, quotients (a
parity quotient, then ℤ with lifted arithmetic and quotient-induction
theorems), propositional truncation, and, as of v3, the two-level chapters:
the fibrant layer and inner path induction, then postulated univalence with
computing transport. The harness loads every chapter, checks every
definition, normalizes the marked expressions, runs the data and quotient
chapters through the JS backend, and asserts that inner-layer constructions
refuse to deploy.

See `CLAUDE.md` for the architecture and standing rules, `NON-GOALS.md` for what
this core deliberately is not, `PARKING-LOT.md` for what was deferred and why, and
`ref_docs/` for the v1-v3 design documents.
