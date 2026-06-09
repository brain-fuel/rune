# Parking Lot

Improvements that were tempting during Phase 0 but are **not required** by the
current goal. Each is parked with a one-line rationale. Nothing here may be built
until a current listing (eventually a *Specify & Verify* listing) needs it.

## Deferred by phase plan (well-understood swaps)

- **Thread quantities through binders.** Pi/Lam carry no `quantity.Qty` yet — there
  is no QTT consumer until Phase 5. The semiring interface and the default 0/1/ω
  instance exist; wiring is deferred. (quantity/)
- **The equality stratum implementation.** `equality.Observational` is a Phase-3
  stub that panics; the interface is fixed now, the OTT machinery comes with the
  Phase-1 NbE it hooks into.
- **eval / quote / conversion.** `core.Val` is shape-only; NbE is Phase 1.
- **Erasure and a backend.** `codegen` is interface + stub; erasure and a concrete
  `Backend` are Phase 7.
- **Universe hierarchy.** One universe `U` now (`type : type` stance); a real
  hierarchy is Phase 6 — a one-time cache nuke, deliberately late.
- **Proof cache + dependency log.** `store.Unfold` is the choke point; the
  write-only `DepSet`, the conversion monad, and the certificate table arrive with
  Phase-1 conversion. The Frame Lemma is already stated as a skipped property.

## Tempted in Phase 0, not built

- **BLAKE3 content hashing.** Phase 0 uses `crypto/sha256` to honour the
  one-dependency rule; `goforge.dev/blake3sum` exists and could be swapped in behind
  `core.Hash` later (bump the preimage tag — it changes every hash). Rationale:
  no measured need yet; stdlib keeps the dependency surface at one (rapid).
- **Recursive-definition resolution.** `store.HashSCC` and positional `Placeholder`
  lay down SCC-as-unit hashing, but the resolver wires only the acyclic case; the
  CLI rejects recursive groups. Cyclic resolution (placeholder rewriting in the
  resolver) is Phase 1. Rationale: no recursive listing to exercise it yet.
- **Multiset / incremental dependency-set hashing.** The proof cache will
  canonicalize the dependency set by sort-then-hash; a commutative incremental
  combiner is a tempting optimization with cancellation/collision hazards. Parked
  behind the sign reading *here be invalidation*.
- **Conditional-fact cache keying.** Keying certificates on "`e` reduces to `v`"
  rather than on `e`'s full content would recover reuse across harmless body edits,
  but reintroduces a validation procedure (invalidation logic). Explicitly out of
  scope; the cache asserts only "this exact configuration of bodies checks."
- **Multi-binder surface sugar / richer layout.** `\x y -> e` desugars to nested
  lambdas; otherwise the surface is exactly: variables, lambda, application, Pi,
  let, U, annotation. No data types, implicits, or modules beyond a flat list of
  definitions until a phase needs them.
- **Sigma types, pairs, equality formers in the surface.** The v1 design's `Tm`
  includes `Sig`/`Pair`/`Eq`/`Cast`; Phase 0's surface and core deliberately omit
  them (no consumer yet). They enter with their phases.
- **Persistence of the store.** The content-addressed map is in-memory; on-disk
  persistence has no Phase-0 consumer.
- **Readable error spans / source positions in core.** Tokens carry offsets; the
  core does not thread positions yet. Deferred until the checker needs them.
