# Rune — Working Discipline

`goforge.dev/rune` — the kernel of a small, content-addressed, dependently typed
language. The end goal is provably correct infrastructure code; the v1 release
criterion is that every listing in the book *Specify & Verify* elaborates, checks,
and runs against this core, and the core contains nothing the book does not use.

This file is authoritative for how work proceeds. The reference designs live in
`ref_docs/` (`rune-v1-design.md`, `rune-v2-design.md`, `rune-v3-design.md`,
`rune-proof-cache-semantics.md`).

## Architecture

Three representations, one rule. The surface is **named**. The core is **locally
nameless**: bound variables are de Bruijn indices; references to top-level
definitions are **content hashes**, never names. The pretty-printer turns core back
into named surface for display. The only elaboration in Phase 0 is **name
resolution** (surface → core): binders to de Bruijn indices, free identifiers to
definition refs. There is no type elaboration in Phase 0.

```
surface/   lexer, parser, named AST, pretty-printer, name resolution  (names live here)
core/      locally-nameless Tm; glued Val domain (shape only); structural Merkle hash
store/     sealed bodies, the Unfold gateway, content-addressed map, SCC hashing
equality/  the EQUALITY stratum interface (+ Phase-3 stub)
quantity/  the QUANTITY stratum interface (+ default 0/1/ω semiring)
codegen/   the CODEGEN stratum interface (Backend; erased IR -> target source)
harness/   property-test scaffolding — the gate from day one
cmd/rune/  the CLI: `rune fmt`, `rune hash`
```

The core term encoding is the conventional Go AST encoding: a **sealed interface
with an unexported marker method, one small struct per constructor, matched by type
switch** (`core.Tm`, `core.Val`, `surface.Exp`). Committed to everywhere; do not mix
encodings.

### The strata are interfaces, not hardcoded choices

The three parts people fight type-theory wars over each sit behind a Go interface so
the right one lives in the right layer. v1 ships one implementation of each.

- **equality/** — equality type formers + future eval/quote/conversion hooks. The
  load-bearing interface of the roadmap: the store, hashing, semiring, codegen, and
  surface/nameless split are all orthogonal to it. v2 *extends* it (quotients); v3
  *swaps* it (two-level type theory).
- **quantity/** — the usage semiring; default instance is 0/1/ω. The 0-fragment is
  the erasure boundary. QTT is wired into binders in Phase 5, not before.
- **codegen/** — `Backend`: erased IR → target source. One plugin per target.

### Glued values (shape only in Phase 0)

`core.Val` is a glued NbE domain: a neutral carries both its un-unfolded spine and a
lazy unfolding (a thunk). Forcing that thunk is the **same operation** as
`store.Unfold`, and is where the Phase-1 proof-cache dependency log hooks. eval/quote
are not implemented in Phase 0.

## Phase map

- **Phase 0 (done):** lock the irreversibles + walking skeleton — term/value/hashing
  shapes, the strata interfaces, the body barrier, name resolution, pretty-printer,
  `rune fmt`/`rune hash`, the property harness.
- **Phase 1:** the MLTT core with glued NbE (eval, quote, conversion).
- **Later:** metavariables/unification/implicits; the OTT equality stratum; data,
  coverage, totality; turn on QTT; universe hierarchy; codegen + a backend.
- The equality stratum is then **extended** (v2 quotients) and a second equality
  stratum is **added** (v3 two-level type theory).

## Standing rules

1. **Parking lot.** Any improvement not required by the current goal goes to
   `PARKING-LOT.md` with a one-line rationale — not into code. Scope is capped by
   demonstrated need: add no feature with no consumer. Eventually the cap is the
   listings in *Specify & Verify*.
2. **Never hash modulo conversion.** A definition's identity is the Merkle hash of
   its elaborated core, computed structurally. Hashing MUST NEVER call eval,
   normalize, or a future conversion routine. Because the core is de Bruijn,
   alpha-equivalent terms are literally equal and hash equal.
3. **The body barrier.** A definition's type is public; its body is sealed inside
   `store/`. Bodies are reachable only through `store.Unfold`, the sole gateway and
   the proof-cache instrumentation point. This is a compile-time fact, not vigilance.
4. **Mutate the shadow, not the source.** Any future optimization IR is built on
   erased, throwaway codegen output, never on the immutable core/store.

## Engineering conventions

- Go standard library only, plus one property-testing library
  (`pgregory.net/rapid`). No other dependency without recording why.
- The harness is the gate from day one: `parse ∘ pretty = id` and hash-invariance
  under alpha-renaming hold now; the Phase-1+ invariants (type preservation,
  conversion as equivalence + congruence, the proof-cache Frame Lemma) exist as
  documented, skipped property stubs that the mutation-testing layer will hunt.
- Conventional Commits for every commit.
