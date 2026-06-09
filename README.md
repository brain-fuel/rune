# Rune

`goforge.dev/rune` — the kernel of a small, **content-addressed, dependently typed**
language. A finished proof is a value, and its identity is the hash of its content,
so verification becomes a cache that never recomputes. Dependent types are a
build-time discipline; what deploys is the erasure. The parts people fight type-theory
wars over — the codegen target, the resource semiring, the notion of equality — each
sit behind a clean interface so the right one lives in the right layer.

The end goal is **provably correct infrastructure code**. The v1 release criterion is
that every code listing in the book *Specify & Verify* elaborates, checks, and runs
against this core — and the core contains nothing the book does not use.

## Current phase: 0 (the walking skeleton)

Phase 0 locks the foundational shapes and ships a runnable, tested skeleton. It does
**not** implement type checking, normalization, conversion, or type-directed
elaboration — those are Phase 1. What exists now:

- A minimal surface language: variables, lambda, application, dependent function type
  (Pi), `let`, a single universe `U`, and type annotation.
- A locally-nameless core (`core.Tm`), the glued NbE value shape (`core.Val`), and
  structural Merkle hashing addressed by **syntax, never modulo conversion**.
- Name resolution (surface → core) — the only elaboration in Phase 0.
- The pretty-printer (core → named surface) with a `parse ∘ pretty = id` round-trip.
- The content-addressed `store/` with the **body-abstraction barrier**: bodies are
  sealed and reachable only via `store.Unfold`, and SCC-as-unit hashing is laid down.
- The three **strata** interfaces (`equality/`, `quantity/`, `codegen/`), each with
  one v1-bound implementation or a documented stub.
- The property harness: round-trip and hash-invariance-under-alpha hold now; the
  Phase-1+ invariants (type preservation, conversion as equivalence + congruence, the
  proof-cache Frame Lemma) are documented, skipped stubs.

See `CLAUDE.md` for the architecture and standing rules, `NON-GOALS.md` for what this
core deliberately is not, and `PARKING-LOT.md` for what was deferred.

## Build, test, run

```sh
go test ./...          # all green
go vet ./...           # clean
go build -o rune ./cmd/rune

./rune fmt  examples/sample.rune    # parse -> resolve to core -> pretty-print back
./rune hash examples/sample.rune    # print each definition's content hash
```

`rune fmt` round-trips a file (modulo comments); `rune hash` prints, per definition,
the Merkle hash of its elaborated core.

## Layout

```
surface/  lexer, parser, named AST, pretty-printer, name resolution
core/     locally-nameless Tm, glued Val (shape), structural Merkle hashing
store/    sealed bodies, the Unfold gateway, content-addressed map, SCC hashing
equality/ the equality stratum interface (+ Phase-3 stub)
quantity/ the quantity stratum interface (+ default 0/1/ω semiring)
codegen/  the codegen stratum interface (Backend)
harness/  property-test scaffolding
cmd/rune/ the CLI
```

## Dependencies

Go standard library, plus two direct dependencies: `pgregory.net/rapid` (property
testing) and `goforge.dev/blake3sum` (BLAKE3 content hashing, behind `core.Hash`).
