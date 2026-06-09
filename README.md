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

## Current phase: 0 (the walking skeleton); surface at v0.2.0

Phase 0 locks the foundational shapes and ships a runnable, tested skeleton. It does
**not** implement type checking, normalization, conversion, or type-directed
elaboration — those are Phase 1. What exists now:

- A surface language (authoritative spec: [`ref_docs/GRAMMAR.md`](ref_docs/GRAMMAR.md)):
  variables, curried `fn (x : A) is e end` lambdas, application, dependent function
  type (Pi), inline `let`, Elixir-style `seq … end` sequencing, a single universe `U`,
  and parenthesized ascription. Definitions are `name : T is e end`.
- A locally-nameless core (`core.Tm`), the glued NbE value shape (`core.Val`), and
  structural Merkle hashing addressed by **syntax, never modulo conversion**.
- Name resolution (surface → core) — the only elaboration. Surface presentation never
  changes a definition's content hash: hashing is over core, so the v0.2.0 syntax
  switch left every hash fixed.
- The pretty-printer (core → named surface) with a `parse ∘ pretty = id` round-trip.
- The content-addressed `store/` with the **body-abstraction barrier**: bodies are
  sealed and reachable only via `store.Unfold`, and SCC-as-unit hashing is laid down.
- The three **strata** interfaces (`equality/`, `quantity/`, `codegen/`), each with
  one v1-bound implementation or a documented stub.
- The property harness: round-trip, hash-invariance-under-alpha, and seq-desugaring
  hold now; the Phase-1+ invariants (type preservation, conversion as equivalence +
  congruence, the proof-cache Frame Lemma) are documented, skipped stubs.

See `CLAUDE.md` for the architecture and standing rules, `NON-GOALS.md` for what this
core deliberately is not, and `PARKING-LOT.md` for what was deferred.

## Build, test, run

```sh
go install goforge.dev/rune/cmd/rune@latest   # installs the `rune` binary into $GOBIN

go test ./...          # all green
go vet ./...           # clean
go build -o rune ./cmd/rune                    # or build from source

./rune fmt  examples/sample.rune    # parse -> resolve to core -> pretty-print back
./rune hash examples/sample.rune    # print each definition's content hash
./rune repl                          # interactive read -> resolve -> show loop
```

`rune fmt` round-trips a file (modulo comments, bound-variable names, and `fn` binder
annotations); `rune hash` prints, per definition, the Merkle hash of its elaborated core.

### `rune repl`

An honest front-end over the existing pipeline: it **resolves and pretty-prints**, and
does not evaluate or type-check (those are Phase 1). Enter an expression to see its
round-tripped core, or a `name : T is e end` definition to add it to the session.
Multi-line forms continue at a `...>` prompt until they parse.

```
rune> id : (A : U) -> A -> A is
...>   fn (A : U) (x : A) is x end
...> end
defined id
rune> id U
id U
rune> :core fn (A : U) (x : A) is x end
(λ. (λ. #0))
```

Commands: `:core <expr>`, `:hash <expr>`, `:type <expr>` (`:t`, a Phase-1 stub),
`:list`, `:load <path>`, `:reset`, `:help` (`:h`), `:quit` (`:q`; Ctrl-D also exits).

## Layout

```
surface/          lexer, parser, named AST, pretty-printer, name resolution
core/             locally-nameless Tm, glued Val (shape), structural Merkle hashing
store/            sealed bodies, the Unfold gateway, content-addressed map, SCC hashing
equality/         the equality stratum interface (+ Phase-3 stub)
quantity/         the quantity stratum interface (+ default 0/1/ω semiring)
codegen/          the codegen stratum interface (Backend)
harness/          property-test scaffolding
internal/session  the shared parse -> resolve -> hash pipeline
internal/repl     the rune repl loop
cmd/rune/         the CLI (`rune fmt`, `rune hash`, `rune repl`)
```

## Dependencies

Go standard library, plus two direct dependencies: `pgregory.net/rapid` (property
testing) and `goforge.dev/blake3sum` (BLAKE3 content hashing, behind `core.Hash`).
