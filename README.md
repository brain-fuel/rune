# Rune

`goforge.dev/rune` — the kernel of a small, **content-addressed, dependently typed**
language. A finished proof is a value, and its identity is the hash of its content,
so verification becomes a cache that never recomputes. Dependent types are a
build-time discipline; what deploys is the erasure — the shadow. The parts people
fight type-theory wars over — the codegen target, the resource semiring, the notion
of equality — each sit behind a clean interface so the right one lives in the right
layer.

## v1.0.0

The release criterion was that every listing in `listings/` (the book's code)
elaborates, checks, and runs against this core — and the core contains nothing the
listings do not use. `harness/listings_test.go` enforces it. What v1 ships:

- **The MLTT core with glued NbE** — eval/quote over a glued value domain
  (neutrals carry an un-unfolded spine plus a lazy unfolding), βδηι-conversion
  with a fast syntactic path that forces only on mismatch.
- **The proof cache** — checking is wrapped, not phased: forcing a glued neutral
  IS `store.Unfold`, the sole body gateway, and logs the dependency. Certificates
  are keyed `(content-hash, ‖unfolded-set‖)`; the table is append-only and there
  is **no invalidation logic anywhere** (see `ref_docs/rune-proof-cache-semantics.md`;
  the Frame Lemma is a live property test).
- **Elaboration** — bidirectional checking with contextual metavariables, Miller
  pattern unification, implicit arguments (`{x : A}`, `f {e}`) and holes (`_`).
- **Observational equality** (Pujet–Tabareau) — proof-irrelevant `Prop`, `Eq`
  computing on type structure (**funext is a reduction**), `cast` and `subst`
  computing on types and never inspecting proofs; UIP holds canonically.
- **Data by eliminators** — `data … is … end` declares a content-addressed group
  (former, constructors, generated eliminator) with strict positivity; the
  eliminator is the only recursion principle, so **coverage and totality are by
  construction** and proofs by induction compute.
- **QTT** — binders carry 0/1/ω quantities; the elaborator counts usage; the
  0-fragment is the erasure boundary.
- **The predicative universe hierarchy** — `U` (= U_0), `U1`…; `U_i : U_{i+1}`,
  cumulativity, impredicative `Prop` at the bottom. No type-in-type.
- **Codegen** — erasure to a target-independent IR and a JavaScript backend:
  `rune emit FILE [NAME]`, `rune run FILE NAME` (needs node). Proofs, types,
  and the 0-fragment are verifiably absent from the output.

Three representations, one rule: the **surface** is named; the **core** is locally
nameless (de Bruijn indices; top-level references are content hashes); the
pretty-printer is the way back. Hashing is structural over elaborated core —
**never modulo conversion** — so alpha-equivalent definitions are literally equal.

## Build, test, run

```sh
go install goforge.dev/rune/cmd/rune@latest

go test ./...        # the whole gate, including the listings corpus
rune fmt  FILE       # parse -> elaborate -> check -> pretty-print
rune hash FILE       # per definition: the Merkle hash of its checked core
rune repl            # type check + normalize, definitions cached
rune emit FILE NAME  # the erased JavaScript shadow, to stdout
rune run  FILE NAME  # emit and execute under node
```

Everything `rune` touches is TYPE CHECKED first; ill-typed files are rejected with
the mismatch, not silently processed.

## The listings

`listings/` is the book-in-progress (*Specify & Verify*) as runnable code:
functions and universes, implicits, observational equality (funext computes,
sym/trans by transport), data and induction (a proof of `add n zero = n` that
reduces to `refl` at numerals), and quantities. The harness loads every chapter,
checks every definition, normalizes the marked expressions, and runs the data
chapter through the JS backend.

See `CLAUDE.md` for the architecture and standing rules, `NON-GOALS.md` for what
this core deliberately is not, `PARKING-LOT.md` for what was deferred and why, and
`ref_docs/` for the v1–v3 design documents.
