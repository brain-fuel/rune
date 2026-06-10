# Rune

`goforge.dev/rune` — the kernel of a small, **content-addressed, dependently typed**
language. A finished proof is a value, and its identity is the hash of its content,
so verification becomes a cache that never recomputes. Dependent types are a
build-time discipline; what deploys is the erasure — the shadow. The parts people
fight type-theory wars over — the codegen target, the resource semiring, the notion
of equality — each sit behind a clean interface so the right one lives in the right
layer.

## v2.0.0

v2 extends the equality stratum with **quotients** — and proves the extension
thesis: the core grew a new type former without one new core constructor, one
new token of syntax, or a hash-format bump. The release criterion is the same
shape as v1: the chapters that use a quotient or a set-level higher inductive
(`listings/ch06`–`ch08`) elaborate, check, and run, and the core gains nothing
they don't use.

- **`A / R` as a first-class type** — `Quot A R`, with point introduction
  `qin`, path introduction `qsound : R a b -> Eq (Quot A R) (qin … a) (qin … b)`,
  the eliminator `qlift` (one respect proof, paid once, then forgotten), and
  Prop-motive induction `qind` (respect free, by proof irrelevance). The five
  are a **builtin group of bodiless, content-addressed definitions** — the
  Phase-4 datatype mechanism, reused whole — and `qlift`/`qind` compute by
  quotient ι-rules in the evaluator: `qlift … (qin … a)` IS `f a`.
- **Eq stays uniform.** Equality at a quotient type is stuck; identification
  is introduced by `qsound` and consumed by the irrelevance machinery v1
  already has. UIP holds untouched; quotient effectiveness is parked with its
  upgrade path (`ref_docs/rune-v2-implementation.md`).
- **Truncation costs nothing.** `‖A‖` is the Church encoding in impredicative
  `Prop` (`listings/ch08`) — introduction, elimination into Prop, and
  functoriality as plain definitions. No former ships because no chapter
  needs one.
- **The shadow stays boring.** A quotient compiles to its carrier: `qin` is
  the identity at runtime, a lift is a plain call, and every respect proof and
  path is a unit. `rune run listings/ch07_integers.rune zresult` computes
  2 + (−1) on ℤ under node with the proofs verifiably absent.
- **The chapters**: a parity quotient (ch06); ℤ as pairs-mod-equal-differences
  with `zadd` by double lift — where the outer respect obligation is an
  equality of *functions*, so funext-as-reduction hands it to a pointwise
  `qind` — plus `znegInvol` and `zaddComm` by quotient induction (ch07);
  truncation and the propositional toolkit (ch08).

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
reduces to `refl` at numerals), quantities, and — as of v2 — quotients (a
parity quotient, then ℤ with lifted arithmetic and quotient-induction
theorems) and propositional truncation. The harness loads every chapter,
checks every definition, normalizes the marked expressions, and runs the data
and quotient chapters through the JS backend.

See `CLAUDE.md` for the architecture and standing rules, `NON-GOALS.md` for what
this core deliberately is not, `PARKING-LOT.md` for what was deferred and why, and
`ref_docs/` for the v1–v3 design documents.
