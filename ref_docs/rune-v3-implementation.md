# Rune v3.0.0 — Implementation Notes

*What shipped, where it lives, and how the honest line of
[rune-v3-design.md](./rune-v3-design.md) — coexistence now, computation at §F —
is drawn in code.*

## What shipped

The second equality stratum as the third builtin group: a fibrant universe à
la Tarski, embedded in the outer MLTT core. Eleven members:

```
UF     : U1                                      the universe of fibrant types (codes)
El     : UF -> U                                 the underlying outer type of a code
fib    : U -> UF                                 small outer types (sets) are fibrant
piF    : (A : UF) -> (El A -> UF) -> UF          closure under Pi
pathF  : (A : UF) -> El A -> El A -> UF          the inner identity type (fibrant)
preflF : (A : UF) -> (x : El A) -> El (pathF A x x)
pathJ  : inner path induction
pathU  : UF -> UF -> U1                          inner paths between fibrant types
ureflU : (A : UF) -> pathU A A
ua     : iso(A, B) -> pathU A B                  POSTULATED univalence
castU  : (A B : UF) -> pathU A B -> El A -> El B transport
```

What computes (ι-rules in `core/eval.go`, `tryFibIota`):

```
El (fib A)                    ~>  A
El (piF A B)                  ~>  (x : El A) -> El (B x)
pathJ A x P d y (preflF _ _)  ~>  d
castU A B (ureflU _) x        ~>  x
castU A B (ua _ _ f _ _ _) x  ~>  f x
```

What does not compute, on purpose: `pathJ` is typed over `pathF` only, and
`pathU` has exactly one eliminator, `castU` — so "path induction over a
ua-path", the thing that needs cubical machinery, is not expressible as a
stuck term; it is *absent*, which is the cleanest possible labelling of the
frontier. `El (pathF …)` stays neutral: the inner path type is abstract.

| Concern | Where |
|---|---|
| The group, hashes, roles, types | `store/fib.go` (`AddFib`, `FibRoleOf`) |
| ι-rules | `core/eval.go` (`tryFibIota`, `core.FibInfo`, `Machine.Fib`) |
| Wiring | `internal/session` (`Reset`, `elaborator`, `NormalizeExpr`), `elaborate/data.go` |
| Deploy refusal | `internal/session.EmitProgram` (inner taint; below) |
| The gate | `listings/ch09_two_level.rune`, `ch10_univalence.rune`; `harness/listings_test.go` |

## How coexistence works (the load-bearing observation)

The design doc's risk list expected "the evaluator and conversion must now
operate at two levels and respect fibrancy" and "the elaborator must track
which layer each judgment lives in". Neither materialized as new machinery,
for one reason: **the fibrant layer is a Tarski universe, so layer membership
is ordinary typing.** A fibrant type is a VALUE of `UF`; an inner term is a
value of `El c`; the elaborator tracks layers exactly by checking types, which
it already did. Conversion needs no fibrancy cases because every inner former
is a neutral spine over a `Ref` head — the same shapes v1 conversion already
compares.

UIP-vs-univalence is settled structurally rather than by side condition:

- The strict `Eq` is a proof-irrelevant **Prop**. UIP holds there, untouched.
- `pathU A B` is an ordinary **U1 type**. Its elements (`ureflU A`,
  `ua … not …`) are data — distinct, permanently neutral heads. The outer
  layer can neither prove them equal (they do not convert; `refl` is
  rejected) nor prove them distinct (no injectivity axiom exists). The two
  equalities never name the same relation, so they never contradict — which
  is the 2LTT thesis, realized with the machinery already in the house.

The two-layer cache-soundness seam the design flagged also collapses: the
only inner-layer "unfolding" conversion can perform is forcing a scrutinee
through the same glued `Force` that datatype and quotient ι use, so the
dependency log already captures it. There is no second store to track.

## The deploy line (§F in code)

`castU` computes in the checker, including through `ua`. It still must not
deploy: erasure sends proofs and paths to units, so an erased
`castU A B p x` could not dispatch on `p` and would silently compute the
identity where the checker computed `f x`. Rather than emit wrong code or
invent a runtime representation for paths (that IS the §F question),
`EmitProgram` computes an inner-taint set — definitions whose erased bodies
reference `preflF`/`pathJ`/`ureflU`/`ua`/`castU`, transitively — skips them,
and refuses a tainted `main` with an error that cites §F. Fibrant TYPE
formers (`UF`, `El`, `fib`, `piF`, `pathF`, `pathU`) erase to units like any
type, so outer definitions that merely mention fibrant types in their types
deploy as always. `harness/listings_test.go` gates the refusal.

## What the chapters demonstrate

- **ch09** — the stratification itself: `fib`/`El` decoding computing
  (an inner function is a plain lambda), inner sym/trans by `pathJ` with the
  β-rule gated, and an OUTER `Eq`-theorem about INNER paths — the strict
  layer reasoning about the fibrant layer, in one definition.
- **ch10** — univalence postulated, transport computing: `ua` turns the
  `not` involution into `notPath : pathU boolF boolF`; `castU` along it IS
  `not` (provable by bare `refl`, because transport reduces); round-trip via
  the outer involution proof; and the coexistence argument stated as code
  comments where the construction sits.

## Deltas from the design doc

1. **No fibrancy judgment.** A Tarski universe makes fibrancy a type, not a
   judgment — the elaborator, conversion, and the cache are untouched (see
   above). This is the v2 lesson (`builtins, not syntax`) applied to the
   layer everyone assumed needed a new judgment form.
2. **`ua` takes an isomorphism telescope, not an `Equiv` record.** The outer
   core has no Sigma, so the four components (map, inverse, both sections,
   with the strict Eq) are separate arguments. `Equiv` as a fibrant type is
   parked with `sigmaF`.
3. **`castU` computes through `ua`.** The design only promised postulated
   univalence; giving transport its β-rule on `ua` is sound (it is exactly
   the application of the iso's forward map) and is what makes "transport
   across an equivalence as a programming idiom" literal in ch10. The
   higher coherences remain postulated — the frontier is J-over-ua, not
   transport.
