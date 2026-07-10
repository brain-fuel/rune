# v4 Float: Guarded-Tier Numeric Type + 9-Backend Numeric Parity

Status: APPROVED (design). Next: implementation plan (writing-plans).
Parent: docs/superpowers/plans/2026-07-06-three-majors-roadmap.md (v4 numerics).
Consumes: the v4 Ord campaign (Ord/DecEq classes + the ops/laws split, shipped
v3.378-380) and the v4 algebra hierarchy (Semiring/SubR ops classes). Float is
the FIRST guarded-tier numeric type - the carrier the class/law split was built
to admit.

## Premise

The numeric tower's classes are split into OPS (dispatchable Sigma-records:
Semiring, SubR, Ord, DecEq) and LAWS (And-chain proof records: SemiringLaws,
OrdLaws, ...). The GUARDED TIER is a carrier that instantiates the ops classes
but has NO laws record. IEEE754 f64 is the canonical guarded-tier carrier: it
violates laws on BOTH axes at once -

- ORDER: `NaN <= NaN` is false, so `fle` is not reflexive; `NaN` is incomparable,
  so the order is not total; no OrdLaws.
- ALGEBRA: float `+` is not associative (`(a+b)+c != a+(b+c)` for many f64), and
  `*` does not distribute exactly; no SemiringLaws.

So Float reuses the EXISTING ops classes as instances WITHOUT laws records, and
the operators `+ - * le compare eqb` all work on Float through the same dispatch
the rest of the tower uses. The prelude already anticipates this: the SubR
section comment reads "so proof-free carriers (IEEE754, later) still get ops".

A second, equal goal: PARITY. Every numeric type (Whole, Int, Frac, Float) must
produce byte-identical arithmetic and comparison results on all 9 backends
(js, py, go, rust, beam, jvm, c, ll, wasm). Float already runs 9-way (f64 ops
baked everywhere; `TestIOFloatDoubleDemo` is a 9-way gate), but float DISPLAY has
a parked divergence (native `show` uses `%g`; WASM has off-corpus rounding
residue). This spec closes that so display parity is proven, not just semantic
parity.

## Current state (verified 2026-07-10)

- `Std.Float` (internal/prelude/prelude.rune:8720) declares `Float : U`, `fromNat`,
  `fadd`/`fsub`/`fmul`/`fdiv`, `parseFloat`, `getFloat`, `printFloat`. NO comparison
  is declared in the prelude.
- `fleqN : Float -> Float -> Whole` (1 if a<=b else 0; NaN yields 0, the IEEE
  partial order) is baked on all 9 backends (codegen/ioprims.go:177 + per-backend
  bodies) but is NOT declared as a `foreign` in the always-on prelude (only in the
  ch217 listing).
- Operators dispatch on existing classes: `+`/`*` need `Semiring A`
  (prelude:148/151); `-` needs `SubR R A` (prelude:553). No "Num" class exists.
- printFloat is ECMAScript Number::toString canonical on ALL 9 backends: js/py/go/
  rust/beam/jvm share `__fmtf`; native C/LL have their own `__fmtf` precision-search
  (c.go:1111); WASM has a WAT formatter (wasm_float.go). It is 9-way byte-identical
  on the ch566-568 lock corpus.
- The parked residue (PARKING-LOT.md:559-573): (a) native `show` of a raw K_FLOAT
  uses `printf("%g")` (c.go:1972, ll_runtime.go:501), which diverges from `__fmtf`
  on some values; (b) WASM formatter's `p>=16` reconstruction and `|k|>22` parse are
  not correctly rounded on edge inputs outside the lock corpus.

## Decision 1: guarded-tier ops instances (no laws)

Add to the prelude (near the tower's other instances / the Std.Float region), each
with a comment naming the exact law the carrier violates:

- `foreign fleqN : Float -> Float -> Whole` declared in `Std.Float` (body exists).
- `instance semiringFloat : Semiring Float is mkSemiring Float (fromNat 0) (fromNat 1) fadd fmul end`
  (enables `+` and `*` on Float). NO `SemiringLaws Float` (non-associativity).
- `instance subRFloat : SubR Float Float is fsub end`
  (enables `-` on Float; result type Float). NO laws.
- `instance ordFloat : Ord Float is mkOrd Float fle (compareFromLe Float fle) end`
  (enables `le`/`compare`). NO `OrdLaws Float` / `OrderedSemiring Float` (NaN).
- `instance decEqFloat : DecEq Float is mkDecEq Float feq end`
  (enables `eqb`). NO DecEq laws (NaN != NaN, -0 = +0).

Use the ACTUAL constructor names from the prelude (`mkSemiring`/`mkOrd`/`mkDecEq`
and their argument order) - confirm each at implementation time.

## Decision 2: derived comparison/equality/NaN (zero new primitives)

All derived from the single `fleqN`, no new foreign, no codegen:

- `fle : Float -> Float -> Bool is fn (a b : Float) is notB (isZero (fleqN a b)) end`
  (fleqN returns 1/0; isZero 0 = true -> notB -> false; isZero 1 = false -> notB ->
  true; confirm `isZero`/`notB` names).
- `feq : Float -> Float -> Bool is fn (a b : Float) is and (fle a b) (fle b a) end`
  Matches IEEE `==`: `-0 == +0` is true (both `fle` directions hold); `NaN == NaN`
  is false (`fle NaN NaN` is false).
- `isNaN : Float -> Bool is fn (x : Float) is notB (fle x x) end`
  A NaN is the only f64 not `<=` itself (`fleqN NaN NaN = 0`).
- `totalCompare : Float -> Float -> Ordering` - a NaN-last TOTAL order for sorting:
  both-NaN -> `eq`; exactly-one-NaN -> the NaN side is greatest (`gt`/`lt`);
  else `compareFromLe Float fle a b`. Reflexive, antisymmetric, total. Documented
  as pragmatic (does not distinguish -0/+0; not the full IEEE754 bit-level
  totalOrder). Written with `fle x x` NaN-tests (a NaN fails `fle x x`).

The guarded-tier lesson is stated in ch577 and (briefly) in prelude comments: the
ops exist and compute, but the laws do NOT hold - a would-be `OrdLaws Float` refl
obligation `Le Float ordFloat x x` is FALSE at `x = NaN`, and a would-be
`SemiringLaws Float` associativity is false for representative f64. The chapter may
include a concrete refutation witness (e.g. a specific f64 triple where
`feq (fadd (fadd a b) c) (fadd a (fadd b c))` is false) to make "no laws" evidence,
not just assertion. If a clean closed refutation is awkward, a narrated comment
plus the ABSENCE of the laws records suffices; the chapter must at minimum RUN the
ops and show they compute.

## Decision 3: float-display parity across all 9 backends (Track B)

Close the parked display divergence so a raw Float displays byte-identically on
every backend, matching ECMAScript Number::toString:

- NATIVE C and LL: route the `show`/value-display K_FLOAT case through the existing
  `__fmtf` (ECMAScript shortest-round-trip) instead of `printf("%g")` (c.go:1972,
  ll_runtime.go:501). Widen the `__fmtf`-emission gate so `__fmtf` is present
  whenever a K_FLOAT can be displayed (today it is gated on float-IO usage; a
  program that shows a Float without float IO must still get `__fmtf`). Keep one
  `__fmtf` definition per native runtime (no duplication).
- WASM: fix the `wasm_float.go` off-corpus residue - the `p >= 16` shortest-digit
  reconstruction must use exact integer (i64/limb) arithmetic rather than `(f64)N`
  (which loses precision), and `|k| > 22` parse must round correctly (or, if a
  correctly-rounded parse for extreme exponents is out of scope, the corpus stays
  within representable-exact range and the residue is re-documented with its exact
  boundary - but the DEFAULT is to fix it).
- SOURCE backends (js/py/go/rust/beam/jvm): already ECMAScript-canonical via
  `__fmtf`/language toString; verify against the broadened corpus, align any gap.

## Decision 4: the numeric-tower parity gate (the unifying acceptance)

A conformance corpus (harness/, mirroring the existing multi-backend gates) with
two parts, both byte-identical across all 9 backends:

- SEMANTIC parity (all four types): Whole/Int/Frac/Float each exercise arithmetic
  (`+ - *`) and comparison (`le`/`compare`/`eqb`), emitting ONLY discrete results
  (Bool->tag, Ordering->tag, Whole->`printNat`). Float arithmetic correctness is
  checked by COMPARISON (`feq (fadd (fromNat 1) (fromNat 2)) (fromNat 3)` -> true),
  never by printing a float - so semantic parity is independent of display.
- DISPLAY parity (Float): a dedicated corpus that PRINTS floats (printFloat and
  `show` of a raw Float) over a broad value set - subnormals, large/small
  exponents, boundary integers, trailing-zero cases, -0, Infinity, NaN - asserted
  byte-identical on all 9 backends. This is the Track-B acceptance and the gate
  that catches any native/WASM formatter gap as a real bug.

Any divergence on any backend on either part is a REAL bug to fix, not to exclude.
No backend is skipped except by the established environment-gating conventions
(missing jvm/wasmtime/clang toolchain uses the existing Skipf pattern).

## Decision 5: chapter + REPL acceptance

- ch577 (self-contained mirror in the ch574/575 style): the guarded-tier Float
  type - `+ - *` and `le`/`compare`/`eqb` on Float, `totalCompare` with a NaN, and
  the guarded-tier point (ops present, laws absent, with the refutation-or-narration
  per Decision 2).
- REPL pins (mandatory acceptance): `fadd`/`+` on Float, `fle`, `compareOf Float
  ordFloat`, `eqbOf Float decEqFloat`, `totalCompare` on a `fdiv (fromNat 0)
  (fromNat 0)` NaN, and `+ - *` operator forms resolving the Float Semiring/SubR
  instances. Confirm the operators pick the Float instances (the Semiring/SubR
  dispatch must resolve Float; test in `rune repl`).

## Architecture / isolation

- TRACK A is PURE PRELUDE (internal/prelude/prelude.rune) + one `foreign`
  declaration whose bodies already exist. No new core, no new codegen, no new
  classes. Kernel frozen.
- TRACK B is CODEGEN SHADOW (codegen/c.go, codegen/ll_runtime.go, codegen/
  wasm_float.go, and the display-path gating). Rule 4 (mutate the shadow). No core
  change, no hash bump.
- The two tracks are independent: Track A can land and be tested (semantic parity)
  before Track B (display parity). The plan sequences A first (small, high
  confidence), then B (the codegen bulk), then the unified parity gate spans both.

## Testing summary

- Track A: prelude loads with the instances; ch577 elaborates/checks/runs; REPL
  pins; semantic parity gate (all four types, discrete outputs, 9-way).
- Track B: display parity gate (broad float value set, printFloat + show, 9-way);
  the native `show` and WASM edge fixes each proven by the corpus.
- The class-hash audit (TestTowerClassHashesDistinct) is UNCHANGED: Float adds
  INSTANCES of existing classes, no new record former.
- Full `go test -timeout 30m ./...` green before the tag.

## Non-goals

- No `FloatLit` core literal and no hash bump (own later sub-project). Float values
  are built from `fromNat` + arithmetic; `3.14` still means an exact `Frac` at the
  literal level.
- No `OrdLaws Float` / `SemiringLaws Float` / any Float laws record - the point is
  that they do not hold. (A guarded-nonzero or NaN-free SUBTYPE with laws is a
  possible far-future refinement; not now.)
- No full IEEE754 bit-level totalOrder (-0/+0 and NaN-sign distinctions);
  `totalCompare` is the pragmatic NaN-last order sufficient for sorting.
- No new float arithmetic primitives beyond the existing fadd/fsub/fmul/fdiv/
  fromNat/fleqN.

## Sequencing

- Plan track A (guarded-tier instances + derived compare/eq/isNaN/totalCompare +
  semantic parity gate + ch577 + REPL). Tag on green.
- Plan track B (native show->__fmtf, WASM edge fix, display parity gate). Tag on
  green. May be one plan with two task groups or two plans; the writing-plans step
  decides based on task independence.
