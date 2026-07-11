# v4 Float Track B: 9-Backend Float DISPLAY Parity (all magnitudes)

Status: APPROVED (design). Parent: docs/superpowers/specs/2026-07-10-v4-float-guarded-tier-design.md
(Decision 3 + Decision 4). Track A (guarded-tier ops + semantic parity) shipped
v3.381.0. This is the deferred display half.

USER DECISION (2026-07-10): the FULL exact formatter, not the broad-corpus-plus-
documented-boundary fallback. WASM float display must be byte-identical to the
other eight backends at ALL f64 magnitudes, including 16-17 significant-digit
values and |k| > 22 exponents. The parked residue is ELIMINATED, not documented.

## Goal

A raw `Float` displays byte-identically on every one of the 9 backends
(js/py/go/rust/beam/jvm/c/ll/wasm), matching ECMAScript `Number::toString(10)`,
for EVERY finite double (subnormals through 1.8e308), plus `-0`, `Infinity`,
`-Infinity`, `NaN`. Proven by a display-parity conformance gate that PRINTS
floats (both `printFloat` and `show` of a raw Float) over a broad edge corpus.

## Current gaps (verified 2026-07-10)

1. NATIVE C: `show()` renders `K_FLOAT` with `printf("%g", dval)` (c.go:1972),
   which diverges from the ECMAScript `__fmtf` used by `printFloat`. `__fmtf`
   exists (c.go:1111) but is emitted only under `usesFloatIO`
   (parseFloat/getFloat/printFloat), so a program that SHOWS a Float without
   float IO would not even have `__fmtf` to call.
2. NATIVE LL: identical `%g` divergence (ll_runtime.go:501); `__fmtf` is emitted
   by emitFloatIOPrimsLL, same IO-only gate.
3. WASM: the `wasm_float.go` shortest-round-trip formatter reconstructs the
   candidate significand as `(f64)N`, which loses precision for p >= 16 sig
   digits (documented wasm_float.go:29-30); and the atof fast path is correctly
   rounded only for |k| <= 22, D < 2^53 (wasm_float.go:32-36). Both are exact on
   the ch566-568 corpus but wrong off-corpus.

BEAM NaN/Inf display already shipped correct (the Track A rebox: atoms ->
"NaN"/"Infinity"/"-Infinity"). js/py/go/rust/jvm are ECMAScript-canonical via
`__fmtf` / language toString and need only re-verification against the broader
corpus.

## Decision B1: native show -> __fmtf (C and LL)

- Extract `__fmtf` from `emitFloatIOPrimsC` into its own emitter
  `emitFmtfC(b, p)` gated on a NEW predicate `usesFloatValue(p)` = true iff any
  float-VALUE-producing foreign is used (the emitFloatPrimsC set: fromNat/fadd/
  fsub/fmul/fdiv/floatToNat/fleqN/dot2/dotList/gemm/np*/py* ... i.e. anything
  that can put a K_FLOAT on the heap) OR usesFloatIO. A K_FLOAT can reach
  `show` only if some such foreign produced it, so this gate is exactly "a
  Float can be displayed". `emitFmtfC` is called BEFORE both emitFloatPrimsC and
  emitFloatIOPrimsC; those two no longer define `__fmtf` (single definition).
- `show()`'s `case K_FLOAT` calls `__fmtf` into a `char buf[64]` and prints it
  (no trailing newline -- show does not add one), replacing `printf("%g", ...)`.
- Mirror exactly in ll_runtime.go: an `emitFmtfLL` (or the existing
  emitFloatIOPrimsLL `__fmtf`) hoisted to the same broadened gate, and the
  ll show K_FLOAT case routed through it.
- No behavior change for programs that already do float IO (same `__fmtf`).

## Decision B2: WASM exact formatter (Dragon4 over the existing limb bignums)

The WASM runtime already carries arbitrary-precision base-1e9 naturals with
`$big_add`/`$big_sub`/`$big_mul`/`$big_cmp`/`$big_divmod`/`$big_divmod_small`
(wasm_runtime.go). The exact float<->decimal routines ride those; NO new bignum
primitive is written.

FORMAT (f64 -> shortest correctly-rounded decimal, the Steele-White / Dragon4
free-format algorithm):
- Decompose the finite nonzero |x| into `f * 2^e` with f the 53-bit (or
  subnormal) integer significand and e the unbiased power (read the IEEE bits
  via i64.reinterpret_f64, extract mantissa/exponent, handle the implicit
  leading 1 and subnormals).
- Set up the exact rational R/S with margins M+ / M- per Dragon4 (scaling
  numerator and denominator by the appropriate powers of 2 and 5 so that R/S =
  |x| and the loop emits the shortest digit string that rounds back to x).
  Powers of 5 and 2 are built as bignums (repeated `$big_mul` by 5 / by 2, or a
  small power table).
- Generate digits by the standard loop: `d = R*10 / S`, `R = R*10 mod S`,
  terminate when `R < M-` (round down) or `R + M+ > S` (round up), applying the
  even/odd tie rule at the boundary. Emit the digit stream + the decimal point
  position k. This is correctly rounded and shortest for EVERY finite double
  incl subnormals -- no (f64)N step, so no p>=16 precision loss.
- Feed the digit string + k through the SAME four ECMAScript dressing cases the
  formatter already has (k<=n<=21 integer; 0<n<k point; -5<=n<=0 leading zeros;
  else exponential), producing byte-identical output.
- Keep the existing f64 fast path as an OPTIONAL pre-check ONLY if it can be
  proven equivalent; otherwise route all finite nonzero values through Dragon4
  (simplicity + provable exactness beats a fast path with no consumer). Zero,
  -0, Inf, NaN keep their direct special-case strings.

PARSE (decimal string -> correctly-rounded f64, for the round-trip-independent
parseFloat/getFloat and to remove the |k|>22 residue):
- Keep the existing i64 fast path for D < 2^53 and |k| <= 22 (already correctly
  rounded, fast).
- OUTSIDE that range, compute the exact value D * 10^k as a bignum rational and
  round to nearest-even f64 by bignum comparison against the candidate double's
  neighbors (build the trial f64 by scaling, then compare D*10^k to the midpoint
  between it and its successor/predecessor using `$big_cmp`). Correctly rounded
  for all inputs; overflow -> inf, underflow -> 0.

## Decision B3: the display-parity conformance gate (Decision 4 realized)

A new harness gate (harness/float_display_parity_test.go) that, for a corpus of
DISTINCT double values, emits a program PRINTING each value on all 9 backends
and asserts byte-identical stdout, with the expected string taken from Go's
`strconv.FormatFloat(v, 'g', -1, 64)`-equivalent ECMAScript rendering (the
oracle; align formatting to Number::toString, not Go's %v, where they differ --
Go's shortest is the same DIGITS; the DRESSING must match ECMAScript, so the
oracle is the existing ECMAScript renderer, cross-checked once against node).

Two print paths, both exercised (the bug lived in show, not printFloat):
- `printFloat v` (the IO path), and
- `show`-ing a raw Float (the value-display path that used %g). The corpus
  program must actually route a Float through `show` -- confirm the surface/
  runtime path that reaches the native show K_FLOAT case (e.g. a Float in a
  shown pair / the REPL show), and use it.

Corpus (distinct doubles, each built without a Float literal -- via fromNat +
fadd/fsub/fmul/fdiv, since FloatLit does not exist yet):
- boundary integers: 2^53, 2^53+2, 2^63-ish
- trailing-zero: 100, 1000000
- fractions needing 17 sig digits: 0.1 (fdiv 1 10 route), 1/3, 2/3
- subnormal: min subnormal ~5e-324, a mid subnormal
- large/small exponent: ~1e308, ~1e-308
- signs: a negative, -0.0 (fdiv -0 route or fsub 0 0 patterns)
- specials: Infinity (fdiv 1 0), -Infinity (fdiv -1 0), NaN (fdiv 0 0)
Building some of these without literals is fiddly; where a value cannot be built
exactly from small-nat arithmetic, construct the nearest constructible witness
that still exercises the same formatter branch (e.g. a 17-sig-digit value from a
division) and DOCUMENT which branch each witness targets. Every listed FORMATTER
BRANCH (each dressing case + subnormal + specials + 16/17-digit) must have at
least one witness.

WASM is INCLUDED (that is the whole point). Native C/LL included. The gate
Skipfs a backend only by the established missing-toolchain convention.

## Testing summary

- B1: a C/LL program that SHOWS a Float (no float IO) compiles (proves the gate
  broadening) and prints the ECMAScript rendering, not %g. Unit + emit test.
- B2: a WASM differential test (harness or codegen) over a large random +
  edge set of doubles: `$flt_fmt` output == Go strconv shortest for format;
  atof output == Go ParseFloat for parse. Both across the full magnitude range
  incl subnormals, 17-digit, |k|>22. This is the high-bug-surface piece --
  adversarial review + a wide random oracle corpus are mandatory.
- B3: the 9-way display-parity gate, byte-identical, no documented residue.
- Full `go test -timeout 30m ./...` green before the tag.

## Non-goals

- No FloatLit (separate sub-project; the corpus builds values arithmetically).
- No change to the semantic-parity gate (Track A) or the guarded-tier prelude.
- No new core constructor, no hash bump: this is codegen shadow only (Rule 4).
- No change to the ECMAScript dressing rules -- only the DIGIT GENERATION and
  parse precision are corrected on WASM; the four dressing cases are shared.
