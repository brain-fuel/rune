# divmod-small port + scale-gate re-admission: Design

Date: 2026-07-06
Status: Approved by the author in-session ("approve, go").
Seed: main @ v3.371.0 (6e ARC complete both native backends; scale gate still
excludes c/ll per the measured Task 4 STOP).

## Premise (what profiling actually found)

The v3.371.0 exclusion blamed "per-alloc bignum volume." A flat profile of
the real workload (one entry reproduced standalone, gprof at -O2, 150-file
sample; artifacts in the session scratchpad perf/ dir) refutes that: 100% of
the hot time is the subtree d6_s2h -> big_divmod. The C runtime's big_divmod
is a general schoolbook division with a ~30-probe binary search per limb
(each probe a big_mul + big_cmp + big_monus with fresh temps): 58.4M big_mul
calls and 24.1M calloc/free pairs per 150 files, all DOWNSTREAM of the
search. The packed-string bignums genuinely reach ~203 base-1e9 limbs.

WASM passes the same gate because wasm_runtime.go:845 ships
`$big_divmod_small`: divide a base-1e9 bignum by a small constant in ONE
O(nlimbs) pass with an i64 running remainder, one quotient allocation, no
temps. The C and LLVM runtimes never got it. Measured prototype of the port:
C goes 440ms -> 6.4ms per entry at -O2 (68x), byte-identical output, FASTER
than the Go backend (7.2ms).

Consequences for the candidate list: int64 small-nat fast lane helps nothing
here (the values are giant string payloads) - stays parked; arena/freelist
is ~1.4x and redundant; codec representation rewrite unnecessary.

## Decision 1: port big_divmod_small to both native runtimes

- C: add `big_divmod_small` to cRuntime mirroring the WASM routine (single
  descending-limb pass, `long long` running remainder `r = r*1e9 + limb;
  q_limb = r / d; r %= d`, one result alloc via the existing K_BIG
  constructors, big_norm at the end).
- `big_divmod(a, b, &rem)` dispatches to it when b fits a single limb
  (nlimbs(b) <= 1, i.e. b < 1e9); the general binary-search path stays for
  large divisors. Callers (d6_s2h's 256, decimal to_radix paths, show)
  benefit without being touched.
- LLVM: the identical function into llRuntimeC under the twin-linkage
  discipline (extern where the .ll calls it - it does not; static suffices
  since only big_divmod calls it; keep `== c.go` sync markers).
- ARC: the fast path allocates exactly the quotient; no temps to release.
  rem is returned as a C `long` out-param where the small path is taken
  internally by big_divmod - keep big_divmod's existing signature and
  semantics (fresh non-aliased *rem bignum) so no caller changes and the
  Plan-A non-aliasing rewrite is preserved.

## Decision 2: the scale gate, re-admitted for real

Remove the c/ll exclusion block in harness/bible_conformance_test.go
(including its measured-miss comment), keep the per-backend t.Logf timings,
run the N=1500 real-data gate with BIBLE_REPO. Expected per the prototype:
c and ll rows within the same order as the source backends. If a row still
materially blows the budget: STOP and report numbers (same contingency as
before) - but the standalone measurement says it will not.

## Decision 3: honest bookkeeping

- PARKING-LOT: the "per-alloc ARC cliff" entry CLOSES with a correction
  note: profiling showed the earlier attribution wrong; the cause was a
  missing division specialization WASM already had; fixed by this port.
- Beta checklist item 13: fully DONE (strike style).
- codegen doc headers: no changes needed beyond the runtime comment at the
  new function.

## Decision 4: regression guards

- Correctness: a divmod-small unit gate - decode/encode round-trip of a
  ~1KB packed payload on c and ll, byte-compared against the go backend's
  output for the same program (the conformance suites already pin this
  broadly; the new test pins the specific shape cheaply).
- Perf: the same test asserts a generous wall-clock ceiling for the 1KB
  decode (e.g. under 2 seconds where the pre-fix shape took tens), so the
  specialization cannot silently regress to the binary-search path. Skip
  cleanly where cc/clang absent.
- ARC balance: the existing TestCARC/TestLLARC bignum gates re-run; the new
  path must keep rt_live invariance.

## Non-goals

- No int64 immediate fast lane (parked; no consumer after this fix).
- No arena/freelist, no codec representation change, no WASM edits
  (wasm_runtime.go already has the routine - frozen).
- No changes to perceus.go/closure.go/kernel.

## Testing summary

New: divmod-small correctness+ceiling gates (c, ll). Re-run: full codegen
(incl ARC families + TestLLConformsToC), harness conformance, THE N=1500
real-data gate with timings recorded, full `go test -timeout 30m ./...`
before tag.
