# Bible Port — Milestone A: Streaming Foundation (Design)

**Author:** Claude (with Matt)
**Date:** 2026-06-29
**Status:** Approved design; ready for implementation plan.
**Source request:** `~/matt/goforge.dev/bible_feature_request.md` (FR1/FR2/FR3, Milestone A).
**Rune baseline:** local checkout `~/matt/goforge.dev/rune`, v3.346.0.

---

## 1. Purpose

Prove the end-to-end toolchain for porting the `bible` ETL pipeline to a Rune-compiled
Go binary: **stream a CoNLL-U file line-by-line, split columns, count tokens, compile to
Go, run with O(1) live memory.** This is the single highest-leverage unblock named in the
feature request (Milestone A). It validates loops + streaming + strings together on a real
backend. Later milestones (builders, SQLite, miners) are out of scope here and get their
own spec → plan cycles.

## 2. Context that reshaped the request

The feature request's framing of FR1 is **stale**. It cites the pre-C4 `PARKING-LOT.md`
note ("no general recursive def / cyclic defs rejected"), but **C4 / R-PART** since landed
`partial` (single) and `mutual` (mutual) general recursion that **runs via codegen** and is
firewalled from the kernel (the head stays permanently neutral, so conversion never
diverges). ch436's `serveG` is a live parametric replica *loop*; ch39-style countdowns run
to Go. So general recursion that RUNS already exists.

Two further corrections from the codebase:

- **FR3 (strings) partially exists.** ch438/ch439 shipped `++` (Semigroup/Monoid), a `runes`
  view (string → list of code points), `Show`, slicing, and `{expr}` interpolation, deployed
  cross-backend (v3.290). Missing for this milestone: a byte-split and a length.
- **FR2 (streaming) gap is narrower than stated.** `readLineCode` (read one line from
  **stdin**) already exists, plus whole-file `readFileCode`/`writeFileCode`, env, argv, exit.
  The genuine gap is **file-handle streaming over an arbitrary path** — what multi-GB inputs
  need.

### 2.1 The design constraint: no "halfass ports"

The goal is to unblock the port pragmatically **without** accumulating `partial`-based code
that a future principled totality checker (Phase-4) would have to rewrite. The chosen answer
is structural, not a migration discipline: **push the data-plane loop into the host runtime**
(Approach A below). The Rune side of the ETL stays **100% total and non-recursive**, so there
is nothing to migrate when/if a real totality checker lands — the port is born total. `partial`
remains available, but is reserved for genuinely-unbounded *control* (e.g. a server loop),
never for the bounded-but-large data sweep.

### 2.2 Loop-location decision (Approach A)

Three places the per-line loop could live; **A is chosen**:

- **A — host-driven fold (CHOSEN).** `foldLines : Path -> (S -> Bytes -> IO S) -> S -> IO S`.
  Loop in the Go/JS runtime; Rune supplies only a total per-line step. No `partial` in the
  data plane. Strongest answer to "no halfass ports."
- **B — handle + `partial` driver.** `openRead`/`readLine`/`closeRead` + a `partial` Rune loop.
  More flexible (early-exit, rich look-ahead state) but accumulates partial defs needing later
  promotion. Rejected for Milestone A.
- **C — fuel recursor.** `iterate : Nat -> (S->S) -> S -> S` (a `NatElim` wrapper, total).
  Needs the line count up front; wrong for unbounded streams. Useful only for bounded passes.

## 3. Architecture

All three deliverables are **host ops on the existing D6 foreign-op mechanism**
(`codegen/ioprims.go` + per-backend bodies, gated by `usesForeign`) — the precedent set by
`printNat`/`readFileCode`/`readLineCode`. **No `core/`, `store/`, `elaborate/` change. No
hash bump.** This fits the established extension point; it is not a frozen-kernel change.

The one genuinely new piece of runtime machinery: `foldLines` is **higher-order** — its host
body calls back into an erased Rune closure once per line. Every existing ioPrim is
first-order. This callback loop is the main implementation risk and is implemented/tested
first.

`Path` and `Bytes` are the existing packed-`String` code (a `Nat`), so host bodies reuse the
`__s2h`/`__h2s` base-256 codec already shipped for D6 and never touch the constructor
encoding. `Byte` is `Nat`.

### 3.1 The three host ops

| Op | Rune type | Host behavior | IO? |
|----|-----------|---------------|-----|
| `foldLines` | `Path -> (S -> Bytes -> IO S) -> S -> IO S` | open path; `bufio.Scanner` one line at a time (explicit ≥1MB buffer); thread state by applying the Rune step closure per line and running its `IO`; O(1) live memory; return final state. Open-fail → return seed `s0`. | yes |
| `splitOn` | `Byte -> Bytes -> List Bytes` | split a packed line on a byte into a Rune `List Bytes`. | no (pure) |
| `byteLen` | `Bytes -> Nat` | byte length of a packed line. | no (pure) |

`List` here is the corpus `List : U -> U` (`nil`/`cons`); the demo listing carries its own,
as listings do.

### 3.2 Backends

Implement on **Go** (the bible port's real target — the only consumer) and **JS** (fast
conformance gate, no compile step). The higher-order callback is straightforward on both
(curried funcs / curried arrows). Fan-out to py/rust/beam is an explicit, consumer-driven
follow-up (Standing Rule 1), not part of Milestone A.

## 4. Data flow — the demo `main`

A CoNLL-U token counter, end to end:

```
-- step: comment ('#') and blank lines don't count; token lines do.
step : Nat -> Bytes -> IO Nat is
  fn (count : Nat) (line : Bytes) is
    pureIO (if isTokenLine line then succ count else count)
  end end

main : IO Nat is
  bindIO (foldLines corpusPath step zero)
         (fn (n : Nat) is printNat n end)
```

`isTokenLine line` = non-blank AND not starting with `#`:
- blank = `byteLen line = zero`;
- comment = first byte is `#` (decimal 35), read via `splitOn`'s head element or a tiny
  `headByte` derived from the `runes` view — **no fourth host op**; the plan picks the
  in-language route.

Runtime sequence of `foldLines corpusPath step zero`:
1. Host opens `corpusPath`, wraps a line-buffered scanner (O(1) live).
2. Per line: apply erased `step(count)(packedLine)`, run the returned `IO` (here `pureIO v`,
   so extract `v`); that is the new `count`.
3. EOF → return final `count` as `IO Nat` (handed to `bindIO`'s continuation, which prints).

The loop is in the host; the Rune `step` is total and non-recursive; memory stays flat
regardless of file size.

## 5. Error handling

- **Open failure** (missing/unreadable path): `foldLines` returns the seed `s0` unchanged
  (counter → `0`). Documented simplification. The richer
  `foldLinesR : Path -> (S -> Bytes -> IO S) -> S -> IO (Result S IOError)` (mirroring ch215
  `readFileR`) is deferred, out of Milestone-A scope.
- **Scanner error mid-stream**: treated as EOF; returns state so far. Strict handling lands
  with the `Result` variant later.
- **Malformed line** (wrong column count): not `foldLines`'s concern — the `step` decides.
  The counter tolerates any line shape, so no error path is needed for the acceptance test.
- **Long lines**: the scanner's default 64KB token cap is raised to ≥1MB in the host body so
  a pathological line never silently truncates.

## 6. Testing

- **Fixture**: a small committed CoNLL-U file under `harness/testdata/`, ~2 sentences, with
  `#` comment lines, blank sentence separators, and a known token count (11).
- **Conformance gate** (new `harness/io_stream_test.go`, mirroring `io_os_test.go`):
  `rune run fixture main` on **JS** (no toolchain) and **Go** (built to a binary, like ch216)
  both print `11`, byte-identical.
- **`splitOn` unit gate**: `splitOn 9 "a\tb\tc"` (9 = tab) → a 3-element `List Bytes`,
  checked via a small listing that counts the parts.
- **Bounded-memory evidence**: a generated large fixture (1M lines, written in test setup,
  not committed) run through `main`; asserts completion and the exact count. Stands in for the
  spec's 10M-line claim — the streaming guarantee is structural (`bufio.Scanner`); CI runs the
  1M case, the 10M is a documented manual note.
- **listings gate**: the demo listing also lives in `listings/` so
  `TestListingsElaborateAndCheck` keeps it elaborating + checking on every run.

## 7. Acceptance criteria

1. `foldLines`, `splitOn`, `byteLen` elaborate, type-check, and erase on Go + JS; no hash bump.
2. The demo `main` streams the committed CoNLL-U fixture and prints the correct token count
   (11) on both Go (built binary) and JS, byte-identical.
3. `splitOn 9 "a\tb\tc"` yields exactly 3 parts.
4. The 1M-line generated fixture runs to completion with the exact count and flat memory.
5. Full `go test ./...` green; the new ops touch only `codegen/` (+ harness/testdata/listings);
   no `core/`/`store/`/`elaborate/` change, no hash-format bump.

## 8. Explicitly out of scope (named for honesty)

`parseNat`/`showNat`/`sha1Hex`/`toLower`/`utf8Decode` (FR3 tail); JSON/XML (FR4); SQLite (FR5);
numerics `log1p`/`log2` (FR6); maps + stable/external sort (FR7); gzip (FR8); record sugar
(FR9); CLI dispatch (FR10); one-step binary build (FR11); the `Result`-returning stream
variants; and py/rust/beam backends for these ops. Each is a later milestone or its own spec.

## 9. Risks

- **Higher-order host op (primary).** `foldLines` invoking an erased Rune closure in a host
  loop is new. Mitigation: implement + test it first; the erased step is a plain curried
  func/arrow on both target backends. If the callback shape proves awkward on a backend, the
  fallback is Approach B's first-order handle ops on that backend only — but A is expected to
  hold on Go + JS.
- **Scanner buffer cap.** Default 64KB truncates long lines silently; mitigated by an explicit
  ≥1MB buffer.
- **Backend parity scope.** Go + JS only this milestone; the conformance harness asserts parity
  across exactly those two, not all backends (consumer-driven).
