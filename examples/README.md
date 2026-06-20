# Examples

Runnable Rune programs. The distributed ones are driven by the `rune simulate`
verb (the better-than-Winglang simulator, `internal/sim`), which folds a
protocol's own verified operations forward under a fault policy and prints a
trace, a CvRDT law report, and a verdict. See the "Distributed" section of the
top-level README and `ref_docs/wootz/R-PROTO.md`.

Each protocol file follows the simulate convention: `init`, `merge`, `value`, and
one local op per replica `op0`, `op1`, ....

## CvRDTs (converge under any schedule)

| file | lattice | run | proof |
|------|---------|-----|-------|
| `gcounter.rune`  | counter, `max` join        | `rune simulate examples/gcounter.rune 2`  | convergence (in file) + ch230/ch231 safety; deploys: `rune run examples/gcounter.rune converged` |
| `gcounter3.rune` | counter, three replicas    | `rune simulate examples/gcounter3.rune 3` | ch72 family |
| `gset.rune`      | grow-only set, `or` join   | `rune simulate examples/gset.rune 2`      | ch233 convergence + ch234 safety |
| `pncounter.rune` | PN-counter, compound P/N   | `rune simulate examples/pncounter.rune 2` | ch235 convergence |

`gcounter.rune` is the headline: one verified source that is PROVED convergent
(it loads, so its proof checks), SIMULATED (diverges under a partition,
re-converges to the correct total), and DEPLOYED (`rune run ... converged` prints
3 on every backend, including the BEAM with `--target erl`).

## Non-CvRDTs (the simulator catches them)

| file | bug | what the linter says |
|------|-----|----------------------|
| `lww.rune`        | last-writer-wins, merge takes the peer (no join) | not commutative -> not guaranteed to converge |
| `badcounter.rune` | merge ADDS instead of `max`, double-counts        | not idempotent -> a happy-path run is "schedule luck, not a property" |

The verdict is linter-authoritative: a protocol whose `merge` is not a join is
flagged even when a particular run happened to converge.

## Other

- `sample.rune` - a small non-distributed sample.
