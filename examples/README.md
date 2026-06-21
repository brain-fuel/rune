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
| `gcounter.rune`  | counter, `max` join          | `rune simulate examples/gcounter.rune 2`  | convergence (in file) + ch410/ch411 safety; deploys: `rune run examples/gcounter.rune converged` |
| `gcounter3.rune` | counter, three replicas      | `rune simulate examples/gcounter3.rune 3` | ch72 family |
| `gset.rune`      | grow-only set, `or` join     | `rune simulate examples/gset.rune 2`      | ch413 convergence + ch414 safety |
| `pncounter.rune` | PN-counter, compound P/N     | `rune simulate examples/pncounter.rune 2` | ch415 convergence |
| `twopset.rune`   | 2P-Set, remove-permanent     | `rune simulate examples/twopset.rune 2`   | ch422 convergence + removeWins |
| `orset.rune`     | OR-Set, add-wins/re-addable  | `rune simulate examples/orset.rune 2`     | ch109 (faithful) / ch430 (companion) |

`gcounter.rune` is the headline: one verified source that is PROVED convergent
(it loads, so its proof checks), SIMULATED (diverges under a partition,
re-converges to the correct total), and DEPLOYED (`rune run ... converged` prints
3 on every backend, including the BEAM with `--target erl`).

## Non-CvRDTs (the simulator catches them)

| file | bug | what the linter says |
|------|-----|----------------------|
| `lww.rune`         | last-writer-wins, merge takes the peer (no join) | not commutative -> not guaranteed to converge |
| `badcounter.rune`  | merge ADDS instead of `max`, double-counts        | not idempotent -> a happy-path run is "schedule luck, not a property" |
| `resetcounter.rune`| merge is a flawless `max` join, but an op RESETS a tally | inflationary: FAIL -> a non-monotone update breaks convergence even with a perfect merge |

The verdict is linter-authoritative: a protocol whose `merge` is not a join, OR
whose updates are not inflationary, is flagged even when a particular run happened
to converge. `resetcounter.rune` is the inflation-side catch (the merge is
impeccable; the op is the bug).

## Wavelet infrastructure (the `rune deploy` verb, E4)

`app.wav` is a whole application's resource graph (a public-facing web app:
compute + queue + kv + database + secret + iam + object + lb + cdn). One agnostic
manifest lowers to an equivalent deployment on every cloud:

```
rune deploy --manifest examples/app.wav --backend aws    > app.tf
rune deploy --manifest examples/app.wav --backend azure  > app.tf
rune deploy --manifest examples/app.wav --backend gcp    > app.tf
```

A single resource lowers with `rune deploy --resource <kind> --name <n> --backend <b>`.
The 22 agnostic kinds (queue, kv, object, compute, database, secret, nosql, dns,
disk, kms, file, stream, cdn, lb, metrics, iam, k8s, network, firewall, logs,
registry, paas) each lower to AWS/Azure/GCP (OpenTofu/Terraform HCL). 13 also have a
**self-hosted FOSS backend** that runs under Podman with NO cloud account: RabbitMQ/
NATS (queue), Valkey (kv), Garage (object), Podman (compute), Postgres (database),
Dotenv/Vault (secret), DynamoLocal (nosql), CoreDNS (dns), registry:2 (registry),
Redpanda (stream), Loki (logs), Prometheus (metrics), k3s (k8s). See
`ref_docs/wootz/R-INFRA.md`.

## Other

- `sample.rune` - a small non-distributed sample.
- `kv_demo.rune` / `kv_string_demo.rune` / `object_demo.rune` / `queue_demo.rune` -
  wavelet data-plane demos (the typed `.rune` interface the app calls).
- `gcounter_protocol.rune` - a `protocol … end` block (the E4 surface): a checked
  CvRDT grouping that rejects a missing convergence proof.
