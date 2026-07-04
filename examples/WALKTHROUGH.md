# Better than Winglang: the full pipeline, end to end

The telos-4 claim is "infra-as-code that is provably correct, simulated and deployed
from the same verified source." Here it is, with real commands you can run, on the
artifacts in this directory. Every stage is gated by a test in the suite.

## 1. PROVE — the protocol carries its own convergence certificate

`examples/gcounter.rune` is a G-Counter CRDT. Its `merge` is proven a join-semilattice
(`mergeComm`/`mergeIdem`/`mergeAssoc`, from `maxComm` &c.), so it converges under ANY
gossip schedule — and "it loads" *is* "the proof checks" (the kernel re-validates every
definition on entry).

```sh
rune emit examples/gcounter.rune converged    # elaborates => the proof is machine-checked
```

## 2. SIMULATE — the better-than-Winglang surface catches non-convergence

```sh
rune simulate examples/gcounter.rune 2
# … step 2 gossip => r0=3 r1=3  [converged] … verdict: CONVERGED (and the join laws
# hold, so under ANY schedule)
```

The simulator drives the protocol's OWN verified ops under a fault policy (partition,
duplicate, crash) and a CvRDT law linter. A non-join `merge` (see `lww.rune`,
`badcounter.rune`) is flagged as "schedule luck, not a property" — the catch no
YAML/HCL tool can make.

## 3. DEPLOY — one agnostic manifest, an equivalent deployment on every cloud

```sh
rune deploy --manifest examples/app.rune --backend aws    > app.tf   # OpenTofu/Terraform
rune deploy --manifest examples/app.rune --backend gcp    > app.tf   # … same logical set
```

`app.rune` is a 9-resource web app (compute/queue/kv/db/secret/iam/object/lb/cdn). The
22-row matrix lowers each kind to AWS/Azure/GCP HCL; the equal-config → equivalent-
deployment equivalence is the gate. 13 abstractions also have a self-hosted FOSS backend
(`--backend valkey|redpanda|…`) that runs under Podman/Docker with no cloud account.

### 3b. APPLY: emit is not enough, stand it up (no cloud account, no bill)

`--apply` does not just write the artifact, it runs it. A FOSS backend comes up on
docker compose; a cloud backend applies through terraform, and `--localstack` redirects
the AWS provider at a local fake cloud so the unchanged emitter HCL applies with no
account. `--destroy` tears it back down (the apply-then-destroy lifecycle a CI gate or a
free-tier demo wants):

```sh
# FOSS: stand a real Valkey up from the emitted compose, then tear it down.
rune deploy --resource kv --name cache --backend valkey --apply --destroy

# CLOUD via LocalStack: apply an S3 bucket from the AWS emitter HCL, no account.
docker run -d --name ls -p 4566:4566 localstack/localstack:3      # the local fake cloud
rune deploy --resource object --name data --backend aws --apply --localstack --destroy
```

The same agnostic source emits, then stands up, on a self-hosted broker AND on a
cloud-API-compatible target, both for free. (Gated by `TestApplyFOSSStandingRoundTrip`,
which PINGs the standing Valkey, and `TestApplyLocalStackBucketReallyCreated`, which
applies an S3 bucket and verifies it really exists in LocalStack before destroying it.)

## 4. RUN — the verified value executes, cross-backend

```sh
rune run examples/gcounter.rune converged --target go    # => succ (succ (succ zero)) = 3
rune run examples/gcounter.rune converged --target erl   # => 3 (the BEAM, the natural target)
```

The proven protocol's actors run live on the OTP scheduler — on the BEAM, or off it
(the Go/JVM/JS scheduler shims: goroutines+channels / virtual-threads+queues / async+
AsyncLocalStorage), all observationally identical, with crash/detect/restart faults.

## 5. LIVE STATE — the data plane talks to a real broker

```sh
# bring up the FOSS kv backend, then run the live-binding listing against it:
rune deploy --resource kv --name cache --backend valkey --out ./kv && (cd kv && docker compose up -d)
WAVELET_KV_URL=redis://localhost:6379 rune run listings/ch444_live_kv.rune main --target go   # => world
```

`kvSetLive`/`kvGetLive` (ch444) and `enqueueLive`/`dequeueLive` (ch445) speak RESP over a
raw socket — the wavelet kv/queue abstraction reads and writes a REAL Valkey, on Go/JVM/JS,
with no third-party dependency. (Gated by `TestLiveKVRoundTrip`, which runs all three
backends against one broker.)

## The five outputs — one source set, one gate

Beta success criterion 2, checkable: from {`examples/twotab/counter.rune`,
`examples/wavelet_demo.rune`, `listings/ch538_control_catalog.rune`} the same
content hashes produce all five outputs:

```sh
rune ledger listings/ch538_control_catalog.rune            # 1 proof + ledger view
rune simulate examples/twotab/counter.rune 2               # 2 fault sim (partition + heal)
rune deploy --manifest examples/wavelet_demo.rune --backend aws   > demo.tf   # 3 Terraform
rune deploy --manifest examples/wavelet_demo.rune --backend azure > demo.tf   #   (x3 clouds)
rune deploy --manifest examples/wavelet_demo.rune --backend gcp   > demo.tf
(cd examples/twotab && node build.mjs)                     # 4 the running two-tab app
rune calm emit > demo.calm.json && rune calm validate demo.calm.json   # 5 CALM, re-validates 1:1
```

The binding is literal: the convergence control in the ledger/CALM doc and the
`merge` the browser tabs run are the SAME content hash (`rune hash` shows it;
`TestFiveOutputs*` in cmd/rune gates it, including the hash pin).

---

**One verified source → proven → simulated → deployed (any cloud) → run (any backend) →
live on a real broker.** That is the "better than Winglang" pipeline, and every arrow is
a test in `go test ./...`.
