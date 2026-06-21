# R-INFRA — the wavelet cloud-abstraction layer (AS BUILT)

*E4's deploy half. The verified-protocol simulator (`internal/sim` + `rune simulate`)
was already built; this is the DEPLOY side — a provider-agnostic resource model lowered
to concrete deployment artifacts behind an Emitter interface, the "better-than-Winglang"
telos. Plan: `~/.claude/plans/scalable-nibbling-stallman.md`. Branch: `feat/wavelet-infra`.*

## The spine (`infra/`, the deploy-side dual of `codegen/`)

`infra/` mirrors the codegen Backend/ByTarget plugin pattern one level up. Zero new
core; infra is a kernel CONSUMER (the shadow rule), emitting throwaway artifacts.

- **Resource** (`infra/infra.go`) — a provider-agnostic capability described by WHAT it
  provides: `Queue`, `KV`, `Bucket` (object), `Compute`, `Database`. A `LogicalResource`
  (kind, name) is the equivalence witness.
- **Emitter** — lowers a resource graph to an **Artifact** (named files + the logical
  set) for one TARGET. `All()` / `ByTarget()` / `Targets()` register the plugins.
- **Cloud emitters** (`infra/clouds.go`) — `AWS` / `Azure` / `GCP` → OpenTofu/Terraform
  HCL (the same HCL serves both tools). A canonical HCL writer (`infra/hcl.go`,
  `=`-aligned attrs + inline empty blocks) makes output pass `tofu fmt -check` /
  `terraform fmt -check`. Each emitter dispatches on resource KIND and emits shared
  provider scaffolding (Azure resource group / Service Bus namespace / storage account /
  vnet+subnet) once per graph.
- **FOSS emitters** (`infra/foss.go`) — self-hosted backends that run locally under
  Podman (Buildah/Podman, Apache-2.0): `RabbitMQ`, `NATS`, `Valkey`, `Garage`, `Podman`
  (compute), `Postgres`. Each emits a Compose spec + `connection.env`, so the layer is
  exercisable with NO cloud account.

**The equivalence gate** ("equal config → equivalent deployment"): one agnostic graph
lowers to the SAME `LogicalResource` set on every cloud, despite different concrete
resources + plumbing (`harness`/`infra` tests assert it, mirroring backend conformance).

## The abstractions (matrix rows landed)

| Kind | AWS | Azure | GCP | FOSS (Podman) | data-plane `.rune` |
|------|-----|-------|-----|---------------|--------------------|
| queue   | SQS | Service Bus | Pub/Sub | RabbitMQ, NATS | `lib/infra/queue.rune` |
| kv      | ElastiCache | Azure Cache | Memorystore | Valkey | `lib/infra/kv.rune` |
| object  | S3 | Blob | Cloud Storage | Garage | `lib/infra/object.rune` |
| compute | EC2 | Linux VM | Compute Engine | Podman (N replicas) | — (control-plane) |
| database| RDS | PostgreSQL Flexible | Cloud SQL | Postgres | — (control-plane) |

Data-plane abstractions (queue/kv/object) carry a typed `.rune` interface the app
CALLS (enqueue/dequeue, put/get/del — over the packed-String code, assume-tier foreign
ops `rune deploy` binds per backend); they type-check on the prelude
(`harness/wavelet_infra_test.go`). Control-plane resources (compute/database) are
PROVISIONED and connected to via the emitted config.

## `rune deploy` (two modes)

- **Infra mode:** `rune deploy --resource queue|kv|object|compute|database --name N
  --backend <b> [--out dir] [--replicas N] [--image ref] [--fifo]` — lower a resource to
  its artifact (HCL or Compose+env), to a directory or stdout.
- **Workload mode:** `rune deploy FILE [NAME] --target beam` — deploy + RUN a verified
  protocol's actor system on a real backend (reuses the emit-and-execute path). The
  Lambert "it runs" gate: ch436's generic serveG replica, deployed at a G-Counter, comes
  up as live gossiping BEAM actors and converges to its certified value (4). Proven,
  deployed, running, from one source.

## The `protocol … end` block (E4 surface)

`protocol Name is … end` (`surface/parser.go` `parseProtocol`) is a CHECKED GROUPING,
not a namespace: members pass through as ordinary bare top-level defs (so `rune simulate`
and the serveG projection consume init/merge/value/op0… unchanged), but the block is
REJECTED unless it defines the full CvRDT contract — init, merge, value, ≥1 op, AND the
three join-semilattice law proofs (mergeComm/mergeIdem/mergeAssoc). Convergence is
structural, not convention; a missing law is a teaching diagnostic. `protocol` is a
CONTEXTUAL keyword (a block only in `protocol Name is …`; a plain identifier otherwise,
so ch71's `protocol` def is unaffected). One protocol per file; zero new core.
`examples/gcounter_protocol.rune` is the teachable artifact (simulates to 3).

## Verification (CI-testable, no cloud account)

Per row: provider-resource golden + `terraform fmt -check` on every emitted main.tf +
cross-provider logical equivalence + the FOSS Compose/env. Plus the `.rune` interface
type-checks (data-plane), the protocol block accepts/rejects correctly, and `rune deploy
… --target beam` runs the verified CvRDT to convergence (skip-if-escript-absent).

## Remaining (roadmap)

- **Runtime data-plane binding:** bind the queue/kv/object foreign ops to backends via
  codegen host bodies (the `codegen/ioprims.go` pattern) + live Podman round-trips
  (deferred where Podman is absent).
- **Matrix breadth:** Networking (VPC/DNS/LB/CDN), Messaging & Streaming, Storage
  breadth (block/file/archival), more Database (NoSQL/warehouse/cache), Security
  (IAM/KMS/DDoS), DevOps (CI/CD), AI/ML.
- **Cloud apply:** graduate from `fmt`/`validate` to real `apply` once accounts + creds
  exist (a credentialed milestone, not CI).

Tags: v3.291.0 (slice 1: queue/kv/object) · v3.292.0 (protocol block) · v3.293.0
(compute + container) · v3.294.0 (deploy runs a protocol on BEAM) · v3.294.1 (contextual
keyword fix) · v3.295.0 (database).
