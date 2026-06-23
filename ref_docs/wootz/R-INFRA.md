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
  Podman (Buildah/Podman, Apache-2.0): `RabbitMQ`/`NATS` (queue), `Valkey` (kv),
  `Garage` (object), `Podman` (compute), `Postgres` (database), `Dotenv`/`Vault` (secret),
  `DynamoLocal` (nosql), `CoreDNS` (dns), `LocalRegistry` (registry:2), `Redpanda`
  (stream, Kafka API), `Loki` (logs), `Prometheus` (metrics), `K3s` (k8s), `NFS` (file), `VaultKMS` (kms transit). Each emits a Compose spec + `connection.env`, so the layer is
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
| secret  | Secrets Manager | Key Vault | Secret Manager | dotenv template, Vault (dev) | — |
| nosql   | DynamoDB | Cosmos DB | Firestore | DynamoDB-local | — |
| dns     | Route 53 | Azure DNS | Cloud DNS | CoreDNS | — |
| disk    | EBS | Managed Disk | Persistent Disk | — | — |
| kms     | KMS | Key Vault key | Cloud KMS | Vault transit | — |
| file    | EFS | Azure Files | Filestore | NFS server | — |
| stream  | Kinesis | Event Hubs | Pub/Sub | Redpanda (Kafka API) | — |
| iam     | IAM role | Managed Identity | Service Account | — | — |
| k8s     | EKS | AKS | GKE | k3s | — |
| network | VPC | VNet | VPC | — | — |
| firewall| WAF | DDoS Plan | Cloud Armor | — | — |
| logs    | CloudWatch | Log Analytics | Cloud Logging | Loki | — |
| registry| ECR | ACR | Artifact Registry | registry:2 (Distribution) | — |
| paas    | Beanstalk | App Service plan | App Engine | — | — |
| cdn     | CloudFront | CDN profile | Cloud CDN backend bucket | — | — |
| lb      | ELBv2 | Load Balancer | Forwarding rule | — | — |
| metrics | CloudWatch dashboard | Monitor workspace | Monitoring dashboard | Prometheus | — |
| warehouse | Redshift Serverless | Data Explorer (Kusto) | BigQuery | ClickHouse | — |
| inference | SageMaker | ML online endpoint | Vertex AI endpoint | Ollama | — |
| archive | Glacier | Storage (Cool tier) | GCS ARCHIVE class | MinIO | — |
| serverless | Lambda | Linux Function App | Cloud Functions (gen2) | Fn Project | — |
| devops | CodeBuild | Container App Job | Cloud Build trigger | Woodpecker CI | — |

Shared Azure scaffolding is emitted once per graph: the resource group (always), the
Service Bus namespace (queue), Event Hub namespace (stream), storage account
(object+file, `needsStorageAccount`), Key Vault (secret+kms, `needsKeyVault`),
vnet+subnet (compute). 27 rows total. A whole multi-resource graph lowers to the same
logical set on every cloud (`TestMultiResourceEquivalence`); `rune deploy --manifest`
emits one app's graph at once.

Data-plane abstractions (queue/kv/object) carry a typed `.rune` interface the app
CALLS (enqueue/dequeue, put/get/del — over the packed-String code); they type-check on
the prelude (`harness/wavelet_infra_test.go`). Control-plane resources are PROVISIONED
and connected to via the emitted config.

## The data plane RUNS (in-process binding, cross-backend)

The queue/kv/object foreign ops bind to real host bodies baked per backend (the
`codegen/ioprims.go` `usesForeign` pattern), so a program using these abstractions
RUNS unaided via `rune run` / `rune deploy --target <b>` — not just emits config:
- **js** module-global `Map`, **py** dict, **go** package map keyed by the `*big.Int`
  code's string (`usesDataPlane`-guarded `__kvkey`), **erl** the per-process dictionary
  (the program runs in one process; object/queue keys tagged `{obj,K}`/`{q,Q}`). Queues
  are FIFO; absent reads return code 0 (the `leW code 1` empty sentinel). Rust parked
  (needs `OnceLock`+`Mutex`, like the fileenv prims).
- `TestDataPlaneRunsCrossBackend`: kv get-after-put = 42, object = 99, queue FIFO, on
  every present backend.

## The data plane is LIVE (a real broker, v3.327.x)

Beyond the in-process binding, the kv + queue abstractions now talk to a REAL broker:
`kvSetLive`/`kvGetLive` (SET/GET) and `enqueueLive`/`dequeueLive` (LPUSH/RPOP = FIFO)
speak the Redis/Valkey wire protocol (RESP) over a raw TCP socket to `$WAVELET_KV_URL`
— hand-rolled, NO third-party dependency. Implemented on **Go** (stdlib `net`) and the
**JVM** (`java.net.Socket`); both block, so RESP is synchronous (a JS binding would need
the async path). ch444 (kv → "world") and ch445 (queue → "first") round-trip through a
live Valkey from `rune deploy`, gated by `TestLiveKVRoundTrip` (docker-up → run on Go
AND JVM → down). Object is the same SET/GET protocol as kv. So the wavelet data plane is
proven → deployed → in-process-run → LIVE on a real broker.

## `rune deploy` (two modes)

- **Infra mode:** `rune deploy --resource queue|kv|object|compute|database --name N
  --backend <b> [--out dir] [--replicas N] [--image ref] [--fifo]` — lower a resource to
  its artifact (HCL or Compose+env), to a directory or stdout.
- **Workload mode:** `rune deploy FILE [NAME] --target beam` — deploy + RUN a verified
  protocol's actor system on a real backend (reuses the emit-and-execute path). The
  Lambert "it runs" gate: ch436's generic serveG replica, deployed at a G-Counter, comes
  up as live gossiping BEAM actors and converges to its certified value (4). Proven,
  deployed, running, from one source.
- **Apply mode (`--apply`, LANDED):** emit is not the end; `--apply` STANDS THE ARTIFACT
  UP (`infra/apply.go`, `infra.Apply`). Dispatched on `Emitter.Cloud()`: a FOSS backend
  comes up on `docker compose up -d` (and `down -v` on `--destroy`); a cloud backend
  applies through `terraform init`/`apply`. `--localstack[=URL]` writes a terraform
  OVERRIDE file (`localstack_override.tf`, whose `_override.tf` merge replaces the
  base `provider "aws"` endpoints + creds) so the UNCHANGED emitter HCL applies against a
  local fake cloud with NO account and NO bill. `--destroy` is the apply-then-teardown
  lifecycle (a CI gate or a free-tier demo that leaves nothing running). So the same
  agnostic source emits, then stands up, on a self-hosted broker AND a cloud-API target,
  both for free. The shadow rule holds: Apply writes throwaway files into a work dir and
  shells out to the standard tools, never touching core/store.

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
The APPLY path is gated live with NO cloud account: `TestApplyFOSSStandingRoundTrip`
(infra) + `TestDeployApplyFOSSCLI` (cmd) stand a real Valkey up via compose and PING it;
`TestApplyLocalStackBucketReallyCreated` (infra) + `TestDeployApplyLocalStackCLI` (cmd)
apply an S3 bucket from the AWS emitter HCL against LocalStack, VERIFY the bucket really
exists, then destroy it. All skip cleanly without docker / terraform / the LocalStack
image, so the suite stays green offline.

## Live data-plane binding (LANDED v3.327.0–v3.327.x, ch444/ch445)

The in-process data-plane ops now point at a REAL broker. `kvSetLive`/`kvGetLive` (ch444)
and `enqueue`/`dequeue` (ch445) speak RESP over a raw socket (stdlib `net`, dep-free) to
`$WAVELET_KV_URL`: kv = SET/GET, queue = Valkey LIST LPUSH/RPOP (FIFO), object = the same
SET/GET as kv. LANDED on ALL FOUR source backends Go+JVM+JS+Rust (Go/JVM block on the
socket, JS awaits `node:net`, Rust over `std::net::TcpStream` with hand-rolled RESP, no
crate — v3.328.3); `TestLiveKVRoundTrip` round-trips all four against one real Valkey
through docker → "world". The packed-string V::Nat is marshalled to/from host bytes by
the same base-256 `_s2h`/`_h2s` pair on every backend.
The live Valkey round-trip itself is `TestKVLiveRoundTripDocker` (emit spec → docker compose
up → PONG → down, skip-if-no-docker).

## Remaining (roadmap)

- **Managed-cloud clients:** beyond the self-hosted RESP broker — managed Redis / SQS / S3
  clients over the wire (needs accounts, the cloud-apply milestone). The four source
  backends (Go/JVM/JS/Rust) all speak RESP to a self-hosted broker today.
- **Matrix breadth: COMPLETE.** The serverless + devops rows landed at representative
  cloud fidelity (the cloud side is dependency-heavy — Lambda packages, CodeBuild source/role —
  emitted with vars; the FOSS forms Fn/Woodpecker are single-container and run with no account).
  (27 rows landed: queue/kv/object/compute/database/secret/nosql/dns/disk/kms/file/
  stream/cdn/lb/metrics/iam/k8s/network/firewall/logs/registry/paas/warehouse/inference/archive/serverless/devops — the
  warehouse/inference/archive rows added: Redshift/Kusto/BigQuery + ClickHouse,
  SageMaker/Azure-ML/Vertex + Ollama, Glacier/Azure-Cool/GCS-ARCHIVE + MinIO — all
  offline-emittable, no account.)
  The two remaining categories (serverless, devops) need a NEW resource shape (handler
  code + packaging; pipeline + repo relationship) beyond the current model — deferred until
  a consumer needs them (Standing Rule 1).
- **Cloud apply:** the apply LIFECYCLE is LANDED and gated live against LocalStack with no
  account (`--apply --localstack`, see Apply mode above). What REMAINS is real-account
  apply against billed AWS/Azure/GCP: the same `--apply` path with the LocalStack override
  dropped (credentials from the environment); a credentialed milestone, not CI. The cloud
  `endpoints` override is AWS-only today; Azure/GCP local-emulation is a later add.

Tags: v3.291.0 (queue/kv/object) · v3.292.0 (protocol block) · v3.293.0 (compute +
container) · v3.294.0 (deploy runs a protocol on BEAM) · v3.294.1 (contextual keyword
fix) · v3.295.0 (database) · v3.296.0 (secrets) · v3.297.0 (nosql) · v3.298.0 (dns) ·
v3.299.0 (block storage) · v3.300.0 (kv runs on JS) · v3.301.0 (kv cross-backend) ·
v3.302.0 (object + queue cross-backend — full data plane runs) · v3.303.0 (kms) ·
v3.304.0 (file) · v3.305.0 (stream) · v3.306.0 (iam) · v3.307.0 (manifest mode) ·
v3.308.0 (String data plane) · v3.309.0 (k8s) · v3.310.0 (app-level equivalence) ·
v3.311.0 (network) · v3.312.0 (firewall) · v3.313.0 (logs) · v3.314.0 (registry) · … · v3.323.1 (live Valkey docker round-trip) ·
v3.327.0 (LIVE kv data-plane binding, ch444) · v3.327.1 (LIVE queue binding, ch445) ·
v3.327.x (JVM + JS live bindings) · v3.327.4 (WALKTHROUGH.md end-to-end) · v3.328.3
(Rust live binding — all four source backends round-trip a real Valkey).
27 matrix rows; the data plane runs cross-backend AND binds LIVE to a real broker on
Go+JVM+JS; `rune deploy` does infra / workload / manifest modes.
