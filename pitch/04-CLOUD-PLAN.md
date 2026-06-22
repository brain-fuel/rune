# Finishing with real cloud accounts

Today the deploy pipeline emits Terraform/OpenTofu HCL for AWS, Azure, and GCP, and runs the
15 FOSS backends end to end locally under Docker/Podman. The honest gap is that the cloud
HCL is generated and format-checked, not yet applied against billed accounts. Closing that
gap is the single most credibility-moving deliverable the funding buys, because it turns
"deploys, in principle" into "deployed, here is the running URL."

## The three things real accounts unlock

1. **Live apply, not just emit.** Run `tofu apply` against real AWS/Azure/GCP from the
   generated graph, stand the resources up, confirm the app runs, then `destroy`. The
   equivalence gate ("equal config to equivalent deployment") becomes an observed fact across
   three clouds, not a structural assertion.
2. **The dependency-heavy matrix tail.** Several matrix rows (managed Kubernetes, streaming,
   some networking) cannot be fully exercised without a cloud account and the associated
   quotas. Real accounts let the remaining rows reach the same "applied and verified" bar as
   the local FOSS rows.
3. **The live distributed story on real managed services.** The CRDT and supervision actors
   run on the BEAM today; with accounts they run across real managed queues and caches
   (SQS/Service Bus/Pub-Sub, ElastiCache/Memorystore), proving the verified protocol holds on
   production infrastructure, not just a local broker.

## Already done, for free: the apply path exists and is gated live

Before any account is bought, the `--apply` lifecycle is BUILT and CI-gated with no cloud
spend. `rune deploy --apply` stands the artifact up: a FOSS backend on docker compose, or a
cloud backend through terraform. `--localstack` redirects the AWS provider at a local fake
cloud (a generated terraform override file), so the UNCHANGED emitter HCL applies against a
cloud-API-compatible target with no account and no bill, and `--destroy` tears it down
after. This is proven by passing tests: one stands a real Valkey up and PINGs it; another
applies an S3 bucket from the AWS HCL against LocalStack, verifies the bucket really exists,
then destroys it. So the engineering risk of the apply path is already retired. What real
accounts buy is not capability, it is validation against the genuine cloud APIs and scale.

## The plan, in phases

### Phase A: one cloud, one app, applied and torn down (weeks 1 to 6)
- Stand up isolated AWS, Azure, and GCP organizations with hard budget caps and
  auto-teardown (every test run applies and destroys; nothing standing overnight without
  intent).
- Take `examples/app.wav` (the 9-resource web app) to a live `apply` on AWS first. Confirm
  the app serves traffic. Capture the run as the next demo artifact (a real URL on three
  clouds is the upgrade to the 90-second video).
- Promote the existing `--apply` path from LocalStack/FOSS to real accounts: drop the
  LocalStack override (credentials from the environment), add state management and safe
  rollback. The lifecycle is built; this points it at billed endpoints.

### Phase B: all three clouds, equivalence observed (weeks 4 to 12)
- Bring Azure and GCP to the same applied bar. Assert the equivalence gate against live
  deployments: the same `app.wav` produces an equivalent running system on each.
- A nightly CI job that applies the matrix to ephemeral cloud resources and destroys them,
  so "it deploys to real clouds" is a continuously verified claim, not a one-time demo.

### Phase C: the matrix tail and the verified distributed app on managed services (weeks 10 to 24)
- Close the dependency-heavy rows (k8s, streaming, networking) against real accounts.
- Run the verified replicated counter and a small consensus core across real managed
  queues/caches on each cloud, with fault injection (kill a node, partition a subnet) and
  confirm the proven recovery holds in the live environment.
- Deliverable: a reference verified distributed application, proven correct and deployed and
  observed-recovering on three real clouds.

## Cost shape (detail in `07-BUDGET.md`)

Cloud spend for this work is mostly ephemeral: apply, observe, destroy. The standing cost is
a few small always-on test fixtures and CI runners. The model is "lots of short-lived
resources, almost nothing left running," which keeps the burn modest and predictable under
hard caps. Budgeted at roughly $5k per month blended, with headroom, plus one-time setup.

## Why this is low-risk engineering, not research

Nothing in this plan is a research question. The hard, uncertain work (the type theory, the
proof-carrying codegen, the process calculus, the kernel) is finished and frozen. Applying
generated, format-valid HCL to a cloud account is well-trodden engineering that thousands of
teams do daily; the only reason it is not done yet is that one builder without cloud budget
prioritized the verification core first, correctly. This phase is buying the accounts and the
engineering time to do a known thing, and the payoff is the demo going from "watch it
generate" to "watch it run, on three clouds, recovering from a real failure."
