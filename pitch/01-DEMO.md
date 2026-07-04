# The demo

The demo is the whole pitch. A technical funder believes what runs in front of them, not
what a deck claims. The sequence below is real: every command exists in the repo, and every
arrow is a test in `go test ./...`. Source of truth: `examples/WALKTHROUGH.md`.

## The 90-second version (record this as a video)

One file. Five transformations. No other tool does all five, and none does the first one
at all.

```sh
# 0. The source: one verified G-Counter CRDT, examples/gcounter.rune
#    Its merge is proven a join-semilattice, so it converges under ANY gossip schedule.

# 1. PROVE: loading the file IS checking the proof. The kernel re-validates every
#           definition on entry. "It elaborates" means "the math is machine-checked."
rune emit examples/gcounter.rune converged

# 2. SIMULATE: drive the protocol's OWN verified ops under a fault policy.
#              Partition the replicas, watch them diverge, watch them reconverge.
rune simulate examples/gcounter.rune 2
#   => step 2 gossip: r0=3 r1=3  [converged]   verdict: CONVERGED
#   The law linter flags lww.rune / badcounter.rune as "schedule luck, not a property",
#   a catch no YAML or HCL tool can make, because they never model behavior.

# 3. DEPLOY: one agnostic manifest, an equivalent deployment on every cloud.
rune deploy --manifest examples/app.rune --backend aws  > app.aws.tf
rune deploy --manifest examples/app.rune --backend gcp  > app.gcp.tf
#   app.rune is a 9-resource web app (compute/queue/kv/db/secret/iam/object/lb/cdn).
#   The 22-row matrix lowers each kind to AWS/Azure/GCP HCL; "equal config => equivalent
#   deployment" is the gate, mirroring backend conformance.

# 4. RUN: the proven protocol's actors execute live, cross-backend.
rune run examples/gcounter.rune converged --target go    # => 3
rune run examples/gcounter.rune converged --target erl   # => 3 on the BEAM (real OTP)
#   crash / detect / restart faults included; Go/JVM/JS scheduler shims are
#   observationally identical to the BEAM.

# 5. LIVE: the data plane talks to a real broker over RESP, no third-party dependency.
rune deploy --resource kv --name cache --backend valkey --out ./kv
(cd kv && docker compose up -d)
WAVELET_KV_URL=redis://localhost:6379 rune run listings/ch444_live_kv.rune main --target go
#   => world   (kvSetLive / kvGetLive read and write a REAL Valkey)
```

**The line to say over the video:** "That is one verified source. It was proven correct,
simulated under a network partition, deployed to three different clouds, run live on the
Erlang VM, and connected to a real database. Every step is a passing test. No other
infrastructure tool proves the system is correct before it ships, because no other tool can
even talk about what the system does. They check the shape of the config. We prove the
behavior."

## The 10-minute version (the in-room demo)

Same spine, but stop and show the moments that land:

1. **Open `gcounter.rune` and `lww.rune` side by side.** They look almost identical. Run
   `rune simulate` on both. The G-Counter is CONVERGED under any schedule because its merge
   is a proven join. The Last-Writer-Wins counter passes by luck on one schedule and the
   linter flags it: "schedule luck, not a property." This is the single most important
   moment. It shows the difference between a test that happened to pass and a proof that
   must hold. Terraform cannot make this distinction. Winglang's simulator cannot make this
   distinction. Only a tool that models the protocol's behavior and checks its algebra can.

2. **Break the proof on purpose.** Edit the merge to a non-commutative operation. The file
   fails to load: the kernel rejects it. "The proof failing is a compile error. You
   physically cannot deploy a counter that does not converge." Then revert.

3. **Show the same source hit three clouds.** Diff `app.aws.tf` against `app.gcp.tf`. The
   concrete resources differ (SQS vs Pub/Sub, ElastiCache vs Memorystore), the logical set
   is identical. "You write the system once. The provider is a backend, exactly like a
   compiler target."

4. **Run it on the BEAM, then crash a replica.** The supervisor detects the DOWN and
   restarts. "The fault tolerance is not configuration. It is the projection of a proven
   supervision protocol. The thing that recovers is the thing we proved recovers."

5. **The furnace.** Show a foreign numpy call wrapped in a contract that checks its result
   against an in-language reference and blames the library if it drifts. "We do not trust
   foreign code blind. We test it into submission, then we can prove the parts that matter.
   This is the on-ramp: a newcomer starts by property-testing, and the same path leads to
   proof."

## Why this demo is unfalsifiable in the way that matters

Offer to run `go test ./...` live. Every claim in the demo is a gate in that suite. The
proof is not a slide; it is the build. A funder can clone the repo and reproduce every
arrow. That reproducibility is the credibility, and it is the thing a deck can never buy.

## What NOT to claim in the demo

Honesty is a feature here, so be explicit about the edges:

- The deploy step emits Terraform/OpenTofu HCL and FOSS Compose specs. **Applying that to a
  live cloud account is exactly what the funding pays for** (see `04-CLOUD-PLAN.md`). Today
  the FOSS path runs end to end locally under Docker/Podman; the cloud path is generated and
  format-checked, not yet applied against billed accounts.
- The standard library has a deep proven core but is not yet at production breadth.
- This is one builder's work. The robustness of the system is real; the team is not yet.

Saying these out loud, before anyone asks, is what makes the rest believable.
