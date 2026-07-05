# The demo

The demo is the whole pitch. A technical funder believes what runs in front of them,
not what a deck claims. Everything below is real: every command exists in the repo,
every transcript is captured from an actual run, and every arrow is a passing test in
`go test ./...`. The named gates are cited inline so a skeptic can check each moment
against the suite. Source of truth for the longer walk: `examples/WALKTHROUGH.md`.

The spine is seven shots, in order: the two-tab app, the five outputs, the IAM
sabotage, the ledger, the schedule-luck contrast, the hash binding, and the close.
Honesty about the edges is written into the script, not left for the Q&A.

## The 3-minute version (record this as a video)

Prep once, off-camera: `bash bin/setup.sh` (installs wabt + puppeteer under
`harness/node_modules`), then from `examples/twotab/` run `node build.mjs` and
`python3 -m http.server 8000`. Run every `rune ...` command from the repo root.

### Shot 1 - the cold open (the two-tab app)

On screen: two browser tabs open to `http://localhost:8000/`, side by side. Click
Bump twice in the left tab and once in the right. Both counters settle on 3. Close the
right tab, reopen it: it re-syncs to 3 on its own.

The page footer reads, verbatim:

```
Machine-checked: the counter state, the merge (convergence proven:
commutative, idempotent, associative), and the wire codec (round-trip
theorem). Trusted glue: WebRTC transport, signaling, and this page's
JavaScript.
```

The counter bytes travel over a real `RTCDataChannel` between the two tabs; no server
sits in the data path. The two tabs pairing and converging on a bump is the assertion
in `TestTwoTabDemo` (it drives two headless pages, bumps twice on one and once on the
other, and waits for both DOMs to read 3). The build and ownership checks are
`TestTwoTabBuild` / `TestTwoTabOwnership`.

SAY: "Two browser tabs, one counter, no server in the data path. The merge that just
reconciled them is a machine-checked theorem. The footer tells you exactly which parts
are proven and which parts are trusted glue. Honesty is part of the product."

### Shot 2 - one source, five outputs

On screen: the source is `examples/twotab/counter.rune` plus the manifest
`examples/wavelet_demo.rune` and the proof catalog
`listings/ch538_control_catalog.rune`. From that one source set, five commands produce
five outputs. Run them in sequence:

```
rune ledger listings/ch538_control_catalog.rune      # 1. assurance ledger  (Shot 4)
rune simulate examples/twotab/counter.rune 2         # 2. fault simulation
rune deploy --manifest examples/wavelet_demo.rune --backend aws   # 3. cloud Terraform
node build.mjs   # (in examples/twotab/)              # 4. the app you just watched
rune calm emit --manifest examples/wavelet_demo.rune \
  --listing listings/ch538_control_catalog.rune > demo.calm.json  # 5a. CALM doc
rune calm validate demo.calm.json \
  --manifest examples/wavelet_demo.rune \
  --listing listings/ch538_control_catalog.rune       # 5b. re-validate 1:1
```

The `deploy` command emits Terraform for AWS (and, by swapping `--backend`, Azure and
GCP). The IAM role in the emitted HCL carries exactly two actions:

```
resource "aws_iam_role_policy" "relay_role_policy" {
  name   = "relay_role-policy"
  role   = aws_iam_role.relay_role.id
  policy = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"kv:Get\",\"kv:Set\"],\"Resource\":\"*\"}]}"
}
```

The `build.mjs` step reports:

```
built counter.wasm + counter.glue.js
```

And the CALM validation prints exactly:

```
CALM document validates against the source 1:1.
```

The five outputs are gated one apiece: `TestFiveOutputsLedger`, `TestFiveOutputsSim`,
`TestFiveOutputsTerraform`, `TestFiveOutputsApp`, `TestFiveOutputsCALM`.

SAY: "One source. Five outputs: the assurance ledger, a fault simulation, Terraform
for three clouds, the app you just watched, and a CALM architecture doc that
re-validates against the source one to one."

### Shot 3 - the IAM diff (you cannot ship the over-broad role)

On screen: open `listings/ch538_control_catalog.rune` at line 137. The relay's granted
capabilities match its needed capabilities exactly:

```
needed  : CodeList is ccons 10 (ccons 11 cnil) end
granted : CodeList is ccons 10 (ccons 11 cnil) end
```

Widen the grant by hand, adding a third capability the code never performs:

```
granted : CodeList is ccons 10 (ccons 11 (ccons 99 cnil)) end
```

Re-run the ledger. The file no longer loads:

```
rune ledger listings/ch538_control_catalog.rune

rune: leastPrivProof: This `refl` does not prove the equation.

  ... the left-hand side reduces to `eqCodes granted needed`, while the
  right-hand side reduces to `true`. (full Socratic diagnostic elided)
exit status 1
```

Revert the edit (`git checkout -- listings/ch538_control_catalog.rune`) and the ledger
is green again. The over-broad policy is a compile error, not a lint warning: the
least-privilege proof is a `refl` that only type-checks when granted equals needed.
The CI twin is `TestOverBroadIAMRejected`.

SAY: "The role ships with exactly the two capabilities the code performs. Watch what
happens when I widen it. The file no longer compiles. You physically cannot ship the
over-broad role."

### Shot 4 - the ledger (proven, and honest about what is not)

On screen: `rune ledger listings/ch538_control_catalog.rune`. It prints a tier per
definition. The four flagship controls are proven, each with its proposition hash and
proof hash; the one thing not yet proven is a labelled postulate that says so:

```
proven    inRegionProof            prop:95f2d7d932d0  proof:c74fb7833c70  by:brain-fuel
proven    encryptedProof           prop:80f0523479b9  proof:669c4ee0db54  by:brain-fuel
proven    leastPrivProof           prop:4def9046e7f6  proof:2e44dc383f3f  by:brain-fuel
proven    convergesProof           prop:1b8631293cf5  proof:e5b92365fdb6  by:brain-fuel
...
postulate liveInRegion             prop:4fceecca1a69  proof:-  why: live cloud region read not yet modeled (provider API); attested out of band  by:brain-fuel
```

(The full table also lists the supporting kit and two `assume` rows for the foreign
`printNat` and `liveRegion` primitives; elided here.) The `liveInRegion` row is the
only postulate: it carries the reason inline and is attributed by author, and it
upgrades to `proven` the moment a proof of the same proposition hash is supplied.

SAY: "Four controls proven. One postulate, and it says so, with the reason and the
upgrade path. The tier is on the page, not in a footnote."

### Shot 5 - schedule luck versus a property

On screen: two simulate runs, side by side. First `examples/gcounter.rune`, the
minimal proven G-Counter, the shape-identical pair to the app's
`examples/twotab/counter.rune`: their `merge` definitions are verbatim-identical, so
under content addressing they carry the same hash (the one Shot 6 shows).

```
rune simulate examples/gcounter.rune 2

step 0 [r0:op0 r1:op1] => r0=succ zero r1=succ zero   [converged]
step 1 [r0:op0] DROP{r0<-r1,r1<-r0} => r0=succ (succ zero) r1=succ zero   [diverged]
step 2 gossip => r0=succ (succ (succ zero)) r1=succ (succ (succ zero))   [converged]

merge laws (CvRDT join):
  commutative: ok
  idempotent:  ok
  associative: ok
  inflationary:ok  (updates only grow the state)
verdict: a CvRDT (join merge + inflationary updates) - will converge

verdict: CONVERGED to succ (succ (succ zero)) on all 2 replicas (and the join
laws hold, so under any schedule).
```

Then a Last-Writer-Wins counter that happens to land on a value this run:

```
rune simulate examples/lww.rune 2

... (steps elided) ...
merge laws (CvRDT join):
  commutative: FAIL
  idempotent:  ok
  associative: ok
  inflationary:ok  (updates only grow the state)
  - not commutative: merge s0 s1 = lw (succ (succ zero)), but merge s1 s0 = lw zero
  ... (further violations elided)
verdict: NOT a CvRDT on the samples - convergence is not guaranteed

verdict: NOT GUARANTEED to converge - merge is not a join (see the failed law
above). This run ended [succ zero succ (succ zero)], but that is schedule luck,
not a property.
```

The pass/fail here is `TestFiveOutputsSim`. The simulator does not just run a schedule;
it checks the merge's algebra, so it can tell a proven property from a lucky run.

SAY: "These two files look almost identical. One is proven to converge under any
schedule. The other passed this run by luck, and the linter says so."

### Shot 6 - the hash binding

On screen: two hash queries, one against the source the browser deploys, one against
the proof catalog:

```
rune hash examples/twotab/counter.rune | grep ' merge$'
d5c7bd7e530c7d66115a7620d8a2c6d62633598f1ec5fc233b2e86fcbff01edf  merge

rune hash listings/ch538_control_catalog.rune | grep ' merge$'
d5c7bd7e530c7d66115a7620d8a2c6d62633598f1ec5fc233b2e86fcbff01edf  merge
```

The same hash. The `merge` whose convergence the ledger and CALM doc certify is
literally the same value, byte for byte, as the `merge` compiled into the WASM the two
tabs run. That identity is what `TestFiveOutputsHashBinding` pins.

SAY: "Same hash. The control in the compliance doc and the merge running in your
browser are one value. That is what content addressing buys."

### Shot 7 - the close

On screen: `go test ./...` scrolling green.

SAY: "Every arrow you just saw is a passing test. Clone it and run go test."

## The 10-minute version (the in-room demo)

Same spine, slower, stopping on the moments that land. The extra beats:

1. **Open `examples/gcounter.rune` and `examples/lww.rune` side by side.** They look
   almost identical. Run `rune simulate` on both (Shot 5). The G-Counter is CONVERGED
   under any schedule because its merge is a proven join; the Last-Writer-Wins counter
   passes this one run by luck and the law linter flags `commutative: FAIL` with the
   concrete counterexample. This is the most important moment: it is the difference
   between a test that happened to pass and a property that must hold. Terraform cannot
   make this distinction, because it never models what the merge does.

2. **The IAM sabotage, live and reverted.** Do the Shot 3 edit on camera: widen
   `granted` at line 137, re-run `rune ledger`, and read the real load failure off the
   screen ("This `refl` does not prove the equation ... the left-hand side reduces to
   `eqCodes granted needed`, while the right-hand side reduces to `true`"). Then
   `git checkout -- listings/ch538_control_catalog.rune` and re-run to show it green.
   "The proof failing is a compile error. You physically cannot ship a role broader
   than the code uses." The CI twin is `TestOverBroadIAMRejected`.

3. **The same manifest hits three clouds.** Emit and diff:

   ```
   rune deploy --manifest examples/wavelet_demo.rune --backend aws
   rune deploy --manifest examples/wavelet_demo.rune --backend gcp
   ```

   The concrete resources differ by provider, the logical set is identical. The kv
   store is `aws_elasticache_cluster` (Redis) on AWS and `google_redis_instance` on
   GCP; the compute node is `aws_instance` versus `google_compute_instance`, both
   running `podman run -d docker.io/library/erlang:slim`; the web node is
   `aws_elastic_beanstalk_application` versus `google_app_engine_application`. The IAM
   is `aws_iam_role_policy` with `["kv:Get","kv:Set"]` versus a
   `google_project_iam_custom_role` with `permissions = ["kv:Get", "kv:Set"]`. Same
   four nodes, same two IAM actions, provider-specific lowering. "You write the system
   once. The provider is a backend, exactly like a compiler target." The gate is
   `TestFiveOutputsTerraform`.

4. **The ledger walk, postulate read aloud.** Run `rune ledger` on the catalog and
   read the four proven flagship controls, then read the postulate row out loud in
   full: `liveInRegion ... why: live cloud region read not yet modeled (provider API);
   attested out of band`. "This is the one thing we have not proven. It is a postulate,
   it carries its reason, and the moment we model the live region read a proof of the
   same proposition hash upgrades it to proven, with no change anywhere else." The
   tiers are gated by `TestFiveOutputsLedger`.

5. **The hash binding, spelled out.** Do the Shot 6 grep pair and let the audience read
   the two hashes character by character. Then re-emit and re-validate the CALM doc
   (`rune calm emit ... | rune calm validate ...`, "CALM document validates against the
   source 1:1") so they see the compliance artifact and the running app are anchored to
   the same content hash. Gates: `TestFiveOutputsHashBinding` and `TestFiveOutputsCALM`.

## Why this demo is unfalsifiable in the way that matters

Offer to run `go test ./...` live. Every moment above is a named gate in that suite:
the two-tab app is `TestTwoTabDemo`; the five outputs are `TestFiveOutputsLedger`,
`TestFiveOutputsSim`, `TestFiveOutputsTerraform`, `TestFiveOutputsApp`, and
`TestFiveOutputsCALM`; the IAM sabotage is `TestOverBroadIAMRejected`; the hash binding
is `TestFiveOutputsHashBinding`. The proof is not a slide; it is the build. A funder
can clone the repo and reproduce every arrow. That reproducibility is the credibility,
and it is the thing a deck can never buy.

## What NOT to claim in the demo

Honesty is a feature here, so be explicit about the edges before anyone asks:

- **The deploy step emits Terraform/OpenTofu HCL; it does not yet apply to a billed
  cloud account.** The emitted HCL is real and is format- and schema-checked (the
  matrix is `fmt -check` gated, and the scoped IAM role is apply-tested no-account on
  AWS LocalStack, `infra/iam_localstack_apply_test.go`). Running that HCL against a
  real billed account is exactly what the funding pays for (see `04-CLOUD-PLAN.md`).

- **The standard library is deep, not broad.** The proven core is genuine and the LA /
  CRDT / OTP tiers are real, but the library is not yet at production breadth. Do not
  claim coverage you cannot name a listing for.

- **This is one builder's work.** The robustness of the system is real and the tests
  back it; the team is not yet. Say so plainly.

Saying these out loud, before anyone asks, is what makes the rest believable.
