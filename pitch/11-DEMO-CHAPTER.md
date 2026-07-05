# The demo, in full: a technical-diligence read

Every tool that builds cloud infrastructure today writes configuration, and every
one of them checks the shape of that configuration rather than the behavior of the
system it provisions. Terraform, Pulumi, CDK, and Winglang will tell you that your
HCL parses, that your resource graph is well-formed, that your IAM JSON is valid.
None of them can tell you that the counter you just deployed converges, that it stays
safe under a partition, or that it recovers from a crash, because none of them model
what the system does. A configuration can be perfectly valid and provision a service
that silently loses writes. This chapter is about a system that checks the second
thing: not the shape of the config but the behavior of what runs. That distinction
is worth a paragraph on any day, but it compounds in the AI era, because as models
write more of the world's infrastructure the gap between "this config is well-formed"
and "this system is correct" becomes the gap a human reviewer used to close by hand
and increasingly cannot. What follows is the read for a funder who has already
watched the three-minute video scripted in `01-DEMO.md` and wants to run each moment
themselves. The spine is the same seven steps, in the same order. Each command below
exists in the repository, each transcript is captured from an actual run, and each
claim is anchored to a named test in `go test ./...`. Honesty about the edges is part
of the argument, so every section says plainly what it does not claim.

## Step 1: the two-tab app

**Run it yourself.** Once, off camera, build the WASM and serve the page
(`examples/twotab/RUN.md` has the full recipe):

```
bash bin/setup.sh
cd examples/twotab && node build.mjs && python3 -m http.server 8000
```

Open `http://localhost:8000/` in two browser tabs, side by side. Click Bump twice in
the left tab and once in the right. Both counters settle on 3. Close the right tab
and reopen it, and it re-syncs to 3 on its own. The counter bytes travel over a real
`RTCDataChannel` between the two tabs, so no server sits in the data path. The page
footer reads, verbatim:

```
Machine-checked: the counter state, the merge (convergence proven:
commutative, idempotent, associative), and the wire codec (round-trip
theorem). Trusted glue: WebRTC transport, signaling, and this page's
JavaScript.
```

**What this means.** The reconciliation you just watched is not a hopeful diff-and-
retry loop. The `merge` that combined the two tabs' states is a proven join-
semilattice: its commutativity, idempotence, and associativity are machine-checked
theorems in the source, which is exactly the state-based CRDT convergence criterion,
so the two tabs land on the same value no matter which order their updates arrive.
The pairing and convergence are the assertion in `TestTwoTabDemo`, which drives two
headless pages, bumps twice on one and once on the other, and waits for both DOMs to
read 3; the build and ownership are `TestTwoTabBuild` and `TestTwoTabOwnership`. A
configuration tool cannot make this guarantee because it never represents the merge
at all. It can provision a key-value store and a relay and call the result a shared
counter, but "shared counter" is a name in its graph, not a property it checked. Here
the property is checked before anything ships, and "it loads" is "the proof checks",
because the kernel re-validates every definition on entry.

**What this does not claim.** The footer is deliberate about the boundary. The
counter state, the merge, and the wire codec are machine-checked; the WebRTC
transport, the signaling, and the page's own JavaScript are trusted glue and are
labeled as such. In particular, peer discovery in this demo runs over the browser's
`BroadcastChannel`, which only works between tabs of the same browser and origin.
That is a same-browser shortcut, not a distributed-systems claim. A production
deployment keeps the real `RTCDataChannel` for the counter bytes and swaps
`BroadcastChannel` for a real signaling channel, a websocket relay or a manual SDP
exchange, so the two peers need not share a browser. The codec's round-trip theorem
covers the in-language representation of the counter; the raw foreign byte edge of
that codec is gated at runtime, not proven in the kernel. We say which parts are
proven and which are glue on the page itself, before anyone asks.

## Step 2: one source, five outputs

**Run it yourself.** The source is `examples/twotab/counter.rune` plus the manifest
`examples/wavelet_demo.rune` and the proof catalog
`listings/ch538_control_catalog.rune`. From that one source set, five commands
produce five outputs:

```
rune ledger listings/ch538_control_catalog.rune
rune simulate examples/twotab/counter.rune 2
rune deploy --manifest examples/wavelet_demo.rune --backend aws
(cd examples/twotab && node build.mjs)
rune calm emit --manifest examples/wavelet_demo.rune \
  --listing listings/ch538_control_catalog.rune > demo.calm.json
rune calm validate demo.calm.json \
  --manifest examples/wavelet_demo.rune \
  --listing listings/ch538_control_catalog.rune
```

The `deploy` command emits Terraform for AWS, and by swapping `--backend` for Azure
or GCP the same manifest lowers to those providers. The IAM role in the emitted HCL
carries exactly two actions:

```
resource "aws_iam_role_policy" "relay_role_policy" {
  name   = "relay_role-policy"
  role   = aws_iam_role.relay_role.id
  policy = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Action\":[\"kv:Get\",\"kv:Set\"],\"Resource\":\"*\"}]}"
}
```

The `build.mjs` step reports `built counter.wasm + counter.glue.js`, and the CALM
validation prints exactly `CALM document validates against the source 1:1.`

**What this means.** These are not five artifacts that happen to describe the same
design and could drift apart the moment someone edits one. They are five lowerings of
one content-addressed source: the assurance ledger, a fault simulation, provider HCL,
the running application, and an architecture document that re-validates against the
source. Because the source is the single point of truth and each output is derived
from it, the compliance document and the deployed role cannot say different things
about the same control without one of them failing its gate. The five outputs are
gated one apiece by `TestFiveOutputsLedger`, `TestFiveOutputsSim`,
`TestFiveOutputsTerraform`, `TestFiveOutputsApp`, and `TestFiveOutputsCALM`. A
config-shaped tool can generate several files too, but it has no notion of one
verified source those files must all agree with, so agreement is a convention its
users maintain rather than a property the tool enforces.

**What this does not claim.** The `deploy` step emits HCL; it does not yet apply that
HCL to a billed cloud account. The emitted configuration is real and is format- and
schema-checked (the provider matrix is `fmt -check` gated, and the scoped IAM role is
apply-tested with no account against AWS LocalStack in
`infra/iam_localstack_apply_test.go`), but standing it up against a real billed
account is engineering the funding pays for, described in `04-CLOUD-PLAN.md`. The
CALM document is a genuine re-validation against the source, not a rubber stamp, yet
it is a documentation artifact: it certifies that the architecture description
matches the source one to one, which is a real check, and it is not itself a proof of
the system's behavior. The proofs live in the source; CALM reports them faithfully.

## Step 3: the IAM diff you cannot ship

**Run it yourself.** Open `listings/ch538_control_catalog.rune` near line 136. The
relay's granted capabilities match its needed capabilities exactly:

```
needed  : CodeList is ccons 10 (ccons 11 cnil) end
granted : CodeList is ccons 10 (ccons 11 cnil) end
```

Widen the grant by hand, adding a third capability the code never performs, and re-
run the ledger:

```
granted : CodeList is ccons 10 (ccons 11 (ccons 99 cnil)) end

rune ledger listings/ch538_control_catalog.rune

rune: leastPrivProof: This `refl` does not prove the equation.

  `refl` proves only that a thing equals itself, so both sides of the
  equation must reduce to the SAME normal form - and these do not: the
  left-hand side reduces to `eqCodes granted needed`, while the right-hand
  side reduces to `true`.
exit status 1
```

Revert the edit with `git checkout -- listings/ch538_control_catalog.rune` and the
ledger is green again.

**What this means.** The over-broad policy is not a lint warning you can suppress; it
is a compile error. The least-privilege control is stated as a proposition,
`leastPrivProof : Eq Bool (eqCodes granted needed) true is refl end`, and `refl` type-
checks only when the two sides reduce to the same normal form. When `granted` equals
`needed`, `eqCodes granted needed` computes to `true` and the proof goes through; when
you widen `granted` by even one capability, it computes to `false`, the two sides no
longer match, and the file fails to elaborate. The proof is the enforcement
mechanism, so you physically cannot load a catalog whose relay is granted more than
its code uses. The CI twin is `TestOverBroadIAMRejected`. No structural IAM linter can
do this, because it can check that a policy is well-formed and even flag a wildcard,
but it has no representation of "the access the code actually performs" to compare
the grant against. Here that set is a value in the program, and equality of the two
sets is the thing being proven.

**What this does not claim.** The proof binds the granted policy to the needed policy
at the level of the model: it certifies that the capability set you declare as
granted equals the capability set the workload needs. That the emitted HCL then
carries exactly those two actions is the emitter's job, and it is checked the way
emitted configuration is checked, by format and schema gates and the LocalStack
apply-test, rather than by the kernel end to end. That a real cloud provider enforces
precisely `kv:Get` and `kv:Set` and nothing more is trusted at the provider boundary,
the same auditable boundary every infrastructure tool has. What is new here is that
the model itself cannot be internally over-broad and still compile.

## Step 4: the ledger, proven and honest about what is not

**Run it yourself.** Run `rune ledger listings/ch538_control_catalog.rune`. It prints
one tier per definition. The flagship controls are proven, each with its proposition
hash and proof hash; the one thing not yet proven is a labeled postulate that says so:

```
proven    inRegionProof            prop:95f2d7d932d0  proof:c74fb7833c70  by:brain-fuel
proven    encryptedProof           prop:80f0523479b9  proof:669c4ee0db54  by:brain-fuel
proven    leastPrivProof           prop:4def9046e7f6  proof:2e44dc383f3f  by:brain-fuel
proven    convergesProof           prop:1b8631293cf5  proof:e5b92365fdb6  by:brain-fuel
...
postulate liveInRegion             prop:4fceecca1a69  proof:-  why: live cloud region read not yet modeled (provider API); attested out of band  by:brain-fuel
```

The full table also carries the supporting kit and two `assume` rows for the foreign
`printNat` and `liveRegion` primitives.

**What this means.** The Assurance Ledger's whole point is that the tier is part of
the emitted artifact, not a footnote. Each control's witness carries a tier: `proven`
means a real proof term, `guarded` means a runtime contract with blame, and
`postulate` or `assume` means a bodiless axiom. The identity of a claim is its
proposition hash, so the ledger is honest in a way a prose compliance report cannot
be: it cannot claim `proven` for a control that has no proof term, because the tier is
read off the actual witness in the store. The tiers are gated by
`TestFiveOutputsLedger`. The single postulate, `liveInRegion`, carries its reason
inline, "live cloud region read not yet modeled (provider API); attested out of
band", and it is attributed by author. Because identity is the proposition hash, that
row upgrades to `proven` the moment someone supplies a proof of the same proposition,
with no change anywhere else, and the upgrade is a tracked transition in version
control rather than a silent edit.

**What this does not claim.** The postulate tier exists, and this catalog uses it.
`liveInRegion` is not proven; it is an axiom that says the live region read is not yet
modeled against a provider API, and it is on the page for anyone to see. The two
`assume` rows, `printNat` and `liveRegion`, are foreign primitives taken on faith at
the same auditable edge every language has for its host operations. The ledger does
not hide these behind a green summary. It shows exactly one postulate and exactly the
foreign assumptions, with reasons and attribution, which is the honest version of a
compliance story rather than a marketing one.

## Step 5: schedule luck versus a property

**Run it yourself.** Simulate two counters. First `examples/gcounter.rune`, the
minimal proven G-Counter whose `merge` is verbatim-identical to the app's:

```
rune simulate examples/gcounter.rune 2

merge laws (CvRDT join):
  commutative: ok
  idempotent:  ok
  associative: ok
  inflationary:ok  (updates only grow the state)
verdict: CONVERGED to succ (succ (succ zero)) on all 2 replicas (and the join
laws hold, so under any schedule).
```

Then a Last-Writer-Wins counter that happens to land on a value this run:

```
rune simulate examples/lww.rune 2

merge laws (CvRDT join):
  commutative: FAIL
  idempotent:  ok
  associative: ok
  inflationary:ok  (updates only grow the state)
  - not commutative: merge s0 s1 = lw (succ (succ zero)), but merge s1 s0 = lw zero
verdict: NOT GUARANTEED to converge - merge is not a join (see the failed law
above). This run ended [succ zero succ (succ zero)], but that is schedule luck,
not a property.
```

**What this means.** The simulator does not merely run a schedule and report the
final state; it inspects the algebra of the merge. That is why it can distinguish a
property that must hold from a run that happened to pass. The two files look almost
identical, and a single execution of either could end on a converged value. The
G-Counter is `CONVERGED` under any schedule because its merge is a proven join, and
the Last-Writer-Wins merge fails the commutativity law with a concrete
counterexample, `merge s0 s1 = lw (succ (succ zero))` against `merge s1 s0 = lw zero`,
so its convergence this run is, in the linter's own words, schedule luck and not a
property. This pass and fail are `TestFiveOutputsSim`. Terraform cannot draw this
line because it never models what a merge does; a test that passes once and a
property that holds always are the same green checkmark to a structural tool, and
they are opposite things here.

**What this does not claim.** The law linter samples: it evaluates the merge on a
finite set of states drawn from the run and reports the laws it can check on those
samples, which is what "NOT a CvRDT on the samples" means in the raw output. That
finite check is what flags the Last-Writer-Wins counter, and it is genuinely useful,
but it is not the source of the universal guarantee. The universal guarantee for the
G-Counter comes from the machine-checked proofs in the source
(`mergeComm`, `mergeIdem`, `mergeAssoc`), which hold for all states, not just the
sampled ones. The simulator is the fast, falsifying front line; the proof is the thing
that makes convergence hold under every schedule.

## Step 6: the hash binding

**Run it yourself.** Query the same name in two files, the source the browser
deploys and the proof catalog:

```
rune hash examples/twotab/counter.rune | grep ' merge$'
d5c7bd7e530c7d66115a7620d8a2c6d62633598f1ec5fc233b2e86fcbff01edf  merge

rune hash listings/ch538_control_catalog.rune | grep ' merge$'
d5c7bd7e530c7d66115a7620d8a2c6d62633598f1ec5fc233b2e86fcbff01edf  merge
```

The same hash, character for character.

**What this means.** The `merge` whose convergence the ledger and the CALM document
certify is literally the same value, byte for byte, as the `merge` compiled into the
WASM the two tabs run. A definition's identity in this system is the Merkle hash of
its elaborated core, computed structurally, so two definitions carry the same hash
only when they are the same term. The compliance artifact and the running code are
therefore not "the same in spirit" or "kept in sync by process"; they are one value.
That identity is what `TestFiveOutputsHashBinding` pins. This is the guarantee that a
config-shaped pipeline structurally cannot offer: it can name a control in a document
and name a resource in a deployment, but it has no content identity tying the claim to
the artifact, so the document and the deployment can drift and the tool will not
notice. Content addressing makes the drift impossible to hide, because a changed merge
is a changed hash.

**What this does not claim.** The hash binds the in-language `merge` term across the
proof catalog and the deployed source, and that is exactly what content addressing
buys. It does not extend a kernel proof over the raw foreign byte encoding at the wire
edge. The wire codec has a round-trip theorem for the counter's in-language
representation, but the codec's foreign `Bin` edge, where in-language values meet raw
host bytes, is gated at runtime rather than proven in the kernel. The identity claim
is about the verified value; the last inch to the wire is a runtime-checked boundary,
and we mark it as such rather than let the hash imply more than it proves.

## Step 7: the close

**Run it yourself.** Run `go test ./...` and watch it go green. Every moment in this
chapter is a named gate in that suite: the two-tab application is `TestTwoTabDemo`;
the five outputs are `TestFiveOutputsLedger`, `TestFiveOutputsSim`,
`TestFiveOutputsTerraform`, `TestFiveOutputsApp`, and `TestFiveOutputsCALM`; the IAM
sabotage is `TestOverBroadIAMRejected`; and the hash binding is
`TestFiveOutputsHashBinding`. The proof is not a slide, it is the build. A funder can
clone the repository, run the suite, and reproduce every arrow above, then break any
one of them and watch the corresponding gate fail. That reproducibility is the
credibility, and it is the thing a deck can never buy.

Three edges are worth restating in plain prose, because saying them before anyone
asks is what makes the rest believable. First, the `deploy` step emits Terraform and
OpenTofu HCL and does not yet apply that HCL to a billed cloud account. The emitted
configuration is real and is format- and schema-checked, and the scoped IAM role is
apply-tested with no account on AWS LocalStack, but running it against a real billed
account is precisely the engineering the funding pays for. Second, the standard
library is deep rather than broad: the proven core is genuine and the linear-algebra,
CRDT, and OTP tiers are real, but the library is not yet at production breadth, and we
do not claim coverage we cannot name a listing for. Third, this is one builder's work.
The robustness of the system is real and the tests back it, but the team is not yet,
and we say so plainly. Those three sentences are not caveats bolted onto a pitch; they
are the same honesty the footer, the ledger, and the simulator's linter practice
throughout. The claim is narrow and it is checkable: this system checks the behavior
of what it deploys, not the shape of its configuration, and every strong word in the
paragraphs above is backed by a named gate or a transcript you can reproduce from a
clone.
