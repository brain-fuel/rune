# After year 3: what happens when the money runs out

The question every serious funder asks, and the one the rest of this package did not yet
answer: am I signing up to fund this forever? The answer is no, by design, and a funder of
Elon's caliber will respect a plan that is built to graduate rather than to drip. The core
idea: the three years is the **build**, and a build ends. What follows is **maintenance**,
which costs a fraction of the build, and the plan reaches self-sustainability before the
build money runs out.

## The cost cliff: build is expensive, maintain is cheap

A finished, frozen-kernel language with a proven core and a deployed pipeline does not need a
build team to keep running. The proofs do not rot (they are content-addressed and re-checked
mechanically), the kernel is already frozen, and the backends are done. Steady-state cost is
maintenance, security response, dependency upkeep, and ecosystem stewardship.

| Phase | Annual cost | What it pays for |
|-------|-------------|------------------|
| Build (years 1 to 3) | ~$1.0M to $1.2M | The full team, cloud apply, AI workflow, stdlib breadth, adoption |
| Maintain (year 4 on) | ~$300k to $450k | 1 to 2 core maintainers, CI/cloud, foundation ops and legal |

So the recurring obligation drops to roughly a quarter of the build. The whole sustainability
problem is "how do we cover $300k to $450k per year after year 3," and there are three
independent answers, pursued together.

## The three pillars of steady-state funding

### 1. An endowment (the "fund it once, never ask again" option)
Seed an endowment whose returns cover steady-state maintenance in perpetuity. At a
conservative ~4.5% real draw, an endowment of roughly $7M to $10M throws off the $300k to
$450k a year that maintenance needs, forever, with no further asks. This is the cleanest
answer for a funder who wants a one-time, bounded commitment, and it is the option to put in
front of Elon explicitly: a single bet that both finishes the work (the build budget) and
guarantees it survives (the endowment), with no recurring obligation. A partial endowment
($3M to $5M) covers half and pairs with the pillars below.

### 2. Foundation membership (adoption funds itself)
This is how Rust, Python, LLVM, and the Linux Foundation projects sustain themselves: the
companies that depend on the infrastructure pay to keep it healthy, because it is far cheaper
than maintaining a fork. If Wootz is adopted (the year-3 adoption push exists precisely to
make this true), corporate and institutional members fund the foundation through dues. The
verified-infrastructure angle makes this especially natural: the organizations that most need
provable correctness (finance, infrastructure, the AI labs themselves) are exactly the ones
with budget to sustain it. Membership scales with adoption, which is the right incentive.

### 3. A commercial subsidiary that funds the nonprofit (the Mozilla model)
The nonprofit owns a wholly-owned commercial arm that sells the things organizations will pay
for around an open core: support contracts, verified-component libraries, training and
certification, and hosted verification-and-deploy tooling. The profits flow up to fund the
foundation's mission, while the substrate stays open and free. This is the Mozilla Foundation
and Mozilla Corporation structure, and it converts adoption into durable funding without
making the core rent-seeking.

## The explicit endgame states (name all of them, including failure)

A funder trusts a plan that knows every way it can end:

- **Self-sustaining foundation (the target).** By end of year 3, membership plus a partial
  endowment plus the commercial arm cover steady state. No further philanthropic ask. The
  substrate is permanent, open, and governed by an independent board.
- **Hosted by a larger foundation.** If standalone governance is more overhead than warranted,
  the project moves under an existing umbrella (the Linux Foundation, the CNCF, or a
  verification-focused host), which provides governance and a membership base at near-zero
  marginal cost. A common, healthy outcome for infrastructure projects.
- **Adopted as core infrastructure by a major.** If an AI lab or a cloud vendor makes Wootz
  part of their stack, they sustain it as their own infrastructure. The open license keeps it
  a public good even then.
- **The honest floor: a dormant but permanent public good.** If adoption never reaches
  self-sustaining scale, the substrate does not disappear. The kernel is frozen, the proofs
  are content-addressed and re-checkable forever, the design is documented, and maintenance
  shrinks to one part-time steward or a community. The worst case is not "it dies," it is "it
  sits finished and free until someone needs it," which is exactly what a public good should
  do. The build money still bought a permanent, open, verified-computing substrate that exists
  whether or not it is popular.

## What "done" means at the end of year 3

The build is complete when: all four teloi are finished (the two remaining have only their
non-research tails left), the stdlib is at production breadth, real cloud apply works across
three clouds, the AI authoring-and-proof-gate workflow ships with a published eval, the
substrate is documented and packaged for adoption, and the sustainability pillars above are in
place. At that point the founder's role shifts from builder to steward, the maintenance team
is small, and the project no longer depends on any single funder.

## The line to say when asked "then what?"

"The three years is the build, and a build ends. After that the cost drops to roughly a
quarter, just maintenance, and three things cover it: a partial endowment so the core is
guaranteed, foundation membership from the companies that depend on it, and a commercial
support arm that funds the mission while the substrate stays open. The target is
self-sustaining by year three with no further ask. And the floor is that even if adoption is
slow, the proofs do not rot: you will have permanently funded an open verified-computing
substrate that exists forever, whether or not it ever needs another dollar. You are buying a
finished public good, not a subscription."
