# The pitch: what to present and what to say

Built on Marketing-Is-An-Argument and the Five Lightbulbs: a prospect commits when they
believe five things in order. They have a real problem. A better future is possible. Your
mechanism is why it is possible. They believe you specifically. They believe it is
achievable. The deck and the talk below walk those five in that order, argued with dignity,
no manufactured urgency.

## The one-liner (lead with this every time)

"Wootz is the first system where the correctness of a distributed system is a machine-checked
theorem, and the same verified source is deployed to any cloud. As AI starts writing the
world's infrastructure, it is the trust boundary that checks what the AI builds."

## The deck (10 slides, demo-first)

**Slide 0, the demo, not a slide.** Open by running it, or playing the 90-second video. Do
not introduce yourself first. Show one file get proven, simulated under a partition, deployed
to three clouds, run on the BEAM, connected live. Then say the one-liner. You have now earned
the next eight minutes.

**Slide 1, the problem (Lightbulb 1).** Civilization runs on distributed software, and AI is
about to write most of it. Today's tools (Terraform, Pulumi, CDK, Winglang) generate
configuration. None can prove the system converges, stays safe under a partition, or recovers
from a crash. They check structure; they cannot see behavior. A valid config can provision a
counter that silently loses writes. As AI authors more of this, the unverifiable gap
compounds into a reliability and safety problem.

**Slide 2, the better future (Lightbulb 2).** A substrate where correctness is checked, not
hoped. The system is proven before it ships, simulated under failure, and deployed from the
exact source that was proven. AI proposes; the kernel checks; only the provably correct
deploys. Verified-by-construction infrastructure for the AI era.

**Slide 3, the mechanism, part one: proof is a compile error (Lightbulb 3).** Wootz is
dependently typed. A correctness property is a type. Violating it does not fail a test later;
it fails to compile now. Show the `lww.rune` vs `gcounter.rune` moment: one is proven to
converge under every schedule, the other passes by luck and is rejected by the law linter.
This is the distinction no structural tool can make.

**Slide 4, the mechanism, part two: one proven source, eight backends, any cloud.**
Proof-carrying codegen lowers one erased intermediate form to eight backends; the deploy side
lowers one agnostic graph to AWS, Azure, GCP, and 15 self-hosted FOSS backends. The provider
is a compiler target. The protocol's proof rides all the way to the running actors.

**Slide 5, the mechanism, part three: the furnace.** What cannot be proven is property-tested
with blame. Foreign code is tested into submission, not trusted, and the path from "tested"
to "proven" is continuous. This is the on-ramp that makes the system teachable and adoptable,
and the discipline that makes AI-proposed foreign calls measurable instead of trusted.

**Slide 6, the AI workflow (the reason it matters now).** The agent authors Wootz; the kernel
is the proof gate; the counterexample feeds back; the proven system deploys. Every place a
human would have to carefully review AI-generated infrastructure, the kernel reviews it
instead, completely, every time. This is concrete, shippable AI safety: not making the model
good, making its output checkable.

**Slide 7, credibility (Lightbulb 4).** What is done, honestly. Two of four foundational
pillars finished: computational univalence and proof-carrying codegen. The other two have
their proven core landed: the verified stdlib and the distributed algebra. 460+ machine
checked proofs. Eight backends. A working prove-to-simulate-to-deploy-to-run-to-live
pipeline. Every claim gated by `go test ./...`, reproducible from a clone. Built by one
person, which is the point: the hard research is done; the build is what scales.

**Slide 8, achievability and the ask (Lightbulb 5).** The kernel is frozen; no more research
risk. What remains is engineering and adoption: real cloud apply, the AI workflow as a
product, stdlib breadth, and the teachable surface. Present the tiered ask (lean / core /
full from `07-BUDGET.md`) and the nonprofit rationale: verified computing infrastructure is a
public good, like LLVM, like the original OpenAI charter; it should be open and not
rent-seeking. This is a long-horizon infrastructure bet that markets underfund and that
matches your funding history.

**Slide 9, the close.** No urgency, no scarcity. "This should exist, and it should be open.
I have built the hard two-thirds alone. Here is exactly what finishing it costs, what it
gives the AI era, and how every dollar is accounted for. I would like you to fund it."

## The verbal pitch (60 seconds, if that is all you get)

"Every tool for building cloud infrastructure today, Terraform, Pulumi, Amazon's CDK,
Winglang, generates configuration. None of them can prove that the system they deploy
actually works: that it converges, that it survives a network partition, that it recovers
from a crash. They check the shape of the config. They cannot see the behavior. I built the
first system that proves the behavior. The correctness of the distributed system is a
machine-checked theorem, and the same proven source deploys to any cloud and runs live. As AI
starts writing the world's infrastructure, this is the trust boundary that checks what the AI
builds, because we stop trusting the model and start checking the artifact. Two of the four
hard pieces are finished and the rest has its proven core done. It runs today, and every
claim is a passing test you can reproduce from a clone. I am raising it as a nonprofit,
because a substrate this fundamental should be open. Here is the demo."

## Objection handling (have these ready, answer honestly)

- **"Coq, Lean, Agda, F\*, Dafny already do verification."** Yes, and Wootz stands on that
  tradition. The difference: those prove math and abstract programs. None deploy real
  multi-cloud infrastructure or project a verified protocol to live actors. Wootz aims
  verification at the place the world actually breaks, with the deploy pipeline that makes the
  proof pay off.
- **"Winglang has a company and funding."** True, and on maturity and adoption Wing wins
  today. On verification and multi-backend portability, Wootz wins, and that gap cannot be
  closed by Wing without rebuilding on a dependent type theory. Funding closes the maturity
  gap; the capability gap runs in our favor.
- **"Why a nonprofit instead of a startup?"** It could be a company. The nonprofit choice is
  deliberate: a trust substrate for the AI era should not be rent-seeking, and open
  infrastructure compounds value for everyone the way LLVM did. This is a values and strategy
  choice, and it matches the original OpenAI thesis.
- **"It is one person."** Yes, and that is the credibility, not the weakness: one builder did
  the hard research the field considers a multi-year team effort. The bus-factor risk is real
  and the budget's first hires mitigate it. The work is robust; the team is what funding
  builds.
- **"Is the proof real or is it marketing?"** Every claim is a `go test ./...` gate, and the
  kernel re-checks every definition on load. Clone it and break it. I will run the suite live
  right now.
- **"What is the riskiest part?"** Adoption, not correctness. The technology works. Getting
  developers and AI agents to author in a dependent type theory is the real challenge, which
  is why the budget funds the teachable on-ramp (the furnace) and the AI authoring loop as
  first-class deliverables.
