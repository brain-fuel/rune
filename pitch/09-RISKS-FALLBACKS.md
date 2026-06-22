# Risks, kill criteria, and who funds this if Elon does not

A funder of this caliber respects a builder who names the risks first. So does a good plan.
This document is the honesty backbone of the package: it states what could go wrong, when to
stop, and the fallbacks that make the project fundable regardless of whether Elon ever sees
it.

## The risks, named

### 1. Reaching Elon is low probability
This is the biggest risk to the *stated goal*, and the smallest risk to the *project*,
because the strategy in `05-OUTREACH-ELON.md` is built so the work funds itself through other
channels even if the meeting never happens. Mitigation: lead with a reproducible demo that
travels through his network on its own; pursue adjacent funders in parallel; treat early
funding from anyone as the credibility that opens the Elon door. Do not bet the project on one
person's attention.

### 2. Adoption, not correctness, is the real technical risk
The technology works and is reproducible. The open question is whether developers and AI
agents will author in a dependent type theory. Mitigation: the furnace is the on-ramp
(property-testing first, proof later), the diagnostics are already human grade, and the AI
authoring loop is funded as a first-class deliverable precisely because the agent, not the
human, may be the primary author. The bet is that AI lowers the adoption cost of rigor, which
is exactly the thesis.

### 3. Bus factor: it is one person
Real risk. Mitigation: the first two hires in every tier exist to spread the knowledge, the
codebase is content-addressed and heavily gated by tests (so it is legible and re-checkable),
and the external review in the Core tier creates a second set of expert eyes. Naming this
honestly is better than having a funder discover it.

### 4. The "nonprofit" framing may be the wrong vehicle
It could be a company. The nonprofit choice is a values-and-strategy bet that a trust
substrate should be open and that this matches Elon's funding history. If a funder prefers a
company, that is a conversation, not a dealbreaker; the work is the same either way. Do not be
dogmatic about the vehicle.

### 5. "Better than Winglang" can read as overclaiming if stated carelessly
Mitigation: the comparison is always scoped (Wootz wins on verification and multi-backend,
loses on maturity today). State the loss before anyone asks. A scoped, honest comparison is
persuasive; an unscoped boast gets dismantled by a technical audience in one question.

### 6. The one open research item never closes
The general all-process adequacy refinement may stay open. Mitigation: it is off the critical
path and no deliverable depends on it. The package already says this. It is a deepening of the
correctness theory, not a product blocker.

## Kill criteria (when to stop or pivot)

Stated plainly, because a funder trusts a plan that knows its own off-ramps:
- **If the live-cloud-apply demo (Lean M1) cannot be made to work on real accounts within the
  Lean budget,** the deploy thesis is weaker than claimed and the project should pivot to the
  verification-library-and-AI-gate angle alone, or pause. (Low risk: this is well-trodden
  engineering.)
- **If the AI eval suite (M2) shows the proof gate does not measurably improve correct-system
  rate,** the AI-centric thesis is wrong and the pitch must change. This is the single most
  important number to measure early, and it is honest to say so.
- **If no funder of any kind commits within 12 months of a polished public demo,** reassess
  whether the framing, the market, or the timing is off, rather than continuing on hope.

## Fallback funders (the project does not depend on Elon)

The same demo and package fund the work through any of these. Pursuing them in parallel is
strategy, not surrender, and each one makes the Elon path more likely by turning "one person
asking" into "a funded, credible project."

- **Science-and-infrastructure philanthropy.** The Schmidt-Futures-style funders and similar
  back long-horizon technical infrastructure that markets underfund. Wootz is squarely in
  scope.
- **AI-safety and verification funders.** Open Philanthropy and aligned funders support
  tractable, engineering-grade contributions to AI safety; the AI-output-verification framing
  in `03-AI-WORKFLOW.md` is exactly that.
- **Formal-methods grant programs.** The Ethereum Foundation and other crypto-adjacent
  organizations fund verification heavily because their domain demands it; a verified
  infrastructure substrate is directly useful to them.
- **Public-good software funds.** The Sovereign Tech Fund and peers fund open foundational
  software as critical infrastructure.
- **Government research.** NSF and DARPA have long-standing programs in verified and
  high-assurance systems; slower, but real and non-dilutive.
- **The cloud vendors themselves.** AWS, Microsoft, and Google each have open-source and
  research arms with an interest in verified infrastructure tooling on their platforms.
- **A commercial path, if the nonprofit vehicle is set aside.** The verification-and-deploy
  product is venture-fundable on its own merits; the nonprofit framing is a choice, not a
  constraint.

## The single most important discipline

Across all of this: the work is the leverage, and honesty is the credibility. The demo is
reproducible, the claims are scoped, the budget is itemized, the risks are named, and the
project survives the failure of any single path including the Elon path. That posture is what
makes a serious funder lean in, and it is the opposite of the manufactured urgency that makes
them leave.
