# Budget

A nonprofit budget, tiered so the ask scales to the funder's appetite. All figures are
24-month totals unless noted, in USD. Numbers are deliberately modest and fully itemized;
for a funder of Elon's scale even the full tier is a low-risk bet, and the itemization is
itself a credibility signal. The founder's pay is included explicitly, as requested, and set
below market on purpose for the nonprofit framing.

## The three tiers

| Tier | Duration | Headline | Buys |
|------|----------|----------|------|
| **Lean** | 12 months | **$650k** | Founder + 1 engineer. Real cloud apply on one then three clouds; the demo upgraded to live; the AI workflow prototype. Proves the thesis end to end on real infrastructure. |
| **Core** | 24 months | **$1.75M** | Founder + 2 engineers + part-time DevRel. Everything in Lean, plus the full matrix tail, the AI authoring loop as a product, stdlib breadth toward production parity, an external formal-methods review, and the teachable on-ramp. |
| **Full** | 36 months | **$3.5M** | The Core team sustained, plus a fourth engineer and a real adoption push: documentation, packaging, a public verified-infrastructure registry, and the reference verified distributed applications. Takes Wootz from "runs and is proven" to "adopted." |

The recommended ask is **Core ($1.75M / 24 months)**, with Lean offered as the low-risk
entry point and Full as the ambition. A funder can start at Lean and step up on milestones.

## Core tier, fully itemized (24 months)

### People (~$1.10M)
| Role | Rate | Period | Total |
|------|------|--------|-------|
| Founder / principal engineer (Matt) | $180k / yr | 24 mo | $360k |
| Senior engineer 1 (compilers / formal methods) | $185k / yr | ~20 mo (start month 4) | ~$310k |
| Senior engineer 2 (distributed systems / cloud) | $185k / yr | ~18 mo (start month 6) | ~$280k |
| DevRel / docs / the teachable on-ramp | $90k / yr | part-time, 18 mo | ~$135k |

Founder pay note: $180k/yr is set well below the market rate for a principal-level
compiler and type-theory engineer (typically $250k and up, more at the AI labs). The
discount is the nonprofit signal and is intentional. Matt can set this figure; it is stated
here as a defensible, livable, below-market number that a funder will read as serious rather
than self-dealing.

### Infrastructure and compute (~$270k)
| Item | Basis | Total |
|------|-------|-------|
| Cloud accounts (AWS/Azure/GCP, mostly ephemeral apply-and-destroy + small fixtures + CI) | ~$5k/mo blended, hard-capped | $120k |
| AI / LLM compute for the agent workflow (API + eval, some GPU) | scaled to the eval suite | $120k |
| One-time cloud + CI setup, org hardening, budget-guard tooling | one-time | $30k |

### Operations and credibility (~$250k)
| Item | Total |
|------|-------|
| 501(c)(3) formation, legal, accounting, insurance, compliance | $60k |
| External formal-methods / security review (independent credibility) | $80k |
| Equipment, software, conferences, travel (including any in-person pitch) | $50k |
| Contingency (~10% of core) | $60k |

### Core total: ~$1.75M

## Lean tier, itemized (12 months, ~$650k)
- Founder $180k + 1 senior engineer ~$170k (start month 2) = ~$350k people.
- Cloud + compute ~$120k (the live-apply demo is the priority spend).
- 501(c)(3) + legal + accounting ~$50k.
- Equipment, travel, misc ~$40k.
- Contingency ~$40k.
- Buys: the demo goes from "generates" to "runs live on three clouds, recovering from a real
  failure," and the AI workflow prototype exists. This is the minimum that proves the full
  thesis on real infrastructure, and it is the de-risking step before Core.

## Full tier additions (36 months, ~$3.5M)
- Sustains the Core team to 36 months and adds a fourth engineer (~$185k/yr).
- Adoption budget: documentation, packaging, a public verified-infrastructure registry,
  reference applications, and community building (the part that turns capability into use).
- Larger compute and cloud footprint for the eval suite and public demos.

## How the money is governed (say this to a funder)

- A 501(c)(3) with an independent board, public financials, and milestone-based release of
  funds. The funder can tie tranches to the milestones in `08-ROADMAP-FUNDED.md`.
- Open source, permissive license, no rent-seeking. The output is a public good.
- Every spend category above maps to a deliverable. Burn is predictable: people plus
  mostly-ephemeral cloud, with hard caps and contingency. There is no speculative line item.

## The framing line on cost

"This is the entire cost of finishing a verified-computing substrate for the AI era. The hard
research is already done and paid for, by one person, for free. What is left is engineering
and adoption, fully itemized, governed by an independent board, with the founder taking a
below-market salary. For the value it creates, it is inexpensive, and it should be open."
