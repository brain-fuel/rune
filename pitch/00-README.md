# Wootz: a funding package

*Goal: secure philanthropic funding (Elon Musk / Musk Foundation as the primary target,
robust to other funders) to finish Wootz as an open, nonprofit, verified-computing
substrate for the AI era.*

## The one paragraph

Civilization now runs on distributed software, and AI is about to write most of it. We
have no way to know that AI-generated infrastructure is correct. Terraform, Pulumi, AWS
CDK, and Winglang all generate configuration; none of them can prove that the system they
deploy actually converges, stays safe under a network partition, or recovers from a crash.
Configuration tools check syntax. Tests sample behavior. Neither proves anything. Wootz is
the first system where the correctness of a distributed system is a machine-checked
theorem, and the very same verified source is simulated under fault injection and deployed
to any cloud. Prove it, simulate it, deploy it, run it, all from one file. Two of the four
foundational pillars are finished; the other two have their proven core landed. It runs
today.

## What this package contains

| File | What it is |
|------|------------|
| `00-README.md` | This index and the core thesis |
| `01-DEMO.md` | The live demo: the exact sequence to show, with real commands |
| `02-COMPARISON.md` | Head to head vs Winglang, Terraform/OpenTofu, Pulumi, AWS CDK |
| `03-AI-WORKFLOW.md` | The AI-centric workflow: agent to Wootz to proof gate to deploy |
| `04-CLOUD-PLAN.md` | Finishing the work with real cloud accounts: the matrix tail and live apply |
| `05-OUTREACH-ELON.md` | How to actually reach Elon, with honest odds and fallbacks |
| `06-PITCH-SCRIPT.md` | The deck outline and what to say, built on Marketing-Is-An-Argument |
| `07-BUDGET.md` | A tiered 12/24/36 month nonprofit budget, including the founder's pay |
| `08-ROADMAP-FUNDED.md` | What the money buys, mapped to the remaining DAG |
| `09-RISKS-FALLBACKS.md` | Honest risks, kill criteria, and who funds this if Elon does not |
| `10-SUSTAINABILITY.md` | After year 3: endowment + membership + commercial arm, and the endgame states |
| `11-DEMO-CHAPTER.md` | The demo, in full: a technical-diligence read (the long-form chapter behind the script) |

## The discipline this package holds itself to

Everything here is honest. Elon's circle will scrutinize every claim, so the package
distinguishes, at every point, what is **landed and testable today** from what is
**remaining work the funding pays for**. Overclaiming is the fastest way to lose a
technical audience. The argument is made with dignity: no manufactured urgency, no
scarcity, no FOMO. The case is strong enough to stand on the facts.

## The honest headline

- **Landed today, runnable, gated by `go test ./...`:** a dependently-typed kernel with
  computational univalence; proof-carrying codegen to 8 backends; 460+ machine-checked
  listings; a process calculus that proves distributed properties and projects them to
  live actors; and a prove to simulate to deploy to run to live pipeline for
  infrastructure-as-code.
- **Remaining, what the funding finishes:** real cloud apply against live AWS/Azure/GCP
  accounts; the AI-centric authoring and proof-gate workflow as a product; standard-library
  breadth for production parity; and the adoption surface (docs, packaging, the teachable
  on-ramp).
- **Where "better than Winglang" is true and where it is not:** Wootz wins decisively on
  verification and on multi-backend portability. It loses today on ecosystem maturity,
  polish, and adoption. The funding closes exactly that gap.

Start with `01-DEMO.md`. The demo is the argument; everything else supports it.
