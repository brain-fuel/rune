# What the money buys: the funded roadmap

This maps the funding to the actual remaining work in the DAG (the live plan is
`~/.claude/plans/humble-humming-elephant.md`, synced to v3.328.36 / ch465). The framing a
funder needs: the hard, uncertain research is finished and the kernel is frozen. Everything
below is engineering, design, or external-gated work, each item an independent shippable
increment with a demonstrated need.

## The state at funding time (what is already done)

- **Telos 1, computational inner univalence: DONE.** The full cubical interior, univalence
  that computes, the inner higher-inductive-type kit. The "cubical rabbit hole," the project's
  named top hazard, is retired.
- **Telos 2, portable proof-carrying codegen: DONE.** One erased intermediate form lowered to
  8 backends (JS, Python, Go, Rust, BEAM, JVM source, plus C and LLVM-IR native), with a
  C-ABI FFI carrying contracts at the boundary.
- **Telos 3, tiered-verified stdlib: proven core DONE, breadth remaining.** 460+ listings
  carry the proven tier (the numeric tower, boolean algebra, lattices, sorting, verified
  collections, a verified compiler toolchain, real BLAS/NumPy interop behind contracts, OTP
  on four backends, the full OS vocabulary).
- **Telos 4, verified distributed algebra: proven core DONE, surface remaining.** CRDTs,
  consensus, state-machine replication, vector clocks, causal delivery, the process calculus
  with bisimulation, and the prove-to-simulate-to-deploy-to-run-to-live infra pipeline.

## The remaining frontier, and what funding does to each

From the DAG's frontier analysis, four things remain. Funding addresses them in priority
order.

### 1. Real cloud apply (engineering, external-gated), funded first
**State:** the deploy pipeline emits Terraform/OpenTofu HCL for AWS/Azure/GCP and runs 15
FOSS backends live under Docker/Podman. Live apply against billed accounts is not yet done.
**Funded work:** cloud accounts plus the `--apply` lifecycle; live deploy of the reference
app on three clouds; the dependency-heavy matrix tail (k8s, streaming, networking); the
verified distributed app observed recovering from real failures on managed services.
**Milestone M1 (Lean delivers this):** the demo runs live on three clouds.

### 2. The AI-centric workflow as a product (engineering + design), funded second
**State:** the proof gate (the kernel) and the furnace exist; the diagnostics are human
grade; the simulator produces real counterexample traces.
**Funded work:** the agent harness that authors Wootz and iterates against the kernel's
diagnostics and the furnace's blame; the proof-repair loop; the verified-building-block
library the agent composes from; and the evaluation suite measuring correct-system rate with
and without the gate.
**Milestone M2:** an agent reliably turns a natural-language spec into a proven, deployed
distributed system, with a published eval showing the gate's effect.

### 3. Stdlib breadth toward production parity (engineering), funded throughout
**State:** deep proven core, partial production breadth.
**Funded work:** the typed-and-tested leaf libraries that a real application needs, promoted
to proven where it pays, plus the remaining D4 ndarray/host-model design.
**Milestone M3:** a real application (a verified web service with a distributed backend)
builds and runs on the stdlib.

### 4. The open research sliver (research, parked, not on the critical path)
**State:** one genuinely open item, the general all-process adequacy refinement (the
par-interleave fuel wall), with a scoped attack documented. It is not required for any
shippable deliverable.
**Funded work:** dedicated research time only after the engineering milestones land; it is a
correctness-theory deepening, not a product blocker. Honesty point for the funder: this is the
one thing money does not simply finish, and it does not need to, because nothing ships depends
on it.

## Timeline (Core tier, 24 months)

| Months | Focus | Milestone |
|--------|-------|-----------|
| 1 to 6 | Cloud apply on one then three clouds; demo upgraded to live; hire engineers 1 and 2 | M1: live on three clouds |
| 4 to 12 | AI authoring loop + proof-repair + eval suite; matrix tail begins | M2: agent produces proven deployed systems |
| 10 to 18 | Stdlib breadth; reference verified distributed app on managed services; external review | M3: a real app on the stack |
| 16 to 24 | Adoption surface (docs, packaging, on-ramp); the open-research sliver if time allows | M4: teachable, packaged, externally reviewed |

## The honest one-liner for the roadmap

"The research that everyone said would take a team years is done, by one person, and frozen.
What you are funding is the well-understood engineering and the adoption that turns a proven
substrate into one people use. Every milestone is independently shippable, and the riskiest
remaining item is not on the critical path."
