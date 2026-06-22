# The AI-centric workflow

This is the section that turns Wootz from "an impressive verified language" into "the thing
Elon should fund." The argument: AI is about to write most of the world's infrastructure
code, and we have no trust boundary for AI-generated systems. Wootz is that trust boundary.

## The problem, stated plainly

An LLM can generate Terraform, Pulumi, or Wing today. It will also confidently generate a
distributed counter that loses writes, a replication scheme that diverges under a partition,
a supervision tree that never recovers, an IAM policy that leaks, and a queue consumer that
drops messages. The generated config is syntactically valid. It deploys. It passes the
tests the same model wrote for it. And it is wrong in ways no one notices until production,
because the tools the AI targets cannot represent the property that is being violated.

As more infrastructure is authored by models, this is not a developer-experience problem. It
is a systemic reliability and safety problem. The trust we place in software is about to be
extended to software no human carefully reviewed, deployed onto substrates that cannot check
behavioral correctness. That gap compounds.

## The Wootz answer: make AI output checkable, not trusted

The workflow inverts the trust relationship. Instead of trusting the AI's output and hoping,
you make the output carry a proof obligation that a kernel discharges or rejects.

```
  ┌─────────────┐   Wootz source   ┌──────────────┐   accept/reject   ┌─────────────┐
  │  AI agent   │ ───────────────> │  proof gate  │ ────────────────> │  simulate   │
  │  (proposes  │   (protocol +    │  (the kernel │   (only provably  │  under fault │
  │   a system) │    infra + app)  │   re-checks  │    correct passes)│   injection) │
  └─────────────┘                  │   every def) │                   └──────┬──────┘
        ^                          └──────────────┘                          │
        │  blame / counterexample feeds back                                 v
        └────────────────────────────────────────────────────────────  ┌─────────┐
                                                                         │ deploy  │
                                                                         │ + run   │
                                                                         │ + live  │
                                                                         └─────────┘
```

1. **The agent proposes a system in Wootz** (the distributed protocol, the infra graph, and
   the application logic, in one source). This is a better target for a model than HCL,
   because the language has real abstractions and a type system that catches whole classes
   of error before any proof.
2. **The proof gate is the kernel itself.** Loading a Wootz file re-validates every
   definition. A protocol that does not converge, a merge that is not a join, a supervision
   tree that does not recover: these fail to elaborate. The AI cannot ship a system whose
   correctness property it violated, because the property is a type, and violating it is a
   compile error. This is the trust boundary: not "the AI promised," but "the kernel
   checked."
3. **What cannot be proven is property-tested with blame (the furnace).** Not every leaf is
   provable, and not every foreign call is trusted. The furnace runs AI-proposed foreign
   code against in-language references and blames it at the boundary when it drifts. The AI's
   confidence is replaced by a measurement.
4. **The same source simulates under fault injection,** so the AI's proposal is exercised
   against partitions, duplication, and crashes before anything is billed.
5. **Then it deploys and runs,** the verified source unchanged.

The point: every place a human would currently have to carefully review AI-generated
infrastructure, the kernel reviews it instead, mechanically, completely, every time.

## Why this is concrete AI safety, not philosophy

Elon's stated concern about AI is real and the funding history (the original nonprofit
OpenAI) shows he funds tractable bets on it. Most "AI safety" work is about model alignment
and is genuinely hard and open-ended. This is different and complementary: it is a
**verification substrate for AI output.** It does not try to make the model good. It makes
the model's output checkable, so a fallible model can still produce systems we can trust,
because we are not trusting the model, we are checking the artifact. That is a shippable,
engineering-grade contribution to AI safety, and it is exactly the kind of first-principles,
infrastructure-level bet that markets underfund.

## What is already true today vs what the funding builds

**Already landed and testable:**
- The proof gate exists and is the kernel. Every one of the 460+ listings is re-checked on
  load; an incorrect protocol fails to elaborate. This is demonstrable now.
- The furnace exists: contract-and-blame over foreign numpy/BLAS, property-test suites that
  catch false laws, and a fast-to-proven tier bridge.
- The full prove to simulate to deploy to run to live pipeline runs from one source.

**What the funding builds (the AI workflow as a product):**
- An **agent harness** that authors Wootz, reads the kernel's diagnostics and the furnace's
  blame, and iterates to a proof. The diagnostics are already human-grade (Elm/Rust style);
  the work is the agent loop that consumes them.
- A **proof-repair loop:** when the kernel rejects, feed the counterexample (the simulator
  produces real divergence traces; the law linter names the violated law with a witness)
  back to the model as a structured failure it can fix.
- A **library of verified building blocks** the agent composes from (CRDTs, consensus,
  supervision, the infra matrix), so the model assembles proven parts rather than inventing
  unproven ones.
- An **evaluation suite** measuring how often models, with and without the proof gate,
  produce correct distributed systems. This is the headline metric for the whole thesis and
  the thing a funder will want to see move.

## The demo extension for the AI story

Add one act to the demo in `01-DEMO.md`: ask a model to generate a replicated counter.
Without the gate, show it generating a plausible Last-Writer-Wins counter that loses writes.
Feed the same task through the Wootz gate: the kernel rejects the non-converging version,
the agent reads the law-linter witness, and iterates to a proven G-Counter. The closing
line: "The model is the same. The difference is that here it cannot ship the wrong answer."
