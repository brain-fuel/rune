# Non-Goals

What Rune's v1 core is deliberately **not**. These are decided, not open questions.

- **Not univalent.** The equality stratum is observational (Pujet–Tabareau): a
  proof-irrelevant `Prop`, an `Eq` that computes on type structure, `cast`. It
  validates **UIP**, which univalence refutes.
- **No higher inductive types in this core.** UIP holds by **mathematics, not
  omission**. It is the trade that buys funext-that-computes, definitional proof
  irrelevance, the tractable glued NbE the speed thesis rests on, and K-friendly
  coverage. v2 adds set-level quotients and truncations (UIP-safe); the genuinely
  higher HITs — the circle, suspensions, synthetic homotopy — live in v3's inner
  layer or nowhere in this core.
- **Not SMT-backed.** Verification you can read, not verification you trust a solver
  for.
- **Not a tactic framework.**
- **Not JVM-first.** Codegen is a stratum; the JVM is one backend plugin if and when
  a consumer needs it.
- **Not a `ucm` clone.** The codebase-manager UX is a decade-scale orthogonal
  project; v1 ships a flat `name → hash` map.
- **Not a runtime for the actor model.** The actor model from the prior Rune notes
  is reclassified as a codegen/runtime concern, out of scope here.

What UIP costs is addressed by v2 and v3 **without un-choosing what it bought**.
