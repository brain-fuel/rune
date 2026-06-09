# Rune v2.0.0 — Design Document

**Quotients and Set-Level HITs · Extending Stratum One** · *a delta on [v1](./rune-v1-design.md)*

> **Release criterion.** Same shape as v1: every v2 listing in *Specify & Verify* — the chapters that use a quotient or a set-level higher inductive type — elaborates, checks, and runs, and the core gains nothing those chapters don't use.

*This is the easy version of the roadmap to believe, because it doesn't touch the Phase-0 irreversible. It proves the equality interface is **extensible** — that the core can grow new type formers inside the existing equality without swapping it. Everything in v1 stands; this document is only the delta.*

---

## Lightbulb 1 — Where you're standing now (you already have Rune)

You shipped v1. Proofs compound, the core is legible, deployment is a plugin. And you keep doing the one thing the substrate makes you do by hand that the mathematics should do for you: **you hand-roll setoids.**

You want the integers, so you carry a pair-with-a-relation and prove every function respects it. You want a multiset, or a finite set, so you carry a list and a permutation relation and re-discharge respect at every use site. You want your discourse structures modulo an equivalence — trees-modulo-a-relation, terms-modulo-α — and you're threading "respects R" proofs through code that should have stopped caring about representatives three definitions ago. It works. It's bookkeeping the type theory could be carrying, and it has no canonical forms, so nothing reduces the way you want and the content-addressed cache can't help you because two equal things hash differently.

## Lightbulb 2 — What you've tried within Rune

Hand-rolled setoids: correct, tedious, no canonicalisation, respect re-proved everywhere. Newtype wrappers that *assert* the quotient: a lie about equality that the kernel will eventually make you pay for. Or you went and looked at Cubical Agda's higher inductive types — and recoiled, correctly, at standing up interval variables and Kan operations to get `ℤ`, when you have no use for the circle and never will. You don't need homotopy. You need a quotient.

## Lightbulb 3 — The idea

**Most of "I wish I had HITs" is "I wish I had quotients" — and quotients are UIP-safe.** A quotient `A / R` is a set-level construction; nothing about it requires the universe to be a groupoid, so it sits inside the OTT equality with no tension at all. And the very thing v1 already gives you computationally — funext and propositional extensionality — is exactly what makes quotients well-behaved: it's what lets the eliminator's "respects `R`" obligation be discharged once and then forgotten. So the move is not to swap the equality stratum. It's to **extend** it: add the type former, add its eliminator, and the rest is the same theory you already trust.

This is the lineage of setoid type theory and quotient inductive types (Altenkirch and collaborators), and it is deliberately the *boring* version of the dream — which is the point. It's reachable as a v2 because it changes nothing irreversible.

## Lightbulb 4 — What you get: Rune v2.0.0 (the delta)

**The type formers.** A quotient former `A / R` with point-introduction `[_] : A → A / R` and the path-introduction that identifies `[a]` and `[b]` when `R a b`; an eliminator into a family that takes a "respects `R`" premise and computes on `[a]`. The set-level higher inductives that fall in the same UIP-safe class: **propositional truncation** `‖A‖` (the workhorse for "exists without choosing a witness"), set truncation, and quotient-inductive definitions generally — types defined by constructors *and* equations, with eliminators that demand the equations be respected. Higher truncations and the genuinely higher HITs (the circle, suspensions, synthetic homotopy) are **not** here, and not deferred — they require ≥ groupoid structure that UIP collapses, and they live in v3's inner layer or nowhere in this core.

**What each subsystem gains, concretely — and this is the whole engineering content of v2:**

- **Conversion / NbE.** New cases: quotient types and `[_]`, the eliminator's β-rule on point-constructors. Because UIP holds, the path-constructors are proof-irrelevant — they carry no computational content to compare, which is precisely why this stays tractable and why glued NbE doesn't choke. There is no interval to evaluate over. This is the sentence that makes v2 a few months rather than a fork.
- **The store and hashing.** Quotient types and their eliminators are new syntax, so they get new hash cases — mechanical. The subtle, satisfying part: `A / R` gives you *canonical-ish* forms where the setoid gave you none, so the content-addressed cache starts earning its keep on quotiented values, where before two equal things hashed apart. The hashing discipline is unchanged (by syntax, never modulo conversion); there is simply more worth caching.
- **Quantities.** The eliminator threads usage like any other; the parametric semiring is untouched.
- **Codegen.** The path-constructors and the respect-proofs are in the 0-fragment — they erase. A quotient compiles to its underlying carrier; `[_]` is the identity at runtime. The shadow stays boring.
- **Totality / coverage.** The one place that needs real care: coverage now has to account for quotient eliminators (a match on `A / R` must go through the eliminator and discharge respect, not pattern-match the carrier directly). The OTT/K-friendly setting from v1 keeps this in the well-understood regime; it is more work, not new mathematics.

**The freeze still rules.** Which quotients and truncations exist in v2 is exactly what the v2 chapters of *Specify & Verify* demonstrate — `ℤ`, `ℚ`, finite multisets, a syntax-modulo-α example, and a discourse-structure-modulo-R example, say — and nothing further. Quotient-inductive-*inductive* types and a general schema for user-declared HITs are real temptations and go in the **parking lot** unless a chapter teaches them.

## Lightbulb 5 — Your new life
Quotients are values. You define `ℤ`, `ℚ`, multisets, finite sets, syntax-modulo-α, and your discourse-structures-modulo-R *once*, and reason about them without ever again threading a respect-proof by hand. You get canonical behaviour where you had setoid sludge, and the content-addressed cache extends to cover it, so quotiented proofs compound like everything else. And the typed-DAG discourse work stops paying a representation tax it never should have owed — the structures you actually study become first-class objects of the theory.

## Non-Goals (v2.0.0)
No univalence (still out by mathematics — UIP holds). No higher truncations, no circle, no synthetic homotopy — those need v3's inner layer. No general user-facing HIT schema in v2; the quotient and truncation formers the book uses, and no more. No change to the equality stratum's interface — this is an extension, and keeping it inside the interface is what makes it a v2.

## Risks and Mitigations
- **Coverage for quotient eliminators** is the real cost. *Mitigation:* the K-friendly regime inherited from v1 keeps it standard; the book caps which eliminators must exist.
- **Definitional-equality interactions.** New formers mean new conversion cases that the cache-soundness seam (§4.4 of v1) must still capture — unfolding-tracking has to see quotient-eliminator reductions. *Mitigation:* the sound full-closure fallback covers it while the precise tracking is extended and re-mutation-tested.
- **Scope creep toward QIITs.** *Mitigation:* parking lot; chapter gates.

## References
v1 design doc (the foundation this extends) · Altenkirch, Boulier, Kaposi & Tabareau, *Setoid Type Theory — A Syntactic Translation* (MPC 2019) · Altenkirch, Capriotti, Dijkstra, Kraus & Nordvall Forsberg, *Quotient Inductive-Inductive Types* (FoSSaCS 2018) · Pujet & Tabareau, *Observational Equality: Now For Good* (POPL 2022) — the equality this stays inside.
