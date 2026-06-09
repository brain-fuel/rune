# Rune v3.0.0

**Univalence, in its own layer — without leaving the substrate you built.**

*Two equalities, stratified. A strict, content-addressed, UIP core that stays exactly as fast and legible as it was — and a fibrant inner layer where univalence and the higher inductive types are finally allowed to live. The architecture's own thesis, applied to equality itself.*

*Told straight: this version ships univalence to **reason with**. Making it **compute** is the labelled frontier — and even that, when it comes, comes contained.*

---

### 1 · Where you're standing now
You shipped v2. You have `ℤ`, multisets, your discourse-structures-modulo-R as first-class objects. You made peace with no univalence. And it still itches — because there's *one* project where you want it. A univalent construction. Transport across an equivalence. A corner of synthetic homotopy that would collapse a theorem you care about into a line.

You're not locked out because univalence is useless to you. You're locked out *globally, for a thing you need locally.* And that asymmetry is the tell: it's the architecture in your way now, not the mathematics.

### 2 · What you've tried
You made peace — the itch stayed. You forked to Cubical Agda for the one project — and left your store, your codegen, your QTT, your whole way of working in exile for a single construction. Or you tried to bolt univalence onto the OTT core, and proved to yourself it can't be done: the proof-irrelevant equality validates UIP, univalence refutes UIP, and no cleverness reconciles two incompatible notions inside *one* equality.

### 3 · The idea
**You don't choose UIP-or-univalence globally. You stratify.** Two equalities, side by side: a strict outer one with UIP — your fast, content-addressed OTT core, doing the definitional work — and an inner, fibrant one where univalence lives. They never contradict, because they're never the same relation. The strict layer reasons *about* the fibrant one.

This isn't a foreign import. It's the substrate's own thesis applied to equality itself — the same move you already made putting names in the layer that reads, quantities behind a parameter, targets behind a menu, now made at the one place everyone said it couldn't be. That's why this is the vindication, not just the sequel.

And the honest half, said up front: this gives you principled coexistence and lets you *postulate* inner univalence and reason with it soundly. Making it *compute* is harder and routes back to cubical machinery for the inner layer. v3 ships the first. The second is the frontier.

### 4 · What you get
A second equality stratum: alongside the unchanged strict `Eq`, an inner identity type in a universe of fibrant types, with a fibrancy discipline the outer layer reasons across. Univalence and the genuinely higher inductive types — the circle, suspensions, higher truncations, everything v2 correctly refused — become expressible in the inner layer, postulated where they don't yet compute, with every UIP-needing proof carried by the outer layer.

The point is the quarantine: the homotopy lives inside, and the outer layer stays exactly the tractable, fast, content-addressed core it was. Everything that made Rune *Rune* — the store and hashing, the QTT semiring, codegen-as-plugin, the names-where-you-read split — is *orthogonal to equality* and transfers untouched. That's why this is a swap, not a rewrite.

The frontier, labelled and not sold: to make inner univalence *compute*, the inner layer must go cubical — interval, Kan operations, Glue. That's the machinery v1 fled — but stratified, it's no longer infecting the core; it's isolated to the inner stratum, with everything around it untouched. Which is the strongest vindication the thesis could have: even the thing you most wanted to avoid, when it finally arrives, arrives contained. (And XTT, which you'll find while reading, is not the door — it keeps UIP.)

### 5 · Your new life
One substrate, two equalities. The univalent project lives in the inner layer without exiling you from your store, codegen, or quantities. The architecture's whole philosophy is finally complete and visible end to end — names where you read, quantities as a parameter, targets as a menu, and now equality as a stratum: the right notion of sameness in the right layer, even for the notion everyone said you'd have to leave home to get. And because a cubical-inner layer on a content-addressed substrate is unexplored, this isn't only your tool reaching its final form — it's the research vehicle, and what the hardest chapters of the book are about.

---

*What v3 is not: not, in the shipping release, computational inner univalence — that's the frontier, and pretending otherwise is the one dishonesty this whole roadmap exists to avoid. Not a replacement of the outer core — the strict layer stays, load-bearing. Not XTT. And not an invitation to live in the inner layer — most work stays in the fast, cached outer core; the inner layer is for the constructions that genuinely need it.*
