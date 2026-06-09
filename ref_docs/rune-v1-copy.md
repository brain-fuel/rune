# Rune v1.0.0

**A dependently typed substrate you own — where verification compounds instead of resetting every morning.**

*Content-addressed proofs. Equality that computes. A core small enough to hold in your head. It deploys anywhere, because what ships is the shadow, not the runtime. And it's built from swappable strata, so it can grow without being rewritten.*

---

### 1 · Where you're standing
You believe in dependent types. You've felt a proof certify what a test could only sample. You want more of it — in code that ships, in a tool you'll still understand in five years.

Instead: you're on a treadmill, deploying through a backend someone else maintains as an afterthought. You re-prove what hasn't changed, because "have I verified this?" is a recomputation when it should be a lookup. You can't read your own core, because de Bruijn arithmetic fights you where you think. You reach for `postulate funext` and resent it. And some of your proofs prove nothing, because totality was never checked.

Under all of it, the gap between *it runs* and *it's correct* keeps reopening — and none of the reasons are fundamental. They're accidents of how your tools were built, below the layer you're allowed to touch. If that's your Tuesday, keep reading. What follows is an argument, not a pitch.

### 2 · What you've already tried
You've tried the good tools. They *are* good. **Idris 2** gave you quantitative types and the cleanest ergonomics — then left the equality inductive, the JVM a treadmill, the codebase uncached. **Agda** explores anything, at research weight, file-based, uncached. **Coq and Lean** are vast and battle-tested, and vast is the problem. **F\*** extracts everywhere by trusting an SMT solver you can't read. **Unison** nailed content-addressing — and isn't dependently typed, with a cache-soundness story that depends on it. Every one nails one or two of the things you want and concedes the rest. The intersection is empty. That's a gap in the field, not your search.

### 3 · The idea behind Rune
One conviction: **the compromises you hate aren't features other tools forgot. They're accidents of architecture — and you can refuse to inherit them by owning a small core and choosing its shape on purpose.**

A finished proof is a value, and its identity is the hash of its content — so verification is a *cache*, and caches don't recompute. Dependent types are a build-time discipline, not a runtime — what deploys is the shadow, so deployment is just codegen and the treadmill was self-inflicted. Equality should be something you compute with, not something you postulate. A proof of a partial function proves nothing — so totality is the floor. Exactly one representation needs names — and a theorem tells you which.

And the parts people fight wars over — the target, the resource discipline, the notion of equality — each sit behind an interface. That's why Rune is a roadmap, not a single release: the same architecture that swaps a backend lets a later core grow the equality, reusing everything around it.

### 4 · What you get
A small dependently typed core you can read end to end: bidirectional elaboration over a glued normaliser (fast conversion, errors that print close to source), an observational equality layer where extensionality computes, quantitative types whose usage semiring is a parameter *you* control, and a content-addressed store that hashes elaborated core by structure — so proofs are cached by their true dependencies and nothing unchanged is re-checked.

Codegen is a plugin over a type-erased form. The first target iterates fast; the JVM is one plugin you write the day the bank needs it. It ships with its own verifier turned on itself — type preservation, conversion as a congruence, normalization stability, cache soundness, all mutation-tested. The tool that checks your code is itself a checked artifact. And it comes with the thing that makes it finishable: a book, *Specify & Verify*, that *is* the specification — v1.0.0 ships when every listing in it runs and the core contains nothing the book doesn't use.

### 5 · Your new life
Verification compounds: Monday's proofs are proven Friday without re-running. You open the core a year later and recognize it. You target a new platform by writing one backend. You stop postulating; your proofs aren't vacuous; the *runs*-versus-*correct* gap stops reopening. And the substrate you built to serve the book becomes the book — and its hardest pieces become chapters, and maybe a thesis. You weren't pushed into any of this. You reasoned your way to it. For a skeptical engineer, that's the only kind of yes that lasts.

---

*What v1 is not: not an Idris replacement, not univalent, not a tactic framework, not SMT-backed, not JVM-first. Univalence and higher inductive types are out by mathematics, not omission — and the roadmap (v2: the quotients you actually wanted; v3: univalence in its own layer) addresses what that costs without un-choosing what it bought. Saying so is part of the argument.*
