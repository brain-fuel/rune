# Rune v1.0.0 — Design Document

**The Foundation · Stratum One** · *organized as an argument*

> **Release criterion.** v1.0.0 ships when every code listing in *Specify & Verify* elaborates, checks, and runs against this core — and the core contains nothing the book does not use. The manuscript is the specification.

*Part one of a three-version roadmap built on one architectural thesis: the parts of a type theory that people fight wars over — the codegen target, the resource discipline, the notion of equality — should each sit behind a clean interface so the right one can live in the right layer. v1 establishes that interface discipline and ships one implementation of each. v2 ([quotients](./rune-v2-design.md)) proves the equality interface is extensible without swapping. v3 ([two-level type theory](./rune-v3-design.md)) proves it is swappable-at-a-cost. This document argues rather than pitches, names the good tools generously, and is explicit about its risks — in the spirit of the framework it is organized on. Working title "Rune"; the actor model from the prior Rune notes is reclassified as a codegen/runtime concern, out of scope.*

---

## Lightbulb 1 — Where You're Standing

You believe in dependent types. You've felt the moment a proof goes through and the compiler certifies something a test could only sample. You want more of that — in code that ships, in a tool you'll still understand in five years. This document's first job isn't to teach you that. You know it. It's to say your situation back to you plainly enough that you trust the rest.

You're on a **treadmill**: the language you love deploys where you need it only through a backend someone maintains as a second-class citizen, on a cadence you don't control, where every upstream change is a fresh chance for it to break. You wanted a foundation; you got a maintenance subscription with an unbounded cost.

You **re-prove what hasn't changed**, because your tools rebuild their world from a file model that doesn't know what moved. "Have I already verified this?" is a recomputation when it should be a lookup. For a corpus that only grows, that's the difference between work that compounds and work that resets every morning.

You **can't read your own core**, because de Bruijn arithmetic fights you exactly where you think.

You reach for **`postulate funext`** and resent it, because your equality won't compute at the moment it should glide.

And some of your **proofs prove nothing**, because totality was never checked and a partial definition slipped through.

Under all five, the gap between *it runs* and *it's correct* keeps reopening — and none of the reasons are fundamental. They're accidents of how the tools were built, living below the layer you're allowed to touch. You're not alone here: researchers wanting a substrate small enough to study, authors wanting a kernel legible enough to teach, the near-empty set of people who want verification both content-addressed *and* dependently typed, and anyone reasoning formally about syntax with binding.

## Lightbulb 2 — What You've Already Tried

You've tried the good tools. They're good — and naming where each stops is how this earns the right to propose another. **Idris 2** gave you quantitative types and the best ergonomics in the family, then left the equality inductive, the JVM a treadmill, the codebase uncached. **Agda** lets you explore anything, cubically if you like, at research-grade weight, file-based, no proof cache, no quantity spine. **Coq and Lean** are vast and battle-tested, and vast is the problem — a kernel you survive, targets that aren't quite *boring*, no content-addressed proofs (though Lean's never-reused `FVarId`s are the precedent §3 founds on theory). **F\*** extracts to everything by leaning on an SMT solver — trusting verification you can't read, the thing you set out to avoid. **Unison** nailed the part nobody else did — content-addressed, incremental, no builds from scratch — but isn't dependently typed, and its cache-soundness story depends on that; you can't bolt your types onto it. Every one nails one or two of {QTT, computing equality with funext, content-addressed incremental proof, a legible small kernel, boring codegen anywhere, principled binding} and concedes the rest — and several concessions are *architectural*. The intersection is empty. That's a gap in the field, not your search.

## Lightbulb 3 — The Idea Behind Rune

This is the idea, not the product. One conviction: **the compromises you hate aren't features other tools forgot — they're accidents of architecture, and you can refuse to inherit them by owning a small core and choosing its shape on purpose.** Six principles follow, and the last is the one that makes this a roadmap rather than a single release.

**A finished proof is a value, and its identity is the hash of its content.** So verification is a *cache*, and caches don't recompute. Prove a thing once; never prove it again. This is the signature idea — the one no file-based tool reaches structurally.

**Dependent types are a build-time discipline, not a runtime.** What deploys is the erasure — the shadow. So "make it run on the JVM" was never the real problem; emitting boring code at a chosen target is, and that's codegen. The treadmill was self-inflicted.

**Equality should be something you compute with, not something you postulate.** Extensionality should fall out of the theory and still compute — not be bolted on by assumption, nor cost you machinery built for homotopy you aren't doing.

**A proof of a partial function proves nothing** — so totality isn't a setting. It's the floor.

**Exactly one representation of your program needs names: the one you read.** A theorem — Pitts' result that you can't pick fresh names by a pure function — decides which layer gets names and which goes without. You stop fighting de Bruijn everywhere and put names only where your eyes are.

**And the parts worth fighting over should each sit behind an interface.** The codegen target, the resource semiring, and the notion of equality are not hardcoded choices baked through the core; they are *strata* with clean boundaries, and the rest of the substrate is built orthogonal to them. This is why Rune is three versions and not one: the same architecture that lets you swap a backend lets a later core extend or swap the equality, reusing everything around it.

## Lightbulb 4 — What You Get: Rune v1.0.0

### 4.1 The freeze condition
v1.0.0 is reached when every listing in *Specify & Verify* elaborates, checks, and runs, and the core contains nothing the book doesn't use. The book's treatment of any topic is the spec for how far the substrate goes. This collapses two projects into one object — the legible core is the book's worked example; the book is its documentation — and supplies the forcing function ownership otherwise removes.

### 4.2 The irreversibles — decided once, in Phase 0
These are the *shape of the core data types*, not phases. **Glued NbE value domain** (smalltt): neutrals carry an un-unfolded spine plus a lazy unfolding, so conversion has a fast syntactic path that forces only on mismatch and errors print near source. **Observational equality stratum** (Pujet–Tabareau): proof-irrelevant `Prop`, an observational `Eq` computing on type structure, `cast`; funext and propext fall out; canonicity preserved; UIP holds. **Parametric usage semiring** (Atkey QTT): usage threaded through binders, the 0/1/ω lattice a swappable module; the 0-fragment *is* the erasure boundary. **Content-addressed, hashable core IR**: locally-nameless, canonically serializable, dependencies-by-hash, addressed by *syntax*, never modulo conversion; format versioned via a preimage tag. **The three-representation split**: de Bruijn at the hash boundary and hot path (theorem-forced), names at the surface. Plus the walking skeleton: lexer, parser, core, **pretty-printer**, and a `parse ∘ pretty` round-trip.

### 4.3 The strata, and the reversibles
The three stratum interfaces are first-class in v1: the **codegen stratum** (erased IR → backend plugin), the **quantity stratum** (the usage semiring module), and the **equality stratum** (the equality type formers + their eval/quote/conversion cases, behind a bounded internal interface). v1 ships one implementation of each. The equality interface is the load-bearing one for the roadmap: the store, hashing, semiring, codegen, and surface/nameless split are all *orthogonal* to it, by design, so that v2 can extend it and v3 can swap it without a rewrite.

Deferred on purpose, because they're well-understood swaps: the **universe hierarchy** (`type : type` until Phase 6, then a real hierarchy — a one-time cache nuke, harmless pre-1.0); the **codebase-manager UX** (a `ucm`-style experience is a decade-scale orthogonal project; v1 ships a flat `name → hash` map plus persistence); and the **JVM backend** (one codegen plugin, when the bank needs it).

### 4.4 Architecture

```
surface/      lexer, parser, named AST, pretty-printer        (names live here)
elaborate/    bidirectional check/infer, metavariables,
              pattern unification, implicit insertion          (reads & writes the store)
core/         locally-nameless Tm; glued Val domain;
              NbE eval/quote; conversion w/ unfolding-tracking  (de Bruijn lives here)
equality/     OTT: Prop, Eq, cast                               (the equality stratum)
quantity/     usage semiring module                            (the quantity stratum)
data/         datatypes, eliminators, coverage, totality, SCC  (Phase 4; the graveyard)
store/        content-addressed blobs; SCC-as-unit hashing;
              name→hash map; proof cache keyed by defn closure  (wraps elaborate/)
codegen/      erasure -> target-independent IR -> backend       (the codegen stratum)
harness/      Iron Sieve properties; the book's listings        (the test corpus)
```

```idris
-- Bound vars are de Bruijn; top-level free vars are content hashes = Pitts constants.
data Tm = Var Idx | Ref Hash
        | Pi Qty Tm (Scope Tm) | Lam Qty (Scope Tm) | App Tm Tm Qty
        | Sig Tm (Scope Tm) | Pair Tm Tm | Fst Tm | Snd Tm
        | U | Eq Tm Tm Tm | Refl Tm | Cast Tm Tm Tm Tm   -- equality stratum

data Val = VNeu Neutral (Lazy Val)            -- glued: spine + lazy unfolding
         | VPi Qty Val (Val -> Val) | VLam Qty (Val -> Val) -- ...

CacheKey : Type
CacheKey = (Hash, SortedSet Hash)             -- (term, definitional closure)

interface Backend where emit : ErasedIR -> TargetSource  -- v1 target: Chez or JS
```

Two facts earn the design its keep. **Content-addressing wraps elaboration; it is not a phase.** You hash elaborated, meta-free core by syntax, never modulo conversion — hashing meaning would need a normal form merely to assign identity (undecidable, divergent pre-totality, fatal to glued laziness). Content-identity finer than definitional equality is the correct price; it keeps content-addressing orthogonal to the type theory. Mutually-recursive definitions are SCCs, hashed as a unit (Unison-style), and that SCC decomposition is the *same* analysis the totality checker needs. **The one genuinely novel seam is cache soundness**, the place this cannot copy Unison: in a dependent theory a definition's *definitional* dependencies can exceed its *syntactic* ones — if checking `foo` unfolded the body of `bar` during conversion but `bar` appears in neither `foo`'s term nor type, a naive hash-keyed cache reports `foo` verified after `bar`'s body changes. The fix makes the cache key `(term-hash, set-of-unfolded-dependency-hashes)` — the true definitional closure; the sound-but-coarser fallback is the full transitive closure. This seam earns a chapter, and Iron Sieve is pointed at it.

### 4.5 The road to 1.0
Gates are the book's chapters, not the phases. **Phase 0** locks the irreversibles + skeleton. **Phase 1**: MLTT core with glued NbE (*weeks*). **Phase 2**: metavariables, pattern unification, implicits (*weeks*). **Phase 3**: the OTT equality stratum (*weeks–months*), before data. **Phase 4**: data, pattern matching, coverage, totality — *the graveyard, months*; non-negotiable, because without totality the proofs are vacuous. **Phase 5**: turn on QTT (*weeks*). **Phase 6**: universe hierarchy (*weeks*; deliberately late). **Phase 7**: codegen + one non-JVM backend (*weeks–months*). **Phase 8**: hardening — readable errors, totality edge cases, a book's-worth of stdlib and nothing more. Ships when the chapters compile.

### 4.6 It verifies itself, and the discipline that ships it
Iron Sieve (PBT + PDDT + MUT, Cupel grading specs) is pointed at the kernel: type preservation, conversion as an equivalence and congruence, `quote ∘ eval` idempotence, `parse ∘ pretty` round-trips, well-typed-term generators with the dual that ill-typed terms are rejected, and the cache-soundness property. A surviving mutant in the soundness region is a hole in the *checker*. And the discipline scaffold — chapters as gates, a version-controlled **parking lot** for every improvement that doesn't serve a current listing, Iron Sieve as forcing-function — is load-bearing, not decoration. Without it the offer never ships.

## Lightbulb 5 — Your New Life
Verification compounds: Monday's proofs are proven Friday without re-running; the corpus grows like a codebase, not a sandcastle. You open the core a year later and recognize it. You target a new platform by writing one backend. You stop postulating. And the substrate built to serve the book *becomes* the book, its hardest pieces becoming chapters and maybe a thesis. You weren't pushed into any of it; you reasoned your way to it — and for a skeptical engineer that's the only yes that lasts, because manipulation bounces off and only an argument sticks.

## Non-Goals (v1.0.0)
Not an Idris replacement; not univalent; not a `ucm` clone; not JVM-first; not SMT-backed; not a tactic framework; not a runtime for the actor model. Univalence and higher inductive types are out **by mathematics, not omission** — the proof-irrelevant OTT equality validates UIP, which univalence refutes; this is the trade that buys funext-that-computes, definitional proof irrelevance, the tractable glued NbE the speed thesis rests on, and the K-friendly coverage of Phase 4. v2 and v3 address what UIP costs without un-choosing what it bought.

## Risks and Mitigations
- **The Phase-4 long tail.** *Mitigation:* book-as-freeze caps scope; the OTT/K-friendly dividend eases coverage; chapters gate progress.
- **Cache-soundness is genuinely novel.** *Mitigation:* sound fallback (full closure) from day one; property-stated and mutation-tested; scoped as a chapter.
- **Nominal-cross-dependent is thinly charted.** *Mitigation:* nominal confined to human-facing layers; hot path and hash form stay nameless.
- **The discipline failure mode.** The substrate expanding to fill all time while the book starves is the *default*, not a tail risk. *Mitigation:* parking lot, chapter gates, Iron Sieve-as-forcing-function.

## References
Pitts, *Nominal Logic* (Inf. & Comp. 186, 2003), §8 — the no-choice result forcing the three-representation split · Gabbay & Pitts, *Abstract Syntax with Variable Binding* (FAC 13, 2002) · Pujet & Tabareau, *Observational Equality: Now For Good* (POPL 2022) · Kovács, *smalltt* and *elaboration-zoo* · Atkey, *Quantitative Type Theory* (LICS 2018) · Unison (content-addressed code; SCC hashing) · Broas, *The Five Lightbulbs* and *Marketing Is An Argument* — the structure this is organized on.
