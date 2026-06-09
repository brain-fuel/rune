# Rune v2.0.0

**The quotients you actually wanted — without becoming a homotopy theorist to get them.**

*`A / R` as a first-class type. Set-level higher inductives. The same equality that already computes, now carrying the bookkeeping you were doing by hand. No interval. No Kan operations. Nothing irreversible touched.*

---

### 1 · Where you're standing now
You shipped Rune v1. Proofs compound, the core is legible, deployment is a plugin. And there's one thing the substrate still makes you do by hand that the mathematics should be doing for you: you hand-roll setoids.

You want the integers — so you carry a pair and a relation and prove every function respects it. You want a multiset, a finite set — a list and a permutation relation, respect re-proved at every use. You want your discourse structures modulo an equivalence — and you're threading "respects R" proofs through code that should have stopped caring about representatives three definitions ago. It works. It's bookkeeping the type theory could carry. And it has no canonical forms, so your content-addressed cache can't even help you, because two equal things hash apart.

### 2 · What you've tried within Rune
Hand-rolled setoids: correct, tedious, no canonicalisation. Newtype wrappers that *assert* the quotient: a lie about equality the kernel will make you pay for. Or you looked at Cubical Agda's higher inductive types and recoiled — correctly — at standing up interval variables to get `ℤ`, when you have no use for the circle and never will. You don't need homotopy. You need a quotient.

### 3 · The idea
**Most of "I wish I had HITs" is "I wish I had quotients" — and quotients are UIP-safe.** `A / R` is a set-level construction; nothing about it asks the universe to be a groupoid, so it sits inside the equality you already have with no tension at all. And the funext and propositional extensionality v1 already gives you *computationally* are exactly what make quotients behave — they're what let the eliminator discharge "respects R" once and then forget it. So you don't swap the equality. You extend it. This is the boring version of the dream, and boring is the point: it's reachable precisely because it changes nothing irreversible.

### 4 · What you get
A quotient former `A / R`, point and path introduction, and an eliminator that takes a "respects R" premise and computes on representatives. Propositional truncation — the workhorse for "this exists, without naming the witness." Quotient-inductive definitions: types given by constructors *and* equations.

Underneath, it just fits. The path-constructors are proof-irrelevant under UIP, so there's nothing extra to compare and glued normalization doesn't choke — there's no interval to evaluate over. Quotients give you canonical behaviour where setoids gave you none, so the content-addressed cache finally earns its keep on quotiented values. The path-proofs erase, so a quotient compiles to its carrier and the shadow stays boring. The only place that needs real care is coverage for the new eliminators — more work, not new mathematics, and the K-friendly setting from v1 keeps it standard.

The higher truncations, the circle, synthetic homotopy — those are *not* here, and not deferred. They need groupoid structure UIP collapses. They live in v3's inner layer or nowhere in this core.

### 5 · Your new life
Quotients are values. You define `ℤ`, `ℚ`, multisets, finite sets, syntax-modulo-α, discourse-modulo-R — *once* — and reason about them without ever threading a respect-proof by hand again. Canonical behaviour where you had setoid sludge. The cache extends to cover it, so quotiented proofs compound like everything else. And the structures you actually study stop paying a representation tax they never should have owed.

---

*What v2 is not: not univalent (UIP still holds), no higher truncations, no circle, no synthetic homotopy — those need v3. No general user-facing HIT schema yet; the formers the book uses, and no more. And no change to the equality interface — extending it inside the interface is exactly what makes this a v2 and not a fork.*
