# Lesson 8 — Hashes and the proof cache

**Goal:** understand "content-addressed": what a definition's identity
really is, and why a proof, once checked, never needs checking again.

This lesson is different — almost no new syntax. It's about what Rune *is*
under the hood, and it's the idea the language is named for.

## 8.1 What's in a name?

In every mainstream language, code refers to other code **by name**. Names
are fragile: rename a function and everything breaks; have two libraries
define `length` and they collide; change a function's body and nothing that
calls it can tell.

Rune's core does something else. When your definition is elaborated, every
reference to another definition is replaced by the **BLAKE3 hash of that
definition's checked core** — a 256-bit fingerprint of its actual content.
Names exist only in the surface syntax, for humans. Watch the machinery with
`:core`, which shows the elaborated form of an expression:

```
rune> data Nat : U is zero : Nat | succ : Nat -> Nat end
declared Nat zero succ NatElim
rune> :core succ zero
(@b944033b476c @d66f345e5d47)
```

No `succ`, no `zero` — an application of one hash to another. The pretty
names are reattached only when printing for you.

## 8.2 Identity up to renaming

What exactly gets hashed? The *core* term, where bound variables aren't
names at all but **de Bruijn indices** — positions, like "the variable bound
2 binders up." A consequence you can check yourself:

```
rune> :hash fn (x : U) is x end
ed9ec0e1491b627003c8773d063d2834d50d9660804cb5119649a08556bc7293
rune> :hash fn (banana : U) is banana end
ed9ec0e1491b627003c8773d063d2834d50d9660804cb5119649a08556bc7293
```

Identical. Your variable names, your whitespace, your comments — none of it
is part of the definition's identity. Two people who write the identity
function, in any notation, have written *the same object*, and the system
knows it. (The jargon: alpha-equivalent terms are literally equal in the
core.) The same works per-definition from the command line:

```sh
$ rune hash listings/ch01_functions.rune
a67967103ad28b82f10f61cb5739e7e2c9a6faacd68dece41312b58b21b95de9  id
...
```

One subtlety, easy to state and worth remembering: hashing is structural,
**never up to computation**. `add two two` and `four` *normalize* to the
same value but are *different terms* with different hashes. Identity is
"same code," not "same behavior" — being too clever here (hashing modulo
computation) would mean running arbitrary programs just to compare names.

## 8.3 The proof cache

Now the payoff. Type checking is expensive — checking a proof can mean
normalizing big terms. Rune wraps every check in a cache keyed by content:

> **certificate** = (hash of the definition, hashes of the definitions it
> actually unfolded during checking)

When was the last time you saw a cache with **no invalidation logic**?
There's a famous joke that cache invalidation is one of the two hard
problems in computer science. Rune's certificate table is **append-only**
— nothing is ever invalidated, *because nothing can go stale*. A
content-hash names an immutable object: if you "change" a definition, you've
created a *new* definition with a *new* hash, and the old certificate still
truthfully describes the old object. "Recheck on change" becomes "look up a
different key."

The second component of the key is the clever part. While checking, the only
way Rune can look inside another definition's body is through a single
gateway (`store.Unfold`), and the gateway *logs every unfolding*. So each
certificate records exactly which definitions' bodies the check actually
depended on — not which names appear in the source, which is a coarser,
wronger answer. A check that compared `f` and `g` without ever opening
`h`'s body is certified independent of `h`'s body, *provably* — there's a
property test (the "Frame Lemma") asserting exactly that.

This is the sense in which **verification becomes a cache that never
recomputes**: a finished proof is a value; its identity is its hash; "is
this proved?" is a table lookup.

## 8.4 Why this matters beyond Rune

You've already used systems built on this idea: git names every commit by
content hash (change anything, new hash); package managers lock dependencies
by hash; content delivery networks dedupe by hash. Rune applies the idea one
level deeper — not to files of source text, but to *checked mathematical
objects*, so that the expensive judgment "this typechecks" attaches to the
hash and travels with it. Imagine `rune` pulling a library and *not
rechecking its proofs* because the certificates' hashes match objects you
already trust. That's the road this design is on.

## Exercises

1. Define `idA : (A : U) -> A -> A is fn (A : U) (x : A) is x end end` and
   `idB : (B : U) -> B -> B is fn (B : U) (y : B) is y end end`. Predict:
   same hash or different? Check with `:hash idA` / `:hash idB`.
2. `:hash (fn (x : U) is x end) U` versus `:hash U` — same or different, and
   why? (Careful: this is the 8.2 subtlety.)
3. With Lesson 6's definitions loaded (`:load listings/ch04_data.rune` from
   the repo root works), compare `:core double` with
   `:core fn (n : Nat) is add n n end`. Explain everything you see in the
   second output — there are three different kinds of thing in it.
4. (Think.) Suppose definitions `f` and `g` both pass checking, and checking
   `f` never unfolded `g`. You now write a new, different `g'`. Why is `f`'s
   certificate still valid with `g'` in the picture?

## Solutions

1. **Same hash.** `A`/`B`, `x`/`y` are bound names — gone in the core. The
   two definitions are the same object.
2. **Different.** `(fn (x : U) is x end) U` is an application node (which
   would *normalize* to `U`, but hashing never normalizes); `U` is a
   universe node. Different structure, different hash.
3. ```
   rune> :core double
   @b93c877f6d4e
   rune> :core fn (n : Nat) is add n n end
   (λ. ((@9d52ecb7c289 #0) #0))
   ```
   The name `double` *is* just a reference — one hash. The inline lambda
   shows the three citizens of the core: `λ` (a binder, with no name — the
   name was surface decoration), `@9d52ecb7c289` (the reference to `add`,
   by content hash), and `#0` (a de Bruijn index: "the variable bound 0
   binders up", i.e. `n`, twice). References are links, not copies — and
   since hashes contain the hashes of what they reference, one fingerprint
   pins down an entire dependency tree (a Merkle tree, same trick as a git
   commit hash).
4. `f`'s certificate is keyed by `f`'s hash and the hashes of what it
   *unfolded* — `g` isn't in that set, and `f`'s own hash didn't change
   (`g'` is a new object; `f` still references whatever it referenced).
   Nothing in the key moved, so the cached judgment still applies. That is
   the Frame Lemma, used as an alibi.
