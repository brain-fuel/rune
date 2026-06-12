# Lesson 3 — Universes

**Goal:** understand the universe ladder `U : U1 : U2 : …`, why it has to be
a ladder, and how to read "universe level mismatch" errors.

## 3.1 The problem with "the set of all sets"

In 1901 Bertrand Russell broke set theory with one question: consider the set
of all sets that don't contain themselves — does it contain itself? Either
answer contradicts itself. The repair, in logic and in type theory alike, is
**stratification**: collections live in layers, and a layer can only contain
things from layers strictly below it. "The set of all sets" simply doesn't
exist on any layer.

Rune inherits this directly. `U` is the type of "small" types. But `U` itself
is a type — so what's *its* type? If we said `U : U`, the system would
contain a Russell-style paradox (in type theory it's called Girard's paradox)
and the logic would collapse: every type would have an inhabitant, every
theorem would be "provable". So instead:

```
rune> U
U : U1
rune> U1
U1 : U2
rune> U2
U2 : U3
```

The ladder never closes the loop. This property — each universe lives in a
strictly bigger one — is called **predicativity**, and the slogan is
**no type-in-type**.

## 3.2 Feeling the wall

This isn't abstract; you hit the wall immediately. Define `id` from Lesson 2
and try to apply it to `U`:

```
rune> id : (A : U) -> A -> A is fn (A : U) (x : A) is x end end
defined id
rune> id U
error: type mismatch: expected U, got U1 (universe level mismatch: U1 vs U0)
```

`id` wants an `A : U`. But `U` has type `U1` — it's one floor up, too big to
be passed where a `U` is expected. (In errors, `U` is spelled `U0` — `U` is
just the friendly name for universe level 0.)

The fix is to write the identity one floor up:

```
rune> id1 : (A : U1) -> A -> A is fn (A : U1) (x : A) is x end end
defined id1
rune> id1 U
fn (x : U) is x end : U -> U
```

Wait — `id1 U` returned... the identity *on* `U`? Look again. `id1 U` is
`id1` applied to the type `U` only; it's still waiting for the `x : U`. The
REPL is showing you the half-applied function, normalized. This is a nice
little composition:

```
rune> idU : U -> U is id1 U end
defined idU
```

The general identity, specialized by application, *is* the specific identity.

## 3.3 Where does a function type live?

If `A : U` and `B : U`, where does `A -> B` live? In `U` — functions between
small types are small. The rule is: **a function type lands at the maximum of
the levels of its pieces**. So quantifying over something big lifts you up:

```
rune> prod : U -> U -> U1 is
...>   fn (A : U) (B : U) is (P : U) -> (A -> B -> P) -> P end
...> end
defined prod
```

`prod A B` says "for any type `P`, give me a way to turn an `A` and a `B`
into a `P`, and I'll give you a `P`." (This is secretly the type of *pairs*
of `A` and `B` — you'll prove that to yourself in Lesson 10.) The body
quantifies over all `P : U`, i.e. over the whole of `U` — and a type that
talks about *all* small types is not itself small. It lands in `U1`. That's
predicativity working: you can't define a small thing by quantifying over the
collection it belongs to.

One comfort: the ladder is **cumulative** — anything in `U` also counts as
being in `U1`, `U2`, and so on. Small things fit in big boxes:

```
rune> small : U1 is U -> U end
defined small
```

`U -> U` lives in `U` (both pieces are small), but checking it against `U1`
succeeds, because `U` sits inside `U1`.

## 3.4 How much does this matter day to day?

Less than this lesson makes it feel. Most working code lives entirely in `U`,
and you'll think about levels only when (a) you pass a universe itself as an
argument, or (b) you quantify over all types, like `prod` does. But when the
"universe level mismatch" error appears, you now know exactly what it means:
something is one floor too big for the slot it's being put in, and the fix is
to raise the slot (use `U1`) — never to wish for `U : U`.

> **Looking ahead:** there is one more universe, `Prop`, the universe of
> *propositions* — claims whose proofs carry no data. It sits at the bottom
> of the ladder and plays by intentionally different rules. It arrives with
> equality in Lesson 5.

## Exercises

1. Predict, then check with `:t`: the type of `U -> U1`. Explain the level.
2. Define `id2 : (A : U2) -> A -> A`, then use it to write `idU1 : U1 -> U1`
   in one line, imitating `idU`.
3. Why can't you define `everything : U is (A : U) -> A -> A end`? Try it,
   read the error, and say in one sentence what rule it violates.
4. (Think, don't type.) `id1 (U -> U)` — does this typecheck? What is its
   type if so?

## Solutions

1. `U -> U1` : `U2`. The domain is at level 1 (`U : U1`)… careful! The
   *domain* is the type `U`, which lives at level 1; the codomain `U1` lives
   at level 2; the arrow lands at the max: `U2`.
2. ```
   id2 : (A : U2) -> A -> A is fn (A : U2) (x : A) is x end end
   idU1 : U1 -> U1 is id2 U1 end
   ```
3. ```
   rune> everything : U is (A : U) -> A -> A end
   error: everything: type mismatch: expected U, got U1 (universe level mismatch: U1 vs U0)
   ```
   The body quantifies over all of `U`, so it lives in `U1` — a type defined
   by quantifying over all small types cannot itself be small (predicativity).
4. Yes. `U -> U` lives in `U`, and cumulativity lifts it into `U1`, so it's a
   legal argument for `id1`. The result has type `(U -> U) -> U -> U`.
