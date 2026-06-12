# Lesson 7 — Quantities

**Goal:** learn the 0/1/ω usage discipline — types that say not just *what*
an argument is, but *how many times* it may be used.

## 7.1 A third thing types can say

So far a binder `(x : A)` constrains what `x` *is*. Rune binders can carry
one more annotation — a **quantity** — constraining how often the function's
body may *use* `x`:

| annotation | name | meaning |
|---|---|---|
| `(0 x : A)` | erased | used **zero** times at runtime (types and proofs only) |
| `(1 x : A)` | linear | used **exactly once** |
| `(x : A)` | unrestricted (ω) | used any number of times — the default |

This is QTT — quantitative type theory. The elaborator literally *counts*
occurrences, and the count is checked like any other type error. Set up a
playground (load a file with `Nat` from Lesson 6, or redeclare it), and
let's get rejected a few times on purpose:

```
rune> f : (1 x : Nat) -> Nat is fn (1 x : Nat) is zero end end
error: f: binder x declared with quantity 1 (exactly once) is used 0 (erased)
```

Declared linear, never used. Rejected.

```
rune> g : (1 x : Nat) -> Nat is fn (1 x : Nat) is add x x end end
error: g: binder x declared with quantity 1 (exactly once) is used ω (unrestricted)
```

Declared linear, used twice. Rejected. The only thing a linear `Nat -> Nat`
can do with its argument is pass it along — once:

```
rune> lin : (1 x : Nat) -> Nat is fn (1 x : Nat) is x end end
defined lin
```

## 7.2 The subtle rule: uses multiply

Here's the one that surprises people. Surely `succ x` uses `x` once?

```
rune> ok : (1 x : Nat) -> Nat is fn (1 x : Nat) is succ x end end
error: ok: binder x declared with quantity 1 (exactly once) is used ω (unrestricted)
```

Rejected! Why: `succ`'s own argument binder is *unannotated* — ω. Passing
`x` into an ω slot means the receiver is free to use it any number of
times, so the cost of that call site is ω, not 1. **Argument positions
multiply by the receiving function's quantity.** Linear data may only flow
through linear-typed pipes:

```
rune> pipe : (1 f : (1 z : U) -> U) -> (1 x : U) -> U is
...>   fn (1 f : (1 z : U) -> U) (1 x : U) is f x end
...> end
defined pipe
```

`f` is itself linear ("use this function exactly once"), its parameter is
linear, and `x` rides through. Quantities are part of a function type's
identity — `(1 z : U) -> U` and `(z : U) -> U` are different types.

## 7.3 Zero: the erasure boundary

Linearity is the exotic half. The *important* half for this language is `0`.
An erased binder may be used freely in **types and proofs**, but never in
runtime code:

```
rune> eid : {0 A : U} -> A -> A is fn {0 A : U} (x : A) is x end end
defined eid
```

This is the polymorphic identity with its type argument marked erased: `A`
appears in the *type* `A -> A` (fine — type positions cost 0), but the
*body* returns `x` without touching `A`. At runtime, `A` will simply not
exist. Compare:

```
rune> use0 : (0 x : Nat) -> Nat is fn (0 x : Nat) is x end end
error: use0: binder x declared with quantity 0 (erased) is used 1 (exactly once)
```

You promised `x` was compile-time-only, then returned it. The counting
works in both directions.

Proofs sit naturally at 0 — they're checked, then never needed again:

```
rune> reflAt : (0 A : U) -> (x : A) -> Eq A x x is
...>   fn (0 A : U) (x : A) is refl x end
...> end
defined reflAt
```

`A` is used inside `refl x`'s type — cost 0 — and nowhere else.

## 7.4 Why bother?

Each quantity buys something concrete:

- **0** marks, *inside the type system*, exactly what the compiler may
  delete. In Lesson 9 you'll compile to JavaScript and see types and proofs
  verifiably absent from the output — that's the 0-fragment being erased.
  No heuristics, no "the optimizer usually gets it": the boundary between
  "exists to convince the checker" and "exists at runtime" is a *checked
  annotation*.
- **1** is a resource discipline. "Used exactly once" is what you want for
  file handles (can't close twice), money (can't spend twice),
  unique-ownership memory (this is the core idea behind Rust's borrow
  checker, met here in its pure form). v1 gives you the discipline; the
  systems-programming payoff is where the language is headed.
- **ω** is ordinary programming, and it's the default — you opt *into*
  strictness.

## Exercises

1. Without typing it: which of these check? Then type them.
   a. `k0 : (0 x : Nat) -> Nat is fn (0 x : Nat) is zero end end`
   b. `k1 : (1 x : Nat) -> Nat is fn (1 x : Nat) is zero end end`
2. `lid : (1 x : U) -> U is fn (1 x : U) is x end end` — a *linear,
   type-level* identity. Does it check? (It's not the same question as
   `eid`: here the type is the runtime value.)
3. Fix `ok` from 7.2 without changing its body `succ x` — change its type
   instead. What did you have to give up?
4. (Think.) Why is it fine that `addZeroRight` from Lesson 6 uses `n` in
   its *motive* and also recurses on it — wouldn't that be "twice"?

## Solutions

1. a. Checks — body ignores `x`, and 0 permits (requires!) zero runtime
      uses.
   b. Fails — `binder x declared with quantity 1 (exactly once) is used 0
      (erased)`. Linear means exactly once, not at most once.
2. It checks. `x : U` is a type used as a *value* (returned), once. Erasure
   (`0`) would forbid returning it; linearity is about the count, and the
   count is one.
3. Drop the linearity: `ok : (x : Nat) -> Nat is fn (x : Nat) is succ x end end`
   (or equivalently leave the type unannotated — ω). You gave up the
   guarantee that callers' linear data can flow through `ok`; since `succ`'s
   parameter is ω, that guarantee was never deliverable anyway.
4. Occurrences inside types and proofs are counted at 0 — the motive is
   type-level. Only runtime positions count toward 1/ω, and `n` is bound
   unannotated (ω) anyway, so any count is fine. The discipline only bites
   when *you* tighten a binder.
