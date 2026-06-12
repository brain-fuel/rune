# Lesson 4 — Implicits and holes

**Goal:** stop writing arguments the type checker can figure out by itself.

## 4.1 The bookkeeping problem

By now you've noticed something tedious. Every polymorphic function drags its
type arguments around explicitly:

```
comp Nat Nat Nat succ double two      -- three type arguments of pure noise
```

The type checker *already knows* those types — it can see that `succ` goes
from `Nat` to `Nat`. Making a human repeat information the machine can infer
is bad interface design. Rune's fix is **implicit arguments**: braces instead
of parentheses in the type.

```
rune> id : {A : U1} -> A -> A is
...>   fn {A : U1} (x : A) is x end
...> end
defined id
```

`{A : U1}` means: "there is an argument `A` here, but the caller won't write
it — figure it out." Now:

```
rune> idU : U -> U is id end
defined idU
```

Look closely: the body is just `id`. No type argument anywhere. The
**elaborator** — the part of Rune that turns what you wrote into the fully
explicit core language — saw that `id` was being checked against `U -> U`,
inserted a hidden slot for `A`, and solved `A = U` from the context.

This is the right mental model for the elaborator: **you write the part a
human reader needs; it reconstructs the part a formal proof needs.** The
underlying core language has no implicits at all — every argument is filled
in. Implicits exist purely at the surface, for you.

## 4.2 Overriding, and holes

Sometimes you want to supply an implicit explicitly (the inference has
nothing to work with, or you want to be precise). Use braces at the call
site:

```
rune> viaOverride : U -> U is id {U} end
defined viaOverride
```

And the mirror-image feature: sometimes you want to leave an *explicit* thing
blank and make the elaborator earn its keep. An underscore `_` is a **hole**:

```
rune> viaHole : U -> _ is fn (x : U) is x end end
defined viaHole
rune> :t viaHole
U -> U
```

The hole stood for "some type — you tell me," and elaboration filled in `U`
from the body. Behind the scenes, holes and implicit arguments are the same
machinery: each becomes a **metavariable** — a placeholder `?m` — and the
elaborator collects constraints (equations the types must satisfy) and solves
them by unification. If you've solved a system of equations by substitution
in algebra class, you have the idea: the elaborator does that, on types.

When it *can't* solve — not enough information — you get an error rather
than a guess. Rune never fills a hole with something unjustified; a solved
metavariable is exactly as checked as something you wrote by hand. (After
elaboration, no metavariables survive: every definition is stored fully
explicit, or rejected.)

## 4.3 The payoff: composition that reads like math

Rewrite `comp` with implicits:

```
rune> compose : {A : U} -> {B : U} -> {C : U} -> (B -> C) -> (A -> B) -> A -> C is
...>   fn {A : U} {B : U} {C : U} (g : B -> C) (f : A -> B) (x : A) is g (f x) end
...> end
defined compose
```

Once we have `Nat` and functions on it (Lesson 6), you'll write
`compose succ double` and all three types will be inferred. g∘f, like the
textbook.

A style rule worth adopting now: **make an argument implicit when it's
determined by the other arguments.** `A`, `B`, `C` are pinned down the moment
the caller supplies `g` and `f` — perfect implicits. An argument that's *not*
determined (nothing else mentions it) makes a bad implicit; the elaborator
will be stuck, and callers will have to write `{...}` overrides anyway.

## Exercises

(Define `id` as above first, with `{A : U1}`.)

1. Predict: does plain `id`, typed alone at the REPL as a bare expression,
   elaborate? What could go wrong? Try it and read the result. Then try
   `id {U -> U}` and explain the difference.
2. Define `flip` from Lesson 2 with all three type arguments implicit, and
   check `:t flip` prints the type you declared.
3. Define `apply2 : {A : U} -> {B : U} -> (A -> B) -> A -> B` and verify that
   `apply2 idU` (using `idU` from above) elaborates, and say what `A` and `B`
   were solved to.
4. Write a definition where the hole `_` appears as the *whole* declared
   type: `mystery : _ is fn (x : U) is x end end`. Does it work? What did the
   elaborator decide `mystery`'s type was?

## Solutions

1. Bare `id` elaborates fine — with nothing pinning `A` down, it just stays
   general:
   ```
   rune> id
   fn {A : U} (x : U) is x end : {A : U1} -> A -> A
   ```
   No context, no pressure, no insertion. `id {U -> U}` supplies `A` by
   hand, so the implicit *is* consumed and you get the specialized identity:
   ```
   rune> id {U -> U}
   fn (x : U) is x end : (U -> U) -> U -> U
   ```
2. ```
   flip : {A : U} -> {B : U} -> {C : U} -> (A -> B -> C) -> B -> A -> C is
     fn {A : U} {B : U} {C : U} (f : A -> B -> C) (b : B) (a : A) is f a b end
   end
   ```
3. ```
   apply2 : {A : U} -> {B : U} -> (A -> B) -> A -> B is
     fn {A : U} {B : U} (f : A -> B) (x : A) is f x end
   end
   ```
   `apply2 idU` solves `A = U`, `B = U` from `idU : U -> U`.
4. It works: the elaborator infers the type of the body and fills the hole
   with `U -> U`. `:t mystery` confirms. Declared types that are all hole are
   legal but bad style — the type is the documentation; write it.
