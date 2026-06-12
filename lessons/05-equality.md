# Lesson 5 — Equality

**Goal:** meet `Eq`, `refl`, `cast`, and `subst` — and understand the
sentence "a proof is a program whose type is the theorem."

This is the lesson where Rune becomes a proof assistant.

## 5.1 Propositions are types

Here's the idea the whole field is built on (it's called the **Curry–Howard
correspondence**, and it's one of the prettiest ideas in computer science):

> A logical proposition can be read as a *type* — the type of its proofs.
> The proposition is true exactly when the type has an element.

"A implies B" is the type `A -> B`: a proof is a function turning proofs of A
into proofs of B. "For all x, P(x)" is the dependent type `(x : A) -> P x` —
which you can now read, because you learned it in Lesson 2. And so on, down
the whole logic textbook. Proving a theorem and writing a program of a given
type are *literally the same activity*. The type checker, which decides
whether your program has its claimed type, is therefore a proof checker.

What we're missing is the atomic proposition of mathematics: *equality*. Rune
provides it: for any type `A` and values `x y : A`, there is a type

```
Eq A x y
```

— "the type of proofs that x equals y." And there's one fundamental way to
produce such a proof: `refl` ("reflexivity"), which proves `Eq A x x` —
anything equals itself.

```
rune> (refl : Eq U1 U U)
refl U : Eq U1 U U
```

(The ascription `(... : ...)` from Lesson 2 earns its keep: bare `refl`
doesn't say equal *to what*, so you check it against the intended type.)

The crucial subtlety: `refl` proves more than the literal `Eq A x x`. The
type checker compares the two sides **up to computation**. If the two sides
*compute* to the same thing, `refl` is a proof. Once we have numbers
(Lesson 6), `Eq Nat (add two two) four` will be provable by bare `refl`,
because `add two two` runs to `four` inside the type checker. Remember this;
it's why proofs in Rune are short.

## 5.2 Where do equality proofs live? `Prop`.

Ask Rune:

```
rune> :t Eq U1 U U
Prop
```

Not `U` — `Prop`. This is the extra universe promised in Lesson 3: the
universe of **propositions**. What distinguishes it: in `Prop`, *the proof
carries no information beyond existing*. Any two proofs of the same
proposition are considered equal — only *whether* it's proved matters, never
*how*. This is called **proof irrelevance**, and it's why (Lesson 9) proofs
can be completely deleted from compiled code without changing what the
program computes.

## 5.3 Equality of functions — for free

Here's where Rune does something most proof assistants can't. When are two
functions equal? Mathematics answers: when they agree on every input
("function extensionality"). Let's test it on two functions that are written
differently but obviously agree:

```
rune> idU : U -> U is fn (x : U) is x end end
defined idU
rune> idU' : U -> U is fn (x : U) is let y = x in y end end
defined idU'
```

In most proof assistants, `Eq (U -> U) idU idU'` would be... awkward.
Extensionality there is an *axiom* you bolt on — believed, not computed. Rune
uses **observational equality**, where `Eq` *computes on the structure of the
type*. The equality type at a function type literally *unfolds* to "pointwise
equal":

```
Eq (U -> U) f g    ~~computes to~~    (x : U) -> Eq U (f x) (g x)
```

So a proof of function equality is just... a function returning proofs:

```
rune> funext : Eq (U -> U) idU idU' is
...>   fn (x : U) is refl end
...> end
defined funext
```

A lambda of refls. Nothing postulated. The slogan: **funext is a reduction,
not an axiom.** (`let y = x in y` computes to `x`, so at each point both
sides are `x`, and `refl` closes it.)

## 5.4 Using equalities: `subst` and `cast`

A proof of `Eq A x y` should let you *do* something: namely, treat `x` and
`y` interchangeably. Two operators deliver this.

**`subst`** (substitution, also known as Leibniz's principle — "equals may
be substituted for equals"): given `p : Eq A x y`, a property
`P : A -> Prop`, and a proof of `P x`, you get a proof of `P y`:

```
subst A x y p P px   :   P y
```

This is the workhorse of every proof you'll write. As your first real
theorems, here are symmetry and transitivity of equality — read them slowly,
they're little puzzles:

```
rune> sym : (A : U) -> (x : A) -> (y : A) -> Eq A x y -> Eq A y x is
...>   fn (A : U) (x : A) (y : A) (p : Eq A x y) is
...>     subst A x y p (fn (z : A) is Eq A z x end) refl
...>   end
...> end
defined sym
```

The trick in `sym`: pick the property `P z := Eq A z x`. Then `P x` is
`Eq A x x` — that's `refl` — and `subst` along `p` carries it to `P y`,
which is `Eq A y x`. We rode our own equality.

```
rune> trans : (A : U) -> (x : A) -> (y : A) -> (z : A) -> Eq A x y -> Eq A y z -> Eq A x z is
...>   fn (A : U) (x : A) (y : A) (z : A) (p : Eq A x y) (q : Eq A y z) is
...>     subst A y z q (fn (w : A) is Eq A x w end) p
...>   end
...> end
defined trans
```

**`cast`** transports a *value* across an equality *of types*: given
`p : Eq U1 A B` and `x : A`, `cast A B p x : B`. If two types are provably
equal, data moves between them:

```
rune> transport : (A : U1) -> (B : U1) -> Eq U1 A B -> A -> B is
...>   fn (A : U1) (B : U1) (p : Eq U1 A B) (x : A) is cast A B p x end
...> end
defined transport
```

A design fact worth knowing (it's what makes this theory tick): `cast`
computes by looking at the *types* `A` and `B`, and **never inspects the
proof `p`**. Proofs being irrelevant isn't just philosophy — the machinery
literally never reads them.

One consequence, famous enough to have a name (UIP — "uniqueness of identity
proofs"): any two equality proofs are equal, and it's provable by `refl`:

```
rune> uip : Eq (Eq U1 U U) (refl U) (refl U) is refl end
defined uip
```

## Exercises

1. Try to prove `Eq (U -> U) idU idU` (both sides the *same* function) two
   ways: with bare `refl`, and with `fn (x : U) is refl end`. One of them
   fails. Which, and why does the error make sense?
2. Define `cong` ("congruence"): applying a function to both sides of an
   equality preserves it.
   ```
   cong : {A : U} -> {B : U} -> (f : A -> B) -> {x : A} -> {y : A}
          -> Eq A x y -> Eq B (f x) (f y)
   ```
   Hint: it's a one-line `subst`, same trick as `sym`. This will be your
   most-used lemma in Lessons 6 and 10.
3. Rewrite `sym` and `trans` with implicit `{A}`, `{x}`, `{y}`, `{z}` — by
   Lesson 4's rule, all of them are determined by the proof arguments. Keep
   these; the project uses them.
4. (Think.) `subst` demands `P : A -> Prop`. Why is it fine that we used
   `fn (z : A) is Eq A z x end` — what universe does `Eq A z x` live in?

## Solutions

1. The lambda form checks; bare `refl` fails:
   ```
   rune> p1 : Eq (U -> U) idU idU is refl end
   error: p1: refl checked against U -> Eq U (idU _) (idU _), which is not an equality type
   ```
   The error is the lesson in miniature: `Eq` at a function type *computes*
   to the pointwise Pi `(x : U) -> Eq U (idU x) (idU x)` before anything is
   checked against it. By the time `refl` arrives, the goal isn't an
   equality type anymore — it's a function type, and the only thing that
   checks against a function type is a function. Hence
   `fn (x : U) is refl end` succeeds. "Funext is a reduction" cuts both
   ways: you *get* extensionality for free, and you *must* prove function
   equalities pointwise.
2. ```
   cong : {A : U} -> {B : U} -> (f : A -> B) -> {x : A} -> {y : A} -> Eq A x y -> Eq B (f x) (f y) is
     fn {A : U} {B : U} (f : A -> B) {x : A} {y : A} (p : Eq A x y) is
       subst A x y p (fn (z : A) is Eq B (f x) (f z) end) refl
     end
   end
   ```
   Property: `P z := Eq B (f x) (f z)`. At `x` it's `refl`; `subst` carries
   it to `y`.
3. ```
   sym : {A : U} -> {x : A} -> {y : A} -> Eq A x y -> Eq A y x is
     fn {A : U} {x : A} {y : A} (p : Eq A x y) is
       subst A x y p (fn (z : A) is Eq A z x end) refl
     end
   end

   trans : {A : U} -> {x : A} -> {y : A} -> {z : A} -> Eq A x y -> Eq A y z -> Eq A x z is
     fn {A : U} {x : A} {y : A} {z : A} (p : Eq A x y) (q : Eq A y z) is
       subst A y z q (fn (w : A) is Eq A x w end) p
     end
   end
   ```
4. `Eq A z x : Prop` — equality types live in `Prop` regardless of where `A`
   lives. So the motive really is a function into `Prop`, exactly as `subst`
   demands.
