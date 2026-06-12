# Lesson 2 — Functions

**Goal:** write functions, apply them, and meet the *dependent function type*
— the single feature that makes everything else in this language possible.

## 2.1 Lambdas

A function in Rune is written with `fn`:

```
fn (x : U) is x end
```

That's "the function taking an `x` of type `U` and returning `x`" — the
identity function on types. Try it:

```
rune> fn (x : U) is x end
fn (x : U) is x end : U -> U
```

Its type is `U -> U`: takes a `U`, returns a `U`. The arrow `->` is the
function type former, exactly like the `f : A → B` notation from math class.

Apply a function by writing it next to its argument — no parentheses, no
comma:

```
rune> (fn (x : U) is x end) U
U : U1
```

Multiple parameters chain:

```
rune> fn (x : U) (y : U) is x end
fn (x : U) (y : U) is x end : U -> U -> U
```

Two things to internalize about the notation, because every functional
language works this way:

- **Application is left-associative:** `f x y` means `(f x) y`.
- **Arrows are right-associative:** `U -> U -> U` means `U -> (U -> U)`.

These two rules are partners. A two-argument function is *really* a function
that takes the first argument and returns a function waiting for the second
(this is called **currying**). So `f x y` first computes `f x` — a function —
then hands it `y`. You can see currying directly:

```
rune> (fn (x : U) (y : U) is x end) U
fn (y : U) is U end : U -> U
```

Half-applied, and the result is itself a function. Nothing is special about
"the last argument".

## 2.2 The dependent function type

Here's where Rune leaves ordinary languages behind. Say you want the identity
function that works for **every** type, not just `U`. You want to take a type
`A`, then a value of *that type*, and return it. The type of such a function
needs the **name** of the first argument to appear in the rest of the type:

```
(A : U) -> A -> A
```

Read it: "for any type `A`, a function from `A` to `A`." The first arrow's
domain is *named* (`A : U`), and that name is used later in the type. This is
called a **dependent function type** (or "Pi type") — the type of the result
*depends on the value* of the argument.

If you've seen the math notation "∀A. A → A", this is exactly that, made
executable. Define it:

```
rune> id : (A : U) -> A -> A is
...>   fn (A : U) (x : A) is x end
...> end
defined id
```

This single definition works at every type. Once we have `Nat` (Lesson 6),
`id Nat zero` will give back `zero`; right now we can apply it to types of
type `U`:

```
rune> id (U -> U) (fn (x : U) is x end)
fn (x : U) is x end : U -> U
```

Pause on what just happened, because it's the core trick of the whole
language: the first argument `U -> U` was a **type**, passed as an ordinary
**value**, and it determined the type of the second argument. Types are
first-class. Functions can compute with them.

Two more classics, which you should type in now — they'll be load-bearing
later:

```
rune> const : (A : U) -> (B : U) -> A -> B -> A is
...>   fn (A : U) (B : U) (x : A) (y : B) is x end
...> end
defined const

rune> comp : (A : U) -> (B : U) -> (C : U) -> (B -> C) -> (A -> B) -> A -> C is
...>   fn (A : U) (B : U) (C : U) (g : B -> C) (f : A -> B) (x : A) is g (f x) end
...> end
defined comp
```

`const` ignores its second value; `comp` is function composition, g∘f. Notice
how much the *types alone* tell you. `(A : U) -> (B : U) -> A -> B -> A` has
essentially one implementation: where else could the `A` come from? This is a
recurring experience in typed programming — the type is a specification, and
sometimes it's so tight the body writes itself.

## 2.3 `let` and `seq`

Local names, when an expression gets unwieldy:

```
rune> let f = fn (x : U) is x end in f U
U : U1
```

`let name = expr in body` — the `in` is mandatory. For a chain of bindings,
`seq` is nicer (one binding per line, or separated by `;`):

```
rune> seq let a = U; let b : U1 = a; b end
U : U1
```

`seq` is pure sugar: it desugars to nested `let … in`. There's no "do this,
then that" in Rune — no side effects, no statements — so the only thing
sequencing can mean is "name these intermediate values, in order."

One more piece of syntax: a **type ascription** `(e : T)` checks `e` against
`T` inline:

```
rune> (fn (x : U) is x end : U -> U)
fn (x : U) is x end : U -> U
```

This looks decorative now, but in Lesson 5 it becomes essential — some
expressions can only be checked when you *tell* Rune what type you intend.

## 2.4 Everything is total

A thing Rune does **not** have: loops, recursion you write yourself, or any
way to make a program run forever. Every Rune program terminates. This sounds
like a limitation (and it does cost some expressiveness), but it's the price
of the language's central promise: a type is a mathematical claim and a
program is its proof. A "proof" that loops forever proves nothing —
in a language with infinite loops you could "prove" any claim `T` by writing
`loop : T is loop end`. Totality is what makes the type checker a *proof*
checker. How Rune still expresses interesting computation without raw
recursion is the punchline of Lesson 6.

## Exercises

1. Define `apply : (A : U) -> (B : U) -> (A -> B) -> A -> B` — it takes a
   function and an argument and applies it. (Trivial on purpose: it's about
   reading the type.)
2. Define `flip : (A : U) -> (B : U) -> (C : U) -> (A -> B -> C) -> B -> A -> C`
   which swaps the argument order of a two-argument function.
3. Define `twice : (A : U) -> (A -> A) -> A -> A` which applies a function
   two times. Check: `twice U (fn (x : U) is x end) U` should print `U : U1`.
4. Without typing it: what is the type of `comp U U U`? Then check with `:t`.

## Solutions

1. ```
   apply : (A : U) -> (B : U) -> (A -> B) -> A -> B is
     fn (A : U) (B : U) (f : A -> B) (x : A) is f x end
   end
   ```
2. ```
   flip : (A : U) -> (B : U) -> (C : U) -> (A -> B -> C) -> B -> A -> C is
     fn (A : U) (B : U) (C : U) (f : A -> B -> C) (b : B) (a : A) is f a b end
   end
   ```
3. ```
   twice : (A : U) -> (A -> A) -> A -> A is
     fn (A : U) (f : A -> A) (x : A) is f (f x) end
   end
   ```
4. `(U -> U) -> (U -> U) -> U -> U` — composition specialized so all three
   types are `U`.
