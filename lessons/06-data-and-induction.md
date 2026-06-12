# Lesson 6 — Data and induction

**Goal:** define your own data types, compute with eliminators, and prove
your first theorem by induction.

This is the longest lesson and the center of the course. Take it in two
sittings if you need to: 6.1–6.4 (computing), then 6.5–6.6 (proving).

## 6.1 Declaring data

So far the only "values" around have been types and functions. Time to build
numbers — from absolutely nothing:

```
rune> data Nat : U is
...>   zero : Nat
...> | succ : Nat -> Nat
...> end
declared Nat zero succ NatElim
```

This declares: a new type `Nat : U`, and exactly two ways to make one —
`zero` is a `Nat`, and `succ n` ("successor", i.e. n+1) is a `Nat` whenever
`n` is. That's the complete definition of the natural numbers: every number
is some chain `succ (succ (... zero))`. Three is `succ (succ (succ zero))`.

```
rune> succ (succ zero)
succ (succ zero) : Nat
```

(Yes, the numerals are chunky. Rune v1 has no literal syntax — you'll feel
arithmetic in your hands, which for this course is a feature.)

Note the reply: `declared Nat zero succ NatElim`. Four names. The fourth,
`NatElim`, you did not write — Rune generated it. It's the entire engine of
this lesson, and we'll get to it in a moment. Two more declarations, for the
toolbox:

```
rune> data Bool : U is
...>   true : Bool
...> | false : Bool
...> end
declared Bool true false BoolElim

rune> data List : U -> U is
...>   nil : (A : U) -> List A
...> | cons : (A : U) -> A -> List A -> List A
...> end
declared List nil cons ListElim
```

`List` is parameterized by an element type: `List Nat`, `List Bool`, even
`List (List Nat)`. A list is either `nil A` (empty) or `cons A x rest` (an
element in front of a smaller list) — same shape of idea as `Nat`, with
cargo.

## 6.2 The missing feature: recursion

Now try to write addition. Your instinct from other languages:

```
add m n = if m == 0 then n else succ (add (m-1) n)     -- NOT RUNE
```

But Lesson 2 told you: Rune has no recursion — a function cannot call
itself. Recall why: if `add` could call itself freely, nothing would stop
`bad : T is bad end`, a "proof" of anything by infinite loop. The type
checker's authority as a proof checker rests on every program halting.

So how do you compute *anything* with `Nat`? With the generated `NatElim`.
Ask for its type:

```
rune> :t NatElim
(m : Nat -> U) -> m zero -> ((x : Nat) -> m x -> m (succ x)) -> (x : Nat) -> m x
```

Decode it piece by piece — it's four arguments:

| argument | type | meaning |
|---|---|---|
| `m` | `Nat -> U` | the **motive**: for each number `x`, what type you're building |
| base | `m zero` | the answer at `zero` |
| step | `(x : Nat) -> m x -> m (succ x)` | given `x` and *the answer at `x`*, the answer at `succ x` |
| target | `(x : Nat)` | which number to do this for |

If you squint, you've seen this before — it's **proof by induction** from
math class: prove it for 0; assuming it for k, prove it for k+1; conclude
it for every n. `NatElim` is induction, *as a function*. And used with a
boring motive (same type for every `x`), induction is just **recursion**:
the "answer at `x`" is the recursive call's result, handed to you as an
argument (`ih`, for *induction hypothesis*) instead of made by a self-call.

## 6.3 Addition, for real

```
rune> add : Nat -> Nat -> Nat is
...>   fn (m : Nat) (n : Nat) is
...>     NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
...>   end
...> end
defined add
```

Read it: recurse on `m`; motive is constantly `Nat`; when `m` is `zero` the
answer is `n`; when `m` is `succ k`, the answer is `succ` of the answer for
`k`. That is exactly `0 + n = n` and `(k+1) + n = (k + n) + 1`.

```
rune> add (succ zero) (succ (succ zero))
succ (succ (succ zero)) : Nat
```

1 + 2 = 3. Built from two constructors and an induction principle.
A couple more, to make the pattern muscle memory:

```
rune> double : Nat -> Nat is fn (n : Nat) is add n n end end
defined double

rune> not : Bool -> Bool is
...>   fn (b : Bool) is BoolElim (fn (x : Bool) is Bool end) false true b end
...> end
defined not

rune> length : (A : U) -> List A -> Nat is
...>   fn (A : U) (xs : List A) is
...>     ListElim A (fn (x : List A) is Nat end)
...>       zero
...>       (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
...>       xs
...>   end
...> end
defined length
```

`BoolElim` is "if-then-else" (two cases, no induction hypothesis — `Bool`
has no recursive structure). `ListElim` gives the cons case the head, the
tail, and the answer for the tail. Check `:t BoolElim` and `:t ListElim` and
match them against the table above; the pattern is identical every time.

## 6.4 Why eliminators instead of pattern matching?

Real talk: most languages with data types give you `match`/`switch` and
recursion, and compilers for proof assistants usually compile matching down
to eliminators behind the scenes. Rune v1 gives you the eliminators
directly. The payoff for the extra typing is severe and worth naming:

- **Coverage is by construction.** An eliminator takes exactly one case per
  constructor. You *cannot* forget a case — there's no syntax for it.
- **Totality is by construction.** The induction hypothesis only ever gives
  you the answer for the *structurally smaller* piece. You physically cannot
  write a non-terminating function, so no termination checker exists or is
  needed — there's nothing for it to reject.

In exchange: every function you write on data *is* a use of induction, which
means the line between "programming" and "proving" stops existing. Watch.

## 6.5 The first theorem

`add` treats its arguments asymmetrically: `add zero n` is `n` *by
computation* (it's the base case — try `add zero (succ zero)` and watch it
vanish), but `add n zero` is stuck — the recursion is on the *first*
argument, and `n` is opaque. So here's a genuine question: is
`add n zero = n`?

Of course it is. But "of course" isn't a proof. The claim, as a type:

```
(n : Nat) -> Eq Nat (add n zero) n
```

For all n, n + 0 = n. A proof is a program with this type — and the program
is induction, with the *motive doing the talking*. First, bring in `cong`
from Lesson 5's exercises (applying a function to both sides of an
equality):

```
rune> cong : {A : U} -> {B : U} -> (f : A -> B) -> {x : A} -> {y : A} -> Eq A x y -> Eq B (f x) (f y) is
...>   fn {A : U} {B : U} (f : A -> B) {x : A} {y : A} (p : Eq A x y) is
...>     subst A x y p (fn (z : A) is Eq B (f x) (f z) end) refl
...>   end
...> end
defined cong
```

Now the theorem:

```
rune> addZeroRight : (n : Nat) -> Eq Nat (add n zero) n is
...>   fn (n : Nat) is
...>     NatElim (fn (k : Nat) is Eq Nat (add k zero) k end)
...>       refl
...>       (fn (k : Nat) (ih : Eq Nat (add k zero) k) is cong succ ih end)
...>       n
...>   end
...> end
defined addZeroRight
```

Same `NatElim` as ever, but look at the motive: for each `k`, the type we're
building is `Eq Nat (add k zero) k` — *the statement of the theorem at `k`*.

- **Base case:** the goal is `Eq Nat (add zero zero) zero`. But `add zero
  zero` *computes* to `zero` (it's `add`'s base case!), so the goal becomes
  `Eq Nat zero zero` — and `refl` closes it. The type checker ran our
  program in the middle of checking our proof.
- **Step case:** we're handed `ih : Eq Nat (add k zero) k` and owe
  `Eq Nat (add (succ k) zero) (succ k)`. The left side computes to
  `succ (add k zero)` (that's `add`'s step case), so we owe
  `Eq Nat (succ (add k zero)) (succ k)` — which is exactly `ih` with `succ`
  applied to both sides: `cong succ ih`.

That's a complete, machine-checked induction proof. It compiles or it's
wrong. And because proofs are programs, you can *run* it:

```
rune> addZeroRight (succ (succ zero))
refl (succ (succ zero)) : Eq Nat (add (succ (succ zero)) zero) (succ (succ zero))
```

At a concrete number the whole proof collapses to `refl` — at numerals,
both sides just compute to the same thing, and the proof normalizes away.

## 6.6 What the motive is

The hardest skill in this lesson is choosing motives, so let's name what
happened. For *computing* (`add`, `length`), the motive was constant —
`fn (x : Nat) is Nat end` — and `NatElim` was a fold. For *proving*, the
motive was the theorem-as-a-function-of-`k`, and `NatElim` was induction.
There is no difference in the machinery. The motive is where you state what
you're building; the eliminator makes you deliver it at `zero` and preserve
it through `succ`. When a proof won't go through, the problem is almost
always the motive — too specific, too general, or quantifying the wrong
variable.

## Exercises

1. Define `pred : Nat -> Nat` (predecessor; `pred zero` should be `zero`).
   Note the step case gets `k` itself, not just `ih` — use it.
2. Define `orB : Bool -> Bool -> Bool` using `BoolElim`.
3. Define `mul : Nat -> Nat -> Nat` (multiplication). Hint:
   `(k+1) · n = n + k·n`, so the step case `add`s `n` to the induction
   hypothesis.
4. Define `isZero : Nat -> Bool`.
5. Prove `mulZeroRight : (n : Nat) -> Eq Nat (mul n zero) zero`. Hint:
   in the step case, work out what `mul (succ k) zero` computes to, and
   then what `add zero anything` computes to. The proof is shorter than
   you expect.
6. (Harder — the boss fight of this lesson.) Prove
   `addSucc : (m : Nat) -> (n : Nat) -> Eq Nat (add m (succ n)) (succ (add m n))`.
   Induct on `m`; both cases follow the `addZeroRight` shape exactly.

## Solutions

1. ```
   pred : Nat -> Nat is
     fn (n : Nat) is
       NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is k end) n
     end
   end
   ```
2. ```
   orB : Bool -> Bool -> Bool is
     fn (a : Bool) (b : Bool) is BoolElim (fn (x : Bool) is Bool end) true b a end
   end
   ```
   ("if a then true else b".)
3. ```
   mul : Nat -> Nat -> Nat is
     fn (m : Nat) (n : Nat) is
       NatElim (fn (x : Nat) is Nat end) zero (fn (k : Nat) (ih : Nat) is add n ih end) m
     end
   end
   ```
4. ```
   isZero : Nat -> Bool is
     fn (n : Nat) is
       NatElim (fn (x : Nat) is Bool end) true (fn (k : Nat) (ih : Bool) is false end) n
     end
   end
   ```
5. ```
   mulZeroRight : (n : Nat) -> Eq Nat (mul n zero) zero is
     fn (n : Nat) is
       NatElim (fn (k : Nat) is Eq Nat (mul k zero) zero end)
         refl
         (fn (k : Nat) (ih : Eq Nat (mul k zero) zero) is ih end)
         n
     end
   end
   ```
   The step case is *just `ih`*: the goal `Eq Nat (mul (succ k) zero) zero`
   computes — `mul (succ k) zero` → `add zero (mul k zero)` → `mul k zero` —
   to exactly the induction hypothesis's type. The type checker does all
   the work; you just hand it the proof it already wants.
6. ```
   addSucc : (m : Nat) -> (n : Nat) -> Eq Nat (add m (succ n)) (succ (add m n)) is
     fn (m : Nat) (n : Nat) is
       NatElim (fn (k : Nat) is Eq Nat (add k (succ n)) (succ (add k n)) end)
         refl
         (fn (k : Nat) (ih : Eq Nat (add k (succ n)) (succ (add k n))) is cong succ ih end)
         m
     end
   end
   ```
   Base: both sides compute to `succ n`. Step: both sides compute to `succ`
   of the ih's sides. Pattern recognition unlocked: motive = statement,
   base = `refl`, step = `cong succ ih`. You'll use it again in Lesson 10.
