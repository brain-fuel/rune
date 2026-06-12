# Lesson 9 — Codegen: the shadow

**Goal:** compile Rune to JavaScript, run it under node, and verify with
your own eyes that the types and proofs are gone.

## 9.1 Two lives of a program

Everything so far happened inside the checker. But a verified program you
can't *ship* is a museum piece. Rune's stance: the dependently typed program
is the **source of truth**, and what deploys is its **erasure** — the
"shadow": the same computation with everything compile-time-only stripped
out. Types, proofs, `cast`s, and every 0-quantity binder from Lesson 7 exist
to convince the checker; the shadow is what's left when conviction is no
longer needed.

This lesson works in files. Make `shadow.rune` (or reuse the repo's
`listings/ch04_data.rune`, which has all of this):

```
data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end

add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    NatElim (fn (x : Nat) is Nat end) n (fn (k : Nat) (ih : Nat) is succ ih end) m
  end
end

double : Nat -> Nat is fn (n : Nat) is add n n end end

four : Nat is double (succ (succ zero)) end
```

Run it (needs node installed):

```sh
$ rune run shadow.rune four
succ (succ (succ (succ zero)))
```

What happened: the file was **type checked first** (always — ill-typed files
are rejected, not compiled), erased, emitted as JavaScript, and executed
under node. To see the JavaScript itself:

```sh
$ rune emit shadow.rune four
```

## 9.2 Reading the shadow

The output is small enough to read in full. The highlights:

```js
const zero = ({tag: 0, name: "zero", args: []});
const succ = a0 => ({tag: 1, name: "succ", args: [a0]});
const NatElim = m => c0 => c1 => x => {
  switch (x.tag) {
  case 0: return c0;
  case 1: return c1(x.args[0])(NatElim(m)(c0)(c1)(x.args[0]));
  }
  throw new Error('impossible: unmatched constructor tag ' + x.tag);
};
```

Map it back to what you know:

- **Constructors** became tagged records — `succ (succ zero)` is just
  nested objects `{tag:1, args:[{tag:1, args:[{tag:0, args:[]}]}]}`.
- **Currying is real**: every function is `a => b => ...`, applied one
  argument at a time, exactly the Lesson 2 story.
- **The eliminator became a switch** — one case per constructor — **and
  ordinary recursion**. Here's a satisfying irony: the JavaScript `NatElim`
  calls itself! Recursion was never evil; *unverified* recursion was. Rune
  let you write only structurally decreasing recursion (via eliminators),
  checked it, and now compiles it to the fast, dangerous-looking thing,
  safely. The `throw` branch is provably dead — coverage was by
  construction — and exists only because JavaScript demands the case.
- The motive `m` is passed along but never used for computation — types
  are inert at runtime.

## 9.3 Watching proofs vanish

Now the real demonstration. Add a proof to your file (plus the `cong` lemma
it uses, from Lesson 6) and emit again:

```
addZeroRight : (n : Nat) -> Eq Nat (add n zero) n is
  fn (n : Nat) is
    NatElim (fn (k : Nat) is Eq Nat (add k zero) k end)
      refl
      (fn (k : Nat) (ih : Eq Nat (add k zero) k) is cong succ ih end)
      n
  end
end
```

In the emitted JavaScript you'll find:

```js
const cong = (A => (B => (f => (x => (y => (p => $unit))))));
```

`$unit` is the single inert token (`null`) that fills every erased position.
`cong` — a *proof-producing* function — compiles to a function that ignores
everything and returns nothing-in-particular. The `Eq` type, the `refl`, the
`subst`: no trace. There is no runtime representation of equality *at all*,
because by proof irrelevance (Lesson 5) a proof's only runtime content is
"yes." The proofs did their work at compile time; the shadow keeps only the
computation they certified.

(Notice erased arguments still get passed as `$unit` rather than removed —
erasure replaces values, it doesn't change function arities. Simple,
predictable, and a real JS engine inlines the difference away.)

## 9.4 The shape of trust

Step back and look at the full pipeline you've now traveled:

```
surface syntax → elaboration → checked core → (hash + certificate)
                                     ↓ erase
                                  erased IR → JavaScript
```

Everything left of the arrow is the verified, content-addressed, immutable
world of Lessons 1–8. Everything right of it is the disposable shadow —
regenerate it anytime; *never edit it* (the header says so). The rule the
compiler lives by: **mutate the shadow, not the source.** Optimizations, if
they come, happen to erased output; the checked core that proofs and hashes
refer to is never touched. Trust flows one direction.

## Exercises

1. `rune run shadow.rune add` — what prints? Why is it not an error, and
   what is node actually printing? (Hint: what *is* `add` after erasure?)
2. Emit `listings/ch04_data.rune` and find `BoolElim`. Why does its compiled
   form, unlike `NatElim`'s, not call itself?
3. Add `lengthAppend`-style data to taste, or simpler: predict what
   `not` compiles to before looking. Check yourself.
4. Take Lesson 7's `eid : {0 A : U} -> A -> A`, put it in a file with a use
   of it, and emit. How does the erased `A` show up at the call site?

## Solutions

1. It prints `<function>` (the `$show` helper's fallback). `add` erases to
   a perfectly good curried JS function; there's just nothing structural to
   print. Only constructor data prints as terms.
2. `Bool` has no recursive constructor arguments — no induction
   hypotheses — so its eliminator is a pure two-way switch:
   `c0` for `true`, `c1` for `false`, no self-call. The recursion in
   `NatElim` came from `succ : Nat -> Nat`'s recursive argument.
3. ```js
   const not = (b => BoolElim((x => $unit))($false)($true)(b));
   ```
   The motive erases to a function returning `$unit` (it was a function in
   the source, and erasure keeps arities), the two branches are the
   `false`/`true` constructor records (renamed `$false`/`$true` — `false`
   is a JS keyword), and dispatch is `BoolElim`.
4. The call site passes `$unit` where `A` would go — e.g. `eid($unit)(x)`.
   The binder survives as an ignored parameter; the *type* never exists.
