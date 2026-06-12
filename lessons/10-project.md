# Lesson 10 — Project: a verified library

**Goal:** build `runelib` — a small standard library where the documentation
is theorems and the theorems are checked. Everything you've learned, used at
once, in one file you keep.

No new concepts in this lesson. That's the point: this is the lesson where
you find out the previous nine took.

## 0. Ground rules

Work in a file, `runelib.rune`. Develop with the loop:

```
rune> :reset
rune> :load runelib.rune
```

after each change (`:reset` first — `:load` adds to the session, and you
want a clean read of your file every time). A definition that breaks will
tell you its name and why. Keep `rune fmt runelib.rune` in your pocket: if
it prints your file back, the file at least parses.

The milestones below each end with a checkpoint. Don't look at
[`solutions/runelib.rune`](solutions/runelib.rune) until you're through —
or at minimum, fight each milestone for twenty minutes first. Getting stuck,
forming a theory, and getting unstuck is the entire skill; the solution file
can't teach it.

## Milestone 1 — Data (Lesson 6)

Declare `Nat`, `Bool`, and `List` exactly as in Lesson 6. Then define, with
eliminators:

- `add : Nat -> Nat -> Nat` and `mul : Nat -> Nat -> Nat`
- `isZero : Nat -> Bool`
- some numerals to test with: `two`, `three`, and `six : Nat is mul two three end`

**Checkpoint:**

```
rune> six
succ (succ (succ (succ (succ (succ zero))))) : Nat
rune> isZero six
false : Bool
```

## Milestone 2 — The proof toolkit (Lesson 5)

The four little lemmas every equality proof leans on. Define them with
implicit arguments (Lesson 4) — you'll be writing these a lot in Milestone
3, and `sym p` reads better than `sym Nat x y p`:

```
cong  : {A : U} -> {B : U} -> (f : A -> B) -> {x : A} -> {y : A} -> Eq A x y -> Eq B (f x) (f y)
sym   : {A : U} -> {x : A} -> {y : A} -> Eq A x y -> Eq A y x
trans : {A : U} -> {x : A} -> {y : A} -> {z : A} -> Eq A x y -> Eq A y z -> Eq A x z
```

All three are one-line `subst`s; you proved them in Lesson 5's exercises.

**Checkpoint:** `:t cong` prints your declared type, and
`cong succ (refl : Eq Nat zero zero)` elaborates.

## Milestone 3 — The arithmetic theorems

Now the library proves things about itself. In increasing order of pain:

1. `addZeroRight : (n : Nat) -> Eq Nat (add n zero) n` — Lesson 6, but
   re-derive it from memory: motive = statement, base = `refl`,
   step = `cong succ ih`.
2. `addSucc : (m : Nat) -> (n : Nat) -> Eq Nat (add m (succ n)) (succ (add m n))`
   — same skeleton, induct on `m`.
3. `addAssoc : (a : Nat) -> (b : Nat) -> (c : Nat) ->
   Eq Nat (add (add a b) c) (add a (add b c))` — induct on `a`. Both other
   arguments just ride along. Still the same skeleton.
4. **The boss: commutativity.**
   `addComm : (m : Nat) -> (n : Nat) -> Eq Nat (add m n) (add n m)`.
   This one is *not* just the skeleton, which is why it's the boss. Induct
   on `m` and look at what you owe:
   - **Base:** `Eq Nat n (add n zero)` — wait, `add zero n` computes to `n`,
     but the right side is stuck. You already have a theorem about
     `add n zero`. Which direction does it point? (Lesson 5 gave you the
     tool for turning a proof around.)
   - **Step:** you have `ih : Eq Nat (add k n) (add n k)` and owe
     `Eq Nat (succ (add k n)) (add n (succ k))`. Chase it in two hops:
     `succ (add k n)` equals `succ (add n k)` (why?), which equals
     `add n (succ k)` (which theorem, pointed which way?). Two hops means
     one `trans`.

**Checkpoint:** proofs *run* (Lesson 6.5). At concrete numbers they should
collapse to `refl`:

```
rune> addComm two three
refl (succ (succ (succ (succ (succ zero))))) : Eq Nat (add two three) (add three two)
```

## Milestone 4 — Verified lists

- `append : (A : U) -> List A -> List A -> List A` (use `ListElim` on the
  first list; the nil case returns the second).
- `length : (A : U) -> List A -> Nat` (Lesson 6).
- The theorem tying them to arithmetic — the library's flagship:

```
lengthAppend : (A : U) -> (xs : List A) -> (ys : List A) ->
    Eq Nat (length A (append A xs ys)) (add (length A xs) (length A ys))
```

"The length of a concatenation is the sum of the lengths." Induct on `xs`
(via `ListElim`, motive over `zs : List A`). Both cases are old friends: the
nil case computes on both sides to `length A ys` — and the cons case is
`cong succ ih`, *again*. By now you should suspect (correctly) that half of
all structural proofs are `cong succ ih`.

**Checkpoint:**

```
rune> lengthAppend Nat (cons Nat two (nil Nat)) (cons Nat three (nil Nat))
refl (succ (succ zero)) : Eq Nat ...
```

## Milestone 5 — Pairs from nothing (Lessons 2–3)

Rune v1 has no built-in pair type. Lesson 3's `prod` hinted that none is
needed — a pair is what you can *do* with one, namely give both parts to
any consumer:

```
Pair : U -> U -> U1 is
  fn (A : U) (B : U) is (P : U) -> (A -> B -> P) -> P end
end
```

Define (implicits on `A`, `B`):

- `pair : {A : U} -> {B : U} -> A -> B -> Pair A B` — store two values by
  building the function that hands them to any `k`.
- `fst : {A : U} -> {B : U} -> Pair A B -> A` — *use* the pair: pick
  `P := A` and pass the consumer that keeps the first part.
- `snd : {A : U} -> {B : U} -> Pair A B -> B`.

**Checkpoint:** `fst (pair two three)` is `two`; `snd (pair two three)` is
`three`. Then sit for a second with how strange this is: a *data structure*
built out of nothing but functions and a universe. (This is called a Church
encoding. Note where `Pair A B` lives, and make sure you can say why —
Lesson 3.)

## Milestone 6 — Ship it (Lessons 8–9)

Your library is done. Now treat it like an artifact:

1. `rune fmt runelib.rune` — the canonical form of your file.
2. `rune hash runelib.rune` — the identity of every definition you wrote.
   This list of hashes *is* your library, in the content-addressed sense:
   anyone, anywhere, who writes these same objects gets these same hashes.
3. `rune emit runelib.rune six` — read your shadow. Find your proofs:
   `addComm` compiles to a function that builds `$unit`s out of `$unit`s.
   Every theorem you sweated over in Milestone 3 is runtime-free; `add` and
   `append` compile to clean curried switches.
4. `rune run runelib.rune six` →
   `succ (succ (succ (succ (succ (succ zero)))))`.

Verified at compile time. Vanished at runtime. That's the whole language in
one sentence — and now it's *your* sentence: you built a library where
"the docs say length is additive" is not a comment that can rot but a
checked object with a hash.

## Where to go next

- **Harder proofs in pure v1:** `mulComm` (commutativity of `mul` — needs
  two auxiliary lemmas; plan them on paper first), `reverse : (A : U) ->
  List A -> List A` and the theorem `Eq Nat (length A (reverse A xs))
  (length A xs)`.
- **Read the listings:** `listings/` in the repo is the book-in-progress
  the language was built to run, chapter by chapter — you can now read all
  five.
- **Read the design docs:** `ref_docs/` in the repo. You've *used*
  everything they specify; reading the "why" behind a system you know is
  how language designers are made.
