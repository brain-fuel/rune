# List/tree structure trio (ch545/ch546/ch547) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add three new proven-tier corpus listings broadening the verified stdlib beyond the Tree<->List homomorphism family: ch545 (take/drop split-join on lists), ch546 (the tree catamorphism recovers the direct size), ch547 (zip/unzip round-trip on pair-lists).

**Architecture:** Three independent self-contained `.rune` listings, one per task (corpus convention: each file re-derives its own toolkit). Each proves one new theorem by structural induction and ships runnable witnesses. They share no code and can be implemented, reviewed, and committed independently.

**Tech Stack:** Rune (`goforge.dev/rune/v3`), eliminator-style dependently-typed proofs. Each listing is gated automatically by `harness/listings_test.go::TestListingsElaborateAndCheck` (`os.ReadDir`s `listings/`, elaborates+checks every `*.rune`; no manifest registration).

## Global Constraints

- Kernel FROZEN: corpus listings only. No `core/`, `store/`, `elaborate/`, `codegen/` change. No hash-format change.
- DEFINITION ORDER MATTERS: rune resolves names top-to-bottom. Each listing's defs are already in a verified working order in the Step 1 blocks -- transcribe verbatim, do not reorder (a helper that references `append`/`zip`/etc. must come after it).
- Each listing is GENUINELY NEW: the existing corpus tree family is ch244/246/267/290/539/540/541/542/543/544. ch545 (take/drop), ch546 (a generic `foldTree` catamorphism), and ch547 (zip/unzip over a pair-list) are new structures/operations not in that family.
- Self-contained: each file re-derives every helper locally; no cross-chapter imports.
- `builtin nat Nat zero succ` immediately after each `Nat` declaration.
- No em-dashes / en-dashes anywhere; use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- There is NO `rune check` subcommand. Use `rune emit FILE NAME` to elaborate+check; `rune run FILE NAME` to execute a closed value.
- Every listing was verified end to end before the plan was written (each `emit`s clean and its witnesses run to the stated values). Transcribe verbatim.

---

### Task 1: ch545 -- take/drop split-join

**Files:**
- Create: `listings/ch545_list_take_drop.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `append`, `congCons`, `take`, `drop`, theorem `takeDropAppend`, witnesses `len`/`sample`/`lenTake`/`lenJoin`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch545_list_take_drop.rune` with EXACTLY this content (verbatim):

```
-- Chapter 545 -- take/drop split-join: append (take n xs) (drop n xs) = xs (proven tier).
--
-- take n keeps the first n elements, drop n discards them; both recurse on n AND the list
-- (a NatElim returning a NatList -> NatList, then a NatListElim inside). Splitting a list at
-- any index and rejoining the pieces recovers the original:
--
--   takeDropAppend : (n : Nat) -> (xs : NatList) -> Eq NatList (append (take n xs) (drop n xs)) xs
--
-- proven by induction on n with a motive that quantifies xs (the succ case applies the IH at
-- the tail). The fundamental list-splitting law every verified collections library needs.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data NatList : U is nnil : NatList | ncons : Nat -> NatList -> NatList end

append : NatList -> NatList -> NatList is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is NatList end) ys (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons h ih end) xs end end

congCons : (h : Nat) -> (xs : NatList) -> (ys : NatList) -> Eq NatList xs ys -> Eq NatList (ncons h xs) (ncons h ys) is
  fn (h : Nat) (xs : NatList) (ys : NatList) (e : Eq NatList xs ys) is
    subst NatList xs ys e (fn (z : NatList) is Eq NatList (ncons h xs) (ncons h z) end) (refl (ncons h xs)) end end

take : Nat -> NatList -> NatList is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is NatList -> NatList end)
      (fn (xs : NatList) is nnil end)
      (fn (k : Nat) (ih : NatList -> NatList) is
         fn (xs : NatList) is
           NatListElim (fn (w : NatList) is NatList end) nnil (fn (h : Nat) (t : NatList) (rec : NatList) is ncons h (ih t) end) xs end end)
      n end end

drop : Nat -> NatList -> NatList is
  fn (n : Nat) is
    NatElim (fn (x : Nat) is NatList -> NatList end)
      (fn (xs : NatList) is xs end)
      (fn (k : Nat) (ih : NatList -> NatList) is
         fn (xs : NatList) is
           NatListElim (fn (w : NatList) is NatList end) nnil (fn (h : Nat) (t : NatList) (rec : NatList) is ih t end) xs end end)
      n end end

-- THEOREM: append (take n xs) (drop n xs) = xs, by induction on n (motive quantifies xs).
takeDropAppend : (n : Nat) -> (xs : NatList) -> Eq NatList (append (take n xs) (drop n xs)) xs is
  fn (n : Nat) is
    NatElim (fn (k : Nat) is (xs : NatList) -> Eq NatList (append (take k xs) (drop k xs)) xs end)
      (fn (xs : NatList) is refl xs end)
      (fn (k : Nat) (ih : (xs : NatList) -> Eq NatList (append (take k xs) (drop k xs)) xs) is
         fn (xs : NatList) is
           NatListElim (fn (w : NatList) is Eq NatList (append (take (succ k) w) (drop (succ k) w)) w end)
             (refl nnil)
             (fn (h : Nat) (t : NatList) (rec : Eq NatList (append (take (succ k) t) (drop (succ k) t)) t) is
                congCons h (append (take k t) (drop k t)) t (ih t) end)
             xs end end)
      n end end

-- witnesses: split [1,2,3] at 2 -> take=[1,2] (len 2), rejoin -> [1,2,3] (len 3).
len : NatList -> Nat is
  fn (xs : NatList) is NatListElim (fn (w : NatList) is Nat end) zero (fn (h : Nat) (t : NatList) (ih : Nat) is succ ih end) xs end end
sample : NatList is ncons (succ zero) (ncons (succ (succ zero)) (ncons (succ (succ (succ zero))) nnil)) end
lenTake : Nat is len (take (succ (succ zero)) sample) end
lenJoin : Nat is len (append (take (succ (succ zero)) sample) (drop (succ (succ zero)) sample)) end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch545_list_take_drop.rune takeDropAppend`
Expected: PASS -- JS tail includes `const takeDropAppend = $unit;` and `console.log($show(takeDropAppend));`. No `expected ... got ...`, no `not in scope`, exit 0.

- [ ] **Step 3: Verify the split-join computes**

Run: `go run ./cmd/rune run listings/ch545_list_take_drop.rune lenTake`
Expected: `2` (take 2 of a 3-element list)

Run: `go run ./cmd/rune run listings/ch545_list_take_drop.rune lenJoin`
Expected: `3` (split-then-rejoin recovers the full 3-element list)

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch545_list_take_drop.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch545_list_take_drop.rune`.

- [ ] **Step 5: Commit**

```bash
git add listings/ch545_list_take_drop.rune
git commit -m "$(printf 'feat(listings): ch545 take/drop split-join law\n\nappend (take n xs) (drop n xs) = xs by induction on n with an xs-quantified\nmotive (the succ case applies the IH at the tail). take/drop both recurse\non n and the list. The fundamental list-splitting law. Proof-only plus\nwitnesses (take 2 of [1,2,3] has len 2, rejoin has len 3).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: ch546 -- the tree catamorphism recovers the direct size

**Files:**
- Create: `listings/ch546_tree_catamorphism.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `natAdd`, `foldTree`, `sizeF`, `size`, toolkit `cong`/`transEq`/`cong2`, theorem `sizeFoldEq`, witnesses `sample`/`sF`/`sD`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch546_tree_catamorphism.rune` with EXACTLY this content (verbatim):

```
-- Chapter 546 -- the tree catamorphism recovers the direct size (proven tier).
--
-- foldTree is the GENERIC fold (catamorphism) over a Nat-keyed binary tree: a leaf value z
-- and a node combiner k. Instantiating it with the size combiner gives sizeF; proving it
-- equals the directly-recursive size shows the catamorphism is adequate -- the generic
-- recursion scheme computes the same thing as the hand-written eliminator:
--
--   sizeFoldEq : (t : Tree) -> Eq Nat (sizeF t) (size t)
--
-- by structural induction (node step: cong succ over cong2 natAdd on the two IHs). foldTree
-- is the reusable scheme behind ch539's count and ch543's sum; this pins its correctness.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Tree : U is leaf : Tree | node : Tree -> Nat -> Tree -> Tree end

cong : (A : U) -> (B : U) -> (f : A -> B) -> (x : A) -> (y : A) -> Eq A x y -> Eq B (f x) (f y) is
  fn (A : U) (B : U) (f : A -> B) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq B (f x) (f z) end) refl end end

transEq : (A : U) -> (x : A) -> (y : A) -> (z : A) -> Eq A x y -> Eq A y z -> Eq A x z is
  fn (A : U) (x : A) (y : A) (z : A) (p : Eq A x y) (q : Eq A y z) is
    subst A y z q (fn (w : A) is Eq A x w end) p end end

cong2 : (A : U) -> (B : U) -> (C : U) -> (f : A -> B -> C) -> (a : A) -> (a2 : A) -> (b : B) -> (b2 : B) ->
        Eq A a a2 -> Eq B b b2 -> Eq C (f a b) (f a2 b2) is
  fn (A : U) (B : U) (C : U) (f : A -> B -> C) (a : A) (a2 : A) (b : B) (b2 : B) (pa : Eq A a a2) (pb : Eq B b b2) is
    transEq C (f a b) (f a2 b) (f a2 b2)
      (cong A C (fn (x : A) is f x b end) a a2 pa) (cong B C (fn (y : B) is f a2 y end) b b2 pb) end end

natAdd : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end end

-- the GENERIC catamorphism: fold a tree to a B via z (leaf) and k (node combiner).
foldTree : (B : U) -> B -> (B -> Nat -> B -> B) -> Tree -> B is
  fn (B : U) (z : B) (k : B -> Nat -> B -> B) (t : Tree) is
    TreeElim (fn (x : Tree) is B end) z
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : B) (ihr : B) is k ihl x ihr end) t end end

-- size as a catamorphism instance.
sizeF : Tree -> Nat is
  fn (t : Tree) is foldTree Nat zero (fn (l : Nat) (x : Nat) (r : Nat) is succ (natAdd l r) end) t end end

-- size by direct structural recursion.
size : Tree -> Nat is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Nat end) zero
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Nat) (ihr : Nat) is succ (natAdd ihl ihr) end) t end end

-- THEOREM: the catamorphism instance equals the direct definition, by induction on t.
sizeFoldEq : (t : Tree) -> Eq Nat (sizeF t) (size t) is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Eq Nat (sizeF x) (size x) end)
      refl
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq Nat (sizeF l) (size l))
          (ihr : Eq Nat (sizeF r) (size r)) is
         cong Nat Nat succ (natAdd (sizeF l) (sizeF r)) (natAdd (size l) (size r))
           (cong2 Nat Nat Nat natAdd (sizeF l) (size l) (sizeF r) (size r) ihl ihr) end)
      t end end

-- witnesses: 3-node tree; both the fold and the direct recursion give 3.
sample : Tree is node (node leaf (succ zero) leaf) (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf) end
sF : Nat is sizeF sample end
sD : Nat is size sample end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch546_tree_catamorphism.rune sizeFoldEq`
Expected: PASS -- JS tail includes `const sizeFoldEq = $unit;`. No type error, exit 0.

- [ ] **Step 3: Verify both definitions compute and agree**

Run: `go run ./cmd/rune run listings/ch546_tree_catamorphism.rune sF`
Expected: `3`

Run: `go run ./cmd/rune run listings/ch546_tree_catamorphism.rune sD`
Expected: `3` (the generic fold and the direct recursion both count 3 nodes).

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch546_tree_catamorphism.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch546_tree_catamorphism.rune`.

- [ ] **Step 5: Commit**

```bash
git add listings/ch546_tree_catamorphism.rune
git commit -m "$(printf 'feat(listings): ch546 tree catamorphism recovers the direct size\n\nA generic foldTree (leaf value + node combiner); instantiating it with the\nsize combiner and proving sizeFoldEq : foldTree-size t = direct size t shows\nthe catamorphism is adequate -- the reusable scheme behind ch539 count and\nch543 sum computes the same as the hand-written eliminator. By induction\n(cong succ over cong2 natAdd). Proof-only plus witnesses (both give 3).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: ch547 -- zip/unzip round-trip

**Files:**
- Create: `listings/ch547_list_zip_unzip.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `NPair`/`PList`/`LPair` datatypes, projections `fstp`/`sndp`/`fstL`/`sndL`, `zip`, `unzip`, `congPcons`, theorem `zipUnzip`, witnesses `plen`/`xs0`/`ys0`/`ps0`/`roundLen`/`origLen`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch547_list_zip_unzip.rune` with EXACTLY this content (verbatim). NOTE: the theorem's cons case eliminates the `NPair` via `NPairElim` so the constructor `mkp a b` appears literally -- this avoids needing definitional record-eta (which a user `data NPair` does not provide):

```
-- Chapter 547 -- zip/unzip round-trip: zipping the unzip of a pair-list recovers it.
--
-- zip pairs two lists elementwise into a list of NPairs (truncating to the shorter); unzip
-- splits a pair-list back into two lists. Round-tripping unzip-then-zip is the identity:
--
--   zipUnzip : (ps : PList) -> Eq PList (zip (fstL (unzip ps)) (sndL (unzip ps))) ps
--
-- proven by induction on ps. unzip always yields equal-length halves, so no length
-- hypothesis is needed (the other direction, zip-then-unzip, would need |xs| = |ys|). The
-- cons case eliminates the NPair so its constructor mkp a b appears literally -- a user
-- datatype has no definitional eta, so the projections only compute on a literal mkp.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data NatList : U is nnil : NatList | ncons : Nat -> NatList -> NatList end
data NPair : U is mkp : Nat -> Nat -> NPair end
data PList : U is pnil : PList | pcons : NPair -> PList -> PList end
data LPair : U is mklp : NatList -> NatList -> LPair end

fstp : NPair -> Nat is fn (p : NPair) is NPairElim (fn (q : NPair) is Nat end) (fn (a : Nat) (b : Nat) is a end) p end end
sndp : NPair -> Nat is fn (p : NPair) is NPairElim (fn (q : NPair) is Nat end) (fn (a : Nat) (b : Nat) is b end) p end end
fstL : LPair -> NatList is fn (p : LPair) is LPairElim (fn (q : LPair) is NatList end) (fn (xs : NatList) (ys : NatList) is xs end) p end end
sndL : LPair -> NatList is fn (p : LPair) is LPairElim (fn (q : LPair) is NatList end) (fn (xs : NatList) (ys : NatList) is ys end) p end end

zip : NatList -> NatList -> PList is
  fn (xs : NatList) is
    NatListElim (fn (w : NatList) is NatList -> PList end)
      (fn (ys : NatList) is pnil end)
      (fn (h : Nat) (t : NatList) (ih : NatList -> PList) is
         fn (ys : NatList) is
           NatListElim (fn (w : NatList) is PList end) pnil (fn (h2 : Nat) (t2 : NatList) (rec : PList) is pcons (mkp h h2) (ih t2) end) ys end end)
      xs end end

unzip : PList -> LPair is
  fn (ps : PList) is
    PListElim (fn (w : PList) is LPair end) (mklp nnil nnil)
      (fn (p : NPair) (rest : PList) (ih : LPair) is mklp (ncons (fstp p) (fstL ih)) (ncons (sndp p) (sndL ih)) end) ps end end

congPcons : (p : NPair) -> (xs : PList) -> (ys : PList) -> Eq PList xs ys -> Eq PList (pcons p xs) (pcons p ys) is
  fn (p : NPair) (xs : PList) (ys : PList) (e : Eq PList xs ys) is
    subst PList xs ys e (fn (z : PList) is Eq PList (pcons p xs) (pcons p z) end) (refl (pcons p xs)) end end

-- THEOREM: zip (fstL (unzip ps)) (sndL (unzip ps)) = ps, by induction on ps.
-- The cons case eliminates the NPair (no record eta) so mkp a b appears literally.
zipUnzip : (ps : PList) -> Eq PList (zip (fstL (unzip ps)) (sndL (unzip ps))) ps is
  fn (ps : PList) is
    PListElim (fn (w : PList) is Eq PList (zip (fstL (unzip w)) (sndL (unzip w))) w end)
      (refl pnil)
      (fn (p : NPair) (rest : PList) (ih : Eq PList (zip (fstL (unzip rest)) (sndL (unzip rest))) rest) is
         NPairElim
           (fn (q : NPair) is Eq PList (zip (fstL (unzip (pcons q rest))) (sndL (unzip (pcons q rest)))) (pcons q rest) end)
           (fn (a : Nat) (b : Nat) is
              congPcons (mkp a b) (zip (fstL (unzip rest)) (sndL (unzip rest))) rest ih end)
           p end)
      ps end end

-- witnesses: zip [1,2]/[3,4] -> unzip -> re-zip; the round-trip and original both have len 2.
plen : PList -> Nat is
  fn (ps : PList) is PListElim (fn (w : PList) is Nat end) zero (fn (p : NPair) (rest : PList) (ih : Nat) is succ ih end) ps end end
xs0 : NatList is ncons (succ zero) (ncons (succ (succ zero)) nnil) end
ys0 : NatList is ncons (succ (succ (succ zero))) (ncons (succ (succ (succ (succ zero)))) nnil) end
ps0 : PList is zip xs0 ys0 end
roundLen : Nat is plen (zip (fstL (unzip ps0)) (sndL (unzip ps0))) end
origLen : Nat is plen ps0 end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch547_list_zip_unzip.rune zipUnzip`
Expected: PASS -- JS tail includes `const zipUnzip = $unit;`. No type error, exit 0.

- [ ] **Step 3: Verify the round-trip preserves the list**

Run: `go run ./cmd/rune run listings/ch547_list_zip_unzip.rune roundLen`
Expected: `2`

Run: `go run ./cmd/rune run listings/ch547_list_zip_unzip.rune origLen`
Expected: `2` (unzip-then-zip recovers the 2-element pair-list).

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch547_list_zip_unzip.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch547_list_zip_unzip.rune`.

- [ ] **Step 5: Run the FULL elaborate-and-check gate (all three new listings + no regression)**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1`
Expected: `ok  goforge.dev/rune/v3/harness` (every chapter, including ch545/ch546/ch547, checks).

- [ ] **Step 6: Commit**

```bash
git add listings/ch547_list_zip_unzip.rune
git commit -m "$(printf 'feat(listings): ch547 zip/unzip round-trip\n\nzip (fstL (unzip ps)) (sndL (unzip ps)) = ps by induction on ps. unzip\nyields equal-length halves so no length hypothesis is needed. The cons case\neliminates the NPair so its constructor mkp a b appears literally (a user\ndatatype has no definitional eta). Proof-only plus witnesses (round-trip\nand original both len 2).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** Three requested theorems -> three tasks: ch545 take/drop split-join (Task 1), ch546 catamorphism adequacy (Task 2), ch547 zip/unzip round-trip (Task 3). Each was verified end to end in scratch before this plan was written (each `emit`s clean with the theorem erasing to `$unit`; witnesses run to lenTake=2/lenJoin=3, sF=sD=3, roundLen=origLen=2). The full elaborate-and-check gate runs once in Task 3 Step 5.

**2. Placeholder scan.** No TBD/TODO/"handle edge cases"/"similar to". Every listing body is inline and complete; every command has an exact expected output.

**3. Type consistency.** Each listing is self-contained and internally consistent (verified by compilation). Cross-task: the three files share NO identifiers at link time (separate listings), so there is no cross-task type contract to mismatch. Within each: `NatListElim` cons case binds `(h)(t)(ih)` or `(h)(rest)(ih)`; `NatElim` succ case binds `(k)(ih)`; `TreeElim` node case binds `(l)(x)(r)(ihl)(ihr)`; the user-datatype eliminators `NPairElim`/`LPairElim`/`PListElim` take `(motive)(...case lambdas...)(scrutinee)` in declaration order. ch545's `take`/`drop` use a `NatElim` returning `NatList -> NatList` with an inner `NatListElim` (double recursion); the theorem's motive is the Pi `(xs : NatList) -> Eq ...` so the IH applies at the tail. ch546's `foldTree` node combiner has type `B -> Nat -> B -> B`; `sizeF`/`size` are syntactically distinct definitions (one via the generic fold, one direct) so `sizeFoldEq` is a real theorem. ch547's cons case wraps the body in `NPairElim` so `fstp (mkp a b)`/`sndp (mkp a b)` reduce definitionally (no record-eta assumed).
