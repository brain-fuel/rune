# Tree proof trio (ch541/ch542/ch543) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add three new proven-tier corpus listings extending the Tree<->List proof family: ch541 (tree membership = list membership over the in-order flatten), ch542 (tree height is mirror-invariant), ch543 (flatten preserves the key-sum).

**Architecture:** Three independent self-contained `.rune` listings, one per task (corpus convention: each file re-derives its own toolkit). Each proves one new theorem by structural induction and ships runnable witnesses. They share no code (each is standalone) and can be implemented, reviewed, and committed independently.

**Tech Stack:** Rune (`goforge.dev/rune/v3`), eliminator-style dependently-typed proofs. Each listing is gated automatically by `harness/listings_test.go::TestListingsElaborateAndCheck` (`os.ReadDir`s `listings/`, elaborates+checks every `*.rune`; no manifest registration).

## Global Constraints

- Kernel FROZEN: corpus listings only. No `core/`, `store/`, `elaborate/`, `codegen/` change. No hash-format change.
- Each listing is GENUINELY NEW. Existing tree listings: mirror-involution (ch244), size-under-mirror (ch246), functor-id + size-preservation (ch267), map-fusion (ch290), flatten length-homomorphism (ch539), mirror-reverses-traversal (ch540). None proves membership-agreement, height-invariance, or sum-preservation.
- Self-contained: each file re-derives every helper locally; no cross-chapter imports.
- `builtin nat Nat zero succ` immediately after each `Nat` declaration.
- ch541 and ch543 use the IN-ORDER flatten (node arm `append ihl (ncons x ihr)`); ch542 has no flatten. ch542 and (where relevant) the others define `mirror` to SWAP subtrees (node arm `node ihr x ihl`).
- No em-dashes / en-dashes anywhere; use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- There is NO `rune check` subcommand. Use `rune emit FILE NAME` to elaborate+check; `rune run FILE NAME` to execute a closed value.
- Every listing in this plan was verified end to end before the plan was written (each `emit`s clean and its witnesses run to the stated values). Transcribe verbatim; do not reword, reorder, or "simplify" any proof term.

---

### Task 1: ch541 -- tree membership equals list membership

**Files:**
- Create: `listings/ch541_tree_member_flatten.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `Bool`, `or`, `eqNat`, `member`, `elem`, `append`, `flatten`, lemmas `orAssoc`/`elemAppend`, theorem `memberFlatten`, witnesses `sample`/`mem2`/`mem5`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch541_tree_member_flatten.rune` with EXACTLY this content (verbatim):

```
-- Chapter 541 -- tree membership equals list membership (Tree<->List bridge, proven tier).
--
-- A decidable membership test member k t on a Nat-keyed binary tree agrees with the list
-- membership test elem k applied to the tree's in-order flatten:
--
--   memberFlatten : (k : Nat) -> (t : Tree) -> Eq Bool (member k t) (elem k (flatten t))
--
-- proven by structural induction on t. The node step rewrites both subtree IHs under the
-- or-spine and bridges to the list side through elemAppend (elem distributes over append,
-- whose cons step needs or-associativity orAssoc). member is defined in-order (left, root,
-- right) so its or-spine matches elem (append (flatten l) (ncons x (flatten r))). Searching
-- the tree and searching its flattened key list give the same answer.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Bool : U is true : Bool | false : Bool end
data NatList : U is nnil : NatList | ncons : Nat -> NatList -> NatList end
data Tree : U is leaf : Tree | node : Tree -> Nat -> Tree -> Tree end

transB : (x : Bool) -> (y : Bool) -> (z : Bool) -> Eq Bool x y -> Eq Bool y z -> Eq Bool x z is
  fn (x : Bool) (y : Bool) (z : Bool) (p : Eq Bool x y) (q : Eq Bool y z) is
    subst Bool y z q (fn (w : Bool) is Eq Bool x w end) p end end

symB : (x : Bool) -> (y : Bool) -> Eq Bool x y -> Eq Bool y x is
  fn (x : Bool) (y : Bool) (p : Eq Bool x y) is
    subst Bool x y p (fn (z : Bool) is Eq Bool z x end) (refl x) end end

or : Bool -> Bool -> Bool is
  fn (a : Bool) (b : Bool) is BoolElim (fn (w : Bool) is Bool end) true b a end end

congOr1 : (a : Bool) -> (a2 : Bool) -> (b : Bool) -> Eq Bool a a2 -> Eq Bool (or a b) (or a2 b) is
  fn (a : Bool) (a2 : Bool) (b : Bool) (p : Eq Bool a a2) is
    subst Bool a a2 p (fn (z : Bool) is Eq Bool (or a b) (or z b) end) (refl (or a b)) end end

congOr2 : (a : Bool) -> (b : Bool) -> (b2 : Bool) -> Eq Bool b b2 -> Eq Bool (or a b) (or a b2) is
  fn (a : Bool) (b : Bool) (b2 : Bool) (p : Eq Bool b b2) is
    subst Bool b b2 p (fn (z : Bool) is Eq Bool (or a b) (or a z) end) (refl (or a b)) end end

orAssoc : (a : Bool) -> (b : Bool) -> (c : Bool) -> Eq Bool (or (or a b) c) (or a (or b c)) is
  fn (a : Bool) (b : Bool) (c : Bool) is
    BoolElim (fn (w : Bool) is Eq Bool (or (or w b) c) (or w (or b c)) end)
      (refl true)
      (refl (or b c))
      a end end

eqNat : Nat -> Nat -> Bool is
  fn (m : Nat) is
    NatElim (fn (x : Nat) is Nat -> Bool end)
      (fn (n : Nat) is NatElim (fn (x : Nat) is Bool end) true (fn (j : Nat) (ihj : Bool) is false end) n end)
      (fn (k : Nat) (ih : Nat -> Bool) is
         fn (n : Nat) is NatElim (fn (x : Nat) is Bool end) false (fn (j : Nat) (ihj : Bool) is ih j end) n end end)
      m end end

append : NatList -> NatList -> NatList is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is NatList end) ys (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons h ih end) xs end end

flatten : Tree -> NatList is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is NatList end) nnil
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : NatList) (ihr : NatList) is append ihl (ncons x ihr) end) t end end

elem : Nat -> NatList -> Bool is
  fn (k : Nat) (xs : NatList) is
    NatListElim (fn (w : NatList) is Bool end) false
      (fn (h : Nat) (rest : NatList) (ih : Bool) is or (eqNat k h) ih end) xs end end

member : Nat -> Tree -> Bool is
  fn (k : Nat) (t : Tree) is
    TreeElim (fn (x : Tree) is Bool end) false
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Bool) (ihr : Bool) is or ihl (or (eqNat k x) ihr) end) t end end

-- LEMMA: elem distributes over append (cons step uses orAssoc).
elemAppend : (k : Nat) -> (xs : NatList) -> (ys : NatList) ->
             Eq Bool (elem k (append xs ys)) (or (elem k xs) (elem k ys)) is
  fn (k : Nat) (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is Eq Bool (elem k (append w ys)) (or (elem k w) (elem k ys)) end)
      (refl (elem k ys))
      (fn (h : Nat) (rest : NatList) (ih : Eq Bool (elem k (append rest ys)) (or (elem k rest) (elem k ys))) is
         transB
           (or (eqNat k h) (elem k (append rest ys)))
           (or (eqNat k h) (or (elem k rest) (elem k ys)))
           (or (or (eqNat k h) (elem k rest)) (elem k ys))
           (congOr2 (eqNat k h) (elem k (append rest ys)) (or (elem k rest) (elem k ys)) ih)
           (symB (or (or (eqNat k h) (elem k rest)) (elem k ys)) (or (eqNat k h) (or (elem k rest) (elem k ys)))
              (orAssoc (eqNat k h) (elem k rest) (elem k ys))) end)
      xs end end

-- THEOREM: member k t = elem k (flatten t), by induction on t.
memberFlatten : (k : Nat) -> (t : Tree) -> Eq Bool (member k t) (elem k (flatten t)) is
  fn (k : Nat) (t : Tree) is
    TreeElim (fn (x : Tree) is Eq Bool (member k x) (elem k (flatten x)) end)
      (refl false)
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq Bool (member k l) (elem k (flatten l)))
          (ihr : Eq Bool (member k r) (elem k (flatten r))) is
         transB
           (or (member k l) (or (eqNat k x) (member k r)))
           (or (elem k (flatten l)) (or (eqNat k x) (elem k (flatten r))))
           (elem k (append (flatten l) (ncons x (flatten r))))
           (transB
              (or (member k l) (or (eqNat k x) (member k r)))
              (or (elem k (flatten l)) (or (eqNat k x) (member k r)))
              (or (elem k (flatten l)) (or (eqNat k x) (elem k (flatten r))))
              (congOr1 (member k l) (elem k (flatten l)) (or (eqNat k x) (member k r)) ihl)
              (congOr2 (elem k (flatten l)) (or (eqNat k x) (member k r)) (or (eqNat k x) (elem k (flatten r)))
                 (congOr2 (eqNat k x) (member k r) (elem k (flatten r)) ihr)))
           (symB
              (elem k (append (flatten l) (ncons x (flatten r))))
              (or (elem k (flatten l)) (or (eqNat k x) (elem k (flatten r))))
              (elemAppend k (flatten l) (ncons x (flatten r)))) end)
      t end end

-- witnesses: keys 1,2,3 in the tree. 2 is present, 5 is absent.
sample : Tree is node (node leaf (succ zero) leaf) (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf) end
mem2 : Bool is member (succ (succ zero)) sample end
mem5 : Bool is member (succ (succ (succ (succ (succ zero))))) sample end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch541_tree_member_flatten.rune memberFlatten`
Expected: PASS -- JS output whose tail includes `const orAssoc = $unit;`, `const elemAppend = $unit;`, `const memberFlatten = $unit;`. No `expected ... got ...`, exit 0.

- [ ] **Step 3: Verify non-vacuity (present vs absent key)**

Run: `go run ./cmd/rune run listings/ch541_tree_member_flatten.rune mem2`
Expected: `true` (key 2 is in the tree)

Run: `go run ./cmd/rune run listings/ch541_tree_member_flatten.rune mem5`
Expected: `false` (key 5 is not) -- a true/false split confirms `member` actually discriminates.

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch541_tree_member_flatten.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch541_tree_member_flatten.rune`.

- [ ] **Step 5: Commit**

```bash
git add listings/ch541_tree_member_flatten.rune
git commit -m "$(printf 'feat(listings): ch541 tree membership equals list membership\n\nmember k t = elem k (flatten t) over the in-order flatten, by structural\ninduction (node step rewrites both subtree IHs under the or-spine, bridges\nto the list side via elemAppend, whose cons step needs or-associativity).\nProof-only plus witnesses: key 2 present (true), key 5 absent (false).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: ch542 -- tree height is mirror-invariant

**Files:**
- Create: `listings/ch542_tree_height_mirror.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `max`, `maxComm`, `height`, `mirror`, toolkit `congNat`/`transEq`/`cong2Nat`, theorem `heightMirror`, witnesses `sample`/`hT`/`hM`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch542_tree_height_mirror.rune` with EXACTLY this content (verbatim). NOTE: `max`/`maxComm`/`congNat` are the standard double-NatElim definitions ported from ch180:

```
-- Chapter 542 -- tree height is mirror-invariant: height (mirror t) = height t (proven tier).
--
-- height t is 1 + the max of its subtree heights; mirror swaps every node's two children.
-- Mirroring never changes the height:
--
--   heightMirror : (t : Tree) -> Eq Nat (height (mirror t)) (height t)
--
-- proven by structural induction. The node step rewrites both subtree IHs under succ (.) of
-- max (cong2Nat), then commutes the swapped max with maxComm (the standard double-induction
-- commutativity of max, ported from ch180). A new tree property: ch246 showed mirror keeps
-- the node COUNT; this shows it keeps the DEPTH.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Tree : U is leaf : Tree | node : Tree -> Nat -> Tree -> Tree end

congNat : (f : Nat -> Nat) -> (x : Nat) -> (y : Nat) -> Eq Nat x y -> Eq Nat (f x) (f y) is
  fn (f : Nat -> Nat) (x : Nat) (y : Nat) (p : Eq Nat x y) is
    subst Nat x y p (fn (z : Nat) is Eq Nat (f x) (f z) end) (refl (f x)) end end

transEq : (A : U) -> (x : A) -> (y : A) -> (z : A) -> Eq A x y -> Eq A y z -> Eq A x z is
  fn (A : U) (x : A) (y : A) (z : A) (p : Eq A x y) (q : Eq A y z) is
    subst A y z q (fn (w : A) is Eq A x w end) p end end

cong2Nat : (f : Nat -> Nat -> Nat) -> (a : Nat) -> (a2 : Nat) -> (b : Nat) -> (b2 : Nat) ->
           Eq Nat a a2 -> Eq Nat b b2 -> Eq Nat (f a b) (f a2 b2) is
  fn (f : Nat -> Nat -> Nat) (a : Nat) (a2 : Nat) (b : Nat) (b2 : Nat) (pa : Eq Nat a a2) (pb : Eq Nat b b2) is
    transEq Nat (f a b) (f a2 b) (f a2 b2)
      (congNat (fn (x : Nat) is f x b end) a a2 pa)
      (congNat (fn (y : Nat) is f a2 y end) b b2 pb) end end

max : Nat -> Nat -> Nat is
  fn (m : Nat) is
    NatElim (fn (x : Nat) is Nat -> Nat end)
      (fn (n : Nat) is n end)
      (fn (k : Nat) (ih : Nat -> Nat) is
         fn (n : Nat) is
           NatElim (fn (x : Nat) is Nat end) (succ k) (fn (j : Nat) (ihj : Nat) is succ (ih j) end) n end end)
      m end end

maxComm : (a : Nat) -> (b : Nat) -> Eq Nat (max a b) (max b a) is
  fn (a : Nat) is
    NatElim (fn (x : Nat) is (b : Nat) -> Eq Nat (max x b) (max b x) end)
      (fn (b : Nat) is
         NatElim (fn (bb : Nat) is Eq Nat (max zero bb) (max bb zero) end)
           (refl zero)
           (fn (b' : Nat) (ihb : Eq Nat (max zero b') (max b' zero)) is refl (succ b') end)
           b end)
      (fn (a' : Nat) (ih : (b : Nat) -> Eq Nat (max a' b) (max b a')) is
         fn (b : Nat) is
           NatElim (fn (bb : Nat) is Eq Nat (max (succ a') bb) (max bb (succ a')) end)
             (refl (succ a'))
             (fn (b' : Nat) (ihb : Eq Nat (max (succ a') b') (max b' (succ a'))) is
                congNat (fn (s : Nat) is succ s end) (max a' b') (max b' a') (ih b') end)
             b end end)
      a end end

height : Tree -> Nat is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Nat end) zero
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Nat) (ihr : Nat) is succ (max ihl ihr) end) t end end

mirror : Tree -> Tree is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Tree end) leaf
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Tree) (ihr : Tree) is node ihr x ihl end) t end end

-- THEOREM: height (mirror t) = height t, by induction on t.
heightMirror : (t : Tree) -> Eq Nat (height (mirror t)) (height t) is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Eq Nat (height (mirror x)) (height x) end)
      (refl zero)
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq Nat (height (mirror l)) (height l))
          (ihr : Eq Nat (height (mirror r)) (height r)) is
         transEq Nat
           (succ (max (height (mirror r)) (height (mirror l))))
           (succ (max (height r) (height l)))
           (succ (max (height l) (height r)))
           (congNat (fn (s : Nat) is succ s end)
              (max (height (mirror r)) (height (mirror l)))
              (max (height r) (height l))
              (cong2Nat max (height (mirror r)) (height r) (height (mirror l)) (height l) ihr ihl))
           (congNat (fn (s : Nat) is succ s end)
              (max (height r) (height l)) (max (height l) (height r))
              (maxComm (height r) (height l))) end)
      t end end

-- witnesses: a right-leaning tree of height 3; mirror has the same height.
sample : Tree is node leaf (succ zero) (node leaf (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf)) end
hT : Nat is height sample end
hM : Nat is height (mirror sample) end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch542_tree_height_mirror.rune heightMirror`
Expected: PASS -- JS tail includes `const maxComm = $unit;`, `const heightMirror = $unit;`. No type error, exit 0.

- [ ] **Step 3: Verify non-vacuity (mirror preserves the asymmetric tree's height)**

Run: `go run ./cmd/rune run listings/ch542_tree_height_mirror.rune hT`
Expected: `3`

Run: `go run ./cmd/rune run listings/ch542_tree_height_mirror.rune hM`
Expected: `3` (the right-leaning sample has height 3; its mirror, left-leaning, also 3).

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch542_tree_height_mirror.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch542_tree_height_mirror.rune`.

- [ ] **Step 5: Commit**

```bash
git add listings/ch542_tree_height_mirror.rune
git commit -m "$(printf 'feat(listings): ch542 tree height is mirror-invariant\n\nheight (mirror t) = height t by structural induction (node step rewrites\nboth subtree IHs under succ-of-max via cong2Nat, then commutes the swapped\nmax with maxComm). ch246 kept the node count under mirror; this keeps the\ndepth. Proof-only plus witnesses (right-leaning sample height 3 = mirror 3).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: ch543 -- flatten preserves the key-sum

**Files:**
- Create: `listings/ch543_tree_sum_flatten.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `natAdd`, `natAddAssoc`, `append`, `sum`, `flatten`, `treeSum`, toolkit `cong`/`transEq`/`symEq`/`cong2`, lemma `sumAppend`, theorem `sumFlatten`, witnesses `sample`/`sumF`/`sumT`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch543_tree_sum_flatten.rune` with EXACTLY this content (verbatim):

```
-- Chapter 543 -- flatten preserves the key-sum (additive Tree<->List homomorphism, proven tier).
--
-- treeSum t adds every key; sum (flatten t) adds the keys of the in-order key list. They
-- agree:
--
--   sumFlatten : (t : Tree) -> Eq Nat (sum (flatten t)) (treeSum t)
--
-- proven by structural induction. The node step splits the sum over the flatten's append
-- (sumAppend, whose cons step needs natAddAssoc) then closes with both subtree IHs (cong2).
-- The additive analogue of ch539's length-homomorphism: flatten is a homomorphism for the
-- count (ch539) and for the sum (here).

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data NatList : U is nnil : NatList | ncons : Nat -> NatList -> NatList end
data Tree : U is leaf : Tree | node : Tree -> Nat -> Tree -> Tree end

cong : (A : U) -> (B : U) -> (f : A -> B) -> (x : A) -> (y : A) -> Eq A x y -> Eq B (f x) (f y) is
  fn (A : U) (B : U) (f : A -> B) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq B (f x) (f z) end) refl end end

transEq : (A : U) -> (x : A) -> (y : A) -> (z : A) -> Eq A x y -> Eq A y z -> Eq A x z is
  fn (A : U) (x : A) (y : A) (z : A) (p : Eq A x y) (q : Eq A y z) is
    subst A y z q (fn (w : A) is Eq A x w end) p end end

symEq : (A : U) -> (x : A) -> (y : A) -> Eq A x y -> Eq A y x is
  fn (A : U) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq A z x end) refl end end

cong2 : (A : U) -> (B : U) -> (C : U) -> (f : A -> B -> C) -> (a : A) -> (a2 : A) -> (b : B) -> (b2 : B) ->
        Eq A a a2 -> Eq B b b2 -> Eq C (f a b) (f a2 b2) is
  fn (A : U) (B : U) (C : U) (f : A -> B -> C) (a : A) (a2 : A) (b : B) (b2 : B) (pa : Eq A a a2) (pb : Eq B b b2) is
    transEq C (f a b) (f a2 b) (f a2 b2)
      (cong A C (fn (x : A) is f x b end) a a2 pa) (cong B C (fn (y : B) is f a2 y end) b b2 pb) end end

natAdd : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a end end

natAddAssoc : (a : Nat) -> (b : Nat) -> (c : Nat) -> Eq Nat (natAdd (natAdd a b) c) (natAdd a (natAdd b c)) is
  fn (a : Nat) (b : Nat) (c : Nat) is
    NatElim (fn (k : Nat) is Eq Nat (natAdd (natAdd k b) c) (natAdd k (natAdd b c)) end)
      refl
      (fn (k : Nat) (ih : Eq Nat (natAdd (natAdd k b) c) (natAdd k (natAdd b c))) is
         cong Nat Nat succ (natAdd (natAdd k b) c) (natAdd k (natAdd b c)) ih end)
      a end end

append : NatList -> NatList -> NatList is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is NatList end) ys (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons h ih end) xs end end

sum : NatList -> Nat is
  fn (xs : NatList) is
    NatListElim (fn (w : NatList) is Nat end) zero (fn (h : Nat) (rest : NatList) (ih : Nat) is natAdd h ih end) xs end end

flatten : Tree -> NatList is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is NatList end) nnil
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : NatList) (ihr : NatList) is append ihl (ncons x ihr) end) t end end

treeSum : Tree -> Nat is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Nat end) zero
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Nat) (ihr : Nat) is natAdd ihl (natAdd x ihr) end) t end end

-- LEMMA: sum distributes over append, by induction on xs (cons step uses natAddAssoc).
sumAppend : (xs : NatList) -> (ys : NatList) -> Eq Nat (sum (append xs ys)) (natAdd (sum xs) (sum ys)) is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is Eq Nat (sum (append w ys)) (natAdd (sum w) (sum ys)) end)
      refl
      (fn (h : Nat) (rest : NatList) (ih : Eq Nat (sum (append rest ys)) (natAdd (sum rest) (sum ys))) is
         transEq Nat
           (natAdd h (sum (append rest ys)))
           (natAdd h (natAdd (sum rest) (sum ys)))
           (natAdd (natAdd h (sum rest)) (sum ys))
           (cong Nat Nat (fn (z : Nat) is natAdd h z end) (sum (append rest ys)) (natAdd (sum rest) (sum ys)) ih)
           (symEq Nat (natAdd (natAdd h (sum rest)) (sum ys)) (natAdd h (natAdd (sum rest) (sum ys)))
              (natAddAssoc h (sum rest) (sum ys))) end)
      xs end end

-- THEOREM: sum (flatten t) = treeSum t, by induction on t.
sumFlatten : (t : Tree) -> Eq Nat (sum (flatten t)) (treeSum t) is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Eq Nat (sum (flatten x)) (treeSum x) end)
      refl
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq Nat (sum (flatten l)) (treeSum l))
          (ihr : Eq Nat (sum (flatten r)) (treeSum r)) is
         transEq Nat
           (sum (append (flatten l) (ncons x (flatten r))))
           (natAdd (sum (flatten l)) (natAdd x (sum (flatten r))))
           (natAdd (treeSum l) (natAdd x (treeSum r)))
           (sumAppend (flatten l) (ncons x (flatten r)))
           (cong2 Nat Nat Nat natAdd
              (sum (flatten l)) (treeSum l)
              (natAdd x (sum (flatten r))) (natAdd x (treeSum r))
              ihl
              (cong Nat Nat (fn (z : Nat) is natAdd x z end) (sum (flatten r)) (treeSum r) ihr)) end)
      t end end

-- witnesses: tree with keys 1,2,3 (in-order); sum = 6.
sample : Tree is node (node leaf (succ zero) leaf) (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf) end
sumF : Nat is sum (flatten sample) end
sumT : Nat is treeSum sample end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch543_tree_sum_flatten.rune sumFlatten`
Expected: PASS -- JS tail includes `const sumAppend = $unit;`, `const sumFlatten = $unit;`. No type error, exit 0.

- [ ] **Step 3: Verify the defs compute and agree**

Run: `go run ./cmd/rune run listings/ch543_tree_sum_flatten.rune sumF`
Expected: `6`

Run: `go run ./cmd/rune run listings/ch543_tree_sum_flatten.rune sumT`
Expected: `6` (keys 1+2+3 = 6 both ways).

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch543_tree_sum_flatten.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch543_tree_sum_flatten.rune`.

- [ ] **Step 5: Run the FULL elaborate-and-check gate (all three new listings + no regression)**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1`
Expected: `ok  goforge.dev/rune/v3/harness` (every chapter, including ch541/ch542/ch543, checks).

- [ ] **Step 6: Commit**

```bash
git add listings/ch543_tree_sum_flatten.rune
git commit -m "$(printf 'feat(listings): ch543 flatten preserves the key-sum\n\nsum (flatten t) = treeSum t by structural induction (node step splits the\nsum over the flatten append via sumAppend, whose cons step needs\nnatAddAssoc, then closes with both subtree IHs). The additive analogue of\nch539 length-homomorphism. Proof-only plus witnesses (sumF = sumT = 6).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** Three requested theorems -> three tasks: ch541 membership-agreement (Task 1), ch542 height-invariance (Task 2), ch543 sum-preservation (Task 3). Each was verified end to end in scratch before this plan was written (each `emit`s clean with lemmas/theorem erasing to `$unit`; witnesses run to mem2=true/mem5=false, hT=hM=3, sumF=sumT=6). The full elaborate-and-check gate runs once in Task 3 Step 5.

**2. Placeholder scan.** No TBD/TODO/"handle edge cases"/"similar to". Every listing body is inline and complete; every command has an exact expected output.

**3. Type consistency.** Each listing is self-contained and internally consistent (verified by compilation). Cross-task: the three files share NO identifiers at link time (separate listings), so there is no cross-task type contract to mismatch. Within each: `TreeElim` node case binds `(l)(x)(r)(ihl)(ihr)`; `NatListElim` cons case binds `(h)(rest)(ih)`; `BoolElim` takes `(motive)(true-case)(false-case)(scrut)`; `or = BoolElim (..) true b a`. ch541's `member`/`elem` share the in-order or-spine shape so the node case aligns through `elemAppend`. ch542 ports `max`/`maxComm`/`congNat` verbatim from ch180. ch543's `treeSum` node arm `natAdd ihl (natAdd x ihr)` matches the in-order flatten `append ihl (ncons x ihr)` so the node case needs no extra associativity rewrite beyond `sumAppend`.
