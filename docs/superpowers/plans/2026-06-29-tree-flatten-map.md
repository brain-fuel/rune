# Tree flatten/map naturality (ch544) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a new proven-tier corpus listing `listings/ch544_tree_flatten_map.rune` establishing that flattening commutes with mapping: `flatten (tmap f t) = map f (flatten t)` for all trees and key functions f.

**Architecture:** A single self-contained `.rune` listing (corpus convention: each file re-derives its own toolkit). Defines `Tree` (Nat-keyed), `NatList`, `append`, list `map`, in-order `flatten`, tree `tmap`; re-derives the equality toolkit; proves one lemma (`mapAppend`, map distributes over append) then the naturality theorem by structural induction (node step closes via `cong2App` + `congCons` on the two subtree IHs, bridging the list side with `mapAppend`).

**Tech Stack:** Rune (`goforge.dev/rune/v3`), eliminator-style dependently-typed proofs. Gated automatically by `harness/listings_test.go::TestListingsElaborateAndCheck` (`os.ReadDir`s `listings/`, elaborates+checks every `*.rune`; no manifest).

## Global Constraints

- Kernel FROZEN: corpus listing only. No `core/`, `store/`, `elaborate/`, `codegen/` change. No hash-format change.
- DEFINITION ORDER MATTERS: rune resolves names top-to-bottom, so `append`/`map`/`flatten`/`tmap` MUST appear BEFORE the equality helpers (`congApp1` etc. reference `append`). The Step 1 code block below is already in the correct, verified order -- transcribe verbatim, do not reorder.
- GENUINELY NEW: existing tree listings prove mirror-involution (ch244), size-under-mirror (ch246), functor-id + size-preservation (ch267), map-fusion (ch290), flatten length-homomorphism (ch539), mirror-reverses-traversal (ch540), membership-agreement (ch541), height-invariance (ch542), sum-preservation (ch543). None proves the flatten/map naturality square (ch267 maps within Tree; ch290 fuses two tree maps; this commutes a tree map with flatten into a list map).
- IN-ORDER flatten (node arm `append ihl (ncons x ihr)`); `tmap` relabels keeping shape (node arm `node ihl (f x) ihr`).
- Self-contained: re-derive every helper locally; no cross-chapter imports.
- `builtin nat Nat zero succ` immediately after the `Nat` declaration.
- No em-dashes / en-dashes anywhere; use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- There is NO `rune check` subcommand. Use `rune emit FILE NAME` to elaborate+check; `rune run FILE NAME` to execute a closed value.

---

### Task 1: ch544 -- flatten/map naturality

**Files:**
- Create: `listings/ch544_tree_flatten_map.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck`

**Interfaces:**
- Consumes: ambient `Eq`/`refl`/`subst`. Nothing from other tasks.
- Produces: `append`, `map`, `flatten`, `tmap`, toolkit `transL`/`symL`/`congCons`/`congApp1`/`congApp2`/`cong2App`, lemma `mapAppend`, theorem `flattenMap`, witnesses `headOr0`/`sample`/`hFlatMap`/`hMapFlat`. Terminal corpus node.

- [ ] **Step 1: Write the listing**

Create `listings/ch544_tree_flatten_map.rune` with EXACTLY this content (verbatim -- this proof was verified end to end before the plan was written; keep the definition order, especially `append`/`map`/`flatten`/`tmap` above the equality helpers):

```
-- Chapter 544 -- flatten commutes with map (functor naturality, Tree<->List bridge, proven tier).
--
-- Relabelling a tree's keys then flattening gives the same list as flattening then mapping
-- the list:
--
--   flattenMap : (f : Nat -> Nat) -> (t : Tree) -> Eq NatList (flatten (tmap f t)) (map f (flatten t))
--
-- the naturality square for the in-order flatten as a natural transformation from the tree
-- functor (tmap) to the list functor (map). Proven by structural induction on t; the node
-- step closes with cong2App + congCons on the two subtree IHs and bridges the list side via
-- mapAppend (map distributes over append). ch267 mapped within Tree and ch290 fused two tree
-- maps; this commutes a tree map with flatten into a list map.

data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data NatList : U is nnil : NatList | ncons : Nat -> NatList -> NatList end
data Tree : U is leaf : Tree | node : Tree -> Nat -> Tree -> Tree end

append : NatList -> NatList -> NatList is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is NatList end) ys (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons h ih end) xs end end

map : (Nat -> Nat) -> NatList -> NatList is
  fn (f : Nat -> Nat) (xs : NatList) is
    NatListElim (fn (w : NatList) is NatList end) nnil
      (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons (f h) ih end) xs end end

flatten : Tree -> NatList is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is NatList end) nnil
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : NatList) (ihr : NatList) is append ihl (ncons x ihr) end) t end end

tmap : (Nat -> Nat) -> Tree -> Tree is
  fn (f : Nat -> Nat) (t : Tree) is
    TreeElim (fn (x : Tree) is Tree end) leaf
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Tree) (ihr : Tree) is node ihl (f x) ihr end) t end end

transL : (xs : NatList) -> (ys : NatList) -> (zs : NatList) -> Eq NatList xs ys -> Eq NatList ys zs -> Eq NatList xs zs is
  fn (xs : NatList) (ys : NatList) (zs : NatList) (p : Eq NatList xs ys) (q : Eq NatList ys zs) is
    subst NatList ys zs q (fn (w : NatList) is Eq NatList xs w end) p end end

symL : (xs : NatList) -> (ys : NatList) -> Eq NatList xs ys -> Eq NatList ys xs is
  fn (xs : NatList) (ys : NatList) (p : Eq NatList xs ys) is
    subst NatList xs ys p (fn (z : NatList) is Eq NatList z xs end) (refl xs) end end

congCons : (h : Nat) -> (xs : NatList) -> (ys : NatList) -> Eq NatList xs ys -> Eq NatList (ncons h xs) (ncons h ys) is
  fn (h : Nat) (xs : NatList) (ys : NatList) (e : Eq NatList xs ys) is
    subst NatList xs ys e (fn (z : NatList) is Eq NatList (ncons h xs) (ncons h z) end) (refl (ncons h xs)) end end

congApp1 : (xs : NatList) -> (xs2 : NatList) -> (ys : NatList) -> Eq NatList xs xs2 -> Eq NatList (append xs ys) (append xs2 ys) is
  fn (xs : NatList) (xs2 : NatList) (ys : NatList) (e : Eq NatList xs xs2) is
    subst NatList xs xs2 e (fn (z : NatList) is Eq NatList (append xs ys) (append z ys) end) (refl (append xs ys)) end end

congApp2 : (xs : NatList) -> (ys : NatList) -> (ys2 : NatList) -> Eq NatList ys ys2 -> Eq NatList (append xs ys) (append xs ys2) is
  fn (xs : NatList) (ys : NatList) (ys2 : NatList) (e : Eq NatList ys ys2) is
    subst NatList ys ys2 e (fn (z : NatList) is Eq NatList (append xs ys) (append xs z) end) (refl (append xs ys)) end end

cong2App : (a : NatList) -> (a2 : NatList) -> (b : NatList) -> (b2 : NatList) ->
           Eq NatList a a2 -> Eq NatList b b2 -> Eq NatList (append a b) (append a2 b2) is
  fn (a : NatList) (a2 : NatList) (b : NatList) (b2 : NatList) (pa : Eq NatList a a2) (pb : Eq NatList b b2) is
    transL (append a b) (append a2 b) (append a2 b2)
      (congApp1 a a2 b pa) (congApp2 a2 b b2 pb) end end

-- LEMMA: map distributes over append, by induction on xs.
mapAppend : (f : Nat -> Nat) -> (xs : NatList) -> (ys : NatList) ->
            Eq NatList (map f (append xs ys)) (append (map f xs) (map f ys)) is
  fn (f : Nat -> Nat) (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is Eq NatList (map f (append w ys)) (append (map f w) (map f ys)) end)
      (refl (map f ys))
      (fn (h : Nat) (rest : NatList) (ih : Eq NatList (map f (append rest ys)) (append (map f rest) (map f ys))) is
         congCons (f h) (map f (append rest ys)) (append (map f rest) (map f ys)) ih end)
      xs end end

-- THEOREM: flatten (tmap f t) = map f (flatten t), by induction on t.
flattenMap : (f : Nat -> Nat) -> (t : Tree) -> Eq NatList (flatten (tmap f t)) (map f (flatten t)) is
  fn (f : Nat -> Nat) (t : Tree) is
    TreeElim (fn (x : Tree) is Eq NatList (flatten (tmap f x)) (map f (flatten x)) end)
      (refl nnil)
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq NatList (flatten (tmap f l)) (map f (flatten l)))
          (ihr : Eq NatList (flatten (tmap f r)) (map f (flatten r))) is
         transL
           (append (flatten (tmap f l)) (ncons (f x) (flatten (tmap f r))))
           (append (map f (flatten l)) (ncons (f x) (map f (flatten r))))
           (map f (append (flatten l) (ncons x (flatten r))))
           (cong2App
              (flatten (tmap f l)) (map f (flatten l))
              (ncons (f x) (flatten (tmap f r))) (ncons (f x) (map f (flatten r)))
              ihl
              (congCons (f x) (flatten (tmap f r)) (map f (flatten r)) ihr))
           (symL
              (map f (append (flatten l) (ncons x (flatten r))))
              (append (map f (flatten l)) (ncons (f x) (map f (flatten r))))
              (mapAppend f (flatten l) (ncons x (flatten r)))) end)
      t end end

-- witnesses: keys 1,2,3 in-order; map succ over the keys; head of each side is 2 (succ of 1).
headOr0 : NatList -> Nat is
  fn (xs : NatList) is NatListElim (fn (w : NatList) is Nat end) zero (fn (h : Nat) (rest : NatList) (ih : Nat) is h end) xs end end
sample : Tree is node (node leaf (succ zero) leaf) (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf) end
hFlatMap : Nat is headOr0 (flatten (tmap (fn (u : Nat) is succ u end) sample)) end
hMapFlat : Nat is headOr0 (map (fn (u : Nat) is succ u end) (flatten sample)) end
```

- [ ] **Step 2: Elaborate + check**

Run: `go run ./cmd/rune emit listings/ch544_tree_flatten_map.rune flattenMap`
Expected: PASS -- JS output whose tail includes `const mapAppend = $unit;`, `const flattenMap = $unit;`, `console.log($show(flattenMap));`. No `expected ... got ...`, no `not in scope`, exit 0. (If you see `I can't find append in scope`, the def order was changed -- restore `append`/`map`/`flatten`/`tmap` ABOVE the congruence helpers.)

- [ ] **Step 3: Verify the naturality holds computationally (both paths agree, f actually transforms)**

Run: `go run ./cmd/rune run listings/ch544_tree_flatten_map.rune hFlatMap`
Expected: `2`

Run: `go run ./cmd/rune run listings/ch544_tree_flatten_map.rune hMapFlat`
Expected: `2`

(`sample`'s in-order first key is 1; `succ` maps it to 2 on both paths -- flatten-then-map and map-then-flatten agree, and the function genuinely changed the key from 1, so the theorem is not vacuous.)

- [ ] **Step 4: Targeted corpus gate**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch544_tree_flatten_map.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch544_tree_flatten_map.rune`.

- [ ] **Step 5: Run the FULL elaborate-and-check gate (no regression)**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1`
Expected: `ok  goforge.dev/rune/v3/harness` (every chapter, including ch544, checks).

- [ ] **Step 6: Commit**

```bash
git add listings/ch544_tree_flatten_map.rune
git commit -m "$(printf 'feat(listings): ch544 flatten commutes with map (functor naturality)\n\nflatten (tmap f t) = map f (flatten t) by structural induction (node step\ncloses with cong2App + congCons on both subtree IHs, bridges the list side\nvia mapAppend). The in-order flatten is a natural transformation from the\ntree functor to the list functor. Proof-only plus witnesses (both paths\nhead to 2 under succ).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** The single requirement -- a genuinely new proven flatten/map naturality theorem gated by the corpus -- is implemented by Task 1 and verified green in scratch before this plan was written (`emit` clean with `mapAppend`/`flattenMap` erasing to `$unit`; witnesses `hFlatMap`=`hMapFlat`=2). Covered.

**2. Placeholder scan.** No TBD/TODO/"handle edge cases"/"similar to". The full listing body is inline and complete; every command has an exact expected output. The def-order pitfall (which caused a `not in scope` failure during verification) is called out explicitly in Global Constraints and Step 2.

**3. Type consistency.** Names are internally consistent and match the verified prototype: `Tree`/`node : Tree -> Nat -> Tree`, `NatList`/`ncons`, `append`/`map`/`flatten`/`tmap`, toolkit `transL`/`symL`/`congCons`/`congApp1`/`congApp2`/`cong2App`, lemma `mapAppend`, theorem `flattenMap`, witnesses `headOr0`/`sample`/`hFlatMap`/`hMapFlat`. `TreeElim` node case binds `(l)(x)(r)(ihl)(ihr)`; `NatListElim` cons case binds `(h)(rest)(ih)`. `tmap` node arm `node ihl (f x) ihr` keeps shape + relabels; `flatten` node arm `append ihl (ncons x ihr)` is in-order; the two align so the node case closes via `cong2App` + `congCons` + the `mapAppend` bridge with no extra associativity.
