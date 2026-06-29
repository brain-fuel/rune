# Tree flatten-length homomorphism (ch539) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a new proven-tier corpus listing `listings/ch539_tree_flatten_length.rune` establishing that the pre-order `flatten : Tree -> NatList` is a length-homomorphism: `length (flatten t) = size t` for all trees.

**Architecture:** A single self-contained `.rune` listing (the corpus convention: each file re-derives its own equality toolkit, "Eq/refl/subst ambient as in ch03"). Defines `Tree` (value-carrying binary tree, ch267 shape), `NatList`, pre-order `flatten`, `size`, `length`, `append`; proves one lemma (`lengthAppend`, length distributes over append) then the main theorem by structural induction. Pre-order flatten is chosen deliberately: the node case reduces to `succ (natAdd …)` matching `size`'s shape, so the proof needs only `cong`/`cong2` + `lengthAppend` — no `natAddSucc` detour that in-order traversal would force.

**Tech Stack:** Rune (`goforge.dev/rune/v3`), eliminator-style dependently-typed proofs. The listing is gated automatically by `harness/listings_test.go::TestListingsElaborateAndCheck`, which `os.ReadDir`s `listings/` and elaborates+checks every `*.rune` — no manifest registration needed.

## Global Constraints

- Kernel FROZEN: this is a corpus listing only. No `core/`, `store/`, `elaborate/`, or `codegen/` change. No hash-format change.
- The listing is GENUINELY NEW: existing tree listings prove mirror-involution (ch244), size-under-mirror (ch246), functor-identity + size-preservation (ch267), and map-fusion (ch290). None bridges Tree to List. Do not duplicate them.
- Self-contained: re-derive `cong`/`transEq`/`cong2` locally (the corpus convention); do NOT import from other chapters.
- `builtin nat Nat zero succ` after the `Nat` declaration (compressed numerals; matches ch267/ch36).
- No em-dashes / en-dashes anywhere (code, comments, commits, docs); use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- There is NO `rune check` subcommand. To elaborate+check a file standalone, use `rune emit FILE NAME` (it elaborates every def before emitting). To RUN a closed value, use `rune run FILE NAME`.

---

### Task 1: The ch539 flatten-length listing

**Files:**
- Create: `listings/ch539_tree_flatten_length.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck` (auto-discovers the new file)

**Interfaces:**
- Consumes: nothing from other tasks. The ambient `Eq`/`refl`/`subst` are session built-ins (every listing relies on them).
- Produces: the checked definitions `Tree`, `NatList`, `flatten`, `size`, `length`, `append`, `lengthAppend`, `flattenLength`, and the runnable witnesses `sample`/`flatLen`/`treeSize`. Nothing downstream depends on them (terminal corpus node).

- [ ] **Step 1: Write the computational core (datatypes + toolkit + flatten/size/length/append)**

Create `listings/ch539_tree_flatten_length.rune` with exactly this content (the full final listing -- later steps append nothing; they only verify):

```
-- Chapter 539 -- flatten is a length-homomorphism (the Tree <-> List bridge, proven tier).
--
-- The pre-order traversal flatten : Tree -> NatList lists a tree's keys (root, then left
-- subtree, then right). Its output length equals the tree's node count:
--
--   flattenLength : (t : Tree) -> Eq Nat (length (flatten t)) (size t)
--
-- proven by structural induction on t, the node step closing with the append-length
-- homomorphism lemma (lengthAppend) and congruence in both subtree IHs (cong2). This is
-- the Tree<->List analogue of ch36's list length-homomorphism, and the first corpus
-- listing connecting the two datatypes (ch244/246/267/290 stay within Tree). Proof-only,
-- plus three runnable witnesses that flatten and size actually compute.

data Nat : U is
  zero : Nat
| succ : Nat -> Nat
end
builtin nat Nat zero succ

data NatList : U is
  nnil  : NatList
| ncons : Nat -> NatList -> NatList
end

data Tree : U is
  leaf : Tree
| node : Tree -> Nat -> Tree -> Tree
end

-- (Eq / refl / subst are ambient, as in ch03.) The equality toolkit, from subst:
cong : (A : U) -> (B : U) -> (f : A -> B) -> (x : A) -> (y : A) -> Eq A x y -> Eq B (f x) (f y) is
  fn (A : U) (B : U) (f : A -> B) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq B (f x) (f z) end) refl
  end
end

transEq : (A : U) -> (x : A) -> (y : A) -> (z : A) -> Eq A x y -> Eq A y z -> Eq A x z is
  fn (A : U) (x : A) (y : A) (z : A) (p : Eq A x y) (q : Eq A y z) is
    subst A y z q (fn (w : A) is Eq A x w end) p
  end
end

cong2 : (A : U) -> (B : U) -> (C : U) -> (f : A -> B -> C) ->
        (a : A) -> (a2 : A) -> (b : B) -> (b2 : B) ->
        Eq A a a2 -> Eq B b b2 -> Eq C (f a b) (f a2 b2) is
  fn (A : U) (B : U) (C : U) (f : A -> B -> C)
     (a : A) (a2 : A) (b : B) (b2 : B) (pa : Eq A a a2) (pb : Eq B b b2) is
    transEq C (f a b) (f a2 b) (f a2 b2)
      (cong A C (fn (x : A) is f x b end) a a2 pa)
      (cong B C (fn (y : B) is f a2 y end) b b2 pb)
  end
end

natAdd : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat end) b (fn (k : Nat) (ih : Nat) is succ ih end) a
  end
end

length : NatList -> Nat is
  fn (xs : NatList) is
    NatListElim (fn (x : NatList) is Nat end)
      zero
      (fn (h : Nat) (rest : NatList) (ih : Nat) is succ ih end)
      xs
  end
end

append : NatList -> NatList -> NatList is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (x : NatList) is NatList end)
      ys
      (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons h ih end)
      xs
  end
end

-- pre-order flatten: root key, then left subtree, then right.
flatten : Tree -> NatList is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is NatList end)
      nnil
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : NatList) (ihr : NatList) is
         ncons x (append ihl ihr) end)
      t
  end
end

size : Tree -> Nat is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Nat end)
      zero
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Nat) (ihr : Nat) is succ (natAdd ihl ihr) end)
      t
  end
end

-- LEMMA: length is an append-homomorphism, by induction on xs.
--   nnil:  length (append nnil ys) = length ys = natAdd 0 (length ys)              -> refl
--   ncons: length (append (ncons h rest) ys) = succ (length (append rest ys))
--          natAdd (length (ncons h rest)) (length ys) = succ (natAdd (length rest) (length ys))
--          close with cong succ on the IH.
lengthAppend : (xs : NatList) -> (ys : NatList) ->
               Eq Nat (length (append xs ys)) (natAdd (length xs) (length ys)) is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (x : NatList) is Eq Nat (length (append x ys)) (natAdd (length x) (length ys)) end)
      refl
      (fn (h : Nat) (rest : NatList) (ih : Eq Nat (length (append rest ys)) (natAdd (length rest) (length ys))) is
         cong Nat Nat succ (length (append rest ys)) (natAdd (length rest) (length ys)) ih end)
      xs
  end
end

-- THEOREM: length (flatten t) = size t, by structural induction on t.
--   leaf:  length (flatten leaf) = length nnil = zero = size leaf                   -> refl
--   node:  length (flatten (node l x r)) = succ (length (append (flatten l) (flatten r)))
--          size (node l x r)             = succ (natAdd (size l) (size r))
--          bridge the two arguments of succ with transEq:
--            length (append (flatten l) (flatten r))
--              = natAdd (length (flatten l)) (length (flatten r))   [lengthAppend]
--              = natAdd (size l) (size r)                           [cong2 natAdd ihl ihr]
flattenLength : (t : Tree) -> Eq Nat (length (flatten t)) (size t) is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Eq Nat (length (flatten x)) (size x) end)
      refl
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq Nat (length (flatten l)) (size l))
          (ihr : Eq Nat (length (flatten r)) (size r)) is
         cong Nat Nat succ
           (length (append (flatten l) (flatten r)))
           (natAdd (size l) (size r))
           (transEq Nat
              (length (append (flatten l) (flatten r)))
              (natAdd (length (flatten l)) (length (flatten r)))
              (natAdd (size l) (size r))
              (lengthAppend (flatten l) (flatten r))
              (cong2 Nat Nat Nat natAdd
                 (length (flatten l)) (size l)
                 (length (flatten r)) (size r) ihl ihr)) end)
      t
  end
end

-- sanity witnesses (computational, runnable): a 3-key tree.
sample : Tree is node (node leaf (succ zero) leaf) (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf) end
flatLen : Nat is length (flatten sample) end
treeSize : Nat is size sample end
```

- [ ] **Step 2: Verify the whole listing elaborates + checks (the proof IS the typecheck)**

Run: `go run ./cmd/rune emit listings/ch539_tree_flatten_length.rune flattenLength`
Expected: PASS -- prints JavaScript (the erased shadow); the last lines show `const lengthAppend = $unit;` and `const flattenLength = $unit;` (Eq-valued proofs erase to unit) and `console.log($show(flattenLength));`. No type error, no `expected ... got ...`, exit 0. If it instead prints the CLI usage banner, you typed `check` -- there is no such subcommand; use `emit`.

- [ ] **Step 3: Verify flatten and size actually COMPUTE and agree (guards against both-wrong-identically)**

Run: `go run ./cmd/rune run listings/ch539_tree_flatten_length.rune flatLen`
Expected: `3`

Run: `go run ./cmd/rune run listings/ch539_tree_flatten_length.rune treeSize`
Expected: `3`

(The 3-key `sample` flattens to a 3-element list and has size 3. Both computing to the same 3 confirms the theorem is not vacuously typechecking over two identically-broken defs.)

- [ ] **Step 4: Run the corpus gate for the new file**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch539_tree_flatten_length.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch539_tree_flatten_length.rune` then `ok  goforge.dev/rune/v3/harness`.

- [ ] **Step 5: Run the FULL elaborate-and-check gate (no regression from adding the file)**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1`
Expected: `ok  goforge.dev/rune/v3/harness` (every chapter still checks).

- [ ] **Step 6: Commit**

```bash
git add listings/ch539_tree_flatten_length.rune
git commit -m "$(printf 'feat(listings): ch539 flatten is a length-homomorphism (Tree<->List)\n\nPre-order flatten : Tree -> NatList, with length (flatten t) = size t by\nstructural induction (node step via the append-length lemma + cong2). The\nfirst corpus listing bridging Tree and List; ch244/246/267/290 stay within\nTree. Proof-only plus runnable flatten/size witnesses (both compute to 3).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** No separate spec doc (a small plan-ready corpus listing chosen directly under writing-plans). The single requirement -- a genuinely new proven Tree<->List theorem gated by the corpus -- is implemented by Task 1 and verified green in scratch before this plan was written (`flatLen`=`treeSize`=3, `emit` clean). Covered.

**2. Placeholder scan.** No TBD/TODO/"handle edge cases"/"similar to". The full listing body is inline and complete; every command has an exact expected output.

**3. Type consistency.** Names used are internally consistent and match the verified prototype: `Tree`/`node : Tree -> Nat -> Tree`, `NatList`/`ncons`, `flatten`/`size`/`length`/`append`/`natAdd`, lemma `lengthAppend`, theorem `flattenLength`, witnesses `sample`/`flatLen`/`treeSize`. The `TreeElim` node case binds `(l)(x)(r)(ihl)(ihr)` (5 args, matching ch267); `NatListElim` cons case binds `(h)(rest)(ih)`. `cong2` arity and argument order match ch267's identical helper.
