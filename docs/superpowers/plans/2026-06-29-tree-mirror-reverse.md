# Tree mirror-reverses-traversal (ch540) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a new proven-tier corpus listing `listings/ch540_tree_mirror_reverse.rune` establishing that mirroring a binary tree reverses its in-order traversal: `flatten (mirror t) = reverse (flatten t)` for all trees.

**Architecture:** A single self-contained `.rune` listing (corpus convention: each file re-derives its own equality toolkit). Defines `Tree` (Nat-keyed binary tree), `NatList`, `append`, `reverse`, IN-ORDER `flatten`, `mirror`; re-derives the list-algebra lemmas `appendNilR` / `appendAssoc` / `revAppend` (monomorphic ports of ch73's polymorphic versions); proves the headline theorem by structural induction, the node case bridging the two sides through the common midpoint `append (reverse FR) (ncons x (reverse FL))` via `revAppend` + `appendAssoc` + both subtree IHs.

**Tech Stack:** Rune (`goforge.dev/rune/v3`), eliminator-style dependently-typed proofs. Gated automatically by `harness/listings_test.go::TestListingsElaborateAndCheck` (`os.ReadDir`s `listings/`, elaborates+checks every `*.rune`; no manifest).

## Global Constraints

- Kernel FROZEN: corpus listing only. No `core/`, `store/`, `elaborate/`, `codegen/` change. No hash-format change.
- IN-ORDER flatten is MANDATORY: the theorem `flatten (mirror t) = reverse (flatten t)` is FALSE for the pre-order flatten of ch539 (the root key lands at the wrong end of the list). This listing re-derives its own in-order `flatten`; it does NOT import ch539's.
- GENUINELY NEW: existing tree listings prove mirror-involution (ch244), size-under-mirror (ch246), functor-identity + size-preservation (ch267), map-fusion (ch290), flatten length-homomorphism (ch539). None proves the mirror/reverse traversal law.
- Self-contained: re-derive every helper locally (corpus convention); no cross-chapter imports.
- `builtin nat Nat zero succ` immediately after the `Nat` declaration.
- No em-dashes / en-dashes anywhere; use `--`.
- Conventional Commits; trailer `Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>`.
- There is NO `rune check` subcommand. Use `rune emit FILE NAME` to elaborate+check; `rune run FILE NAME` to execute a closed value.

---

### Task 1: The ch540 mirror-reverse listing

**Files:**
- Create: `listings/ch540_tree_mirror_reverse.rune`
- Gate (no edit): `harness/listings_test.go::TestListingsElaborateAndCheck` (auto-discovers the new file)

**Interfaces:**
- Consumes: the ambient `Eq` / `refl` / `subst` session built-ins. Nothing from other tasks.
- Produces: checked `Tree`, `NatList`, `append`, `reverse`, `flatten`, `mirror`, the toolkit `symL`/`transL`/`congCons`/`congApp1`/`congApp2`, the lemmas `appendNilR`/`appendAssoc`/`revAppend`, the theorem `flattenMirror`, and the runnable witnesses `headOr0`/`sample`/`headF`/`headFM`/`headRF`. Terminal corpus node; nothing downstream depends on it.

- [ ] **Step 1: Write the listing**

Create `listings/ch540_tree_mirror_reverse.rune` with EXACTLY this content (verbatim -- this proof was verified end to end before the plan was written; do not reword, reorder, or "simplify" any term, especially the nested `transL`/`symL` chain in `flattenMirror`):

```
-- Chapter 540 -- mirroring a tree reverses its traversal (Tree<->List bridge, proven tier).
--
-- With the IN-ORDER traversal flatten : Tree -> NatList (left subtree, root key, right
-- subtree), mirroring a tree -- swapping every node's two children -- exactly reverses the
-- key sequence:
--
--   flattenMirror : (t : Tree) -> Eq NatList (flatten (mirror t)) (reverse (flatten t))
--
-- proven by structural induction on t. The node case bridges the two sides through the
-- common midpoint  append (reverse FR) (ncons x (reverse FL))  using the reverse anti-
-- homomorphism (revAppend) and append associativity, plus both subtree IHs. NOTE: this
-- needs in-order flatten; the pre-order flatten of ch539 does NOT satisfy the law (the
-- root key would land at the wrong end), so this listing defines its own in-order flatten.
-- First corpus listing relating mirror and reverse across the Tree<->List boundary.

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

append : NatList -> NatList -> NatList is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is NatList end)
      ys
      (fn (h : Nat) (rest : NatList) (ih : NatList) is ncons h ih end)
      xs
  end
end

-- reverse via append: reverse (ncons h t) = reverse t ++ [h].
reverse : NatList -> NatList is
  fn (xs : NatList) is
    NatListElim (fn (w : NatList) is NatList end)
      nnil
      (fn (h : Nat) (rest : NatList) (ih : NatList) is append ih (ncons h nnil) end)
      xs
  end
end

-- in-order flatten: left, root, right.
flatten : Tree -> NatList is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is NatList end)
      nnil
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : NatList) (ihr : NatList) is
         append ihl (ncons x ihr) end)
      t
  end
end

-- mirror: swap the two subtrees, recursively.
mirror : Tree -> Tree is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Tree end)
      leaf
      (fn (l : Tree) (x : Nat) (r : Tree) (ihl : Tree) (ihr : Tree) is node ihr x ihl end)
      t
  end
end

-- equational plumbing over Eq NatList (from subst):
symL : (xs : NatList) -> (ys : NatList) -> Eq NatList xs ys -> Eq NatList ys xs is
  fn (xs : NatList) (ys : NatList) (e : Eq NatList xs ys) is
    subst NatList xs ys e (fn (z : NatList) is Eq NatList z xs end) (refl xs)
  end
end

transL : (xs : NatList) -> (ys : NatList) -> (zs : NatList) -> Eq NatList xs ys -> Eq NatList ys zs -> Eq NatList xs zs is
  fn (xs : NatList) (ys : NatList) (zs : NatList) (e1 : Eq NatList xs ys) (e2 : Eq NatList ys zs) is
    subst NatList ys zs e2 (fn (z : NatList) is Eq NatList xs z end) e1
  end
end

congCons : (h : Nat) -> (xs : NatList) -> (ys : NatList) -> Eq NatList xs ys -> Eq NatList (ncons h xs) (ncons h ys) is
  fn (h : Nat) (xs : NatList) (ys : NatList) (e : Eq NatList xs ys) is
    subst NatList xs ys e (fn (z : NatList) is Eq NatList (ncons h xs) (ncons h z) end) (refl (ncons h xs))
  end
end

congApp1 : (xs : NatList) -> (xs2 : NatList) -> (ys : NatList) -> Eq NatList xs xs2 -> Eq NatList (append xs ys) (append xs2 ys) is
  fn (xs : NatList) (xs2 : NatList) (ys : NatList) (e : Eq NatList xs xs2) is
    subst NatList xs xs2 e (fn (z : NatList) is Eq NatList (append xs ys) (append z ys) end) (refl (append xs ys))
  end
end

congApp2 : (xs : NatList) -> (ys : NatList) -> (ys2 : NatList) -> Eq NatList ys ys2 -> Eq NatList (append xs ys) (append xs ys2) is
  fn (xs : NatList) (ys : NatList) (ys2 : NatList) (e : Eq NatList ys ys2) is
    subst NatList ys ys2 e (fn (z : NatList) is Eq NatList (append xs ys) (append xs z) end) (refl (append xs ys))
  end
end

-- RIGHT UNIT: append xs nnil = xs (left unit append nnil ys ~> ys is definitional).
appendNilR : (xs : NatList) -> Eq NatList (append xs nnil) xs is
  fn (xs : NatList) is
    NatListElim (fn (w : NatList) is Eq NatList (append w nnil) w end)
      (refl nnil)
      (fn (h : Nat) (rest : NatList) (ih : Eq NatList (append rest nnil) rest) is
         congCons h (append rest nnil) rest ih end)
      xs
  end
end

-- ASSOCIATIVITY: (NatList, append, nnil) is a monoid.
appendAssoc : (xs : NatList) -> (ys : NatList) -> (zs : NatList) -> Eq NatList (append (append xs ys) zs) (append xs (append ys zs)) is
  fn (xs : NatList) (ys : NatList) (zs : NatList) is
    NatListElim (fn (w : NatList) is Eq NatList (append (append w ys) zs) (append w (append ys zs)) end)
      (refl (append ys zs))
      (fn (h : Nat) (rest : NatList) (ih : Eq NatList (append (append rest ys) zs) (append rest (append ys zs))) is
         congCons h (append (append rest ys) zs) (append rest (append ys zs)) ih end)
      xs
  end
end

-- REVERSE IS AN ANTI-HOMOMORPHISM: reverse (append xs ys) = append (reverse ys) (reverse xs).
revAppend : (xs : NatList) -> (ys : NatList) -> Eq NatList (reverse (append xs ys)) (append (reverse ys) (reverse xs)) is
  fn (xs : NatList) (ys : NatList) is
    NatListElim (fn (w : NatList) is Eq NatList (reverse (append w ys)) (append (reverse ys) (reverse w)) end)
      (symL (append (reverse ys) nnil) (reverse ys) (appendNilR (reverse ys)))
      (fn (h : Nat) (rest : NatList) (ih : Eq NatList (reverse (append rest ys)) (append (reverse ys) (reverse rest))) is
         transL
           (append (reverse (append rest ys)) (ncons h nnil))
           (append (append (reverse ys) (reverse rest)) (ncons h nnil))
           (append (reverse ys) (append (reverse rest) (ncons h nnil)))
           (congApp1 (reverse (append rest ys)) (append (reverse ys) (reverse rest)) (ncons h nnil) ih)
           (appendAssoc (reverse ys) (reverse rest) (ncons h nnil))
       end)
      xs
  end
end

-- THE THEOREM: flatten (mirror t) = reverse (flatten t), by induction on t.
--   leaf: flatten (mirror leaf) = nnil = reverse nnil = reverse (flatten leaf)            -> refl
--   node (l x r): write FL = flatten l, FR = flatten r.
--     LHS reduces to  append (flatten (mirror r)) (ncons x (flatten (mirror l)))
--     RHS reduces to  reverse (append FL (ncons x FR))
--     Bridge both to the midpoint  append (reverse FR) (ncons x (reverse FL)):
--       LHS -> midpoint  by ihr (on the outer append's left) then ihl (under ncons), congApp1/congApp2/congCons
--       RHS -> midpoint  by revAppend FL (ncons x FR) then appendAssoc (reverse FR) (ncons x nnil) (reverse FL)
--                        (reverse (ncons x FR) = append (reverse FR) (ncons x nnil) and
--                         append (ncons x nnil) ys = ncons x ys are both definitional).
flattenMirror : (t : Tree) -> Eq NatList (flatten (mirror t)) (reverse (flatten t)) is
  fn (t : Tree) is
    TreeElim (fn (x : Tree) is Eq NatList (flatten (mirror x)) (reverse (flatten x)) end)
      (refl nnil)
      (fn (l : Tree) (x : Nat) (r : Tree)
          (ihl : Eq NatList (flatten (mirror l)) (reverse (flatten l)))
          (ihr : Eq NatList (flatten (mirror r)) (reverse (flatten r))) is
         transL
           (append (flatten (mirror r)) (ncons x (flatten (mirror l))))
           (append (reverse (flatten r)) (ncons x (reverse (flatten l))))
           (reverse (append (flatten l) (ncons x (flatten r))))
           (transL
              (append (flatten (mirror r)) (ncons x (flatten (mirror l))))
              (append (reverse (flatten r)) (ncons x (flatten (mirror l))))
              (append (reverse (flatten r)) (ncons x (reverse (flatten l))))
              (congApp1 (flatten (mirror r)) (reverse (flatten r)) (ncons x (flatten (mirror l))) ihr)
              (congApp2 (reverse (flatten r)) (ncons x (flatten (mirror l))) (ncons x (reverse (flatten l)))
                 (congCons x (flatten (mirror l)) (reverse (flatten l)) ihl)))
           (symL
              (reverse (append (flatten l) (ncons x (flatten r))))
              (append (reverse (flatten r)) (ncons x (reverse (flatten l))))
              (transL
                 (reverse (append (flatten l) (ncons x (flatten r))))
                 (append (append (reverse (flatten r)) (ncons x nnil)) (reverse (flatten l)))
                 (append (reverse (flatten r)) (ncons x (reverse (flatten l))))
                 (revAppend (flatten l) (ncons x (flatten r)))
                 (appendAssoc (reverse (flatten r)) (ncons x nnil) (reverse (flatten l)))))
       end)
      t
  end
end

-- witnesses (computational): an asymmetric tree, in-order [1,2,3]; mirror's in-order is [3,2,1].
headOr0 : NatList -> Nat is
  fn (xs : NatList) is
    NatListElim (fn (w : NatList) is Nat end) zero (fn (h : Nat) (rest : NatList) (ih : Nat) is h end) xs
  end
end
sample : Tree is node (node leaf (succ zero) leaf) (succ (succ zero)) (node leaf (succ (succ (succ zero))) leaf) end
headF  : Nat is headOr0 (flatten sample) end
headFM : Nat is headOr0 (flatten (mirror sample)) end
headRF : Nat is headOr0 (reverse (flatten sample)) end
```

- [ ] **Step 2: Verify the whole listing elaborates + checks (the proof IS the typecheck)**

Run: `go run ./cmd/rune emit listings/ch540_tree_mirror_reverse.rune flattenMirror`
Expected: PASS -- prints JavaScript whose tail includes `const revAppend = $unit;`, `const flattenMirror = $unit;` (the Eq-valued lemmas/theorem erase to unit) and `console.log($show(flattenMirror));`. No `expected ... got ...` type error, exit 0. (If you instead see the CLI usage banner, you typed `check` -- there is no such subcommand; use `emit`.)

- [ ] **Step 3: Verify the theorem is NON-VACUOUS -- mirror genuinely reorders, both sides agree**

Run: `go run ./cmd/rune run listings/ch540_tree_mirror_reverse.rune headF`
Expected: `1` (in-order first key of the asymmetric sample)

Run: `go run ./cmd/rune run listings/ch540_tree_mirror_reverse.rune headFM`
Expected: `3` (first key after mirroring = original last key)

Run: `go run ./cmd/rune run listings/ch540_tree_mirror_reverse.rune headRF`
Expected: `3` (first key of the reversed flatten)

(`headFM` = `headRF` = 3 confirms `flatten (mirror sample)` and `reverse (flatten sample)` agree as the theorem claims; `headF` = 1 differs, confirming mirror actually reordered the keys -- so the proof is not vacuously over a palindrome.)

- [ ] **Step 4: Run the corpus gate for the new file**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck/ch540_tree_mirror_reverse.rune' -count=1 -v`
Expected: `--- PASS: TestListingsElaborateAndCheck/ch540_tree_mirror_reverse.rune` then `ok  goforge.dev/rune/v3/harness`.

- [ ] **Step 5: Run the FULL elaborate-and-check gate (no regression)**

Run: `go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1`
Expected: `ok  goforge.dev/rune/v3/harness` (every chapter still checks).

- [ ] **Step 6: Commit**

```bash
git add listings/ch540_tree_mirror_reverse.rune
git commit -m "$(printf 'feat(listings): ch540 mirror reverses the traversal (Tree<->List)\n\nIn-order flatten : Tree -> NatList, with flatten (mirror t) = reverse\n(flatten t) by structural induction (node step bridges both sides through\nappend (reverse FR) (ncons x (reverse FL)) via the reverse anti-homomorphism\nrevAppend + append associativity + both subtree IHs). Uses in-order flatten\n(the pre-order ch539 flatten does not satisfy the law). Proof-only plus\nasymmetric-tree witnesses (headFM = headRF = 3, headF = 1).\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** The single requirement -- a genuinely new proven Tree<->List theorem (`flatten (mirror t) = reverse (flatten t)`) gated by the corpus -- is implemented by Task 1 and verified green in scratch before this plan was written (`emit` clean, witnesses `headF`=1 / `headFM`=3 / `headRF`=3). Covered.

**2. Placeholder scan.** No TBD/TODO/"handle edge cases"/"similar to". The full listing body is inline and complete; every command has an exact expected output.

**3. Type consistency.** Names are internally consistent and match the verified prototype: `Tree`/`node : Tree -> Nat -> Tree`, `NatList`/`ncons`, `append`/`reverse`/`flatten`/`mirror`, toolkit `symL`/`transL`/`congCons`/`congApp1`/`congApp2`, lemmas `appendNilR`/`appendAssoc`/`revAppend`, theorem `flattenMirror`, witnesses `headOr0`/`sample`/`headF`/`headFM`/`headRF`. `TreeElim` node case binds `(l)(x)(r)(ihl)(ihr)`; `NatListElim` cons case binds `(h)(rest)(ih)`. `congApp2` (second-argument append congruence) is defined here because ch73's polymorphic toolkit only ships `congApp1`; the node-case midpoint bridge needs both.
