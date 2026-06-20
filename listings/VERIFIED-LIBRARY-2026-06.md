# Verified library — 2026-06-20 session (ch220–ch302)

A navigable index of the listings added in the 2026-06-20 autonomous session
(tags v3.25.0–v3.111.0+). Two strands: **D3/D4 machine-numeric + interop**
(runnable, cross-backend, contract-guarded) and the **proven-tier stdlib**
(proof-only, machine-checked by `TestListingsElaborateAndCheck`).

Conventions for the proof listings: `Eq`/`refl`/`subst` are ambient; the
equality toolkit (`cong`, `symEq`, `transEq`, `cong2`) is derived from `subst`;
proofs are by `…Elim` induction with `refl` bases and `cong`/`trans` steps.

## Summary

~80 listings spanning two teloi. **Telos 3 (stdlib):** the full D3 float kit
(+,-,*,/,abs,cmp,sqrt,pow) + BLAS (dot/matrix) + the D4 NumPy interop suite
(dot/mean/matmul/var/max/norm) behind tolerance contracts, byte-identical across
7 backends; the furnace property-test spine; and an exhaustive **proven tier** —
the **Nat commutative semiring** (add/mul comm+assoc, distributivity, identities),
a **complete boolean algebra** (and/or/not/xor, De Morgan, lattice laws), the
**functor/monad zoo** (List, Option, Sum, Prod, Tree, all laws), monus, exponent,
parity, the decidable **leb preorder + totality + min/max lattice bounds**, list
combinator laws (append/reverse/take/drop/concat/elem/fold), a verified **Map**
(lookup-after-insert), **insertion-sort correctness** (length + sum = permutation),
and a verified **compiler toolchain** (rewrites, constant-folding, a stack-machine
compiler, the composed optimize→compile pipeline). **Telos 4 (distributed):**
CvRDT convergence (flag semilattice, product, lattice order) and a finite trace
bisimulation.

**Substrate limits found** (bound the proof space): no large elimination into `U`,
no zero-constructor empty type, no scrutinee-exposing `case` (non-dependent
`BoolElim`), no user-defined indexed families (GADTs), and only structural /
eliminator recursion (no general/well-founded recursion — so e.g. merge-sort is
inexpressible). Reserved/builtin names to avoid as user identifiers: `Eq`, `U`,
`Dec`, `Pair`/`mkPair`, `inc`, `loop`.

## D3 — machine floats + BLAS (runnable, all backends unless noted)

- `ch220_gemm_matrix` — matrix BLAS `gemmSum` (cblas_dgemm, C/LLVM) + portable triple-loop floor.
- `ch221_numpy_interop` — `npDot` behind a tolerance contract: NumPy / OpenBLAS / reference.
- `ch222_numpy_mean` — `npMean` (numpy.mean) + hand floor.
- `ch223_numpy_matmul` — `npMatSum` (numpy matmul, 2-D marshalling) / cblas_dgemm.
- `ch224_numpy_var` — `npVar` (numpy.var, 2-pass reference).
- `ch225_numpy_max` — `npMax` (order reduction).
- `ch226_float_sqrt` — `fsqrt` primitive (host sqrt; native -lm).
- `ch227_numpy_norm` — `npNorm` (numpy.linalg.norm), reference via fsqrt.
- `ch228_numpy_pipeline` — capstone: mean → center → norm, contract-guarded.
- `ch229_float_pow` — `fpow` primitive (host pow).

## D4/furnace — property-testing foreign code (Savage)

- `ch230_furnace_float_laws` — float laws (pow b 2 ~ b*b; sqrt(b*b) ~ b; sqrt(pow b 2) ~ b).
- `ch231_furnace_numpy` — discriminating: a FALSE law reports 0 (the furnace catches the lie).
- `ch232_furnace_tier_bridge` — fast foreign (npDot) vs EXACT proven Nat arithmetic agree.

## Proven tier — shapes (the R-INTEROP "shapes proven, not checked" headline)

- `ch233_shape_proven` — `safeDot` needs `Eq Nat (len xs) (len ys)`; ragged call = compile error.
- `ch234_shape_preserved` — `len (mapInc xs) = len xs` (output shape is a theorem).

## Proven tier — list algebra (functor / monoid / foldable)

- `ch235_append_length` — `len (xs ++ ys) = len xs + len ys`.
- `ch236_list_monoid` — append right-identity + associativity (NatList is a monoid).
- `ch237_map_length` — `len (mapList f xs) = len xs` (any f).
- `ch238_map_fusion` — `mapList g (mapList f xs) = mapList (g . f) xs`.
- `ch295_map_id` — list functor identity: `mapList id xs = xs` (with ch238, the functor laws).
- `ch239_sum_homomorphism` — `sum (xs ++ ys) = sum xs + sum ys`.
- `ch297_prod_append` — `prod (xs ++ ys) = prod xs * prod ys` (product homomorphism).
- `ch240_map_append` — `mapList f (xs ++ ys) = mapList f xs ++ mapList f ys`.
- `ch249_safe_head` — total `safeHead` via a non-emptiness proof (runnable, all backends).
- `ch259_all_append` — `all p (xs ++ ys) = all p xs && all p ys` (homomorphism).
- `ch291_any_all_demorgan` — quantifier De Morgan: `any p xs = not (all (not . p) xs)`.
- `ch262_reverse_length` — `len (reverse xs) = len xs`.
- `ch292_reverse_involution` — `reverse (append xs ys) = reverse ys ++ reverse xs` and
  `reverse (reverse xs) = xs` (reverse distribution + involution).
- `ch268_sum_replicate` — `sum (replicate n x) = n * x`.
- `ch279_take_drop` — split-join: `(take n xs) ++ (drop n xs) = xs`.
- `ch280_reverse_acc` — accumulator reverse equals naive reverse (`revAcc xs nnil = reverse xs`),
  via the generalized spec `revAcc xs acc = reverse xs ++ acc` — an optimization-correctness result.
- `ch281_map_take` — naturality: `take n (map f xs) = map f (take n xs)`.
- `ch293_map_reverse` — naturality: `map f (reverse xs) = reverse (map f xs)`.
- `ch294_sum_reverse` — `sum (reverse xs) = sum xs` (reverse preserves the sum).
- `ch285_foldl_sum` — left fold equals right fold for addition (`sumL 0 xs = sum xs`),
  via `sumL acc xs = acc + sum xs` — fold-direction independence for the additive monoid.
- `ch286_concat_length` — `len (concat xss) = sumLens xss` (flatten length = sum of lengths).
- `ch288_elem_snoc` — appending x makes it a member: `elem x (snoc xs x) = true`.

## Proven tier — a verified map (association list)

- `ch278_assoc_lookup` — `lookup k (insert k v m) = some v` (lookup-after-insert),
  via `beqNat` reflexivity transported into the lookup's key comparison.

## Proven tier — sorting correctness (insertion sort)

- `ch264_insert_length` — `len (insert x xs) = succ (len xs)` (insert adds one).
- `ch265_insert_sum` — `sum (insert x xs) = x + sum xs` (insert preserves the multiset).
- `ch266_sort_sum` — `sum (insertSort xs) = sum xs` (insertion sort is a permutation).
- `ch289_sort_length` — `len (insertSort xs) = len xs` (sort preserves length).

## Proven tier — trees

- `ch244_tree_mirror` — `mirror (mirror t) = t`.
- `ch246_tree_size_mirror` — `size (mirror t) = size t` (uses addComm).
- `ch260_flatten_size` — `len (flatten t) = size t` (inorder length = size).
- `ch267_tree_functor` — tree functor: `tmap id = id` + `size (tmap f t) = size t`.
- `ch290_tree_map_fusion` — tree functor composition: `tmap g (tmap f t) = tmap (g . f) t`.

## Proven tier — arithmetic & order

- `ch245_add_comm` — `addZeroR`, `addSuccR`, `addComm`.
- `ch250_parity_double` — `even (double n) = true` (+ not-involution).
- `ch251_nat_order` — `leb` reflexive + add-monotone.
- `ch252_leb_trans` — `leb` transitive (Boolean explosion `everythingTrue`).
- `ch253_leb_total` — `leb` total (so a total preorder).
- `ch258_mul_identities` — `mulZeroR`, `mulOneL`, `mulOneR`.
- `ch276_pow_add` — exponentiation + the exponent law `a^(m+n) = a^m * a^n`.
- `ch282_monus` — truncated subtraction: `monus n 0 = n`, `monus n n = 0`.
- `ch298_max_idem` — maximum: `natMax a a = a`, `natMax a 0 = a` (idempotence + identity).
- `ch303_monus_leb` — leb weakening (`a<=b -> a<=succ b`) + the monus bound (`monus a b <= a`).
- `ch304_leb_add_mono` — <= monotone in the bound: `a <= b -> a <= b + c` (subst transport).
- `ch305_leb_add_left_mono` — addition monotone on the left: `a <= b -> c+a <= c+b`.
- `ch299_min_idem` — minimum: `natMin a a = a`, `natMin a 0 = 0` (idempotence + zero-absorption).
- `ch300_leb_max` — max is an upper bound: `a <= max a b` (structural max unblocks the bound proof).
- `ch301_leb_min` — min is a lower bound: `min a b <= a` (the dual; min/max lattice bounds).
- `ch302_leb_minmax_r` — the other bounds: `b <= max a b` and `min a b <= b` (4 bounds complete).
- `ch283_binary_inc` — verified binary numerals: `toNat (bsucc b) = succ (toNat b)`
  (binary increment agrees with unary successor).
- `ch269_mul_comm` — `mulSuccR` and **commutativity of multiplication** (`a*b = b*a`).
- `ch270_mul_distrib` — four-term interchange + **left distributivity** (`a*(b+c) = a*b + a*c`).
- `ch271_mul_assoc` — right distributivity + **associativity of multiplication** (`(a*b)*c = a*(b*c)`)
  — the Nat commutative semiring core (comm + assoc + distrib + identities) is now hermetic.

## Proven tier — booleans & Option

- `ch247_dec_eq_bool` — Boolean equality reflects propositional equality (sound + complete).
- `ch296_beq_sym` — symmetry of the Nat equality test: `beqNat a b = beqNat b a`.
- `ch248_option_monad` — Option monad laws (left/right id, assoc).
- `ch254_option_functor` — Option functor laws (id, composition).
- `ch263_sum_bifunctor` — Sum (Either) bifunctor laws (id, composition).
- `ch284_swap_involutions` — swap is an involution on products and sums
  (`swapP (swapP p) = p`, `sswap (sswap e) = e`).
- `ch255_de_morgan` — De Morgan's laws.
- `ch256_bool_lattice` — distributivity + absorption.
- `ch257_bool_monoid` — and/or commutativity + associativity (→ full boolean algebra).
- `ch287_xor` — exclusive-or laws (`xor a a = false`, `xor a false = a`, commutativity).
- `ch306_bool_to_nat` — the 0/1 embedding: `b2n (a∧b) = min`, `b2n (a∨b) = max` (Bool→Nat lattice).

## Proven tier — verified metatheory (a tiny optimizer)

- `ch272_expr_rewrites` — expression evaluator + semantics-preserving rewrites
  (`eval (add a b) = eval (add b a)` via addComm; `eval (mul a (add b c)) =
  eval (add (a*b) (a*c))` via distribL) — verified compiler-optimization rules.
- `ch273_const_fold` — a constant-folding optimizer proven correct (`eval (fold e) = eval e`
  for all e) via value-preserving smart constructors — the canonical verified-compiler-pass.
- `ch274_rewrite_catalog` — verified algebraic rewrite rules (add/mul comm + assoc, distribute),
  each a semantics-preserving rewrite from the corresponding semiring law.
- `ch275_stack_compiler` — a verified compiler from expressions to a stack machine
  (`exec (compile e) s = eval e :: s`) — the canonical compiler-correctness theorem,
  via `execAppend` (exec distributes over code concatenation).
- `ch277_opt_compile` — the verified PIPELINE: `exec (compile (fold e)) s = eval e :: s`
  — optimizer (ch273) and compiler (ch275) compose; optimize-then-compile is correct.

## Telos 4 — distributed (CRDTs + bisimulation)

- `ch241_crdt_flag` — grow-only flag CvRDT: merge semilattice laws (convergence).
- `ch242_crdt_product` — product of CvRDTs is a CvRDT (laws lift via cong2).
- `ch243_crdt_lattice_order` — merge is the join (LUB); updates inflationary.
- `ch261_lts_bisim` — finite trace bisimulation of two differently-stated machines.

## Substrate limits found (recorded for future work)

- Zero-constructor `data` (empty type) is unsupported; the Church bottom `(A:U)->A` is U1.
- `BoolElim` cannot eliminate into `U` (no large elimination into the universe).
- ⇒ refutation-into-arbitrary-type proofs (leb antisymmetry, full `Dec`/`decEq`) are blocked;
  Boolean explosion (`everythingTrue`, into `Eq Bool` only) is the available idiom.
- JVM float bodies remain (the jvm emitter targets java-25; the local jvm is 17).
- matplotlib / scipy / pandas absent locally, so plotting + those interops are untested here.
