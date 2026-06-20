# Verified library — 2026-06-20 session (ch220–ch318)

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
- `ch327_len_append_sym` — append length is swap-symmetric: `len (xs++ys) = len (ys++xs)`.
- `ch236_list_monoid` — append right-identity + associativity (NatList is a monoid).
- `ch237_map_length` — `len (mapList f xs) = len xs` (any f).
- `ch238_map_fusion` — `mapList g (mapList f xs) = mapList (g . f) xs`.
- `ch295_map_id` — list functor identity: `mapList id xs = xs` (with ch238, the functor laws).
- `ch239_sum_homomorphism` — `sum (xs ++ ys) = sum xs + sum ys`.
- `ch297_prod_append` — `prod (xs ++ ys) = prod xs * prod ys` (product homomorphism).
- `ch240_map_append` — `mapList f (xs ++ ys) = mapList f xs ++ mapList f ys`.
- `ch344_map_snoc` — map commutes with snoc: `map f (snoc xs x) = snoc (map f xs)(f x)`.
- `ch314_sum_map_succ` — mapping succ adds the length to the sum: `sum (map succ xs) = len xs + sum xs`.
- `ch354_sum_linear` — sum is linear under scaling: `sum (map (c*) xs) = c * sum xs`.
- `ch360_sum_double_list` — `sum (xs++xs) = double (sum xs)`.
- `ch249_safe_head` — total `safeHead` via a non-emptiness proof (runnable, all backends).
- `ch330_nonempty` — non-empty list with a STRUCTURALLY total head (no precondition); conversion is never empty.
- `ch259_all_append` — `all p (xs ++ ys) = all p xs && all p ys` (homomorphism).
- `ch322_filter_length` — filter never increases length: `len (filter p xs) <= len xs`.
- `ch323_filter_append` — filter distributes over append: `filter p (xs++ys) = filter p xs ++ filter p ys`.
- `ch291_any_all_demorgan` — quantifier De Morgan: `any p xs = not (all (not . p) xs)`.
- `ch358_any_map` — any/map fusion: `any p (map f xs) = any (p . f) xs`.
- `ch359_all_map` — all/map fusion: `all p (map f xs) = all (p . f) xs`.
- `ch262_reverse_length` — `len (reverse xs) = len xs`.
- `ch325_reverse_replicate` — `reverse (replicate n x) = replicate n x` (n copies are self-reverse).
- `ch292_reverse_involution` — `reverse (append xs ys) = reverse ys ++ reverse xs` and
  `reverse (reverse xs) = xs` (reverse distribution + involution).
- `ch349_reverse_snoc` — `reverse (snoc xs x) = ncons x (reverse xs)` (snoc-reverse).
- `ch268_sum_replicate` — `sum (replicate n x) = n * x`.
- `ch340_replicate_append` — replicate additive: `replicate m x ++ replicate n x = replicate (m+n) x`.
- `ch356_prod_replicate` — product of n copies is a power: `prod (replicate n x) = pow x n`.
- `ch324_map_replicate` — `map f (replicate n x) = replicate n (f x)`.
- `ch279_take_drop` — split-join: `(take n xs) ++ (drop n xs) = xs`.
- `ch341_takewhile_dropwhile` — predicate split-join: `takeWhile p xs ++ dropWhile p xs = xs`.
- `ch320_len_drop` — drop shrinks length by n: `len (drop n xs) = monus (len xs) n`.
- `ch321_len_take` — take caps the length: `len (take n xs) = min n (len xs)`.
- `ch336_zipwith_length` — zip stops at the shorter: `len (zipWith f xs ys) = min (len xs)(len ys)`.
- `ch280_reverse_acc` — accumulator reverse equals naive reverse (`revAcc xs nnil = reverse xs`),
  via the generalized spec `revAcc xs acc = reverse xs ++ acc` — an optimization-correctness result.
- `ch281_map_take` — naturality: `take n (map f xs) = map f (take n xs)`.
- `ch385_head_map` — head is natural: `headOpt (map f xs) = omap f (headOpt xs)`.
- `ch293_map_reverse` — naturality: `map f (reverse xs) = reverse (map f xs)`.
- `ch294_sum_reverse` — `sum (reverse xs) = sum xs` (reverse preserves the sum).
- `ch355_prod_reverse` — reverse preserves the product: `prod (reverse xs) = prod xs`.
- `ch285_foldl_sum` — left fold equals right fold for addition (`sumL 0 xs = sum xs`),
  via `sumL acc xs = acc + sum xs` — fold-direction independence for the additive monoid.
- `ch361_foldr_id` — catamorphism identity: `foldr ncons nnil xs = xs`.
- `ch362_foldr_append` — foldr-append fusion: `foldr f z (xs++ys) = foldr f (foldr f z ys) xs`.
- `ch363_map_as_fold` — map as a fold: `map f xs = foldr (\h a. (f h)::a) nil xs`.
- `ch364_sum_as_fold` — sum as a fold: `sum xs = foldr natAdd 0 xs`.
- `ch286_concat_length` — `len (concat xss) = sumLens xss` (flatten length = sum of lengths).
- `ch357_sum_concat` — sum of flatten = sum of sums: `sum (concat xss) = sumAll xss`.
- `ch346_isempty_append` — empty iff both empty: `isEmpty (xs++ys) = isEmpty xs && isEmpty ys`.
- `ch288_elem_snoc` — appending x makes it a member: `elem x (snoc xs x) = true`.
- `ch312_elem_append` — membership distributes over append: `elem x (xs++ys) = elem x xs || elem x ys`.
- `ch309_count_snoc` — appending x increments its count: `countOccur x (snoc xs x) = succ (countOccur x xs)`.
- `ch310_count_append` — count is additive over append: `countOccur x (xs++ys) = countOccur x xs + countOccur x ys` (multiset homomorphism).
- `ch311_count_replicate` — `countOccur x (replicate n x) = n` (n copies contain x n times).
- `ch342_count_length` — count bounded by length: `countOccur x xs <= len xs`.
- `ch382_count_filter` — count = equality-filter length: `countOccur x xs = len (filter (beqNat x) xs)`.
  (The full sort-by-count permutation theorem is NOT reachable: it needs a count-swap lemma
  commuting two conditionals on different scrutinees, which hits the no-scrutinee-equation limit;
  sort-by-sum (ch266) + sort-by-length (ch289) are the reachable permutation evidence.)

## Proven tier — a verified queue

- `ch308_queue_size` — two-list FIFO queue: `qsize (enqueue x q) = succ (qsize q)`.

## Proven tier — a verified map (association list)

- `ch278_assoc_lookup` — `lookup k (insert k v m) = some v` (lookup-after-insert),
  via `beqNat` reflexivity transported into the lookup's key comparison.
- `ch376_beqnat_sound` — decidable equality is sound: `beqNat a b = true -> a = b` (with ch278 converse, fully characterizes =).
- `ch377_beq_implies_leb` — equal implies <=: `beqNat a b = true -> leb a b = true` (decidable eq refines order).
- `ch378_leb_monus_zero` — below means nothing to subtract: `a <= b -> monus a b = 0`.

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
- `ch337_leaves_nodes` — leaves = internal nodes + 1: `countLeaves t = succ (countNodes t)`.
- `ch338_leaves_mirror` — mirror preserves leaf count: `countLeaves (mirror t) = countLeaves t`.
- `ch326_flatten_mirror` — mirroring reverses the inorder traversal: `flatten (mirror t) = reverse (flatten t)`.
- `ch365_flatten_tmap` — flatten natural in the tree functor: `flatten (tmap f t) = map f (flatten t)`.
- `ch366_sum_flatten` — inorder sum equals tree sum: `sum (flatten t) = tsum t`.
- `ch367_tsum_mirror` — mirror preserves tree sum: `tsum (mirror t) = tsum t`.

## Proven tier — arithmetic & order

- `ch245_add_comm` — `addZeroR`, `addSuccR`, `addComm`.
- `ch250_parity_double` — `even (double n) = true` (+ not-involution).
- `ch328_double_add` — doubling equals self-addition: `double n = n + n`.
- `ch329_even_self_add` — self-sum is even: `even (n + n) = true`.
- `ch380_odd_succ_double` — succ of even is odd: `even (succ (double n)) = false`.
- `ch381_even_add` — parity of a sum is XNOR of parities: `even (a+b) = eqBool (even a)(even b)`.
- `ch251_nat_order` — `leb` reflexive + add-monotone.
- `ch334_compare_refl` — three-way compare reflexive: `compare n n = eq` (lt|eq|gt trichotomy).
- `ch335_compare_sym` — three-way compare antisymmetric: `compare a b = flip (compare b a)`.
- `ch252_leb_trans` — `leb` transitive (Boolean explosion `everythingTrue`).
- `ch253_leb_total` — `leb` total (so a total preorder).
- `ch258_mul_identities` — `mulZeroR`, `mulOneL`, `mulOneR`.
- `ch276_pow_add` — exponentiation + the exponent law `a^(m+n) = a^m * a^n`.
- `ch282_monus` — truncated subtraction: `monus n 0 = n`, `monus n n = 0`.
- `ch315_monus_add` — add-then-subtract cancels: `monus (a + b) b = a`.
- `ch316_monus_over` — over-subtraction is zero: `monus a (a + b) = 0`.
- `ch317_monus_pred` — subtracting one is predecessor: `monus n 1 = pred n`.
- `ch298_max_idem` — maximum: `natMax a a = a`, `natMax a 0 = a` (idempotence + identity).
- `ch303_monus_leb` — leb weakening (`a<=b -> a<=succ b`) + the monus bound (`monus a b <= a`).
- `ch304_leb_add_mono` — <= monotone in the bound: `a <= b -> a <= b + c` (subst transport).
- `ch305_leb_add_left_mono` — addition monotone on the left: `a <= b -> c+a <= c+b`.
- `ch339_leb_add_self` — addition only grows: `a <= a + b`.
- `ch379_leb_add_cancel` — additive cancellation: `leb (c+a)(c+b) = leb a b`.
- `ch299_min_idem` — minimum: `natMin a a = a`, `natMin a 0 = 0` (idempotence + zero-absorption).
- `ch300_leb_max` — max is an upper bound: `a <= max a b` (structural max unblocks the bound proof).
- `ch301_leb_min` — min is a lower bound: `min a b <= a` (the dual; min/max lattice bounds).
- `ch302_leb_minmax_r` — the other bounds: `b <= max a b` and `min a b <= b` (4 bounds complete).
- `ch368_min_max_sum` — min + max = a + b: `natAdd (natMin a b)(natMax a b) = natAdd a b`.
- `ch369_minmax_comm` — structural min/max ARE commutative: `min a b = min b a`, `max a b = max b a` (leb-if version was blocked; structural is not).
- `ch370_min_assoc` — min is associative: `min (min a b) c = min a (min b c)` (with comm+idem, a meet-semilattice).
- `ch371_max_assoc` — max is associative: `max (max a b) c = max a (max b c)` (join-semilattice). min/max are dual semilattices.
- `ch372_minmax_absorb` — lattice absorption: `min a (max a b) = a`, `max a (min a b) = a` (min/max form a lattice).
- `ch373_min_dist_max` — min distributes over max: `min a (max b c) = max (min a b)(min a c)` (DISTRIBUTIVE lattice).
- `ch374_leb_min` — order determines the meet: `a <= b -> min a b = a` (+ exFalsoNat: false=true explodes into any Nat eq).
- `ch375_leb_antisym` — leb ANTISYMMETRIC: `a<=b -> b<=a -> a=b` (ch253 had abandoned this; exFalsoNat unblocks it) + `a<=b -> max a b = b`. <= is a partial order (refl ch251 + trans ch252 + antisym).
- `ch283_binary_inc` — verified binary numerals: `toNat (bsucc b) = succ (toNat b)`
  (binary increment agrees with unary successor).
- `ch269_mul_comm` — `mulSuccR` and **commutativity of multiplication** (`a*b = b*a`).
- `ch270_mul_distrib` — four-term interchange + **left distributivity** (`a*(b+c) = a*b + a*c`).
- `ch271_mul_assoc` — right distributivity + **associativity of multiplication** (`(a*b)*c = a*(b*c)`)
- `ch347_pow_add` — exponents add: `pow b (m+n) = pow b m * pow b n`.
- `ch352_pow_mul` — power of a power: `pow b (m*n) = pow (pow b m) n`.
- `ch353_pow_mul_base` — power distributes over product: `pow (a*b) n = pow a n * pow b n` (+ 4-factor interchange).
- `ch348_pow_one_base` — one to any power: `pow 1 n = 1`.
- `ch351_mul_one_pow_one` — mult right identity `b*1 = b` + first power `pow b 1 = b`.
  — the Nat commutative semiring core (comm + assoc + distrib + identities) is now hermetic.

## Proven tier — booleans & Option

- `ch247_dec_eq_bool` — Boolean equality reflects propositional equality (sound + complete).
- `ch296_beq_sym` — symmetry of the Nat equality test: `beqNat a b = beqNat b a`.
- `ch248_option_monad` — Option monad laws (left/right id, assoc).
- `ch254_option_functor` — Option functor laws (id, composition).
- `ch313_option_orelse` — Option `orElse` is a monoid (none identity + associativity; the alternative structure).
- `ch318_option_bind_map` — monad/functor coherence: `bind m (some . f) = map f m`.
- `ch263_sum_bifunctor` — Sum (Either) bifunctor laws (id, composition).
- `ch319_sum_monad` — Sum as the error monad: the three monad laws.
- `ch331_option_sum_iso` — Option ≅ Sum Unit: round-trip `sumToOption (optionToSum m) = m`.
- `ch383_option_some_none` — some-or-none, not both: `isSome m || isNone m = true`, `isSome m && isNone m = false`.
- `ch384_option_some_map` — `isNone = not isSome` + map preserves definedness `isSome (omap f m) = isSome m`.
- `ch332_sum_option_iso` — the other direction: `optionToSum (sumToOption s) = s` (full iso, unit eta).
- `ch333_sum_swap` — Sum symmetry is involutive: `swap (swap s) = s` (Sum A B ≅ Sum B A).
- `ch388_prod_swap` — product swap involutive: `swap (swap p) = p` (Prod A B ≅ Prod B A).
- `ch389_curry` — curry/uncurry mutually inverse: `(Prod A B -> C) ≅ (A -> B -> C)`.
- `ch284_swap_involutions` — swap is an involution on products and sums
  (`swapP (swapP p) = p`, `sswap (sswap e) = e`).
- `ch255_de_morgan` — De Morgan's laws.
- `ch256_bool_lattice` — distributivity + absorption.
- `ch257_bool_monoid` — and/or commutativity + associativity (→ full boolean algebra).
- `ch287_xor` — exclusive-or laws (`xor a a = false`, `xor a false = a`, commutativity).
- `ch343_eqbool` — boolean equality (XNOR) reflexive + symmetric: `eqBool a a = true`, `eqBool a b = eqBool b a`.
- `ch345_implb` — boolean implication: `implb a a = true`, `implb a b = ¬a ∨ b`.
- `ch350_bool_lem` — excluded middle + non-contradiction: `b || ¬b = true`, `b && ¬b = false`.
- `ch386_bool_distrib` — boolean distributive laws: `a&&(b||c) = (a&&b)||(a&&c)`, and dual.
- `ch387_bool_absorb` — boolean absorption: `a&&(a||b) = a`, `a||(a&&b) = a` (Bool is a boolean algebra).
- `ch306_bool_to_nat` — the 0/1 embedding: `b2n (a∧b) = min`, `b2n (a∨b) = max` (Bool→Nat lattice).
- `ch307_bool_nat_roundtrip` — `n2b (b2n b) = b` (left inverse) + `b2n (¬b) = 1 - b2n b`.

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
