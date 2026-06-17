# R-NUM — compressed core numerals + kernel-accelerated arithmetic

> Scope note. This node is about the **OUTER** core (the strict OTT kernel:
> `core/term.go`, `core/val.go`, `core/eval.go`, `core/conv.go`, `core/hash.go`)
> *and* the elaborator's numeral lowering (`surface/numeral.go`). It is a
> **deliberate, last-resort kernel change** — it adds one core `Tm` constructor
> (a literal carrying a `*big.Int`) and an opt-in kernel reduction table. The
> roadmap previously named only C-OVERLAP and R-SUM as qualifying kernel changes
> (`humble-humming-elephant.md:175`); R-NUM is the **third**, justified by a real
> UX wall the roadmap's own numeric tower (D1) hits. It is the DAG node **C7**.

## Problem (what's stuck/absent today, with file:line)

Two distinct deficiencies — exactly the user's two complaints (the `:core`
representation, and the arithmetic):

1. **The unary `builtin nat` caps at 4096, and past it elaboration just errors.**
   `surface/numeral.go:35` `const NatMax = 1 << 12`; `numeral.go:42-44` raises
   *"numeral N is too large for the unary `builtin nat` (cap N); declare a
   `builtin bin` and give it a binary type."* The cap is real and load-bearing: a
   unary literal is **nothing but its succ-chain** (`GRAMMAR.md:266-267`), so `n`
   is `n` nested `App{succ, …}` nodes — `1000` is a 1000-deep term. Past ~4096 the
   checker stalls (`numeral.go:32-34`). This is poor UX: a bare `5000` is a
   compile error with no recourse but to hand-wire a binary type.

2. **Even WITHIN the cap, kernel arithmetic is structural recursion over the
   succ-chain, so it blows up.** `add`/`mul` are defined by the Nat eliminator
   (`NatElim`, the Phase-4 ι-rule), which recurses on the scrutinee's succ-chain:
   `add a b` is O(a) eliminator steps building an O(a+b)-node result; `mul a b` is
   O(a·b). So `mul 4096 4096` materialises ~16.7M succ nodes in the NbE domain —
   the "4096 × 4096 killing my perfectly good computer" the user reports. This is
   *separate* from the cap: it is the cost of computing on unary at all, and it
   bites every D1 proof obligation that forces a numeral
   (`addAssoc`/`mulDistR`, ch63 — `CLAUDE.md` D1 line).

3. **The binary `builtin bin` stopgap removed the CAP but not the arithmetic
   blowup, and is not the default.** `numeral.go:65` `Bin` lowers a numeral
   checked at a `builtin bin BN …` type to an O(log n) bit-spine (no cap,
   `PARKING-LOT.md:201-213`, 2026-06). But (a) it is still a constructor SPINE,
   not a single node; (b) there are **no arithmetic ι-rules on it** — `add`/`mul`
   over a `bin` type are still the user's structural recursion over the spine, no
   faster in the kernel than unary per digit; and (c) the **untyped resolver still
   lowers a bare numeral to the unary default** (`GRAMMAR.md:268-271`), so plain
   `5000` STILL hits the cap unless the user declares a binary type and annotates.
   It fixed literal *size* for declared-binary types; it fixed neither default UX
   nor kernel arithmetic.

4. **The real fix — a compressed core numeral — is explicitly parked.**
   `PARKING-LOT.md:209-213`: *"A TRUE compressed core numeral (a core Tm node
   carrying a bigint, with a hash-format bump) is still parked — unneeded now that
   binary literals carry big constants compactly."* That "unneeded" is what this
   node revisits: binary carries big *constants* compactly but does nothing for
   *arithmetic*, and a bare numeral still can't exceed 4096. Pretty also never
   folds a binary spine back to a digit on output (`PARKING-LOT.md:211-212`,
   cosmetic-but-bad-UX).

The honest conclusion: the contained patterns cannot carry a `*big.Int` payload
(builtin-group members are *bodiless fixed-hash refs* — they cannot hold a varying
value; the unary/binary spines ARE the blowup). A compressed literal therefore
needs a **core `Tm` node carrying the integer**, and fast arithmetic needs a
**kernel reduction** on that node. That is the minimal Thompson justification for
touching the kernel — the same bar R-SUM cleared for Σ.

## Prior art (what the literature/other systems do; cite)

- **Lean 4 — GMP-backed `Nat`/`Int` literals with kernel-accelerated arithmetic;
  STILL the inductive type.** `Nat` is the ordinary inductive `zero`/`succ`, but a
  literal is stored as an arbitrary-precision GMP integer, and the kernel
  **special-cases** `Nat.add`, `Nat.mul`, `Nat.sub`, `Nat.div`, `Nat.mod`,
  `Nat.decEq`, `Nat.ble`/`Nat.blt`, `Nat.pow`, … to compute on the GMP value
  rather than unfolding `Nat.rec` — O(1)-ish bigint ops instead of O(n) peels.
  Crucially the literal is *definitionally* the succ-chain: `0 ≡ Nat.zero`, the
  literal `n+1 ≡ Nat.succ n`, so `Nat.rec` still fires and **every induction proof
  transfers unchanged**. The accelerated reductions are part of the kernel and are
  trusted to agree with the recursive definitions — the exact thing R-NUM
  property- and mutation-tests rather than trusts. This is the model R-NUM adopts.
  ([Lean kernel `Nat`/`Int` literals + reductions: `src/kernel/type_checker.cpp`,
  `library/Init/Data/Nat/Basic.lean`](https://github.com/leanprover/lean4/blob/master/src/kernel/type_checker.cpp),
  [Lean4Lean — verifying the kernel incl. the Nat/Int accelerations](https://arxiv.org/pdf/2403.14064),
  [Carneiro, "The Type Theory of Lean"](https://github.com/digama0/lean-type-theory/releases))

- **Coq — primitive integers `Uint63`/`PrimInt63` + `PrimFloat`, and the binary
  `N`/`Z` tower.** Coq grew genuine 63-bit machine ints as an axiomatic type with
  kernel reduction rules, *proven equivalent to a `Z` model*, precisely so
  reflection-heavy proofs compute without unary blowup; bignums (`bignums`
  library, formerly `BigN`/`BigZ`) are trees of those words. The stdlib `N`/`Z` are
  binary positives with `Number Notation` bridging digit syntax to the term. The
  Coq lesson is twofold: **(a)** a fast representation must carry a *proven bridge*
  to the inductive model so existing proofs apply (R-NUM keeps the bridge `refl`
  by accelerating the *same* function, not a parallel one); **(b)** `vm_compute`/
  `native_compute` exist *because* structural reduction in the kernel is too slow —
  R-NUM moves the fast path *into* the kernel for the Nat case rather than
  compiling out to OCaml.
  ([Coq primitive integers](https://coq.inria.fr/doc/master/refman/language/core/primitive.html),
  [Armand et al., "A modular integration of SAT/SMT solvers to Coq" — native ints rationale](https://hal.inria.fr/hal-00690044),
  [Number Notation](https://coq.inria.fr/doc/master/refman/user-extensions/syntax-extensions.html#number-notations))

- **Agda — `BUILTIN NATURAL` + machine-int back-end for literals.** Agda's
  `Agda.Builtin.Nat` is the inductive `zero`/`suc`, but the compiler/evaluator
  represents `BUILTIN NATURAL` literals as GMP integers and maps the registered
  `NATPLUS`/`NATTIMES`/`NATMINUS`/`NATEQUALS`/`NATLESS`/`NATDIVSUCAUX`/`NATMODSUCAUX`
  builtins to native operations — same "inductive type, native runtime + evaluator
  representation, registered ops" pattern, registration keyed by a `BUILTIN`
  pragma (R-NUM's opt-in annotation is the content-addressed analogue).
  ([Agda built-ins — Natural numbers](https://agda.readthedocs.io/en/latest/language/built-ins.html#natural-numbers))

- **GMP / `math/big`.** The payload is a `*big.Int` (`math/big`, already an
  indirect-free stdlib package — no new dependency, unlike `rapid`/`blake3sum`,
  `CLAUDE.md` engineering conventions). Bigint add/mul/divmod are the primitives
  the ι-rules call.

The convergent verdict across Lean/Coq/Agda: **keep the inductive type, give it a
machine-integer literal, accelerate a fixed set of operations in the kernel, and
keep a (here: definitional) bridge so proofs are unaffected.**

## Chosen approach for THIS substrate (concrete; respects containment)

### Decision 0 — one core literal node `NatLit{ *big.Int }`, definitionally the succ-chain.

Add a single `Tm` constructor (append-only, after `tagSnd`):

```go
type NatLit struct { N *big.Int }   // a compressed succ^N zero
func (NatLit) isTm() {}
```

with the canonical value `VNatLit{ N *big.Int }`. It is tied to the `builtin nat`
zero/succ by **eval ι-rules** so it is *literally* the unary value, just folded:

- `succ (NatLit n) ~> NatLit (n+1)` — folding (the succ ref is the session's
  `builtin nat` succ; recognised by hash, like the numeral resolver already does,
  `numeral.go` `NumConfig.NatSucc`).
- `zero ≡ NatLit 0` in conversion (and the resolver emits `NatLit 0` for `0`).
- **The eliminator peels a literal lazily.** `NatElim … (NatLit k)` with `k>0`
  fires the **succ** branch with the IH on `NatLit (k-1)`; with `k=0` fires the
  **zero** branch (`core/eval.go`, the Phase-4 ι site). One layer per step — the
  literal stays compressed, the chain is never materialised, and **defeq with the
  unary form is preserved** (so D1's `NatElim`-based proofs transfer verbatim).

Decision 0 alone fixes deficiency **1** (no cap — a literal is one node of any
size) and makes every eliminator *terminate without materialising the chain*. It
does **not** by itself fix deficiency **2**: lazy peeling of `mul 4096 4096` is
still O(a·b) *steps* (cheap each, but 16.7M of them). That is Decision 1.

### Decision 1 — an opt-in kernel acceleration table: fixed ops compute on the payload in one bigint step.

A definition may be tagged as a kernel-accelerated arithmetic op via a
`builtin`-style annotation (mirroring `builtin nat`/`builtin bin`,
`numeral.go` `NumConfig`, validated by TYPE not shape — the rung-C4 discipline,
`CLAUDE.md` §5.5):

```
builtin natAdd add    -- "accelerate this def as bignum addition"
builtin natMul mul
builtin natSub sub    -- truncated subtraction (monus)
builtin natDivMod divmod
builtin natEq  eqNat   -- decidable equality  -> Bool
builtin natLe  leNat   -- decidable order     -> Bool
builtin natPow pow
```

Each binds a **session table keyed by the def's content hash** (not its name —
content-addressing, `CLAUDE.md` Architecture). The kernel adds a fast-path ι-rule
in `core/eval.go` that fires **only when every argument forces to a `VNatLit`**:

```
add  (NatLit a) (NatLit b) ~> NatLit (a+b)      -- big.Int.Add
mul  (NatLit a) (NatLit b) ~> NatLit (a*b)      -- big.Int.Mul
le   (NatLit a) (NatLit b) ~> true | false      -- big.Int.Cmp
…
```

On any **neutral** argument the rule does **not** fire — the def reduces by its
ordinary recursive body / eliminator (Decision 0's peeling), so the firewall holds
(the normalizer cannot diverge or get stuck differently than today) and open-term
reasoning is unchanged. The acceleration is a *short-circuit of the function's own
reduction on closed literal inputs*, never a new function.

**Why this keeps the bridge `refl` (the soundness crux).** There is no separate
`natAdd` vs `add`: `builtin natAdd add` accelerates **`add` itself**. So
`natAddCorrect : (a b) -> Eq Nat (natAdd a b) (add a b)` is `refl` — there is
nothing to prove, because it is the same definition. The *only* trust obligation
is that `NatLit (a+b)` equals what `add`'s recursive body would peel to — and that
is **property-tested and mutation-tested** (Decision below), not assumed. This is
strictly safer than Lean's "trust the kernel special-case": the accelerated op is
the user's *own* def, the kernel verifies its TYPE (`Nat -> Nat -> Nat` etc.) at
the `builtin` site, and the differential test pins agreement with the unfolding on
random literals.

### Decision 2 — bare numerals lower to `NatLit` by DEFAULT; the cap is gone for everyone.

`surface/numeral.go`: the untyped/default lowering emits `NatLit n` (not a
succ-chain). The expected-type lowering still chooses per literal: at `Nat` →
`NatLit`; a user `builtin bin` type → that type's lowering (but see Decision 5 —
`builtin bin` is retired). `NatMax` and the `numeral.go:42-44` error are
**deleted**. A bare `5000`, `4096 * 4096`, `factorial 20` all just elaborate and
compute. This is the "good UX" the request asks for.

**Hash impact (priced in).** The default lowering changing from unary succ-chain
to `NatLit` is a content-hash event for every numeral-bearing def
(`GRAMMAR.md:268-271`'s "resolver lowers to unary default, so existing hashes are
unchanged" stops holding — by design). Plus the new `Tm` tag bumps
`hashFormatVersion 0x05 → 0x06` (`core/hash.go:34`). One-time cache nuke, the same
event Phase 6 and R-SUM each priced in — Rule 5: take the hash event, do not
defer.

### Decision 3 — pretty folds back to a digit (the cosmetic half of "good UX").

`VNatLit`/`NatLit` pretty-prints its `big.Int` directly; a residual
`succ (succ … zero)` over a `NatLit` base folds to a digit on output (nat chains
already fold, `PARKING-LOT.md:212`). This retires the binary-spine-doesn't-fold
gap and means inputs and outputs are both ordinary digits.

### Decision 4 — erasure/codegen: a literal IS the target's native integer.

`NatLit n` erases to the backend's native big/native integer and the accelerated
ops to native arithmetic — `BigInt` (JS), `math/big` or `int64` (Go),
`i128`/`num-bigint` (Rust), native (BEAM/JVM). This is the user's "it'll be better
when lowered to the target" — but now it is automatic *and* the kernel itself no
longer blows up before you ever reach codegen. The existing nat fast-path in the
backends (`codegen/*.go`, the `$show`/nat path noted in B3) generalises to read
the literal payload.

### Decision 5 — retire `builtin bin` (Rule 5: delete the superseded).

`NatLit` + the acceleration table **subsume** the binary literal entirely: the
no-cap benefit (a literal is one node), *and* fast arithmetic (which `bin` never
had), *and* default-applicability (which `bin` never had). The less-fundamental
artifact is dead weight that misleads future readers into thinking it is
load-bearing (`CLAUDE.md` Rule 5, the `ua`/`uaGlue` precedent). So R-NUM
**deletes** `builtin bin` / `NumConfig.Bin`/`pos`/`PosLit` (`numeral.go:54-79`)
and migrates its sole consumer — ch14's hand-written `Pos` constants
(`listings/ch14_binary.rune:429`, the showcase) — to `NatLit`, taking the
deliberate hash event. (If a genuinely *non-Nat* binary positive is later needed
as a distinct type, it returns as a library `data Pos`, not a parser special-case.)

### Decision 6 — scope is Nat; Int/Rational/Float ride the same mechanism later (parked).

The literal node can generalise to a tagged payload (Nat/Int) but ships **Nat
only** — that is the 4096 wall and the D1 semiring substrate. `Int` (signed),
`Rational`, and machine `Float`/`Double` are D3 (`humble-humming-elephant.md:318`)
and reuse the identical "literal node + accel table" pattern; park them
(`PARKING-LOT.md`, add-no-feature-with-no-consumer, Standing Rule 1).

## Interfaces & signatures to add

### core/term.go, core/val.go
```go
// term.go — append-only after Snd
type NatLit struct { N *big.Int }
func (NatLit) isTm() {}
// val.go
type VNatLit struct { N *big.Int }
func (VNatLit) isVal() {}
```
`usesVar` (literals mention no var → false), `pretty` (print `N`), the resolver,
quote (`VNatLit → NatLit`) all gain the trivial case.

### core/eval.go
```go
case NatLit: return VNatLit{N: tm.N}
// in apply/ι, recognising the session's builtin-nat succ ref:
//   succ (VNatLit n)           ~> VNatLit n+1
// in NatElim ι (Phase-4 site):
//   elim P z s (VNatLit 0)     ~> z
//   elim P z s (VNatLit k>0)   ~> s (VNatLit k-1) (elim P z s (VNatLit k-1))
// accel table (Decision 1), fires only when ALL args are VNatLit:
//   <addHash> (VNatLit a)(VNatLit b) ~> VNatLit (new(big.Int).Add(a,b))
//   … mul/sub(monus)/divmod/eq/le/pow, each guarded on VNatLit args, else fall
//     through to the def's ordinary body.
```
The accel lookup is a hash-set membership test on the applied head — O(1), and the
guard "all args are `VNatLit`" means it never fires on open terms, so conversion
of open numeric goals is byte-identical to today.

### core/conv.go
```go
// VNatLit vs VNatLit: big.Int.Cmp == 0.
// VNatLit n  vs  VNeu(succ …) / VNeu(zero): peel/normalise one side to compare
//   (zero ≡ NatLit 0; succ x ≡ NatLit (k+1) when x ≡ NatLit k) — so a literal and
//   a hand-written succ-chain are interconvertible (defeq preserved).
```

### core/hash.go
`tagNatLit` appended after `tagSnd`; the preimage hashes the `big.Int` bytes
(canonical big-endian, sign-free for Nat). Bump `hashFormatVersion 0x05 → 0x06`
(`hash.go:34`). The builtin-group `defFormatVersion` is untouched **except** any
member type that embeds a numeral — none do today, so fib/interval/path/… stay
byte-identical (verify with the hash-stability regression).

### surface/numeral.go (rewrite)
`NumConfig.Nat` emits `NatLit n` (no cap, delete `NatMax`/`:42-44`); the
default/untyped path uses it; **delete** `Bin`/`pos`/`PosLit` (Decision 5). The
`builtin natAdd`/`natMul`/… declarations register the accel table (new, alongside
the existing `builtin nat` handling).

## Worked micro-example (the teachable artifact)

```
-- no declaration, no cap, no blowup:
big   : Nat is 4096 * 4096          -- elaborates to NatLit 16777216 in ONE bigint
                                    -- mul; today: a 16.7M-node succ tree (or a cap
                                    -- error at 4096) — the program the user wrote.

-- accel is the SAME function, so proofs are untouched (bridge = refl):
fast  : Eq Nat (add 4096 4096) (8192) is refl 8192   -- both sides ~> NatLit 8192

-- open-term reasoning is byte-identical to today (accel does NOT fire on a var):
lemma : (n : Nat) -> Eq Nat (add zero n) n is
  fn (n : Nat) is refl n end        -- add reduces by its ordinary body on neutral n
```
The lesson (Savage): *"a literal is just `succ`-many `succ`s written compactly;
the kernel may add them with a machine adder instead of one-at-a-time, but only
when it can SEE both numbers — so closed arithmetic is fast and open proofs are
exactly as before."*

## Risks / open sub-questions

1. **The acceleration table is a TCB-adjacent trust point (the metatheory-review
   event).** A wrong accel rule (`add`'s ι returning `a*b`) would be unsound.
   Mitigation, layered: **(a)** the accel op is the user's *own* def, type-checked
   at the `builtin` site (`Nat -> Nat -> Nat`, the rung-C4 type validation);
   **(b)** a **differential property test** — for random literals `a,b`,
   `accel(NatLit a)(NatLit b)` must equal the def's *unfolded* (peeled) result;
   **(c)** **mutation-test** the guard and the bigint call (a surviving mutant in
   the literal-only guard or the `big.Int` op is a soundness hole), exactly the
   bar R-SUM set for η. This is the single genuine review event, called out as
   such (`humble-humming-elephant.md:175`).

2. **Defeq integrity of peeling.** The literal↔succ-chain interconversion
   (Decision 0 / conv.go) must be confluent with the eliminator peel or conversion
   could disagree on `succ (NatLit k)` vs `NatLit (k+1)`. Mitigation: canonicalise
   to `NatLit` on both sides before comparing; property: for all `k`,
   `Conv (succ (NatLit k)) (NatLit (k+1))` and `Conv zero (NatLit 0)`.

3. **Hash bump + default-lowering change blast radius.** 0x06 nukes the cache and
   *every numeral-bearing def re-hashes* (the default lowering changed, Decision
   2). Priced in (Rule 5). Gate: confirm no shipped builtin member type embeds a
   numeral so the `defFormatVersion` groups stay byte-stable (a `grep` of
   `store/*.go` `*Types()`; none today).

4. **`big.Int` allocation pressure.** Accel returns fresh `*big.Int`s; in a tight
   proof these churn. Mitigation: it is still vastly less than the succ-tree it
   replaces, and the values are immutable/shareable; a small-int (`int64`)
   fast-lane inside `VNatLit` is a parked optimisation (Standing Rule 1) if a
   profile demands it.

5. **Interaction with QTT / erasure.** A `NatLit` is data, runs, is not
   inner-tainted; it erases to a native integer (Decision 4) with no quantity
   subtlety (it binds nothing). Straightforward.

6. **Which ops to accelerate.** Recommend the closed set add/mul/monus/divmod/
   eq/le/pow — everything D1's semiring + decidable order
   (`CLAUDE.md` D1 line: `mul`/`mulDistR`/`lte`/`min`) and D2's collection
   indices need. More (gcd, bit ops) are parked until a listing needs them.

## Test/gate plan

- **Differential accel harness (the review event):** generate random literal
  pairs; assert each accel op equals the def's peeled unfolding; mutation-test the
  literal-only guard and each `big.Int` call.
- **Conversion harness:** literal↔succ-chain interconvertibility
  (`succ (NatLit k) ≡ NatLit (k+1)`, `zero ≡ NatLit 0`); accel does **not** fire on
  open terms (an open numeric goal converts byte-identically to pre-R-NUM); the
  existing "conversion is equivalence + congruence" property extended to `NatLit`.
- **Performance pin:** `mul 4096 4096` checks in O(1) bigint ops, not O(n²) — a
  timing regression mirroring `deepchain_test.go:60-90`'s superlinearity guard but
  asserting *constant* time for closed literal arithmetic.
- **Eliminator-over-literal:** `NatElim` on a `NatLit` peels correctly (a `factR`/
  `addR` defined by the eliminator computes the right value without materialising
  the chain); a memory pin that no 16M-node tree is built.
- **Hash regression:** `hashFormatVersion == 0x06`; `tagNatLit` append-only
  (existing tags unmoved); every shipped builtin group hashes byte-identical
  before/after.
- **UX / cap gone:** a bare `5000` (and `10^100`) elaborates and runs on all
  backends; pretty round-trips a literal to a digit (Decision 3); `parse ∘ pretty
  = id` over big literals.
- **D1 acceleration:** ch63's semiring proofs (`addAssoc`/`mulDistR`) still check
  (bridge = refl) and any concrete numeric evaluation in them is now O(1).
- **Rule-5 migration:** ch14's `Pos` constants re-elaborate as `NatLit` with the
  deliberate hash event recorded; `builtin bin`/`Bin`/`PosLit` deleted and no test
  references them.

## Unblocks (which implement nodes, and what they still need)

- **C7 (compressed numerals + kernel arithmetic)** — *this node*.
  **Ready-to-build:** the `NatLit` core node + eval/conv/hash/quote cases (hash
  bump 0x06), the default-lowering switch + cap deletion, the opt-in accel table
  for the recommended op set, pretty folding, and the `builtin bin` retirement.
- **D1 (numeric tower)** — *accelerated, not unblocked* (D1 already landed its
  laws, `CLAUDE.md` D1 line): every closed numeric computation inside a proof now
  runs in bigint ops instead of unary recursion, and the laws transfer `refl`
  (the accel op is the same def). The wall the tower's `mul`/`mulDistR` hit goes
  away.
- **D2 (verified collections)** — collection indices/lengths over large `Nat`
  stop being a blowup; `Vec n`/array bounds at realistic `n` become tractable.
- **D3 (reals/floats + LA)** — reuses the *identical* literal-node + accel-table
  pattern for `Int`/`Rational`/`Float` (Decision 6); R-NUM is its template.
- **Retires:** `PARKING-LOT.md:209-213` (true compressed numeral — built),
  `PARKING-LOT.md:211-212` (binary-spine pretty-fold — folded),
  `surface/numeral.go:32-35,42-44` (the 4096 cap), and the `builtin bin`
  machinery (`numeral.go:54-79`, Decision 5).

**Status of this node: ready-to-build.** One core constructor (`NatLit`), one
hash bump (0x06), one opt-in kernel reduction table, the default-lowering switch,
and the `builtin bin` retirement. The single genuine metatheory-review event is
the **accel table's agreement with the unfolding** — differential- and
mutation-tested as the soundness gate, the same discipline R-SUM applied to η,
which is exactly what Thompson and the roadmap demand for an outer-core change.

Sources:
[Lean 4 kernel `Nat`/`Int` literal reductions (`src/kernel/type_checker.cpp`)](https://github.com/leanprover/lean4/blob/master/src/kernel/type_checker.cpp),
[Lean4Lean: Verifying a Typechecker for Lean (incl. the Nat/Int kernel accelerations)](https://arxiv.org/pdf/2403.14064),
[Mario Carneiro, "The Type Theory of Lean"](https://github.com/digama0/lean-type-theory/releases),
[Coq primitive integers (`Uint63`/`PrimInt63`/`PrimFloat`)](https://coq.inria.fr/doc/master/refman/language/core/primitive.html),
[Coq Number Notations](https://coq.inria.fr/doc/master/refman/user-extensions/syntax-extensions.html#number-notations),
[Agda built-ins — Natural numbers (BUILTIN NATURAL + native ops)](https://agda.readthedocs.io/en/latest/language/built-ins.html#natural-numbers).
