# Rune — Proof-Cache Semantics

**Companion to the v1–v3 roadmap · the formal treatment of the cache-soundness seam**

> **The claim, up front.** With the right semantics, the proof cache needs *no invalidation logic at all* — not a coarse transitive-closure fallback, not dirty-bit propagation, not timestamp comparison. Those are all artifacts of *mutable* stores. A content-addressed store makes the entire category of cache-invalidation bug **unrepresentable**, and the soundness argument collapses to a single frame lemma enforced by one abstraction barrier. This document says exactly what can be honestly claimed, and no more.

---

## 0. The scoping decision that makes everything clean

We cache **the well-typedness of elaborated, metavariable-free core terms** — the expensive checking-and-conversion work, *after* elaboration. We do **not** cache elaboration (surface → core) in the same table. This matters because elaboration carries the nondeterminism — metavariables, implicit insertion, instance resolution — and `check` on fixed core input is, by contrast, a **deterministic total function of its inputs and the bodies it unfolds**. Every clean property below depends on that determinism. Elaboration results are separately content-addressable (keyed on surface-hash), but they are a different, non-soundness-critical layer; keep them out of this one.

So the object of study is the judgment

```
S ⊢ h ✓        "the definition stored at content-hash h is well-typed in store S"
```

where `S` is the immutable definition store (§5) and `h = ‖d‖` is the Merkle content-hash of an elaborated core definition `d`, computed with internal references to other definitions replaced by *their* content-hashes — so `h` transitively summarizes `d`'s entire syntactic content (its type and its body).

## 1. What a cache entry *means* (the semantics)

A cache entry is not the assertion "`h` is verified." It is a **derivation of a hypothetical judgment**

```
Δ ⊢ (h ✓)
```

read: *"the term at `h` is well-typed, under the definitional assumptions `Δ`"* — where `Δ` is the set of definitions whose **bodies** the checking actually unfolded, each named by its content-hash. The honest content of a cache hit is conditional, and the condition is explicit.

Two facts about `Δ` make the conditional trivial to discharge:

1. **`Δ` is recorded by content-hash, and content-hashes are immutable.** "The definition with hash `u`" is a fixed object for all time; its body cannot change underneath you (a change *produces a different hash*, hence a different object). So the proposition `Δ ⊢ (h ✓)` mentions only immutable things and therefore has a **fixed truth value, forever**.

2. **To even *form* the lookup, you must hold `Δ`'s hashes.** The conditionality is baked into the key (§5): you cannot ask "is this hit valid?" without already naming the exact assumptions it was closed over. There is nothing to separately re-validate.

The picture this yields: the cache is a **sound, append-only, monotone memo table**. An entry, once written, is correct in every store that realizes its key, eternally. Editing source does not *invalidate* an entry; it mints *new* content-hashes that **miss** the cache and get fresh entries, while the old entry remains valid-but-unqueried. **Invalidation is never a correctness concern; at most it is a space policy (GC eviction).**

## 2. The Frame Lemma (the load-bearing content)

Everything above rests on one lemma, and it is the separation-logic frame rule wearing a type-theory hat.

> **Frame Lemma.** Let `check(d)` run in store `S` and report `S ⊢ h ✓` with **consulted set** `U` — the set of content-hashes whose bodies were unfolded during the run. Then for *any* dependency-closed store `S′` that agrees with `S` on the bodies of every `u ∈ U`, we have `S′ ⊢ h ✓` with the same derivation.
>
> Equivalently: **`check(d)`'s result is invariant under every store mutation that preserves `h` and the bodies in `U`.** The check *frames off* everything it did not unfold.

*Proof sketch.* `check` is a deterministic function of (a) the content at `h`, fixed by the hash; (b) the **types** of the definitions `d` references — fixed too, because those references are content-hashes *inside* `h`, and each referenced definition's type is part of *its* content-hash; and (c) the **bodies** in `U`. `S` and `S′` agree on (c) by hypothesis, and (a),(b) are pinned the moment `h` is present in a dependency-closed store. A deterministic function of unchanged inputs returns an unchanged result. ∎

> **Corollary (Cache Soundness).** With `CertHash = hash(h, ‖U‖)` (§5), the table `CertHash ↦ result` is sound and append-only. There is no invalidation step, therefore no invalidation bug.

Three things this buys, stated as what's *honestly* true:

- The recorded `U` is **exact, not an over-approximation.** It is precisely the bodies touched — transitively, since each further unfold is itself a logged call (§3) — and it is a *subset* of the syntactic reachable closure. The coarse "full transitive closure" fallback mentioned in earlier drafts is **retired**: it was only ever needed if you couldn't instrument conversion, and §3 shows you always can.
- The "type-deps are captured, body-deps must be recorded" distinction is now a theorem, not a slogan: reading a referenced definition's **type** is part (b), already summarized by `h`; reading its **body** is part (c), the out-of-band thing `U` exists to record.
- There is exactly **one** side-condition at lookup — "does `S′` realize `U`?" — and by immutability that is just "do these hashes still resolve?", which is a set of store lookups, not a validation procedure.

## 3. The interface that makes it true *by construction*

The Frame Lemma's hypothesis — *the only thing the check consults of a definition's definiens is what it unfolded, and we logged exactly that* — must be **enforced**, or it is a vigilance task waiting to fail (especially across two layers in v3). Encode it in the interface so the failure mode is *unrepresentable*:

```idris
-- A definition's body is SEALED. There is no public projection.
abstract Body

-- A definition's TYPE is public and pure: it is part of the content-hash that
-- any referencing term already carries, so reading it incurs NO recorded dependency.
typeOfDef : ContentHash -> Ty                    -- pure; logs nothing

-- The conversion monad carries a write-only dependency log.
ConvM a                                          -- accumulates a DepSet (write-only)

-- THE ONLY gateway to a body. Obtaining a definiens *necessarily* logs its hash.
unfold    : ContentHash -> ConvM Term            -- logs the hash, returns the body

-- A glued value's lazy unfolding IS unfold. Forcing a neutral logs the dependency.
data Val  = VNeu Neutral (ConvM Val) | {- ... -}

-- You cannot obtain a result without its dependency witness; they come together.
runConv   : ConvM a -> (a, DepSet)
```

The three interface invariants, each a one-liner the *types* enforce:

1. **`Body` is abstract** ⇒ the only path to any definiens is `unfold` ⇒ **every** definitional dependency is logged, at every reduction layer, by construction. No code outside the conversion module can pattern-match a body; the barrier is checkable (and is itself a property Iron Sieve asserts).
2. **`typeOfDef` is pure** ⇒ reading a referenced definition's type logs nothing, formalizing "type-deps are in-band."
3. **`runConv` returns `(result, DepSet)` as a pair** ⇒ you cannot mint a certificate without the exact assumptions it was closed over.

The coincidence worth naming: the glued-NbE value already keeps a *lazy unfolding*, and the only way conversion consults a body is to **force** it. So `force` and `unfold` are the same operation, and the dependency log **rides on the machinery you built for speed** — the fast syntactic conversion path compares un-unfolded spines and logs *nothing*; you log exactly when you force, which is exactly when you incur the dependency. The instrumentation is free.

Note also what `unfold` covers and doesn't: it is **global δ-reduction only**. Local `let`-bindings live in the term and are part of `h` (in-band, framed by the hash). Datatype constructor reductions and eliminator β-rules are **structural** — they consult no global definiens — so they add nothing to `U`. `unfold` fires precisely on *global definition unfolding*, which is precisely the out-of-band hazard.

## 4. The data structures

Four structures, with a sharp line between the soundness-critical and the not:

- **Immutable definition store** — `Map ContentHash Definition`, bodies sealed (§3). Append-only; content-addressed; dependency-closed by construction (you cannot have computed `‖d‖` without holding the hashes of `d`'s dependencies, so you held them). This is in the trusted base.
- **The name layer** — `Map Name ContentHash`. The **only mutable** structure, and deliberately **outside** the trusted base: it merely records *which* immutable object is "current" for a human-facing name. Editing source rebinds names here; it touches nothing in the store and can produce only cache *misses*, never wrong hits.
- **The certificate table** — `Map CertHash Result`, append-only. `CertHash = hash(h, ‖U‖)`.
- **The dependency set `U`** — accumulated write-only in `ConvM`, then canonicalized for the key. **Canonicalize by sort-then-hash**: `‖U‖ = hash(sort(elements U))`. (A commutative/incremental multiset hash — fold the element hashes under a commutative combiner so insertion order doesn't matter — is a tempting optimization, but XOR-style combiners have cancellation/collision hazards; dependency sets are small, so sort-then-hash is the safe default and the optimization is parking-lot.)

The whole correctness story lives in the first and third being append-only and immutable; the second is mutable but harmless; the fourth is the frame, canonicalized.

## 5. What the precision *is*, and the one honest limit

The cache is exact **on what was consulted**. It is not, and is deliberately not, exact on *meaning*. Concretely:

If a definition `e ∈ U` has its body changed in a way that **preserves the reductions the check actually relied on**, the cache will *not* recognize the change as harmless. `e` gets a new content-hash, `U` changes, `CertHash` changes, you get a **miss**, and you re-check — correctly, but redundantly. Recovering that reuse would require keying on *conditional facts* ("`e` reduces to `v`") rather than on `e`'s full content, and **that is where the games live**: conditional-fact keying reintroduces a validation procedure whose soundness you must separately maintain, and re-validating facts is precisely the invalidation logic this design exists to avoid.

So we **stick with what can honestly be said**: a certificate asserts *"this exact configuration of bodies checks,"* never *"this checks for all bodies satisfying property P."* The first is sound by a one-line frame argument over immutable objects. The second is a real, sometimes-worthwhile optimization, and it is explicitly out of scope — in the parking lot, behind a clear sign reading *here be invalidation*.

## 6. Across the roadmap

The same semantics covers all three versions because soundness rides on the **barrier**, not on enumerating dependencies — and a barrier is indifferent to how many reduction layers run behind it.

- **v1 (one equality).** Single-layer conversion; `force = unfold` is the choke point; certificates are append-only; the Frame Lemma is the v1 soundness theorem. This is also exactly the cache-soundness chapter of *Specify & Verify*.
- **v2 (quotients, set-level HITs).** Quotient eliminators add **structural** reductions (β on point-constructors) that consult **no** global bodies, so they contribute nothing to `U`. The path-constructors and the "respects-`R`" coherences are **proof-irrelevant under UIP** — they carry no computational content to compare and are never unfolded against a mutable body, so they never enter `U`. The Frame Lemma is unchanged; only globally-defined functions unfolded en route are logged, exactly as in v1.
- **v3 (two equalities).** Conversion now runs at the strict and the fibrant layers. **Both** route every definiens access through the same sealed-`Body`/`unfold` gateway, so `U` captures inner-layer definitional dependencies identically. The Frame Lemma holds *verbatim*; the layer count is invisible to it. v3's only added obligation is an **audit** — that no inner-layer code path peeks a body around the barrier — and the abstract `Body` type discharges that audit at compile time rather than by review.

  The **honest exception** is the cubical-inner frontier (§F of the v3 doc): Kan operations, composition, and `Glue` reduce structurally over types, and global unfolds within them still funnel through `unfold`, so the Frame Lemma is **architecturally protected**. But it is **not yet proven** for cubical reduction, because those reductions are not built. Confirming the frame property survives `Glue` and composition is a *research obligation* of §F, not a settled fact, and this document does not claim otherwise.

## 7. The property to prove and mutation-test

State the Frame Lemma as the Iron Sieve property and let MUT do the adversarial reading you asked for:

> For a well-typed core definition `d`, let `(ok, U) = runConv (check d)` in store `S`. For any `S′` agreeing with `S` on the bodies in `U`, `runConv (check d)` in `S′` returns `(ok, U)`.

Two mutants this is designed to kill, which are exactly the ways the seam could rot:

- A mutant that lets *some* body be read around the barrier (an unlogged definiens access) makes some `S′`-run disagree → the property fails. A *surviving* mutant here means the barrier has a hole — a bug in the **checker's soundness**, not the tests.
- A mutant that lets a certificate be written or read **without** its `DepSet` (decoupling result from witness) breaks the pairing in `runConv` → caught.

And assert the barrier itself as a static property: *no code outside the conversion module pattern-matches `Body`.* That one check is what turns the whole soundness story from a careful argument into an enforced invariant — which is the entire point of picking the right semantics instead of playing games.

---

### References
v1–v3 design docs (this is the formal core of their shared cache-soundness seam) · the frame rule of separation logic (Reynolds; O'Hearn–Reynolds–Yang) — the structural analogy the Frame Lemma instantiates · Unison (content-addressed, append-only code store) — the immutability that makes "no invalidation" true · Kovács, *smalltt* — the glued value domain whose `force` is the instrumented gateway.
