# CLOCKS-DESIGN — clock quantification `∀κ`, the gate for `bisimilar ⟹ path`

*Research design for the C5b/R-COIND tail. Grounded in the landed guarded-recursion
group (`store/guard.go`: `Later`/`next`/`dfix`/`lmap`/`lap`) and the landed
bisimilarity-as-path FORWARD direction (`listings/ch66_stream_bisim.rune`). This is
the missing primitive that turns guarded (productivity-by-typing) recursion into
genuine coinduction, and so lets the E2 CONVERSE — bisimilar ⟹ path — actually be
constructed. Decision-locked, in the style of GLUE-DESIGN / RBOX-DESIGN /
FACE-RESTRICTED-EVAL-DESIGN.*

## 1. The wall (why the converse is stuck today)

The forward direction is done (ch66): a `pathF (Str A) s t` carries to a path
between every observation, so a path ENTAILS pointwise bisimilarity. The CONVERSE —
given equal heads and a bisimilarity of tails, BUILD a `pathF (Str A) s t` — is the
E2 payoff and the longest pole of telos-4. It is corecursive: the path is an
infinite object, produced one observation at a time.

`dfix` gives productive corecursion, but its result is **trapped under `Later`**.
Concretely, the guarded recursive call has type `El (Later X)`, and there is **no
sound way to eliminate `Later`**: a `force : Later A -> A` would discharge the guard
unconditionally and the normalizer would diverge (it is exactly what `convDfix`'s
bounded one-step discipline refuses). So with a single implicit clock we can write
productive definitions and prove their one-step unfolding (`dfixUnfold`, ch62), but
we can never CASH a guarded value into an actual `Nu`-stream or a `pathF`. The
converse needs to escape the `Later`, and escaping it soundly is precisely what a
clock quantifier buys.

This is not an engine wart — it is the known TCTT result (Atkey–McBride; Bizjak–
Møgelberg; Bahr–Grathwohl–Møgelberg, *Clocks and Cubical Types*). Guarded recursion
alone is an internal-productivity discipline; **coinduction = guarded recursion +
clock quantification**.

## 2. The mechanism (CCTT clocks, minimal contained form)

Add clocks as a new, contained layer — the same discipline every prior group used
(own hash space, heads neutral, ι-rules on intros, no outer-core constructor unless
forced).

- **Clock context.** A clock is a variable `κ` from a clock context `Δ`, threaded
  alongside the term context. Today `guard.go`'s `Later` is implicitly indexed by a
  single ambient clock; the change is to make the clock **explicit** so it can be
  bound and quantified.
- **`▹κ A`** — later w.r.t. a NAMED clock (generalises the current `Later A`). `next^κ`
  / `dfix^κ` carry the clock. `lmap`/`lap` (the landed applicative algebra,
  ch62 `lmapAsLap`/`lapInterchange`) are reused per-clock unchanged.
- **`∀κ. A`** — clock quantification, a Π over the clock context. Introduced by a
  clock abstraction `Λκ. e` (κ not free in any value-variable's type — the standard
  freshness side-condition), eliminated by clock application `e [κ']`.
- **`force : (∀κ. ▹κ A) -> ∀κ. A`** — the keystone isomorphism. Sound BECAUSE the
  `∀κ` witnesses that the value does not depend on any particular clock, so the
  guard on a quantified delayed value carries no information and may be discharged.
  This is the ONLY elimination of `▹`, and it is gated by the quantifier — never the
  naked `force : ▹A -> A` that would diverge.

The defining computation is the clock-β / force-next coherence:

```
force (Λκ. next^κ x)  ~>  Λκ. x            (the later collapses under ∀κ)
```

plus the guarded fixpoint per clock (the landed `convDfix`, now `dfix^κ`).

## 3. Soundness — why `force` does not reopen divergence

The `convDfix` firewall (ch62: `dfix` unfolds exactly once in conversion, never in
eval; the reappearing inner `dfix` is matched by spine comparison) is preserved
verbatim per clock. `force` does NOT unfold `dfix`; it strips a `▹κ` from a value
that is **parametric in κ**. Parametricity is the safety argument: a closed
inhabitant of `∀κ. ▹κ A` cannot inspect κ, so the delay it announces is vacuous.
Operationally `force` only fires on a `Λκ. next^κ …` intro (an ι-rule, like every
other group's), staying neutral otherwise — so the normalizer still cannot loop. No
new eval recursion that isn't bounded by an intro; the C4 firewall discipline holds.

Open obligation (the genuine research, not assembly): the **freshness/parametricity
side-condition on `Λκ`** must be enforced by the elaborator (κ not escaping into a
value-type), analogous to the QTT usage check already in `elaborate/`. This is the
one place clocks touch a checker rather than landing as a pure additive builtin
group — comparable in size to the FACE-RESTRICTED-EVAL branch-elaboration residue,
smaller than a hash-format bump.

## 4. The payoff — `bisimilar ⟹ path`, constructed

With clocks, the converse is the standard Møgelberg–Veltri construction:

1. A bisimilarity `Bisim A s t` is the clock-quantified guarded relation
   `∀κ. (head s = head t) × ▹κ (Bisim A (tail s) (tail t))` (well-formed because the
   recursive occurrence sits under `▹κ`, productivity by typing).
2. `dfix^κ` corecursively assembles, from a `Bisim`, a guarded path between the
   streams' `out`-observations; `force` cashes the `∀κ`-quantified guarded path into
   an honest `pathF (Str A) s t`.
3. The endpoint coherences land via the already-shipped `papp` boundary
   (`papp … p i0 ~> s`, `… i1 ~> t`) and the ch66 `headPath`/`tailPath`
   observations — the forward direction becomes the round-trip's other half, giving
   `Bisim ≃ pathF (Str A)` (bisimilarity IS path, both ways: the E2 currency).

This unblocks E2 (usable equational reasoning over processes), and transitively the
liveness halves of D5/R-OTP, E3/R-PROTO, R-CALC's behavioral-equivalence layer.

## 5. Build order (each step a green, contained checkpoint)

1. **Explicit single clock.** Re-index `guard.go` `Later`→`▹κ` with one named clock
   threaded through the context; `next`/`dfix`/`lmap`/`lap` gain the clock param.
   ch62 re-elaborates (digest shift in its own space, no golden test). NO new
   capability yet — pure refactor, proves the threading.
2. **`∀κ` former + `Λκ`/clock-app** as a builtin group (own hash space), heads
   neutral, clock-β by ι. Elaborator freshness check (the §3 obligation) — the real
   research step; land it behind a pin that REJECTS an escaping κ.
3. **`force`** with the `force (Λκ. next^κ x) ~> Λκ. x` ι-rule + neutral otherwise.
   Pin: force/next coherence by `refl`; force on a neutral stays stuck.
4. **`Bisim` + the converse** in a new listing (`ch68_stream_bisim_converse.rune`),
   composing forward (ch66) + converse into `Bisim ≃ pathF (Str A)`. The E2 gate.
5. **fmapF-over-Later** (the cont.24-deferred item) falls out once `▹κ` is explicit:
   the stream functor maps under the clock via `lmap^κ`, recovering the seed code
   from the clock-indexed carrier — the Nu+Later guarded-stream `map`.

## 6. Decisions locked

- **Clocks, not sized types.** Sizes were rejected upstream (R-COIND); CCTT clocks
  compose with the cubical stratum (the whole reason C5b waited for M2) and need no
  ordinal bookkeeping.
- **Contained, additive where possible.** Steps 1–3 are builtin groups on their own
  hash spaces; the ONLY checker touch is the `Λκ` freshness side-condition (step 2),
  isolated like the QTT usage pass.
- **No outer-core constructor / no hash-format bump.** Clocks live as neutral heads
  + ι-rules, exactly as `interval`/`face`/`path`/`Kan`/`guard` did; assert
  `hashFormatVersion` unchanged.
- **`force` is the SOLE `▹` elimination, always under `∀κ`.** The naked
  `force : ▹A -> A` is never added — that is the divergence hole the whole design
  exists to avoid.

## 7. Status

Substrate landed: `▹` is a verified applicative functor (next/lmap/lap +
`lmapAsLap`/`lapInterchange` laws, ch62); the forward bisimilarity-as-path direction
runs (ch66); `convDfix` firewall proven.

**STEPS 1–3 LANDED (ch68, store/guard.go now clock-explicit, 8 members).** Step 1:
the guard group is re-indexed — `Later : Clock -> UF -> UF`, every `next`/`dfix`/
`lmap`/`lap` carries the clock; `convDfix` and the eval ι-rules (tryGuardIota/
tryGuardLapIota) thread it; ch62 re-elaborates clock-indexed (its own hash space, no
golden test). Step 2: `Clock : U` (a pretype, no eliminator) + `k0 : Clock`; `∀κ. A`
is realised as an ordinary Pi over `Clock` (no new former needed — the elegant
contraction). Step 3: `force` with the `force A (λκ. next κ A x) ~> λκ. x` ι-rule
(tryForceIota, probing a fresh clock sentinel, firing only on a `next` on the bound
clock with a clock-closed payload; neutral otherwise — C4 firewall intact).

KEY soundness realisation refining §3: the `Λκ` freshness side-condition is NOT
needed for SOUNDNESS — clock-indexing the `Later` already makes the inconsistent
`Later A -> A` UNTYPABLE (a `λκ. next c A x` delayed off the quantified clock fails
to elaborate; `force` on a neutral or wrong-clock `next` stays stuck). The freshness
pass is now a COMPLETENESS concern (admitting more clock-irrelevant values), the
remaining research-grade increment — not a soundness gate. Pins: ch68 (forceNext /
escapeId / forceLmap, refl) + internal/session/clock_test.go (force fires;
stays stuck on neutral; wrong-clock REJECTED at elaboration).

**STEP 4 SUBSTRATE LANDED (`gfix` guarded-recursive types; ch69).** The converse needs
to STATE the guarded stream `Str^g κ A = A × ▹κ (Str^g κ A)` and the bisimilarity
relation — self-referential CODES, which `dfix` (value-level, fixed code) cannot build.
Added `gfix : (k:Clock) -> (UF -> UF) -> UF`, the TYPE-level guarded fixpoint (9th guard
member): head neutral in eval, the equation `gfix k F ≡ F (gfix k F)` a bounded
conversion (convGfix) with a PROGRESS GUARD (unfolds only when F makes head progress, so
an unguarded `λX. X` cannot loop the checker — TestGfixUnguardedTerminates). ch69 builds
the guarded stream + `consG`/`headG`/`tailG` (type-check through the convGfix fold AND
compute) + a `dfix`-driven producer `repeatG`. So the converse's three pillars are all
landed: `gfix` (state the guarded stream/relation), `dfix` (productive corecursion),
`force` (cash the `∀κ`-quantified guarded path into a `pathF`).

**STEP 5 LANDED (fmapF-over-Later; ch69).** The stream functor's carrier map `sfmap`
lifts under ▹κ via `lmap` and computes on a `next` of a `pairF` intro (`lmap κ (sfmap g)
(next κ (h,x)) ~> next κ (h, g x)`, refl) — the map the guarded corecursor pushes through
a delayed observation.

REMAINING: the `Bisim ≃ pathF (Str A)` ASSEMBLY itself — but it surfaced a final gap:
`force` needs a clock-CLOSED payload, while a guarded stream's tail `▹κ (gStr κ A)` is
clock-VARYING. So the assembly needs the DEPENDENT force `forceD` + the `Str ≃ ∀κ. gStr`
bridge + guarded-recursive type FAMILIES (the `Bisim` former), all decision-locked in
**CLOCK-IRRELEVANCE-DESIGN.md**. Plus the optional `Λκ` freshness elaborator pass for
completeness.
