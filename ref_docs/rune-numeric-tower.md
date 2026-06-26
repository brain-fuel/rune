# Rune numeric tower — split the glyphs

**Status:** accepted (Proposal C of three; A and B summarized in §8).
**Scope:** surface grammar (one token), preludes, quotient libraries.
**Core impact: none.** The hash format, the eliminator discipline, and
totality by construction are untouched at every step.

## 0. The position

One grammar amendment — a sixth operator, `//` — buys a permanent property no
amount of library discipline can: **every semantically loaded division symbol
means one thing, everywhere, forever.**

- `/` is **exact division**. It exists only at types where exact division is
  total modulo the zero convention — fields. On Nat and Int it is *absent*:
  not "rounds, surprisingly," but unbound. The C-family footgun (`1 / 2 = 0`)
  becomes unrepresentable rather than documented.
- `//` is the **flooring quotient**. Exists at every numeric type, including
  Rat, where it is the floor function in disguise.
- `%` is the **flooring remainder**, `//`'s matched other half.

The pair is governed by one law, the same law that already silently anchors
the v3.4.0 prelude's conventions:

```
DIV-LAW   a = (a // b) * b + a % b        for ALL a, b — including b = 0
BOUND     b > 0  →  0 ≤ a % b < b
```

with the totality conventions `a // 0 = 0`, `a % 0 = a`, `a / 0 = 0`. DIV-LAW
holds at b = 0 by arithmetic (`0·b + a = a`), not by special pleading. This is
the theorem-shaped design test: the decomposition is right because the law is
provable uniformly at every type in the tower, and because `%` stays *useful*
all the way up — at Rat it is the fractional-part operator, not a vestigial
zero.

## 1. mod vs rem: floor, decided

Three candidate conventions for integer division (Boute's taxonomy):

| | quotient rounds | remainder sign | `-7 ? 2` | `7 ? -2` |
|---|---|---|---|---|
| **T** (truncate) | toward 0 | follows dividend | q −3, r −1 | q −3, r 1 |
| **F** (floor) | toward −∞ | follows divisor | q −4, r 1 | q −4, r −1 |
| **E** (Euclid) | varies | always ≥ 0 | q −4, r 1 | q −3, r 1 |

**Verdict: F.** Reasons, in order of weight:

1. **F extends to Rat as the literal floor function.** `a // b = ⌊a/b⌋` reads
   identically at Int and Rat; E's "remainder under |b|" has no floor reading,
   T has no clean Rat story at all. C's whole bet is uniformity up the tower;
   F is the only convention that climbs.
2. Clock arithmetic works: `(-1) % 12 = 11`. Periodicity proofs
   (`(a + b) % b = a % b`) hold without sign side-conditions for b > 0 — and
   b > 0 is where E and F agree anyway, so E's advantage is confined to
   negative divisors, which are rare and explicitly signposted here.
3. T is rejected outright: `%` sign following the *dividend* breaks BOUND,
   breaks periodicity, and exists mainly as C heritage.

Truncated `quot`/`rem` get named (non-operator) definitions in the Int library
for interop honesty, with the T-law stated beside them. They never touch a
glyph.

## 2. The operator table, per type

| | Nat | Int | Rat |
|---|---|---|---|
| `+` `-` `*` | yes (`-` truncated, as today) | yes (`-` true subtraction) | yes |
| `/` exact | **absent** | **absent** | yes; `a / 0 = 0`; law: `b ≠ 0 → (a / b) * b = a` |
| `//` floor quotient | yes (= v3.4.0's `/`) | yes, rounds −∞ | DONE (ch13 RPair; ch203 QPair `floorQ`): floorQ via qlift, respect = Euclidean uniqueness |
| `%` floor remainder | yes (= v3.4.0's `%`; F = E = T at Nat) | yes, sign follows divisor | DONE (ch13): `a % 1` = fractional part, certified |
| `gcd` (named) | yes | via `natAbs` | — |

Notes:

- At Nat nothing changes semantically: v3.4.0's `/` *is* the flooring quotient
  and its `%` *is* the flooring remainder (all three conventions coincide
  without negatives). The migration is a rename, `/` → `//`.
- `-` at Nat stays truncated subtraction. The "wrong" subtraction lives at the
  type that can't do better; Int's is exact. Same philosophy as `/`: Nat
  refuses to *pretend* (`5 - 7 = 0` is the one concession, kept because Nat
  without any subtraction is unusable and monus is standard; the honest
  alternative — dropping `-` from Nat too — was considered and not taken).
- `%` at Rat is the working fractional-part/periodicity operator:
  `7/2 % 1 = 1/2`, `x % (2 * pi)` once trig ever exists. It is the proof that
  `//`/`%` carve reality at a joint.

## 3. Grammar amendment

GRAMMAR.md §2: the closed operator set becomes `+ - * / // %`. Still closed;
still no user fixity. §3:

```
MulOp ::= "*" | "/" | "//" | "%"
```

Same precedence level as before (Mul, binds tighter than Add),
left-associative. `(//) : Nat -> Nat -> Nat` works for free via the existing
`"(" Op ")"` atom.

Lexer: one longest-match case, `/` followed by `/` → `//`, sitting exactly
where `--` and `->` already sit in the longest-match ladder. No collision:
comments are `--`, arrows `->`.

**No existing program changes meaning.** Before the amendment `a//b` lexes as
two `/` tokens and is a parse error; every previously-valid file stays valid
with the same parse. The amendment is purely additive at the token level. The
printer round-trips `//`; `rune fmt` is the proof.

This breaks the letter of "the table is closed" while keeping its spirit: the
set is still fixed, versioned with the grammar, and not user-extensible. The
honest cost is precedent — the next proposal to add a token cites this one.
Accepted: a table revised once a major version by a design doc is still a
closed table.

## 4. The tower itself

All v2 machinery, no new core anywhere.

**Int** = `Quot (PairNN)` under `(a, b) ~ (c, d) iff a + d = b + c` (the
difference representation, already exercised by the v2 gate: `2 + (−1)`
computes). Operations by `qlift`, one respect proof each; negation is the
swap. `//` and `%` defined by case analysis on signs over Nat's flooring
engine, then lifted; the sign cases are where the F-convention is *earned*.

**Rat** = `Quot QPair QRel` (CANONICAL as of the 2026-06-17 consolidation; the
original `RPair` form below is superseded). A `QPair` is `qpair (n : Z) (d :
Nat)` denoting `n / succ d`: the numerator is a genuine integer (ch107's `Z`,
the difference quotient over `NatPair`), the denominator a Nat stored off by
one (the +1 trick, structurally nonzero, no positivity proof, total
constructors). Equivalence is cross-multiplication in the **Z ring**: `qpair a
b ~ qpair c e  iff  zmul a (zden (qpair c e)) = zmul c (zden (qpair a b))`.
Normalization (divide by gcd) is not part of the type; the quotient makes `1/2
= 2/4` by `qsound`, not by representation.

The Z-valued numerator is the whole point: every well-definedness obligation
collapses to a Z-ring identity (`zmulInterchange`, `zmulComm`, `zmulHom`),
reusing the proven integer ring instead of re-deriving sign algebra on Nat
pairs. The realized division of labor across the corpus:

- **field laws** (`qadd`/`qmul`, commutativity/associativity/distributivity):
  ch108, on QPair, by the representative formulas with one respect proof each.
- **order** (`qle`, reflexivity/transitivity/antisymmetry/totality, `qaddMono`):
  ch119, on QPair, lifted through QRel by cross-multiplication (`a/sb <= c/sd
  iff zle (a*sd) (c*sb)`).
- **division/floor** (`floorQ`, `//`, `%`, exact `/`): the canonical form lifts
  ch12's `zfdiv` (flooring division on `Z`) over the Z-valued numerator, so
  `floorQ` is essentially `zfdiv (qnum p) (zden p)` and the qlift respect proof
  is `zfdiv`-respects-QRel, simpler than ch13's RPair sign-trick. Ported from
  ch13 during the consolidation.
- **`to_radix` / repeating-decimal display**: `qlift` of `digits` after
  `reduce` (lowest terms), respect by reduce-to-lowest-terms canonicity.

ORIGINAL RPair form (SUPERSEDED, Rule 5). Rat was first built (ch13) as `Quot
RPair`, `rp a b d` denoting `(a − b)/(d+1)`, a Nat-pair difference numerator,
with division as an equation-only `flip` and `floorQ` discharged by Euclidean
uniqueness (`divUnique`/`floorUnique`, delivered there, not parked). That
predates ch107's `Z` and ch108's QPair upgrade. Since the entire downstream
tower (field ch113-119, reals ch121-131, matrices/det ch141-154) is built on
QPair, QPair is the load-bearing rational and RPair is retired: ch13's
division/floor content is ported to QPair, after which ch13 is repurposed.

**Injections** are explicit, named, and one-directional: `intOf : Nat -> Int`,
`ratOf : Int -> Rat`, composite `ratOfNat`. No coercion, no overloading escape
hatch. Mixed-type expressions name their injections — in a language where a
proof's identity is the hash of its content, silent coercion is a lie about
what was written.

## 5. Numerals

Two milestones, deliberately severable:

**C-minor (ships with the tower):** numerals stay Nat-bound. `ratOf (intOf 3)`
or library sugar where wanted. Ugly, honest, zero surface change.

**C-major (a separate rung):** generalize the binding — `builtin nat T z s`
accepts any in-scope *definitions or constructors* `z : T`, `s : T -> T`, not
only constructors of a data declaration. The Rat library then declares
`builtin nat Rat zeroQ succQ` and `3 : Rat` parses as
`succQ (succQ (succQ zeroQ))`, exactly the existing n-fold expansion. The
existing constructor form remains valid (constructors are terms). Consequences
to carry: the parse-time literal cap applies per binding; the BigInt codegen
shadow is keyed to the *Nat data* binding, so non-Nat literals compute through
their successor term in the shadow until a per-binding shadow rule is added
(parked beside the existing compressed-numerals entry).

Only the *last* `builtin nat` in scope binds digits (the current
single-binding rule, unchanged). A Rat file re-binds; a Nat file doesn't
notice.

## 6. What gets certified (the gate)

Following the ch11 precedent — the fuel style proves its results by running;
the general division-algorithm theorems belong to the course-of-values
chapter — each rung lands with closed-instance certificates, where every
`refl` is a machine-checked arithmetic fact:

- `DIV-LAW` instances at each type (`(0 - 7) // 2 = 0 - 4 is refl` at Int,
  `7/2 % 1 = 1/2 is refl` at Rat), including the b = 0 conventions.
- Exactness instances at Rat: `(a / b) * b = a` for closed a, b ≠ 0.
- Cross-representation equalities by `qsound`/`refl`: `1/2 = 2/4`.
- Shadow agreement: `rune run` on the closed instances prints the same digits
  the checker certified.

The general theorems (DIV-LAW ∀, BOUND, ring laws for Int and field laws for
Rat by `qind`) are the course-of-values chapter's payload and are parked with
it — stated here so the operator semantics are pinned by instances now and by
quantified proofs when that chapter lands.

## 7. Migration ladder

Each rung is one commit, suite-green, independently shippable; nothing touches
core or hashes.

1. **C1 — the token.** Lexer/parser/printer learn `//`; GRAMMAR.md §2/§3/§5.4
   bump. Nat prelude, ch11 listing, and lessons take the mechanical `/` → `//`
   pass; `/` becomes unbound at Nat. REPL banner line updates. *Breaking at
   the surface for `/`-users; the fix is one character, and the error is
   immediate and named, not a wrong answer.*
2. **C2 — Int.** Quotient library: `+ - * // %` at Int, `quot`/`rem` named,
   `intOf`, `natAbs`. Sign-case instances and DIV-LAW instances certified.
3. **C3 — Rat.** The +1-denominator quotient, exact `/` (as multiplication
   by an equation-only flip — no sign analysis), `+ - *`, full respect
   proofs. Exactness and zero-convention instances. This is the rung where
   `/` first exists. *Amended in execution:* `//` and `%` at Rat are
   well-defined on the quotient only by Euclidean uniqueness — the
   course-of-values theorem ch11 already deferred — so they are parked with
   it (PARKING-LOT) with their semantics pinned by §2; shipping them without
   the uniqueness proof would mean an unprovable qlift obligation.
4. **C4 — numerals beyond Nat** (C-major above). Separately decided,
   separately reversible.

Rung C1 is the decision commit: after it, the glyph meanings are frozen and
rungs 2–4 are pure library work.

## 8. Considered and rejected

- **Euclidean convention (E):** remainder always ≥ 0 is elegant at Int but has
  no floor reading at Rat; uniformity loses. F = E wherever b > 0, which is
  everywhere the lessons go.
- **`%` absent at Rat:** keeps the table smaller but surrenders the best
  evidence that the decomposition is right (fractional part for free) and
  breaks DIV-LAW's uniform statement.
- **Proposal B — symbols migrate up the tower by prelude shadowing:** each
  numeric layer re-binds `+ - * / %` at its own type; `/` floor at Nat/Int,
  exact at Rat. One glyph, two semantics, scope-dependent. Beautiful ℚ, but
  rejected *here* precisely because C's reason to exist is that no glyph ever
  changes meaning. (Its generalized numeral binding survives as rung C4.)
- **Proposal A — Nat keeps the symbols, Int/Rat are named-function libraries
  (`addZ`, `divQ`, …):** zero grammar cost, kernel silence, and ℚ arithmetic
  stops reading like arithmetic. Rejected because ch11's thesis was the other
  way. Remains the fallback if *Specify & Verify* never needs ℚ on stage.
