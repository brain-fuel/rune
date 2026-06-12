# Rune Surface Ergonomics — Design Document

**Arithmetic that reads like arithmetic** · *a surface/elaboration delta; the core does not change*

> **Release criterion.** A new listing, `ch11_arithmetic.rune` — naturals with
> literals and infix, `mul`/`pow`/`div`/`mod`/`gcd` written in `case … of … end`
> sugar, their specs proved in `calc` blocks — elaborates, checks, and runs fast
> (BigInt shadow). And ch07 *could* be rewritten to read like mathematics using
> only this document's sugar. No core constructor is added or changed; the hash
> format stays 0x04. Every rung here is desugaring into the v3.0.0 core.

The pain, in one line from ch07:

```
Eq Nat (add (add a b) c) (add a (add b c))      -- today
(a + b) + c = a + (b + c)                       -- after
```

Four wounds: no numerals, no infix, eliminator-only definitions, trans/cong/sym
pyramids. Each gets one rung. Rungs are ordered by cost/benefit and are
independently shippable; each is **pure sugar** — parsed in `surface/`,
eliminated during elaboration, invisible to `core/`, `store/`, and the hash.

---

## Rung 1 — Infix operators

Operator tokens are **ordinary identifiers** that happen to be symbolic. They
are declared, resolved, content-addressed, and printed exactly like `add`:

```
+ : Nat -> Nat -> Nat is … end
x + y          -- resolves to the same hash as the prefix application
```

- **Fixed precedence table, no user fixity declarations** (Elixir's posture).
  Levels, loosest to tightest, all left-associative except `=`:

  | level | operators | note |
  |-------|-----------|------|
  | 1 | `=` | non-associative; rung 3 |
  | 2 | `+` `-` | |
  | 3 | `*` `/` `%` | |

  All levels bind tighter than `->` and looser than application.
  The table is closed: only listed tokens lex as operators. Extending the table
  is a grammar change, deliberately. No backticks, no sections, no custom
  fixity — parked until a listing starves without them.
- **Parsing**: precedence climbing over the existing application grammar;
  an operator token after a complete operand is the only trigger, so the
  one-token-lookahead rule (GRAMMAR §1) holds. `(+)` — operator in parens —
  is the prefix/first-class form, usable anywhere an identifier is.
- **Pretty-printer** emits infix for saturated applications of operator-named
  definitions, parenthesized per the table; `parse ∘ pretty = id` is preserved
  (the harness property extends to operator-bearing terms).
- **Name resolution is unchanged in kind**: `+` is a name in the same flat
  namespace. Shadowing and hashing work because nothing downstream knows
  operators exist.

## Rung 2 — Numeric literals (builtin binding, Agda's trick)

A new declaration form registers which data type numerals elaborate to:

```
builtin nat Nat zero succ
```

- `3` in expression position elaborates to `succ (succ (succ zero))` — the
  registered constructors, by hash. No binding in scope → literals are a
  resolve-time error ("no builtin nat declared").
- The binding is **per-file surface state** (like the keyword table), not a
  core artifact; it never enters the store. Two files may bind different Nats.
- **Pretty-printer** folds saturated `succ`-chains terminating in `zero` back
  to digits when a binding is registered. Round-trip holds: digits re-parse to
  the same core term.
- Validity check at declaration: `Nat : U`, `zero : Nat`, `succ : Nat -> Nat`,
  all three resolving to constructors/formers of one data group.
- Hash hygiene: a literal is *nothing but* the succ-chain; `1000000` produces a
  big core term. Fine for listings; the shadow handles runtime cost (rung 6),
  and a compressed core numeral is parked until a listing actually embeds a
  huge constant.

## Rung 3 — `=` sugar for propositional equality (decided: `=`, not `==`)

```
x = y        ~~>        Eq _ x y
```

The hole elaborates as a Phase-2 metavariable; unification solves the type
from the operands exactly as it already does for `_`. **Input-only sugar,
like `seq`**: the core `Eq` stores its solved type, so the printer keeps
emitting `Eq A x y` — printing `x = y` would drop the type and break
`parse ∘ pretty = id`. Non-associative (`x = y = z` is a parse error);
tighter than `->` so `a = b -> c = d` is an implication between equations;
looser than `+`/`*` so `a + b = b + a` parses as expected. One carve-out:
in a `let`/`seq` binding's *type annotation*, `=` always ends the annotation
(it is the binding's own `=`); an equality type there is parenthesized —
`let p : (x = y) = prf in …`.

## Rung 4 — `case … of … end`

ALGOL W's vocabulary, ML's clause shape (decided 2026-06):

```
add : Nat -> Nat -> Nat is
  fn (m : Nat) (n : Nat) is
    case m of
    | zero -> n
    | succ k with ih -> succ ih
    end
  end
end
```

**Grammar.**

```
CaseExpr ::= "case" Expr "of" ( "|" Clause )+ "end"
Clause   ::= Pattern [ "with" Ident+ ] "->" Expr
Pattern  ::= Ident Ident*                            -- constructor, then binders
```

Every clause leads with `|` (first included), so clause boundaries never
depend on layout and the parser needs no newline sensitivity here. The `->`
after a pattern cannot be confused with a Pi arrow in one token of lookahead:
inside a clause the parser is reading a Pattern, whose grammar is flat —
a constructor name and variable binders, no nesting, no literals-in-patterns
(both parked). `of` joins the keyword list.

**Desugaring — the eliminator, mechanically.** For scrutinee `s` whose type
elaborates to data type `D` with constructors `c₁ … cₙ`:

- The clause set must be **exactly** the constructors of `D`, each once, any
  order. Coverage stays by construction; a missing or alien clause is an
  elaboration error naming the constructor.
- Each clause's binders match the constructor's arity; `with` binds the
  induction hypotheses the eliminator already provides for recursive
  arguments, in order. `with` on a non-recursive constructor is an error.
  Omitting `with` when an IH exists is fine — the IH binder is generated
  fresh and unused.
- **Motive**: `case` elaborates in checking mode against expected type `T`.
  If the scrutinee is a variable `x`, the motive is `fn (x : D) is T end`
  (T generalized over x — dependent case for free). If the scrutinee is a
  compound expression, the motive is the constant family. A `case` in
  inferring position is an error ("annotate or use the eliminator") — honest
  and removable later.
- Output is a plain application of `DElim`. The core, the hash, the proof
  cache, codegen: all see the eliminator they already know. **Totality remains
  by construction; no recursion enters the language.** This graduates
  PARKING-LOT's "pattern-matching sugar" with the IH question answered: the
  recursive *call* spelling (Agda/Haskell equations) is a later, compatible
  layer that desugars to this one.

**Printer**: eliminator applications whose motive/branch shapes match the
desugaring image print back as `case`; anything else prints as the raw
eliminator. Round-trip is preserved in both directions.

## Rung 5 — `calc` blocks

Equational-reasoning sugar over the ch07 toolkit (`trans`, `sym`, `cong` are
library definitions, not core):

```
calc add m n
  = add n m        by addComm m n
  = add n (mul one m) … 
end
```

```
CalcExpr ::= "calc" Expr ( "=" Expr "by" Expr )+ "end"
```

Desugars right-fold to `trans step₁ (trans step₂ …)`; each `by e` must check
against `Eq _ lhsᵢ rhsᵢ` (metas solve the type). Pure expression sugar; the
printer never emits `calc` (input-only, like `seq`).

## Rung 6 — Fast naturals on the shadow

The JS backend, given a file whose `builtin nat` binding is registered,
compiles that data group specially:

- values of the bound `Nat` are **BigInt**; `zero` is `0n`, `succ` is `x + 1n`;
- `NatElim` compiles to a loop (accumulator from the zero branch, succ branch
  applied k times) instead of recursive switch dispatch;
- everything else is untouched — constructors of other types, quotients,
  eliminators all compile as today.

This is the "mutate the shadow" rule working as designed: the core stays
Peano and provable; the erased output gets machine arithmetic. Gate: `gcd` on
8-digit inputs terminates in milliseconds under `rune run`.

## Rung 7 — `ch11_arithmetic.rune`, the gate listing

Naturals with the full kit: `mul`, `pow` by `case`; **course-of-values
recursion derived from `NatElim`** as a library (`cvRec`), then `div`, `mod`,
`gcd` on top of it; specs proved in `calc` (`mod` correctness,
`gcd` divides both arguments, `(div m n) * n + mod m n = m`). No new core:
course-of-values is a definable gadget, which is the point of the chapter.

---

## What this is not

- No typeclasses, no overloading: `+` names one definition per scope; integer
  and natural addition are different names (or different files) until a
  listing demands more.
- No user-defined fixity, no nested patterns, no pattern literals, no
  definition-level equation clauses — each parked with this document as the
  context to extend.
- No core change of any kind. If an implementation step seems to need one,
  the step is wrong; stop and re-read the desugaring.

## Order of work

1 (infix) → 2 (literals) → 4 (`case`) → 3 (`=`) → 5 (`calc`) → 6 (BigInt) →
7 (ch11). GRAMMAR.md is amended per rung *before* code, per house rule; the
harness round-trip generators extend with each rung. Rungs 1+2 alone already
let ch07's statements read as `(a + b) + c = a + (b + c)` — ship value early.
