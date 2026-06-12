# Rune Surface Grammar

**Authoritative.** The lexer, parser, named AST, name resolution, and pretty-printer conform to
this document. If you find an ambiguity or a gap here, STOP and resolve it in this file before
writing code — do not improvise in the parser.

## Scope and status

This is the **named** surface layer. It resolves to the unchanged locally-nameless core (bound
variables are de Bruijn indices; references to top-level definitions are content hashes). **Surface
presentation never affects a definition's content hash** — hashing is over core, so reformatting,
renaming bound variables, or changing layout leaves the hash fixed. That invariant is worth a test.

As of **v0.2.0** the surface gains Elixir-style block syntax (`is … end`) and an explicit
sequencing form (`seq … end`). The language is otherwise the Phase-0 calculus and nothing more:
variables, the universe `U`, dependent function types, lambda, application, inline `let`, and type
ascription. There is no evaluation, type checking, data, or literals yet.

**v2.0.0 and v3.0.0 add no syntax.** The quotient kit (`Quot`, `qin`, `qsound`, `qlift`, `qind`)
and the two-level kit (`UF`, `El`, `fib`, `piF`, `pathF`, `preflF`, `pathJ`, `pathU`, `ureflU`,
`ua`, `castU`) are groups of BUILTIN DEFINITIONS bound ambiently in every session — ordinary
identifiers resolving to content-hash references, applied like any function. They are
deliberately NOT reserved words and NOT former heads in this grammar (contrast
`Eq`/`refl`/`cast`/`subst`); shadowing them with a user definition is possible and merely
unwise. See `rune-v2-implementation.md` and `rune-v3-implementation.md`.

## 1. Design rules (read first)

- **Blocks are keyword-delimited, not indentation-delimited.** `is`/`seq` open a block and `end`
  closes it. There is no offside rule.
- **The grammar is whitespace-insensitive, with exactly one exception:** inside a `seq` block, a
  newline or `;` separates items (§5.3), exactly as Elixir separates statements in a `do` block.
  Everywhere else, newlines are insignificant; top-level definitions self-delimit via their `end`.
- **`is` blocks hold exactly one expression.** A definition body and a lambda body are each a
  single expression. There is no implicit sequencing.
- **`seq` is the only sequencing construct,** and it desugars to nested `let … in`.
- **One lambda form** (`fn`). The inline arrow lambda is parked (§8).
- **Disambiguation needs at most one token of lookahead** (§5).

## 2. Lexical structure

- **Whitespace** (space, tab) separates tokens and is otherwise insignificant.
- **Newline** is insignificant *except* as a `seq` item separator (§5.3). Implementation note: the
  simplest realization is for the lexer to emit a `NEWLINE` token that the parser skips everywhere
  except while collecting `seq` items.
- **Comments** are insignificant and are **not** preserved by the pretty-printer (round-trip is
  modulo comments): line comment `-- … <end-of-line>`; block comment `{- … -}`, **nestable**.
- **Identifier:** `(letter | "_") (letter | digit | "_" | "'")*`. Case-sensitive.
- **Numeral:** `digit+`, one token. In binder-quantity position (`(0 x : A)`, `{1 x : A}`) the
  numerals `0` and `1` are usage annotations — position disambiguates, never the lexer. In
  expression position a numeral is a literal of the `builtin nat`-bound type (§5.5).
- **Reserved words** (never identifiers): `fn`, `is`, `end`, `seq`, `let`, `in`, `U`, the
  equality stratum's `Prop`, `Eq`, `refl`, `cast`, `subst` (Phases 3–4), `data` (Phase 4),
  `builtin` (ergonomics rung 2), `case`, `of`, `with` (rung 4), and `calc`, `by` (rung 5). The bare underscore `_` is
  reserved as the hole; identifiers may still begin with `_` (`_x` is a name). `|` separates
  constructors inside a `data` block and leads each clause of a `case`.
- **Braces** `{` `}` open implicit binders/arguments (Phase 2); `{-` always opens a block comment
  instead, so an implicit form cannot begin with a literal `-`.
- **Punctuation:** `(` `)` `:` `=` `->` `;`. The lexer takes the longest match, so `->`
  is one token and is never read as `-` `-` `>`, and `--` begins a comment rather than two `-`.
- **Operators** (ergonomics, 2026-06): `+` `-` `*` `/` `%` are **symbolic identifiers** — they
  name top-level definitions exactly as alphabetic identifiers do, and additionally parse infix
  (§3, §5.4). The set is **closed**; no other token lexes as an operator and there are no user
  fixity declarations. `=` is punctuation that *also* parses infix as the equality-proposition
  sugar (§5.4); it is not an identifier and cannot be defined. A bare `-` is the operator;
  `--` and `->` still take the longest match.

## 3. Grammar (EBNF)

Notation: `X*` zero+ , `X+` one+ , `[X]` optional, `|` alternation, `"…"` terminal. This is the
generative skeleton; §5 gives the deterministic parse for the `(`-forms and for `seq`.

```
Program   ::= (Definition | DataDecl | BuiltinDecl)*

Definition ::= DefName ":" Expr "is" Expr "end"
DefName   ::= Ident | Op

BuiltinDecl ::= "builtin" "nat" Ident Ident Ident   -- type, zero, succ (§5.5)

DataDecl  ::= "data" Ident ":" Expr "is" ["|"] Ctor ("|" Ctor)* "end"   -- Phase 4
Ctor      ::= Ident ":" Expr

Expr      ::= Let
           |  Arrow

Let       ::= "let" Ident [":" AnnExpr] "=" Expr "in" Expr   -- inline; `in` is mandatory
AnnExpr   ::=                             -- an Expr in which "=" never begins an equality
                                          -- proposition at the spine (§5.4); parenthesize one

Arrow     ::= Binder "->" Expr            -- dependent:     (x : A) -> B   (right-assoc via Expr)
           |  IBinder "->" Expr           -- implicit:      {x : A} -> B   (Phase 2)
           |  EqE ["->" Expr]             -- non-dependent: A -> B,  or just EqE

EqE       ::= Add ["=" Add]               -- equality proposition; NON-associative (§5.4)
Add       ::= Mul (AddOp Mul)*            -- left-associative
Mul       ::= App (MulOp App)*            -- left-associative
AddOp     ::= "+" | "-"
MulOp     ::= "*" | "/" | "%"
Op        ::= AddOp | MulOp

App       ::= Atom Arg*                   -- application, left-associative
Arg       ::= Atom                        -- explicit argument
           |  "{" Expr "}"                -- implicit argument, given explicitly (Phase 2)

Atom      ::= Ident
           |  Num                         -- a numeral; requires a builtin nat in scope (§5.5)
           |  "(" Op ")"                  -- an operator, prefix/first-class: (+) x y
           |  Case                        -- case expression (§5.6)
           |  Calc                        -- equational ladder (§5.7)
           |  "_"                         -- a hole: a metavariable for elaboration (Phase 2)
           |  "U"
           |  "Prop"                      -- the universe of propositions (Phase 3)
           |  "Eq" | "refl" | "cast"      -- equality-former heads, applied like functions:
                                          --   Eq T l r · refl x (or bare refl when checking)
                                          --   cast A B p x      (Phase 3)
           |  "subst"                     -- Leibniz transport: subst A x y p P px (Phase 4)
           |  Lam
           |  Seq
           |  "(" Expr ")"                -- parenthesized expression
           |  "(" Expr ":" Expr ")"       -- type ascription

Lam       ::= "fn" (Binder | IBinder)+ "is" Expr "end"

Binder    ::= "(" [Qty] Ident ":" Expr ")"
IBinder   ::= "{" [Qty] Ident ":" Expr "}"   -- implicit binder (Phase 2)
Qty       ::= "0" | "1"                      -- usage annotation (Phase 5); default ω

Seq       ::= "seq" SeqBind* Result "end"          -- separators per §5.3
SeqBind   ::= "let" Ident [":" Expr] "=" Expr       -- NOTE: no `in`
Result    ::= Expr

Case      ::= "case" Expr "of" ("|" Clause)+ "end"
Clause    ::= Pattern ["with" PatName+] "->" Expr
Pattern   ::= Ident PatName*               -- constructor, then binders; flat (no nesting)
PatName   ::= Ident | "_"

Calc      ::= "calc" Expr ("=" Expr "by" Expr)+ "end"   -- equational ladder (§5.7)
```

Precedence, loosest to tightest: `let … in` and `->` (arrow is **right-associative**), then
`=` (**non-associative**), then `+` `-` (**left**), then `*` `/` `%` (**left**), then
application (**left-associative**), then atoms. So `a = b -> c = d` is an implication between
equations, and `a + b * c = c * b + a` parses as mathematics reads it. `fn`, `seq`, and
parenthesized forms are fully delimited, so they are atoms and need no surrounding parentheses.

## 4. The core forms, unchanged in meaning

These resolve to exactly the Phase-0 core nodes; only their concrete spelling is fixed here.

- **Variable** `x` — a bound variable (resolved to a de Bruijn index) or a reference to a top-level
  definition (resolved to that definition's content hash).
- **Universe** `U` — the single universe. (`U : U` for now; a hierarchy arrives in a later phase.)
- **Dependent function type** `(x : A) -> B` and **non-dependent** `A -> B`.
- **Lambda** `fn (x : A) is body end`; multi-parameter `fn (A : U) (x : A) is body end`.
- **Application** `f x`, `f x y`.
- **Inline let** `let x = e in body`, `let x : T = e in body`.
- **Type ascription** `(e : T)`.

## 5. Disambiguation (the deterministic parse)

### 5.1 The parenthesis rule — the one to get right

A `(` at the head of an `Arrow`/`App` position is ambiguous between a **Pi binder**, a **type
ascription**, and a **parenthesized expression**. Resolve with one token of lookahead past the `)`:

1. Parse the contents: an `Expr`, optionally followed by `:` and a second `Expr`; then `)`.
   Remember (a) whether a `:` was present, and (b) whether the first `Expr` was a bare `Ident`.
2. Peek the token after `)`:
   - **It is `->`** → if the group was exactly `( Ident : Expr )`, it is a **dependent Pi binder**
     (the `Ident` is bound in the codomain). Otherwise it is a **non-dependent arrow** whose domain
     is the parenthesized expression.
   - **It is anything else** → the group is an **atom**: a type ascription `(e : T)` if a `:` was
     present, else a parenthesized expression `(e)`.

So `(x : A) -> B` is dependent Pi; `(x : A)` standing alone is the ascription of variable `x` to
`A`; `(A) -> B` is the non-dependent `A -> B`.

### 5.2 `let`

A leading `let` begins an inline `Let` (`= Expr` then mandatory `in Expr`) everywhere except as a
non-final `seq` item, where it begins a `SeqBind` (`= Expr`, **no** `in`). See §5.3.

### 5.3 `seq`

```
"seq" then, separated by one or more separators (newline or ";"), with optional leading/trailing
separators:
    zero or more SeqBind, then exactly one Result,
"end".
```

- A **separator** is a newline or `;`. At least one separator sits between adjacent items; leading
  and trailing separators are allowed and ignored. The separator is what makes binding right-hand
  sides unambiguous — `let x = f g` reads `f g` as one application because the next item begins
  only after a separator.
- **Non-final items must be `SeqBind`s** (`let Ident [: Expr] = Expr`, no `in`). A bare expression
  in non-final position is rejected: under a total, pure core, evaluating and discarding a value is
  meaningless, so it is a parse error (write `let _ = e` if you truly mean to bind-and-ignore).
  Encountering `in` on a `SeqBind` is an error — bindings do not take `in`.
- **The final item is the `Result`,** any `Expr` (it may itself be an inline `let … in …`).
- A `seq` with no result (`seq end`, or a `seq` ending in a binding) is an error: a `seq` must
  produce a value.

### 5.4 Operators and `=`

- **Operators are names.** `x + y` parses to the application `(+) x y` — `EVar "+"` applied
  left-then-right — and resolution treats `+` like any free identifier: it must resolve to a
  top-level definition (else "unresolved name: +"). Operators cannot be binder names (`fn` and
  `let` binders are alphabetic `Ident`s only); the parenthesized atom `(+)` is the prefix form.
- **`=` is the equality-proposition sugar:** `l = r` parses to `Eq _ l r` — the `Eq` former with
  a hole for elaboration to solve. It is non-associative: `x = y = z` is a parse error
  ("an equality cannot be chained; parenthesize"). `=` is INPUT sugar only (§8).
- **The `let`-annotation carve-out.** Inside the optional `[":" AnnExpr]` of an inline `let` or a
  `SeqBind`, a spine-level `=` always ends the annotation and begins the binding's value — it
  never begins an equality proposition. The carve-out is inherited through arrows and operator
  expressions within the annotation, and is **reset inside any bracketed group** (`(…)`, `{…}`),
  so an equality type in a binding annotation is written parenthesized:
  `let p : (x = y) = prf in …`. Definitions, binders, and `data` need no carve-out — their type
  positions self-delimit (`is`, `)`, `}`, `|`, `end`).
- One token of lookahead still suffices everywhere: an operator token after a complete operand
  is the only infix trigger, and `( Op )` is recognized by the token after the `(`.

### 5.5 Numerals and `builtin nat`

```
builtin nat Nat zero succ
```

registers which data type numerals mean: the type, its zero, and its successor, by name. From
the declaration to the end of the file (and in REPL expressions once a loaded file declared it),
a numeral `n` in expression position parses as the n-fold application of the successor to the
zero — pure parser sugar; downstream the constructors are ordinary names resolving to ordinary
content-hash references, and the declaration itself leaves no trace in the core or the store.

- **No binding in scope → a numeral is a parse error** ("no `builtin nat` declared").
- The session validates the declaration: the type must be a `data` type and the two names must
  be its constructors.
- A literal is nothing but its unary succ-chain, so numerals **cap at 65536**; a compressed core
  numeral is parked until a listing embeds a constant that big.
- `0` and `1` in binder-quantity position are usage annotations, not literals (§2); position
  decides, so `(0 x : Nat)` annotates while `f 0` applies `f` to the literal.

### 5.6 `case … of … end`

```
case m of
| zero -> n
| succ k with ih -> succ ih
end
```

Sugar for one saturated application of the scrutinee type's **eliminator** — elaboration-time
desugaring, since the motive comes from typing:

- The scrutinee's inferred type must be a `data` type, fully applied to its parameters; the
  clause set must be **exactly** its constructors, each once, any order. Coverage stays by
  construction; a missing, duplicate, or alien clause is an error naming the constructor.
- A clause binds the constructor's arguments in order (`_` for unused), and `with` names the
  induction hypotheses the eliminator provides for the recursive arguments, in argument order.
  `with` may name fewer than there are (the rest bind fresh and unused); `with` on a
  constructor with no recursive argument is an error. **The IH is a binding, not a recursive
  call — totality remains by construction, and no termination checker exists or is needed.**
- A `case` elaborates in **checking position only**: the expected type is the motive,
  generalized over the scrutinee when the scrutinee is a bound variable (the dependent case),
  constant otherwise. In inferring position, ascribe it: `(case … end : T)`.
- The output is a plain application of `DElim`: the core, the hash, the proof cache, and
  codegen never learn `case` exists.
- Parsing: every clause leads with `|`, so clause boundaries never depend on layout; a clause
  body extends until the next `|` or the closing `end`. The block self-delimits (an atom) and
  resets the §5.4 annotation carve-out like any bracketed group.

### 5.7 `calc … end`

```
calc succ k + n
  = succ (k + n) by refl (succ (k + n))
  = succ (n + k) by cong succ (ih n)
  = n + succ k   by sym (addSucc n k)
end
```

An equational-reasoning ladder proving `first = last`: each `by` proof is checked against
exactly its own step's equation. Parse-time desugaring, with no library dependency — the first
step's proof is ascribed to its equation, and each later step chains through the `subst`
former: `subst _ prev next prf (fn (w : _) is first = w end) acc`. The motive's binder is
freshened against every identifier in `first`, so nothing is captured. Inside the block a
spine-level `=` is the ladder's own separator (the §5.4 carve-out mechanism); parenthesize an
equality proposition appearing within a step. `calc` is an atom; like the other blocks it is
input-only sugar (§8).

## 6. Desugaring to core

Surface is sugar over the core; resolution lowers as follows.

- **Curried lambda.** `fn (x₁ : A₁) … (xₙ : Aₙ) is B end` ⟶ nested single-parameter core
  lambdas `λx₁. λx₂. … λxₙ. B`. The Phase-0 core lambda is **un-annotated** (`core.Lam` stores
  only a binder scope, no domain). Resolution therefore **resolves each binder's domain annotation
  in the enclosing scope — scope-checking it, so an unbound name in a domain is an error — and then
  discards it.** The annotation is real syntax with no core counterpart: it constrains nothing yet
  and is not recoverable from the core. (Domain annotations gain a core home when type checking
  arrives in Phase 1.)
- **Dependent function type.** `(x : A) -> B` ⟶ the core Pi node; `A -> B` ⟶ Pi with a fresh
  unused binder.
- **Sequence.** `seq let x₁ = e₁ … let xₙ = eₙ R end` ⟶
  `let x₁ = e₁ in (let x₂ = e₂ in (… in (let xₙ = eₙ in R)))`, with each typed binding carrying
  its annotation. `seq R end` ⟶ `R`.
- **Ascription.** `(e : T)` ⟶ the core annotation node, as in Phase 0.
- **Inline let** and **application** ⟶ their existing core nodes directly.
- **Infix operators.** `x + y` ⟶ the application `(+) x y`; nothing downstream of the parser
  knows operators exist. `l = r` ⟶ `Eq _ l r` (the hole becomes a metavariable in elaboration).
- **Numerals.** Under `builtin nat Nat zero succ`, the literal `3` ⟶ `succ (succ (succ zero))`
  (§5.5), at parse time.
- **Case.** `case s of | C₁ a* [with ih*] -> e₁ | … end` ⟶ `DElim params* motive branch* s`
  (§5.6), at elaboration time; each branch is the clause body under its argument and IH binders.
- **Calc.** A ladder desugars to ascription + nested `subst` (§5.7), at parse time.

## 7. Examples

Identity and `const` (everything is `U`-typed for now; there are no other base types yet):

```
id : (A : U) -> A -> A is
  fn (A : U) (x : A) is x end
end

const : (A : U) -> (B : U) -> A -> B -> A is
  fn (A : U) (B : U) (x : A) (y : B) is x end
end
```

Explicit sequencing with `seq`:

```
example : U is
  seq
    let a = U
    let b : U = a
    b
  end
end
```

To **sequence inside a lambda body**, make the single body expression a `seq` (note the three
closing `end`s — `seq`, then `fn`, then the definition):

```
f : (A : U) -> A -> A is
  fn (A : U) (x : A) is
    seq
      let y = x
      y
    end
  end
end
```

One-liner `seq` using `;` separators: `seq let y = x; y end`.

## 8. Pretty-printer and the round-trip contract

The pretty-printer (core → named surface) emits one canonical sentence of this grammar. The
governing property is **`parse ∘ pretty = id` on terms** (the printed form re-parses to the same
core), held by the harness over a generator of well-formed terms in this grammar. Whitespace and
comments are not preserved; bound-variable names chosen by the printer are arbitrary, which is why
**alpha-equivalent terms print to forms with equal content hashes** — also a harness property.

Two things the core does not carry, and so the printer does not reconstruct — the round-trip is
modulo each, exactly as it is modulo comments and bound-variable names:

- **Lambda domain annotations.** A core `Lam` has no domain (see §6), so the printer emits the sole
  base type of this phase, `U`, for every `fn` binder: `λx. x` prints as `fn (x : U) is x end`.
  Re-resolution discards it, so the core round-trips; but `rune fmt` is deliberately lossy here,
  rewriting every lambda binder's annotation to `U`. (Function *type* domains — `Pi` — are in the
  core and are preserved faithfully.)
- **`seq`.** `seq` desugars to nested `let … in …` (§6) and leaves no trace in the core, so the
  printer's canonical sequencing form is inline nested `let … in …`; it never re-emits `seq`.
  `seq` is input sugar only.
- **`=`.** The core `Eq` node stores its type explicitly; `l = r` elaborates that type from a
  hole. Printing `x = y` would drop the solved type, so the printer always emits the saturated
  prefix form `Eq A x y`. `=` is input sugar only, exactly like `seq`.
- **`case`.** Desugars to an eliminator application at elaboration (§5.6) and leaves no trace
  in the core; the printer emits the eliminator application. Input sugar only. (Folding
  recognizable eliminator applications back into `case` is a later cosmetic step.)
- **`calc`.** Desugars to `subst` chains at parse time (§5.7); the printer emits those. Input
  sugar only.

The printer DOES emit infix operators: an application `Ref⁺ x y` whose definition hash maps to an
operator name prints as `x + y`, parenthesized per the §3 precedence table; unsaturated or
non-operator applications print prefix (`(+) x`). Both forms re-parse to the same core, so the
round-trip property extends unchanged.

The printer also folds numerals when a `builtin nat` binding is registered: a saturated
succ-chain terminating in the bound zero prints as digits (`succ (succ zero)` prints `2`), which
re-parse to the same core under the same binding. A chain over a variable tail (`succ n`) is not
a numeral and prints as application.

Canonical layout: definitions break across lines; short `fn … is … end` and `let … in …` may stay
on one line. The `seq` keyword is accepted on input (one item per line, or `;`-separated) but is
not produced on output.

## 9. Deliberately excluded (parked)

Not in v0.2.0; each has a home later or in `PARKING-LOT.md`. Do not add any of these because they
would be convenient — add nothing with no current consumer.

- Inline arrow lambda (`fn x => body`) — ergonomics, parked.
- Unannotated lambda parameters (`fn x is …`) — parked; v0.2.0 params are `(name : Type)`.
- Multi-binder Pi telescopes (`(x : A) (y : B) -> C`) — parked; chain with `->`.
- Equation-style multi-clause definitions — a later layer that desugars to `case` (§5.6).
- Nested patterns, literal patterns, default/catch-all clauses — `case` patterns are flat and
  the clause set is exactly the constructor set; parked until a listing starves.
- User-defined operators / fixity declarations — the §3 table is closed; parked.
- Data type and record declarations — Phase 4.
- Modules, imports, visibility — later; v0.2.0 is a flat list of definitions that may reference one
  another (mutual recursion is handled at the store/SCC level, not the surface).
- String and character literals — later. (Numeric literals arrived with `builtin nat`, §5.5.)

## 10. Input contexts

A **file** is a `Program` (a sequence of `Definition`s). The **REPL** additionally accepts a bare
`Expr` and `:`-prefixed commands; that input layer is specified in `v0.2.0_PROMPT.md`, not here.
