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
- **Reserved words** (never identifiers): `fn`, `is`, `end`, `seq`, `let`, `in`, `U`. The bare
  underscore `_` is reserved as the hole; identifiers may still begin with `_` (`_x` is a name).
- **Braces** `{` `}` open implicit binders/arguments (Phase 2); `{-` always opens a block comment
  instead, so an implicit form cannot begin with a literal `-`.
- **Punctuation/operators:** `(` `)` `:` `=` `->` `;`. The lexer takes the longest match, so `->`
  is one token and is never read as `-` `-` `>`, and `--` begins a comment rather than two `-`.

## 3. Grammar (EBNF)

Notation: `X*` zero+ , `X+` one+ , `[X]` optional, `|` alternation, `"…"` terminal. This is the
generative skeleton; §5 gives the deterministic parse for the `(`-forms and for `seq`.

```
Program   ::= Definition*

Definition ::= Ident ":" Expr "is" Expr "end"

Expr      ::= Let
           |  Arrow

Let       ::= "let" Ident [":" Expr] "=" Expr "in" Expr      -- inline; `in` is mandatory

Arrow     ::= Binder "->" Expr            -- dependent:     (x : A) -> B   (right-assoc via Expr)
           |  IBinder "->" Expr           -- implicit:      {x : A} -> B   (Phase 2)
           |  App ["->" Expr]             -- non-dependent: A -> B,  or just App

App       ::= Atom Arg*                   -- application, left-associative
Arg       ::= Atom                        -- explicit argument
           |  "{" Expr "}"                -- implicit argument, given explicitly (Phase 2)

Atom      ::= Ident
           |  "_"                         -- a hole: a metavariable for elaboration (Phase 2)
           |  "U"
           |  Lam
           |  Seq
           |  "(" Expr ")"                -- parenthesized expression
           |  "(" Expr ":" Expr ")"       -- type ascription

Lam       ::= "fn" (Binder | IBinder)+ "is" Expr "end"

Binder    ::= "(" Ident ":" Expr ")"
IBinder   ::= "{" Ident ":" Expr "}"      -- implicit binder (Phase 2)

Seq       ::= "seq" SeqBind* Result "end"          -- separators per §5.3
SeqBind   ::= "let" Ident [":" Expr] "=" Expr       -- NOTE: no `in`
Result    ::= Expr
```

Precedence, loosest to tightest: `let … in` and `->` (arrow is **right-associative**), then
application (**left-associative**), then atoms. `fn`, `seq`, and parenthesized forms are fully
delimited, so they are atoms and need no surrounding parentheses.

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

Canonical layout: definitions break across lines; short `fn … is … end` and `let … in …` may stay
on one line. The `seq` keyword is accepted on input (one item per line, or `;`-separated) but is
not produced on output.

## 9. Deliberately excluded (parked)

Not in v0.2.0; each has a home later or in `PARKING-LOT.md`. Do not add any of these because they
would be convenient — add nothing with no current consumer.

- Inline arrow lambda (`fn x => body`) — ergonomics, parked.
- Unannotated lambda parameters (`fn x is …`) — parked; v0.2.0 params are `(name : Type)`.
- Multi-binder Pi telescopes (`(x : A) (y : B) -> C`) — parked; chain with `->`.
- Operators / infix notation, multi-clause definitions, pattern matching — later phases.
- Data type and record declarations — Phase 4.
- Modules, imports, visibility — later; v0.2.0 is a flat list of definitions that may reference one
  another (mutual recursion is handled at the store/SCC level, not the surface).
- Literals (numbers, strings, characters) and holes (`_` as a term) — later.

## 10. Input contexts

A **file** is a `Program` (a sequence of `Definition`s). The **REPL** additionally accepts a bare
`Expr` and `:`-prefixed commands; that input layer is specified in `v0.2.0_PROMPT.md`, not here.
