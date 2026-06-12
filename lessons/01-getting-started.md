# Lesson 1 — Getting started

**Goal:** get comfortable in the REPL, learn what an expression and a type are
in Rune, and write your first definition.

## 1.1 The conversation

Start the REPL (bare, as explained in the [course README](README.md) — this
course builds everything from nothing):

```sh
rune repl --no-prelude
```

Everything you type is either an **expression** (Rune evaluates it and tells
you its type), a **definition** (Rune checks it and remembers it), or a
**command** starting with `:` (instructions to the REPL itself).

Type the simplest expression in the language:

```
rune> U
U : U1
```

Read the reply as "`U`, which has type `U1`". Every reply has this shape:
`value : type`. The colon is pronounced "has type". Rune will never show you a
value without its type, because in this language the type is the interesting
part.

So what is `U`? It's the **type of types** — the "universe". `Nat` (natural
numbers, coming in Lesson 6) will be a type, so `Nat : U`. The number `zero`
will have type `Nat`. There's a hierarchy here:

```
zero : Nat : U : U1 : U2 : ...
```

Why `U` itself has type `U1` instead of `U` is a genuinely good question with
a genuinely interesting answer — that's Lesson 3.

## 1.2 Your first definition

A definition has the shape:

```
name : Type is
  body
end
```

Read it as: "`name` has this `Type`, and is defined to be this `body`." Try
one (the REPL accepts multi-line input — it shows a `...>` prompt until the
form is complete):

```
rune> myType : U1 is
...>   U
...> end
defined myType
```

Two things happened, and they're worth separating:

1. Rune **checked** that the body (`U`) actually has the declared type
   (`U1`). If it didn't, the definition would be rejected.
2. Rune **remembered** it. Now `myType` is usable:

```
rune> myType
U : U1
```

Notice the REPL didn't print `myType : U1` — it printed `U : U1`. The REPL
always **normalizes**: it runs the expression as far as it will go and prints
the result. `myType` unfolds to `U`, so that's what you see.

Point 1 is the heart of everything in this course. In most languages, a type
declaration is a vague promise that the runtime might or might not honor. In
Rune, *nothing enters the system unchecked*. The phrase for this is that
definitions are **type checked on entry**. There is no way to define something
ill-typed, which means everything you can refer to is something that has
already been verified.

Try to break it:

```
rune> bad : U is U end
error: bad: type mismatch: expected U, got U1 (universe level mismatch: U1 vs U0)
```

`U` has type `U1`, not `U`, so the definition is refused. Read that error
carefully — "expected X, got Y" mismatches will be your most common
conversation with Rune, and they always mean exactly what they say: the type
you *declared* is X, the type the body *actually has* is Y.

## 1.3 REPL survival kit

Commands you'll use constantly:

```
rune> :list
myType : U1
```

`:list` shows everything defined this session, with types.

```
rune> :type myType
U1
```

`:type` (or `:t`) shows an expression's type *without* normalizing it. This is
the question you'll ask most often in this course.

```
rune> :reset
session cleared
```

`:reset` wipes the session. `:load <path>` loads definitions from a file —
you'll use that from Lesson 6 onward. `:help` lists everything; `:quit`
exits.

Comments, for when we get to files: `-- like this` to end of line, or
`{- like this -}` (nestable) for blocks.

## 1.4 What you should believe so far

- Everything is an expression, every expression has a type, and the REPL
  prints `normal-form : type`.
- A definition is only accepted if its body checks against its declared type.
- `U` is the type of types, and it lives in `U1`.

That third point is small now, but it's the seed of the whole course: if types
are themselves values that have types, then *functions can take and return
types*, and that's where everything interesting comes from.

## Exercises

1. Define `myType2 : U2` whose body is `U1`. Then ask `:t myType2` and
   explain to yourself why `:t` and plain evaluation print different things.
2. Predict the output of `:t U2` before typing it. Then type it.
3. Try `nonsense` (an undefined name). Read the error. Then try defining
   `x : U is nonsense end`. Are the errors the same kind?

## Solutions

1. ```
   rune> myType2 : U2 is U1 end
   defined myType2
   rune> :t myType2
   U2
   rune> myType2
   U1 : U2
   ```
   `:t` reports the declared/inferred type (`U2`); plain evaluation unfolds
   the definition and prints the normal form *with* its type (`U1 : U2`).
2. `U2 : U3`. The ladder continues forever.
3. ```
   rune> nonsense
   error: unbound identifier "nonsense"
   rune> x : U is nonsense end
   error: x: unbound identifier "nonsense"
   ```
   Same kind of error — an unbound name — but the second is reported while
   checking the definition `x`, so the error is prefixed with the
   definition's name and nothing gets defined.
