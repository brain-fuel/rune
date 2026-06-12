# Learning Rune — a course in ten lessons

Rune is a small, content-addressed, dependently typed programming language. That
sentence will make complete sense by Lesson 8. The short version: Rune is a
language where **types can talk about values**, which means a type can be a
mathematical claim — like "this function returns a list exactly as long as its
input" — and a program with that type **is a proof of the claim**. The type
checker is a proof checker. When your program compiles, the math is done.

This course takes you from zero to writing your own machine-checked proofs and
compiling them to JavaScript. It assumes you're comfortable with algebra and
have written *some* code before (any language). It does not assume you've seen
type theory, logic, or functional programming.

## How to use this course

Every lesson is meant to be done **at the keyboard**, typing into the Rune
REPL (read-eval-print loop). Reading without typing teaches roughly nothing —
the whole point of a proof assistant is the conversation you have with it,
including the arguments you lose.

Each lesson has:

- **Concepts** — the ideas, explained from scratch.
- **REPL sessions** — transcripts to reproduce. The outputs shown are real;
  if you see something different, stop and figure out why.
- **Exercises** — do them. Solutions are at the bottom of each lesson, but
  the exercises are calibrated so you can get them with effort.

Lessons 1–9 live in the REPL. Lesson 10 is a project: you'll build a small
verified library in files, prove real theorems about it, and compile it to
JavaScript you can run with node.

## Setup

You need Go installed. Then:

```sh
go install goforge.dev/rune/v3/cmd/rune@latest
```

Check it works:

```sh
rune repl
```

You should see:

```
rune repl — expressions are type checked and normalized; definitions are checked and cached.
type :help for commands, :quit to exit.
rune>
```

Type `:quit` (or press Ctrl-D) to exit. For Lesson 9 you'll also want
[node](https://nodejs.org) on your PATH.

> **Note:** if `rune repl` prints `usage: rune (fmt|hash) <file>` you have an
> old build on your PATH. Reinstall with the command above.

## The lessons

| # | Title | You will learn |
|---|-------|----------------|
| 1 | [Getting started](01-getting-started.md) | The REPL, expressions, types, your first definition |
| 2 | [Functions](02-functions.md) | Lambdas, application, dependent function types, `let` and `seq` |
| 3 | [Universes](03-universes.md) | `U`, `U1`, `U2`, … — why types have types, and why there's a ladder |
| 4 | [Implicits and holes](04-implicits-and-holes.md) | Making the elaborator write the boring parts for you |
| 5 | [Equality](05-equality.md) | `Eq`, `refl`, proofs as programs, and why function extensionality is free |
| 6 | [Data and induction](06-data-and-induction.md) | Natural numbers, booleans, lists — and proofs by induction |
| 7 | [Quantities](07-quantities.md) | The 0/1/ω usage discipline: erased data and linear data |
| 8 | [Hashes and the proof cache](08-hashes-and-the-cache.md) | Content addressing: why a finished proof never needs rechecking |
| 9 | [Codegen: the shadow](09-codegen.md) | Compiling to JavaScript, and watching the proofs vanish |
| 10 | [Project: a verified library](10-project.md) | Build `runelib`: verified arithmetic, lists, and pairs |

A complete, checked solution to the project is in
[`solutions/runelib.rune`](solutions/runelib.rune). Don't open it until you've
fought with Lesson 10 for real.

## A note on difficulty

Dependent types are taught in graduate courses, but there's nothing about them
that requires a graduate degree — only patience with precise rules, which is
the same skill as algebra. You will get type errors you don't understand at
first. That's not you failing; that's the type checker holding up its end of
the conversation. Read the error, form a guess about what it means, and test
the guess. That loop *is* the skill.
