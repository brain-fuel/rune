# Error diagnostics: the design

Wootz's pedagogy telos (the Savage smith) says every part of the language must be
teachable, and the errors are where a learner spends the most time. So an error is
not a verdict; it is a short lesson. The model is Elm and Rust for shape, Patrick
Henry Winston and Socrates for tone:

- **Name the parts (Winston).** Say what the thing is and what was expected, in the
  reader's own words, not the implementation's. "I expected this to have type `U`,
  but it has type `U1`" beats "type mismatch: U vs U1".
- **Explain the why.** When a concept is load-bearing (universe levels, the erasure
  boundary, exhaustive coverage), spend a sentence teaching it. The error is the
  teachable moment.
- **Ask the deciding question (Socrates).** End on the choice the reader must make:
  "are the two sides really equal (then rewrite, do not `refl`) or genuinely
  different (then the statement is false)? Which is it?"
- **Be concrete and kind.** Suggest the fix. Offer a "did you mean?" when a name is
  close. Never blame.

## Mechanism

`elaborate/diagnostic.go` defines `Diagnostic{Summary, Body []string, Hints []string}`,
an ordinary `error` whose `Error()` renders the Elm/Rust shape: the verdict, then the
explanation indented and word-wrapped, then `help:` lines. Because it is just an
`error`, it threads through the existing pipeline with no plumbing: the REPL and the
file loader already print `error: <err>`.

Parse errors are handled one layer out, by presentation, in `surface/diagnostics.go`:
`RenderParseError(src, err)` recovers the byte offset the parser already threads
through its messages ("... at offset N") and renders the offending source line with a
caret beneath the exact column. No parser site changes.

Helpers worth reusing: `suggest` (conservative Levenshtein "did you mean?"),
`orList`/`andList` (human enumerations), `e.inScopeNames(c)` (the candidate pool of
everything a name could resolve to here).

## Covered today

unbound identifier (with did-you-mean), type mismatch (with universe-level teaching),
`refl` against a false or non-equation goal (showing both normal forms), applying a
non-function and a `fn` at a non-function type, non-exhaustive `case` (all missing
constructors at once) and unknown constructor (with did-you-mean), unsolved
metavariables, QTT quantity/erasure violations, using a value where a type is needed,
and a `case` with no known result type. Parse errors get a source caret everywhere.

## Adding one

Write a `*Diagnostic` (or a small builder beside the others) and return it from the
`fmt.Errorf` site. Then grep the tests (`--include=*_test.go`) for the old message
substring and update the assertions; most use `strings.Contains`. Verify the new
message in `rune repl` before committing: a feature is not done until it reads well to
a human at the prompt.
