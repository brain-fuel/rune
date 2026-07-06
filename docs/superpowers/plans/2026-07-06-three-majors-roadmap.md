# The three major bumps (author directive, 2026-07-06)

Author-defined contents of the three rune major version bumps that precede
cutting a Wavelet beta (sequencing decided 2026-07-04). Backwards
compatibility is explicitly NOT a goal for the foreseeable future; once a
Wavelet minor exists, migration scripts carry code across breaking versions.

## v4: Numerics

- A tower/graph appropriate to the circumstances with types for:
  {Nat, Whole, Int, Rat}, {Irrat}, {IEEE754}, {Real}, {Complex},
  {Quaternion}, Other if necessary.
- Math as fast as possible: the only "slow" thing is elaboration. Anything
  about the host implementation of things known to be true must be just as
  fast as the host implementation of anything. This includes the REPL and
  running a script.
- ALL targets in parity with one another, so that any one of them can serve
  as the back end for the REPL, and for running programs when configured as
  the default back end.
- DOCTRINE CHANGE: reverses "if there ain't a book chapter for it, don't
  build it" in favor of "if we need a book chapter for it, then make both
  the feature and the chapter." (Standing Rule 1's scope cap is superseded
  at v4: features and chapters are co-created.)

## v5: Polyglot builds

Machinery for a polyglot project with Rune alongside any of its target
languages: Maven/Gradle/Buck2, playing nicely with javac, playing nicely
with Android for Java, and the canonical build tools for the other targets.

## v6: Stdlib (the easy 80%)

A standard library of the easy 80% of what developers want, to make the
ecosystem read as mature:
- All the infra primitives and the guarantees around them.
- The machine-learning stack people want: PyTorch / PyTorch Lightning /
  scikit-learn / NumPy / JAX / Pandas / Hugging Face Transformers / OpenCV /
  MXNet / Flux equivalents.
- Sockets and all the abstractions over them: writing to files, reading
  from files, and protocols over that.
- Everything that comes with Go + BEAM/Erlang/Elixir + Python's nice
  libraries + Java's nice libraries that makes the ecosystem feel mature on
  contact.

## Status

v4 scoping in discussion (controller proposals pending author reaction);
v5/v6 not yet decomposed. Each bump gets its own spec -> plan(s) ->
implementation cycles when its turn comes.
