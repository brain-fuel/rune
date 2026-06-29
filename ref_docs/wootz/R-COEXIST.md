# R-COEXIST -- codebase coexistence (`rune build`: ship checked core as host artifacts)

> Roadmap track B (`portable codegen + FFI`, telos 2). Where R-IR/R-ERASE2 make the
> erased shadow PORTABLE across backends and R-FFI lets Rune call OUT to a host, this
> node is the dual: let a host codebase call IN to checked Rune. A checked, meta-free
> program is emitted not just as a runnable app but as a reusable host-language
> library artifact (a Go package today; a Rust crate/module, a JVM package, a
> JS/Python module, a BEAM module, a C library, or a WASM component as backends grow),
> so a verified core can live INSIDE an ordinary polyglot repo. The driving consumer
> is Furnace (the polyglot property-test subset): it embeds a Rune-checked core as a
> Go package and exercises it from host tests.

This is codegen-only and deploy-side: zero core change, no hash event, no new surface
syntax. It rides the existing `Backend` (Phase-7 erased IR -> target source).

## The contract (`codegen/artifact.go`)

A target-neutral build API, the deploy-side dual of the `Backend` interface:

- `BuildSpec{ Kind, Module, Exports }` -- `Kind` is `app` or `library`; `Module` is the
  host ecosystem's namespace (a Go import path, JVM package, Rust crate, …); `Exports`
  name the Rune defs visible at the host boundary (`Export{RuneName, HostName}`,
  `HostName` optional -- the backend picks a stable target-native spelling).
- `ArtifactSet{ Target, Kind, Artifacts, Run, Link }` -- one or more `Artifact{Path,
  Role, Data}` (roles: `source`/`manifest`/`header`/`runtime`), plus advisory `Run`
  (app) / `Link` (library) command hints the host build tool may replace.
- `Build(b, p, spec)` -- APP mode wraps `b.Emit` for EVERY existing backend (one source
  `Artifact` + a per-target run hint). LIBRARY mode dispatches to the optional
  `LibraryBackend` interface (`EmitLibrary(p, spec)`); a backend that does not implement
  it returns the shared sentinel `ErrLibraryUnsupported`. So library support grows
  target by target without widening the base `Backend` contract.

## The CLI (`cmd/rune/build.go`)

`rune build FILE [name] [--target T] [--kind app|library] [--module M]
[--export Rune[:Host]]... [--out dir]`.

`runBuildCLI` is the raw-argv handler (parse flags -> read file -> `Build` -> write),
mirroring `runDeploy`'s shape so the whole dispatch path is one test seam. With no
`--out`, artifacts print to stdout prefixed `# <path>`; with `--out`, they are written
under that directory (status: `wrote N file(s)`). Artifacts are emitted in sorted-path
order for determinism.

## Status -- first slice LANDED (in flight, not yet tagged)

- **APP mode: all 8 backends.** `Build` wraps `Emit`; every backend yields its source
  artifact at the canonical path (`main.go`/`main.js`/`module.wat`/…) with a run hint.
  Gated by `TestBuildAppArtifactAllBackends`.
- **LIBRARY mode: Go only** (`codegen/golang_library.go`). `Go.EmitLibrary` re-emits the
  program with `Main` cleared, rewrites `package main` -> `package <pkg>`, drops
  `func main`, exposes `Show` plus one exported wrapper per `Export` (raw curried-`any`
  ABI: `func Name(args ...any) any`). The header is stamped `rune build (go backend,
  library)`, not the `rune emit` app shadow. The artifact is verified to genuinely
  COMPILE + run: `TestBuildGoLibraryArtifactCompiles` writes a temp module and runs
  `go test` against the emitted package. Other backends return `ErrLibraryUnsupported`.
- **CLI dispatch** covered end to end (`TestRunBuildCLIWritesOutDir`,
  `TestRunBuildCLIMissingFile`, `TestParseBuildArgs`).
- **Furnace consumer wired.** `furnace/furnacecore/furnacecore.rune` is built with
  `rune build --target go --kind library` into `furnace/internal/furnacecore`; the
  existing Furnace validator imports that generated package and calls the Rune
  `nonzeroNat` check on live validation paths. This closes the Standing Rule 1
  consumer gap for the first library-mode slice.

## Open / the honest remainder

- **Library mode beyond Go.** Rust crate / JVM package / JS-Python module / BEAM module
  / C library / WASM component each await a `LibraryBackend` impl, consumer-driven.
- **Typed host ABI.** The Go wrapper is intentionally raw (`...any -> any`); typed
  wrappers wait until Furnace fixes the host boundary types (the marshalling R-FFI
  already does for scalars/String/Ptr is the template).
- **Manifests / multi-file artifacts.** `ArtifactRole` admits `manifest`/`header`/
  `runtime`, but only single-source artifacts ship today (no `go.mod`/`Cargo.toml`
  emission).

No core change, no hash bump. The boundary holds: this mutates only emitted shadow
output, never core/store (Standing Rule 4).
