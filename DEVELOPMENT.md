# Development environment

Everything `goforge.dev/rune` needs to build, test, and exercise every backend and
the wavelet infra layer. **Run `./bin/setup.sh`** to provision the toolchains and
report what (if anything) still needs a system package. The Go toolchain + `go test
./...` alone covers the pure language core; the dependencies below unlock the
multi-backend conformance gates and the D3/D4/E4 tiers.

## The kernel is authored in Go+ (v3.383.0; elaborate/store since v3.384.0)

`core/`, `quantity/`, `equality/`, `elaborate/`, and `store/` are written in
[Go+](https://goforge.dev/goplus) — the `.gp` files are the source of
truth and the committed `*_gp.go` files are their generated Go, so
plain `go build`/`go test` needs NO extra toolchain. To EDIT the
kernel: change the `.gp` file, then regenerate with
`go run goforge.dev/goplus/cmd/goplus@v0.14.0 gen .` in that directory (the
pinned `//go:generate` line in `core/hash.go` does the same via
`go generate ./core/...`). Never edit a `*_gp.go` file by hand.
`cmd/hashdump` is the hash-stability harness: run it before and after
any kernel change and diff — the content-hash format is frozen and any
difference is a regression.

## How the project resolves its tooling (project-specific notes)

- **Language toolchains are pinned in `.tool-versions` (asdf).** The project pins
  `java temurin-25` and `opentofu` — the two things the machine-global config gets
  "wrong" for this repo. Go/Erlang/Node/Python/Rust are resolved from `PATH` (asdf,
  volta, pyenv, rustup, or system — any satisfying the minimums below works).
- **Java 25 via an asdf glob, not active PATH.** The JVM backend targets Java 25+.
  `harness.findJava25` locates `~/.asdf/installs/java/temurin-25*/bin/javac` directly,
  so the JVM conformance gate runs even when the active `java` is older. `asdf install`
  in this directory makes 25 the in-project default too.
- **Container runtime is docker OR podman.** The FOSS backends emit OCI-standard
  `compose.yaml` specs (`# podman-compose / docker-compose`), so either runtime brings
  them up. Rootless podman additionally needs `newuidmap`/`newgidmap` (`apt-get install
  uidmap`, requires root); docker has no such requirement.
- **HCL fmt is tofu OR terraform.** `infra`'s `iacBinary` prefers `tofu`, falls back to
  `terraform`; the fmt-check sweep skips only if neither is on `PATH`.

## Dependency matrix

| Dependency | Needed by | Provisioned by |
|---|---|---|
| Go ≥ 1.24 | the kernel + every Go-side test | system / asdf `golang` |
| Erlang/OTP (escript) | D5 live OTP on the BEAM backend | asdf `erlang` / `apt install erlang` |
| Node.js ≥ 20 | JS backend `rune run` | volta / asdf `nodejs` |
| Python 3.11+ + numpy | D4 NumPy interop (py backend) | pip (`numpy`) |
| Python matplotlib | D4 plotting tier | pip (`matplotlib`) |
| Python.h (python3-dev) | D4 CPython-embed path | pyenv / `apt install python3-dev` |
| Rust (rustc/cargo) | Rust source backend | rustup |
| Java 25+ (temurin) | JVM backend (targets Java 25) | **asdf `java` (.tool-versions)** |
| libopenblas + cblas.h | D3 native BLAS (C/LLVM link `-lopenblas`) | `apt install libopenblas-dev` |
| cc + clang | C and LLVM native backends | build-essential / clang |
| docker **or** podman + compose | E4 wavelet live round-trip (run FOSS compose specs) | docker / `apt install podman uidmap` |
| tofu **or** terraform | E4 HCL `fmt -check` sweep | **asdf `opentofu` (.tool-versions)** / terraform |

## Quick start

```sh
# clone, then:
asdf install            # java-25 + opentofu, per .tool-versions
./bin/setup.sh          # pip libs + report any missing system packages
go test -timeout 30m ./...   # full suite (skips a gate only if its dep is absent)
```

The `-timeout 30m` matters: the `harness` package compiles hundreds of listings across
seven real toolchains, so it can exceed Go's default 10-minute per-package timeout on a
modest machine. That timeout surfaces as `FAIL ... 600.019s` with zero `--- FAIL:`
assertion lines; it is a wall-clock artifact, not a correctness failure.

System packages that require root (only if `bin/setup.sh` reports them missing):

```sh
sudo apt-get install -y libopenblas-dev build-essential clang erlang python3-dev
# container runtime — docker, or rootless podman:
sudo apt-get install -y podman uidmap
```
