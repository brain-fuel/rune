# WASM browser-library shim (Wavelet beta item 6d): Design

Date: 2026-07-03
Status: Draft, pending author review. Two AUTHOR-CONFIRM decisions taken on
recommendation while the author was AFK, flagged inline.
Seed: main @ v3.359.0 (K_BIN Bin-over-ARC landed v3.358.0; CLet double-free
guard v3.359.0)

## Premise

6f needs a two-tab browser CRDT demo: WASM holds the proven merge logic and
state, JS glue owns WebRTC and the DOM, serialized state crosses the data
channel as bytes. Today an emitted WASM module cannot serve that role: it
exports only `memory` and `_start` (an invoke-main-and-print wrapper), it
declares 12 unconditional `wasi_snapshot_preview1` imports a browser must
satisfy or instantiation throws, there is no way to call a named function
with arguments from the host, and the proven G-Counter's state is a
constructor of two nats, not a Bin.

6d closes exactly that gap: a browser-consumable library artifact with a
defined ABI, plus the proven state codec. WebRTC itself and the DOM stay in
6f; they are plain JS against the surface this item defines.

## Decision 1: passive-library model

WASM is a passive pure-function library; JS owns the event loop, the
network, and the DOM. This matches the emitted module's nature (no threads,
no scheduler, run-to-completion) and R-FFI's class-3 sandbox interop model
(the shim is host-side JS, not a native symbol). No imports are added; the
module never calls out. Rejected alternatives: an imports model (WASM
importing send/onmessage and owning the loop) fights re-entrancy for no
consumer; bare export flags without a library artifact leave every consumer
hand-rolling the loader.

## Decision 2: a WASM LibraryBackend under `rune build --kind library`

R-COEXIST's `BuildSpec`/`ArtifactSet` API (codegen/artifact.go) is the
designed home for "a checked program shipped as a host artifact"; today only
Go implements `LibraryBackend`. 6d adds the WASM one:

    rune build counter.rune --kind library --target wasm --export merge --export encode ... --out dir

emits two artifacts:

1. `<module>.wat`: the normal WASM emission plus a library export section
   (below) and an `init` export that seeds `$UNIT` WITHOUT running main
   (library programs need no main; if one exists it is simply not invoked).
2. `<module>.glue.js`: a generated, dependency-free ES module the browser
   (or node) consumes:
   - `load(watOrWasm)`: instantiates with a generated 12-function no-op
     `wasi_snapshot_preview1` stub object (the imports stay unconditional in
     codegen; stubbing at the host is the Rule-1-cheap fix), calls `init`.
   - `mkBin(uint8array) -> ptr` and `readBin(ptr) -> Uint8Array`: Bin
     marshalling over the exported constructors (never raw layout pokes,
     though readBin may use the documented (ptr+8, len) window for speed;
     the layout is part of the ABI below).
   - `call(name, ...ptrs) -> ptr`: the curried-application wrapper
     (`def_<name>()` accessor then an `rt_apply` chain).
   - `retain(ptr)`/`release(ptr)`: the ARC surface, re-exported.

## Decision 3: the export ABI (versioned, documented in the artifact)

The library module exports, in addition to the existing `memory`:

    init                        seed $UNIT; MUST be called once before any other export
    rt_apply(clo, arg) -> val
    rt_retain(v)  rt_release(v)
    rt_mkbin(len) -> bin        rt_bin_set(bin, i, byte)
    rt_bin_len(bin) -> len      rt_bin_at(bin, i) -> byte
    rt_live() -> n              live heap-block count (ARC balance probe, test surface)
    rt_nat_to_u32(v) -> i32     saturating nat read (the glue readNat helper; added
                                during implementation, emitted in library mode only)
    def_<name>() -> clo         one per --export'ed definition (memoized accessor)

Ownership contract (documented in the glue header and R-FFI's table): values
returned to JS are owned by JS (release when done); arguments passed to
`rt_apply` are consumed (moved) exactly as in-module; the `def_<name>`
accessor result is borrowed (the memoized cache owns it) and MUST NOT be
released by JS: retain first if a reference is kept. Bin payload bytes live
at `ptr+8` for `rt_bin_len(ptr)` bytes; this (ptr,len) window is ABI, stable
per the K_BIN design.

`_start` remains for app-mode modules; library mode does not emit it (no
main invocation, no stdout print), only `init`.

## Decision 4: the proven state codec (AUTHOR-CONFIRM: recommended option
taken while AFK; alternatives were an unproven codec at assume tier, or a
JS-side codec reading heap internals)

New listing: `encodeGC : GC -> Bin` and `decodeGC : Bin -> GC` in rune, with
the machine-checked round-trip theorem `decodeGC (encodeGC s) = s` for all
s, joining the proven tier beside ch72's mergeComm/Idem/Assoc. Wire format:
each of GC's two nat slots encoded length-prefixed base-256 (1 length byte
suffices only for bounded nats, so: a length byte N followed by N base-256
digits, little-endian; counts beyond 255 digits are outside the demo's
universe but the encoding stays total by construction since a nat's digit
list is finite). The exact listing-level encoding may be adjusted by the
implementer if the round-trip proof demands (e.g. digit lists via the
existing radix machinery); the REQUIREMENTS are fixed: total encode, total
decode over well-formed images, round-trip theorem proven, output is Bin.

Merge stays over GC with its existing convergence certificate; the codec is
the only new proof obligation. This keeps the demo's honest claim: state,
merge, and the wire representation are all machine-checked.

## Decision 5: the browser-shaped test gate (AUTHOR-CONFIRM: recommended
option taken while AFK; alternatives were a wasm-tools system binary, or
wasmtime --invoke only which never exercises a JS host)

New harness gate `TestWasmBrowserLibrary` (skips cleanly when node or the
assembler is absent, like every toolchain gate):

1. `bin/setup.sh` gains a harness-local `npm install wabt` (pinned version)
   into a git-ignored `harness/node_modules`; the gate requires it the way
   other gates require javac or wasmtime.
2. The gate builds the library artifact for a G-Counter program exporting
   `merge`, `bump`, `value`, `encodeGC`, `decodeGC`; assembles the WAT via
   wabt in node; instantiates TWO instances with the glue (stub WASI, init).
3. Drives the full 6f flow minus WebRTC/DOM: instance A bumps twice, B bumps
   once; A encodes its state, the bytes cross (a plain JS byte copy standing
   in for the data channel), B decodes and merges, and symmetrically B to A;
   assert both instances' `value` agree (3) and match the in-rune expected
   value; assert ARC balance by releasing everything and checking `rt_live`
   returns to its post-init baseline.
4. A second scenario reverses message order (B's state arrives at A first)
   and asserts the same converged value: the commutativity the proof
   guarantees, observed at the ABI.

Existing wasmtime gates are untouched; app-mode emission is byte-identical.

## Non-goals

- No WebRTC, no DOM, no browser automation (6f).
- No import-block gating in codegen (the JS stub covers it; park a note if a
  zero-import pure-module story ever gains a consumer).
- No MV-Register / note CRDT (G-Counter is the demo payload; the note
  variant is a 6f stretch goal with its own codec + proofs when chosen).
- No general FFI-decl-driven JS shims (R-FFI class 3's per-decl target
  table); this is one fixed library ABI, not the general mechanism.
- No library-mode support on other backends beyond what exists (Go).

## Consumers

- 6f: consumes the glue + ABI directly; adds only WebRTC signaling, the DOM,
  and the two-tab page.
- The funding-pitch demo narrative: "the bytes on the wire are the output of
  a proven codec" becomes literally true.
- The 1.0 embedded-mode pillar (library emission beyond Go) gets its second
  LibraryBackend and its first browser-class one.

## Testing summary

Proven tier: the round-trip theorem + existing ch72 certificates (checked by
the listings gate). Runtime: TestWasmBrowserLibrary two-instance convergence
+ order-reversal (node + wabt). Regression: full existing WASM suite
byte-identical; `rune build` Go library mode untouched; listings gate green
with the new codec listing.
