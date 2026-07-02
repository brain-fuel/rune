# WASM Bin over ARC (Wavelet beta item 6c): Design

Date: 2026-07-02
Status: Draft, pending author review (author AFK at the big-buffer question; the
recommended option was taken and is flagged below for confirmation)
Seed: main @ v3.354.0 (9-way bible divergence-lock incl WASM; 6b-2 steady-flat;
WASM partials landed)

## Premise

WASM is the only backend with no `Bin` representation. The other eight lower
Bin to a real byte container (JS Array of Numbers, Go []int, Rust Vec<u8>, JVM
int[], BEAM byte list, C/LLVM `K_BYTES=8` under mark-sweep GC); WASM has
neither a byte kind nor the `binEmpty/binCons/binLen/binAt/printBin` ops, and
`LitBytes` is not even an `ILit` case. Strings on WASM today are
I/O-boundary-only `K_BIG` bignums (base-256 packed) whose codec costs one
bignum long-division per byte: no O(1) byte indexing exists.

6c gives WASM a first-class refcounted byte buffer: the prerequisite for 6d
(WebRTC shim sends/receives byte payloads; JS glue must read WASM bytes
directly, not decode a bignum per message) and 6f (CRDT state serializes to
bytes to cross the channel).

## Decision 1: K_BIN, a packed-byte ARC leaf kind

New heap kind `K_BIN = 8` in `codegen/wasm_runtime.go` (aligns with native's
`K_BYTES = 8` for the 6e ARC-port; WASM's `K_BOUNCE` stays 7, kind numbers
never cross a boundary so the K_BOUNCE 7-vs-9 divergence stays documented,
not fixed).

Layout (payload words after the generic [size][refcount] header):

    [+0] kind word = 8
    [+4] byte length n
    [+8 ...] raw bytes, packed, allocation rounded to 4 bytes

Packed bytes, NOT native's slot-per-byte: what ports to 6e is the leaf
discipline (no child recursion), not the layout, and packed is 4x denser and
is what the 6d JS glue wants to read via a (ptr,len) pair.

ARC integration:
- `$rt_free`: explicit leaf case (like K_BIG, no child-pointer recursion).
- `$rt_mkbin (param $n i32) (result i32)`: allocates 8 + round4(n) payload,
  writes kind + length, returns pointer with refcount 1. Bytes filled by the
  caller (a `$rt_bin_set`/direct stores, mirroring `rt_con_set` style).
- retain/release/`$live`/`rt_counted` need no change: K_BIN is just another
  counted pointer.
- `$show`: a K_BIN arm rendering exactly what the other backends print for a
  Bin (the conformance gate output of ch483 is the contract; match it
  byte-for-byte, whatever form the shared printer uses).

## Decision 2: free-list big buckets (steady-flat for large payloads)

The existing free list pools payloads under 256 bytes only; larger frees are
orphaned. CRDT/WebRTC payloads routinely exceed 256B and the 6f demo is a
long-running message loop, so orphaning would slowly grow memory, regressing
exactly what 6b-2 bought.

Extend the allocator with coarse power-of-two size classes for payloads in
[256B, 64KB]: 9 extra buckets (256, 512, ..., 64K), same intrusive
singly-linked scheme, allocation rounds the payload up to the bucket size
(bounded internal fragmentation, max 2x). Frees above 64KB stay orphaned
(nothing in the demo approaches it; parked with a one-line rationale).

This benefits every future large leaf (not just K_BIN); K_BIG allocations
also flow through it for free.

AUTHOR-CONFIRM: this is the recommended option taken while the author was
AFK. Alternatives considered: accept-orphaning (smallest change, slow leak
per message) and exact-fit large list (better mixed-size reuse, adds scan +
fragmentation logic the demo does not need).

## Decision 3: codegen wiring (parity, not novelty)

- `ILit` gains a `LitBytes` case: emit `$rt_mkbin` + byte stores (constant
  data; long literals may use a passive data segment + `memory.init` if size
  warrants, implementer's call, output-identical either way).
- `binEmpty/binCons/binLen/binAt/printBin` join WASM's foreign vocabulary
  (`wasmSupportedForeign`) with WAT bodies whose semantics come verbatim from
  the other backends (ioprims.go:140-141 is the contract): `binCons` prepends
  the LOW BYTE of the nat (& 255; new buffer, byte 0 = head), `binAt`
  out-of-range returns nat 0,
  `printBin` prints the shared rendering and returns the Bin.
- Perceus: no pass change expected. A K_BIN is produced owned by its
  constructor op and consumed/dropped like any owned value; the ownership
  pass already handles opaque leaf values (K_BIG precedent). The steady-flat
  receiver (below) is the check that this claim holds.
- Packed-String K_BIG codec: UNTOUCHED. The bible/D6 ops keep their 9-way
  byte-identical lock. Bin and packed String coexist on WASM exactly as they
  already coexist on C/LLVM. No migration in 6c (no consumer; Rule 1).

## Testing

1. Conformance: WASM joins the existing Bin gate family, mirroring how
   TestBytesNative/TestBytesJVM joined: a `TestBytesWasm` running
   ch483_bytes (and the other Bin listings the existing gates run) under
   wasmtime, byte-identical to the 8-way reference.
2. ARC unit tests: K_BIN alloc/release balance, `$live` returning to
   baseline, big-bucket reuse (allocate 1KB, release, allocate 1KB again:
   same pointer, `$live` flat), the >64KB orphan path documented by a test
   that shows `$live` balanced even though memory is not reused.
3. Steady-flat receiver: a loop that builds/serializes/releases a >256B Bin
   per iteration reaches zero per-run `$live` increment (the 6f message-loop
   shape; this is the payoff test proving Decision 2 mattered).
4. Sabotage-style check for the free-list change: existing ARC tests
   (TestARCFreeListReuse et al) stay byte-for-byte green; the small-object
   path is untouched.

## Non-goals

- No LitStr support on WASM (packed-String literals stay v1-unsupported; Bin
  is the byte story).
- No K_BIG codec migration to K_BIN internals.
- No renumbering of WASM K_BOUNCE to native's 9.
- No 6e work (ARC on C/LLVM) beyond keeping K_BIN a portable leaf.
- No WebRTC/JS glue (that is 6d; 6c only guarantees the (ptr,len) byte layout
  6d will read).

## Consumers

- 6d WebRTC shim: sends/receives K_BIN payloads via (ptr,len).
- 6f demo: CRDT state serialization to Bin.
- Immediate: the Bin conformance gates (ch483 family) extend to WASM,
  closing the last "WASM is the only backend without X" row for Bin.
