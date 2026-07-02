# Bible Ops Cross-Backend Tier 4 (WASM) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port the packed-String codec, all 11 "bible" host ops, and the 7 D6 file/env/argv ops to the WASM (WAT / wasmtime) backend, then fold WASM into the cross-backend divergence-lock so the bible builders are byte-identical across NINE backends (js/go/py/rust/erl/jvm/c/ll/wasm).

**Architecture:** WASM is the sandboxed backend and the LEAST provisioned: today it rejects ANY `foreign` op (`wasmCheckSupported` errors), has no file-I/O WASI imports (only `fd_write`/stdout), no bignum division, no foreign-op emit hook, and rejects IO-main. This tier builds that infrastructure first (a foreign-op whitelist + a `CForeign`→named-WAT-func emit path + `emitForeignPrimsWasm` + the WASI imports + IO-main), then the codec (`$d6_s2h`/`$d6_h2s` over the existing base-1e9 bignum, needing one new `$big_divmod_small`), then the op bodies in layers (pure → file-read → file-write → env/argv). A Rune `String` is a `K_BIG` bignum exactly as on C/LLVM, so byte-exactness is free (raw bytes, no charset). The one op the sandbox forbids is `dbApply` (no WASI subprocess): WASM writes the `.sql` file and the host runs sqlite3 out-of-band.

**Tech Stack:** Go (the emitter, `codegen/wasm.go` + `codegen/wasm_runtime.go`); emitted WAT run under `wasmtime` (`~/.wasmtime/bin/wasmtime` or PATH); WASI snapshot_preview1 (`path_open`/`fd_read`/`fd_readdir`/`fd_write`/`fd_close`/`environ_*`/`args_*`/`proc_exit`); the `harness/bible_conformance_test.go` divergence-lock as the consumer.

## Global Constraints

- **Kernel FROZEN.** Touch only `codegen/wasm.go`, `codegen/wasm_runtime.go`, `harness/bible_conformance_test.go`, `harness/io_os_test.go`, `codegen/wasm_test.go`, and `PARKING-LOT.md`. NO changes to `core/`, `store/`, `elaborate/`. NO hash-format bump.
- **BYTE-EXACT.** The corpus is non-ASCII Greek/Hebrew. A `String` is a `K_BIG` bignum; the codec is bignum↔raw-bytes (base-256, leading-`1` sentinel), byte-identical to the C/Rust `_s2h`/`_h2s`. No charset/UTF-8 transform anywhere; `printStrCode` writes raw bytes via `fd_write`.
- **WASM is sandboxed.** File access requires wasmtime `--dir=<preopen>`; env requires `--env`; argv passes after `--`. `dbApply` CANNOT shell out (no WASI subprocess): on WASM it WRITES the `.sql` and the host loads it (the divergence-lock's byte-compared artifact is the `.sql`/`.jsonl` produced by writeChunk/sortFile, NOT dbApply).
- **Honesty gate preserved.** `wasmCheckSupported` must keep rejecting genuinely-unsupported foreigns (unknown names) with a clear error and keep `TestWasmRejectsUnsupported` meaningful — only the whitelisted prims become supported.
- **The op semantics are the fixed contract.** The C bodies (`codegen/c.go`, Tier 3) and Rust bodies (`codegen/rust.go:186-262`) are the semantic reference; every WASM op must compute the byte-identical result. The 8 source+native backends already agree (sha256 `cff27bc` shared-root / `c4246e3` lexicon.sql).

## Reference: confirmed WASM runtime facts (from the runtime map)

- **Value cell** (linear memory, 4-byte words): `K_CLO=0 [0][code_idx][nenv][env..]`, `K_CON=1 [1][tag][name_ptr][nfield][slot..]`, `K_PAIR=2`, `K_UNIT=5` (`$UNIT` global / `$rt_unit`), `K_BIG=6 [6][nlimbs][limb0..]` base-1e9 LE, `K_BOUNCE=7`. Immediate int = `(n<<1)|1`.
- **Allocator** `$alloc (param $n i32)(result i32)`: bump + size-classed freelist; 8-byte hidden header `[size][rc=1]`. **ARC**: `$rt_retain`/`$rt_release`/`$rt_free`; `$rt_con_set`/`$rt_clo_set` MOVE ownership into the slot (do not also release the moved value); scratch bignums you allocate must be `$rt_release`d when done (template: `$rt_big_succ` at wasm_runtime.go:236). `$rt_unit` is immortal (retain/release are no-ops). No `$malloc`/`$free` — for scratch bytes use a dedicated linear-memory region below the cstr area (the `[24,512)` gap) or keep the divmod loop in locals.
- **Bignum PRESENT** (wasm_runtime.go): `$big_alloc`,`$big_nlimbs`,`$big_limb`,`$big_setlimb`,`$big_norm`,`$rt_big_from_long`,`$big_add`,`$big_cmp`,`$rt_big_succ`,`$rt_nat_mul`,`$rt_nat_monus`,`$rt_big_parse`,`$rt_big_cmp`. **MISSING**: any bignum division — must write `$big_divmod_small (param $v i32)(param $d i32)(result i32 i32)` returning (quotient_ptr, remainder_i32).
- **apply**: `$rt_apply (param $clo i32)(param $arg i32)(result i32)` via `call_indirect (type $codety)` where `$codety = (func (param i32 i32)(result i32))`. `apply(apply(apply(step,s),line),unit)` = three `$rt_apply` calls; the intermediate closures must be `$rt_release`d (template: the trampoline at wasm_runtime.go:609). World token = `(call $rt_unit)`.
- **constructors**: `$rt_mkcon (tag)(name_ptr)(nfield)(result ptr)` + `$rt_con_set (c)(i)(x)`. nil=`tag 0,nfield 1,slot0=unit`; cons=`tag 1,nfield 3,slot0=unit,slot1=elem,slot2=tail`; none=`tag 0,nfield 1,slot0=unit`; some=`tag 1,nfield 2,slot0=unit,slot1=val`. name_ptr = an interned cstr offset (Go-side `em.intern(name)`; interned cstrs live at `[512,4096)`, `wasmStrBase=512`).
- **`$show` / `$emit_u32`** (wasm_runtime.go:362) is the structural model for writing bytes to a fd via `fd_write` + an iovec at `[16,24)`.
- **Foreign-op mechanism: DOES NOT EXIST.** `wasmCheckSupported` (wasm.go:92) errors on every `IForeign`; `emitIn`'s `CForeign` case (wasm.go:~651) returns the `(call $rt_unit)` stub; there is no `emitForeignPrimsWasm`; `printNat`/IO-main are NOT supported (IO-main rejected). All of this is built in Task 1.
- **WASI imported today**: only `fd_write`. All other imports must be ADDED, and WASI imports MUST precede `(memory ...)` in the module.
- **Test harness**: `wasmtimePath()` (wasm_test.go:24), `runWasm(t, wat)` (wasm_test.go:40) does `wasmtime run module.wat` with NO flags. `TestWasmConformsToJS` (wasm_test.go:121) is the existing cross-backend gate; `TestWasmRejectsUnsupported` (wasm_test.go:168) pins the honesty guarantee. WASM is NOT in `bibleBackends()`.

### The exact WASI preview1 imports to declare (before `(memory ...)`)

```wat
(import "wasi_snapshot_preview1" "fd_read"           (func $fd_read     (param i32 i32 i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "fd_close"          (func $fd_close    (param i32) (result i32)))
(import "wasi_snapshot_preview1" "fd_seek"           (func $fd_seek     (param i32 i64 i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "fd_readdir"        (func $fd_readdir  (param i32 i32 i32 i64 i32) (result i32)))
(import "wasi_snapshot_preview1" "path_open"         (func $path_open   (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "environ_sizes_get" (func $environ_sizes_get (param i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "environ_get"       (func $environ_get (param i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "args_sizes_get"    (func $args_sizes_get (param i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "args_get"          (func $args_get    (param i32 i32) (result i32)))
(import "wasi_snapshot_preview1" "proc_exit"         (func $proc_exit   (param i32)))
```

### WASI preview1 ABI constants (the binding contract for the syscall bodies)

These are the authoritative constant values a WASI syscall WAT body MUST use. They are the contract — transcribe them exactly and VERIFY each syscall body compiles + runs under wasmtime before moving on (do not guess variants):
- **`path_open`** params: `(fd, dirflags:i32, path_ptr:i32, path_len:i32, oflags:i32, fs_rights_base:i64, fs_rights_inheriting:i64, fdflags:i32, opened_fd_out_ptr:i32)`. The preopened dir fd is **3** (first preopen from `--dir`). `oflags`: `O_CREAT=1`, `O_DIRECTORY=2`, `O_EXCL=4`, `O_TRUNC=8`. `fs_rights_base` bits: `FD_READ=0x2`, `FD_SEEK=0x4`, `FD_WRITE=0x40`, `FD_READDIR=0x4000`, `PATH_OPEN=0x2000`, `PATH_CREATE_FILE=0x40000`. For a read-open pass rights `FD_READ|FD_SEEK`; for a write-create pass `oflags=O_CREAT|O_TRUNC (9)` + rights `FD_WRITE`; for a dir-open pass `oflags=O_DIRECTORY (2)` + rights `FD_READDIR`. `dirflags=1` (LOOKUP_SYMLINK_FOLLOW). Result: `0` = success, else errno; the opened fd is written to `*opened_fd_out_ptr`.
- **`fd_read`/`fd_write`** use an **iovec array**: each iovec = `[buf_ptr:i32][buf_len:i32]` (8 bytes). Params `(fd, iovs_ptr, iovs_len, nread_out_ptr)`; bytes transferred written to `*nread_out_ptr`. Return 0 = ok.
- **`fd_seek`** `(fd, offset:i64, whence:i32, newoffset_out:i32)`; `whence`: `SET=0,CUR=1,END=2`. (Use END to size a file, then SET back to 0.)
- **`fd_readdir`** `(fd, buf_ptr, buf_len, cookie:i64, bufused_out_ptr)`. The buffer is a packed sequence of **dirent** records: each = `[d_next:u64][d_ino:u64][d_namlen:u32][d_type:u8]` (24-byte header) immediately followed by `d_namlen` name bytes. Iterate: read header at offset, name at offset+24, advance by 24+d_namlen; `d_next` is the cookie for the next call; stop when `bufused < requested` and all consumed. `.`/`..` appear — skip them.
- **`environ_sizes_get`** `(count_out, bufsize_out)`; **`environ_get`** `(environ_ptrs_out, environ_buf_out)` fills an array of pointers + a buffer of `KEY=VAL\0` cstrings. **`args_sizes_get`/`args_get`** are identical in shape for argv (argv[0] = the module/program name).
- **`proc_exit`** `(exit_code:i32)` — terminates.

Reserve a scratch linear-memory region for these syscalls' buffers (iovec, path bytes, read/dir buffers, fd-out cells). The runtime uses `[16,24)` for the show iovec and `[512,4096)` for interned cstrs; use a fresh non-overlapping window (e.g. reserve `[65536, 131072)` by placing it above the current `$hp` start OR carve a fixed scratch page and bump `$hp`'s initial value past it — confirm the current `$hp` init in wasm_runtime.go and place the scratch below it so it never collides with heap allocation).

---

### Task 1: WASM foreign-op mechanism + printNat + IO-main (the infrastructure slice)

The smallest end-to-end slice proving the whole new path, with NO codec and NO file I/O yet. After this, WASM supports IO-main programs and the first foreign op (`printNat`, stdout-only).

**Files:**
- Modify: `codegen/wasm.go` (`wasmCheckSupported` whitelist; `CForeign` emit-by-name; new `emitForeignPrimsWasm`; IO-main support)
- Modify: `codegen/wasm_runtime.go` (add `$printNat` WAT body; if IO-main needs runtime support, add it)
- Modify: `codegen/wasm_test.go` (a `runWasmIO` helper if needed; a printNat test)

**Interfaces:**
- Produces: a `usesForeign`-gated `emitForeignPrimsWasm(b *strings.Builder, p Program)`; a supported-foreign whitelist `wasmSupportedForeign` (a `map[string]bool` or a slice); `printNat` on WASM; IO-main (`p.IOMain`) accepted. Later tasks add ops to `emitForeignPrimsWasm` and names to the whitelist.

- [ ] **Step 1: Study the existing WASM emit path + IO lowering**

Read `codegen/wasm.go`: `Emit()` (the top-level assembly of imports + runtime + defs + `$_start`), `wasmCheckSupported` (wasm.go:92), the `CForeign` case in `emitIn` (~wasm.go:651), `em.intern` (wasm.go:257), how `$_start` renders the result value (`$rt_show_line`), and where `p.IOMain` is rejected. Read `codegen/wasm_runtime.go`: `$show`/`$emit_u32`/`$fd_write` + the iovec at `[16,24)`, `$rt_apply`, `$rt_unit`, the bindIO/pureIO handling on OTHER backends (how `codegen/c.go` lowers IO-main to a sequence that runs the effects and yields the final value). Write a short note (in the report) of exactly how IO-main is lowered on C/JS so WASM mirrors it. This step writes NO code.

- [ ] **Step 2: Add the supported-foreign whitelist + route CForeign by name**

In `codegen/wasm.go`, define the whitelist (start with just `printNat`; later tasks append):
```go
// wasmSupportedForeign lists the foreign ops with a WAT body in emitForeignPrimsWasm.
// wasmCheckSupported allows these; any other foreign is still a hard, honest error.
var wasmSupportedForeign = map[string]bool{
	"printNat": true,
}
```
In `wasmCheckSupported`, replace the blanket `IForeign` error with:
```go
	case IForeign:
		if !wasmSupportedForeign[x.Name] {
			err = fmt.Errorf("codegen(wasm): foreign %q is not supported (no WASI/WAT body)", x.Name)
			return
		}
		// supported: fall through (no error)
```
In `emitIn`, change the `CForeign` case from the `(call $rt_unit)` stub to a named call:
```go
	case CForeign:
		return "(call $" + x.Name + ")"
```
(The WAT function `$<name>` is emitted by `emitForeignPrimsWasm`. The names are valid WAT identifiers — confirm none collide with runtime `$rt_*` names; the bible/D6 names do not.)

- [ ] **Step 3: Add `emitForeignPrimsWasm` + the `$printNat` body, wire it into `Emit()`**

Add to `codegen/wasm.go`:
```go
// emitForeignPrimsWasm appends WAT (func $name ...) bodies for each supported foreign op
// referenced by p, gated by usesForeign. Mirrors emitStreamPrimsC on the native backends.
func emitForeignPrimsWasm(b *strings.Builder, p Program) {
	if usesForeign(p, "printNat") {
		b.WriteString(wasmPrintNat)
	}
}
```
Call `emitForeignPrimsWasm(&b, p)` in `Emit()` AFTER the runtime preamble and BEFORE `$_start` (so `$printNat` is defined before use; confirm ordering against how defs are placed). Add the `$printNat` WAT to `codegen/wasm_runtime.go` as a Go const `wasmPrintNat` — printNat writes the decimal of its Nat arg + a newline to stdout (fd 1) then returns the arg. It is an IO op: `printNat : Nat -> IO Nat`, so it is curried `(arg)(unit)`. Reuse `$show`/`$emit_u32` for the decimal render. Model the body on `$rt_show_line` (which already renders a bignum to the show buffer + `fd_write`). Concretely, `$printNat` takes the Nat, renders it to the show buffer with a trailing `\n`, `fd_write`s to fd 1, and is wrapped in the curried closure shape the apply ABI expects (a code block taking `(arg env)` — the first apply binds the Nat, the second binds the unit world token and performs the write). Follow the closure-emission pattern the emitter already uses for a 2-arg function (study how a normal 2-ary def becomes `$mkclo` + code blocks in wasm.go; `printNat`'s accessor `(call $printNat)` must return the first closure).

**NOTE to implementer:** the exact closure shape for a foreign IO op on WASM (accessor → closure taking the value → closure taking the world token → effect) is the load-bearing pattern reused by EVERY later IO op (getEnvCode/readFileCode/foldLines/...). Nail it here and document it in the report as the template. If the emitter has no existing "foreign returns a curried closure" path, build a small helper that, given a WAT body for the innermost effect, emits the accessor + intermediate closures.

- [ ] **Step 4: Enable IO-main on WASM**

Make `Emit()` accept `p.IOMain == true`: lower the IO main the same way C/JS do (per your Step-1 note) — run the effect chain and render the final value with `$rt_show_line` (or, for an `IO Nat` main, the effects print and the final value is shown, matching the other backends' observable output). Remove/relax the `p.IOMain` rejection. The acceptance is behavioral (Step 5).

- [ ] **Step 5: Verify printNat + IO-main under wasmtime**

`ls listings/ | grep -E 'ch210'` — confirm the printNat demo (ch210 prints `1` then `2`, observable `1\n2\n2` on the source backends per CLAUDE.md D6 notes; confirm the exact expected string by running it on JS: `go build -o /tmp/runeT ./cmd/rune && /tmp/runeT run listings/ch210_*.rune main --target js`). Then:
```bash
/tmp/runeT emit listings/ch210_*.rune main --target wasm > /tmp/ch210.wat && wasmtime run /tmp/ch210.wat
```
Must match the JS output byte-for-byte. Debug the closure shape / fd_write until it does.

- [ ] **Step 6: Add a WASM printNat test + commit**

Add a focused test to `codegen/wasm_test.go` (or harness) that emits ch210 to WASM, runs under wasmtime, asserts the JS-equal output; skip if wasmtime absent. Confirm `TestWasmRejectsUnsupported` still passes (an unknown foreign still errors) and `TestWasmConformsToJS` still green.
```bash
go test ./codegen/ -run 'TestWasm' -count=1 -v
git add codegen/wasm.go codegen/wasm_runtime.go codegen/wasm_test.go
git commit -m "$(printf 'feat(codegen): WASM foreign-op mechanism + printNat + IO-main\n\nReplaces the blanket IForeign rejection with a supported-foreign whitelist, routes\nCForeign to a named WAT func, adds emitForeignPrimsWasm, enables IO-main, and lands\nprintNat (fd_write) as the first host op + the curried-IO-closure template. WASM\nnow runs IO programs. Unknown foreigns still error honestly. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: WASM packed-String codec ($big_divmod_small + $d6_s2h/$d6_h2s) + the 4 pure bible ops

**Files:**
- Modify: `codegen/wasm_runtime.go` (the `$big_divmod_small` + `$d6_s2h` + `$d6_h2s` WAT, and the 4 pure-op bodies as Go consts)
- Modify: `codegen/wasm.go` (append byteLen/splitOn/jsonStrField/sqlQuote to the whitelist + `emitForeignPrimsWasm`)
- Test: `harness/bible_conformance_test.go` (a WASM-direct pure-op gate)

**Interfaces:**
- Consumes: the Task-1 mechanism + `$rt_mkcon`/`$rt_con_set`/`$rt_apply`/bignum ops. Produces: `$d6_s2h (param $code i32)(result i32 i32)` → (byte_buf_ptr, len) into a scratch region; `$d6_h2s (param $buf i32)(param $len i32)(result i32)` → a K_BIG; `byteLen`/`splitOn`/`jsonStrField`/`sqlQuote` on WASM.

- [ ] **Step 1: Write `$big_divmod_small` (bignum ÷ small i32, the codec prerequisite)**

Base-1e9 long division by a small divisor `d` (256 for the codec). Add to `wasm_runtime.go`:
```wat
;; $big_divmod_small: divide K_BIG $v by small i32 $d; returns (quotient K_BIG, remainder i32).
;; Classic long division over base-1e9 limbs, most-significant first: carry the running
;; remainder as an i64, digit = (carry*BASE + limb)/d, carry = ...%d. Twin of Rust _big_divmod_small.
(func $big_divmod_small (param $v i32) (param $d i32) (result i32 i32)
  (local $n i32) (local $i i32) (local $q i32) (local $rem i64) (local $cur i64) (local $dd i64)
  (local.set $n (call $big_nlimbs (local.get $v)))
  (local.set $q (call $big_alloc (local.get $n)))
  (local.set $dd (i64.extend_i32_u (local.get $d)))
  (local.set $rem (i64.const 0))
  (local.set $i (local.get $n))
  (block $done (loop $lp
    (br_if $done (i32.eqz (local.get $i)))
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (local.set $cur (i64.add (i64.mul (local.get $rem) (i64.const 1000000000))
                             (i64.extend_i32_u (call $big_limb (local.get $v) (local.get $i)))))
    (call $big_setlimb (local.get $q) (local.get $i) (i32.wrap_i64 (i64.div_u (local.get $cur) (local.get $dd))))
    (local.set $rem (i64.rem_u (local.get $cur) (local.get $dd)))
    (br $lp)))
  (local.get $q) (call $big_norm (local.get $q))   ;; NOTE: fix -- see below
  (i32.wrap_i64 (local.get $rem)))
```
**Implementer correction:** the two trailing result expressions above are muddled — return exactly two values: the normalized quotient and the remainder. Use:
```wat
  (call $big_norm (local.get $q))          ;; result 1: quotient (normalized)
  (i32.wrap_i64 (local.get $rem)))         ;; result 2: remainder byte
```
Verify `$big_alloc` zero-inits limbs (it does per the map) so unset high limbs are 0. Multi-value WAT results (`(result i32 i32)`) are supported by wasmtime; confirm the module validates.

- [ ] **Step 2: Write `$d6_s2h` and `$d6_h2s`**

`$d6_s2h` decodes a String bignum to raw bytes in a scratch region; `$d6_h2s` folds bytes back. Reserve a scratch byte window (see the ABI-constants note; confirm the `$hp` init and place a fixed buffer that never collides). WAT:
```wat
;; $d6_s2h: decode packed-String bignum $code -> bytes at $D6BUF; returns (buf_ptr, len).
;; while big_cmp(b, 1) > 0: (b, rem) = divmod_small(b, 256); buf[n++] = rem; b keeps shrinking.
(func $d6_s2h (param $code i32) (result i32 i32)
  (local $b i32) (local $n i32) (local $rem i32) (local $one i32)
  (local.set $b (local.get $code))
  (local.set $one (call $rt_big_from_long (i32.const 1)))
  (local.set $n (i32.const 0))
  (block $done (loop $lp
    (br_if $done (i32.le_s (call $big_cmp (local.get $b) (local.get $one)) (i32.const 0)))
    (call $big_divmod_small (local.get $b) (i32.const 256))   ;; -> (q, rem) on stack
    (local.set $rem)                                          ;; pop rem
    (local.set $b)                                            ;; pop q  (order: rem is top; adjust if needed)
    (i32.store8 (i32.add (global.get $D6BUF) (local.get $n)) (local.get $rem))
    (local.set $n (i32.add (local.get $n) (i32.const 1)))
    (br $lp)))
  (global.get $D6BUF) (local.get $n))
```
**Implementer note:** `$big_divmod_small` returns `(quotient, remainder)` in that stack order, so the top of stack is `remainder`; pop `rem` first (`local.set $rem`) then `b` (`local.set $b`) ONLY IF the result order is (q, rem) — verify the actual multi-value pop order against Step 1 and swap the two `local.set`s if needed. Release the intermediate `$one` and each intermediate quotient per ARC (each `$big_divmod_small` allocates a new quotient bignum; the previous `$b`, once replaced, must be `$rt_release`d — add the release, matching `$rt_big_succ`'s discipline, EXCEPT do not release `$code` itself which is borrowed). Define `(global $D6BUF i32 (i32.const <scratch_offset>))` sized for the largest line (grow the scratch window or cap-and-document; the lexicon lines are < a few KB).
```wat
;; $d6_h2s: fold bytes [$buf, $buf+$len) -> packed String bignum. n=1; for i=len-1..0: n = n*256 + buf[i].
(func $d6_h2s (param $buf i32) (param $len i32) (result i32)
  (local $n i32) (local $m i32) (local $i i32) (local $byte i32) (local $t i32)
  (local.set $n (call $rt_big_from_long (i32.const 1)))
  (local.set $m (call $rt_big_from_long (i32.const 256)))
  (local.set $i (local.get $len))
  (block $done (loop $lp
    (br_if $done (i32.eqz (local.get $i)))
    (local.set $i (i32.sub (local.get $i) (i32.const 1)))
    (local.set $byte (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
    (local.set $t (call $big_add (call $rt_nat_mul (local.get $n) (local.get $m))
                                 (call $rt_big_from_long (local.get $byte))))
    (call $rt_release (local.get $n))     ;; release the old accumulator (ARC)
    (local.set $n (local.get $t))
    (br $lp)))
  (call $rt_release (local.get $m))
  (local.get $n))
```
(`$rt_nat_mul` allocates a product that `$big_add` consumes; release the intermediates carefully — follow the ARC template. The returned `$n` is owned by the caller.)

- [ ] **Step 3: Emit the 4 pure ops**

Append `byteLen`/`splitOn`/`jsonStrField`/`sqlQuote` to `wasmSupportedForeign` and to `emitForeignPrimsWasm` (as `wasm_runtime.go` consts). These are PURE (no world token, single arg → result). Port the C bodies (c.go, Tier 3) / rust.go:186-197:
- `byteLen`: `$d6_s2h(arg)` → `(buf,len)`; return `$rt_big_from_long(len)`.
- `splitOn`: 2-arg curried (sep)(code). `$d6_s2h(code)`; the separator byte = `big_limb(sep,0)` (0 if nlimbs 0); scan the buffer splitting on that byte; build the cons list in REVERSE from nil (`$rt_mkcon` 0/"nil"/1 slot0=unit; then for each segment right-to-left `$rt_mkcon` 1/"cons"/3 slot0=unit slot1=`$d6_h2s(seg)` slot2=tail). Intern "nil"/"cons" via `em.intern` in the Go emitter and pass the offsets.
- `jsonStrField`: 2-arg (field)(doc). Build needle = `"` + field bytes + `"` in a scratch buffer; find it in doc bytes; skip whitespace/`\t`/`:`; if `"`, scan to the next `"`; return `some`(`$d6_h2s(value)`) or `none`. Cell shapes none=0/some=1.
- `sqlQuote`: 1-arg. `$d6_s2h`; build output doubling `'` and wrapping in `'`; `$d6_h2s`.

Because these build constructor cells with interned names, the accessor emission must intern the names Go-side when assembling the module (before the final WAT is produced) and hard-code the resulting offsets into the emitted WAT. Confirm the interning order is stable.

- [ ] **Step 4: Verify the 4 pure ops under wasmtime**

```bash
/tmp/runeT emit listings/ch551_json_field.rune strongLen --target wasm > /tmp/a.wat && wasmtime run /tmp/a.wat    # 5
/tmp/runeT emit listings/ch557_sql_quote.rune quoteEmbedded --target wasm > /tmp/b.wat && wasmtime run /tmp/b.wat  # 6
```
Match the cross-backend values (5, 6). A mismatch is a real codec/cell bug — debug (most likely the divmod result-order pop, the scratch-buffer offset, or the cons slot order).

- [ ] **Step 5: Add a WASM-direct pure-op gate + commit**

Add `runWasmListing(t, listing, main, cwd)` to `harness/bible_conformance_test.go` (emit `--target wasm`, `wasmtime run` with the flags the op needs — pure ops need none, add `--dir`/`--env` in later tasks; return trimmed stdout + ok flag, skip if wasmtime absent). Then `TestBibleWasmPure` asserting ch551→5, ch557→6.
```bash
go test ./harness/ -run 'TestBibleWasmPure' -count=1 -v
git add codegen/wasm.go codegen/wasm_runtime.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): packed-String codec + pure bible ops on WASM\n\n$big_divmod_small + $d6_s2h/$d6_h2s over the existing base-1e9 bignum; byteLen/\nsplitOn/jsonStrField/sqlQuote port the C/Rust bodies. Byte-exact (raw bytes, no\ncharset). TestBibleWasmPure verifies 5/6 under wasmtime. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: WASM file-read WASI + foldLines/foldDir

**Files:**
- Modify: `codegen/wasm_runtime.go` (add `fd_read`/`fd_close`/`fd_seek`/`fd_readdir`/`path_open` imports; a `$d6_readfile (path_buf,path_len)->(buf,len)` helper; `$d6_opendir`/read helpers; foldLines/foldDir bodies)
- Modify: `codegen/wasm.go` (whitelist + emit)
- Modify: `codegen/wasm_test.go` + `harness/bible_conformance_test.go` (pass `--dir=.` when the listing reads files)

**Interfaces:**
- Consumes: codec (Task 2), apply ABI. Produces: `$d6_readfile` (open a preopened-relative path, slurp bytes into a scratch buffer, return (ptr,len) or (0,0) on failure); foldLines/foldDir on WASM.

- [ ] **Step 1: Add the read/dir WASI imports (before `(memory ...)`)**

Add `fd_read`, `fd_close`, `fd_seek`, `fd_readdir`, `path_open` from the verbatim import list. They must precede `(memory ...)` — find the current `fd_write` import placement and add these adjacent, all before memory. Confirm the module still validates (`wasmtime run` a trivial existing test).

- [ ] **Step 2: Write `$d6_readfile` (open + slurp a file)**

Using `path_open` (preopen fd 3, oflags=0, rights `FD_READ|FD_SEEK`), then `fd_read` in a loop into a growing scratch region (or `fd_seek` END to size then one `fd_read`), then `fd_close`. Returns `(buf_ptr, len)`; on any errno returns `(0,0)` (caller treats as "missing"). Write the path bytes to a scratch cstr region first (the path is the `$d6_s2h` of the path String — ASCII). **This is a WASI-ABI-bound body: transcribe against the ABI constants above and VERIFY it reads a known file under `wasmtime run --dir=.` before proceeding.** Test it in isolation with a tiny WAT harness that opens `harness/testdata/crlf.txt` and prints the byte count.

- [ ] **Step 3: Emit foldLines**

Port the C/Rust foldLines (rust.go:207): `$d6_readfile(path)`; on `(0,0)` return `s0` unchanged; else split the buffer on `\n` (KEEP `\r`), drop a SINGLE trailing-empty segment iff the buffer ends in `\n`, and for each segment in order `s = apply(apply(apply(step, s), $d6_h2s(seg)), unit)`, releasing intermediate closures per ARC. 5-arg curry (_s)(path)(step)(s0)(unit). The step/s0/path captured through the closure chain (mirror the Task-1 IO-closure template).

- [ ] **Step 4: Emit foldDir**

Port rust.go:213 / the C `d6_foldwalk`: open the dir (`path_open` oflags=`O_DIRECTORY`, rights `FD_READDIR`), `fd_readdir` into a buffer, parse the dirent records (24-byte header + name; skip `.`/`..`), COLLECT names, SORT them (bytewise — implement a small insertion sort over the collected name offsets, matching Go `filepath.WalkDir` per-dir lexical order), then for each: if it is a subdir recurse; else if the name ends with the suffix, `$d6_readfile` it and `apply` the step. Depth-first pre-order. **WASI-ABI-bound: transcribe the dirent parsing against the ABI layout and VERIFY against `harness/testdata/foldfix` (3 matching files → count 3) under `--dir=.`.** foldDir needs recursion — either a WAT helper `$d6_foldwalk` (a real recursive func over a dir fd) or an explicit work-stack; a recursive func is simplest.

- [ ] **Step 5: Verify + gate + commit**

```bash
( cd harness/testdata && wasmtime run --dir=. <(/tmp/runeT emit ../../listings/ch549_conllu_count.rune main --target wasm) )   # 11\n11
```
(Adjust invocation; the gate helper is authoritative.) Extend `runWasmListing` to pass `--dir=<cwd>` and set the wasmtime cwd for relative fixtures. `TestBibleWasmFold`: ch549→11\n11, ch554→3\n3, ch560→16\n16.
```bash
go test ./harness/ -run 'TestBibleWasmFold' -count=1 -v
git add codegen/wasm.go codegen/wasm_runtime.go codegen/wasm_test.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): WASM file-read WASI + foldLines/foldDir\n\npath_open/fd_read/fd_readdir imports + $d6_readfile; foldLines (\\n-split keep \\r)\nand foldDir (fd_readdir, sorted, depth-first) port the C/Rust bodies. Runs under\nwasmtime --dir. Verifies 11/3/16. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: WASM file-write WASI + write-stream (Handle/openWrite/writeChunk/closeWrite/sortFile) + dbApply (write-.sql-only)

**Files:**
- Modify: `codegen/wasm_runtime.go` (write-open helper via `path_open` O_CREAT|O_TRUNC + rights FD_WRITE; a static fd-handle table in linear memory; the 5 write bodies + dbApply)
- Modify: `codegen/wasm.go` (whitelist + emit)
- Test: `harness/bible_conformance_test.go`

**Interfaces:**
- Consumes: codec + the write WASI. Produces: `Handle` (erases to unit), openWrite/writeChunk/closeWrite/sortFile + dbApply on WASM. Write handle = a small-int token into a fixed fd table in linear memory (the native `d6_wh[256]` analogue).

- [ ] **Step 1: Write the write-open helper + handle table**

Reserve a fixed linear-memory region for a `d6_wh` table (256 i32 fd slots) + a counter global `$d6_whid`. `openWrite(path)`: `path_open` (oflags=`O_CREAT|O_TRUNC`=9, rights `FD_WRITE`) → an fd; store fd in `d6_wh[++id]`; return `$rt_big_from_long(id)`; on failure return `$rt_big_from_long(0)`. `writeChunk(h,chunk)`: id = `big_limb(h,0)`; if valid, `$d6_s2h(chunk)` then `fd_write(d6_wh[id], iovec(buf,len))` then write a single `\n`; return h. `closeWrite(h)`: `fd_close(d6_wh[id])`, clear the slot, return unit. **WASI-ABI-bound: verify a written file round-trips under `--dir=.`.**

- [ ] **Step 2: Emit sortFile**

Port the C/Rust sortFile: `$d6_readfile(in)`; split on `\n`, drop single trailing-empty; BYTEWISE sort the segments (insertion sort, byte compare = Go sort.Strings); write-open `out` (O_CREAT|O_TRUNC), write each sorted line + `\n`, close. On read failure, write-open `out` and close (empty file). Reuse `$d6_readfile` + the write helper.

- [ ] **Step 3: Emit Handle + dbApply (write-.sql-only)**

`Handle`: gated on `usesForeign(p,"Handle")`, `(func $Handle (result i32) (call $rt_unit))`. `dbApply(db, sql)`: WASM CANNOT run sqlite3 (no subprocess). Per the chosen design, dbApply on WASM WRITES the `.sql`'s already-built content is NOT dbApply's job — dbApply receives the DB path + the SQL-script path and normally shells `sqlite3 db ".read sql"`. On WASM there is no shell. Implement dbApply as a **documented no-op that returns unit** (the SQL script file itself was already written by the builder's writeChunk/sortFile; the actual sqlite3 load happens on the HOST in the gate — Task 6). Emit a WAT comment explaining the sandbox limitation. (This keeps IO-main programs that call dbApply runnable under wasmtime — they simply don't build the .db in-sandbox.)

- [ ] **Step 4: Verify + gate + commit**

```
ch552_write_stream.rune main (--dir=<tmp>) -> 2\n2
ch553_sort_file.rune main   (--dir=<tmp>) -> 5\n5
```
`TestBibleWasmWriteStream`: ch552→2\n2, ch553→5\n5, run under wasmtime with `--dir=<tempcwd>`. (dbApply/ch558 is handled in Task 6 via host-side sqlite3.)
```bash
go test ./harness/ -run 'TestBibleWasmWriteStream' -count=1 -v
git add codegen/wasm.go codegen/wasm_runtime.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): WASM file-write WASI + write-stream + dbApply(no-op)\n\npath_open create/fd_write + a linear-memory fd handle table; openWrite/writeChunk/\ncloseWrite/sortFile port the C/Rust bodies (bytewise sort). dbApply is a documented\nno-op in-sandbox (no WASI subprocess); the host loads the emitted .sql (Task 6).\nVerifies 2/5 under wasmtime. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 5: WASM D6 env/argv/exit layer (getEnvCode/readFileCode/writeFileCode/printStrCode/argAtCode/argCountCode/exitWith)

**Files:**
- Modify: `codegen/wasm_runtime.go` (environ/args/proc_exit imports + the 7 bodies)
- Modify: `codegen/wasm.go` (whitelist + emit)
- Test: `harness/io_os_test.go` (WASM D6 gate)

**Interfaces:**
- Consumes: codec + read/write helpers (Tasks 2-4). Produces: the 7 D6 ops on WASM, identical to rust.go:237-262.

- [ ] **Step 1: Add environ/args/proc_exit imports; write env/argv accessor helpers**

Add `environ_sizes_get`/`environ_get`/`args_sizes_get`/`args_get`/`proc_exit` imports (before memory). Write `$d6_getenv (key_buf,key_len)->(val_buf,val_len)` (call `environ_sizes_get`, `environ_get` into a scratch region, linear-scan for `KEY=`, return the value bytes) and `$d6_argat (idx)->(buf,len)` (via `args_sizes_get`/`args_get`, idx+1 to skip argv[0]). **WASI-ABI-bound: verify with `wasmtime run --env=RUNE_D6=ok ... ` and `... -- alpha beta`.**

- [ ] **Step 2: Emit the 7 bodies** (ports of rust.go:237-262)

- `printStrCode(c)(u)`: `$d6_s2h(c)`; `fd_write(1, iovec(buf,len))` then `fd_write` a `\n`; return c. (Raw bytes, no re-encode.)
- `getEnvCode(c)(u)`: `$d6_getenv($d6_s2h(c))`; `$d6_h2s(val)` (empty → bignum 1).
- `readFileCode(c)(u)`: `$d6_readfile(path)`; `(0,0)` → `$rt_big_from_long(1)`; else `$d6_h2s(buf,len)`.
- `writeFileCode(p)(c)(u)`: write-open (O_CREAT|O_TRUNC), `fd_write` `$d6_s2h(c)`, close; return c.
- `argCountCode(u)`: `args_sizes_get` count minus 1 (saturating); `$rt_big_from_long`.
- `argAtCode(i)(u)`: `$d6_argat(big_limb(i,0))`; out-of-range → `$rt_big_from_long(1)`.
- `exitWith(n)(u)`: `proc_exit(big_limb(n,0))`.

- [ ] **Step 3: Verify ch215/ch216 under wasmtime + gate + commit**

```bash
D=$(mktemp -d); ( cd "$D" && wasmtime run --dir=. --env=RUNE_D6=unit <(/tmp/runeT emit .../ch215_*.rune main --target wasm) )   # hello, wootz\nok\nunit
wasmtime run <(/tmp/runeT emit .../ch216_*.rune main --target wasm) -- alpha beta ; echo "exit=$?"                              # 2\nalpha\nbeta ; exit 2
```
Add `TestD6WasmFileEnv` + `TestD6WasmArgvExit` to `harness/io_os_test.go` (mirror the native D6 gates, `runWasmListing` with `--dir`/`--env`/argv; assert the exit status for ch216). Skip if wasmtime absent.
```bash
go test ./harness/ -run 'TestD6Wasm' -count=1 -v
git add codegen/wasm.go codegen/wasm_runtime.go harness/io_os_test.go
git commit -m "$(printf 'feat(codegen): D6 file/env/argv layer on WASM (parity)\n\ngetEnvCode/readFileCode/writeFileCode/printStrCode/argAtCode/argCountCode/exitWith\nvia environ/args/proc_exit WASI + the codec; printStrCode writes raw bytes. ch215/\nch216 run under wasmtime. Closes WASM D6 parity. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 6: WASM into the divergence-lock (9-way) + dbApply host-load + regression

**Files:**
- Modify: `harness/bible_conformance_test.go` (`bibleBackends()` appends WASM conditionally; the dbApply/ch558 gate runs sqlite3 host-side on WASM's emitted .sql; the real-data scale gate handles WASM)

**Interfaces:**
- Consumes: every WASM op (Tasks 1-5). Produces: WASM in `bibleBackends()` → all shared gates 9-way byte-identical.

- [ ] **Step 1: Append WASM to `bibleBackends()`**

Conditional on `wasmtimePath() != ""`. WASM's `bibleBackend`: `emit=codegen.Wasm{}.Emit`, `ext="wat"`, `bin=wasmtime`, `run=exec.Command(wasmtime, "run", "--dir=.", binOrSrc)`, no separate compile (WAT runs directly). WASM's `run` needs `--dir` (and cwd) because the builders read/write files — confirm the existing `bibleBackend` run wiring can express the `--dir` + cwd; extend minimally if not (the LLVM `runtime` field precedent shows how the struct was extended). For gates whose listing reads relative fixtures, the run must `--dir=<cwd>` and set `cmd.Dir`.

- [ ] **Step 2: Handle dbApply/ch558 host-side for WASM**

WASM's dbApply is a no-op (Task 4), so ch558 does not build the `.db` in-sandbox. In `TestBibleConformanceDbApply` (or the equivalent), for the WASM backend: after running the builder (which writes the `.sql` script via the write ops), have the HOST run `sqlite3 <db> ".read <sql>"` on the emitted artifacts, THEN assert count=2 — proving WASM produced a correct, loadable `.sql`. If restructuring the shared gate is heavy, add a dedicated `TestBibleWasmDbViaHost` that does this explicitly for WASM and exclude WASM from the in-sandbox dbApply path. Document the split.

- [ ] **Step 3: Verify all shared gates run 9-way byte-identical**

```
go test ./harness/ -run 'TestBibleConformance' -count=1 -v
```
`TestBibleConformanceBuilders` now compares WASM's `shared-root.out` + `lexicon.sql` against the other eight. They MUST be byte-identical (sha256 `cff27bc` / `c4246e3`). **A WASM divergence is a real bug in a Task 2-5 body** (codec, cell, sort, line-split, or a WASI read/write truncation) — do NOT weaken the assertion; capture the diverging bytes and fix the responsible WASM op. Only a genuine 9-way pass is acceptable.

- [ ] **Step 4: Real-data scale gate + regression + commit**

WASM will likely be slow like native (bignum codec); if the 1500-entry `TestBibleConformanceRealData` is impractical under wasmtime, exclude WASM the SAME way native c/ll are excluded (the `t.Logf`+`continue` scale-only skip, NOT the `skipped` inconclusive path — byte-identity is proven by the Builders gate over the Hebrew/Greek `lexdbfix`). Add a PARKING-LOT note if so.
```
go test ./harness/ -run 'TestBible|TestD6Wasm|TestD6Native' -count=1
go test ./harness/ -run 'TestBackendConformance' -count=1
go test ./codegen/ -run 'TestWasm' -count=1
go test ./... 2>&1 | tail -20
```
```bash
git add harness/bible_conformance_test.go
git commit -m "$(printf 'test(harness): add WASM to the bible divergence-lock (9-way)\n\nWASM joins bibleBackends(); all conformance gates now assert byte-identity across\nNINE backends (js/go/py/rust/erl/jvm/c/ll/wasm). dbApply is host-loaded on WASM\n(no in-sandbox sqlite3). Tier 4 complete: the bible builders are byte-identical on\nall nine. WASM D6 parity closed.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** User chose "Full parity (bible + WASM D6)." Infrastructure (foreign-op whitelist + CForeign routing + emitForeignPrimsWasm + IO-main + printNat) → Task 1. Codec (`$big_divmod_small`+`$d6_s2h`/`$d6_h2s`) + 4 pure ops → Task 2. File-read WASI + foldLines/foldDir → Task 3. File-write WASI + write-stream + dbApply(no-op) → Task 4. D6 env/argv/exit → Task 5. WASM into the 9-way lock + dbApply host-load + regression → Task 6. dbApply's sandbox impossibility is handled (write-only + host loads the .sql), matching the user's earlier "emit .sql, host loads."

**2. Placeholder scan.** The determinate code (codec algorithm, op logic ported from the confirmed C/Rust reference, cell-building, apply ABI, whitelist refactor, emit wiring, the exact WASI import signatures) is given in full WAT/Go. The WASI-SYSCALL bodies (`$d6_readfile`, `$d6_foldwalk`'s readdir, the write helper, env/argv) are specified by their exact import signature + the WASI preview1 ABI constants (oflags/rights/dirent layout/iovec) + the algorithm, with a MANDATORY "verify under wasmtime before proceeding" step — this is the binding contract, not a placeholder, but it is honestly flagged as ABI-transcription-plus-verification rather than pretending byte-perfect syscall WAT can be emitted sight-unseen. Two WAT snippets are deliberately flagged for correction inline (the `$big_divmod_small` trailing results; the `$d6_s2h` multi-value pop order) with the fix given. The implementer MUST verify the multi-value result order of `$big_divmod_small` empirically and adjust the two `local.set`s.

**3. Type consistency.** Foreign accessors are `(func $<name> ...)` called as `(call $<name>)` (the CForeign lowering); pure ops take value args and return values; IO ops are curried `(val)...(unit)` returning the value/handle/unit via the Task-1 IO-closure template; the apply ABI is three `$rt_apply` calls with `(call $rt_unit)` last; cells use `$rt_mkcon`+`$rt_con_set` with slot0=unit (nil=0/cons=1, none=0/some=1). The codec `$d6_s2h`/`$d6_h2s` + `$big_divmod_small` are defined in Task 2 and consumed by Tasks 3-5. `wasmSupportedForeign` grows monotonically across tasks (printNat → +4 pure → +foldLines/foldDir → +write-stream/Handle/dbApply → +7 D6). `runWasmListing` (Task 2) gains `--dir`/`--env`/argv in later tasks. Listings reused: ch210 (printNat), ch551/557 (pure), ch549/554/560 (fold), ch552/553/558 (write/db), ch215/216 (D6) — all exist. WASM joins `bibleBackends()` in Task 6 exactly as jvm (Tier 2) and c/ll (Tier 3) did. ARC discipline (release intermediate closures + scratch bignums) is called out in every op that allocates.
