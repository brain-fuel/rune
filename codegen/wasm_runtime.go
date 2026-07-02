package codegen

// wasm_runtime.go - the NINTH backend's WAT runtime: a self-contained WebAssembly
// runtime emitted INLINE in the module (unlike the C/LLVM backends, whose runtime is
// a linked C blob - wasmtime runs a single `.wat` with no host linkage, so the whole
// runtime IS WAT here). It is the design-locked "R-NATIVE Cranelift/WASM" node, shipped
// as a WAT emitter testable via `wasmtime run module.wat`.
//
// VALUE REP (mirrors the C/LLVM rep, narrowed to i32): a Rune value is an i32 that is
// either an IMMEDIATE small int - `(n<<1)|1`, low bit set - or an even-aligned heap
// pointer into linear memory (low bit clear). The emitter never inspects the tag; it
// defers to the `rt_*` helpers below, so the rep stays owned by ONE place.
//
// HEAP LAYOUT: a bump allocator over linear memory (no collection in v1 - see the GC
// note in wasm.go). Every object starts with a kind word; the field layout per kind:
//
//	K_CLO  [kind=0][code_idx][nenv][env0][env1]...     code_idx indexes the func table
//	K_CON  [kind=1][tag][name_ptr][nfield][slot0]...   name_ptr -> a NUL-terminated cstr
//	K_PAIR [kind=2][a][b]
//	K_UNIT [kind=5]                                    the boxed unit singleton
//	K_BIG  [kind=6][nlimbs][limb0][limb1]...           base-1e9 little-endian bignum
//
// All words are 4 bytes. The function table holds every code block + constructor/
// eliminator code block; AppClosure loads the closure's code_idx and `call_indirect`s
// it with (arg, env_ptr). `$show` renders to a scratch buffer and writes it to stdout
// via the WASI `fd_write` import (so wasmtime prints the observable integer).

// wasmRuntime is the WAT runtime: memory, the bump allocator, the value constructors/
// accessors, apply via call_indirect, the naive base-1e9 bignum (builtin-nat), and the
// $show writer over WASI fd_write. It is emitted verbatim into every module, before the
// program's own functions. The `$table`/`$elem` and `$rune_main` are emitted by wasm.go
// (they depend on the program), as is the final `(start)`/export wiring.
const wasmRuntime = `
  ;; ---- WASI imports (all imports MUST precede the memory/func definitions) ----
  ;; write the show buffer to stdout (fd 1)
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  ;; read a clock: clock_time_get(clock_id, precision, time_out_ptr) -> errno; writes an
  ;; i64 nanosecond count to *time_out_ptr. clock_id 1 = MONOTONIC. Used by timeNanos
  ;; (Task 1). Declared unconditionally -- wasmtime provides every preview1 import for a
  ;; _start module, so an unused import in a non-IO module is harmless.
  (import "wasi_snapshot_preview1" "clock_time_get"
    (func $clock_time_get (param i32 i64 i32) (result i32)))
  ;; ---- file/dir WASI (Task 3): read-open, streamed read, seek, close, readdir ----
  ;; Declared unconditionally (like clock_time_get): wasmtime provides every preview1
  ;; import for a _start module, so an unused import in a non-IO module is harmless. Their
  ;; WAT bodies ($d6_readfile / $d6_foldwalk) are baked only when a file/dir foreign is
  ;; present. All imports MUST precede the memory/func definitions.
  (import "wasi_snapshot_preview1" "fd_read"    (func $fd_read    (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_close"   (func $fd_close   (param i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_seek"    (func $fd_seek    (param i32 i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_readdir" (func $fd_readdir (param i32 i32 i32 i64 i32) (result i32)))
  (import "wasi_snapshot_preview1" "path_open"
    (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))

  ;; ---- linear memory + a bump allocator (no GC in v1) ----
  (memory (export "memory") 256)        ;; 256 pages = 16 MiB initial
  ;; heap pointer; below it: scratch + cstrs. The reserved low region is [0, 1573888):
  ;; iovec [16,24), timeNanos [24,32), fixed msgs [32,512), interned cstrs [512,4096),
  ;; show buffer [4096,...), freelist [8192,8448), the bible-codec byte buffers
  ;; $D6BUF/$D6BUF2/$D6BUF3 (64 KiB each at 65536/131072/196608, declared in the codec),
  ;; the Task-3 file/dir WASI scratch (declared in wasmBibleReadFile/wasmBibleFoldDir):
  ;; $D6SYS syscall cells [262144,327680), $D6PATH path-build [327680,393216), $D6DIR
  ;; dirent buffer [393216,458752), $D6NAMES foldwalk index [458752,524288),
  ;; $D6SLURP 1 MiB whole-file slurp window [524288,1572864), and the Task-4 fd handle
  ;; table $D6WH (256 i32 slots = 1 KiB at [1572864,1573888), declared in wasmBibleWriteOps).
  ;; Heap allocation starts at 1573888 so it never overwrites any scratch windows.
  (global $hp (mut i32) (i32.const 1573888)) ;; heap pointer; scratch below, heap above
  (global $UNIT (mut i32) (i32.const 0))    ;; the boxed unit singleton (set in init)
  (global $live (mut i32) (i32.const 0))   ;; count of live heap blocks (ARC leak probe)
  (global $freelist (mut i32) (i32.const 8192)) ;; 64 i32 bucket heads at [8192, 8448)

  ;; ARC alloc: reserve an 8-byte hidden header [size][rc] below the payload. Rounds the
  ;; requested payload size to 4-byte alignment, stores the rounded size and rc=1, bumps
  ;; $live, returns the PAYLOAD pointer. Before bumping $hp, checks the size-classed free
  ;; list ($freelist buckets indexed by rounded_size/4): if the matching bucket is non-empty
  ;; the head block is popped, its rc reset to 1, and returned without touching $hp.
  (func $alloc (param $n i32) (result i32)
    (local $base i32) (local $payload i32) (local $bkt i32) (local $head i32)
    (local.set $n (i32.and (i32.add (local.get $n) (i32.const 3)) (i32.const -4)))
    ;; try the size-classed free list first
    (local.set $bkt (call $rt_bucket_addr (local.get $n)))
    (if (local.get $bkt)
      (then
        (local.set $head (i32.load (local.get $bkt)))
        (if (local.get $head)
          (then
            ;; pop: advance bucket head to the link stored in the freed block's first word
            (i32.store (local.get $bkt) (i32.load (local.get $head)))
            ;; reset rc=1 at [head-4]
            (i32.store (i32.sub (local.get $head) (i32.const 4)) (i32.const 1))
            (global.set $live (i32.add (global.get $live) (i32.const 1)))
            (return (local.get $head))))))
    ;; bump allocate from $hp
    (local.set $base (global.get $hp))
    (local.set $payload (i32.add (local.get $base) (i32.const 8)))
    (global.set $hp (i32.add (local.get $payload) (local.get $n)))
    (i32.store (local.get $base) (local.get $n))                         ;; [payload-8] = size
    (i32.store (i32.add (local.get $base) (i32.const 4)) (i32.const 1)) ;; [payload-4] = rc
    (global.set $live (i32.add (global.get $live) (i32.const 1)))
    (local.get $payload))

  ;; immediate-int tag helpers (FFI LitInt path; nats are K_BIG below).
  (func $rt_mkint (param $n i32) (result i32)
    (i32.or (i32.shl (local.get $n) (i32.const 1)) (i32.const 1)))
  (func $is_int (param $v i32) (result i32)
    (i32.and (local.get $v) (i32.const 1)))

  ;; load/store a 4-byte word at obj+i*4.
  (func $w (param $o i32) (param $i i32) (result i32)
    (i32.load (i32.add (local.get $o) (i32.shl (local.get $i) (i32.const 2)))))
  (func $sw (param $o i32) (param $i i32) (param $x i32)
    (i32.store (i32.add (local.get $o) (i32.shl (local.get $i) (i32.const 2))) (local.get $x)))

  ;; ---- closures: [K_CLO=0][code_idx][nenv][env0..] ----
  (func $rt_mkclo (param $code i32) (param $nenv i32) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.add (i32.const 12) (i32.shl (local.get $nenv) (i32.const 2)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 0))      ;; K_CLO
    (call $sw (local.get $o) (i32.const 1) (local.get $code))
    (call $sw (local.get $o) (i32.const 2) (local.get $nenv))
    (local.get $o))
  (func $rt_clo_set (param $c i32) (param $i i32) (param $x i32)
    (call $sw (local.get $c) (i32.add (i32.const 3) (local.get $i)) (local.get $x)))

  ;; apply: read code_idx, call_indirect with (arg, env_base). env_base = c+12.
  (func $rt_apply (param $clo i32) (param $arg i32) (result i32)
    (call_indirect (type $codety)
      (local.get $arg)
      (i32.add (local.get $clo) (i32.const 12))
      (call $w (local.get $clo) (i32.const 1))))

  ;; env-slot load (used by emitted code blocks): env_base + i*4.
  (func $rt_env (param $env i32) (param $i i32) (result i32)
    (i32.load (i32.add (local.get $env) (i32.shl (local.get $i) (i32.const 2)))))

  ;; ---- constructors: [K_CON=1][tag][name_ptr][nfield][slot0..] ----
  (func $rt_mkcon (param $tag i32) (param $name i32) (param $nfield i32) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.add (i32.const 16) (i32.shl (local.get $nfield) (i32.const 2)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 1))      ;; K_CON
    (call $sw (local.get $o) (i32.const 1) (local.get $tag))
    (call $sw (local.get $o) (i32.const 2) (local.get $name))
    (call $sw (local.get $o) (i32.const 3) (local.get $nfield))
    (local.get $o))
  (func $rt_con_set (param $c i32) (param $i i32) (param $x i32)
    (call $sw (local.get $c) (i32.add (i32.const 4) (local.get $i)) (local.get $x)))
  (func $rt_con_get (param $c i32) (param $i i32) (result i32)
    (call $w (local.get $c) (i32.add (i32.const 4) (local.get $i))))
  (func $rt_con_tag (param $c i32) (result i32)
    (call $w (local.get $c) (i32.const 1)))
  (func $rt_con_nfield (param $c i32) (result i32)
    (call $w (local.get $c) (i32.const 3)))

  ;; ---- pairs: [K_PAIR=2][a][b] ----
  (func $rt_mkpair (param $a i32) (param $b i32) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.const 12)))
    (call $sw (local.get $o) (i32.const 0) (i32.const 2))
    (call $sw (local.get $o) (i32.const 1) (local.get $a))
    (call $sw (local.get $o) (i32.const 2) (local.get $b))
    (local.get $o))
  (func $rt_pair_fst (param $p i32) (result i32) (call $w (local.get $p) (i32.const 1)))
  (func $rt_pair_snd (param $p i32) (result i32) (call $w (local.get $p) (i32.const 2)))

  ;; the boxed unit singleton.
  (func $rt_unit (result i32) (global.get $UNIT))

  (func $rt_abort
    (call $puts (i32.const 1) (global.get $abort_msg) (global.get $abort_len))
    unreachable)

  ;; ---- naive base-1e9 bignum (builtin-nat); twin of the C/LLVM bignum ----
  ;; K_BIG=6 layout: [kind][nlimbs][limb0..]; little-endian, zero = empty.
  (func $big_alloc (param $nlimbs i32) (result i32)
    (local $o i32) (local $i i32)
    (local.set $o (call $alloc (i32.add (i32.const 8) (i32.shl (local.get $nlimbs) (i32.const 2)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 6))
    (call $sw (local.get $o) (i32.const 1) (local.get $nlimbs))
    ;; zero limb slots: free-list reuse may carry stale data; rt_nat_mul accumulates
    ;; r[i+j] += a[i]*b[j] and requires all limbs start at zero.
    (local.set $i (i32.const 0))
    (block $bz (loop $lz
      (br_if $bz (i32.ge_s (local.get $i) (local.get $nlimbs)))
      (call $big_setlimb (local.get $o) (local.get $i) (i32.const 0))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lz)))
    (local.get $o))
  (func $big_nlimbs (param $v i32) (result i32) (call $w (local.get $v) (i32.const 1)))
  (func $big_limb (param $v i32) (param $i i32) (result i32)
    (call $w (local.get $v) (i32.add (i32.const 2) (local.get $i))))
  (func $big_setlimb (param $v i32) (param $i i32) (param $x i32)
    (call $sw (local.get $v) (i32.add (i32.const 2) (local.get $i)) (local.get $x)))
  ;; normalize: drop trailing zero limbs in place (shrink nlimbs).
  (func $big_norm (param $v i32) (result i32)
    (local $n i32)
    (local.set $n (call $big_nlimbs (local.get $v)))
    (block $done
      (loop $l
        (br_if $done (i32.eqz (local.get $n)))
        (br_if $done (i32.ne (call $big_limb (local.get $v) (i32.sub (local.get $n) (i32.const 1))) (i32.const 0)))
        (local.set $n (i32.sub (local.get $n) (i32.const 1)))
        (br $l)))
    (call $sw (local.get $v) (i32.const 1) (local.get $n))
    (local.get $v))

  ;; build a bignum from a small i32 magnitude (n >= 0; only small here).
  (func $rt_big_from_long (param $n i32) (result i32)
    (local $v i32) (local $k i32) (local $t i32)
    (if (i32.le_s (local.get $n) (i32.const 0))
      (then (return (call $big_alloc (i32.const 0)))))
    (local.set $t (local.get $n)) (local.set $k (i32.const 0))
    (block $b (loop $l (br_if $b (i32.le_s (local.get $t) (i32.const 0)))
      (local.set $k (i32.add (local.get $k) (i32.const 1)))
      (local.set $t (i32.div_u (local.get $t) (i32.const 1000000000)))
      (br $l)))
    (local.set $v (call $big_alloc (local.get $k)))
    (local.set $t (local.get $n)) (local.set $k (i32.const 0))
    (block $b2 (loop $l2 (br_if $b2 (i32.le_s (local.get $t) (i32.const 0)))
      (call $big_setlimb (local.get $v) (local.get $k) (i32.rem_u (local.get $t) (i32.const 1000000000)))
      (local.set $t (i32.div_u (local.get $t) (i32.const 1000000000)))
      (local.set $k (i32.add (local.get $k) (i32.const 1)))
      (br $l2)))
    (local.get $v))

  ;; build a bignum from an i64 magnitude (n >= 0; base-1e9 LE limbs). The i32 twin
  ;; rt_big_from_long tops out at ~2e9; a clock reading (timeNanos, ~1e18 ns) needs the
  ;; full 64-bit range, so this extracts limbs by i64 divmod-by-1e9.
  (func $rt_big_from_i64 (param $n i64) (result i32)
    (local $v i32) (local $k i32) (local $t i64)
    (if (i64.le_s (local.get $n) (i64.const 0))
      (then (return (call $big_alloc (i32.const 0)))))
    (local.set $t (local.get $n)) (local.set $k (i32.const 0))
    (block $b (loop $l (br_if $b (i64.le_s (local.get $t) (i64.const 0)))
      (local.set $k (i32.add (local.get $k) (i32.const 1)))
      (local.set $t (i64.div_u (local.get $t) (i64.const 1000000000)))
      (br $l)))
    (local.set $v (call $big_alloc (local.get $k)))
    (local.set $t (local.get $n)) (local.set $k (i32.const 0))
    (block $b2 (loop $l2 (br_if $b2 (i64.le_s (local.get $t) (i64.const 0)))
      (call $big_setlimb (local.get $v) (local.get $k)
        (i32.wrap_i64 (i64.rem_u (local.get $t) (i64.const 1000000000))))
      (local.set $t (i64.div_u (local.get $t) (i64.const 1000000000)))
      (local.set $k (i32.add (local.get $k) (i32.const 1)))
      (br $l2)))
    (local.get $v))

  ;; big_add: c = a + b (base-1e9 ripple-carry).
  (func $big_add (param $a i32) (param $b i32) (result i32)
    (local $na i32) (local $nb i32) (local $n i32) (local $r i32)
    (local $i i32) (local $carry i32) (local $s i32)
    (local.set $na (call $big_nlimbs (local.get $a)))
    (local.set $nb (call $big_nlimbs (local.get $b)))
    (local.set $n (i32.add (select (local.get $na) (local.get $nb) (i32.gt_s (local.get $na) (local.get $nb))) (i32.const 1)))
    (local.set $r (call $big_alloc (local.get $n)))
    (local.set $i (i32.const 0)) (local.set $carry (i32.const 0))
    (block $b (loop $l (br_if $b (i32.ge_s (local.get $i) (local.get $n)))
      (local.set $s (local.get $carry))
      (if (i32.lt_s (local.get $i) (local.get $na))
        (then (local.set $s (i32.add (local.get $s) (call $big_limb (local.get $a) (local.get $i))))))
      (if (i32.lt_s (local.get $i) (local.get $nb))
        (then (local.set $s (i32.add (local.get $s) (call $big_limb (local.get $b) (local.get $i))))))
      (call $big_setlimb (local.get $r) (local.get $i) (i32.rem_u (local.get $s) (i32.const 1000000000)))
      (local.set $carry (i32.div_u (local.get $s) (i32.const 1000000000)))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $l)))
    (call $big_norm (local.get $r)))

  ;; big_cmp: -1/0/1 for a<b / a==b / a>b.
  (func $big_cmp (param $a i32) (param $b i32) (result i32)
    (local $na i32) (local $nb i32) (local $i i32) (local $x i32) (local $y i32)
    (local.set $na (call $big_nlimbs (local.get $a)))
    (local.set $nb (call $big_nlimbs (local.get $b)))
    (if (i32.ne (local.get $na) (local.get $nb))
      (then (return (select (i32.const -1) (i32.const 1) (i32.lt_s (local.get $na) (local.get $nb))))))
    (local.set $i (i32.sub (local.get $na) (i32.const 1)))
    (block $b (loop $l (br_if $b (i32.lt_s (local.get $i) (i32.const 0)))
      (local.set $x (call $big_limb (local.get $a) (local.get $i)))
      (local.set $y (call $big_limb (local.get $b) (local.get $i)))
      (if (i32.ne (local.get $x) (local.get $y))
        (then (return (select (i32.const -1) (i32.const 1) (i32.lt_u (local.get $x) (local.get $y))))))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (br $l)))
    (i32.const 0))

  (func $rt_big_succ (param $a i32) (result i32) (local $one i32) (local $r i32)
    ;; big_add reads both inputs and returns a fresh K_BIG; the "1" we allocate is
    ;; ours to own, so release it once big_add has consumed it. Without this, every
    ;; rt_big_succ (the per-iteration counter step in emitNatFold, and every builtin
    ;; succ application) leaks one K_BIG(1) per call.
    (local.set $one (call $rt_big_from_long (i32.const 1)))
    (local.set $r (call $big_add (local.get $a) (local.get $one)))
    (call $rt_release (local.get $one))
    (local.get $r))
  (func $rt_big_cmp (param $a i32) (param $b i32) (result i32)
    (call $big_cmp (local.get $a) (local.get $b)))
  (func $rt_nat_add (param $a i32) (param $b i32) (result i32)
    (call $big_add (local.get $a) (local.get $b)))

  ;; big_mul: schoolbook base-1e9 (i64 partials to avoid 32-bit overflow).
  (func $rt_nat_mul (param $a i32) (param $b i32) (result i32)
    (local $na i32) (local $nb i32) (local $r i32)
    (local $i i32) (local $j i32) (local $carry i64) (local $cur i64)
    (local.set $na (call $big_nlimbs (local.get $a)))
    (local.set $nb (call $big_nlimbs (local.get $b)))
    (if (i32.or (i32.eqz (local.get $na)) (i32.eqz (local.get $nb)))
      (then (return (call $big_alloc (i32.const 0)))))
    (local.set $r (call $big_alloc (i32.add (local.get $na) (local.get $nb))))
    (local.set $i (i32.const 0))
    (block $bi (loop $li (br_if $bi (i32.ge_s (local.get $i) (local.get $na)))
      (local.set $carry (i64.const 0)) (local.set $j (i32.const 0))
      (block $bj (loop $lj (br_if $bj (i32.ge_s (local.get $j) (local.get $nb)))
        (local.set $cur (i64.add (i64.add
          (i64.extend_i32_u (call $big_limb (local.get $r) (i32.add (local.get $i) (local.get $j))))
          (i64.mul (i64.extend_i32_u (call $big_limb (local.get $a) (local.get $i)))
                   (i64.extend_i32_u (call $big_limb (local.get $b) (local.get $j)))))
          (local.get $carry)))
        (call $big_setlimb (local.get $r) (i32.add (local.get $i) (local.get $j))
          (i32.wrap_i64 (i64.rem_u (local.get $cur) (i64.const 1000000000))))
        (local.set $carry (i64.div_u (local.get $cur) (i64.const 1000000000)))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $lj)))
      (call $big_setlimb (local.get $r) (i32.add (local.get $i) (local.get $nb))
        (i32.add (call $big_limb (local.get $r) (i32.add (local.get $i) (local.get $nb)))
                 (i32.wrap_i64 (local.get $carry))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $li)))
    (call $big_norm (local.get $r)))

  ;; big_monus: saturating a - b (0 floor).
  (func $rt_nat_monus (param $a i32) (param $b i32) (result i32)
    (local $na i32) (local $nb i32) (local $r i32) (local $i i32) (local $borrow i32) (local $d i32)
    (if (i32.le_s (call $big_cmp (local.get $a) (local.get $b)) (i32.const 0))
      (then (return (call $big_alloc (i32.const 0)))))
    (local.set $na (call $big_nlimbs (local.get $a)))
    (local.set $nb (call $big_nlimbs (local.get $b)))
    (local.set $r (call $big_alloc (local.get $na)))
    (local.set $i (i32.const 0)) (local.set $borrow (i32.const 0))
    (block $b (loop $l (br_if $b (i32.ge_s (local.get $i) (local.get $na)))
      (local.set $d (i32.sub (i32.sub (call $big_limb (local.get $a) (local.get $i)) (local.get $borrow))
        (select (call $big_limb (local.get $b) (local.get $i)) (i32.const 0) (i32.lt_s (local.get $i) (local.get $nb)))))
      (if (i32.lt_s (local.get $d) (i32.const 0))
        (then (local.set $d (i32.add (local.get $d) (i32.const 1000000000))) (local.set $borrow (i32.const 1)))
        (else (local.set $borrow (i32.const 0))))
      (call $big_setlimb (local.get $r) (local.get $i) (local.get $d))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $l)))
    (call $big_norm (local.get $r)))

  ;; rt_big_parse: parse a NUL-terminated decimal cstr at $p into a bignum.
  ;; Builds via repeated acc = acc*10 + digit using rt_nat_mul / big_add (naive,
  ;; runs once per literal). Each iteration's per-digit temporaries are OWNED and
  ;; released before the next iteration: $old (the previous acc), $mid (acc*10),
  ;; and $dig (the digit K_BIG) are freed after big_add returns the new acc.
  ;; $ten is released after the loop. The returned $acc is owned (rc=1).
  ;;
  ;; rt_nat_mul and big_add borrow their inputs (they read without retaining), so
  ;; releasing $old/$mid/$dig after big_add is safe -- all reads are complete.
  (func $rt_big_parse (param $p i32) (result i32)
    (local $acc i32) (local $ten i32) (local $c i32)
    (local $old i32) (local $mid i32) (local $dig i32)
    (local.set $acc (call $big_alloc (i32.const 0)))
    (local.set $ten (call $rt_big_from_long (i32.const 10)))
    (block $b (loop $l
      (local.set $c (i32.load8_u (local.get $p)))
      (br_if $b (i32.eqz (local.get $c)))
      (local.set $old (local.get $acc))
      (local.set $mid (call $rt_nat_mul (local.get $old) (local.get $ten)))
      (local.set $dig (call $rt_big_from_long (i32.sub (local.get $c) (i32.const 48))))
      (local.set $acc (call $big_add (local.get $mid) (local.get $dig)))
      (call $rt_release (local.get $old))
      (call $rt_release (local.get $mid))
      (call $rt_release (local.get $dig))
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (br $l)))
    (call $rt_release (local.get $ten))
    (local.get $acc))

  ;; ---- $show: render a value to the scratch buffer, then fd_write to stdout ----
  ;; The scratch buffer grows downward in the [0,65536) reserved region; we use a
  ;; cursor and append bytes, then emit it. Renders byte-identical to the C show.
  (global $sbuf (mut i32) (i32.const 4096))   ;; show cursor (scratch starts at 4096)
  (func $emit_byte (param $b i32)
    (i32.store8 (global.get $sbuf) (local.get $b))
    (global.set $sbuf (i32.add (global.get $sbuf) (i32.const 1))))
  (func $emit_cstr (param $p i32)
    (local $c i32)
    (block $b (loop $l
      (local.set $c (i32.load8_u (local.get $p)))
      (br_if $b (i32.eqz (local.get $c)))
      (call $emit_byte (local.get $c))
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (br $l))))
  ;; emit a base-10 unsigned i32 (for limbs).
  (func $emit_u32 (param $n i32)
    (local $q i32)
    (if (i32.ge_u (local.get $n) (i32.const 10))
      (then
        (local.set $q (i32.div_u (local.get $n) (i32.const 10)))
        (call $emit_u32 (local.get $q))
        (local.set $n (i32.sub (local.get $n) (i32.mul (local.get $q) (i32.const 10))))))
    (call $emit_byte (i32.add (local.get $n) (i32.const 48))))
  ;; emit an i32 limb zero-padded to 9 digits (for non-top bignum limbs).
  (func $emit_u32_pad9 (param $n i32)
    (local $d i32) (local $div i32)
    (local.set $div (i32.const 100000000))
    (block $b (loop $l (br_if $b (i32.eqz (local.get $div)))
      (local.set $d (i32.rem_u (i32.div_u (local.get $n) (local.get $div)) (i32.const 10)))
      (call $emit_byte (i32.add (local.get $d) (i32.const 48)))
      (local.set $div (i32.div_u (local.get $div) (i32.const 10)))
      (br $l))))
  (func $emit_big (param $v i32)
    (local $n i32) (local $i i32)
    (local.set $n (call $big_nlimbs (local.get $v)))
    (if (i32.eqz (local.get $n)) (then (call $emit_byte (i32.const 48)) (return)))
    (call $emit_u32 (call $big_limb (local.get $v) (i32.sub (local.get $n) (i32.const 1))))
    (local.set $i (i32.sub (local.get $n) (i32.const 2)))
    (block $b (loop $l (br_if $b (i32.lt_s (local.get $i) (i32.const 0)))
      (call $emit_u32_pad9 (call $big_limb (local.get $v) (local.get $i)))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (br $l))))

  ;; show_paren: parenthesize a constructor with >=1 non-unit field.
  (func $show_paren (param $v i32)
    (local $o i32) (local $i i32) (local $nf i32) (local $shown i32)
    (if (i32.eqz (call $is_int (local.get $v)))
      (then
        (if (i32.eq (call $w (local.get $v) (i32.const 0)) (i32.const 1))
          (then
            (local.set $nf (call $rt_con_nfield (local.get $v)))
            (local.set $shown (i32.const 0)) (local.set $i (i32.const 0))
            (block $b (loop $l (br_if $b (i32.ge_s (local.get $i) (local.get $nf)))
              (if (i32.ne (call $rt_con_get (local.get $v) (local.get $i)) (global.get $UNIT))
                (then (local.set $shown (i32.add (local.get $shown) (i32.const 1)))))
              (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $l)))
            (if (i32.gt_s (local.get $shown) (i32.const 0))
              (then
                (call $emit_byte (i32.const 40))   ;; '('
                (call $show (local.get $v))
                (call $emit_byte (i32.const 41))   ;; ')'
                (return)))))))
    (call $show (local.get $v)))

  (func $show (param $v i32)
    (local $o i32) (local $i i32) (local $nf i32)
    (if (i32.eq (local.get $v) (global.get $UNIT))
      (then (call $emit_byte (i32.const 40)) (call $emit_byte (i32.const 41)) (return)))  ;; "()"
    (if (call $is_int (local.get $v))
      (then (call $emit_u32 (i32.shr_s (local.get $v) (i32.const 1))) (return)))
    (block $sw
      ;; K_BIG = 6
      (if (i32.eq (call $w (local.get $v) (i32.const 0)) (i32.const 6))
        (then (call $emit_big (local.get $v)) (return)))
      ;; K_CLO = 0
      (if (i32.eq (call $w (local.get $v) (i32.const 0)) (i32.const 0))
        (then (call $emit_cstr (global.get $fn_msg)) (return)))
      ;; K_PAIR = 2
      (if (i32.eq (call $w (local.get $v) (i32.const 0)) (i32.const 2))
        (then
          (call $emit_byte (i32.const 40))
          (call $show (call $rt_pair_fst (local.get $v)))
          (call $emit_byte (i32.const 44)) (call $emit_byte (i32.const 32))  ;; ", "
          (call $show (call $rt_pair_snd (local.get $v)))
          (call $emit_byte (i32.const 41))
          (return)))
      ;; K_CON = 1
      (if (i32.eq (call $w (local.get $v) (i32.const 0)) (i32.const 1))
        (then
          (call $emit_cstr (call $w (local.get $v) (i32.const 2)))   ;; name_ptr
          (local.set $nf (call $rt_con_nfield (local.get $v)))
          (local.set $i (i32.const 0))
          (block $b (loop $l (br_if $b (i32.ge_s (local.get $i) (local.get $nf)))
            (if (i32.ne (call $rt_con_get (local.get $v) (local.get $i)) (global.get $UNIT))
              (then
                (call $emit_byte (i32.const 32))   ;; ' '
                (call $show_paren (call $rt_con_get (local.get $v) (local.get $i)))))
            (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $l)))
          (return)))
      ;; default: "()"
      (call $emit_byte (i32.const 40)) (call $emit_byte (i32.const 41))))

  ;; rt_show_line: render v, append a newline, fd_write the buffer to stdout.
  (func $rt_show_line (param $v i32)
    (local $len i32)
    (global.set $sbuf (i32.const 4096))
    (call $show (local.get $v))
    (call $emit_byte (i32.const 10))   ;; '\n'
    (local.set $len (i32.sub (global.get $sbuf) (i32.const 4096)))
    (call $puts (i32.const 1) (i32.const 4096) (local.get $len)))

  ;; puts: write [ptr,len) to file descriptor fd via fd_write. Uses an iovec at
  ;; address 16 (in the reserved low region, below the show buffer at 4096).
  (func $puts (param $fd i32) (param $ptr i32) (param $len i32)
    (i32.store (i32.const 16) (local.get $ptr))
    (i32.store (i32.const 20) (local.get $len))
    (drop (call $fd_write (local.get $fd) (i32.const 16) (i32.const 1) (i32.const 24))))

  ;; ARC live-block count (alloc bumps, free drops): the leak/double-free probe.
  (func $rt_live (result i32) (global.get $live))

  ;; $rt_bucket_addr: return the $freelist bucket address for a rounded payload size, or 0
  ;; if the size is out of range (index >= 64, i.e. size >= 256 bytes). The bucket holds
  ;; the payload pointer of the first free block of that exact size (intrusive linked list:
  ;; the link to the next block of the same size is stored at the freed payload's word 0).
  (func $rt_bucket_addr (param $size i32) (result i32)
    (local $idx i32)
    (local.set $idx (i32.shr_u (local.get $size) (i32.const 2)))
    (if (result i32) (i32.lt_u (local.get $idx) (i32.const 64))
      (then (i32.add (global.get $freelist) (i32.shl (local.get $idx) (i32.const 2))))
      (else (i32.const 0))))

  ;; $rt_hp: current heap pointer (for the free-list reuse test: hp must not advance under
  ;; same-size alloc/free churn once the free list is seeded).
  (func $rt_hp (result i32) (global.get $hp))

  ;; rt_counted: true iff v participates in refcounting -- not an immediate (low bit
  ;; set), not null, and not the immortal UNIT singleton.
  (func $rt_counted (param $v i32) (result i32)
    (i32.and
      (i32.and
        (i32.eqz (i32.and (local.get $v) (i32.const 1)))   ;; low bit clear => pointer
        (i32.ne (local.get $v) (i32.const 0)))             ;; non-null
      (i32.ne (local.get $v) (global.get $UNIT))))         ;; not the immortal unit

  ;; rt_retain: a no-op on immediates (low bit set) and the immortal UNIT; otherwise
  ;; increment the refcount at [v-4].
  (func $rt_retain (param $v i32)
    (if (call $rt_counted (local.get $v))
      (then
        (i32.store (i32.sub (local.get $v) (i32.const 4))
          (i32.add (i32.load (i32.sub (local.get $v) (i32.const 4))) (i32.const 1))))))

  ;; rt_release: a no-op on immediates and UNIT; otherwise decrement the refcount; at
  ;; zero, free the block (Task 3 adds child recursion before the free).
  (func $rt_release (param $v i32)
    (local $rc i32)
    (if (call $rt_counted (local.get $v))
      (then
        (local.set $rc (i32.sub (i32.load (i32.sub (local.get $v) (i32.const 4))) (i32.const 1)))
        (i32.store (i32.sub (local.get $v) (i32.const 4)) (local.get $rc))
        (if (i32.eqz (local.get $rc))
          (then (call $rt_free (local.get $v)))))))

  ;; rt_free: release the object's child pointers by kind (so the whole immutable
  ;; structure is reclaimed), then drop the live count. The value graph is acyclic
  ;; (immutable functional data), so this terminates.
  ;; Kinds: K_CLO=0 env slots at word 3 (count at word 2);
  ;;        K_CON=1 field slots at word 4 (count at word 3);
  ;;        K_PAIR=2 halves at words 1 and 2;
  ;;        K_BIG=6 and others: leaf -- limbs are raw ints, not pointers.
  ;; $rt_free releases child pointers by kind (so the whole structure is reclaimed),
  ;; decrements $live, then pushes the block onto its size-class free list for reuse.
  (func $rt_free (param $v i32)
    (local $kind i32) (local $n i32) (local $i i32) (local $base i32)
    (local $size i32) (local $bkt i32)
    (local.set $kind (call $w (local.get $v) (i32.const 0)))
    (block $done
      ;; K_CLO=0: env slots start at word 3, count at word 2
      (if (i32.eqz (local.get $kind))
        (then
          (local.set $n (call $w (local.get $v) (i32.const 2)))
          (local.set $base (i32.const 3))
          (br $done)))
      ;; K_CON=1: field slots start at word 4, count at word 3
      (if (i32.eq (local.get $kind) (i32.const 1))
        (then
          (local.set $n (call $w (local.get $v) (i32.const 3)))
          (local.set $base (i32.const 4))
          (br $done)))
      ;; K_PAIR=2: two child pointers at words 1 and 2
      (if (i32.eq (local.get $kind) (i32.const 2))
        (then
          (call $rt_release (call $w (local.get $v) (i32.const 1)))
          (call $rt_release (call $w (local.get $v) (i32.const 2)))
          (local.set $n (i32.const 0))
          (local.set $base (i32.const 0))
          (br $done)))
      ;; K_BOUNCE=7: arg slots start at word 3, count (nargs) at word 2. The step at
      ;; word 1 is the borrowed cached _step closure -- not owned, not released here.
      (if (i32.eq (local.get $kind) (i32.const 7))
        (then
          (local.set $n (call $w (local.get $v) (i32.const 2)))
          (local.set $base (i32.const 3))
          (br $done)))
      ;; K_BIG and anything else: leaf, no child pointers
      (local.set $n (i32.const 0))
      (local.set $base (i32.const 0)))
    ;; release the $n child slots starting at word $base (closures + constructors)
    (local.set $i (i32.const 0))
    (block $brk (loop $lp
      (br_if $brk (i32.ge_u (local.get $i) (local.get $n)))
      (call $rt_release (call $w (local.get $v) (i32.add (local.get $base) (local.get $i))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    (global.set $live (i32.sub (global.get $live) (i32.const 1)))
    ;; push onto the size-classed free list (if poolable: rounded size < 256 bytes)
    (local.set $size (i32.load (i32.sub (local.get $v) (i32.const 8))))
    (local.set $bkt (call $rt_bucket_addr (local.get $size)))
    (if (local.get $bkt)
      (then
        (i32.store (local.get $v) (i32.load (local.get $bkt)))  ;; link := old head
        (i32.store (local.get $bkt) (local.get $v))))           ;; head := this payload
  )

  ;; print an unsigned i32 in decimal to stdout (reuses the show buffer + fd_write).
  (func $rt_print_u32 (param $n i32)
    (global.set $sbuf (i32.const 4096))
    (call $emit_u32 (local.get $n))
    (call $puts (i32.const 1) (i32.const 4096)
      (i32.sub (global.get $sbuf) (i32.const 4096))))

  ;; ---- T2 trampoline: [K_BOUNCE=7][step][nargs][arg0..] ----
  ;; A partial's saturated tail call lowers to a K_BOUNCE: the cached _step closure
  ;; (slot at word 1, BORROWED) plus nargs eagerly-evaluated OWNED args (words 3..).
  (func $rt_mkbounce (param $step i32) (param $nargs i32) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.add (i32.const 12) (i32.shl (local.get $nargs) (i32.const 2)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 7))      ;; K_BOUNCE
    (call $sw (local.get $o) (i32.const 1) (local.get $step))
    (call $sw (local.get $o) (i32.const 2) (local.get $nargs))
    (local.get $o))
  (func $rt_bounce_set (param $b i32) (param $i i32) (param $x i32)
    (call $sw (local.get $b) (i32.add (i32.const 3) (local.get $i)) (local.get $x)))

  ;; shell-free: reclaim the bounce object WITHOUT releasing the args (moved into the
  ;; step call) or the step (borrowed). Mirrors the live--/freelist tail of $rt_free.
  (func $rt_bounce_free_shell (param $b i32)
    (local $size i32) (local $bkt i32)
    (global.set $live (i32.sub (global.get $live) (i32.const 1)))
    (local.set $size (i32.load (i32.sub (local.get $b) (i32.const 8))))
    (local.set $bkt (call $rt_bucket_addr (local.get $size)))
    (if (local.get $bkt)
      (then
        (i32.store (local.get $b) (i32.load (local.get $bkt)))
        (i32.store (local.get $bkt) (local.get $b)))))

  ;; tramp: force the bounce chain. While v is a K_BOUNCE, re-apply its step to its
  ;; args (one partial-body iteration -> the next bounce or a value), shell-free the
  ;; spent bounce (args MOVED into the applies), and continue. O(1) WASM stack.
  (func $rt_tramp (param $v i32) (result i32)
    (local $step i32) (local $nargs i32) (local $i i32) (local $f i32) (local $next i32)
    (block $done (loop $lp
      ;; immediates are never bounces; a heap value is a bounce iff word0 == 7.
      (br_if $done (call $is_int (local.get $v)))
      (br_if $done (i32.ne (call $w (local.get $v) (i32.const 0)) (i32.const 7)))
      (local.set $step (call $w (local.get $v) (i32.const 1)))
      (local.set $nargs (call $w (local.get $v) (i32.const 2)))
      ;; Apply the BORROWED step to each owned arg. Each apply but the last builds an
      ;; owned intermediate closure (it captures the arg); release it before the next
      ;; apply. The arg MOVES into that closure (or is consumed by the body on the last
      ;; apply), so shell-free below does not touch the slots. f starts as the borrowed
      ;; step (i==0), so it is NOT released; intermediates (i>0) are.
      (local.set $f (local.get $step))
      (local.set $i (i32.const 0))
      (block $ab (loop $al
        (br_if $ab (i32.ge_u (local.get $i) (local.get $nargs)))
        (local.set $next (call $rt_apply (local.get $f)
          (call $w (local.get $v) (i32.add (i32.const 3) (local.get $i)))))
        (if (local.get $i) (then (call $rt_release (local.get $f))))
        (local.set $f (local.get $next))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $al)))
      (call $rt_bounce_free_shell (local.get $v))
      (local.set $v (local.get $f))
      (br $lp)))
    (local.get $v))
`

// WasmRuntime returns the WAT runtime string for the WASM backend. The underlying
// const is unexported so the runtime cannot be modified from outside codegen; this
// accessor gives the external test package read-only access.
func WasmRuntime() string { return wasmRuntime }

// wasmBibleCodec is the packed-String CODEC for the bible/D6 host-op family: the
// base-256, leading-1-sentinel encoding shared byte-for-byte with the C (codegen/c.go)
// and Rust (codegen/rust.go) _s2h/_h2s, over the existing base-1e9 bignum. A Rune
// String is a K_BIG exactly as on C/LLVM, so byte-exactness is free -- the codec is
// bignum <-> raw bytes with no charset/UTF-8 transform (Greek/Hebrew pass through raw).
//
// It is emitted (via emitBibleCodecWasm) only when the program uses a codec-consuming
// foreign, gated by usesBibleCodec. The three byte scratch windows $D6BUF/$D6BUF2/
// $D6BUF3 (64 KiB each) live below the bumped $hp (262144) so they never collide with
// the heap, the show buffer, the freelist, the iovec, or the interned cstrs.
const wasmBibleCodec = `
  ;; ---- packed-String codec scratch windows (64 KiB each, below the heap at 262144) ----
  (global $D6BUF  i32 (i32.const 65536))
  (global $D6BUF2 i32 (i32.const 131072))
  (global $D6BUF3 i32 (i32.const 196608))

  ;; $big_divmod_small: divide K_BIG $v by a small i32 $d; returns (quotient K_BIG,
  ;; remainder i32) -- classic base-1e9 long division, most-significant limb first,
  ;; carrying the running remainder as an i64. Twin of the C big_divmod / Rust
  ;; _big_divmod_small. RESULT ORDER: (quotient, remainder) -- big_norm(q) is pushed
  ;; first (the deeper/first result), the remainder second (top). $big_alloc zero-inits
  ;; limbs, so unset high limbs read as 0.
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
      (call $big_setlimb (local.get $q) (local.get $i)
        (i32.wrap_i64 (i64.div_u (local.get $cur) (local.get $dd))))
      (local.set $rem (i64.rem_u (local.get $cur) (local.get $dd)))
      (br $lp)))
    (call $big_norm (local.get $q))        ;; result 1: quotient (normalized)
    (i32.wrap_i64 (local.get $rem)))       ;; result 2: remainder byte

  ;; $d6_memeq: byte-compare [a,a+n) == [b,b+n)? (memcmp==0). 1 = equal.
  (func $d6_memeq (param $a i32) (param $b i32) (param $n i32) (result i32)
    (local $i i32)
    (local.set $i (i32.const 0))
    (block $done (loop $lp
      (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
      (if (i32.ne (i32.load8_u (i32.add (local.get $a) (local.get $i)))
                  (i32.load8_u (i32.add (local.get $b) (local.get $i))))
        (then (return (i32.const 0))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    (i32.const 1))

  ;; $d6_s2h_to: decode packed-String bignum $code -> raw bytes at $dst; returns length.
  ;; while big_cmp(b,1) > 0: (q, rem) = divmod_small(b, 256); dst[n++] = rem; b = q.
  ;; ARC: each divmod allocates a fresh quotient; release the previous $b (never $code,
  ;; which is borrowed) before advancing, release the final $b (an owned quotient) and
  ;; the intermediate $one.
  (func $d6_s2h_to (param $code i32) (param $dst i32) (result i32)
    (local $b i32) (local $n i32) (local $rem i32) (local $one i32) (local $q i32)
    (local.set $b (local.get $code))
    (local.set $one (call $rt_big_from_long (i32.const 1)))
    (local.set $n (i32.const 0))
    (block $done (loop $lp
      (br_if $done (i32.le_s (call $big_cmp (local.get $b) (local.get $one)) (i32.const 0)))
      (call $big_divmod_small (local.get $b) (i32.const 256))  ;; -> (q, rem); rem on top
      (local.set $rem)                                         ;; pop rem (top)
      (local.set $q)                                           ;; pop q (deeper)
      (i32.store8 (i32.add (local.get $dst) (local.get $n)) (local.get $rem))
      (local.set $n (i32.add (local.get $n) (i32.const 1)))
      (if (i32.ne (local.get $b) (local.get $code))
        (then (call $rt_release (local.get $b))))
      (local.set $b (local.get $q))
      (br $lp)))
    (if (i32.ne (local.get $b) (local.get $code))
      (then (call $rt_release (local.get $b))))
    (call $rt_release (local.get $one))
    (local.get $n))

  ;; $d6_s2h: decode $code into the primary window $D6BUF; returns (buf_ptr, len).
  (func $d6_s2h (param $code i32) (result i32 i32)
    (global.get $D6BUF)
    (call $d6_s2h_to (local.get $code) (global.get $D6BUF)))

  ;; $d6_h2s: fold raw bytes [$buf, $buf+$len) -> a packed-String bignum. n = 1; for
  ;; i = len-1..0: n = n*256 + buf[i]. Empty input -> the bignum 1 (the empty-String
  ;; sentinel). ARC: release each iteration's product / digit / prior accumulator; the
  ;; returned $n is owned by the caller.
  (func $d6_h2s (param $buf i32) (param $len i32) (result i32)
    (local $n i32) (local $m i32) (local $i i32) (local $byte i32)
    (local $prod i32) (local $dig i32) (local $t i32)
    (local.set $n (call $rt_big_from_long (i32.const 1)))
    (local.set $m (call $rt_big_from_long (i32.const 256)))
    (local.set $i (local.get $len))
    (block $done (loop $lp
      (br_if $done (i32.eqz (local.get $i)))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (local.set $byte (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
      (local.set $prod (call $rt_nat_mul (local.get $n) (local.get $m)))
      (local.set $dig (call $rt_big_from_long (local.get $byte)))
      (local.set $t (call $big_add (local.get $prod) (local.get $dig)))
      (call $rt_release (local.get $prod))
      (call $rt_release (local.get $dig))
      (call $rt_release (local.get $n))
      (local.set $n (local.get $t))
      (br $lp)))
    (call $rt_release (local.get $m))
    (local.get $n))
`

// WasmBibleCodec returns the WAT packed-String codec ($big_divmod_small + $d6_s2h/
// $d6_h2s + the scratch-window globals). Exported for the external test package (a
// divmod-order probe assembles it against WasmRuntime()).
func WasmBibleCodec() string { return wasmBibleCodec }

// wasmBibleReadFile is the WASI FILE-READ helper for the bible/D6 host-op family: open a
// preopen-relative path READ-only, slurp its whole contents into a scratch window, and
// return (buf_ptr, len) -- or (0,0) on any errno (the caller treats (0,0) as "missing",
// mirroring the C `if (!fp) return s0`). It is the shared primitive foldLines/foldDir
// (Task 3) and readFileCode (Task 5) build on.
//
// SYSCALL CELLS ($D6SYS, a 64 KiB block whose first 32 bytes hold the cells): the fd_read
// iovec at [+0,+8) = [buf_ptr][buf_len], the path_open opened-fd out at [+8,+12), and the
// fd_read nread out at [+24,+28). The whole file lands in $D6SLURP (1 MiB). These windows
// sit ABOVE the codec windows ($D6BUF/2/3, which end at 262144) and BELOW the bumped $hp
// (1572864), so they never collide with the heap, the codec byte buffers, the show
// buffer, the freelist, the iovec, or the interned cstrs.
//
// WASI preview1 contract used here: path_open(fd=3 the first --dir preopen, dirflags=1
// SYMLINK_FOLLOW, path_ptr, path_len, oflags=0, fs_rights_base=FD_READ|FD_SEEK=0x6,
// fs_rights_inheriting=0, fdflags=0, opened_fd_out); fd_read(fd, iovs_ptr, iovs_len=1,
// nread_out). The path bytes are the raw ASCII the caller decodes via $d6_s2h; path_open
// reads them immediately, so the caller's decode window is free to reuse afterward.
const wasmBibleReadFile = `
  ;; ---- Task-3 file/dir WASI scratch: syscall cells + whole-file slurp window ----
  (global $D6SYS   i32 (i32.const 262144))  ;; syscall cells (iovec/fd-out/nread), 64 KiB block
  (global $D6SLURP i32 (i32.const 524288))  ;; whole-file slurp buffer (1 MiB, cap below)

  ;; $d6_readfile: open $pbuf[0,$plen) READ-only under preopen fd 3, stream its bytes into
  ;; $D6SLURP, close, and return (buf_ptr, len). Returns (0,0) on a path_open OR fd_read
  ;; errno (missing/unreadable). The read loops until EOF (nread==0) so a short read is
  ;; handled; a file exceeding the 1 MiB window is truncated at the cap (documented).
  (func $d6_readfile (param $pbuf i32) (param $plen i32) (result i32 i32)
    (local $fd i32) (local $n i32) (local $tot i32) (local $rc i32) (local $sys i32) (local $slurp i32)
    (local.set $sys (global.get $D6SYS))
    (local.set $slurp (global.get $D6SLURP))
    ;; path_open(3, SYMLINK_FOLLOW=1, pbuf, plen, oflags=0, rights=FD_READ|FD_SEEK=0x6,
    ;;   inheriting=0, fdflags=0, fd_out=$sys+8)
    (local.set $rc (call $path_open
      (i32.const 3) (i32.const 1)
      (local.get $pbuf) (local.get $plen)
      (i32.const 0)
      (i64.const 6) (i64.const 0)
      (i32.const 0)
      (i32.add (local.get $sys) (i32.const 8))))
    (if (local.get $rc) (then (return (i32.const 0) (i32.const 0))))
    (local.set $fd (i32.load (i32.add (local.get $sys) (i32.const 8))))
    (local.set $tot (i32.const 0))
    (block $done (loop $lp
      ;; iovec at $sys: buf = slurp+tot, len = remaining cap
      (i32.store (local.get $sys) (i32.add (local.get $slurp) (local.get $tot)))
      (i32.store (i32.add (local.get $sys) (i32.const 4))
        (i32.sub (i32.const 1048576) (local.get $tot)))
      (local.set $rc (call $fd_read (local.get $fd)
        (local.get $sys) (i32.const 1) (i32.add (local.get $sys) (i32.const 24))))
      (if (local.get $rc)
        (then (drop (call $fd_close (local.get $fd))) (return (i32.const 0) (i32.const 0))))
      (local.set $n (i32.load (i32.add (local.get $sys) (i32.const 24))))
      (br_if $done (i32.eqz (local.get $n)))                          ;; EOF
      (local.set $tot (i32.add (local.get $tot) (local.get $n)))
      (br_if $done (i32.ge_u (local.get $tot) (i32.const 1048576)))   ;; cap reached
      (br $lp)))
    (drop (call $fd_close (local.get $fd)))
    (local.get $slurp) (local.get $tot))
`

// WasmBibleReadFile returns the WAT file-read helper ($d6_readfile + its scratch globals).
// Exported for the external test package's isolation harness (opens a known fixture under
// `wasmtime run --dir=.` and checks the byte count matches `wc -c`).
func WasmBibleReadFile() string { return wasmBibleReadFile }

// wasmBibleFoldDir is the recursive directory-walk helper for foldDir: it opens a dir via
// path_open(O_DIRECTORY), reads its entries via fd_readdir, copies each kept child's FULL
// path (dir + "/" + name) into a bump region, bytewise-sorts them (== sorting by filename,
// since every sibling shares the same dir prefix -- matches Go filepath.WalkDir per-dir
// lexical order), then walks them depth-first PRE-order: a subdir recurses, a suffix-
// matching file is slurped ($d6_readfile) and folded through the erased step. Skips
// "."/"..". Depends on wasmBibleReadFile ($d6_readfile/$D6SYS) and the codec ($d6_h2s).
//
// RECURSION SAFETY: fd_readdir writes into the single $D6DIR window, so the dir fd is
// CLOSED and every kept name is copied out (into the $D6PATH bump) BEFORE any recursion;
// a child's fd_readdir then reuses $D6DIR freely. Both bumps ($d6_path_sp path bytes,
// $d6_idx_sp the (off,len,type) index) are saved on entry and restored on exit, so a
// child's entries never clobber a parent's pending ones. WASI filetype 3 = directory.
//
// $d6_foldwalk(dirPtr, dirLen, sfxPtr, sfxLen, step, s) -> s. `step` is BORROWED (owned by
// foldDir's env); `s` is threaded by ownership (consumed by each apply, the fresh result
// owned) exactly as foldLines folds -- the incoming s is owned, the returned s is owned.
const wasmBibleFoldDir = `
  ;; ---- foldDir dir-walk scratch (each 64 KiB, below $D6SLURP) ----
  (global $D6PATH  i32 (i32.const 327680))  ;; full-path build bump (path bytes)
  (global $D6DIR   i32 (i32.const 393216))  ;; fd_readdir dirent buffer
  (global $D6NAMES i32 (i32.const 458752))  ;; foldwalk index: (pathOff,pathLen,type) x12B
  (global $d6_path_sp (mut i32) (i32.const 327680))  ;; $D6PATH bump cursor
  (global $d6_idx_sp  (mut i32) (i32.const 458752))  ;; $D6NAMES bump cursor

  ;; $d6_pathcmp: bytewise compare [a,a+na) vs [b,b+nb); -1/0/1 (memcmp then length).
  (func $d6_pathcmp (param $a i32) (param $na i32) (param $b i32) (param $nb i32) (result i32)
    (local $i i32) (local $m i32) (local $x i32) (local $y i32)
    (local.set $m (select (local.get $na) (local.get $nb) (i32.lt_s (local.get $na) (local.get $nb))))
    (local.set $i (i32.const 0))
    (block $done (loop $lp
      (br_if $done (i32.ge_s (local.get $i) (local.get $m)))
      (local.set $x (i32.load8_u (i32.add (local.get $a) (local.get $i))))
      (local.set $y (i32.load8_u (i32.add (local.get $b) (local.get $i))))
      (if (i32.ne (local.get $x) (local.get $y))
        (then (return (select (i32.const -1) (i32.const 1) (i32.lt_u (local.get $x) (local.get $y))))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $lp)))
    (if (i32.ne (local.get $na) (local.get $nb))
      (then (return (select (i32.const -1) (i32.const 1) (i32.lt_s (local.get $na) (local.get $nb))))))
    (i32.const 0))

  (func $d6_foldwalk (param $dirPtr i32) (param $dirLen i32)
                     (param $sfxPtr i32) (param $sfxLen i32)
                     (param $step i32) (param $s i32) (result i32)
    (local $savePath i32) (local $saveIdx i32) (local $fd i32) (local $rc i32)
    (local $cookie i64) (local $bufused i32) (local $off i32) (local $count i32)
    (local $namlen i32) (local $dtype i32) (local $nameOff i32) (local $cp i32) (local $childLen i32)
    (local $i i32) (local $j i32) (local $ei i32) (local $ej i32)
    (local $pOff i32) (local $pLen i32) (local $pType i32)
    (local $tOff i32) (local $tLen i32) (local $tType i32)
    (local $sys i32) (local $dir i32) (local $names i32)
    (local $buf i32) (local $len i32) (local $content i32) (local $ca i32) (local $cb i32) (local $k i32)
    (local.set $sys (global.get $D6SYS))
    (local.set $dir (global.get $D6DIR))
    (local.set $names (global.get $D6NAMES))
    (local.set $savePath (global.get $d6_path_sp))
    (local.set $saveIdx (global.get $d6_idx_sp))
    ;; open dir: path_open(3, 1, dirPtr, dirLen, oflags=O_DIRECTORY=2, rights=FD_READDIR=0x4000,
    ;;   inheriting=0, fdflags=0, fd_out=$sys+8)
    (local.set $rc (call $path_open
      (i32.const 3) (i32.const 1)
      (local.get $dirPtr) (local.get $dirLen)
      (i32.const 2)
      (i64.const 16384) (i64.const 0)
      (i32.const 0)
      (i32.add (local.get $sys) (i32.const 8))))
    (if (local.get $rc) (then (return (local.get $s))))
    (local.set $fd (i32.load (i32.add (local.get $sys) (i32.const 8))))
    ;; readdir: collect kept entries' full paths + index. cookie starts 0.
    (local.set $cookie (i64.const 0))
    (local.set $count (i32.const 0))
    (block $rddone (loop $rdlp
      (local.set $rc (call $fd_readdir (local.get $fd) (local.get $dir) (i32.const 65536)
        (local.get $cookie) (i32.add (local.get $sys) (i32.const 28))))
      (if (local.get $rc) (then (br $rddone)))
      (local.set $bufused (i32.load (i32.add (local.get $sys) (i32.const 28))))
      (br_if $rddone (i32.eqz (local.get $bufused)))
      (local.set $off (i32.const 0))
      (block $eb (loop $el
        ;; need a full 24-byte header
        (br_if $eb (i32.gt_u (i32.add (local.get $off) (i32.const 24)) (local.get $bufused)))
        (local.set $cookie (i64.load (i32.add (local.get $dir) (local.get $off))))       ;; d_next
        (local.set $namlen (i32.load (i32.add (local.get $dir) (i32.add (local.get $off) (i32.const 16)))))
        (local.set $dtype (i32.load8_u (i32.add (local.get $dir) (i32.add (local.get $off) (i32.const 20)))))
        (local.set $nameOff (i32.add (local.get $off) (i32.const 24)))
        ;; the name must be fully present in this buffer; else re-read from this cookie
        (br_if $eb (i32.gt_u (i32.add (local.get $nameOff) (local.get $namlen)) (local.get $bufused)))
        ;; skip "." and ".."
        (if (i32.eqz (i32.and
              (i32.eq (local.get $namlen) (i32.const 1))
              (i32.eq (i32.load8_u (i32.add (local.get $dir) (local.get $nameOff))) (i32.const 46))))
          (then
            (if (i32.eqz (i32.and (i32.and
                  (i32.eq (local.get $namlen) (i32.const 2))
                  (i32.eq (i32.load8_u (i32.add (local.get $dir) (local.get $nameOff))) (i32.const 46)))
                  (i32.eq (i32.load8_u (i32.add (local.get $dir) (i32.add (local.get $nameOff) (i32.const 1)))) (i32.const 46))))
              (then
                ;; build full child path = dir + "/" + name at $d6_path_sp
                (local.set $cp (global.get $d6_path_sp))
                (local.set $k (i32.const 0))
                (block $cb1 (loop $cl1
                  (br_if $cb1 (i32.ge_s (local.get $k) (local.get $dirLen)))
                  (i32.store8 (i32.add (local.get $cp) (local.get $k))
                    (i32.load8_u (i32.add (local.get $dirPtr) (local.get $k))))
                  (local.set $k (i32.add (local.get $k) (i32.const 1)))
                  (br $cl1)))
                (i32.store8 (i32.add (local.get $cp) (local.get $dirLen)) (i32.const 47)) ;; '/'
                (local.set $k (i32.const 0))
                (block $cb2 (loop $cl2
                  (br_if $cb2 (i32.ge_s (local.get $k) (local.get $namlen)))
                  (i32.store8 (i32.add (local.get $cp) (i32.add (local.get $dirLen) (i32.add (i32.const 1) (local.get $k))))
                    (i32.load8_u (i32.add (local.get $dir) (i32.add (local.get $nameOff) (local.get $k)))))
                  (local.set $k (i32.add (local.get $k) (i32.const 1)))
                  (br $cl2)))
                (local.set $childLen (i32.add (local.get $dirLen) (i32.add (i32.const 1) (local.get $namlen))))
                (global.set $d6_path_sp (i32.add (local.get $cp) (local.get $childLen)))
                ;; index entry (pathOff, pathLen, type)
                (i32.store (global.get $d6_idx_sp) (local.get $cp))
                (i32.store (i32.add (global.get $d6_idx_sp) (i32.const 4)) (local.get $childLen))
                (i32.store (i32.add (global.get $d6_idx_sp) (i32.const 8)) (local.get $dtype))
                (global.set $d6_idx_sp (i32.add (global.get $d6_idx_sp) (i32.const 12)))
                (local.set $count (i32.add (local.get $count) (i32.const 1)))))))
        (local.set $off (i32.add (local.get $off) (i32.add (i32.const 24) (local.get $namlen))))
        (br $el)))
      (br_if $rddone (i32.lt_u (local.get $bufused) (i32.const 65536)))  ;; all entries fit
      (br $rdlp)))
    (drop (call $fd_close (local.get $fd)))
    ;; insertion sort the index [saveIdx, saveIdx+count*12) by full-path bytes (== by name)
    (local.set $i (i32.const 1))
    (block $sb (loop $sl
      (br_if $sb (i32.ge_s (local.get $i) (local.get $count)))
      (local.set $ei (i32.add (local.get $saveIdx) (i32.mul (local.get $i) (i32.const 12))))
      (local.set $tOff (i32.load (local.get $ei)))
      (local.set $tLen (i32.load (i32.add (local.get $ei) (i32.const 4))))
      (local.set $tType (i32.load (i32.add (local.get $ei) (i32.const 8))))
      (local.set $j (i32.sub (local.get $i) (i32.const 1)))
      (block $wb (loop $wl
        (br_if $wb (i32.lt_s (local.get $j) (i32.const 0)))
        (local.set $ej (i32.add (local.get $saveIdx) (i32.mul (local.get $j) (i32.const 12))))
        (local.set $pOff (i32.load (local.get $ej)))
        (local.set $pLen (i32.load (i32.add (local.get $ej) (i32.const 4))))
        (br_if $wb (i32.le_s (call $d6_pathcmp (local.get $pOff) (local.get $pLen)
                                               (local.get $tOff) (local.get $tLen)) (i32.const 0)))
        ;; shift ej -> ej+12
        (i32.store (i32.add (local.get $ej) (i32.const 12)) (local.get $pOff))
        (i32.store (i32.add (local.get $ej) (i32.const 16)) (local.get $pLen))
        (i32.store (i32.add (local.get $ej) (i32.const 20)) (i32.load (i32.add (local.get $ej) (i32.const 8))))
        (local.set $j (i32.sub (local.get $j) (i32.const 1)))
        (br $wl)))
      (local.set $ej (i32.add (local.get $saveIdx) (i32.mul (i32.add (local.get $j) (i32.const 1)) (i32.const 12))))
      (i32.store (local.get $ej) (local.get $tOff))
      (i32.store (i32.add (local.get $ej) (i32.const 4)) (local.get $tLen))
      (i32.store (i32.add (local.get $ej) (i32.const 8)) (local.get $tType))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $sl)))
    ;; walk sorted entries depth-first pre-order
    (local.set $i (i32.const 0))
    (block $ib (loop $il
      (br_if $ib (i32.ge_s (local.get $i) (local.get $count)))
      (local.set $ei (i32.add (local.get $saveIdx) (i32.mul (local.get $i) (i32.const 12))))
      (local.set $pOff (i32.load (local.get $ei)))
      (local.set $pLen (i32.load (i32.add (local.get $ei) (i32.const 4))))
      (local.set $pType (i32.load (i32.add (local.get $ei) (i32.const 8))))
      (if (i32.eq (local.get $pType) (i32.const 3))
        (then
          ;; subdir: recurse (threads ownership of $s through)
          (local.set $s (call $d6_foldwalk (local.get $pOff) (local.get $pLen)
            (local.get $sfxPtr) (local.get $sfxLen) (local.get $step) (local.get $s))))
        (else
          ;; file: apply the step iff the name ends with the suffix
          (if (i32.and (i32.ge_s (local.get $pLen) (local.get $sfxLen))
                       (call $d6_memeq (i32.add (local.get $pOff) (i32.sub (local.get $pLen) (local.get $sfxLen)))
                                       (local.get $sfxPtr) (local.get $sfxLen)))
            (then
              (call $d6_readfile (local.get $pOff) (local.get $pLen))
              (local.set $len)
              (local.set $buf)
              (if (local.get $buf)
                (then
                  (local.set $content (call $d6_h2s (local.get $buf) (local.get $len)))
                  ;; s = apply(apply(apply(step, s), content), unit)
                  (local.set $ca (call $rt_apply (local.get $step) (local.get $s)))
                  (local.set $cb (call $rt_apply (local.get $ca) (local.get $content)))
                  (call $rt_release (local.get $ca))
                  (local.set $s (call $rt_apply (local.get $cb) (call $rt_unit)))
                  (call $rt_release (local.get $cb))))))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $il)))
    ;; restore bump cursors (free this level's path bytes + index entries)
    (global.set $d6_path_sp (local.get $savePath))
    (global.set $d6_idx_sp (local.get $saveIdx))
    (local.get $s))
`

// WasmBibleFoldDir returns the WAT recursive dir-walk helper ($d6_foldwalk + its scratch
// globals). Exported for the external test package's isolation harness (walks the foldfix
// fixture under `wasmtime run --dir=.` and checks the matched-file count).
func WasmBibleFoldDir() string { return wasmBibleFoldDir }

// wasmBibleWriteOps is the Task-4 write-stream infrastructure: a fixed 256-slot i32 fd
// handle table at $D6WH (1572864, just below the bumped $hp=1573888), a monotonic counter
// $d6_whid, and a write-open helper $d6_wopen. Emitted when any of openWrite/writeChunk/
// closeWrite/sortFile is referenced; always follows wasmBibleReadFile (needs $D6SYS for the
// path_open fd_out cell at $D6SYS+8 and for the fd_write iovec at $D6SYS+0/+4/+12/+24).
//
// Handle table layout: zero-initialized by WAT linear memory (every slot starts as 0,
// meaning "no fd assigned"). Valid handle IDs are 1..255. openWrite allocates slots by
// incrementing $d6_whid; the table is intentionally not recycled (simple + sufficient for
// the bible builder's sequential write pattern). $d6_wopen uses $D6SYS+8 as the fd_out
// cell -- safe because write-open and file-read never overlap (single-threaded module).
const wasmBibleWriteOps = `
  ;; ---- Task-4 write-stream: handle table (256 i32 fd slots at $D6WH) ----
  ;; $D6WH: 256 i32 fd slots at offset 1572864. Zero-initialized by WAT linear memory;
  ;; slot 0 is unused (0 = invalid handle). Valid IDs: 1..255. $d6_whid: the next-id counter.
  (global $D6WH    i32 (i32.const 1572864))  ;; handle fd table: 256*4=1024 bytes
  (global $d6_whid (mut i32) (i32.const 0))  ;; handle counter (0 = none assigned yet)

  ;; $d6_wopen: create/truncate a file at $pbuf[0,$plen) for writing.
  ;; Calls path_open(preopen=3, dirflags=SYMLINK_FOLLOW=1, oflags=O_CREAT|O_TRUNC=9,
  ;;   rights=FD_WRITE=0x40, inheriting=0, fdflags=0, fd_out=$D6SYS+8).
  ;; Returns the opened fd (>=0) on success, or -1 on any errno.
  (func $d6_wopen (param $pbuf i32) (param $plen i32) (result i32)
    (local $rc i32)
    (local.set $rc (call $path_open
      (i32.const 3) (i32.const 1)
      (local.get $pbuf) (local.get $plen)
      (i32.const 9)
      (i64.const 64) (i64.const 0)
      (i32.const 0)
      (i32.add (global.get $D6SYS) (i32.const 8))))
    (if (local.get $rc) (then (return (i32.const -1))))
    (i32.load (i32.add (global.get $D6SYS) (i32.const 8))))
`

// WasmBibleWriteOps returns the Task-4 write-stream infrastructure WAT fragment: the fd
// handle table globals ($D6WH/$d6_whid) and the write-open helper ($d6_wopen). Exported
// for external tests; needs wasmBibleReadFile to precede it (uses $D6SYS).
func WasmBibleWriteOps() string { return wasmBibleWriteOps }
