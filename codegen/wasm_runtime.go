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
  ;; ---- WASI import: write the show buffer to stdout (fd 1) ----
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))

  ;; ---- linear memory + a bump allocator (no GC in v1) ----
  (memory (export "memory") 256)        ;; 256 pages = 16 MiB initial
  (global $hp (mut i32) (i32.const 65536))  ;; heap pointer; below it: scratch + cstrs
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
    (local $o i32)
    (local.set $o (call $alloc (i32.add (i32.const 8) (i32.shl (local.get $nlimbs) (i32.const 2)))))
    (call $sw (local.get $o) (i32.const 0) (i32.const 6))
    (call $sw (local.get $o) (i32.const 1) (local.get $nlimbs))
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

  (func $rt_big_succ (param $a i32) (result i32)
    (call $big_add (local.get $a) (call $rt_big_from_long (i32.const 1))))
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
  ;; Reads the digit count, then folds groups of 9 digits (most-significant first
  ;; into the top limb). Builds via repeated *10 + digit using big_mul/big_add - naive
  ;; but only runs once per literal.
  (func $rt_big_parse (param $p i32) (result i32)
    (local $acc i32) (local $ten i32) (local $c i32)
    (local.set $acc (call $big_alloc (i32.const 0)))
    (local.set $ten (call $rt_big_from_long (i32.const 10)))
    (block $b (loop $l
      (local.set $c (i32.load8_u (local.get $p)))
      (br_if $b (i32.eqz (local.get $c)))
      (local.set $acc (call $big_add
        (call $rt_nat_mul (local.get $acc) (local.get $ten))
        (call $rt_big_from_long (i32.sub (local.get $c) (i32.const 48)))))
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (br $l)))
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

  ;; rt_print_nl: write a single newline byte (10) to stdout.
  ;; Stores the byte at the scratch buffer base (4096) and calls puts with length 1.
  (func $rt_print_nl
    (i32.store8 (i32.const 4096) (i32.const 10))
    (call $puts (i32.const 1) (i32.const 4096) (i32.const 1)))
`

// WasmRuntime returns the WAT runtime string for the WASM backend. The underlying
// const is unexported so the runtime cannot be modified from outside codegen; this
// accessor gives the external test package read-only access.
func WasmRuntime() string { return wasmRuntime }
