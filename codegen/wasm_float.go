package codegen

import (
	"fmt"
	"strings"
)

// wasm_float.go -- Task 4b: the machine-float (f64) IO host ops on the ninth (WASM)
// backend: parseFloat (Nat -> Option Float), getFloat (IO Float), printFloat
// (Float -> IO Float), plus the base D3 float kit the listings need (fromNat/fmul + the
// Float TYPE). WAT has no snprintf/strtod, so the shortest-round-trip formatter and the
// exact-enough parser are built here from integer + f64 machinery.
//
// VALUE REP: a Float is a K_FLOAT (kind=9) heap box holding one IEEE-754 f64 at payload
// offset 4 (a LEAF like K_BIG/K_BIN -- rt_free's default branch reclaims it, no child
// pointers). The K_FLOAT helpers ($rt_mkfloat/$rt_float_val/$big_to_double) + the
// validate/atof/format helpers live in wasmFloatRuntime, emitted once when any float op
// is present (usesFloatWasm) -- self-contained, referencing only base-runtime symbols
// ($alloc/$sw/$w/$big_nlimbs/$big_limb/$is_int/$puts/$fd_read) plus, for parseFloat, the
// codec ($d6_s2h_to/$D6BUF, pulled in by adding parseFloat to usesBibleCodec).
//
// FORMAT (f64 -> shortest decimal): EXACT Dragon4 (Steele-White FPP2 free-format) over the
// base-1e9 limb bignums ($flt_dragon4). It sets up the rational R/S with high/low margins per
// the four IEEE boundary cases and emits the shortest digit string that rounds back to x,
// correctly rounded (ties to even), for EVERY finite double incl subnormals and 16/17-sig-digit
// values -- there is no (f64)N reconstruction, so no p>=16 precision loss. The per-digit
// quotient (0..9) is computed by repeated subtraction (the runtime has no general bignum
// division). Then the same four ECMAScript Number::toString dressing cases place the point.
//
// PARSE (decimal string -> f64): the strtod fast path -- accumulate the significand D
// (<= 18 digits) into an i64, track the decimal exponent k, then x = (f64)D * 10^k (or
// / 10^-k) via an exact power-of-ten multiply. Correctly rounded for |k| <= 22 and
// D < 2^53; anything wider routes to the correctly-rounded bignum slow path ($flt_atof_slow).
//
// MEMORY: the float ops use a dedicated 64 KiB scratch window $D6FLT at [1968128,2033664)
// (the reserved-region top, above the Task-6 foldDir windows). $hp is bumped to 2033664 in
// wasm_runtime.go so the heap never overwrites it (new windows above existing ones, bump
// $hp, document -- the ledger convention). Sub-layout within $D6FLT: [+0,+12) getFloat
// fd_read iovec+nread cells, [+16,+59016) getFloat stdin line buffer, [+60000,...)
// printFloat format-output buffer, [+61000,+61024) the Dragon4 digit string, [+61024,+61028)
// the Dragon4 decimal-position (n) cell. Bignums allocate transiently on the heap (above $hp).

// wasmFloatWindow is $D6FLT's base (documented in wasm_runtime.go's heap-layout comment).
const wasmFloatWindow = 1968128

// wasmFloatRuntime is the WAT float runtime: the K_FLOAT box helpers, big-nat -> f64, the
// power-of-ten builders, the float-literal DFA validator, the atof (fast path), and the
// ECMAScript shortest-round-trip formatter. Emitted once (emitFloatRuntimeWasm) when any
// float op is used.
const wasmFloatRuntime = `
  ;; ---- Task-4b machine-float (f64) runtime: K_FLOAT box + validate/atof/format ----
  (global $D6FLT i32 (i32.const 1968128))  ;; float ops scratch (64 KiB, reserved-region top)

  ;; K_FLOAT=9 box: [kind=9][f64 at payload offset 4]. A leaf (rt_free default branch).
  (func $rt_mkfloat (param $d f64) (result i32)
    (local $o i32)
    (local.set $o (call $alloc (i32.const 12)))
    (call $sw (local.get $o) (i32.const 0) (i32.const 9))   ;; K_FLOAT
    (f64.store (i32.add (local.get $o) (i32.const 4)) (local.get $d))
    (local.get $o))
  (func $rt_float_val (param $v i32) (result f64)
    (f64.load (i32.add (local.get $v) (i32.const 4))))

  ;; $big_to_double: a builtin-nat magnitude as an f64 (for fromNat: Nat -> Float). Handles
  ;; both the immediate-int rep ((n<<1)|1) and the K_BIG base-1e9 limb rep.
  (func $big_to_double (param $v i32) (result f64)
    (local $n i32) (local $i i32) (local $acc f64)
    (if (call $is_int (local.get $v))
      (then (return (f64.convert_i32_s (i32.shr_s (local.get $v) (i32.const 1))))))
    (local.set $n (call $big_nlimbs (local.get $v)))
    (local.set $acc (f64.const 0))
    (local.set $i (i32.sub (local.get $n) (i32.const 1)))
    (block $b (loop $l (br_if $b (i32.lt_s (local.get $i) (i32.const 0)))
      (local.set $acc (f64.add (f64.mul (local.get $acc) (f64.const 1000000000))
                               (f64.convert_i32_u (call $big_limb (local.get $v) (local.get $i)))))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (br $l)))
    (local.get $acc))

  ;; $flt_pow10: 10^k as f64 (k >= 0), by repeated multiply. Exact for k <= 22.
  (func $flt_pow10 (param $k i32) (result f64)
    (local $r f64)
    (local.set $r (f64.const 1))
    (block $b (loop $l (br_if $b (i32.le_s (local.get $k) (i32.const 0)))
      (local.set $r (f64.mul (local.get $r) (f64.const 10)))
      (local.set $k (i32.sub (local.get $k) (i32.const 1)))
      (br $l)))
    (local.get $r))
  ;; $flt_wr_uint: write a non-negative i32 in decimal at out[o..]; returns the new cursor.
  (func $flt_wr_uint (param $out i32) (param $o i32) (param $a i32) (result i32)
    (if (i32.ge_u (local.get $a) (i32.const 10))
      (then (local.set $o (call $flt_wr_uint (local.get $out) (local.get $o)
              (i32.div_u (local.get $a) (i32.const 10))))))
    (i32.store8 (i32.add (local.get $out) (local.get $o))
      (i32.add (i32.rem_u (local.get $a) (i32.const 10)) (i32.const 48)))
    (i32.add (local.get $o) (i32.const 1)))

  ;; $flt_validate: the float-literal DFA over [buf,buf+len) --
  ;; ^[+-]?((\d+(\.\d*)?)|(\.\d+))([eE][+-]?\d+)?$ (a mantissa is valid iff a digit sits
  ;; before the dot or a dot is followed by a digit; the exponent needs >= 1 digit). 1/0.
  ;; Byte-for-byte the C d6_validate_float / the Rust DFA reference.
  (func $flt_validate (param $buf i32) (param $len i32) (result i32)
    (local $i i32) (local $c i32) (local $hasdig i32) (local $hd i32)
    (if (i32.eqz (local.get $len)) (then (return (i32.const 0))))
    (local.set $i (i32.const 0))
    (local.set $c (i32.load8_u (local.get $buf)))
    (if (i32.or (i32.eq (local.get $c) (i32.const 43)) (i32.eq (local.get $c) (i32.const 45)))
      (then (local.set $i (i32.const 1))))
    (block $b1 (loop $l1
      (br_if $b1 (i32.ge_s (local.get $i) (local.get $len)))
      (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
      (br_if $b1 (i32.or (i32.lt_u (local.get $c) (i32.const 48)) (i32.gt_u (local.get $c) (i32.const 57))))
      (local.set $hasdig (i32.const 1))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $l1)))
    (if (i32.and (i32.lt_s (local.get $i) (local.get $len))
                 (i32.eq (i32.load8_u (i32.add (local.get $buf) (local.get $i))) (i32.const 46)))
      (then
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (block $b2 (loop $l2
          (br_if $b2 (i32.ge_s (local.get $i) (local.get $len)))
          (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
          (br_if $b2 (i32.or (i32.lt_u (local.get $c) (i32.const 48)) (i32.gt_u (local.get $c) (i32.const 57))))
          (local.set $hasdig (i32.const 1))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $l2)))))
    (if (i32.eqz (local.get $hasdig)) (then (return (i32.const 0))))
    (if (i32.lt_s (local.get $i) (local.get $len))
      (then
        (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
        (if (i32.or (i32.eq (local.get $c) (i32.const 101)) (i32.eq (local.get $c) (i32.const 69)))
          (then
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (if (i32.lt_s (local.get $i) (local.get $len))
              (then
                (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
                (if (i32.or (i32.eq (local.get $c) (i32.const 43)) (i32.eq (local.get $c) (i32.const 45)))
                  (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))))
            (local.set $hd (i32.const 0))
            (block $b3 (loop $l3
              (br_if $b3 (i32.ge_s (local.get $i) (local.get $len)))
              (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
              (br_if $b3 (i32.or (i32.lt_u (local.get $c) (i32.const 48)) (i32.gt_u (local.get $c) (i32.const 57))))
              (local.set $hd (i32.const 1))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $l3)))
            (if (i32.eqz (local.get $hd)) (then (return (i32.const 0))))))))
    (i32.eq (local.get $i) (local.get $len)))

  ;; $flt_atof: parse a VALIDATED float literal [buf,buf+len) to f64 (strtod fast path:
  ;; significand D (<=18 digits) into i64, decimal exponent k, x = D * 10^k). Correctly
  ;; rounded for |k| <= 22 and D < 2^53 (the whole corpus).
  (func $flt_atof (param $buf i32) (param $len i32) (result f64)
    (local $i i32) (local $neg i32) (local $D i64) (local $frac i32) (local $dot i32)
    (local $esign i32) (local $eval i32) (local $k i32) (local $x f64) (local $c i32) (local $ndig i32)
    (local.set $i (i32.const 0)) (local.set $neg (i32.const 0)) (local.set $D (i64.const 0))
    (if (i32.lt_s (local.get $i) (local.get $len))
      (then
        (local.set $c (i32.load8_u (local.get $buf)))
        (if (i32.eq (local.get $c) (i32.const 45)) (then (local.set $neg (i32.const 1)) (local.set $i (i32.const 1))))
        (if (i32.eq (local.get $c) (i32.const 43)) (then (local.set $i (i32.const 1))))))
    (block $ib (loop $il
      (br_if $ib (i32.ge_s (local.get $i) (local.get $len)))
      (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
      (if (i32.and (i32.ge_u (local.get $c) (i32.const 48)) (i32.le_u (local.get $c) (i32.const 57)))
        (then
          (if (i32.lt_s (local.get $ndig) (i32.const 18))
            (then
              (local.set $D (i64.add (i64.mul (local.get $D) (i64.const 10))
                (i64.extend_i32_u (i32.sub (local.get $c) (i32.const 48)))))
              (local.set $ndig (i32.add (local.get $ndig) (i32.const 1)))
              (if (local.get $dot) (then (local.set $frac (i32.add (local.get $frac) (i32.const 1))))))
            (else
              (if (i32.eqz (local.get $dot)) (then (local.set $frac (i32.sub (local.get $frac) (i32.const 1)))))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $il)))
      (if (i32.eq (local.get $c) (i32.const 46))
        (then (local.set $dot (i32.const 1)) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $il)))
      (br $ib)))
    (local.set $eval (i32.const 0)) (local.set $esign (i32.const 1))
    (if (i32.lt_s (local.get $i) (local.get $len))
      (then
        (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
        (if (i32.or (i32.eq (local.get $c) (i32.const 101)) (i32.eq (local.get $c) (i32.const 69)))
          (then
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (if (i32.lt_s (local.get $i) (local.get $len))
              (then
                (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
                (if (i32.eq (local.get $c) (i32.const 45)) (then (local.set $esign (i32.const -1)) (local.set $i (i32.add (local.get $i) (i32.const 1)))))
                (if (i32.eq (local.get $c) (i32.const 43)) (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))))
            (block $eb (loop $el
              (br_if $eb (i32.ge_s (local.get $i) (local.get $len)))
              (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
              (br_if $eb (i32.or (i32.lt_u (local.get $c) (i32.const 48)) (i32.gt_u (local.get $c) (i32.const 57))))
              (local.set $eval (i32.add (i32.mul (local.get $eval) (i32.const 10)) (i32.sub (local.get $c) (i32.const 48))))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $el)))))))
    (local.set $k (i32.sub (i32.mul (local.get $esign) (local.get $eval)) (local.get $frac)))
    ;; Fast path only when the i64 significand is EXACT (<= 15 accumulated digits, so
    ;; D < 10^15 < 2^53) AND the power of ten is exact (|k| <= 22): then (f64)D * 10^k is
    ;; correctly rounded (the classic single-rounding result). Anything wider double-rounds,
    ;; so route it to the correctly-rounded bignum slow path (Task 3).
    (if (i32.eqz (i32.and (i32.and (i32.le_s (local.get $ndig) (i32.const 15))
                                   (i32.ge_s (local.get $k) (i32.const -22)))
                          (i32.le_s (local.get $k) (i32.const 22))))
      (then (return (call $flt_atof_slow (local.get $buf) (local.get $len)))))
    (local.set $x (f64.convert_i64_u (local.get $D)))
    (if (i32.ge_s (local.get $k) (i32.const 0))
      (then (local.set $x (f64.mul (local.get $x) (call $flt_pow10 (local.get $k)))))
      (else (local.set $x (f64.div (local.get $x) (call $flt_pow10 (i32.sub (i32.const 0) (local.get $k)))))))
    (if (local.get $neg) (then (local.set $x (f64.neg (local.get $x)))))
    (local.get $x))

  ;; ---- Task-3 correctly-rounded bignum atof slow path (|k| > 22 or > 15 sig digits) ----
  ;; A faithful port of atofRef in wasm_float_display_test.go (the executable spec pinned
  ;; bit-for-bit to strconv.ParseFloat over 20000+ inputs). It composes ONLY the base-1e9
  ;; bignum ops already in wasm_runtime.go ($big_alloc/$big_setlimb/$big_nlimbs/$big_add/
  ;; $big_cmp/$rt_nat_mul/$rt_nat_monus/$rt_big_from_long/$rt_big_from_i64/$rt_release) --
  ;; no bignum/bignum division and no new runtime primitive. Every helper returns a FRESH
  ;; owned bignum and releases its transient temporaries (ARC discipline), so the slow path
  ;; is leak-free.

  ;; $flt_pow10big: 10^k as a base-1e9 bignum (k >= 0). 10^k = 10^(9*a+b) is the number
  ;; whose limb[a] = 10^b (< 1e9) and all lower limbs are zero -- one direct limb write.
  (func $flt_pow10big (param $k i32) (result i32)
    (local $a i32) (local $b i32) (local $v i32) (local $p i32)
    (local.set $a (i32.div_u (local.get $k) (i32.const 9)))
    (local.set $b (i32.rem_u (local.get $k) (i32.const 9)))
    (local.set $v (call $big_alloc (i32.add (local.get $a) (i32.const 1))))
    (local.set $p (i32.const 1))
    (block $pb (loop $pl (br_if $pb (i32.le_s (local.get $b) (i32.const 0)))
      (local.set $p (i32.mul (local.get $p) (i32.const 10)))
      (local.set $b (i32.sub (local.get $b) (i32.const 1)))
      (br $pl)))
    (call $big_setlimb (local.get $v) (local.get $a) (local.get $p))
    (local.get $v))

  ;; $flt_pow2big: 2^s as a base-1e9 bignum (s >= 0) by repeated doubling (r = r + r).
  (func $flt_pow2big (param $s i32) (result i32)
    (local $r i32) (local $old i32)
    (local.set $r (call $rt_big_from_long (i32.const 1)))
    (block $b (loop $l (br_if $b (i32.le_s (local.get $s) (i32.const 0)))
      (local.set $old (local.get $r))
      (local.set $r (call $big_add (local.get $old) (local.get $old)))
      (call $rt_release (local.get $old))
      (local.set $s (i32.sub (local.get $s) (i32.const 1)))
      (br $l)))
    (local.get $r))

  ;; $flt_shl2: a FRESH bignum equal to x * 2^s (s >= 0). s = 0 returns a fresh copy of x
  ;; (via x + 0) so callers can uniformly release the result.
  (func $flt_shl2 (param $x i32) (param $s i32) (result i32)
    (local $p i32) (local $r i32) (local $z i32)
    (if (i32.le_s (local.get $s) (i32.const 0))
      (then
        (local.set $z (call $big_alloc (i32.const 0)))
        (local.set $r (call $big_add (local.get $x) (local.get $z)))
        (call $rt_release (local.get $z))
        (return (local.get $r))))
    (local.set $p (call $flt_pow2big (local.get $s)))
    (local.set $r (call $rt_nat_mul (local.get $x) (local.get $p)))
    (call $rt_release (local.get $p))
    (local.get $r))

  ;; $flt_ge: is num/den / 2^e2 >= 2^p ? i.e. num >= 2^(p+e2) * den, checked with a single
  ;; power-of-two shift on whichever side keeps both operands integral. Borrows num/den.
  (func $flt_ge (param $num i32) (param $den i32) (param $e2 i32) (param $p i32) (result i32)
    (local $sh i32) (local $l i32) (local $r i32) (local $res i32) (local $lo i32) (local $ro i32)
    (local.set $sh (i32.add (local.get $p) (local.get $e2)))
    (if (i32.ge_s (local.get $sh) (i32.const 0))
      (then
        (local.set $l (local.get $num)) (local.set $lo (i32.const 0))
        (local.set $r (call $flt_shl2 (local.get $den) (local.get $sh))) (local.set $ro (i32.const 1)))
      (else
        (local.set $l (call $flt_shl2 (local.get $num) (i32.sub (i32.const 0) (local.get $sh)))) (local.set $lo (i32.const 1))
        (local.set $r (local.get $den)) (local.set $ro (i32.const 0))))
    (local.set $res (i32.ge_s (call $big_cmp (local.get $l) (local.get $r)) (i32.const 0)))
    (if (local.get $lo) (then (call $rt_release (local.get $l))))
    (if (local.get $ro) (then (call $rt_release (local.get $r))))
    (local.get $res))

  ;; $flt_round: the f64 nearest num/den (round to nearest, ties to even), num,den > 0.
  ;; $binexp is a cheap estimate of floor(log2(num/den)) from the decimal exponent, used
  ;; only to seed the search and to screen clear over/underflow; the exact path decides all
  ;; boundary cases. Mirrors atofRoundBits. Borrows num/den.
  (func $flt_round (param $num i32) (param $den i32) (param $binexp i32) (result f64)
    (local $e2 i32) (local $a i32) (local $b i32) (local $q i64) (local $j i32)
    (local $cand i64) (local $cb i32) (local $t i32) (local $qb i32) (local $rem i32)
    (local $tworem i32) (local $cmp i32) (local $E i32) (local $bits i64)
    ;; clear-overflow / clear-underflow screens (safe margins over the +-1 estimate error).
    (if (i32.ge_s (local.get $binexp) (i32.const 1026)) (then (return (f64.const inf))))
    (if (i32.le_s (local.get $binexp) (i32.const -1082)) (then (return (f64.const 0))))
    ;; find e2 with 2^52 <= (num/den)/2^e2 < 2^53
    (local.set $e2 (i32.sub (local.get $binexp) (i32.const 53)))
    (block $b1 (loop $l1
      (br_if $b1 (i32.eqz (call $flt_ge (local.get $num) (local.get $den) (local.get $e2) (i32.const 53))))
      (local.set $e2 (i32.add (local.get $e2) (i32.const 1)))
      (br $l1)))
    (block $b2 (loop $l2
      (br_if $b2 (call $flt_ge (local.get $num) (local.get $den) (local.get $e2) (i32.const 52)))
      (local.set $e2 (i32.sub (local.get $e2) (i32.const 1)))
      (br $l2)))
    (if (i32.lt_s (local.get $e2) (i32.const -1074)) (then (local.set $e2 (i32.const -1074))))
    ;; A = num << max(0,-e2), B = den << max(0,e2)
    (if (i32.ge_s (local.get $e2) (i32.const 0))
      (then
        (local.set $a (call $flt_shl2 (local.get $num) (i32.const 0)))
        (local.set $b (call $flt_shl2 (local.get $den) (local.get $e2))))
      (else
        (local.set $a (call $flt_shl2 (local.get $num) (i32.sub (i32.const 0) (local.get $e2))))
        (local.set $b (call $flt_shl2 (local.get $den) (i32.const 0)))))
    ;; q = floor(A/B), built MSB-first: keep bit j iff (q | 2^j) * B <= A. 0 <= q < 2^53.
    (local.set $q (i64.const 0))
    (local.set $j (i32.const 52))
    (block $bj (loop $lj (br_if $bj (i32.lt_s (local.get $j) (i32.const 0)))
      (local.set $cand (i64.or (local.get $q) (i64.shl (i64.const 1) (i64.extend_i32_s (local.get $j)))))
      (local.set $cb (call $rt_big_from_i64 (local.get $cand)))
      (local.set $t (call $rt_nat_mul (local.get $cb) (local.get $b)))
      (if (i32.le_s (call $big_cmp (local.get $t) (local.get $a)) (i32.const 0))
        (then (local.set $q (local.get $cand))))
      (call $rt_release (local.get $cb))
      (call $rt_release (local.get $t))
      (local.set $j (i32.sub (local.get $j) (i32.const 1)))
      (br $lj)))
    ;; round to nearest even from the exact remainder: compare 2*rem to B.
    (local.set $cb (call $rt_big_from_i64 (local.get $q)))
    (local.set $qb (call $rt_nat_mul (local.get $cb) (local.get $b)))
    (call $rt_release (local.get $cb))
    (local.set $rem (call $rt_nat_monus (local.get $a) (local.get $qb)))
    (local.set $tworem (call $big_add (local.get $rem) (local.get $rem)))
    (local.set $cmp (call $big_cmp (local.get $tworem) (local.get $b)))
    (call $rt_release (local.get $qb))
    (call $rt_release (local.get $rem))
    (call $rt_release (local.get $tworem))
    (call $rt_release (local.get $a))
    (call $rt_release (local.get $b))
    (if (i32.or (i32.gt_s (local.get $cmp) (i32.const 0))
                (i32.and (i32.eqz (local.get $cmp)) (i32.and (i32.wrap_i64 (local.get $q)) (i32.const 1))))
      (then (local.set $q (i64.add (local.get $q) (i64.const 1)))))
    ;; rounding carry out of the 53-bit significand bumps the exponent.
    (if (i64.eq (local.get $q) (i64.shl (i64.const 1) (i64.const 53)))
      (then (local.set $q (i64.shl (i64.const 1) (i64.const 52))) (local.set $e2 (i32.add (local.get $e2) (i32.const 1)))))
    (if (i64.eqz (local.get $q)) (then (return (f64.const 0))))
    (if (i64.ge_u (local.get $q) (i64.shl (i64.const 1) (i64.const 52)))
      (then
        (local.set $E (i32.add (local.get $e2) (i32.const 1075)))
        (if (i32.ge_s (local.get $E) (i32.const 2047)) (then (return (f64.const inf))))
        (local.set $bits (i64.or (i64.shl (i64.extend_i32_u (local.get $E)) (i64.const 52))
                                 (i64.sub (local.get $q) (i64.shl (i64.const 1) (i64.const 52)))))
        (return (f64.reinterpret_i64 (local.get $bits)))))
    ;; subnormal: biased exponent 0, q is the 52-bit fraction directly.
    (f64.reinterpret_i64 (local.get $q)))

  ;; $flt_atof_slow: the fully signed correctly-rounded parse of a VALIDATED literal
  ;; [buf,buf+len). It re-scans the buffer to build the exact integer significand Dfull as
  ;; a bignum (all mantissa digits) and the net decimal exponent k so |value| = Dfull*10^k,
  ;; forms the exact rational num/den, and rounds via $flt_round. Sign is applied here.
  (func $flt_atof_slow (param $buf i32) (param $len i32) (result f64)
    (local $i i32) (local $c i32) (local $neg i32) (local $dot i32) (local $frac i32)
    (local $nd i32) (local $seen i32) (local $D i32) (local $ten i32) (local $old i32)
    (local $mid i32) (local $dig i32) (local $esign i32) (local $eval i32) (local $k i32)
    (local $d10 i32) (local $binexp i32) (local $num i32) (local $den i32) (local $tmp i32) (local $x f64)
    (local.set $i (i32.const 0)) (local.set $neg (i32.const 0))
    (if (i32.lt_s (local.get $i) (local.get $len))
      (then
        (local.set $c (i32.load8_u (local.get $buf)))
        (if (i32.eq (local.get $c) (i32.const 45)) (then (local.set $neg (i32.const 1)) (local.set $i (i32.const 1))))
        (if (i32.eq (local.get $c) (i32.const 43)) (then (local.set $i (i32.const 1))))))
    ;; mantissa -> Dfull bignum; frac counts ALL fractional digits, nd counts SIGNIFICANT
    ;; digits (those from the first nonzero on) for the magnitude estimate.
    (local.set $D (call $big_alloc (i32.const 0)))
    (local.set $ten (call $rt_big_from_long (i32.const 10)))
    (block $mb (loop $ml
      (br_if $mb (i32.ge_s (local.get $i) (local.get $len)))
      (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
      (if (i32.and (i32.ge_u (local.get $c) (i32.const 48)) (i32.le_u (local.get $c) (i32.const 57)))
        (then
          (local.set $old (local.get $D))
          (local.set $mid (call $rt_nat_mul (local.get $old) (local.get $ten)))
          (local.set $dig (call $rt_big_from_long (i32.sub (local.get $c) (i32.const 48))))
          (local.set $D (call $big_add (local.get $mid) (local.get $dig)))
          (call $rt_release (local.get $old))
          (call $rt_release (local.get $mid))
          (call $rt_release (local.get $dig))
          (if (i32.ne (local.get $c) (i32.const 48)) (then (local.set $seen (i32.const 1))))
          (if (local.get $seen) (then (local.set $nd (i32.add (local.get $nd) (i32.const 1)))))
          (if (local.get $dot) (then (local.set $frac (i32.add (local.get $frac) (i32.const 1)))))
          (local.set $i (i32.add (local.get $i) (i32.const 1)))
          (br $ml)))
      (if (i32.eq (local.get $c) (i32.const 46))
        (then (local.set $dot (i32.const 1)) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $ml)))
      (br $mb)))
    (call $rt_release (local.get $ten))
    ;; exponent
    (local.set $esign (i32.const 1)) (local.set $eval (i32.const 0))
    (if (i32.lt_s (local.get $i) (local.get $len))
      (then
        (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
        (if (i32.or (i32.eq (local.get $c) (i32.const 101)) (i32.eq (local.get $c) (i32.const 69)))
          (then
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (if (i32.lt_s (local.get $i) (local.get $len))
              (then
                (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
                (if (i32.eq (local.get $c) (i32.const 45)) (then (local.set $esign (i32.const -1)) (local.set $i (i32.add (local.get $i) (i32.const 1)))))
                (if (i32.eq (local.get $c) (i32.const 43)) (then (local.set $i (i32.add (local.get $i) (i32.const 1)))))))
            (block $eb (loop $el
              (br_if $eb (i32.ge_s (local.get $i) (local.get $len)))
              (local.set $c (i32.load8_u (i32.add (local.get $buf) (local.get $i))))
              (br_if $eb (i32.or (i32.lt_u (local.get $c) (i32.const 48)) (i32.gt_u (local.get $c) (i32.const 57))))
              (local.set $eval (i32.add (i32.mul (local.get $eval) (i32.const 10)) (i32.sub (local.get $c) (i32.const 48))))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $el)))))))
    (local.set $k (i32.sub (i32.mul (local.get $esign) (local.get $eval)) (local.get $frac)))
    ;; zero mantissa -> signed zero
    (if (i32.eqz (call $big_nlimbs (local.get $D)))
      (then
        (call $rt_release (local.get $D))
        (if (local.get $neg) (then (return (f64.neg (f64.const 0)))))
        (return (f64.const 0))))
    ;; num/den: k>=0 -> num = Dfull*10^k, den = 1; k<0 -> num = Dfull, den = 10^-k.
    (if (i32.ge_s (local.get $k) (i32.const 0))
      (then
        (local.set $tmp (call $flt_pow10big (local.get $k)))
        (local.set $num (call $rt_nat_mul (local.get $D) (local.get $tmp)))
        (call $rt_release (local.get $tmp))
        (local.set $den (call $rt_big_from_long (i32.const 1))))
      (else
        (local.set $num (call $flt_shl2 (local.get $D) (i32.const 0)))
        (local.set $den (call $flt_pow10big (i32.sub (i32.const 0) (local.get $k))))))
    (call $rt_release (local.get $D))
    ;; binexp ~= floor((nd-1+k) * log2(10)); 217706/65536 = 3.3219299 approximates log2(10).
    (local.set $d10 (i32.add (i32.sub (local.get $nd) (i32.const 1)) (local.get $k)))
    (local.set $binexp (i32.shr_s (i32.mul (local.get $d10) (i32.const 217706)) (i32.const 16)))
    (local.set $x (call $flt_round (local.get $num) (local.get $den) (local.get $binexp)))
    (call $rt_release (local.get $num))
    (call $rt_release (local.get $den))
    (if (local.get $neg) (then (local.set $x (f64.neg (local.get $x)))))
    (local.get $x))

  ;; $flt_dragon4: the EXACT Steele-White FPP2 free-format shortest formatter over the base-1e9
  ;; limb bignums -- a faithful port of dragon4Digits in wasm_float_display_test.go (pinned to
  ;; strconv over 50000+ doubles). It writes the shortest significant decimal digits (ASCII,
  ;; MSD first) of the finite nonzero positive $x into $sd, stores the decimal position n at
  ;; [$np] (value = digits * 10^(n - p)), and returns the digit count p. No (f64)N step, so no
  ;; p>=16 precision loss. The per-digit quotient d (always 0..9) is computed by REPEATED
  ;; SUBTRACTION ($rt_nat_monus) since the runtime has no general bignum/bignum division; all
  ;; other steps use $rt_nat_mul / $big_add / $big_cmp / $flt_shl2 / $flt_pow2big. Every
  ;; transient bignum is released (ARC discipline), so the formatter is leak-free.
  (func $flt_dragon4 (param $x f64) (param $sd i32) (param $np i32) (result i32)
    (local $bits i64) (local $mant i64) (local $expo i32) (local $e i32) (local $even i32) (local $isbnd i32)
    (local $f i32) (local $be i32) (local $R i32) (local $S i32) (local $Mp i32) (local $Mm i32) (local $ten i32)
    (local $k i32) (local $p i32) (local $d i32) (local $rpm i32) (local $t1 i32)
    (local $c i32) (local $cLow i32) (local $cHigh i32) (local $low i32) (local $high i32) (local $roundup i32)
    (local $i i32) (local $ch i32)
    ;; decompose |x| = f * 2^e from the IEEE bits.
    (local.set $bits (i64.reinterpret_f64 (local.get $x)))
    (local.set $mant (i64.and (local.get $bits) (i64.const 4503599627370495)))     ;; 2^52 - 1
    (local.set $expo (i32.wrap_i64 (i64.and (i64.shr_u (local.get $bits) (i64.const 52)) (i64.const 2047))))
    (if (i32.eqz (local.get $expo))
      (then                                                                        ;; subnormal
        (local.set $f (call $rt_big_from_i64 (local.get $mant)))
        (local.set $e (i32.const -1074)))
      (else                                                                        ;; normal
        (local.set $f (call $rt_big_from_i64 (i64.or (local.get $mant) (i64.shl (i64.const 1) (i64.const 52)))))
        (local.set $e (i32.sub (local.get $expo) (i32.const 1075)))))
    (local.set $even (i32.eqz (i32.and (i32.wrap_i64 (local.get $mant)) (i32.const 1))))
    (local.set $isbnd (i32.and (i32.ne (local.get $expo) (i32.const 0)) (i64.eqz (local.get $mant))))
    (local.set $ten (call $rt_big_from_long (i32.const 10)))
    ;; R/S with high/low margins Mp/Mm (the four Dragon4 boundary cases).
    (if (i32.ge_s (local.get $e) (i32.const 0))
      (then
        (local.set $be (call $flt_pow2big (local.get $e)))                          ;; 2^e
        (if (i32.eqz (local.get $isbnd))
          (then                                                                     ;; equal gaps
            (local.set $t1 (call $rt_nat_mul (local.get $f) (local.get $be)))
            (local.set $R (call $flt_shl2 (local.get $t1) (i32.const 1)))           ;; R = f*be*2
            (call $rt_release (local.get $t1))
            (local.set $S (call $rt_big_from_long (i32.const 2)))
            (local.set $Mp (call $flt_shl2 (local.get $be) (i32.const 0)))
            (local.set $Mm (call $flt_shl2 (local.get $be) (i32.const 0))))
          (else                                                                     ;; unequal gaps (2^n boundary)
            (local.set $t1 (call $rt_nat_mul (local.get $f) (local.get $be)))
            (local.set $R (call $flt_shl2 (local.get $t1) (i32.const 2)))           ;; R = f*be*4
            (call $rt_release (local.get $t1))
            (local.set $S (call $rt_big_from_long (i32.const 4)))
            (local.set $Mp (call $flt_shl2 (local.get $be) (i32.const 1)))         ;; Mp = be*2
            (local.set $Mm (call $flt_shl2 (local.get $be) (i32.const 0)))))
        (call $rt_release (local.get $be)))
      (else
        (if (i32.or (i32.le_s (local.get $expo) (i32.const 1)) (i32.eqz (local.get $isbnd)))
          (then                                                                     ;; equal gaps
            (local.set $R (call $flt_shl2 (local.get $f) (i32.const 1)))           ;; R = f*2
            (local.set $S (call $flt_pow2big (i32.sub (i32.const 1) (local.get $e)))) ;; S = 2^(1-e)
            (local.set $Mp (call $rt_big_from_long (i32.const 1)))
            (local.set $Mm (call $rt_big_from_long (i32.const 1))))
          (else                                                                     ;; unequal gaps
            (local.set $R (call $flt_shl2 (local.get $f) (i32.const 2)))           ;; R = f*4
            (local.set $S (call $flt_pow2big (i32.sub (i32.const 2) (local.get $e)))) ;; S = 2^(2-e)
            (local.set $Mp (call $rt_big_from_long (i32.const 2)))
            (local.set $Mm (call $rt_big_from_long (i32.const 1)))))))
    (call $rt_release (local.get $f))
    ;; scale: bump S up (k++) then R,Mp,Mm up (k--) so value = 0.d1d2... * 10^k, first digit 1..9.
    (local.set $k (i32.const 0))
    (block $hb (loop $hl
      (local.set $rpm (call $big_add (local.get $R) (local.get $Mp)))
      (local.set $c (call $big_cmp (local.get $rpm) (local.get $S)))
      (call $rt_release (local.get $rpm))
      (br_if $hb (i32.eqz (i32.or (i32.gt_s (local.get $c) (i32.const 0))
                                  (i32.and (local.get $even) (i32.eqz (local.get $c))))))
      (local.set $t1 (call $rt_nat_mul (local.get $S) (local.get $ten)))
      (call $rt_release (local.get $S)) (local.set $S (local.get $t1))
      (local.set $k (i32.add (local.get $k) (i32.const 1)))
      (br $hl)))
    (block $lb (loop $ll
      (local.set $rpm (call $big_add (local.get $R) (local.get $Mp)))
      (local.set $t1 (call $rt_nat_mul (local.get $rpm) (local.get $ten)))
      (call $rt_release (local.get $rpm))
      (local.set $c (call $big_cmp (local.get $t1) (local.get $S)))
      (call $rt_release (local.get $t1))
      (br_if $lb (i32.eqz (i32.or (i32.lt_s (local.get $c) (i32.const 0))
                                  (i32.and (local.get $even) (i32.eqz (local.get $c))))))
      (local.set $t1 (call $rt_nat_mul (local.get $R) (local.get $ten))) (call $rt_release (local.get $R)) (local.set $R (local.get $t1))
      (local.set $t1 (call $rt_nat_mul (local.get $Mp) (local.get $ten))) (call $rt_release (local.get $Mp)) (local.set $Mp (local.get $t1))
      (local.set $t1 (call $rt_nat_mul (local.get $Mm) (local.get $ten))) (call $rt_release (local.get $Mm)) (local.set $Mm (local.get $t1))
      (local.set $k (i32.sub (local.get $k) (i32.const 1)))
      (br $ll)))
    ;; generate: emit digits until within a neighbour's margin, then round.
    (local.set $p (i32.const 0))
    (block $gb (loop $gl
      (local.set $t1 (call $rt_nat_mul (local.get $R) (local.get $ten))) (call $rt_release (local.get $R)) (local.set $R (local.get $t1))
      (local.set $t1 (call $rt_nat_mul (local.get $Mp) (local.get $ten))) (call $rt_release (local.get $Mp)) (local.set $Mp (local.get $t1))
      (local.set $t1 (call $rt_nat_mul (local.get $Mm) (local.get $ten))) (call $rt_release (local.get $Mm)) (local.set $Mm (local.get $t1))
      ;; d = floor(R/S) in 0..9 by repeated subtraction; R becomes R mod S.
      (local.set $d (i32.const 0))
      (block $sb (loop $sl
        (br_if $sb (i32.lt_s (call $big_cmp (local.get $R) (local.get $S)) (i32.const 0)))
        (local.set $t1 (call $rt_nat_monus (local.get $R) (local.get $S))) (call $rt_release (local.get $R)) (local.set $R (local.get $t1))
        (local.set $d (i32.add (local.get $d) (i32.const 1)))
        (br $sl)))
      (local.set $cLow (call $big_cmp (local.get $R) (local.get $Mm)))
      (local.set $low (i32.or (i32.lt_s (local.get $cLow) (i32.const 0))
                              (i32.and (local.get $even) (i32.eqz (local.get $cLow)))))
      (local.set $rpm (call $big_add (local.get $R) (local.get $Mp)))
      (local.set $cHigh (call $big_cmp (local.get $rpm) (local.get $S)))
      (call $rt_release (local.get $rpm))
      (local.set $high (i32.or (i32.gt_s (local.get $cHigh) (i32.const 0))
                               (i32.and (local.get $even) (i32.eqz (local.get $cHigh)))))
      (if (i32.and (i32.eqz (local.get $low)) (i32.eqz (local.get $high)))
        (then
          (i32.store8 (i32.add (local.get $sd) (local.get $p)) (i32.add (local.get $d) (i32.const 48)))
          (local.set $p (i32.add (local.get $p) (i32.const 1)))
          (br $gl)))
      ;; terminal digit: decide whether to round up.
      (local.set $roundup (i32.const 0))
      (if (i32.and (local.get $high) (i32.eqz (local.get $low)))
        (then (local.set $roundup (i32.const 1)))
        (else (if (i32.and (local.get $low) (local.get $high))
          (then                                                                     ;; both margins: nearer, ties to even
            (local.set $rpm (call $big_add (local.get $R) (local.get $R)))         ;; 2R
            (local.set $c (call $big_cmp (local.get $rpm) (local.get $S)))
            (call $rt_release (local.get $rpm))
            (if (i32.gt_s (local.get $c) (i32.const 0))
              (then (local.set $roundup (i32.const 1)))
              (else (if (i32.eqz (local.get $c))
                (then (local.set $roundup (i32.and (local.get $d) (i32.const 1))))))))))) ;; tie -> even digit
      (i32.store8 (i32.add (local.get $sd) (local.get $p)) (i32.add (local.get $d) (i32.const 48)))
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (if (local.get $roundup)
        (then
          (local.set $i (i32.sub (local.get $p) (i32.const 1)))
          (block $cb (loop $cl
            (br_if $cb (i32.lt_s (local.get $i) (i32.const 0)))
            (local.set $ch (i32.add (i32.load8_u (i32.add (local.get $sd) (local.get $i))) (i32.const 1)))
            (i32.store8 (i32.add (local.get $sd) (local.get $i)) (local.get $ch))
            (br_if $cb (i32.ne (local.get $ch) (i32.const 58)))                    ;; no overflow past '9' -> done
            (i32.store8 (i32.add (local.get $sd) (local.get $i)) (i32.const 48))   ;; carry: this digit -> '0'
            (local.set $i (i32.sub (local.get $i) (i32.const 1)))
            (br $cl)))
          (if (i32.lt_s (local.get $i) (i32.const 0))
            (then                                                                   ;; full carry: 0.99..9 -> 1.0, digits "1", k++
              (i32.store8 (local.get $sd) (i32.const 49))
              (local.set $p (i32.const 1))
              (local.set $k (i32.add (local.get $k) (i32.const 1)))))))))
    ;; strip trailing zeros (only a round-up carry can create them; shortest never needs them).
    (block $tb (loop $tl
      (br_if $tb (i32.le_s (local.get $p) (i32.const 1)))
      (br_if $tb (i32.ne (i32.load8_u (i32.add (local.get $sd) (i32.sub (local.get $p) (i32.const 1)))) (i32.const 48)))
      (local.set $p (i32.sub (local.get $p) (i32.const 1)))
      (br $tl)))
    (call $rt_release (local.get $R))
    (call $rt_release (local.get $S))
    (call $rt_release (local.get $Mp))
    (call $rt_release (local.get $Mm))
    (call $rt_release (local.get $ten))
    (i32.store (local.get $np) (local.get $k))
    (local.get $p))

  ;; $flt_fmt: write the ECMAScript Number::toString(10) shortest-round-trip rendering of
  ;; $x into $out (no trailing NUL); returns the byte length. Digits come from the EXACT
  ;; Dragon4 formatter ($flt_dragon4); the four dressing cases place the decimal point.
  (func $flt_fmt (param $x f64) (param $out i32) (result i32)
    (local $neg i32) (local $p i32) (local $o i32) (local $i i32)
    (local $sdig i32) (local $n i32) (local $en i32)
    ;; NaN
    (if (f64.ne (local.get $x) (local.get $x))
      (then
        (i32.store8 (local.get $out) (i32.const 78)) (i32.store8 (i32.add (local.get $out) (i32.const 1)) (i32.const 97))
        (i32.store8 (i32.add (local.get $out) (i32.const 2)) (i32.const 78))
        (return (i32.const 3))))
    ;; +Infinity
    (if (f64.gt (local.get $x) (f64.const 1.7976931348623157e308))
      (then (return (call $flt_wr_inf (local.get $out) (i32.const 0)))))
    ;; -Infinity
    (if (f64.lt (local.get $x) (f64.const -1.7976931348623157e308))
      (then
        (i32.store8 (local.get $out) (i32.const 45))
        (return (call $flt_wr_inf (local.get $out) (i32.const 1)))))
    ;; +/-0 -> "0" (f64.eq treats -0 == 0)
    (if (f64.eq (local.get $x) (f64.const 0))
      (then (i32.store8 (local.get $out) (i32.const 48)) (return (i32.const 1))))
    (local.set $neg (i32.const 0))
    (if (f64.lt (local.get $x) (f64.const 0)) (then (local.set $neg (i32.const 1)) (local.set $x (f64.neg (local.get $x)))))
    ;; EXACT Dragon4 digit generation (no (f64)N reconstruction): fills $sdig with $p shortest
    ;; digits and stores the decimal position n at [$D6FLT+61024]. $x is finite nonzero positive.
    (local.set $sdig (i32.add (global.get $D6FLT) (i32.const 61000)))
    (local.set $p (call $flt_dragon4 (local.get $x) (local.get $sdig)
      (i32.add (global.get $D6FLT) (i32.const 61024))))
    (local.set $n (i32.load (i32.add (global.get $D6FLT) (i32.const 61024))))
    (local.set $o (i32.const 0))
    (if (local.get $neg) (then (i32.store8 (local.get $out) (i32.const 45)) (local.set $o (i32.const 1))))
    (if (i32.and (i32.le_s (local.get $p) (local.get $n)) (i32.le_s (local.get $n) (i32.const 21)))
      (then
        ;; digits, then (n-p) trailing zeros
        (local.set $i (i32.const 0))
        (block $c1b (loop $c1l (br_if $c1b (i32.ge_s (local.get $i) (local.get $p)))
          (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.load8_u (i32.add (local.get $sdig) (local.get $i))))
          (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $c1l)))
        (local.set $i (local.get $p))
        (block $z1b (loop $z1l (br_if $z1b (i32.ge_s (local.get $i) (local.get $n)))
          (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 48))
          (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $z1l))))
      (else (if (i32.and (i32.gt_s (local.get $n) (i32.const 0)) (i32.lt_s (local.get $n) (local.get $p)))
        (then
          ;; digits[0..n) '.' digits[n..p)
          (local.set $i (i32.const 0))
          (block $c2b (loop $c2l (br_if $c2b (i32.ge_s (local.get $i) (local.get $n)))
            (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.load8_u (i32.add (local.get $sdig) (local.get $i))))
            (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $c2l)))
          (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 46)) (local.set $o (i32.add (local.get $o) (i32.const 1)))
          (local.set $i (local.get $n))
          (block $c2b2 (loop $c2l2 (br_if $c2b2 (i32.ge_s (local.get $i) (local.get $p)))
            (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.load8_u (i32.add (local.get $sdig) (local.get $i))))
            (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $c2l2))))
        (else (if (i32.and (i32.ge_s (local.get $n) (i32.const -5)) (i32.le_s (local.get $n) (i32.const 0)))
          (then
            ;; "0." (-n) zeros digits
            (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 48)) (local.set $o (i32.add (local.get $o) (i32.const 1)))
            (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 46)) (local.set $o (i32.add (local.get $o) (i32.const 1)))
            (local.set $i (i32.const 0))
            (block $z3b (loop $z3l (br_if $z3b (i32.ge_s (local.get $i) (i32.sub (i32.const 0) (local.get $n))))
              (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 48))
              (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $z3l)))
            (local.set $i (i32.const 0))
            (block $c3b (loop $c3l (br_if $c3b (i32.ge_s (local.get $i) (local.get $p)))
              (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.load8_u (i32.add (local.get $sdig) (local.get $i))))
              (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $c3l))))
          (else
            ;; scientific: d1 ['.' d2..dp] 'e' sign(n-1) |n-1|
            (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.load8_u (local.get $sdig))) (local.set $o (i32.add (local.get $o) (i32.const 1)))
            (if (i32.gt_s (local.get $p) (i32.const 1))
              (then
                (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 46)) (local.set $o (i32.add (local.get $o) (i32.const 1)))
                (local.set $i (i32.const 1))
                (block $c4b (loop $c4l (br_if $c4b (i32.ge_s (local.get $i) (local.get $p)))
                  (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.load8_u (i32.add (local.get $sdig) (local.get $i))))
                  (local.set $o (i32.add (local.get $o) (i32.const 1))) (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $c4l)))))
            (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 101)) (local.set $o (i32.add (local.get $o) (i32.const 1)))
            (local.set $en (i32.sub (local.get $n) (i32.const 1)))
            (if (i32.ge_s (local.get $en) (i32.const 0))
              (then (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 43)) (local.set $o (i32.add (local.get $o) (i32.const 1))))
              (else
                (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 45)) (local.set $o (i32.add (local.get $o) (i32.const 1)))
                (local.set $en (i32.sub (i32.const 0) (local.get $en)))))
            (local.set $o (call $flt_wr_uint (local.get $out) (local.get $o) (local.get $en)))))))))
    (local.get $o))

  ;; $flt_wr_inf: write "Infinity" at out[o..]; returns the new cursor (o + 8).
  (func $flt_wr_inf (param $out i32) (param $o i32) (result i32)
    (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 73))                         ;; I
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 1))) (i32.const 110)) ;; n
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 2))) (i32.const 102)) ;; f
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 3))) (i32.const 105)) ;; i
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 4))) (i32.const 110)) ;; n
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 5))) (i32.const 105)) ;; i
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 6))) (i32.const 116)) ;; t
    (i32.store8 (i32.add (local.get $out) (i32.add (local.get $o) (i32.const 7))) (i32.const 121)) ;; y
    (i32.add (local.get $o) (i32.const 8)))
`

// usesFloatWasm reports whether p references any machine-float host op (so the K_FLOAT
// box + validate/atof/format runtime is baked exactly once).
func usesFloatWasm(p Program) bool {
	for _, op := range []string{
		"Float", "fromNat", "fmul", "parseFloat", "getFloat", "printFloat",
		// Track A: the remaining arithmetic (fadd/fsub/fdiv) + comparison (fleqN) ops --
		// same K_FLOAT box, same $rt_float_val reader.
		"fadd", "fsub", "fdiv", "fleqN",
	} {
		if usesForeign(p, op) {
			return true
		}
	}
	return false
}

// emitFloatRuntimeWasm bakes the float runtime (K_FLOAT box + validate/atof/format).
func (em *wasmEmitter) emitFloatRuntimeWasm(b *strings.Builder) { b.WriteString(wasmFloatRuntime) }

// emitFloatTypeWasm bakes `Float : U` -- an opaque foreign TYPE that erases to unit (it
// carries no runtime payload as a type; a Float VALUE is a K_FLOAT box). The accessor thunk
// returns $rt_unit (the immortal singleton), mirroring emitHandleWasm.
func (em *wasmEmitter) emitFloatTypeWasm(b *strings.Builder) {
	em.emitCachedThunk(b, "Float", func(_ *wasmFunc, _ *strings.Builder) string {
		return "(call $rt_unit)"
	})
}

// emitFromNatWasm bakes `fromNat : Nat -> Float` -- box a builtin-nat magnitude as an f64.
// PURE 1-arg: accessor -> closure(nat) -> Float. The owned $arg is consumed (released) --
// the result is a fresh K_FLOAT.
func (em *wasmEmitter) emitFromNatWasm(b *strings.Builder) {
	c1 := em.codeRef("fromNat_c1")
	b.WriteString("  (func $fromNat_c1 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_mkfloat (call $big_to_double (local.get $arg))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	em.emitCachedThunk(b, "fromNat", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFmulWasm bakes `fmul : Float -> Float -> Float` -- multiply two boxed doubles. PURE
// 2-arg curried: accessor -> c1(x->env) -> c2(y->result). c2 reads env[0] (x) BORROWED
// (float_val, no retain; freed when c2's closure dies) and the owned $arg (y), which it
// consumes (released); the result is a fresh K_FLOAT.
func (em *wasmEmitter) emitFmulWasm(b *strings.Builder) {
	c1 := em.codeRef("fmul_c1")
	c2 := em.codeRef("fmul_c2")
	b.WriteString("  (func $fmul_c2 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_mkfloat (f64.mul\n")
	b.WriteString("      (call $rt_float_val (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("      (call $rt_float_val (local.get $arg)))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	fmt.Fprintf(b, "  (func $fmul_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "fmul", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFaddWasm bakes `fadd : Float -> Float -> Float` -- add two boxed doubles. Structurally
// identical to emitFmulWasm (PURE 2-arg curried: accessor -> c1(x->env) -> c2(y->result),
// c2 reads env[0] (x) BORROWED and the owned $arg (y) which it consumes), only the WAT op
// (f64.add) and the closure names differ. Argument order: fadd x y = x + y, so this is
// commutative and the env[0]/$arg placement is not load-bearing here -- but it still
// matches the fmul convention (first curried arg -> env[0], second -> $arg) for uniformity.
func (em *wasmEmitter) emitFaddWasm(b *strings.Builder) {
	c1 := em.codeRef("fadd_c1")
	c2 := em.codeRef("fadd_c2")
	b.WriteString("  (func $fadd_c2 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_mkfloat (f64.add\n")
	b.WriteString("      (call $rt_float_val (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("      (call $rt_float_val (local.get $arg)))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	fmt.Fprintf(b, "  (func $fadd_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "fadd", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFsubWasm bakes `fsub : Float -> Float -> Float` -- subtract two boxed doubles.
// NON-COMMUTATIVE: fsub x y = x - y, and (matching emitFmulWasm's convention) the first
// curried argument x lands in env[0] and the second argument y arrives as $arg, so the
// WAT must compute f64.sub(env[0], $arg) -- x - y, NOT $arg - env[0].
func (em *wasmEmitter) emitFsubWasm(b *strings.Builder) {
	c1 := em.codeRef("fsub_c1")
	c2 := em.codeRef("fsub_c2")
	b.WriteString("  (func $fsub_c2 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_mkfloat (f64.sub\n")
	b.WriteString("      (call $rt_float_val (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("      (call $rt_float_val (local.get $arg)))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	fmt.Fprintf(b, "  (func $fsub_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "fsub", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFdivWasm bakes `fdiv : Float -> Float -> Float` -- divide two boxed doubles.
// NON-COMMUTATIVE: fdiv x y = x / y, first curried arg x in env[0], second arg y as $arg,
// so f64.div(env[0], $arg) -- x / y. WASM's f64.div is native IEEE-754: 0.0/0.0 is NaN,
// x/0.0 is +-inf, with no special-casing needed (unlike the Python/Erlang backends).
func (em *wasmEmitter) emitFdivWasm(b *strings.Builder) {
	c1 := em.codeRef("fdiv_c1")
	c2 := em.codeRef("fdiv_c2")
	b.WriteString("  (func $fdiv_c2 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_mkfloat (f64.div\n")
	b.WriteString("      (call $rt_float_val (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("      (call $rt_float_val (local.get $arg)))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	fmt.Fprintf(b, "  (func $fdiv_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "fdiv", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFleqNWasm bakes `fleqN : Float -> Float -> Whole` -- a<=b as a Whole/Nat literal (1
// if true, 0 if false), the guarded-tier primitive Std.Float.fle/feq/isNaN derive from
// (internal/prelude/prelude.rune ~8734). Same 2-arg curried shape as the arithmetic ops
// (first curried arg x in env[0], second arg y as $arg), but the RESULT is not a K_FLOAT:
// f64.le(env[0], $arg) yields an i32 0/1 directly, wrapped into a genuine K_BIG Whole via
// $rt_big_from_long -- the SAME builtin-nat constructor emitNatFold uses to seed its loop
// counter at 0 (wasm.go's emitNatFold). This matters: Whole/Nat is the compiler's builtin
// bignum type (codegen/ir.go's Nat field), and every consumer that pattern-matches a Whole
// (isZero's `case n of zero | succ`, which lowers to the WholeElim/NatElim spine and
// compares via $rt_big_cmp/$big_nlimbs) expects a REAL K_BIG box, not the separate
// immediate-int tag ($rt_mkint, reserved for the unrelated FFI LitInt path) -- $rt_big_cmp
// reads word[1] as nlimbs unconditionally and does not understand that tag. f64.le(NaN, x)
// is 0 for any x (including NaN itself), matching every other backend's "NaN is not <= to
// anything" semantics with no extra NaN check required.
func (em *wasmEmitter) emitFleqNWasm(b *strings.Builder) {
	c1 := em.codeRef("fleqN_c1")
	c2 := em.codeRef("fleqN_c2")
	b.WriteString("  (func $fleqN_c2 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_big_from_long (f64.le\n")
	b.WriteString("      (call $rt_float_val (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("      (call $rt_float_val (local.get $arg)))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	fmt.Fprintf(b, "  (func $fleqN_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "fleqN", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitParseFloatWasm bakes `parseFloat : Nat -> Option Float` -- decode the packed String
// (via the codec $d6_s2h_to into $D6BUF), validate the float-literal grammar, and on
// success atof + wrap in some, else none. PURE 1-arg: accessor -> closure(code) -> Option.
// none/some are built via mkcon(0,"none",1)/mkcon(1,"some",2) (the jsonStrField pattern).
// The owned $arg (the packed-String bignum) is consumed (released).
func (em *wasmEmitter) emitParseFloatWasm(b *strings.Builder) {
	noneOff := em.intern("none")
	someOff := em.intern("some")
	c1 := em.codeRef("parseFloat_c1")
	b.WriteString("  (func $parseFloat_c1 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $len i32) (local $buf i32) (local $none i32) (local $some i32) (local $fv i32)\n")
	b.WriteString("    (local.set $len (call $d6_s2h_to (local.get $arg) (global.get $D6BUF)))\n")
	b.WriteString("    (local.set $buf (global.get $D6BUF))\n")
	b.WriteString("    (if (i32.eqz (call $flt_validate (local.get $buf) (local.get $len)))\n")
	b.WriteString("      (then\n")
	fmt.Fprintf(b, "        (local.set $none (call $rt_mkcon (i32.const 0) (i32.const %d) (i32.const 1)))\n", noneOff)
	b.WriteString("        (call $rt_con_set (local.get $none) (i32.const 0) (call $rt_unit))\n")
	b.WriteString("        (call $rt_release (local.get $arg))\n")
	b.WriteString("        (return (local.get $none))))\n")
	b.WriteString("    (local.set $fv (call $rt_mkfloat (call $flt_atof (local.get $buf) (local.get $len))))\n")
	fmt.Fprintf(b, "    (local.set $some (call $rt_mkcon (i32.const 1) (i32.const %d) (i32.const 2)))\n", someOff)
	b.WriteString("    (call $rt_con_set (local.get $some) (i32.const 0) (call $rt_unit))\n")
	b.WriteString("    (call $rt_con_set (local.get $some) (i32.const 1) (local.get $fv))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $some))\n")
	em.emitCachedThunk(b, "parseFloat", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitGetFloatWasm bakes `getFloat : IO Float` -- read the first '\n'-terminated line of
// stdin (WASI fd_read on fd 0), strip ALL trailing '\r', validate the whole remainder, and
// atof it (garbage -> 0.0). World-only arity (like timeNanos): accessor -> closure(world)
// -> effect. The read buffer + fd_read iovec/nread cells live in the dedicated $D6FLT
// window (no codec dependency). The result is a fresh/owned K_FLOAT.
func (em *wasmEmitter) emitGetFloatWasm(b *strings.Builder) {
	w := em.codeRef("getFloat_w")
	b.WriteString("  (func $getFloat_w (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $nread i32) (local $lineLen i32) (local $bufp i32)\n")
	b.WriteString("    (local.set $bufp (i32.add (global.get $D6FLT) (i32.const 16)))\n")
	// iovec at $D6FLT: [bufp][cap]; nread out at $D6FLT+8.
	b.WriteString("    (i32.store (global.get $D6FLT) (local.get $bufp))\n")
	b.WriteString("    (i32.store (i32.add (global.get $D6FLT) (i32.const 4)) (i32.const 59000))\n")
	b.WriteString("    (drop (call $fd_read (i32.const 0) (global.get $D6FLT) (i32.const 1) (i32.add (global.get $D6FLT) (i32.const 8))))\n")
	b.WriteString("    (local.set $nread (i32.load (i32.add (global.get $D6FLT) (i32.const 8))))\n")
	// first line: up to '\n' (or all of nread)
	b.WriteString("    (local.set $lineLen (i32.const 0))\n")
	b.WriteString("    (block $b (loop $l\n")
	b.WriteString("      (br_if $b (i32.ge_s (local.get $lineLen) (local.get $nread)))\n")
	b.WriteString("      (br_if $b (i32.eq (i32.load8_u (i32.add (local.get $bufp) (local.get $lineLen))) (i32.const 10)))\n")
	b.WriteString("      (local.set $lineLen (i32.add (local.get $lineLen) (i32.const 1)))\n")
	b.WriteString("      (br $l)))\n")
	// strip ALL trailing '\r'
	b.WriteString("    (block $rb (loop $rl\n")
	b.WriteString("      (br_if $rb (i32.le_s (local.get $lineLen) (i32.const 0)))\n")
	b.WriteString("      (br_if $rb (i32.ne (i32.load8_u (i32.add (local.get $bufp) (i32.sub (local.get $lineLen) (i32.const 1)))) (i32.const 13)))\n")
	b.WriteString("      (local.set $lineLen (i32.sub (local.get $lineLen) (i32.const 1)))\n")
	b.WriteString("      (br $rl)))\n")
	b.WriteString("    (if (call $flt_validate (local.get $bufp) (local.get $lineLen))\n")
	b.WriteString("      (then (return (call $rt_mkfloat (call $flt_atof (local.get $bufp) (local.get $lineLen))))))\n")
	b.WriteString("    (call $rt_mkfloat (f64.const 0)))\n")
	em.emitCachedThunk(b, "getFloat", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, w)
		return "(local.get " + r + ")"
	})
}

// emitPrintFloatWasm bakes `printFloat : Float -> IO Float` -- print the canonical
// ECMAScript rendering + '\n' to stdout (fd 1), return the argument. 2-step curried
// IO-closure chain: c1(x->env) -> w(world->effect). The world step formats env[0]
// (BORROWED) into the $D6FLT format buffer, writes it via $puts, then RETAINS + returns
// the Float as an independent owned reference (mirrors printNat handing back its Nat).
func (em *wasmEmitter) emitPrintFloatWasm(b *strings.Builder) {
	c1 := em.codeRef("printFloat_c1")
	w := em.codeRef("printFloat_w")
	b.WriteString("  (func $printFloat_w (param $arg i32) (param $env i32) (result i32) (local $x i32) (local $len i32) (local $out i32)\n")
	b.WriteString("    (local.set $x (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $out (i32.add (global.get $D6FLT) (i32.const 60000)))\n")
	b.WriteString("    (local.set $len (call $flt_fmt (call $rt_float_val (local.get $x)) (local.get $out)))\n")
	b.WriteString("    (i32.store8 (i32.add (local.get $out) (local.get $len)) (i32.const 10))\n") // '\n'
	b.WriteString("    (call $puts (i32.const 1) (local.get $out) (i32.add (local.get $len) (i32.const 1)))\n")
	b.WriteString("    (call $rt_retain (local.get $x))\n") // yield an independent owned ref
	b.WriteString("    (local.get $x))\n")
	fmt.Fprintf(b, "  (func $printFloat_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", w)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "printFloat", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}
