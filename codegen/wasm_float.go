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
// FORMAT (f64 -> shortest decimal): the precision-search route the C __fmtf uses, but
// with WAT-native digit extraction: for p = 1..17, extract the p leading decimal digits
// (scale by 10^(E-p+1), f64.nearest to an integer significand N), reconstruct N * 10^k
// and compare to x; the first p that round-trips is the shortest. Then the four
// ECMAScript Number::toString dressing cases. All corpus values round-trip at p <= 3, so
// N stays well under 2^53 and every scale is an exact power of ten -- the reconstruction
// equals the fast-path atof of the same digits, so the round-trip test is exact and the
// output byte-identical to the other eight backends. (For p >= 16 -- never reached by the
// corpus -- (f64)N loses precision; documented, no consumer.)
//
// PARSE (decimal string -> f64): the strtod fast path -- accumulate the significand D
// (<= 18 digits) into an i64, track the decimal exponent k, then x = (f64)D * 10^k (or
// / 10^-k) via an exact power-of-ten multiply. Correctly rounded for |k| <= 22 and
// D < 2^53, which covers every corpus value, so parseFloat's double == strtod's on the
// other backends. Huge exponents overflow to inf / underflow to 0 (sane, untested).
//
// MEMORY: the float ops use a dedicated 64 KiB scratch window $D6FLT at [1968128,2033664)
// (the reserved-region top, above the Task-6 foldDir windows). $hp is bumped to 2033664 in
// wasm_runtime.go so the heap never overwrites it (new windows above existing ones, bump
// $hp, document -- the ledger convention). Sub-layout within $D6FLT: [+0,+12) getFloat
// fd_read iovec+nread cells, [+16,+59016) getFloat stdin line buffer, [+60000,...)
// printFloat format-output buffer, [+61000,...) the format's internal digit string.

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
  ;; $flt_pow10i: 10^k as i64 (k in [0,18]; 10^18 < 2^63).
  (func $flt_pow10i (param $k i32) (result i64)
    (local $r i64)
    (local.set $r (i64.const 1))
    (block $b (loop $l (br_if $b (i32.le_s (local.get $k) (i32.const 0)))
      (local.set $r (i64.mul (local.get $r) (i64.const 10)))
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
    (local.set $x (f64.convert_i64_u (local.get $D)))
    (if (i32.ge_s (local.get $k) (i32.const 0))
      (then (local.set $x (f64.mul (local.get $x) (call $flt_pow10 (local.get $k)))))
      (else (local.set $x (f64.div (local.get $x) (call $flt_pow10 (i32.sub (i32.const 0) (local.get $k)))))))
    (if (local.get $neg) (then (local.set $x (f64.neg (local.get $x)))))
    (local.get $x))

  ;; $flt_fmt: write the ECMAScript Number::toString(10) shortest-round-trip rendering of
  ;; $x into $out (no trailing NUL); returns the byte length. The precision search finds
  ;; the fewest digits that round-trip, then the four dressing cases dress them.
  (func $flt_fmt (param $x f64) (param $out i32) (result i32)
    (local $neg i32) (local $E i32) (local $p i32) (local $N i64) (local $j i32)
    (local $y f64) (local $recon f64) (local $t f64) (local $o i32) (local $i i32)
    (local $sdig i32) (local $n i32) (local $div i64) (local $en i32) (local $found i32)
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
    (local.set $sdig (i32.add (global.get $D6FLT) (i32.const 61000)))
    (local.set $p (i32.const 1)) (local.set $found (i32.const 0))
    (block $pb (loop $pl
      (br_if $pb (i32.gt_s (local.get $p) (i32.const 17)))
      ;; E = floor(log10(x)) via an incremental exact power of ten
      (local.set $E (i32.const 0)) (local.set $t (f64.const 1))
      (if (f64.ge (local.get $x) (f64.const 1))
        (then (block $eb1 (loop $el1
          (br_if $eb1 (f64.gt (f64.mul (local.get $t) (f64.const 10)) (local.get $x)))
          (local.set $t (f64.mul (local.get $t) (f64.const 10)))
          (local.set $E (i32.add (local.get $E) (i32.const 1)))
          (br $el1))))
        (else (block $eb2 (loop $el2
          (br_if $eb2 (f64.le (local.get $t) (local.get $x)))
          (local.set $t (f64.div (local.get $t) (f64.const 10)))
          (local.set $E (i32.sub (local.get $E) (i32.const 1)))
          (br $el2)))))
      (local.set $j (i32.add (i32.sub (local.get $E) (local.get $p)) (i32.const 1)))
      (if (i32.ge_s (local.get $j) (i32.const 0))
        (then (local.set $y (f64.div (local.get $x) (call $flt_pow10 (local.get $j)))))
        (else (local.set $y (f64.mul (local.get $x) (call $flt_pow10 (i32.sub (i32.const 0) (local.get $j)))))))
      (local.set $N (i64.trunc_f64_s (f64.nearest (local.get $y))))
      (if (i64.ge_s (local.get $N) (call $flt_pow10i (local.get $p)))
        (then
          (local.set $N (i64.div_s (local.get $N) (i64.const 10)))
          (local.set $E (i32.add (local.get $E) (i32.const 1)))
          (local.set $j (i32.add (i32.sub (local.get $E) (local.get $p)) (i32.const 1)))))
      (local.set $recon (f64.convert_i64_s (local.get $N)))
      (if (i32.ge_s (local.get $j) (i32.const 0))
        (then (local.set $recon (f64.mul (local.get $recon) (call $flt_pow10 (local.get $j)))))
        (else (local.set $recon (f64.div (local.get $recon) (call $flt_pow10 (i32.sub (i32.const 0) (local.get $j)))))))
      (if (f64.eq (local.get $recon) (local.get $x)) (then (local.set $found (i32.const 1)) (br $pb)))
      (local.set $p (i32.add (local.get $p) (i32.const 1)))
      (br $pl)))
    (if (i32.eqz (local.get $found)) (then (local.set $p (i32.const 17))))
    ;; extract p decimal digits of N (MSD first) into sdig
    (local.set $div (call $flt_pow10i (i32.sub (local.get $p) (i32.const 1))))
    (local.set $i (i32.const 0))
    (block $db (loop $dl
      (br_if $db (i32.ge_s (local.get $i) (local.get $p)))
      (i32.store8 (i32.add (local.get $sdig) (local.get $i))
        (i32.add (i32.wrap_i64 (i64.rem_u (i64.div_u (local.get $N) (local.get $div)) (i64.const 10))) (i32.const 48)))
      (local.set $div (i64.div_u (local.get $div) (i64.const 10)))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br $dl)))
    (local.set $n (i32.add (local.get $E) (i32.const 1)))
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
	for _, op := range []string{"Float", "fromNat", "fmul", "parseFloat", "getFloat", "printFloat"} {
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
