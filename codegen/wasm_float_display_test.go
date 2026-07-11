package codegen

// wasm_float_display_test.go -- Task 3 (atof half): the executable Go spec of the
// correctly-rounded bignum decimal->f64 parser the WAT $flt_atof slow path implements.
//
// atofRef is the SAME algorithm the WAT port runs, expressed with math/big but
// RESTRICTED to the operations the WASM runtime actually provides (multiply, add,
// saturating subtract, compare, shift-by-power-of-two, build 10^k). It does NOT
// delegate to strconv; strconv is only the oracle. TestAtofBigMatchesStrconv pins
// atofRef bit-for-bit against strconv.ParseFloat over the plan's edge cases plus a
// deterministic 20000-input random sweep, so a green run is the correctness proof of
// the algorithm before it is transcribed into WAT.

import (
	"errors"
	"fmt"
	"math"
	"math/big"
	"math/rand"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// atofSweepSeed is the fixed PRNG seed for the random sweep. The sandbox forbids
// nondeterministic entropy, so the sweep is reproducible run-to-run and across the Go
// spec and any re-derivation of the WAT port.
const atofSweepSeed = 0x0f107ab3

// ---- the division-free correctly-rounded decimal->f64 algorithm (atofRef) ----
//
// Value V = Dfull * 10^k (Dfull the integer formed by all mantissa digits, k the net
// decimal exponent). Write V = NUM/DEN exactly (k>=0: NUM=Dfull*10^k, DEN=1; k<0:
// NUM=Dfull, DEN=10^-k). The nearest f64 is significand q * 2^e2 with q a 53-bit integer
// (normal) or q<2^52 with e2=-1074 (subnormal). We find e2 by shift-and-compare, build
// q=floor(V/2^e2) one bit at a time (each bit kept iff cand*B<=A, division-free), then
// round to nearest-even from the exact remainder (compare 2*rem to B). Overflow -> +Inf,
// underflow -> +0. Sign is applied by the caller. No bignum/bignum division is used --
// only multiply, add, saturating subtract, compare, and shift-by-power-of-two, exactly
// the WASM runtime's $rt_nat_mul / $big_add / $rt_nat_monus / $big_cmp / (shift = multiply
// by a power-of-two bignum) primitives.

// atofRef parses a validated decimal float literal (the grammar the WAT $flt_validate
// accepts) to the correctly-rounded f64.
func atofRef(s string) float64 {
	neg, dfull, k, ok := atofParseDecimal(s)
	if !ok {
		return math.NaN()
	}
	if dfull.Sign() == 0 {
		if neg {
			return math.Copysign(0, -1)
		}
		return 0
	}
	var num, den *big.Int
	if k >= 0 {
		num = new(big.Int).Mul(dfull, atofPow10(k))
		den = big.NewInt(1)
	} else {
		num = new(big.Int).Set(dfull)
		den = atofPow10(-k)
	}
	bits := atofRoundBits(num, den)
	f := math.Float64frombits(bits)
	if neg {
		f = -f
	}
	return f
}

// atofParseDecimal splits a validated literal into sign, the integer Dfull of all mantissa
// digits, and the net decimal exponent k so that |value| = Dfull * 10^k. It mirrors the
// digit-gathering the WAT slow path performs over [buf,buf+len).
func atofParseDecimal(s string) (neg bool, dfull *big.Int, k int, ok bool) {
	dfull = new(big.Int)
	i := 0
	n := len(s)
	if i < n && (s[i] == '+' || s[i] == '-') {
		neg = s[i] == '-'
		i++
	}
	ten := big.NewInt(10)
	sawDigit := false
	frac := 0
	dot := false
	for i < n {
		c := s[i]
		if c >= '0' && c <= '9' {
			dfull.Mul(dfull, ten)
			dfull.Add(dfull, big.NewInt(int64(c-'0')))
			sawDigit = true
			if dot {
				frac++
			}
			i++
			continue
		}
		if c == '.' && !dot {
			dot = true
			i++
			continue
		}
		break
	}
	if !sawDigit {
		return false, nil, 0, false
	}
	exp := 0
	esign := 1
	if i < n && (s[i] == 'e' || s[i] == 'E') {
		i++
		if i < n && (s[i] == '+' || s[i] == '-') {
			if s[i] == '-' {
				esign = -1
			}
			i++
		}
		sawExp := false
		for i < n {
			c := s[i]
			if c < '0' || c > '9' {
				break
			}
			exp = exp*10 + int(c-'0')
			sawExp = true
			i++
		}
		if !sawExp {
			return false, nil, 0, false
		}
	}
	if i != n {
		return false, nil, 0, false
	}
	k = esign*exp - frac
	return neg, dfull, k, true
}

// atofPow10 returns 10^n as a big.Int (n >= 0). The WAT builds the same value directly in
// base-1e9 limbs; the RESULT is identical.
func atofPow10(n int) *big.Int {
	return new(big.Int).Exp(big.NewInt(10), big.NewInt(int64(n)), nil)
}

// atofRoundBits returns the IEEE-754 bit pattern of the non-negative f64 nearest num/den
// (round to nearest, ties to even). num, den > 0.
func atofRoundBits(num, den *big.Int) uint64 {
	const infBits = 0x7FF0000000000000
	// Cheap magnitude screen so the exponent search stays O(1) iterations and no
	// astronomically large shift is built for clear over/underflow.
	binexp := num.BitLen() - den.BitLen() // within +-1 of floor(log2(V))
	if binexp >= 1027 {
		return infBits
	}
	if binexp <= -1077 {
		return 0
	}
	// ge(e2,p): is V / 2^e2 >= 2^p ? i.e. NUM >= 2^(p+e2) * DEN, checked with a single
	// power-of-two shift on whichever side keeps both operands integral.
	ge := func(e2, p int) bool {
		sh := p + e2
		var l, r *big.Int
		if sh >= 0 {
			l = num
			r = new(big.Int).Lsh(den, uint(sh))
		} else {
			l = new(big.Int).Lsh(num, uint(-sh))
			r = den
		}
		return l.Cmp(r) >= 0
	}
	// Seed e2 from the bit-length estimate, then adjust so 2^52 <= V/2^e2 < 2^53.
	e2 := binexp - 53
	for ge(e2, 53) {
		e2++
	}
	for !ge(e2, 52) {
		e2--
	}
	if e2 < -1074 {
		e2 = -1074 // clamp into the subnormal grid
	}
	// A/B = V / 2^e2 as exact integers (A=NUM<<max(0,-e2), B=DEN<<max(0,e2)).
	var a, b *big.Int
	if e2 >= 0 {
		a = new(big.Int).Set(num)
		b = new(big.Int).Lsh(den, uint(e2))
	} else {
		a = new(big.Int).Lsh(num, uint(-e2))
		b = new(big.Int).Set(den)
	}
	// q = floor(A/B), built MSB-first; kept bit j iff (q|2^j)*B <= A. 0 <= q < 2^53.
	q := int64(0)
	for j := 52; j >= 0; j-- {
		cand := q | (int64(1) << uint(j))
		t := new(big.Int).Mul(big.NewInt(cand), b)
		if t.Cmp(a) <= 0 {
			q = cand
		}
	}
	// Round to nearest even from the exact remainder: compare 2*rem to B.
	qb := new(big.Int).Mul(big.NewInt(q), b)
	rem := new(big.Int).Sub(a, qb)
	twoRem := new(big.Int).Lsh(rem, 1)
	c := twoRem.Cmp(b)
	if c > 0 || (c == 0 && q&1 == 1) {
		q++
	}
	// Rounding carry out of the 53-bit significand bumps the exponent.
	if q == (int64(1) << 53) {
		q >>= 1
		e2++
	}
	if q == 0 {
		return 0
	}
	if q >= (int64(1) << 52) {
		e := e2 + 1075 // biased exponent
		if e >= 2047 {
			return infBits
		}
		return (uint64(e) << 52) | (uint64(q) - (uint64(1) << 52))
	}
	// subnormal: biased exponent 0, q is the 52-bit fraction directly.
	return uint64(q)
}

// ---- the differential test ----

func TestAtofBigMatchesStrconv(t *testing.T) {
	edge := []string{
		// over/underflow boundaries
		"1e309", "1.7976931348623157e308", "1.7976931348623159e308",
		"1e308", "1e-308", "2.2250738585072014e-308", "2.2250738585072009e-308",
		// subnormals down to the minimum
		"1e-320", "1e-323", "4.9e-324", "5e-324", "2.4703282292062327e-324",
		"1.5e-323", "2.5e-323",
		// power-of-ten fast/slow boundary (|k| just past 22)
		"1.1e22", "1.1e23", "1e22", "1e23", "1e-22", "1e-23",
		// values near 2^53
		"9007199254740992", "9007199254740993", "9007199254740994",
		"9007199254740995", "18014398509481984", "18014398509481985",
		// 16/17 significant digits
		"123456789012345678e-5", "0.30000000000000004", "0.1", "0.2", "0.3",
		"1234567890123456789", "9999999999999999", "12345678901234567",
		// extreme exponents (well past the double range)
		"1e1000", "1e-1000", "1.23456789e-999", "9.87654321e999",
		// tie / rounding stressors
		"0.5", "2.5", "0.125", "1.0000000000000002", "0.9999999999999999",
		// zeros and signs
		"0", "-0", "0.0", "-0.0", "0e10", "-1.5e-300",
	}
	for _, s := range edge {
		want, err := strconv.ParseFloat(s, 64)
		// ErrRange still yields the correctly-rounded value (Inf on overflow, 0 or a
		// subnormal on underflow); only a syntax error means the input is unusable.
		if err != nil && !errors.Is(err, strconv.ErrRange) {
			t.Fatalf("oracle rejected edge input %q: %v", s, err)
		}
		got := atofRef(s)
		if math.Float64bits(got) != math.Float64bits(want) {
			t.Fatalf("atofRef(%q) bits=%016x (%v) want %016x (%v)",
				s, math.Float64bits(got), got, math.Float64bits(want), want)
		}
	}

	r := rand.New(rand.NewSource(atofSweepSeed))
	const sweep = 20000
	tested := 0
	for i := 0; i < sweep; i++ {
		s := randDecimalString(r, i)
		want, err := strconv.ParseFloat(s, 64)
		if err != nil && !errors.Is(err, strconv.ErrRange) {
			continue // syntactically unusable for Go too -- skip
		}
		got := atofRef(s)
		if math.Float64bits(got) != math.Float64bits(want) {
			t.Fatalf("sweep[%d] atofRef(%q) bits=%016x (%v) want %016x (%v)",
				i, s, math.Float64bits(got), got, math.Float64bits(want), want)
		}
		tested++
	}
	if tested < sweep/2 {
		t.Fatalf("sweep exercised only %d/%d inputs (generator too often out of range)", tested, sweep)
	}
	t.Logf("atofRef matched strconv on %d edge + %d/%d swept inputs (seed %#x)",
		len(edge), tested, sweep, atofSweepSeed)
}

// randDecimalString produces a valid decimal float literal (the WAT $flt_validate grammar)
// with an index-varied magnitude regime so the sweep covers subnormals, the normal range,
// the over/underflow boundaries, and extreme exponents deterministically.
func randDecimalString(r *rand.Rand, idx int) string {
	var b strings.Builder
	if r.Intn(2) == 0 {
		b.WriteByte('-')
	}
	// mantissa: 1..19 digits, first nonzero, an optional decimal point somewhere inside.
	nd := 1 + r.Intn(19)
	digits := make([]byte, nd)
	digits[0] = byte('1' + r.Intn(9))
	for i := 1; i < nd; i++ {
		digits[i] = byte('0' + r.Intn(10))
	}
	dotAt := -1
	if r.Intn(3) != 0 {
		dotAt = r.Intn(nd + 1) // 0..nd (0 => ".ddd", nd => "ddd.")
	}
	for i := 0; i < nd; i++ {
		if i == dotAt {
			b.WriteByte('.')
		}
		b.WriteByte(digits[i])
	}
	if dotAt == nd {
		b.WriteByte('.')
	}
	// exponent regime, biased across the interesting ranges.
	var exp int
	switch (idx + r.Intn(7)) % 7 {
	case 0: // subnormal / underflow neighbourhood
		exp = -340 + r.Intn(40)
	case 1: // overflow neighbourhood
		exp = 290 + r.Intn(40)
	case 2: // deep underflow (mostly rounds to 0)
		exp = -1000 + r.Intn(650)
	case 3: // deep overflow (mostly inf)
		exp = 320 + r.Intn(700)
	case 4: // just around the |k|<=22 fast/slow seam
		exp = -30 + r.Intn(60)
	default: // ordinary normal range
		exp = -200 + r.Intn(400)
	}
	if r.Intn(4) != 0 { // usually attach an explicit exponent
		b.WriteByte('e')
		if exp >= 0 {
			if r.Intn(2) == 0 {
				b.WriteByte('+')
			}
		}
		b.WriteString(strconv.Itoa(exp))
	}
	return b.String()
}

// ---- formatter-independent WAT-level acceptance: the $flt_atof_slow port ----

// wasmtimeForTest resolves the wasmtime binary (~/.wasmtime/bin/wasmtime or PATH), or ""
// if absent (the test then skips). Mirrors the codegen_test wasmtimePath helper, inlined
// so this internal-package test stays self-contained.
func wasmtimeForTest() string {
	if home, err := os.UserHomeDir(); err == nil {
		p := filepath.Join(home, ".wasmtime", "bin", "wasmtime")
		if _, err := os.Stat(p); err == nil {
			return p
		}
	}
	if p, err := exec.LookPath("wasmtime"); err == nil {
		return p
	}
	return ""
}

// atofSlowModule wraps the base + float WAT runtimes with the module scaffolding and a
// $_start that writes `s` into linear memory, calls $flt_atof_slow, and prints the raw
// f64 result bits as "HI,LO" (two u32 halves) -- NO formatter, so the parsed double is
// observed directly and exactly.
func atofSlowModule(s string) string {
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(WasmRuntime())
	b.WriteString(wasmFloatRuntime)
	b.WriteString("  (global $fn_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_len i32 (i32.const 0))\n")
	b.WriteString("  (global $unit_name i32 (i32.const 32))\n")
	b.WriteString("  (table 1 funcref)\n")
	b.WriteString("  (func $_start (export \"_start\")\n")
	b.WriteString("    (local $x f64) (local $bits i64)\n")
	b.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (i32.const 32) (i32.const 0)))\n")
	const at = 40000 // scratch address inside the reserved region, clear of the heap
	for i := 0; i < len(s); i++ {
		fmt.Fprintf(&b, "    (i32.store8 (i32.const %d) (i32.const %d))\n", at+i, s[i])
	}
	fmt.Fprintf(&b, "    (local.set $x (call $flt_atof_slow (i32.const %d) (i32.const %d)))\n", at, len(s))
	b.WriteString("    (local.set $bits (i64.reinterpret_f64 (local.get $x)))\n")
	b.WriteString("    (call $rt_print_u32 (i32.wrap_i64 (i64.shr_u (local.get $bits) (i64.const 32))))\n")
	b.WriteString("    (i32.store8 (i32.const 4096) (i32.const 44))\n") // ','
	b.WriteString("    (call $puts (i32.const 1) (i32.const 4096) (i32.const 1))\n")
	b.WriteString("    (call $rt_print_u32 (i32.wrap_i64 (local.get $bits)))\n")
	b.WriteString("  )\n)\n")
	return b.String()
}

// ---- Task 4 (format half): the executable Go spec of the exact Dragon4 formatter ----
//
// dragon4Digits is the Steele-White FPP2 free-format shortest algorithm the WAT $flt_dragon4
// port implements. It works over exact bignum rationals R/S with high/low margins Mp/Mm and
// emits the fewest decimal digits that round back to x, correctly rounded (ties to even). It
// composes ONLY the operations the WASM runtime provides: multiply ($rt_nat_mul), add
// ($big_add), compare ($big_cmp), subtract-when-not-smaller (mirrors $rt_nat_monus, used for
// the per-digit quotient by REPEATED SUBTRACTION -- there is no general bignum division in
// the runtime), and power-of-two shift ($flt_shl2 / $flt_pow2big). It does NOT delegate to
// strconv. It returns the shortest significant DIGITS and the position n where the value is
// digits * 10^(n - len(digits)).
//
// fmtSweepSeed is the fixed PRNG seed for the format sweep (sandbox forbids nondeterministic
// entropy, so the >=50000-double differential is reproducible run-to-run).
const fmtSweepSeed = 0x5eed4d16

// dragon4Digits: |x| = f * 2^e decomposed from its IEEE mantissa/exponent (mant the 52-bit
// fraction, expo the 11-bit biased exponent). Returns the shortest decimal digit string and
// the decimal position n (value = digits * 10^(n-len)).
func dragon4Digits(mant uint64, expo int) (string, int) {
	one := big.NewInt(1)
	ten := big.NewInt(10)
	var f *big.Int
	var e int
	if expo == 0 { // subnormal: f = mant, e = -1074
		f = new(big.Int).SetUint64(mant)
		e = -1074
	} else { // normal: f = mant | 2^52, e = expo - 1075
		f = new(big.Int).SetUint64(mant | (uint64(1) << 52))
		e = expo - 1075
	}
	even := mant&1 == 0                  // f's low bit == mant's low bit (2^52 is even)
	isBoundary := expo != 0 && mant == 0 // f == 2^52: unequal gaps at the power-of-two boundary

	R := new(big.Int)
	S := new(big.Int)
	Mp := new(big.Int)
	Mm := new(big.Int)
	if e >= 0 {
		be := new(big.Int).Lsh(one, uint(e)) // 2^e
		if !isBoundary {
			R.Mul(f, be)
			R.Lsh(R, 1) // R = f*be*2
			S.SetInt64(2)
			Mp.Set(be)
			Mm.Set(be)
		} else {
			R.Mul(f, be)
			R.Lsh(R, 2) // R = f*be*4
			S.SetInt64(4)
			Mp.Lsh(be, 1) // Mp = be*2
			Mm.Set(be)
		}
	} else {
		if expo <= 1 || !isBoundary {
			R.Lsh(f, 1)             // R = f*2
			S.Lsh(one, uint(1-e))   // S = 2^(1-e)
			Mp.SetInt64(1)
			Mm.SetInt64(1)
		} else {
			R.Lsh(f, 2)             // R = f*4
			S.Lsh(one, uint(2-e))   // S = 2^(2-e)
			Mp.SetInt64(2)
			Mm.SetInt64(1)
		}
	}

	// scale: choose k so the first emitted digit is in [1,9] and value = 0.d1d2... * 10^k.
	k := 0
	for {
		rpm := new(big.Int).Add(R, Mp)
		c := rpm.Cmp(S)
		if !(c > 0 || (even && c == 0)) { // R+Mp <= S (or < S when even)
			break
		}
		S.Mul(S, ten)
		k++
	}
	for {
		rpm := new(big.Int).Add(R, Mp)
		rpm.Mul(rpm, ten)
		c := rpm.Cmp(S)
		if !(c < 0 || (even && c == 0)) { // (R+Mp)*10 >= S
			break
		}
		R.Mul(R, ten)
		Mp.Mul(Mp, ten)
		Mm.Mul(Mm, ten)
		k--
	}

	// generate: emit digits until within a margin of a neighbouring representable value.
	var digs []int
	for {
		R.Mul(R, ten)
		Mp.Mul(Mp, ten)
		Mm.Mul(Mm, ten)
		d := 0 // d = floor(R/S) in 0..9 by repeated subtraction (no general division)
		for R.Cmp(S) >= 0 {
			R.Sub(R, S)
			d++
		}
		cLow := R.Cmp(Mm)
		low := cLow < 0 || (even && cLow == 0)
		rpm := new(big.Int).Add(R, Mp)
		cHigh := rpm.Cmp(S)
		high := cHigh > 0 || (even && cHigh == 0)
		if !low && !high {
			digs = append(digs, d)
			continue
		}
		roundUp := false
		switch {
		case low && !high:
			roundUp = false
		case high && !low:
			roundUp = true
		default: // both margins reached: pick the nearer, ties to even
			twoR := new(big.Int).Lsh(R, 1)
			c := twoR.Cmp(S)
			switch {
			case c < 0:
				roundUp = false
			case c > 0:
				roundUp = true
			default:
				roundUp = d&1 == 1 // exact tie -> even digit
			}
		}
		digs = append(digs, d)
		if roundUp {
			i := len(digs) - 1
			for i >= 0 {
				digs[i]++
				if digs[i] < 10 {
					break
				}
				digs[i] = 0
				i--
			}
			if i < 0 { // full carry: 0.99..9 * 10^k rounded to 1.0 * 10^k = 0.1 * 10^(k+1)
				digs = []int{1}
				k++
			}
		}
		break
	}
	// strip trailing zeros (only a round-up carry can introduce them; shortest never needs them)
	for len(digs) > 1 && digs[len(digs)-1] == 0 {
		digs = digs[:len(digs)-1]
	}
	buf := make([]byte, len(digs))
	for i, d := range digs {
		buf[i] = byte('0' + d)
	}
	return string(buf), k
}

// dragon4Ref renders x exactly as ECMAScript Number::toString(10) would, deriving the digits
// from dragon4Digits (NOT strconv) and dressing them with the four shared ECMAScript cases.
func dragon4Ref(x float64) string {
	if x != x {
		return "NaN"
	}
	if math.IsInf(x, 1) {
		return "Infinity"
	}
	if math.IsInf(x, -1) {
		return "-Infinity"
	}
	if x == 0 {
		return "0"
	}
	neg := math.Signbit(x)
	if neg {
		x = -x
	}
	bits := math.Float64bits(x)
	mant := bits & ((uint64(1) << 52) - 1)
	expo := int((bits >> 52) & 0x7ff)
	digits, n := dragon4Digits(mant, expo)
	out := dressEcma(digits, n)
	if neg {
		out = "-" + out
	}
	return out
}

// ecmaFloat is the INDEPENDENT oracle: it takes Go's shortest DIGITS (strconv 'e',-1) and
// applies the same four ECMAScript dressing cases. dragon4Ref and ecmaFloat agreeing over a
// 50000-double sweep is the correctness proof of the Dragon4 digit generation.
func ecmaFloat(x float64) string {
	if x != x {
		return "NaN"
	}
	if math.IsInf(x, 1) {
		return "Infinity"
	}
	if math.IsInf(x, -1) {
		return "-Infinity"
	}
	if x == 0 {
		return "0" // ECMAScript: -0 renders "0"
	}
	neg := math.Signbit(x)
	if neg {
		x = -x
	}
	s, n := parseEcmaDigits(strconv.AppendFloat(nil, x, 'e', -1, 64))
	out := dressEcma(s, n)
	if neg {
		out = "-" + out
	}
	return out
}

// parseEcmaDigits reads strconv's 'e' shortest form ("d.ddde+NN" / "de+NN") and returns the
// digit string (point removed) plus the position n where value = digits * 10^(n-len). Because
// the 'e' form always has exactly one digit before the point, n = exp+1 regardless of length.
func parseEcmaDigits(b []byte) (string, int) {
	s := string(b)
	ei := strings.IndexAny(s, "eE")
	exp, _ := strconv.Atoi(s[ei+1:])
	mant := strings.Replace(s[:ei], ".", "", 1)
	return mant, exp + 1
}

// dressEcma applies the four ECMAScript Number::toString dressing cases to a shortest digit
// string s and position n (value = s * 10^(n-len(s))). This is the SHARED dressing the WAT
// $flt_fmt performs; it must match byte-for-byte.
func dressEcma(s string, n int) string {
	p := len(s)
	switch {
	case p <= n && n <= 21: // integer: digits then (n-p) trailing zeros
		return s + strings.Repeat("0", n-p)
	case 0 < n && n < p: // point inside: digits[:n] "." digits[n:]
		return s[:n] + "." + s[n:]
	case -5 <= n && n <= 0: // leading zeros: "0." (-n) zeros then digits
		return "0." + strings.Repeat("0", -n) + s
	default: // exponential: d1 ["." d2..dp] "e" sign(n-1) |n-1|
		var b strings.Builder
		b.WriteByte(s[0])
		if p > 1 {
			b.WriteByte('.')
			b.WriteString(s[1:])
		}
		b.WriteByte('e')
		en := n - 1
		if en >= 0 {
			b.WriteByte('+')
		} else {
			b.WriteByte('-')
			en = -en
		}
		b.WriteString(strconv.Itoa(en))
		return b.String()
	}
}

// TestEcmaFloatKnown pins the oracle (and thus the dressing) against ECMAScript ground truth
// so a shared dressEcma bug cannot silently agree with dragon4Ref.
func TestEcmaFloatKnown(t *testing.T) {
	cases := []struct {
		x    float64
		want string
	}{
		{1234567, "1234567"},
		{100, "100"},
		{1000000, "1000000"},
		{0.1, "0.1"},
		{0.5, "0.5"},
		{1e21, "1e+21"},
		{1e-7, "1e-7"},
		{1e-6, "0.000001"},
		{5e-324, "5e-324"},
		{1.7976931348623157e308, "1.7976931348623157e+308"},
		{0.30000000000000004, "0.30000000000000004"},
		{123456789012345680, "123456789012345680"},
		{-1234567, "-1234567"},
		{math.Copysign(0, -1), "0"},
	}
	for _, c := range cases {
		if got := ecmaFloat(c.x); got != c.want {
			t.Errorf("ecmaFloat(%v)=%q want %q", c.x, got, c.want)
		}
	}
}

// TestDragon4MatchesEcma is the executable spec of the Dragon4 formatter: dragon4Ref (the
// algorithm the WAT implements) must equal ecmaFloat (the strconv-digit oracle) over the
// plan's edge cases plus a deterministic 50000-double sweep spanning the full range incl
// subnormals. A green run pins the algorithm before it is transcribed into WAT.
func TestDragon4MatchesEcma(t *testing.T) {
	edge := []float64{
		1234567, 100, 1000000, 1e21, 1e-7, 1e-6, 5e-324, 2.2250738585072014e-308,
		9007199254740992, 9007199254740994, 9007199254740993, 0.1, 0.2, 0.3,
		1.0 / 3.0, 2.0 / 3.0, 1.7976931348623157e308, 123456789012345.6,
		0.30000000000000004, 0.5, 2.5, 1.5, 1e308, 1e-308, 1e-323, 1e-322,
		123456789012345680, 1e100, 1e-100, 9999999999999998, 12345678901234567,
		1.0000000000000002, 0.9999999999999999, 4.9e-324, 3e-323, 9.5, 99.5,
	}
	for _, x := range edge {
		if got, want := dragon4Ref(x), ecmaFloat(x); got != want {
			t.Fatalf("dragon4Ref(%v [%016x])=%q want %q", x, math.Float64bits(x), got, want)
		}
	}
	r := rand.New(rand.NewSource(fmtSweepSeed))
	const target = 50000
	tested := 0
	for tested < target {
		bits := r.Uint64()
		x := math.Float64frombits(bits)
		if math.IsNaN(x) || math.IsInf(x, 0) {
			continue
		}
		if got, want := dragon4Ref(x), ecmaFloat(x); got != want {
			t.Fatalf("sweep dragon4Ref(%016x)=%q want %q", bits, got, want)
		}
		tested++
	}
	t.Logf("dragon4Ref matched ecmaFloat on %d edge + %d swept doubles (seed %#x)",
		len(edge), tested, fmtSweepSeed)
}

// ---- WAT-level acceptance: the $flt_fmt / $flt_dragon4 port under wasmtime ----

// fmtModule wraps the base + float WAT runtimes and a $_start that, for each f64 bit pattern,
// calls $flt_fmt into the float scratch window and prints the rendered string + '\n'. One
// wasmtime run emits every value newline-separated, exercising the ACTUAL WAT digit generator
// (not just the Go ref) at 16/17-digit + subnormal magnitudes.
func fmtModule(bitsList []uint64) string {
	var b strings.Builder
	b.WriteString("(module\n")
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	b.WriteString(WasmRuntime())
	b.WriteString(wasmFloatRuntime)
	b.WriteString("  (global $fn_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_msg i32 (i32.const 32))\n")
	b.WriteString("  (global $abort_len i32 (i32.const 0))\n")
	b.WriteString("  (global $unit_name i32 (i32.const 32))\n")
	b.WriteString("  (table 1 funcref)\n")
	b.WriteString("  (func $_start (export \"_start\")\n")
	b.WriteString("    (local $x f64) (local $len i32) (local $buf i32)\n")
	b.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (i32.const 32) (i32.const 0)))\n")
	b.WriteString("    (local.set $buf (i32.add (global.get $D6FLT) (i32.const 60000)))\n")
	for _, bits := range bitsList {
		fmt.Fprintf(&b, "    (local.set $x (f64.reinterpret_i64 (i64.const %d)))\n", int64(bits))
		b.WriteString("    (local.set $len (call $flt_fmt (local.get $x) (local.get $buf)))\n")
		b.WriteString("    (i32.store8 (i32.add (local.get $buf) (local.get $len)) (i32.const 10))\n")
		b.WriteString("    (call $puts (i32.const 1) (local.get $buf) (i32.add (local.get $len) (i32.const 1)))\n")
	}
	b.WriteString("  )\n)\n")
	return b.String()
}

// TestFltFmtWatMatchesEcma proves the compiled WAT $flt_fmt renders byte-for-byte the same as
// ecmaFloat over a broad value set incl 16/17-significant-digit values, subnormals, the
// over/underflow boundaries, negatives, and specials -- so the actual WAT (not only the Go
// ref) is verified at the magnitudes that used to lose precision. Skips without wasmtime.
func TestFltFmtWatMatchesEcma(t *testing.T) {
	wt := wasmtimeForTest()
	if wt == "" {
		t.Skip("wasmtime not found")
	}
	vals := []float64{
		// dressing-branch + boundary witnesses
		1234567, 100, 1000000, 1e21, 1e-7, 1e-6, 0.1, 0.2, 0.3, 0.5, 2.5, 1.5,
		1.0 / 3.0, 2.0 / 3.0, 0.30000000000000004, 123456789012345.6, 123456789012345680,
		// 16/17 significant digits (the (f64)N precision-loss regime the port removes)
		9007199254740992, 9007199254740993, 9007199254740994, 12345678901234567,
		1.0000000000000002, 0.9999999999999999, 9999999999999998,
		// subnormals + min/max magnitude boundaries
		5e-324, 4.9e-324, 3e-323, 1e-323, 1e-322, 2.2250738585072014e-308,
		1e308, 1.7976931348623157e308, 1e-308, 1e100, 1e-100, 9.5, 99.5,
		// negatives + specials
		-1234567, -0.1, -5e-324, math.Copysign(0, -1),
		math.Inf(1), math.Inf(-1), math.NaN(),
	}
	r := rand.New(rand.NewSource(fmtSweepSeed ^ 0x1))
	for len(vals) < 1200 {
		x := math.Float64frombits(r.Uint64())
		if math.IsNaN(x) || math.IsInf(x, 0) {
			continue
		}
		vals = append(vals, x)
	}
	bitsList := make([]uint64, len(vals))
	for i, x := range vals {
		bitsList[i] = math.Float64bits(x)
	}
	dir := t.TempDir()
	f := filepath.Join(dir, "fmt.wat")
	if err := os.WriteFile(f, []byte(fmtModule(bitsList)), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command(wt, "run", f).CombinedOutput()
	if err != nil {
		t.Fatalf("wasmtime: %v\n%s", err, out)
	}
	lines := strings.Split(strings.TrimRight(string(out), "\n"), "\n")
	if len(lines) != len(vals) {
		t.Fatalf("got %d output lines, want %d", len(lines), len(vals))
	}
	mismatch := 0
	for i, x := range vals {
		want := ecmaFloat(x)
		if lines[i] != want {
			mismatch++
			if mismatch <= 20 {
				t.Errorf("flt_fmt(%016x)=%q want %q", math.Float64bits(x), lines[i], want)
			}
		}
	}
	if mismatch == 0 {
		t.Logf("WAT $flt_fmt matched ecmaFloat on all %d values", len(vals))
	}
}

// TestFltAtofSlowWatBits proves the WAT $flt_atof_slow port produces bit-for-bit the same
// f64 as strconv.ParseFloat over a range of slow-path inputs (large exponents, subnormals,
// near-2^53, overflow to inf). It reads the raw result BITS, so it is independent of the
// (separately fixed) shortest formatter -- it isolates the parse. Skips without wasmtime.
func TestFltAtofSlowWatBits(t *testing.T) {
	wt := wasmtimeForTest()
	if wt == "" {
		t.Skip("wasmtime not found")
	}
	inputs := []string{
		"1e-42", "1.1e23", "1e42", "1e-100", "1e100", "1.5e300", "3e300",
		"7e-299", "4.5e123", "6.02e-260", "1.7e250", "1e-200", "1e200",
		"9007199254740993", "12345678901234567", "1.234567890123e45",
		"1.999999999999999e100", "1e300", "9e307", "5e-300",
		// subnormals (formatter-independent here) and boundaries
		"5e-324", "4.9e-324", "1e-323", "2.2250738585072014e-308", "1e-320",
		// overflow -> +inf, deep underflow -> 0
		"1e309", "1e1000", "1e-1000",
		// a couple of negatives (sign handled inside the slow path)
		"-1.5e300", "-7e-299",
	}
	for _, s := range inputs {
		out, err := func() (string, error) {
			dir := t.TempDir()
			f := filepath.Join(dir, "m.wat")
			if err := os.WriteFile(f, []byte(atofSlowModule(s)), 0o644); err != nil {
				return "", err
			}
			o, err := exec.Command(wt, "run", f).CombinedOutput()
			if err != nil {
				return "", fmt.Errorf("%v: %s", err, o)
			}
			return strings.TrimSpace(string(o)), nil
		}()
		if err != nil {
			t.Fatalf("input %q: run: %v", s, err)
		}
		halves := strings.SplitN(out, ",", 2)
		if len(halves) != 2 {
			t.Fatalf("input %q: bad output %q", s, out)
		}
		hi, err1 := strconv.ParseUint(halves[0], 10, 32)
		lo, err2 := strconv.ParseUint(halves[1], 10, 32)
		if err1 != nil || err2 != nil {
			t.Fatalf("input %q: parse halves %q: %v %v", s, out, err1, err2)
		}
		gotBits := hi<<32 | lo
		want, _ := strconv.ParseFloat(s, 64)
		if gotBits != math.Float64bits(want) {
			t.Errorf("input %q: WAT slow atof bits=%016x want %016x", s, gotBits, math.Float64bits(want))
		}
	}
}
