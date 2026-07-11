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
