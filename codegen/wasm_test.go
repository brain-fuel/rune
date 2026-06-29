package codegen_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"goforge.dev/rune/v3/codegen"
)

// wasm_test.go — the NINTH backend (R-NATIVE Cranelift/WASM node) conformance gate.
// The WASM backend emits a SELF-CONTAINED WebAssembly text module (WAT) — program logic
// + an inline runtime (linear-memory bump allocator, heap closures/constructors, a
// function table for call_indirect, a naive base-1e9 bignum, and a `$show` writer over
// WASI fd_write) — that `wasmtime run module.wat` executes directly. The gate is
// byte-identity with the other backends' observable `$show` on the supported subset:
// nat arithmetic, recursive ListElim, a multi-field constructor, native literals, and
// accel arithmetic.

// wasmtimePath resolves the wasmtime binary: the user's ~/.wasmtime/bin/wasmtime (where
// it is installed here) or a PATH wasmtime. Empty string ⇒ not found.
func wasmtimePath() string {
	if home, err := os.UserHomeDir(); err == nil {
		cand := filepath.Join(home, ".wasmtime", "bin", "wasmtime")
		if _, err := os.Stat(cand); err == nil {
			return cand
		}
	}
	if p, err := exec.LookPath("wasmtime"); err == nil {
		return p
	}
	return ""
}

// runWasm writes the WAT to a temp file and runs it with `wasmtime run`, returning
// trimmed stdout. Skips gracefully (testing.Short or no wasmtime) so CI without the wasm
// toolchain stays green.
func runWasm(t *testing.T, wat string) string {
	t.Helper()
	if testing.Short() {
		t.Skip("skipping wasmtime run in -short mode")
	}
	wt := wasmtimePath()
	if wt == "" {
		t.Skip("wasmtime not found (~/.wasmtime/bin/wasmtime or PATH)")
	}
	dir := t.TempDir()
	f := dir + "/module.wat"
	if err := os.WriteFile(f, []byte(wat), 0o644); err != nil {
		t.Fatal(err)
	}
	out, err := exec.Command(wt, "run", f).CombinedOutput()
	if err != nil {
		t.Fatalf("wasmtime: %v\n%s\n--- emitted .wat ---\n%s", err, out, wat)
	}
	return strings.TrimSpace(string(out))
}

// TestWasmEmitAndRunNat pins native nat arithmetic on the WASM backend: zero/succ/
// NatElim become the bignum rep + a fold loop, add computes the same successor chain the
// other backends show — emitted as WAT and run by wasmtime.
func TestWasmEmitAndRunNat(t *testing.T) {
	src := natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`
	if got := runWasm(t, emitWith(t, codegen.Wasm{}, src, "three")); got != "succ (succ (succ zero))" {
		t.Fatalf("wasm nat: got %q", got)
	}
}

// TestWasmEmitListLength is the ninth-backend keystone: a recursive ListElim (lowered
// through the shared IR, closure-converted, then emitted as WAT functions over a
// {code_idx,env} closure runtime) computes the same fold as every other backend, run by
// wasmtime.
func TestWasmEmitListLength(t *testing.T) {
	src := natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	if got := runWasm(t, emitWith(t, codegen.Wasm{}, src, "two")); got != "succ (succ zero)" {
		t.Fatalf("wasm list length: got %q", got)
	}
}

// TestWasmNatLitAndAccelRun pins C7 / R-NUM on the WASM backend: compressed numerals
// deploy as base-1e9 bignums (parsed once) and the accel add/mul/monus become runtime
// native bignum arithmetic — byte-identical to the eliminator-loop result (incl. the
// saturating Peano monus 0-floor monusN 3 5 = 0).
func TestWasmNatLitAndAccelRun(t *testing.T) {
	for _, tc := range []struct{ src, main, want string }{
		{bigNatSrc, "big", "5000"},
		{bigNatSrc, "bigger", "5002"},
		{bigNatSrc, "product", "10000"},
		{accelNatSrc, "sum", "8000"},
		{accelNatSrc, "prod", "10000"},
		{accelNatSrc, "diff", "5"},
		{accelNatSrc, "diffZero", "0"},
	} {
		if got := runWasm(t, emitWith(t, codegen.Wasm{}, tc.src, tc.main)); got != tc.want {
			t.Fatalf("wasm %s: got %q, want %q", tc.main, got, tc.want)
		}
	}
}

// TestWasmConformsToJS is the CROSS-BACKEND conformance gate: the WASM backend's
// observable $show output is byte-identical to the JS backend (the reference) on the
// supported corpus subset — nat arithmetic, recursive ListElim, a multi-field
// constructor, native literals, and accel arithmetic. The same checked program observes
// the same value on the ninth target, run by wasmtime.
func TestWasmConformsToJS(t *testing.T) {
	const listSrc = natSrc + `
data List : U -> U is
  nil : (A : U) -> List A
| cons : (A : U) -> A -> List A -> List A
end
length : (A : U) -> List A -> Nat is
  fn (A : U) (xs : List A) is
    ListElim A (fn (x : List A) is Nat end)
      zero
      (fn (x : A) (rest : List A) (ih : Nat) is succ ih end)
      xs
  end
end
two : Nat is length Nat (cons Nat zero (cons Nat (succ zero) (nil Nat))) end
`
	const pairSrc = natSrc + `
data Pairing : U -> U -> U is
  mk : (A : U) -> (B : U) -> A -> B -> Pairing A B
end
p : Pairing Nat Nat is mk Nat Nat (succ zero) (succ (succ zero)) end
`
	cases := []struct{ src, main string }{
		{natSrc + `three : Nat is add (succ zero) (succ (succ zero)) end`, "three"},
		{listSrc, "two"},
		{pairSrc, "p"},
		{bigNatSrc, "big"},
		{bigNatSrc, "bigger"},
		{bigNatSrc, "product"},
		{accelNatSrc, "sum"},
		{accelNatSrc, "prod"},
		{accelNatSrc, "diff"},
		{accelNatSrc, "diffZero"},
	}
	for _, tc := range cases {
		jsOut := runNode(t, emitJS(t, tc.src, tc.main))
		wasmOut := runWasm(t, emitWith(t, codegen.Wasm{}, tc.src, tc.main))
		if jsOut != wasmOut {
			t.Fatalf("%s: WASM output %q != JS reference %q", tc.main, wasmOut, jsOut)
		}
	}
}

// TestWasmRejectsUnsupported is the HONESTY gate: the v1 WASM backend returns a CLEAR
// error for a form it cannot lower (a foreign axiom, a string/ptr literal, an IO main)
// rather than emitting broken WAT. This is a pure emit-time check (no wasmtime needed),
// so it runs even under -short.
func TestWasmRejectsUnsupported(t *testing.T) {
	cases := []struct {
		name string
		p    codegen.Program
		want string
	}{
		{"foreign", codegen.Program{
			Defs: []codegen.DefSpec{{Name: "main", Body: codegen.IForeign{Name: "id"}}}, Main: "main",
		}, "foreign"},
		{"str", codegen.Program{
			Defs: []codegen.DefSpec{{Name: "main", Body: codegen.ILit{Kind: codegen.LitStr, Str: "hi"}}}, Main: "main",
		}, "string"},
		{"ptr", codegen.Program{
			Defs: []codegen.DefSpec{{Name: "main", Body: codegen.ILit{Kind: codegen.LitPtr, Int: 1}}}, Main: "main",
		}, "pointer"},
		{"iomain", codegen.Program{
			Defs: []codegen.DefSpec{{Name: "main", Body: codegen.IUnit{}}}, Main: "main", IOMain: true,
		}, "IO main"},
	}
	for _, tc := range cases {
		_, err := codegen.Wasm{}.Emit(tc.p)
		if err == nil {
			t.Fatalf("%s: expected an error, got none", tc.name)
		}
		if !strings.Contains(err.Error(), tc.want) {
			t.Fatalf("%s: error %q does not mention %q", tc.name, err.Error(), tc.want)
		}
	}
}

// wasmTestModule wraps the bare runtime body (codegen.WasmRuntime()) into a standalone
// module for a synthetic runtime test: it adds the fd_write import, the apply type, a
// 1-slot table, and STUBS the three program-emitted globals the runtime references
// ($abort_msg/$abort_len/$fn_msg, normally emitted by emitData). startBody is spliced
// as the body of a $main exported as _start (wasmtime invokes the WASI _start export).
func wasmTestModule(runtimeBody, startBody string) string {
	// The runtime body LEADS with its own fd_write import (WAT requires all imports
	// first), so the test-only definitions ($codety, the table, the program-emitted
	// global stubs $abort_msg/$abort_len/$fn_msg, and $main) follow it.
	return `(module
` + runtimeBody + `
  (type $codety (func (param i32 i32) (result i32)))
  (table 1 funcref)
  (global $abort_msg i32 (i32.const 32))
  (global $abort_len i32 (i32.const 5))
  (global $fn_msg i32 (i32.const 64))
  (func $main (export "_start")
` + startBody + `))`
}

// TestWasmBounceRuntimeRoundTrip pins the Task-1 runtime: a 0-arg K_BOUNCE of an
// immediate-int step tramps to a non-bounce and shell-frees, leaving $rt_live at the
// pre-bounce baseline (the shell was reclaimed, the borrowed step is an immediate).
func TestWasmBounceRuntimeRoundTrip(t *testing.T) {
	rt := codegen.WasmRuntime()
	for _, fn := range []string{"$rt_mkbounce", "$rt_tramp", "$rt_bounce_free_shell", "$rt_bounce_set"} {
		if !strings.Contains(rt, fn) {
			t.Fatalf("runtime missing %s", fn)
		}
	}
	start := `    (local $b i32) (local $r i32) (local $base i32)
    (local.set $base (global.get $live))
    (local.set $b (call $rt_mkbounce (call $rt_mkint (i32.const 7)) (i32.const 0)))
    (local.set $r (call $rt_tramp (local.get $b)))
    (if (i32.eqz (call $is_int (local.get $r))) (then unreachable))
    (call $rt_print_u32 (i32.sub (global.get $live) (local.get $base)))`
	if got := runWasm(t, wasmTestModule(rt, start)); strings.TrimSpace(got) != "0" {
		t.Fatalf("bounce round-trip live delta = %q, want 0", got)
	}
}

// TestWasmBounceUnforcedRelease: a K_BOUNCE built then released WITHOUT tramp (the
// totality path) frees its owned args exactly once via rt_free's K_BOUNCE branch --
// live returns to baseline, no double-free of the arg, the immediate-int step untouched.
// (Reuses wasmTestModule.)
func TestWasmBounceUnforcedRelease(t *testing.T) {
	start := `    (local $arg i32) (local $b i32) (local $base i32)
    (local.set $base (global.get $live))
    (local.set $arg (call $rt_mkcon (i32.const 0) (i32.const 0) (i32.const 0))) ;; one owned K_CON, 0 fields
    (local.set $b (call $rt_mkbounce (call $rt_mkint (i32.const 1)) (i32.const 1)))
    (call $rt_bounce_set (local.get $b) (i32.const 0) (local.get $arg))
    ;; never tramp: release the whole bounce. rt_free's K_BOUNCE branch releases the arg.
    (call $rt_release (local.get $b))
    (call $rt_print_u32 (i32.sub (global.get $live) (local.get $base)))`
	if got := runWasm(t, wasmTestModule(codegen.WasmRuntime(), start)); strings.TrimSpace(got) != "0" {
		t.Fatalf("unforced-bounce release live delta = %q, want 0 (arg freed once via full-free)", got)
	}
}
