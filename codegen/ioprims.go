package codegen

// D6 / R-EFFECT — the standard OS/IO primitive vocabulary, shipped WITH the
// compiler. A `foreign` axiom named one of these gets a real host body baked into
// the backend runtime, exactly as D5's OTP primitives get beamOTPRuntime — no test
// injection, so `rune run` executes them unaided. This is the directly-unblocked
// D6 slice: console out + the OS clock, over `Nat`/`IO` only (richer fs/net wait on
// D1's Result/IOError + B4's String/Ptr marshalling).
//
//	printNat     : Nat -> IO Nat   write a number to stdout (decimal), return it
//	getNat       : IO Nat          read a decimal number from stdin
//	timeNanos    : IO Nat          read the OS clock in nanoseconds (a native int)
//	readLineCode : IO Nat          read a line, return its PACKED bytes (B4): a
//	                               bignum, first byte least-significant, 0x01
//	                               sentinel on top — `bytes <code>` is then a String.
//
// IO A erases to a deferred world thunk, so each op respects that calling
// convention (js: a 0-arg thunk; py/go/rust: a unit-argument closure; beam: a
// `fun(_U) -> …`), matching each backend's pureIO/bindIO so the monad forces them
// the same way it forces any action. printNat takes its `Nat` argument first, then
// the world; timeNanos takes only the world.
var ioPrims = map[string]bool{
	"printNat":     true,
	"getNat":       true,
	"timeNanos":    true,
	"readLineCode": true,
	// D6 net/fs: env + file vocabulary, over the PACKED-String code (a bignum, B4).
	// Each takes/returns a bare `Nat` code (first byte LSB, 0x01 sentinel) — the
	// SAME representation `readLineCode` returns and `bytes`/`codeOf` wrap/unwrap on
	// the Rune side, so the host bodies never touch the constructor encoding. fs/net
	// FAILURE is surfaced as the EMPTY string (code 1), which the Rune side lifts
	// into ch212's `Result _ IOError` (the parseWhole pattern, ch213).
	"getEnvCode":    true, // getEnvCode    name        : Nat -> IO Nat
	"readFileCode":  true, // readFileCode  path        : Nat -> IO Nat  (1 = unreadable)
	"writeFileCode": true, // writeFileCode path content : Nat -> Nat -> IO Nat
	"printStrCode":  true, // printStrCode  s           : Nat -> IO Nat  (decode + println)
	// D6 argv + process: command-line arguments and process exit.
	"argCountCode": true, // argCountCode             : IO Nat          number of argv entries
	"argAtCode":    true, // argAtCode     i          : Nat -> IO Nat   argv[i] code (1 if oob)
	"exitWith":     true, // exitWith      code       : Nat -> IO Unit  terminate with status
	// D3 machine floats (f64) + the BLAS dot kernel — PURE host bodies (native f64
	// arithmetic), not IO. A Float is the host's native double; a comparison returns
	// a Nat (1/0) the Rune side cases into Bool (no host constructor).
	"fromNat":    true, // fromNat    n             : Nat -> Float
	"fadd":       true, // fadd/fsub/fmul/fdiv a b   : Float -> Float -> Float
	"fsub":       true,
	"fmul":       true,
	"fdiv":       true,
	"fabsP":      true, // fabsP      x             : Float -> Float (absolute value)
	"floatToNat": true, // floatToNat x             : Float -> Nat   (truncate toward zero)
	"fleqN":      true, // fleqN      a b           : Float -> Float -> Nat (1 if a<=b else 0)
	"dot2":       true, // dot2       a0 a1 b0 b1    : Float^4 -> Float (a0*b0 + a1*b1)
}

// fileEnvPrims are the D6 prims whose host body needs the packed-String codec
// (decode a code to a host string, encode a host string to a code). A backend
// emits the shared codec helpers once when any of these is referenced.
var fileEnvPrims = []string{"getEnvCode", "readFileCode", "writeFileCode", "printStrCode", "argAtCode"}

// usesOS reports whether the program references any D6 prim needing the host OS
// module (the file/env codec users plus argv/process), so the Go backend knows to
// add the "os" import.
func usesOS(p Program) bool {
	return usesFileEnv(p) || usesForeign(p, "argCountCode") || usesForeign(p, "exitWith")
}

// usesFileEnv reports whether the program references any D6 net/fs primitive, so a
// backend knows to emit the shared packed-String codec helpers.
func usesFileEnv(p Program) bool {
	for _, n := range fileEnvPrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}

// usesForeign reports whether any emitted definition references a `foreign` axiom
// of the given name (erased to an IForeign accessor). Mirrors usesOTP's walk.
func usesForeign(p Program, name string) bool {
	var walk func(Ir) bool
	walk = func(t Ir) bool {
		switch x := t.(type) {
		case IForeign:
			return x.Name == name
		case ILam:
			return walk(x.Body)
		case IApp:
			return walk(x.Fn) || walk(x.Arg)
		case ILet:
			return walk(x.Val) || walk(x.Body)
		case IPair:
			return walk(x.A) || walk(x.B)
		case IFst:
			return walk(x.P)
		case ISnd:
			return walk(x.P)
		default:
			return false
		}
	}
	for _, d := range p.Defs {
		if walk(d.Body) {
			return true
		}
	}
	return false
}
