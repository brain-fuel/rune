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
