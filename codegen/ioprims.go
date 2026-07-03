package codegen

import (
	"fmt"
	"strings"
)

// primName returns the last dot-segment of a (possibly qualified) foreign name.
// A plain (unqualified) name is returned unchanged.
// Used to extract the prim identity from a module-qualified foreign axiom:
// "Std.Float.getFloat" -> "getFloat", "printNat" -> "printNat".
//
// INVARIANT: prim identity is the last dot-segment. Two distinct qualified
// foreign names (e.g. "A.send" and "B.send") share a prim identity if and only
// if their last segments are equal. A program must not contain two IForeign
// nodes with distinct qualified names but the same last segment where EITHER
// segment is a known ioPrim -- use CheckPrimCollisions to enforce this before
// emitting.
//
// Every prim gate and every backend IForeign call site uses primName so that a
// foreign axiom declared inside a module block (e.g. `module Std.Float is
// foreign getFloat : IO Float end end`) is recognised as the same prim as if
// it were declared at the top level.
func primName(n string) string {
	if i := strings.LastIndex(n, "."); i >= 0 {
		return n[i+1:]
	}
	return n
}

// CheckPrimCollisions reports an error when a program contains two distinct
// IForeign names whose last dot-segments collide AND at least one of the
// colliding segments is a known ioPrim. Such a collision would cause one prim
// body to silently gate for a different foreign axiom, producing wrong output.
//
// Call this immediately after EmitProgram (or before Emit) to surface the
// problem with a clear message rather than a runtime surprise.
func CheckPrimCollisions(p Program) error {
	// Collect the mapping from prim-segment -> full IForeign name, walking all defs.
	seen := map[string]string{} // primName -> first full foreign name that produced it
	var collisions []string
	var walk func(Ir)
	walk = func(t Ir) {
		switch x := t.(type) {
		case IForeign:
			pn := primName(x.Name)
			if !ioPrims[pn] {
				return
			}
			first, ok := seen[pn]
			if !ok {
				seen[pn] = x.Name
				return
			}
			if first != x.Name {
				collisions = append(collisions,
					fmt.Sprintf("foreign %q and %q share prim segment %q", first, x.Name, pn))
			}
		case ILam:
			walk(x.Body)
		case IApp:
			walk(x.Fn)
			walk(x.Arg)
		case ILet:
			walk(x.Val)
			walk(x.Body)
		case IPair:
			walk(x.A)
			walk(x.B)
		case IFst:
			walk(x.P)
		case ISnd:
			walk(x.P)
		case IField:
			walk(x.Scrut)
		case ICase:
			walk(x.Scrut)
			for _, arm := range x.Arms {
				walk(arm.Body)
			}
		case IBounce:
			walk(x.Call)
		default:
			// A future Ir node type was added without updating this switch.
			// Panic loudly so the test suite catches the omission immediately.
			panic("CheckPrimCollisions: unknown Ir node type; update walk in ioprims.go")
		}
	}
	for _, d := range p.Defs {
		walk(d.Body)
	}
	if len(collisions) > 0 {
		return fmt.Errorf("prim name collision(s) in emitted program: %s",
			strings.Join(collisions, "; "))
	}
	return nil
}

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
	"foldLines":    true, // foldLines (S:U) path step s0 : (S:U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S
	"foldDir":      true, // foldDir (S:U) dir suffix step s0 : (S:U) -> Nat -> Nat -> (S -> Nat -> IO S) -> S -> IO S
	"splitOn":      true, // splitOn   sep line          : Nat -> Nat -> List Nat   (split packed line on a byte)
	"byteLen":      true, // byteLen   line              : Nat -> Nat               (byte length of a packed line)
	"jsonStrField": true, // jsonStrField field doc : Nat -> Nat -> Option Nat  (top-level string field)
	"sqlQuote":     true, // sqlQuote v : Nat -> Nat  (SQL-escape: double ' , wrap in '...')
	"openWrite":    true, // openWrite  path      : Nat -> IO Handle
	"writeChunk":   true, // writeChunk h chunk   : Handle -> Nat -> IO Handle  (writes chunk + "\n")
	"closeWrite":   true, // closeWrite h         : Handle -> IO Unit
	"sortFile":     true, // sortFile inPath outPath : Nat -> Nat -> IO Unit  (bytewise line sort)
	"dbApply":      true, // dbApply dbPath sqlPath : Nat -> Nat -> IO Unit  (sqlite3 db ".read sql")
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
	// scribe L4: the rasterizer fast path. PURE host body (native float64
	// font-rs, exactly the Go engine's raster.line); input is a flat stream
	// [nPolys, len_i, subpixel coords...] of SNAPPED 1/256 coordinates,
	// bias-encoded (sub = v*256 + 2^23, so negatives fit in a Nat); output is
	// the PACKED alpha mask (w*h bytes, first byte least significant, 0x01
	// sentinel) so the Rune side can compare it against the pure exact
	// rasterizer with one equality (the ch564 divergence lock).
	"rasterFill": true, // rasterFill w h stream : Nat -> Nat -> List Nat -> Nat
	"fromNat":    true, // fromNat    n             : Nat -> Float
	"fadd":       true, // fadd/fsub/fmul/fdiv a b   : Float -> Float -> Float
	"fsub":       true,
	"fmul":       true,
	"fdiv":       true,
	"fabsP":      true, // fabsP      x             : Float -> Float (absolute value)
	"floatToNat": true, // floatToNat x             : Float -> Nat   (truncate toward zero)
	"fleqN":      true, // fleqN      a b           : Float -> Float -> Nat (1 if a<=b else 0)
	"fsqrt":      true, // fsqrt      x             : Float -> Float (IEEE-754 square root; host sqrt — C/LLVM link -lm)
	"fpow":       true, // fpow       b e           : Float -> Float -> Float (host pow b^e — C/LLVM link -lm)
	// Float IO: the standard IO vocabulary for machine floats.
	"parseFloat": true, // parseFloat s : Nat -> Option Float (packed string -> float; none on reject)
	"getFloat":   true, // getFloat      : IO Float (read stdin line, parse; garbage -> 0.0)
	"printFloat": true, // printFloat x  : Float -> IO Float (canonical ECMAScript Number::toString + \n; returns x)
	"dot2":       true, // dot2       a0 a1 b0 b1    : Float^4 -> Float (a0*b0 + a1*b1)
	"dotList":    true, // dotList    xs ys          : FList -> FList -> Float (cblas_ddot over a marshalled double[]; C/LLVM)
	"gemmSum":    true, // gemmSum    m k n A B       : Nat^3 -> FList -> FList -> Float (cblas_dgemm A·B, sum of entries; C/LLVM)
	// D4 INTEROP — a uniform numeric CAPABILITY with per-backend impls, all behind one
	// Rune tolerance contract: the py backend binds REAL NumPy (np.dot), the native
	// backends OpenBLAS (cblas_ddot), the rest a portable reference floor. Parity is of
	// the CONTRACT, not the library — the guard checks each impl against an in-language
	// reference, so a divergent third-party result is BLAMED, never trusted blindly.
	"npDot":    true, // npDot      xs ys          : FList -> FList -> Float (NumPy on py; OpenBLAS on C/LLVM; reference elsewhere)
	"npMean":   true, // npMean     xs             : FList -> Float (numpy.mean on py; hand sum/count elsewhere — no BLAS)
	"npMatSum": true, // npMatSum   m k n A B       : Nat^3 -> FList -> FList -> Float (numpy matmul (A@B).sum() on py; cblas_dgemm on C/LLVM; triple loop elsewhere)
	"npVar":    true, // npVar      xs             : FList -> Float (numpy.var on py; hand 2-pass mean/sq-dev elsewhere — no BLAS)
	"npMax":    true, // npMax      xs             : FList -> Float (numpy.max on py; fold-max elsewhere — no BLAS)
	"npNorm":   true, // npNorm     xs             : FList -> Float (numpy.linalg.norm on py; sqrt(sum sq) elsewhere — native -lm)
}

// fileEnvPrims are the D6 prims whose host body needs the packed-String codec
// (decode a code to a host string, encode a host string to a code). A backend
// emits the shared codec helpers once when any of these is referenced.
var fileEnvPrims = []string{"getEnvCode", "readFileCode", "writeFileCode", "printStrCode", "argAtCode"}

// usesOS reports whether the program references any D6 prim whose Go body needs the
// "os" module, so the Go backend knows to add the import. printStrCode is excluded:
// its body uses fmt.Println, not os, so a program that only prints would otherwise
// import "os" unused (a Go compile error).
func usesOS(p Program) bool {
	return usesForeign(p, "getEnvCode") || usesForeign(p, "readFileCode") ||
		usesForeign(p, "writeFileCode") || usesForeign(p, "argAtCode") ||
		usesForeign(p, "argCountCode") || usesForeign(p, "exitWith")
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

// streamPrims need the __s2h/__h2s String codec; foldLines additionally needs os+bufio.
var streamPrims = []string{"foldLines", "foldDir", "splitOn", "byteLen", "jsonStrField", "openWrite", "writeChunk", "closeWrite", "sortFile", "sqlQuote", "dbApply"}

func usesStream(p Program) bool {
	for _, n := range streamPrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}

// dataPlanePrims are the E4 / wavelet in-process data-plane ops (kv / object /
// queue): each backend bakes a local host body so a program using these RUNS unaided.
var dataPlanePrims = []string{
	"kvPutCode", "kvGetCode", "kvDelCode",
	"objPutCode", "objGetCode", "objDelCode",
	"enqueueCode", "dequeueCode",
}

// usesDataPlane reports whether the program references any wavelet data-plane op.
func usesDataPlane(p Program) bool {
	for _, n := range dataPlanePrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}

// binPrims are the Phase-0 real-byte-string ops (the honest counterpart to the
// packed-Nat String): a `Bin` is a length-prefixed sequence of arbitrary bytes.
// Each backend bakes a host body so a program using them RUNS unaided. binEmpty :
// Bin; binCons : Nat -> Bin -> Bin (prepend the low byte of the nat); binLen :
// Bin -> Nat; binAt : Bin -> Nat -> Nat (the byte, or 0 oob); printBin : Bin ->
// IO Unit (write the canonical `$show` rendering, see ir.go LitBytes, + newline).
var binPrims = []string{"binEmpty", "binCons", "binLen", "binAt", "printBin"}

// usesBin reports whether the program references any Phase-0 Bin op, so a backend
// knows to emit the byte-string host bodies (and any value-domain carrier).
func usesBin(p Program) bool {
	for _, n := range binPrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}

// netPrims are the Phase-1 uniform socket vocabulary (unparks `net`): a minimal
// synchronous, blocking TCP surface. Handles cross as the host's own socket object
// (opaque, the `Ptr` foreign type); payloads cross as Bin. sockConnect host port :
// Bin -> Nat -> IO Ptr; sockWrite conn data : Ptr -> Bin -> IO Nat (bytes written);
// sockRead conn n : Ptr -> Nat -> IO Bin (up to n bytes, empty = EOF/closed);
// sockClose conn : Ptr -> IO Unit. On JS (no synchronous TCP) these ride the same
// ioRuntimeAsync the OTP/live-KV paths use; the other backends block natively.
var netPrims = []string{"sockConnect", "sockWrite", "sockRead", "sockClose", "sockListen", "sockAccept"}

// usesNet reports whether the program references any Phase-1 socket op.
func usesNet(p Program) bool {
	for _, n := range netPrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}

// fsPrims are the Phase-1 expanded OS/filesystem vocabulary over the real Bin
// carrier (Go's `os`/`io/fs` surface, subsuming the raw syscall layer): fsWrite path
// content : Bin -> Bin -> IO Nat (bytes written); fsRead path : Bin -> IO Bin
// (content, empty if missing); fsExists path : Bin -> IO Nat (1/0); fsRemove path :
// Bin -> IO Nat (1/0); fsMkdir path : Bin -> IO Nat (1/0, mkdir -p). Unlike D6's
// readFileCode/writeFileCode (packed-Nat String codes), these are Bin-native and
// carry arbitrary bytes.
var fsPrims = []string{"fsWrite", "fsRead", "fsExists", "fsRemove", "fsMkdir"}

// usesFS reports whether the program references any Phase-1 filesystem op.
func usesFS(p Program) bool {
	for _, n := range fsPrims {
		if usesForeign(p, n) {
			return true
		}
	}
	return false
}

// usesProc reports whether the program references os/exec (procRun program arg :
// Bin -> Bin -> IO Bin — run a program with one argument, return its stdout bytes).
func usesProc(p Program) bool { return usesForeign(p, "procRun") }

// usesCrypto reports whether the program references a crypto hash prim. sha256 data
// : Bin -> Bin is a 32-byte digest via the host's crypto (deterministic, vector-
// checkable). Available where the host ships sha256 (js/py/go/erl/jvm); Rust + native
// C/LLVM lack a built-in (would need a vendored/linked lib).
func usesCrypto(p Program) bool { return usesForeign(p, "sha256") }

// usesTLS reports whether the program references TLS. tlsGet host port path : Bin ->
// Nat -> Bin -> IO Bin does an HTTPS GET and returns the response body (host HTTPS
// client, cert verification skipped for the harness self-signed server). Available
// where the host ships TLS (go/py/js here; jvm/erl addable); Rust + native C/LLVM
// lack a TLS lib (same gap as crypto).
func usesTLS(p Program) bool { return usesForeign(p, "tlsGet") }

// UsesCrypto / UsesTLS are exported so the harness can decide native link flags
// (the vendored libbearssl.a, and the TLS shim object) per program.
func UsesCrypto(p Program) bool { return usesCrypto(p) }
func UsesTLS(p Program) bool    { return usesTLS(p) }

// usesPar reports whether any emitted definition references the ambient `par`
// combinator (an IGlobal, not an IForeign, because par is a bodiless builtin).
// When true the Go backend must emit goFrontierRuntime and pull in "os" and "fmt"
// (already in the default import set) so __schedState can read WAVELET_SEED.
func usesPar(p Program) bool {
	var walk func(Ir) bool
	walk = func(t Ir) bool {
		switch x := t.(type) {
		case IGlobal:
			return x.Name == "par"
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
		case IField:
			return walk(x.Scrut)
		case ICase:
			if walk(x.Scrut) {
				return true
			}
			for _, arm := range x.Arms {
				if walk(arm.Body) {
					return true
				}
			}
			return false
		case IBounce:
			return walk(x.Call)
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

// usesForeign reports whether any emitted definition references a `foreign` axiom
// of the given name (erased to an IForeign accessor). Mirrors usesOTP's walk.
func usesForeign(p Program, name string) bool {
	var walk func(Ir) bool
	walk = func(t Ir) bool {
		switch x := t.(type) {
		case IForeign:
			return primName(x.Name) == name
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
		case IField:
			return walk(x.Scrut)
		case ICase:
			if walk(x.Scrut) {
				return true
			}
			for _, arm := range x.Arms {
				if walk(arm.Body) {
					return true
				}
			}
			return false
		case IBounce:
			return walk(x.Call)
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
