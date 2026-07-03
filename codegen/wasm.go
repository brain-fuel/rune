package codegen

import (
	"fmt"
	"sort"
	"strings"

	"goforge.dev/rune/v3/core"
)

// Wasm is the NINTH backend (R-NATIVE Cranelift/WASM node): it emits WebAssembly
// text (WAT) over the CLOSURE-CONVERTED IR (codegen/closure.go), runnable directly
// by `wasmtime run module.wat`. Where the LLVM backend (codegen/ll.go, the closest
// template) emits LLVM IR that links a C runtime, WASM emits a SELF-CONTAINED module:
// the runtime (linear-memory bump allocator, heap records for closures/constructors,
// a function table for `call_indirect`, the naive base-1e9 bignum, and a `$show`
// writer over WASI `fd_write`) is WAT emitted inline (wasmRuntime), because wasmtime
// runs a single `.wat` with no host linkage.
//
// VALUE REP: a Rune value is an i32 — an immediate small int `(n<<1)|1` (FFI LitInt)
// or an even-aligned heap pointer. The emitter never inspects the tag; it defers to
// the `rt_*` runtime helpers, so the rep is owned by ONE place (wasmRuntime).
//
// SUPPORTED FRAGMENT (byte-identical to the C/LLVM backends on this subset): builtin-
// nat arithmetic (zero/succ/NatElim + accel add/mul/monus), constructors + eliminators
// (CCase/CField over ListElim/NatElim/any datatype), curried closures + application,
// dependent pairs (CPair/CFst/CSnd), the erased token (CUnit), native int/nat/bytes
// literals (CLit{LitInt,LitNat,LitBytes}), and the whitelisted `foreign` IO/value
// vocabulary (wasmSupportedForeign). UNSUPPORTED (the emitter returns a clear error,
// never broken WAT): LitStr/LitPtr, and any foreign not on the whitelist. GC: none:
// a bump allocator only (v1); a program that out-allocates the 16 MiB initial memory
// traps. See the deliverable notes.
type Wasm struct{}

func (Wasm) Target() string { return "wasm" }

// emitModuleCore closure-converts and Perceus-balances the program, then renders the
// SHARED module prefix every WASM emission mode (app or library) needs: the
// function-table type, the inline runtime, the interned string-constant data segments,
// and every lifted code block / constructor / eliminator / definition thunk (via
// emitDefs + emitForeignPrimsWasm). It stops short of "(module\n" (callers open the
// module themselves, since app and library mode stamp different header comments) and
// short of the table/elem + entry point (app mode appends `$rune_main` + `_start`;
// library mode appends `init` + the export ABI). Callers MUST call em.emitTable after
// appending their own code blocks (if any) and before closing the module.
//
// This is a pure refactor split out of Emit's body: app-mode output is unchanged (the
// same WriteString calls in the same order), gated by the full existing WASM/ARC/
// Perceus suite.
func emitModuleCore(p Program) (*wasmEmitter, *strings.Builder) {
	cp := ClosureConvert(p)
	cp = Perceus(cp)
	em := newWasmEmitter(cp)

	var body strings.Builder
	em.emitDefs(&body)
	// Bake the whitelisted foreign IO ops + the IO monad (bindIO/pureIO) into the module.
	// This runs BEFORE emitTable so each op's code blocks are registered (codeRef) and
	// land in the function table; WAT is order-agnostic across function references, so
	// an entry point below may forward-reference these thunks freely.
	em.emitForeignPrimsWasm(&body, p)

	var b strings.Builder
	b.WriteString("  (type $codety (func (param i32 i32) (result i32)))\n")
	emitWasmRuntime(&b, usesFloatWasm(p))
	b.WriteString("\n")
	// String-constant data segments (constructor names) + the fixed runtime messages,
	// laid out in the reserved low region [64, 4096). Each cstr is NUL-terminated.
	em.emitData(&b)
	b.WriteString("\n")
	b.WriteString(body.String())
	return em, &b
}

// Emit closure-converts the program and emits a self-contained WAT module: the inline
// runtime, every lifted code block + constructor/eliminator code block as a `(func)`,
// the definition thunks (lazy cached, as mutable globals), the function table the
// `call_indirect` apply reads, and `$rune_main` (evaluates the entry and `$show`s it),
// exported as `_start` so `wasmtime run` invokes it.
func (Wasm) Emit(p Program) (TargetSource, error) {
	// Reject the unsupported fragment up front with a clear error (HONESTY: never
	// emit broken WAT for a form we cannot lower).
	if err := wasmCheckSupported(p); err != nil {
		return "", err
	}

	em, b := emitModuleCore(p)

	// $rune_main: evaluate the entry thunk and show it. An IO main (p.IOMain) is a
	// deferred world thunk (unit -> A): mirror C/JS by running the effect chain --
	// apply the entry to the world token ($rt_unit) -- then show the yielded value.
	var mainB strings.Builder
	mainB.WriteString("  (func $rune_main\n")
	if p.Main != "" {
		f := &wasmFunc{em: em}
		var inner strings.Builder
		mainV := f.emit(&inner, CGlobal{Name: p.Main}, nil)
		if p.IOMain {
			r := f.fresh()
			fmt.Fprintf(&inner, "    (local.set %s (call $rt_apply %s (call $rt_unit)))\n", r, mainV)
			mainV = "(local.get " + r + ")"
		}
		fmt.Fprintf(&inner, "    (call $rt_show_line %s)\n", mainV)
		mainB.WriteString(f.localDecls())
		mainB.WriteString(inner.String())
	}
	mainB.WriteString("  )\n")

	// Assemble the module: header, the shared core (function-table type, inline
	// runtime, string-constant data, program bodies), the entry point, the table +
	// elem, and the init/export.
	var out strings.Builder
	out.WriteString(";; Generated by rune emit (wasm backend) — the erased shadow, native via wasmtime.\n")
	out.WriteString(";; Run: wasmtime run module.wat  (WAT runs directly; the runtime is inline).\n")
	out.WriteString("(module\n")
	out.WriteString(b.String())
	out.WriteString(mainB.String())
	// The function table + elem: every code block by its assigned index.
	em.emitTable(&out)
	// init: seed UNIT, run main. Exported as _start so wasmtime invokes it.
	out.WriteString("  (func $_start (export \"_start\")\n")
	out.WriteString("    (global.set $UNIT (call $rt_mkcon (i32.const 0) (global.get $unit_name) (i32.const 0)))\n")
	out.WriteString("    (call $rune_main))\n")
	out.WriteString(")\n")
	return TargetSource(out.String()), nil
}

// wasmSupportedForeign lists the foreign ops with a WAT body in emitForeignPrimsWasm.
// wasmCheckSupported allows these; any other foreign is still a hard, honest error.
//
// Task 1 (the D6/bible-ops WASM tier foundation) opens this whitelist with the two
// foreign IO ops the ch210 demo exercises: printNat (console-out via fd_write) and
// timeNanos (the OS monotonic clock via WASI clock_time_get). Both are baked in
// emitForeignPrimsWasm as CURRIED IO-closure chains -- the load-bearing template every
// later IO op (getEnvCode/readFileCode/foldLines/...) reuses. Later tasks append names
// here as their WAT bodies land; an unknown foreign STILL hard-errors (the honesty gate).
var wasmSupportedForeign = map[string]bool{
	"printNat":  true,
	"timeNanos": true,
	// Task 2: the 4 PURE bible ops over the packed-String codec (no world token).
	"byteLen":      true,
	"splitOn":      true,
	"jsonStrField": true,
	"sqlQuote":     true,
	// Task 3: the 2 HIGHER-ORDER file/dir bible ops (WASI file-read + fd_readdir).
	"foldLines": true,
	"foldDir":   true,
	// Task 4: write-stream ops (WASI file-write + fd handle table) + dbApply (WASM no-op).
	"Handle":     true,
	"openWrite":  true,
	"writeChunk": true,
	"closeWrite": true,
	"sortFile":   true,
	"dbApply":    true,
	// Task 5: the D6 env/argv/exit layer (environ_*/args_*/proc_exit WASI + the codec).
	"getEnvCode":    true,
	"readFileCode":  true,
	"writeFileCode": true,
	"printStrCode":  true,
	"argAtCode":     true,
	"argCountCode":  true,
	"exitWith":      true,
	// Task 3 (6c): the Phase-0 Bin vocabulary (binPrims, ioprims.go) over K_BIN.
	"binEmpty": true,
	"binCons":  true,
	"binLen":   true,
	"binAt":    true,
	"printBin": true,
	// Task 4b: machine-float (f64) IO host ops + the base D3 kit the listings need.
	"Float":      true,
	"fromNat":    true,
	"fmul":       true,
	"parseFloat": true,
	"getFloat":   true,
	"printFloat": true,
}

// wasmCheckSupported walks the program's IR and returns a clear error for any form the
// v1 WASM backend cannot lower (rather than emit broken WAT). Mirrors foreignNames'
// walk over every node kind so a nested unsupported form is found.
func wasmCheckSupported(p Program) error {
	var err error
	var walk func(Ir)
	walk = func(t Ir) {
		if err != nil {
			return
		}
		switch x := t.(type) {
		case IForeign:
			if !wasmSupportedForeign[primName(x.Name)] {
				err = fmt.Errorf("codegen(wasm): foreign %q is not supported (no WASI/WAT body)", x.Name)
				return
			}
			// whitelisted: a WAT body is baked by emitForeignPrimsWasm -- no error.
		case ILit:
			switch x.Kind {
			case LitInt, LitNat, LitBytes:
				// supported
			case LitStr:
				err = fmt.Errorf("codegen(wasm): string literals are not supported in v1")
			case LitPtr:
				err = fmt.Errorf("codegen(wasm): opaque host pointers are not supported in v1")
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
		}
	}
	for _, d := range p.Defs {
		walk(d.Body)
	}
	if err != nil {
		return err
	}
	// IO-main is supported since Task 1 (the effect chain is run at $rune_main by
	// applying the world token, then the final value is shown) -- see Emit().
	return nil
}

// wasmEmitter renders CIr terms to WAT. Like llEmitter it knows the nat eliminator's
// name + the accel table, accumulates constructor-name string constants (interned into
// the data segment), and assigns each emitted code block a function-table index so
// `call_indirect` can reach it.
type wasmEmitter struct {
	cp      ClosureProgram // the program being emitted; set by newWasmEmitter
	natElim string
	accel   map[string]core.NatOp

	// codeIdx maps a code-block func name (without leading $) to its table index, in
	// first-seen order. The order is also the (elem) order.
	codeIdx   map[string]int
	codeOrder []string

	// strs maps a string constant (constructor name) to its byte offset in the data
	// segment, allocated bump-style from strBase upward.
	strs     map[string]int
	strOrder []string
	strBytes int

	// ctorByName maps a non-nat constructor's emitted name to its CtorSpec, so the
	// AppClosure emit path can recognize a SATURATED constructor application (an
	// AppClosure spine headed by a CGlobal naming a constructor, with exactly Arity
	// args) and build the K_CON directly via rt_mkcon (satCtorDispatch), instead of
	// currying through emitCtorBlock and allocating an intermediate partial-application
	// K_CLO. The builtin-nat data group is EXCLUDED (its zero/succ compile to native
	// bignum via emitNat, not K_CON), so a `succ n` spine never matches here.
	ctorByName map[string]CtorSpec
}

// newWasmEmitter constructs a wasmEmitter for the given closure-converted program,
// wiring the nat eliminator name and accel table when a NatSpec is present.
func newWasmEmitter(cp ClosureProgram) *wasmEmitter {
	em := &wasmEmitter{
		cp:         cp,
		codeIdx:    map[string]int{},
		strs:       map[string]int{},
		ctorByName: map[string]CtorSpec{},
	}
	if cp.Nat != nil {
		em.natElim = cp.Nat.ElimName
		em.accel = cp.Nat.Ops
	}
	// Index every non-nat constructor for satCtorDispatch. Skip the builtin-nat data
	// group (zero/succ compile to native bignum via emitNat, not K_CON) so a saturated
	// `succ n` never mis-routes into the rt_mkcon path.
	for _, d := range cp.Datas {
		if cp.Nat != nil && d.ElimName == cp.Nat.ElimName {
			continue
		}
		for _, c := range d.Ctors {
			em.ctorByName[c.Name] = c
		}
	}
	return em
}

// emitDefs emits all constructor/eliminator code blocks + thunks, the lifted lambda
// code blocks, and the top-level definition thunks into b. This is the "program
// bodies" section of the module, factored out of Emit so WasmSteadyModule can reuse
// the same emission path with a custom entry point.
func (em *wasmEmitter) emitDefs(b *strings.Builder) {
	// Constructor + eliminator code blocks and thunks.
	for _, d := range em.cp.Datas {
		if em.cp.Nat != nil && d.ElimName == em.cp.Nat.ElimName {
			em.emitNat(b, *em.cp.Nat)
			continue
		}
		for _, c := range d.Ctors {
			em.emitCtor(b, c)
		}
	}
	// The lifted code blocks (definitions' and eliminators' bodies).
	for _, blk := range em.cp.Blocks {
		em.emitBlock(b, blk)
	}
	// Definition thunks (eliminators converted as CDefSpec, then user defs). A `partial`
	// def is split into a _step body + curried drivers + a public trampoline thunk.
	for _, def := range em.cp.Defs {
		if em.cp.Partials[def.Name] {
			em.emitPartialWasm(b, def.Name, def.Arity, def.Body)
			continue
		}
		em.emitDefThunk(b, def.Name, def.Body)
	}
}

// codeRef registers (on first sight) a code-block name and returns its table index.
func (em *wasmEmitter) codeRef(name string) int {
	if idx, ok := em.codeIdx[name]; ok {
		return idx
	}
	idx := len(em.codeOrder)
	em.codeIdx[name] = idx
	em.codeOrder = append(em.codeOrder, name)
	return idx
}

// strBase is where constructor-name cstrs live: above the iovec/messages, below the
// show scratch buffer (4096). The runtime's reserved low region is [0,4096): the iovec
// is at [16,24), fixed runtime messages at [32,512), and interned cstrs at [512,4096).

// wasmStrBase is where interned constructor-name / numeral cstrs begin (above the
// fixed runtime messages at [32,512)).
const wasmStrBase = 512

// wasmShowBuf is the show scratch buffer base; interned cstrs must stay below it.
const wasmShowBuf = 4096

// intern registers a string constant and returns its byte offset.
func (em *wasmEmitter) intern(s string) int {
	if off, ok := em.strs[s]; ok {
		return off
	}
	off := wasmStrBase + em.strBytes
	if off+len(s)+1 > wasmShowBuf {
		// Out of reserved cstr space; v1 caps interned constants at ~3.5 KiB. A real
		// program rarely needs this much in names, but be honest about the bound.
		panic("codegen(wasm): interned string constants exceed the reserved region")
	}
	em.strs[s] = off
	em.strOrder = append(em.strOrder, s)
	em.strBytes += len(s) + 1 // + NUL
	return off
}

// emitData writes the data segments: the fixed runtime messages (referenced by the
// runtime via $fn_msg/$abort_msg/$unit_name globals) and the interned constructor-name
// cstrs. The globals are declared here too (offsets are known only after interning).
func (em *wasmEmitter) emitData(b *strings.Builder) {
	// Fixed runtime messages live at [32,512), referenced by the runtime via the
	// $fn_msg/$abort_msg/$unit_name globals (the iovec occupies [16,32)).
	fnMsg := "<function>"
	abortMsg := "rune-wasm: impossible (unmatched constructor tag)\n"
	unitName := "" // the boxed unit's con name is unused by show (UNIT is special-cased)

	fnOff := 32
	abortOff := fnOff + len(fnMsg) + 1
	unitOff := abortOff + len(abortMsg) + 1
	if unitOff+len(unitName)+1 > wasmStrBase {
		panic("codegen(wasm): fixed runtime messages overflow the reserved region")
	}

	fmt.Fprintf(b, "  (global $fn_msg i32 (i32.const %d))\n", fnOff)
	fmt.Fprintf(b, "  (global $abort_msg i32 (i32.const %d))\n", abortOff)
	fmt.Fprintf(b, "  (global $abort_len i32 (i32.const %d))\n", len(abortMsg))
	fmt.Fprintf(b, "  (global $unit_name i32 (i32.const %d))\n", unitOff)
	fmt.Fprintf(b, "  (data (i32.const %d) %s)\n", fnOff, wasmDataStr(fnMsg))
	fmt.Fprintf(b, "  (data (i32.const %d) %s)\n", abortOff, wasmDataStr(abortMsg))
	fmt.Fprintf(b, "  (data (i32.const %d) %s)\n", unitOff, wasmDataStr(unitName))

	for _, s := range em.strOrder {
		fmt.Fprintf(b, "  (data (i32.const %d) %s)\n", em.strs[s], wasmDataStr(s))
	}
}

// emitTable writes the function table sized to hold every registered code block, and
// the (elem) that installs each at its assigned index.
func (em *wasmEmitter) emitTable(b *strings.Builder) {
	n := len(em.codeOrder)
	fmt.Fprintf(b, "  (table %d funcref)\n", n)
	if n > 0 {
		b.WriteString("  (elem (i32.const 0)")
		for _, name := range em.codeOrder {
			fmt.Fprintf(b, " $%s", name)
		}
		b.WriteString(")\n")
	}
}

// wasmFunc tracks per-function emission state. A code block / def thunk body lowers a
// CIr to a SEQUENCE of `(local.set $tN value)` statements and returns the WAT
// expression `(local.get $tN)` naming the result, so nested control flow (CCase) is
// straightforward. Locals are declared lazily and flushed into the func header.
type wasmFunc struct {
	em    *wasmEmitter
	tmp   int
	decls []string // local declarations to splice into the func header
}

// fresh allocates a fresh i32 local and returns its name (e.g. "$t3").
func (f *wasmFunc) fresh() string {
	f.tmp++
	name := fmt.Sprintf("$t%d", f.tmp)
	f.decls = append(f.decls, name)
	return name
}

// localDecls renders the accumulated local declarations for the func header.
func (f *wasmFunc) localDecls() string {
	if len(f.decls) == 0 {
		return ""
	}
	var b strings.Builder
	b.WriteString("   ")
	for _, d := range f.decls {
		fmt.Fprintf(&b, " (local %s i32)", d)
	}
	b.WriteString("\n")
	return b.String()
}

// emitDefThunk emits a memoized thunk for a top-level definition as a lazy cached
// global: a value cache + a done flag; the first call computes and caches, so recursion
// + eliminator self-reference terminate and run once.
func (em *wasmEmitter) emitDefThunk(b *strings.Builder, name string, body CIr) {
	em.emitCachedThunk(b, name, func(f *wasmFunc, bb *strings.Builder) string {
		return f.emit(bb, body, nil)
	})
}

// emitCachedThunk emits the memoized-thunk wrapper around a body producing a Value.
// Shared by definitions, constructors, and the nat thunks. The cache/done live as two
// mutable globals; the func computes body into a temp buffer (so its local decls and
// the cache-check share one func header).
func (em *wasmEmitter) emitCachedThunk(b *strings.Builder, name string, body func(*wasmFunc, *strings.Builder) string) {
	cache := "$cache_" + wasmName(name)
	done := "$done_" + wasmName(name)
	fmt.Fprintf(b, "  (global %s (mut i32) (i32.const 0))\n", cache)
	fmt.Fprintf(b, "  (global %s (mut i32) (i32.const 0))\n", done)
	thunk := wasmThunkName(name)
	f := &wasmFunc{em: em}
	var inner strings.Builder
	v := body(f, &inner)
	fmt.Fprintf(b, "  (func $%s (result i32)\n", thunk)
	b.WriteString(f.localDecls())
	fmt.Fprintf(b, "    (if (global.get %s) (then (return (global.get %s))))\n", done, cache)
	fmt.Fprintf(b, "    (global.set %s (i32.const 1))\n", done)
	b.WriteString(inner.String())
	fmt.Fprintf(b, "    (global.set %s %s)\n", cache, v)
	fmt.Fprintf(b, "    (global.get %s))\n", cache)
}

// wasmStepName is the partial's memoized _step thunk func name. It matches the cache
// key name+"_step" passed to emitCachedThunk (cName is a plain sanitizer, so
// "def_"+cName(name)+"_step" == "def_"+cName(name+"_step")).
func wasmStepName(name string) string { return wasmThunkName(name) + "_step" }

// emitPartialWasm lowers a `partial` def to the T2 trampoline split (the WASM dual of
// emitPartialC): a memoized _step thunk (the body, where a CBounce is a K_BOUNCE), then
// `arity` curried driver code blocks that collect args into the closure env, the last of
// which saturates _step and drives the bounce chain via $rt_tramp; and a public thunk
// returning a closure entering driver 0.
func (em *wasmEmitter) emitPartialWasm(b *strings.Builder, name string, arity int, body CIr) {
	// _step: the memoized body (a curried lambda of `arity` args; a tail call inside is
	// a K_BOUNCE). Its func name is wasmStepName(name).
	em.emitCachedThunk(b, name+"_step", func(f *wasmFunc, bb *strings.Builder) string {
		return f.emit(bb, body, nil)
	})
	step := wasmStepName(name)

	if arity == 0 {
		// No args to collect: the public thunk drives the (bounce-free) step once.
		em.emitCachedThunk(b, name, func(f *wasmFunc, bb *strings.Builder) string {
			r := f.fresh()
			fmt.Fprintf(bb, "    (local.set %s (call $rt_tramp (call $%s)))\n", r, step)
			return "(local.get " + r + ")"
		})
		return
	}

	base := "drvfn_" + wasmName(name)
	for i := 0; i < arity; i++ {
		em.emitPartialDriverBlock(b, fmt.Sprintf("%s_%d", base, i), name, arity, i)
	}
	em.emitCachedThunk(b, name, func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, em.codeRef(base+"_0"))
		return "(local.get " + r + ")"
	})
}

// emitPartialDriverBlock emits the i-th curry block of a partial's public driver. For
// i<arity-1 it builds the next driver closure capturing env[0..i-1] + arg (identical to
// emitCtorBlock's collect arm). For i==arity-1 it saturates _step() with env[0..i-1] +
// arg via $rt_apply, then $rt_tramp's the result.
func (em *wasmEmitter) emitPartialDriverBlock(b *strings.Builder, fname, name string, arity, i int) {
	em.codeRef(fname)
	base := "drvfn_" + wasmName(name)
	step := wasmStepName(name)
	f := &wasmFunc{em: em}
	var inner strings.Builder
	var ret string
	if i < arity-1 {
		// Collect block: build the next driver closure capturing env[0..i-1] + arg. Unlike
		// emitCtorBlock (whose constructor spines are recognized + never released), a DRIVER
		// intermediate IS released by the generic apply path, so each forwarded env value
		// must be RETAINED into the child (the child takes its own reference; the parent's
		// later release is then balanced). The fresh $arg is owned by this block, so it MOVES.
		cl := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const %d)))\n", cl, em.codeRef(fmt.Sprintf("%s_%d", base, i+1)), i+1)
		for k := 0; k < i; k++ {
			fmt.Fprintf(&inner, "    (call $rt_retain (call $rt_env (local.get $env) (i32.const %d)))\n", k)
			fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (call $rt_env (local.get $env) (i32.const %d)))\n", cl, k, k)
		}
		fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (local.get $arg))\n", cl, i)
		ret = "(local.get " + cl + ")"
	} else {
		// Saturating block: build the starting bounce {step(), env[0..i-1], arg} and drive
		// it. The driver closure OWNS its captured env (and is released by the caller after
		// this returns), so each env value is RETAINED into the bounce (the bounce takes its
		// own reference). The fresh $arg is owned by this block, so it MOVES into the bounce.
		// $rt_tramp then drives uniformly.
		bn := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_mkbounce (call $%s) (i32.const %d)))\n", bn, step, arity)
		for k := 0; k < i; k++ {
			fmt.Fprintf(&inner, "    (call $rt_retain (call $rt_env (local.get $env) (i32.const %d)))\n", k)
			fmt.Fprintf(&inner, "    (call $rt_bounce_set (local.get %s) (i32.const %d) (call $rt_env (local.get $env) (i32.const %d)))\n", bn, k, k)
		}
		fmt.Fprintf(&inner, "    (call $rt_bounce_set (local.get %s) (i32.const %d) (local.get $arg))\n", bn, i)
		rv := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_tramp (local.get %s)))\n", rv, bn)
		ret = "(local.get " + rv + ")"
	}
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32)\n", fname)
	b.WriteString(f.localDecls())
	b.WriteString(inner.String())
	fmt.Fprintf(b, "    %s)\n", ret)
}

// emitBlock emits one lifted code block as a WAT func taking (arg, env) -> i32 and
// registers it in the table. The body refers to its argument as CVar{0} and captures
// via CEnv{slot} (an env load). Locals are tracked as a stack of WAT expressions
// (innermost = index 0); the seed is the $arg parameter.
func (em *wasmEmitter) emitBlock(b *strings.Builder, blk CodeBlock) {
	name := wasmCodeName(blk.Name)
	em.codeRef(name)
	f := &wasmFunc{em: em}
	var inner strings.Builder
	v := f.emitIn(&inner, blk.Body, []string{"(local.get $arg)"})
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32)\n", name)
	b.WriteString(f.localDecls())
	b.WriteString(inner.String())
	fmt.Fprintf(b, "    %s)\n", v)
}

// emitCtor emits a constructor as a curried closure, mirroring emitCtorLL: a nullary
// constructor is the {tag} object thunk; an arity-k constructor builds k code blocks,
// each capturing the prefix and the last building the {tag,fields} object.
func (em *wasmEmitter) emitCtor(b *strings.Builder, c CtorSpec) {
	nameOff := em.intern(c.Name)
	if c.Arity == 0 {
		em.emitCachedThunk(b, c.Name, func(f *wasmFunc, bb *strings.Builder) string {
			r := f.fresh()
			fmt.Fprintf(bb, "    (local.set %s (call $rt_mkcon (i32.const %d) (i32.const %d) (i32.const 0)))\n", r, c.Tag, nameOff)
			return "(local.get " + r + ")"
		})
		return
	}
	base := "ctorfn_" + wasmName(c.Name)
	for i := 0; i < c.Arity; i++ {
		em.emitCtorBlock(b, fmt.Sprintf("%s_%d", base, i), c, i, nameOff)
	}
	em.emitCachedThunk(b, c.Name, func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, em.codeRef(base+"_0"))
		return "(local.get " + r + ")"
	})
}

// emitCtorBlock emits the i-th currying block of a k-ary constructor. The block i<k-1
// builds the next closure capturing env[0..i-1] + arg; block i==k-1 builds the
// {tag,fields} constructor object.
func (em *wasmEmitter) emitCtorBlock(b *strings.Builder, fname string, c CtorSpec, i, nameOff int) {
	em.codeRef(fname)
	base := "ctorfn_" + wasmName(c.Name)
	var inner strings.Builder
	f := &wasmFunc{em: em}
	var ret string
	if i == c.Arity-1 {
		o := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_mkcon (i32.const %d) (i32.const %d) (i32.const %d)))\n", o, c.Tag, nameOff, c.Arity)
		for k := 0; k < i; k++ {
			fmt.Fprintf(&inner, "    (call $rt_con_set (local.get %s) (i32.const %d) (call $rt_env (local.get $env) (i32.const %d)))\n", o, k, k)
		}
		fmt.Fprintf(&inner, "    (call $rt_con_set (local.get %s) (i32.const %d) (local.get $arg))\n", o, i)
		ret = "(local.get " + o + ")"
	} else {
		cl := f.fresh()
		fmt.Fprintf(&inner, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const %d)))\n", cl, em.codeRef(fmt.Sprintf("%s_%d", base, i+1)), i+1)
		for k := 0; k < i; k++ {
			fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (call $rt_env (local.get $env) (i32.const %d)))\n", cl, k, k)
		}
		fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (local.get $arg))\n", cl, i)
		ret = "(local.get " + cl + ")"
	}
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32)\n", fname)
	b.WriteString(f.localDecls())
	b.WriteString(inner.String())
	fmt.Fprintf(b, "    %s)\n", ret)
}

// emitNat emits the builtin-nat group natively, mirroring emitNatLL: zero is the empty
// bignum, succ a +1 closure, the eliminator a 4-block curried fold loop.
func (em *wasmEmitter) emitNat(b *strings.Builder, n NatSpec) {
	// zero: the empty bignum.
	em.emitCachedThunk(b, n.Zero, func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_big_from_long (i32.const 0)))\n", r)
		return "(local.get " + r + ")"
	})
	// succ code: x -> x+1 (bignum increment). rt_big_succ BORROW-reads $arg and
	// returns a fresh K_BIG; under PATH B the caller hands the closure an OWNED arg
	// (Perceus dups a borrowed operand at the owning AppClosure position), so succ
	// must FREE its arg like any user closure code block does -- otherwise a
	// succ-chain (succ (succ x)) leaks every intermediate K_BIG each run.
	sc := wasmName(n.Succ) + "_code"
	em.codeRef(sc)
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32) (local $r i32)\n", sc)
	b.WriteString("    (local.set $r (call $rt_big_succ (local.get $arg)))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	em.emitCachedThunk(b, n.Succ, func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, em.codeRef(sc))
		return "(local.get " + r + ")"
	})
	// eliminator: m -> c0 -> c1 -> x -> fold. env order {m,c0,c1}, arg = x.
	base := wasmName(n.ElimName)
	// b3: the fold. env = {m,c0,c1}, arg = x (a bignum).
	em.emitNatFold(b, base+"_b3")
	// b2: arg=c1, env={m,c0}; build b3 capturing {m,c0,c1}.
	em.emitCurryBlock(b, base+"_b2", base+"_b3", 3, []int{0, 1}, 2)
	em.emitCurryBlock(b, base+"_b1", base+"_b2", 2, []int{0}, 1)
	em.emitCurryBlock(b, base+"_b0", base+"_b1", 1, []int{}, 0)
	em.emitCachedThunk(b, n.ElimName, func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, em.codeRef(base+"_b0"))
		return "(local.get " + r + ")"
	})
}

// emitNatFold emits the eliminator's inner fold: env = {m, c0, c1}, arg = x. acc starts
// at c0; k counts from 0; while k < x: acc = (c1 k) acc; k = k+1. Returns acc.
//
// Ownership: $arg (the bound) is BORROWED from the caller -- do NOT release it. $c1
// (env slot 2, the step closure) is BORROWED from the env -- do NOT release it. $acc
// is moved into the step each iteration (the step owns+consumes it). $k is owned by the
// loop: we RETAIN $k before the first apply so the step gets its own reference while the
// loop keeps its own; after the step we RELEASE the old $k, and after the loop we RELEASE
// the final $k (which was never consumed by the step because the loop exited). $step (the
// partially-applied step closure from rt_apply($c1, $k)) is released after the second
// apply -- it is a fresh K_CLO each iteration and nothing else owns it.
func (em *wasmEmitter) emitNatFold(b *strings.Builder, fname string) {
	em.codeRef(fname)
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32)\n", fname)
	b.WriteString("    (local $c1 i32) (local $acc i32) (local $k i32) (local $step i32) (local $knext i32)\n")
	b.WriteString("    (local.set $c1 (call $rt_env (local.get $env) (i32.const 2)))\n")
	b.WriteString("    (local.set $acc (call $rt_env (local.get $env) (i32.const 1)))\n")
	// $acc starts as a BORROWED alias of env slot 1 (the base c0). RETAIN it so the loop
	// owns its own reference: each iteration the step CONSUMES (frees) $acc and returns a
	// fresh owned one, and on a 0-iteration fold $acc is returned directly. Without this
	// retain, a caller that RELEASES the eliminator's env (satElimDispatch frees the b3
	// fold closure after the call) would free the same K_BIG the fold returned or the
	// step already freed -- a use-after-free / double-free. The retain makes the returned
	// $acc independent of the env's slot-1 ownership.
	b.WriteString("    (call $rt_retain (local.get $acc))\n")
	b.WriteString("    (local.set $k (call $rt_big_from_long (i32.const 0)))\n")
	b.WriteString("    (block $done (loop $l\n")
	b.WriteString("      (br_if $done (i32.ge_s (call $rt_big_cmp (local.get $k) (local.get $arg)) (i32.const 0)))\n")
	b.WriteString("      ;; retain $k: the step owns its argument copy; the loop keeps its own\n")
	b.WriteString("      (call $rt_retain (local.get $k))\n")
	b.WriteString("      (local.set $step (call $rt_apply (local.get $c1) (local.get $k)))\n")
	b.WriteString("      (local.set $acc (call $rt_apply (local.get $step) (local.get $acc)))\n")
	b.WriteString("      ;; $step is a fresh K_CLO each iteration; release it now\n")
	b.WriteString("      (call $rt_release (local.get $step))\n")
	b.WriteString("      ;; advance $k: compute successor, release old $k, install new\n")
	b.WriteString("      (local.set $knext (call $rt_big_succ (local.get $k)))\n")
	b.WriteString("      (call $rt_release (local.get $k))\n")
	b.WriteString("      (local.set $k (local.get $knext))\n")
	b.WriteString("      (br $l)))\n")
	b.WriteString("    ;; final $k (loop exited because $k >= $arg): release it\n")
	b.WriteString("    (call $rt_release (local.get $k))\n")
	b.WriteString("    (local.get $acc))\n")
}

// emitCurryBlock emits one currying block of a primitive: builds the next closure of
// nslots slots over $next, copying env slots copyEnv (by index, into the same slots)
// then placing $arg at argSlot.
func (em *wasmEmitter) emitCurryBlock(b *strings.Builder, name, next string, nslots int, copyEnv []int, argSlot int) {
	em.codeRef(name)
	f := &wasmFunc{em: em}
	cl := f.fresh()
	var inner strings.Builder
	fmt.Fprintf(&inner, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const %d)))\n", cl, em.codeRef(next), nslots)
	for _, k := range copyEnv {
		fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (call $rt_env (local.get $env) (i32.const %d)))\n", cl, k, k)
	}
	fmt.Fprintf(&inner, "    (call $rt_clo_set (local.get %s) (i32.const %d) (local.get $arg))\n", cl, argSlot)
	fmt.Fprintf(b, "  (func $%s (param $arg i32) (param $env i32) (result i32)\n", name)
	b.WriteString(f.localDecls())
	b.WriteString(inner.String())
	fmt.Fprintf(b, "    (local.get %s))\n", cl)
}

// emit lowers a CIr in the empty-locals context (def-thunk top level).
func (f *wasmFunc) emit(b *strings.Builder, t CIr, locals []string) string {
	return f.emitIn(b, t, locals)
}

// emitIn lowers a CIr in a context of `locals` (WAT expressions naming each binder
// visible by de Bruijn index, innermost = 0), writing `(local.set ...)` statements to b
// and returning a WAT expression naming the i32 result.
func (f *wasmFunc) emitIn(b *strings.Builder, t CIr, locals []string) string {
	switch x := t.(type) {
	case CVar:
		if x.Idx < len(locals) {
			return locals[x.Idx]
		}
		return "(call $rt_unit)" // closed-program safety marker
	case CEnv:
		return fmt.Sprintf("(call $rt_env (local.get $env) (i32.const %d))", x.Idx)
	case CGlobal:
		return fmt.Sprintf("(call $%s)", wasmThunkName(x.Name))
	case CForeign:
		// A whitelisted foreign lowers to its baked accessor thunk (emitForeignPrimsWasm),
		// named identically to a CGlobal thunk. The accessor returns the curried IO-closure
		// chain; wasmCheckSupported has already rejected any non-whitelisted foreign.
		return fmt.Sprintf("(call $%s)", wasmThunkName(x.Name))
	case CUnit:
		return "(call $rt_unit)"
	case CLit:
		return f.emitLit(b, x)
	case MkClosure:
		return f.emitMkClosure(b, x, locals)
	case AppClosure:
		if out, ok := f.satCtorDispatch(b, x, locals); ok {
			return out
		}
		if out, ok := f.satElimDispatch(b, x, locals); ok {
			return out
		}
		if out, ok := f.accelDispatch(b, x, locals); ok {
			return out
		}
		clo := f.emitIn(b, x.Clo, locals)
		arg := f.emitIn(b, x.Arg, locals)
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s (call $rt_apply %s %s))\n", r, clo, arg)
		return "(local.get " + r + ")"
	case CLet:
		v := f.emitIn(b, x.Val, locals)
		// Bind v to a fresh local so the body can reference it by name (de Bruijn 0).
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s %s)\n", r, v)
		return f.emitIn(b, x.Body, prependW("(local.get "+r+")", locals))
	case CPair:
		a := f.emitIn(b, x.A, locals)
		bb := f.emitIn(b, x.B, locals)
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s (call $rt_mkpair %s %s))\n", r, a, bb)
		return "(local.get " + r + ")"
	case CFst:
		p := f.emitIn(b, x.P, locals)
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s (call $rt_pair_fst %s))\n", r, p)
		return "(local.get " + r + ")"
	case CSnd:
		p := f.emitIn(b, x.P, locals)
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s (call $rt_pair_snd %s))\n", r, p)
		return "(local.get " + r + ")"
	case CField:
		s := f.emitIn(b, x.Scrut, locals)
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s (call $rt_con_get %s (i32.const %d)))\n", r, s, x.Index)
		return "(local.get " + r + ")"
	case CCase:
		return f.emitCase(b, x, locals)
	case CDup:
		// Retain V (a CVar/CEnv -- pure, no side effect) then evaluate K.
		v := f.emitIn(b, x.V, locals)
		fmt.Fprintf(b, "    (call $rt_retain %s)\n", v)
		return f.emitIn(b, x.K, locals)
	case CDrop:
		// Release V (a CVar/CEnv -- pure, no side effect) then evaluate K.
		v := f.emitIn(b, x.V, locals)
		fmt.Fprintf(b, "    (call $rt_release %s)\n", v)
		return f.emitIn(b, x.K, locals)
	case CBounce:
		return f.emitBounce(b, x.Call, locals)
	default:
		panic(fmt.Sprintf("codegen(wasm): unknown CIr node %T", t))
	}
}

// emitBounce lowers a CBounce (a saturated tail call to a partial member) to a K_BOUNCE:
// rt_mkbounce((call $<head>_step), nargs) then store each evaluated arg. The driver loop
// ($rt_tramp) forces the chain. A non-CGlobal head is not a recognizable partial spine,
// so emit the call directly (it runs, just not as a bounce).
func (f *wasmFunc) emitBounce(b *strings.Builder, call CIr, locals []string) string {
	var args []CIr
	t := call
	for {
		app, ok := t.(AppClosure)
		if !ok {
			break
		}
		args = append([]CIr{app.Arg}, args...)
		t = app.Clo
	}
	g, ok := t.(CGlobal)
	if !ok {
		return f.emitIn(b, call, locals)
	}
	o := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_mkbounce (call $%s) (i32.const %d)))\n", o, wasmStepName(g.Name), len(args))
	for i, a := range args {
		fmt.Fprintf(b, "    (call $rt_bounce_set (local.get %s) (i32.const %d) %s)\n", o, i, f.emitIn(b, a, locals))
	}
	return "(local.get " + o + ")"
}

// emitLit lowers a native literal. v1 supports LitInt (immediate-tagged i32), LitNat
// (a parsed bignum), and LitBytes (a constant Bin, allocated + filled byte-by-byte at
// the use site via $rt_mkbin/$rt_bin_set, Task 3); other kinds are rejected by
// wasmCheckSupported.
func (f *wasmFunc) emitLit(b *strings.Builder, x CLit) string {
	r := f.fresh()
	switch x.Kind {
	case LitNat:
		off := f.em.intern(x.Nat)
		fmt.Fprintf(b, "    (local.set %s (call $rt_big_parse (i32.const %d)))\n", r, off)
	case LitBytes:
		fmt.Fprintf(b, "    (local.set %s (call $rt_mkbin (i32.const %d)))\n", r, len(x.Str))
		for i := 0; i < len(x.Str); i++ {
			fmt.Fprintf(b, "    (call $rt_bin_set (local.get %s) (i32.const %d) (i32.const %d))\n", r, i, x.Str[i])
		}
	default: // LitInt
		fmt.Fprintf(b, "    (local.set %s (call $rt_mkint (i32.const %d)))\n", r, x.Int)
	}
	return "(local.get " + r + ")"
}

// emitMkClosure allocates a closure and fills its env slots (each evaluated in the
// enclosing context). The env terms are evaluated FIRST (into locals), then the closure
// is built and filled — so a nested AppClosure inside an env slot does not interleave
// with the rt_clo_set calls.
func (f *wasmFunc) emitMkClosure(b *strings.Builder, x MkClosure, locals []string) string {
	idx := f.em.codeRef(wasmCodeName(x.Code))
	envVals := make([]string, len(x.Env))
	for i, e := range x.Env {
		ev := f.emitIn(b, e, locals)
		// Bind into a local so the value is stable across the alloc + sets.
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s %s)\n", r, ev)
		envVals[i] = "(local.get " + r + ")"
	}
	cl := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const %d)))\n", cl, idx, len(x.Env))
	for i, ev := range envVals {
		fmt.Fprintf(b, "    (call $rt_clo_set (local.get %s) (i32.const %d) %s)\n", cl, i, ev)
	}
	return "(local.get " + cl + ")"
}

// emitCase lowers an eliminator's tag dispatch. WAT has no n-way switch expression, so
// we read the tag into a local and emit a chain of `(if (i32.eq tag K) (then ...))`,
// each arm storing its result into a shared result local; an unmatched tag aborts.
func (f *wasmFunc) emitCase(b *strings.Builder, x CCase, locals []string) string {
	s := f.emitIn(b, x.Scrut, locals)
	tag := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_con_tag %s))\n", tag, s)
	res := f.fresh()
	// Sort arms by tag for deterministic output (matches the LLVM switch's stable
	// iteration; arms are already in declaration order but be explicit).
	arms := append([]CCaseArm(nil), x.Arms...)
	sort.SliceStable(arms, func(i, j int) bool { return arms[i].Tag < arms[j].Tag })
	for _, arm := range arms {
		fmt.Fprintf(b, "    (if (i32.eq (local.get %s) (i32.const %d)) (then\n", tag, arm.Tag)
		v := f.emitIn(b, arm.Body, locals)
		fmt.Fprintf(b, "      (local.set %s %s)\n", res, v)
		b.WriteString("    ))\n")
	}
	return "(local.get " + res + ")"
}

// satCtorDispatch recognizes a SATURATED constructor application -- an AppClosure spine
// headed by a CGlobal naming a (non-nat) constructor, with EXACTLY Arity args -- and emits
// the K_CON DIRECTLY: rt_mkcon(tag, name, arity) + one rt_con_set per arg (each arg value
// evaluated once and MOVED into its field slot). No intermediate partial-application K_CLO
// is ever allocated, so the arity>=2 currying container leak (the +1/run K_CLO_mk1 the
// recognize-then-skip rule could not release without double-freeing the moved field) is
// gone. Returns the result expression + true on a hit; "" + false otherwise.
//
// A PARTIALLY applied constructor (fewer than Arity args) does NOT match (len(args) !=
// c.Arity) and falls through to the generic rt_apply path -- it curries through
// emitCtorBlock as before, building a legitimate partial-application K_CLO the Perceus pass
// owns and drops normally.
//
// OUTPUT-INVARIANCE: the rt_mkcon name offset REUSES em.intern(c.Name), the SAME interning
// emitCtorBlock uses (emitDefs interns every ctor name before any body is emitted, so this
// is an idempotent lookup, not a re-intern), and the tag / field order match emitCtorBlock
// exactly -- the printed K_CON is byte-identical to the curried path.
//
// This is DISTINCT from an accel / nat-eliminator spine: those are headed by a CGlobal
// naming a builtin op / eliminator def, never a constructor in ctorByName (the builtin-nat
// ctors are excluded), so the dispatch order does not shadow accelMatchC / NatElimSpine.
func (f *wasmFunc) satCtorDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	// Unwind the application spine into head + args (left-to-right), mirroring
	// accelMatchC. The Perceus pass keeps a recognized ctor spine's BACKBONE bare (only
	// the leaf args carry consumeOwning wrappers), so the Clo chain here is a bare
	// AppClosure chain ending in the head CGlobal.
	var args []CIr
	t := CIr(app)
	for {
		ap, isApp := t.(AppClosure)
		if !isApp {
			break
		}
		args = append([]CIr{ap.Arg}, args...)
		t = ap.Clo
	}
	g, isGlobal := t.(CGlobal)
	if !isGlobal {
		return "", false
	}
	c, isCtor := f.em.ctorByName[g.Name]
	if !isCtor || len(args) != c.Arity {
		return "", false
	}
	nameOff := f.em.intern(c.Name) // idempotent: emitDefs already interned every ctor name
	// Evaluate each arg into a stable local FIRST (so a nested AppClosure inside an arg
	// does not interleave with the rt_con_set fills), then build + fill the con. Mirrors
	// emitMkClosure's evaluate-then-alloc-then-set ordering.
	vals := make([]string, len(args))
	for i, a := range args {
		ev := f.emitIn(b, a, locals)
		r := f.fresh()
		fmt.Fprintf(b, "    (local.set %s %s)\n", r, ev)
		vals[i] = "(local.get " + r + ")"
	}
	o := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_mkcon (i32.const %d) (i32.const %d) (i32.const %d)))\n", o, c.Tag, nameOff, c.Arity)
	for i, v := range vals {
		fmt.Fprintf(b, "    (call $rt_con_set (local.get %s) (i32.const %d) %s)\n", o, i, v)
	}
	return "(local.get " + o + ")", true
}

// satElimDispatch recognizes a SATURATED builtin-nat eliminator application -- a
// NatElimSpine `NatElim mot z step n` with EXACTLY 4 args -- and runs the fold
// DIRECTLY: it builds ONLY the b3 fold closure (env {unit, z, step}) and applies it
// to n, instead of currying through the cached eliminator thunk's b0->b1->b2 chain
// (three intermediate partial-application K_CLOs that leak -- the eliminator analogue
// of the satCtorDispatch container leak). This is what makes an inline NatElim fold
// reach steady-flat.
//
// The erased MOTIVE is evaluated (running its consumeOwning side effects) and freed
// immediately -- the fold never reads env slot 0, so the motive is not stored. z and
// step are MOVED into the b3 env (the closure owns them); the b3 closure is RELEASED
// after the call, freeing them. n is borrow-read by the fold and RELEASED here
// afterward. All four spine args arrive OWNED (annotateBareSpine consumeOwning'd each
// leaf), so these releases are balanced; emitNatFold retains its $acc so the base
// survives the b3 release (0-iteration return, and the step's per-iteration consume).
//
// A non-4-arg (partial or over-applied) nat-elim spine returns false and falls through
// to the generic rt_apply path unchanged. The head is the eliminator def CGlobal, never
// a ctor in ctorByName nor an accel op, so the dispatch order does not shadow.
func (f *wasmFunc) satElimDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	if f.em.natElim == "" {
		return "", false
	}
	args, ok := NatElimSpine(f.em.natElim, app)
	if !ok || len(args) != 4 {
		return "", false
	}
	mot, z, step, n := args[0], args[1], args[2], args[3]
	// Erased motive: evaluate (for its annotated side effects) and release. Not stored.
	emot := f.emitIn(b, mot, locals)
	fmt.Fprintf(b, "    (call $rt_release %s)\n", emot)
	// z, step, n into stable locals (a nested AppClosure in one must not interleave the
	// closure fills); z and step are moved into the env, n is the fold bound.
	ez := f.emitIn(b, z, locals)
	rz := f.fresh()
	fmt.Fprintf(b, "    (local.set %s %s)\n", rz, ez)
	estep := f.emitIn(b, step, locals)
	rstep := f.fresh()
	fmt.Fprintf(b, "    (local.set %s %s)\n", rstep, estep)
	en := f.emitIn(b, n, locals)
	rn := f.fresh()
	fmt.Fprintf(b, "    (local.set %s %s)\n", rn, en)
	// Build the b3 fold closure {unit, z, step} and run it on n.
	b3idx := f.em.codeRef(wasmName(f.em.natElim) + "_b3")
	cl := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 3)))\n", cl, b3idx)
	fmt.Fprintf(b, "    (call $rt_clo_set (local.get %s) (i32.const 0) (call $rt_unit))\n", cl)
	fmt.Fprintf(b, "    (call $rt_clo_set (local.get %s) (i32.const 1) (local.get %s))\n", cl, rz)
	fmt.Fprintf(b, "    (call $rt_clo_set (local.get %s) (i32.const 2) (local.get %s))\n", cl, rstep)
	r := f.fresh()
	fmt.Fprintf(b, "    (local.set %s (call $rt_apply (local.get %s) (local.get %s)))\n", r, cl, rn)
	fmt.Fprintf(b, "    (call $rt_release (local.get %s))\n", cl) // frees env z + step
	fmt.Fprintf(b, "    (call $rt_release (local.get %s))\n", rn) // the bound, dead now
	return "(local.get " + r + ")", true
}

// accelDispatch emits a registered accel-op def on two args as native bignum arithmetic
// (the rt_nat_add/mul/monus helpers), interchangeable with the eliminator loop.
func (f *wasmFunc) accelDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	op, a, bb, ok := accelMatchC(app, f.em.accel)
	if !ok {
		return "", false
	}
	ea := f.emitIn(b, a, locals)
	eb := f.emitIn(b, bb, locals)
	r := f.fresh()
	var opName string
	switch op {
	case core.NatOpAdd:
		opName = "rt_nat_add"
	case core.NatOpMul:
		opName = "rt_nat_mul"
	case core.NatOpMonus:
		opName = "rt_nat_monus"
	default:
		return "", false
	}
	fmt.Fprintf(b, "    (local.set %s (call $%s %s %s))\n", r, opName, ea, eb)
	// The accel op BORROW-reads its two operands (rt_nat_* read both inputs and
	// return a fresh K_BIG) and has now read them, so both operands are dead. Every
	// operand reaching here is a PRIVATE OWNED reference: annotateBareSpine routes each
	// accel leaf through consumeOwning (a bare owned CVar stays a single private ref; a
	// borrowed CEnv/CGlobal/projection is dup'd to a fresh owned ref), and a local
	// consumed in both operand sub-spines is dup'd once by annotateBareSpine's
	// shared-owned-local dup, while a local also used outside the spine is dup'd by the
	// enclosing scope. So exactly one live reference reaches the op per operand, and the
	// op must free both -- the accel dual of succ_code freeing its $arg under PATH B.
	// Releasing unconditionally (not just fresh-producing forms) is what closes the
	// owned-CVar / dup'd-borrow operand leak; a borrowed root is never reached bare here
	// because consumeOwning already replaced it with an owned dup.
	fmt.Fprintf(b, "    (call $rt_release %s)\n", ea)
	fmt.Fprintf(b, "    (call $rt_release %s)\n", eb)
	return "(local.get " + r + ")", true
}

// emitForeignPrimsWasm bakes the WAT bodies for the supported foreign IO ops referenced
// by p, plus the IO monad (bindIO/pureIO) when the program uses IO. It mirrors the JS
// ioRuntime + the D6 host-body bake (js.go) and the C cIORuntime + printNat (c.go), but
// over the WASM value rep + ARC. Each op is a CURRIED IO-CLOSURE CHAIN (the load-bearing
// template Tasks 2-5 reuse verbatim):
//
//	accessor $def_<op>  : (result i32)                 -> mkclo(code_0, 0)   [the CGlobal thunk]
//	  code_0 (arg=v0, env={})     -> mkclo(code_1, 1); slot0 = v0            [bind a value arg]
//	  ...                                                                    [more value args]
//	  code_k (arg=world, env={..}) -> <perform effect>; return <owned value> [the world step]
//
// The world token is $rt_unit (immortal: no retain/release). ARC discipline: a code block
// receives its $arg OWNED (move it into a captured slot, or consume it) and its $env slots
// BORROWED (retain before forwarding into a child closure that will be released; never
// release the env here -- rt_free reclaims it when the closure dies). A world step that
// returns a captured/borrowed value RETAINS it first so the yielded result is an
// independent owned reference. Intermediate closures produced by rt_apply are released.
func (em *wasmEmitter) emitForeignPrimsWasm(b *strings.Builder, p Program) {
	if usesIO(p) {
		em.emitPureIOWasm(b)
		em.emitBindIOWasm(b)
	}
	if usesForeign(p, "printNat") {
		em.emitPrintNatWasm(b)
	}
	if usesForeign(p, "timeNanos") {
		em.emitTimeNanosWasm(b)
	}
	// Task 2: the packed-String codec + the 4 PURE bible ops. The codec ($big_divmod_small
	// + $d6_s2h/$d6_h2s) is baked once when any codec-consuming foreign is present; the
	// ops are curried accessor -> closure(arg) -> result chains (PURE: no world token).
	if usesBibleCodec(p) {
		b.WriteString(wasmBibleCodec)
	}
	if usesForeign(p, "byteLen") {
		em.emitByteLenWasm(b)
	}
	if usesForeign(p, "splitOn") {
		em.emitSplitOnWasm(b)
	}
	if usesForeign(p, "jsonStrField") {
		em.emitJsonStrFieldWasm(b)
	}
	if usesForeign(p, "sqlQuote") {
		em.emitSqlQuoteWasm(b)
	}
	// Task 3: the file-read WASI helper + the 2 HIGHER-ORDER file/dir ops. $d6_readfile is
	// baked once when any file/dir foreign is present (foldLines/foldDir both slurp files;
	// write-stream ops reuse $D6SYS from this helper for their fd_write iovec cells).
	// $d6_foldwalk only when foldDir is present. The ops are curried IO-closure chains that
	// run a host fold loop applying the erased Rune step per line / per file.
	if usesForeign(p, "foldLines") || usesForeign(p, "foldDir") ||
		usesForeign(p, "openWrite") || usesForeign(p, "writeChunk") ||
		usesForeign(p, "closeWrite") || usesForeign(p, "sortFile") ||
		usesForeign(p, "readFileCode") || usesForeign(p, "writeFileCode") {
		b.WriteString(wasmBibleReadFile)
	}
	if usesForeign(p, "foldLines") {
		em.emitFoldLinesWasm(b)
	}
	if usesForeign(p, "foldDir") {
		b.WriteString(wasmBibleFoldDir)
		em.emitFoldDirWasm(b)
	}
	// Task 4: write-stream ops (WASI file-write + linear-memory fd handle table). The
	// wasmBibleWriteOps fragment ($D6WH/$d6_whid/$d6_wopen) is baked once when any
	// write-stream op is present; it depends on wasmBibleReadFile ($D6SYS) above.
	// dbApply is a documented WASM sandbox no-op: sqlite3 subprocess is unavailable in
	// wasmtime without a WASI subprocess extension; the host (Task 6) runs sqlite3 instead.
	if usesForeign(p, "openWrite") || usesForeign(p, "writeChunk") ||
		usesForeign(p, "closeWrite") || usesForeign(p, "sortFile") ||
		usesForeign(p, "writeFileCode") {
		b.WriteString(wasmBibleWriteOps)
	}
	if usesForeign(p, "Handle") {
		em.emitHandleWasm(b)
	}
	if usesForeign(p, "openWrite") {
		em.emitOpenWriteWasm(b)
	}
	if usesForeign(p, "writeChunk") {
		em.emitWriteChunkWasm(b)
	}
	if usesForeign(p, "closeWrite") {
		em.emitCloseWriteWasm(b)
	}
	if usesForeign(p, "sortFile") {
		em.emitSortFileWasm(b)
	}
	if usesForeign(p, "dbApply") {
		em.emitDbApplyWasm(b)
	}
	// Task 5: the D6 env/argv/exit layer. wasmBibleEnvArgv ($d6_getenv/$d6_argat/
	// $d6_argcount + their scratch windows) is baked once when any of getEnvCode/
	// argAtCode/argCountCode is present -- independent of the Task-3/4 file windows
	// (ch216 uses argCountCode/argAtCode/exitWith/printStrCode with no file op).
	// readFileCode/writeFileCode instead reuse the Task-3/4 $d6_readfile/$d6_wopen
	// (already baked above by the wasmBibleReadFile/wasmBibleWriteOps gates).
	if usesForeign(p, "getEnvCode") || usesForeign(p, "argAtCode") || usesForeign(p, "argCountCode") {
		b.WriteString(wasmBibleEnvArgv)
	}
	if usesForeign(p, "printStrCode") {
		em.emitPrintStrCodeWasm(b)
	}
	if usesForeign(p, "getEnvCode") {
		em.emitGetEnvCodeWasm(b)
	}
	if usesForeign(p, "readFileCode") {
		em.emitReadFileCodeWasm(b)
	}
	if usesForeign(p, "writeFileCode") {
		em.emitWriteFileCodeWasm(b)
	}
	if usesForeign(p, "argCountCode") {
		em.emitArgCountCodeWasm(b)
	}
	if usesForeign(p, "argAtCode") {
		em.emitArgAtCodeWasm(b)
	}
	if usesForeign(p, "exitWith") {
		em.emitExitWithWasm(b)
	}
	// Task 3 (6c): the Phase-0 Bin vocabulary over K_BIN (usesBin, ioprims.go).
	if usesBin(p) {
		em.emitBinEmptyWasm(b)
		em.emitBinConsWasm(b)
		em.emitBinLenWasm(b)
		em.emitBinAtWasm(b)
		em.emitPrintBinWasm(b)
	}
	// Task 4b: machine-float (f64) IO host ops (parseFloat/getFloat/printFloat) + the base
	// D3 kit the float listings need (the Float TYPE, fromNat, fmul). The K_FLOAT box +
	// validate/atof/format runtime is baked once (emitFloatRuntimeWasm), before the per-op
	// emitters, so $D6FLT/$flt_* are declared ahead of their uses. parseFloat additionally
	// leans on the codec (usesBibleCodec includes it, so $d6_s2h_to/$D6BUF are present).
	if usesFloatWasm(p) {
		em.emitFloatRuntimeWasm(b)
	}
	if usesForeign(p, "Float") {
		em.emitFloatTypeWasm(b)
	}
	if usesForeign(p, "fromNat") {
		em.emitFromNatWasm(b)
	}
	if usesForeign(p, "fmul") {
		em.emitFmulWasm(b)
	}
	if usesForeign(p, "parseFloat") {
		em.emitParseFloatWasm(b)
	}
	if usesForeign(p, "getFloat") {
		em.emitGetFloatWasm(b)
	}
	if usesForeign(p, "printFloat") {
		em.emitPrintFloatWasm(b)
	}
}

// usesBibleCodec reports whether p references any foreign op that decodes/encodes a
// packed String via the codec ($d6_s2h/$d6_h2s), so the codec is baked exactly once.
func usesBibleCodec(p Program) bool {
	for _, op := range []string{
		"byteLen", "splitOn", "jsonStrField", "sqlQuote", "foldLines", "foldDir",
		// Task 4: write-stream ops all decode packed Strings (paths/content) via $d6_s2h.
		"openWrite", "writeChunk", "closeWrite", "sortFile", "dbApply",
		// Task 5: the D6 env/argv/exit layer decodes/encodes packed Strings too (exitWith
		// and argCountCode are the only two that carry no String, so they are excluded).
		"getEnvCode", "readFileCode", "writeFileCode", "printStrCode", "argAtCode",
		// Task 4b: parseFloat decodes its packed-String code via $d6_s2h_to into $D6BUF.
		"parseFloat",
	} {
		if usesForeign(p, op) {
			return true
		}
	}
	return false
}

// emitByteLenWasm bakes `byteLen : Nat -> Nat` -- decode the packed String, return its
// raw byte length. PURE: accessor -> closure(code) -> Nat. The owned $arg is consumed
// (released) since the returned value is a fresh bignum.
func (em *wasmEmitter) emitByteLenWasm(b *strings.Builder) {
	c1 := em.codeRef("byteLen_c1")
	b.WriteString("  (func $byteLen_c1 (param $arg i32) (param $env i32) (result i32) (local $len i32) (local $r i32)\n")
	b.WriteString("    (call $d6_s2h (local.get $arg))\n")
	b.WriteString("    (local.set $len)\n") // pop len (top)
	b.WriteString("    (drop)\n")           // pop buf (deeper, unused)
	b.WriteString("    (local.set $r (call $rt_big_from_long (local.get $len)))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	em.emitCachedThunk(b, "byteLen", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitSplitOnWasm bakes `splitOn : Nat -> Nat -> List Nat` -- split the decoded String on
// a single separator byte, returning a cons list of segment Strings. 2-arg curried:
// accessor -> closure(sep) -> closure(code) -> list. The list is built in forward order
// by scanning the byte buffer right-to-left and prepending each segment onto the tail.
func (em *wasmEmitter) emitSplitOnWasm(b *strings.Builder) {
	nilOff := em.intern("nil")
	consOff := em.intern("cons")
	c1 := em.codeRef("splitOn_c1")
	c2 := em.codeRef("splitOn_c2")
	// c2: env={sep}, arg=code. Decode + segment + build the list.
	b.WriteString("  (func $splitOn_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $sep i32) (local $sb i32) (local $len i32) (local $buf i32)\n")
	b.WriteString("    (local $lst i32) (local $i i32) (local $segEnd i32) (local $part i32) (local $cons i32)\n")
	b.WriteString("    (local.set $sep (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (if (i32.gt_s (call $big_nlimbs (local.get $sep)) (i32.const 0))\n")
	b.WriteString("      (then (local.set $sb (i32.and (call $big_limb (local.get $sep) (i32.const 0)) (i32.const 255))))\n")
	b.WriteString("      (else (local.set $sb (i32.const 0))))\n")
	b.WriteString("    (call $d6_s2h (local.get $arg))\n")
	b.WriteString("    (local.set $len)\n")
	b.WriteString("    (local.set $buf)\n")
	fmt.Fprintf(b, "    (local.set $lst (call $rt_mkcon (i32.const 0) (i32.const %d) (i32.const 1)))\n", nilOff)
	b.WriteString("    (call $rt_con_set (local.get $lst) (i32.const 0) (call $rt_unit))\n")
	b.WriteString("    (local.set $segEnd (local.get $len))\n")
	b.WriteString("    (local.set $i (local.get $len))\n")
	b.WriteString("    (block $done (loop $lp\n")
	b.WriteString("      (br_if $done (i32.eqz (local.get $i)))\n")
	b.WriteString("      (local.set $i (i32.sub (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (if (i32.eq (i32.load8_u (i32.add (local.get $buf) (local.get $i))) (local.get $sb))\n")
	b.WriteString("        (then\n")
	b.WriteString("          (local.set $part (call $d6_h2s (i32.add (local.get $buf) (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("                                         (i32.sub (local.get $segEnd) (i32.add (local.get $i) (i32.const 1)))))\n")
	fmt.Fprintf(b, "          (local.set $cons (call $rt_mkcon (i32.const 1) (i32.const %d) (i32.const 3)))\n", consOff)
	b.WriteString("          (call $rt_con_set (local.get $cons) (i32.const 0) (call $rt_unit))\n")
	b.WriteString("          (call $rt_con_set (local.get $cons) (i32.const 1) (local.get $part))\n")
	b.WriteString("          (call $rt_con_set (local.get $cons) (i32.const 2) (local.get $lst))\n")
	b.WriteString("          (local.set $lst (local.get $cons))\n")
	b.WriteString("          (local.set $segEnd (local.get $i))))\n")
	b.WriteString("      (br $lp)))\n")
	// final segment [0, segEnd)
	b.WriteString("    (local.set $part (call $d6_h2s (local.get $buf) (local.get $segEnd)))\n")
	fmt.Fprintf(b, "    (local.set $cons (call $rt_mkcon (i32.const 1) (i32.const %d) (i32.const 3)))\n", consOff)
	b.WriteString("    (call $rt_con_set (local.get $cons) (i32.const 0) (call $rt_unit))\n")
	b.WriteString("    (call $rt_con_set (local.get $cons) (i32.const 1) (local.get $part))\n")
	b.WriteString("    (call $rt_con_set (local.get $cons) (i32.const 2) (local.get $lst))\n")
	b.WriteString("    (local.set $lst (local.get $cons))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $lst))\n")
	// c1: arg=sep (owned), build c2 capturing sep (MOVE).
	fmt.Fprintf(b, "  (func $splitOn_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "splitOn", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitJsonStrFieldWasm bakes `jsonStrField : Nat -> Nat -> Option Nat` -- extract a top-
// level JSON string field's value. 2-arg curried: accessor -> closure(field) ->
// closure(doc) -> Option. Builds needle = '"' + field + '"', finds it in doc, skips
// whitespace/':' , and if the value is a quoted string returns some(value) else none.
// Uses three scratch windows: field bytes in $D6BUF2, doc bytes in $D6BUF, needle in
// $D6BUF3 (so field and doc decodes do not clobber each other).
func (em *wasmEmitter) emitJsonStrFieldWasm(b *strings.Builder) {
	noneOff := em.intern("none")
	someOff := em.intern("some")
	c1 := em.codeRef("jsonStrField_c1")
	c2 := em.codeRef("jsonStrField_c2")
	b.WriteString("  (func $jsonStrField_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $field i32) (local $fl i32) (local $dl i32) (local $nl i32)\n")
	b.WriteString("    (local $needle i32) (local $ds i32) (local $found i32)\n")
	b.WriteString("    (local $i i32) (local $j i32) (local $k i32) (local $ch i32)\n")
	b.WriteString("    (local $result i32) (local $val i32) (local $some i32)\n")
	b.WriteString("    (local.set $field (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $fl (call $d6_s2h_to (local.get $field) (global.get $D6BUF2)))\n")
	b.WriteString("    (local.set $dl (call $d6_s2h_to (local.get $arg) (global.get $D6BUF)))\n")
	b.WriteString("    (local.set $ds (global.get $D6BUF))\n")
	// needle = '"' + field + '"' in D6BUF3
	b.WriteString("    (local.set $needle (global.get $D6BUF3))\n")
	b.WriteString("    (i32.store8 (local.get $needle) (i32.const 34))\n")
	b.WriteString("    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $cpb (loop $cpl\n")
	b.WriteString("      (br_if $cpb (i32.ge_s (local.get $i) (local.get $fl)))\n")
	b.WriteString("      (i32.store8 (i32.add (local.get $needle) (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("                  (i32.load8_u (i32.add (global.get $D6BUF2) (local.get $i))))\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $cpl)))\n")
	b.WriteString("    (i32.store8 (i32.add (local.get $needle) (i32.add (local.get $fl) (i32.const 1))) (i32.const 34))\n")
	b.WriteString("    (local.set $nl (i32.add (local.get $fl) (i32.const 2)))\n")
	// result = none
	fmt.Fprintf(b, "    (local.set $result (call $rt_mkcon (i32.const 0) (i32.const %d) (i32.const 1)))\n", noneOff)
	b.WriteString("    (call $rt_con_set (local.get $result) (i32.const 0) (call $rt_unit))\n")
	// search doc for needle
	b.WriteString("    (local.set $found (i32.const -1))\n")
	b.WriteString("    (if (i32.le_s (local.get $nl) (local.get $dl))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (local.set $i (i32.const 0))\n")
	b.WriteString("        (block $sb (loop $sl\n")
	b.WriteString("          (br_if $sb (i32.gt_s (i32.add (local.get $i) (local.get $nl)) (local.get $dl)))\n")
	b.WriteString("          (if (call $d6_memeq (i32.add (local.get $ds) (local.get $i)) (local.get $needle) (local.get $nl))\n")
	b.WriteString("            (then (local.set $found (local.get $i)) (br $sb)))\n")
	b.WriteString("          (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("          (br $sl)))))\n")
	b.WriteString("    (if (i32.ge_s (local.get $found) (i32.const 0))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (local.set $j (i32.add (local.get $found) (local.get $nl)))\n")
	// skip ' ' | '\t' | ':'
	b.WriteString("        (block $skb (loop $skl\n")
	b.WriteString("          (br_if $skb (i32.ge_s (local.get $j) (local.get $dl)))\n")
	b.WriteString("          (local.set $ch (i32.load8_u (i32.add (local.get $ds) (local.get $j))))\n")
	b.WriteString("          (br_if $skb (i32.eqz (i32.or (i32.or (i32.eq (local.get $ch) (i32.const 32))\n")
	b.WriteString("                                               (i32.eq (local.get $ch) (i32.const 9)))\n")
	b.WriteString("                                       (i32.eq (local.get $ch) (i32.const 58)))))\n")
	b.WriteString("          (local.set $j (i32.add (local.get $j) (i32.const 1)))\n")
	b.WriteString("          (br $skl)))\n")
	b.WriteString("        (if (i32.and (i32.lt_s (local.get $j) (local.get $dl))\n")
	b.WriteString("                     (i32.eq (i32.load8_u (i32.add (local.get $ds) (local.get $j))) (i32.const 34)))\n")
	b.WriteString("          (then\n")
	b.WriteString("            (local.set $j (i32.add (local.get $j) (i32.const 1)))\n")
	b.WriteString("            (local.set $k (local.get $j))\n")
	b.WriteString("            (block $vb (loop $vl\n")
	b.WriteString("              (br_if $vb (i32.ge_s (local.get $k) (local.get $dl)))\n")
	b.WriteString("              (br_if $vb (i32.eq (i32.load8_u (i32.add (local.get $ds) (local.get $k))) (i32.const 34)))\n")
	b.WriteString("              (local.set $k (i32.add (local.get $k) (i32.const 1)))\n")
	b.WriteString("              (br $vl)))\n")
	b.WriteString("            (local.set $val (call $d6_h2s (i32.add (local.get $ds) (local.get $j)) (i32.sub (local.get $k) (local.get $j))))\n")
	fmt.Fprintf(b, "            (local.set $some (call $rt_mkcon (i32.const 1) (i32.const %d) (i32.const 2)))\n", someOff)
	b.WriteString("            (call $rt_con_set (local.get $some) (i32.const 0) (call $rt_unit))\n")
	b.WriteString("            (call $rt_con_set (local.get $some) (i32.const 1) (local.get $val))\n")
	b.WriteString("            (call $rt_release (local.get $result))\n") // drop the none we built
	b.WriteString("            (local.set $result (local.get $some))))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n") // doc owned; field borrowed via env
	b.WriteString("    (local.get $result))\n")
	// c1: arg=field (owned), build c2 capturing field (MOVE).
	fmt.Fprintf(b, "  (func $jsonStrField_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "jsonStrField", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitSqlQuoteWasm bakes `sqlQuote : Nat -> Nat` -- wrap the decoded String in single
// quotes, doubling each embedded quote, then re-encode. PURE 1-arg: accessor ->
// closure(s) -> String. Input bytes in $D6BUF, output built in $D6BUF2.
func (em *wasmEmitter) emitSqlQuoteWasm(b *strings.Builder) {
	c1 := em.codeRef("sqlQuote_c1")
	b.WriteString("  (func $sqlQuote_c1 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $len i32) (local $buf i32) (local $out i32) (local $o i32) (local $i i32) (local $ch i32) (local $r i32)\n")
	b.WriteString("    (local.set $len (call $d6_s2h_to (local.get $arg) (global.get $D6BUF)))\n")
	b.WriteString("    (local.set $buf (global.get $D6BUF))\n")
	b.WriteString("    (local.set $out (global.get $D6BUF2))\n")
	b.WriteString("    (local.set $o (i32.const 0))\n")
	b.WriteString("    (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 39))\n") // "'"
	b.WriteString("    (local.set $o (i32.add (local.get $o) (i32.const 1)))\n")
	b.WriteString("    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $done (loop $lp\n")
	b.WriteString("      (br_if $done (i32.ge_s (local.get $i) (local.get $len)))\n")
	b.WriteString("      (local.set $ch (i32.load8_u (i32.add (local.get $buf) (local.get $i))))\n")
	b.WriteString("      (if (i32.eq (local.get $ch) (i32.const 39))\n")
	b.WriteString("        (then\n")
	b.WriteString("          (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 39))\n")
	b.WriteString("          (local.set $o (i32.add (local.get $o) (i32.const 1)))))\n")
	b.WriteString("      (i32.store8 (i32.add (local.get $out) (local.get $o)) (local.get $ch))\n")
	b.WriteString("      (local.set $o (i32.add (local.get $o) (i32.const 1)))\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $lp)))\n")
	b.WriteString("    (i32.store8 (i32.add (local.get $out) (local.get $o)) (i32.const 39))\n")
	b.WriteString("    (local.set $o (i32.add (local.get $o) (i32.const 1)))\n")
	b.WriteString("    (local.set $r (call $d6_h2s (local.get $out) (local.get $o)))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	em.emitCachedThunk(b, "sqlQuote", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFoldLinesWasm bakes `foldLines : (S:U) -> Nat -> (S -> Nat -> IO S) -> S -> IO S` --
// stream a file line by line, folding the erased Rune step over each line. 5-arg curried
// IO-closure chain (_s)(path)(step)(s0)(world): the world step (c5, env={step,s0,path})
// slurps the file via $d6_readfile, splits the bytes on '\n' KEEPING '\r' (drops the single
// trailing empty segment iff the buffer ends in '\n' -- i.e. the final segment is applied
// only when non-empty), and folds `s = apply(apply(apply(step, s), $d6_h2s(seg)), unit)`.
// On a missing file ($d6_readfile -> (0,0)) it returns s0 unchanged. Ports rust.go:207 /
// the C foldLines_c5 byte-for-byte.
//
// ARC: env slots (step/s0/path) are BORROWED -- never released here (rt_free reclaims them
// when c5's closure dies). The fold's `s` is threaded by ownership: s0 is retained once so
// the first apply consumes an owned ref, each rt_apply(step,s) consumes the current s and
// yields a fresh owned intermediate (step stays borrowed), the intermediate apply closures
// $ca/$cb are released, and the final s is returned owned. $d6_h2s lines are owned and
// consumed by the apply chain (Perceus releases the arg inside the step body).
func (em *wasmEmitter) emitFoldLinesWasm(b *strings.Builder) {
	c1 := em.codeRef("foldLines_c1")
	c2 := em.codeRef("foldLines_c2")
	c3 := em.codeRef("foldLines_c3")
	c4 := em.codeRef("foldLines_c4")
	c5 := em.codeRef("foldLines_c5")
	// c5: env={step,s0,path}, arg=world. Slurp + split + fold.
	b.WriteString("  (func $foldLines_c5 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $path i32) (local $plen i32) (local $buf i32) (local $len i32)\n")
	b.WriteString("    (local $s i32) (local $i i32) (local $segStart i32) (local $emit i32)\n")
	b.WriteString("    (local $line i32) (local $ca i32) (local $cb i32)\n")
	b.WriteString("    (local.set $path (call $rt_env (local.get $env) (i32.const 2)))\n")
	b.WriteString("    (call $d6_s2h (local.get $path))\n")
	b.WriteString("    (local.set $plen)\n") // pop len (top)
	b.WriteString("    (drop)\n")            // pop buf ptr (D6BUF; readfile reuses its own window)
	b.WriteString("    (call $d6_readfile (global.get $D6BUF) (local.get $plen))\n")
	b.WriteString("    (local.set $len)\n") // pop len (top)
	b.WriteString("    (local.set $buf)\n") // pop buf ptr
	// s := s0 (env[1]); retain so it is an owned reference the fold can consume.
	b.WriteString("    (local.set $s (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (call $rt_retain (local.get $s))\n")
	// missing file: $d6_readfile returned (0,0) -> return s0 unchanged (already retained).
	b.WriteString("    (if (i32.eqz (local.get $buf)) (then (return (local.get $s))))\n")
	// stream-split on '\n', keeping '\r'; k runs 0..len INCLUSIVE (k==len emits the tail
	// iff non-empty -- the single-trailing-empty drop).
	b.WriteString("    (local.set $segStart (i32.const 0))\n")
	b.WriteString("    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $done (loop $lp\n")
	b.WriteString("      (local.set $emit (i32.const 0))\n")
	b.WriteString("      (if (i32.lt_s (local.get $i) (local.get $len))\n")
	b.WriteString("        (then (if (i32.eq (i32.load8_u (i32.add (local.get $buf) (local.get $i))) (i32.const 10))\n")
	b.WriteString("                (then (local.set $emit (i32.const 1)))))\n")
	b.WriteString("        (else (if (i32.gt_s (local.get $len) (local.get $segStart)) (then (local.set $emit (i32.const 1))))))\n")
	b.WriteString("      (if (local.get $emit)\n")
	b.WriteString("        (then\n")
	b.WriteString("          (local.set $line (call $d6_h2s (i32.add (local.get $buf) (local.get $segStart))\n")
	b.WriteString("                                         (i32.sub (local.get $i) (local.get $segStart))))\n")
	b.WriteString("          (local.set $ca (call $rt_apply (call $rt_env (local.get $env) (i32.const 0)) (local.get $s)))\n")
	b.WriteString("          (local.set $cb (call $rt_apply (local.get $ca) (local.get $line)))\n")
	b.WriteString("          (call $rt_release (local.get $ca))\n")
	b.WriteString("          (local.set $s (call $rt_apply (local.get $cb) (call $rt_unit)))\n")
	b.WriteString("          (call $rt_release (local.get $cb))\n")
	b.WriteString("          (local.set $segStart (i32.add (local.get $i) (i32.const 1)))))\n")
	b.WriteString("      (br_if $done (i32.ge_s (local.get $i) (local.get $len)))\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $lp)))\n")
	b.WriteString("    (local.get $s))\n")
	// c4: env={step,path}, arg=s0 (owned). Build c5 {step(retain),s0(move),path(retain)}.
	fmt.Fprintf(b, "  (func $foldLines_c4 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 3)))\n", c5)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 2) (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (local.get $c))\n")
	// c3: env={path}, arg=step (owned). Build c4 {step(move),path(retain)}.
	fmt.Fprintf(b, "  (func $foldLines_c3 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", c4)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.get $c))\n")
	// c2: arg=path (owned). Build c3 {path(move)}.
	fmt.Fprintf(b, "  (func $foldLines_c2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c3)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// c1: arg=S (type/unit, discarded). Return mkclo(c2, 0).
	fmt.Fprintf(b, "  (func $foldLines_c1 (param $arg i32) (param $env i32) (result i32)\n")
	fmt.Fprintf(b, "    (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", c2)
	em.emitCachedThunk(b, "foldLines", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitFoldDirWasm bakes `foldDir : (S:U) -> Nat -> Nat -> (S -> Nat -> IO S) -> S -> IO S`
// -- recursively walk a directory (sorted, suffix-filtered, depth-first pre-order) folding
// the erased step over each matching file's contents. 6-arg curried IO-closure chain
// (_s)(dir)(suffix)(step)(s0)(world): the world step (c6) decodes the dir path into
// $D6FDDIR and the suffix into $D6FDSUF, retains s0, and runs the recursive $d6_foldwalk
// (which owns + returns the threaded accumulator). Ports rust.go:213 / the C d6_foldwalk.
//
// ARC: env slots (step/s0/dirc/suf) are BORROWED; s0 is retained before it enters the fold
// (foldwalk consumes it and returns an owned result). $d6_foldwalk applies the borrowed
// step exactly as foldLines does. The decode windows MUST be foldDir's OWN dedicated
// $D6FDDIR (dir) / $D6FDSUF (suffix) -- NOT the shared codec windows $D6BUF/$D6BUF2 -- since
// the applied step runs INSIDE the walk and may itself invoke a codec-consuming op
// (jsonStrField/splitOn/sqlQuote/byteLen), which would clobber $D6BUF/$D6BUF2 as its own
// throwaway scratch after the first matched file, silently breaking every subsequent
// entry's suffix comparison (the bug the 9-way TestBibleConformanceBuilders divergence-lock
// caught on ch555/ch559: only the walk's first file was ever folded). foldwalk otherwise
// touches only $D6DIR/$D6PATH/$D6NAMES/$D6SYS/$D6SLURP + the heap, never the codec windows.
func (em *wasmEmitter) emitFoldDirWasm(b *strings.Builder) {
	c1 := em.codeRef("foldDir_c1")
	c2 := em.codeRef("foldDir_c2")
	c3 := em.codeRef("foldDir_c3")
	c4 := em.codeRef("foldDir_c4")
	c5 := em.codeRef("foldDir_c5")
	c6 := em.codeRef("foldDir_c6")
	// c6: env={step,s0,dirc,suf}, arg=world. Decode dir + suffix, walk.
	b.WriteString("  (func $foldDir_c6 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $dirLen i32) (local $sufLen i32) (local $s i32)\n")
	b.WriteString("    (local.set $dirLen (call $d6_s2h_to (call $rt_env (local.get $env) (i32.const 2)) (global.get $D6FDDIR)))\n")
	b.WriteString("    (local.set $sufLen (call $d6_s2h_to (call $rt_env (local.get $env) (i32.const 3)) (global.get $D6FDSUF)))\n")
	b.WriteString("    (local.set $s (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (call $rt_retain (local.get $s))\n")
	b.WriteString("    (call $d6_foldwalk (global.get $D6FDDIR) (local.get $dirLen) (global.get $D6FDSUF) (local.get $sufLen)\n")
	b.WriteString("      (call $rt_env (local.get $env) (i32.const 0)) (local.get $s)))\n")
	// c5: env={step,dirc,suf}, arg=s0 (owned). Build c6 {step,s0,dirc,suf}.
	fmt.Fprintf(b, "  (func $foldDir_c5 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 4)))\n", c6)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 2) (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 2)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 3) (call $rt_env (local.get $env) (i32.const 2)))\n")
	b.WriteString("    (local.get $c))\n")
	// c4: env={dirc,suf}, arg=step (owned). Build c5 {step(move),dirc,suf}.
	fmt.Fprintf(b, "  (func $foldDir_c4 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 3)))\n", c5)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 2) (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (local.get $c))\n")
	// c3: env={dirc}, arg=suf (owned). Build c4 {dirc(retain),suf(move)}.
	fmt.Fprintf(b, "  (func $foldDir_c3 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", c4)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// c2: arg=dirc (owned). Build c3 {dirc(move)}.
	fmt.Fprintf(b, "  (func $foldDir_c2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c3)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// c1: arg=S (type/unit, discarded). Return mkclo(c2, 0).
	fmt.Fprintf(b, "  (func $foldDir_c1 (param $arg i32) (param $env i32) (result i32)\n")
	fmt.Fprintf(b, "    (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", c2)
	em.emitCachedThunk(b, "foldDir", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitHandleWasm bakes `Handle : U` -- the opaque write-stream handle type. In the erased
// program Handle is a foreign TYPE that evaluates to unit (it carries no runtime payload;
// the actual handle is a bignum id threaded through the IO chain). The accessor thunk
// returns $rt_unit (the immortal singleton).
func (em *wasmEmitter) emitHandleWasm(b *strings.Builder) {
	em.emitCachedThunk(b, "Handle", func(_ *wasmFunc, _ *strings.Builder) string {
		return "(call $rt_unit)"
	})
}

// emitOpenWriteWasm bakes `openWrite : Nat -> IO Handle` -- create/truncate a file and
// return a handle (small-int id into the $D6WH fd table). 2-step curried IO-closure chain:
// c1(path→captured) → c2(world→effect). The world step (c2) decodes the path via $d6_s2h
// into $D6BUF, calls $d6_wopen, stores the fd in $D6WH[++$d6_whid], and returns an owned
// bignum id. On path_open failure returns big(0) (the "null handle").
//
// ARC: path arg is MOVED into c2's env[0] by c1; c2 reads env[0] BORROWED (never released;
// rt_free reclaims the env slot when c2's closure is freed). World arg ($rt_unit) is
// immortal. The returned bignum (rt_big_from_long) is a freshly allocated owned ref.
func (em *wasmEmitter) emitOpenWriteWasm(b *strings.Builder) {
	c1 := em.codeRef("openWrite_c1")
	c2 := em.codeRef("openWrite_c2")
	// c2: env=[path], arg=world. Decode path, open file, return id bignum.
	b.WriteString("  (func $openWrite_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $plen i32) (local $fd i32) (local $id i32)\n")
	b.WriteString("    (call $d6_s2h (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $plen)\n") // pop len (top); d6_s2h returns (D6BUF, len)
	b.WriteString("    (drop)\n")            // discard buf ptr (always $D6BUF)
	// check table not full (slot 0 = invalid; slots 1..255 valid)
	b.WriteString("    (if (i32.ge_s (global.get $d6_whid) (i32.const 255))\n")
	b.WriteString("      (then (return (call $rt_big_from_long (i32.const 0)))))\n")
	// path_open O_CREAT|O_TRUNC=9, rights=FD_WRITE=0x40=64
	b.WriteString("    (local.set $fd (call $d6_wopen (global.get $D6BUF) (local.get $plen)))\n")
	b.WriteString("    (if (i32.lt_s (local.get $fd) (i32.const 0))\n")
	b.WriteString("      (then (return (call $rt_big_from_long (i32.const 0)))))\n")
	// allocate slot: ++id, store fd at D6WH[id]
	b.WriteString("    (local.set $id (i32.add (global.get $d6_whid) (i32.const 1)))\n")
	b.WriteString("    (global.set $d6_whid (local.get $id))\n")
	b.WriteString("    (i32.store\n")
	b.WriteString("      (i32.add (global.get $D6WH) (i32.shl (local.get $id) (i32.const 2)))\n")
	b.WriteString("      (local.get $fd))\n")
	b.WriteString("    (call $rt_big_from_long (local.get $id)))\n")
	// c1: arg=path (owned). Build c2 {path(move)}.
	fmt.Fprintf(b, "  (func $openWrite_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "openWrite", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitWriteChunkWasm bakes `writeChunk : Handle -> Nat -> IO Handle` -- append content
// bytes + '\n' to the file behind a handle. 3-step curried IO-closure chain: c1(h→env) →
// c2(chunk→env) → c3(world→effect). The world step (c3) reads the handle id from env[0],
// decodes the chunk string via $d6_s2h into $D6BUF, then issues TWO fd_write calls via
// the $D6SYS iovec: one for the chunk bytes, one for the single '\n' (0x0A) byte stored
// at $D6SYS+12. Returns h (env[0]) as an owned reference (RETAINED: env is borrowed; the
// env slot decrement after the call would otherwise drop the only ref).
//
// ARC: h MOVED by c1 into c2's env[0]; c2 RETAINS env[0] (h) when building c3's env[0]
// and MOVES chunk (arg) into c3's env[1]. c3 borrows both; retains h before returning.
func (em *wasmEmitter) emitWriteChunkWasm(b *strings.Builder) {
	c1 := em.codeRef("writeChunk_c1")
	c2 := em.codeRef("writeChunk_c2")
	c3 := em.codeRef("writeChunk_c3")
	// c3: env=[h, chunk], arg=world. Write chunk+'\n', return h (owned).
	b.WriteString("  (func $writeChunk_c3 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $h i32) (local $id i32) (local $fd i32) (local $clen i32)\n")
	b.WriteString("    (local.set $h (call $rt_env (local.get $env) (i32.const 0)))\n")
	// get handle id: if nlimbs==0 the bignum is 0 (error handle)
	b.WriteString("    (if (i32.gt_s (call $big_nlimbs (local.get $h)) (i32.const 0))\n")
	b.WriteString("      (then (local.set $id (call $big_limb (local.get $h) (i32.const 0))))\n")
	b.WriteString("      (else (local.set $id (i32.const 0))))\n")
	// decode chunk into $D6BUF
	b.WriteString("    (call $d6_s2h (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (local.set $clen)\n")
	b.WriteString("    (drop)\n")
	// write if id valid (1..255) and slot is occupied (fd != 0)
	b.WriteString("    (if (i32.and\n")
	b.WriteString("          (i32.and (i32.gt_s (local.get $id) (i32.const 0))\n")
	b.WriteString("                   (i32.lt_s (local.get $id) (i32.const 256)))\n")
	b.WriteString("          (i32.ne (i32.load (i32.add (global.get $D6WH) (i32.shl (local.get $id) (i32.const 2)))) (i32.const 0)))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (local.set $fd (i32.load (i32.add (global.get $D6WH) (i32.shl (local.get $id) (i32.const 2)))))\n")
	// fd_write iovec for chunk bytes: ptr=$D6BUF, len=$clen at $D6SYS+0/+4, nwritten at +24
	b.WriteString("        (i32.store (global.get $D6SYS) (global.get $D6BUF))\n")
	b.WriteString("        (i32.store (i32.add (global.get $D6SYS) (i32.const 4)) (local.get $clen))\n")
	b.WriteString("        (drop (call $fd_write (local.get $fd) (global.get $D6SYS) (i32.const 1) (i32.add (global.get $D6SYS) (i32.const 24))))\n")
	// fd_write '\n' (0x0A) stored at $D6SYS+12; iovec reused at $D6SYS+0/+4
	b.WriteString("        (i32.store8 (i32.add (global.get $D6SYS) (i32.const 12)) (i32.const 10))\n")
	b.WriteString("        (i32.store (global.get $D6SYS) (i32.add (global.get $D6SYS) (i32.const 12)))\n")
	b.WriteString("        (i32.store (i32.add (global.get $D6SYS) (i32.const 4)) (i32.const 1))\n")
	b.WriteString("        (drop (call $fd_write (local.get $fd) (global.get $D6SYS) (i32.const 1) (i32.add (global.get $D6SYS) (i32.const 24))))))\n")
	// return h owned: retain (env is borrowed; rt_free releases env[0] when c3's closure dies)
	b.WriteString("    (call $rt_retain (local.get $h))\n")
	b.WriteString("    (local.get $h))\n")
	// c2: env=[h], arg=chunk (owned). Build c3 {h(retain), chunk(move)}.
	fmt.Fprintf(b, "  (func $writeChunk_c2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", c3)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// c1: arg=h (owned). Build c2 {h(move)}.
	fmt.Fprintf(b, "  (func $writeChunk_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "writeChunk", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitCloseWriteWasm bakes `closeWrite : Handle -> IO Unit` -- flush + close the file
// behind a handle. 2-step curried IO-closure chain: c1(h→env) → c2(world→effect). The
// world step (c2) extracts the fd from $D6WH[id], calls fd_close, zeros the slot (so
// subsequent writeChunk/closeWrite on a stale handle is a no-op), then returns $rt_unit.
//
// ARC: h MOVED into c2's env[0] by c1; c2 borrows env[0] (never released here). World arg
// is immortal. Unit is the immortal singleton -- no allocation, no retain.
func (em *wasmEmitter) emitCloseWriteWasm(b *strings.Builder) {
	c1 := em.codeRef("closeWrite_c1")
	c2 := em.codeRef("closeWrite_c2")
	// c2: env=[h], arg=world. Close fd, clear slot, return unit.
	b.WriteString("  (func $closeWrite_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $h i32) (local $id i32) (local $slot i32)\n")
	b.WriteString("    (local.set $h (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (if (i32.gt_s (call $big_nlimbs (local.get $h)) (i32.const 0))\n")
	b.WriteString("      (then (local.set $id (call $big_limb (local.get $h) (i32.const 0))))\n")
	b.WriteString("      (else (local.set $id (i32.const 0))))\n")
	b.WriteString("    (if (i32.and\n")
	b.WriteString("          (i32.and (i32.gt_s (local.get $id) (i32.const 0))\n")
	b.WriteString("                   (i32.lt_s (local.get $id) (i32.const 256)))\n")
	b.WriteString("          (i32.ne (i32.load (i32.add (global.get $D6WH) (i32.shl (local.get $id) (i32.const 2)))) (i32.const 0)))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (local.set $slot (i32.add (global.get $D6WH) (i32.shl (local.get $id) (i32.const 2))))\n")
	b.WriteString("        (drop (call $fd_close (i32.load (local.get $slot))))\n")
	b.WriteString("        (i32.store (local.get $slot) (i32.const 0))))\n")
	b.WriteString("    (call $rt_unit))\n")
	// c1: arg=h (owned). Build c2 {h(move)}.
	fmt.Fprintf(b, "  (func $closeWrite_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "closeWrite", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitSortFileWasm bakes `sortFile : Nat -> Nat -> IO Unit` -- read the input file path
// (env[0]) via $d6_readfile into $D6SLURP, split on '\n' (keeping '\r'), drop the single
// trailing-empty segment iff the file ended with '\n', bytewise insertion-sort the
// segments via $d6_linecmp (which is identical to $d6_pathcmp but a separate function to
// avoid duplicate-name conflicts when foldDir is also present), write-open the output path
// (env[1]) via $d6_wopen, write each sorted line + '\n' via fd_write, close, return unit.
// On read failure (file missing or unreadable): write-open + close the output (empty file).
//
// Segment index: stored in $D6BUF3 as (ptr i32)(len i32) pairs = 8 bytes each (up to 8192
// segments; fine for any bible file). The ptr values are absolute pointers into $D6SLURP.
// The $D6SYS iovec ($D6SYS+0/+4, nwritten at +24) and the '\n' scratch cell ($D6SYS+12)
// are reused from the write-chunk pattern. Ports rust.go:213 / the C sortFile_c3 exactly.
//
// ARC: env=[inp,out] BORROWED. inp is READ only (used to call $d6_readfile). out is READ
// only (used to call $d6_wopen). Neither is retained or released in this body (rt_free
// reclaims both env slots when c3's closure dies). World arg and $rt_unit are immortal.
func (em *wasmEmitter) emitSortFileWasm(b *strings.Builder) {
	c1 := em.codeRef("sortFile_c1")
	c2 := em.codeRef("sortFile_c2")
	c3 := em.codeRef("sortFile_c3")
	// $d6_linecmp: bytewise comparison for insertion sort (cf. $d6_pathcmp in foldDir;
	// distinct name to avoid duplicate-function errors when foldDir is also present).
	b.WriteString("  (func $d6_linecmp (param $a i32) (param $na i32) (param $b i32) (param $nb i32) (result i32)\n")
	b.WriteString("    (local $i i32) (local $m i32) (local $x i32) (local $y i32)\n")
	b.WriteString("    (local.set $m (select (local.get $na) (local.get $nb) (i32.lt_s (local.get $na) (local.get $nb))))\n")
	b.WriteString("    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $done (loop $lp\n")
	b.WriteString("      (br_if $done (i32.ge_s (local.get $i) (local.get $m)))\n")
	b.WriteString("      (local.set $x (i32.load8_u (i32.add (local.get $a) (local.get $i))))\n")
	b.WriteString("      (local.set $y (i32.load8_u (i32.add (local.get $b) (local.get $i))))\n")
	b.WriteString("      (if (i32.ne (local.get $x) (local.get $y))\n")
	b.WriteString("        (then (return (select (i32.const -1) (i32.const 1) (i32.lt_u (local.get $x) (local.get $y))))))\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $lp)))\n")
	b.WriteString("    (if (i32.ne (local.get $na) (local.get $nb))\n")
	b.WriteString("      (then (return (select (i32.const -1) (i32.const 1) (i32.lt_s (local.get $na) (local.get $nb))))))\n")
	b.WriteString("    (i32.const 0))\n")
	// c3: env=[inp,out], arg=world. Read inp, sort, write out, return unit.
	b.WriteString("  (func $sortFile_c3 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $iplen i32) (local $oplen i32) (local $buf i32) (local $len i32)\n")
	b.WriteString("    (local $k i32) (local $nseg i32) (local $segStart i32) (local $fd i32)\n")
	b.WriteString("    (local $si i32) (local $ta i32) (local $tl i32) (local $pa i32) (local $pl i32)\n")
	b.WriteString("    (local $sj i32)\n")
	// decode inp path into $D6BUF via $d6_s2h_to (returns len; buf at $D6BUF)
	b.WriteString("    (local.set $iplen (call $d6_s2h_to (call $rt_env (local.get $env) (i32.const 0)) (global.get $D6BUF)))\n")
	// read input file: consumes path from $D6BUF (path_open inside d6_readfile reads it)
	b.WriteString("    (call $d6_readfile (global.get $D6BUF) (local.get $iplen))\n")
	b.WriteString("    (local.set $len)\n") // pop len (top)
	b.WriteString("    (local.set $buf)\n") // pop buf ptr (= $D6SLURP on success, 0 on error)
	// decode out path into $D6BUF (safe: inp path consumed by d6_readfile's path_open)
	b.WriteString("    (local.set $oplen (call $d6_s2h_to (call $rt_env (local.get $env) (i32.const 1)) (global.get $D6BUF)))\n")
	// write-open out file
	b.WriteString("    (local.set $fd (call $d6_wopen (global.get $D6BUF) (local.get $oplen)))\n")
	// on read failure: write-open succeeded? close it (produces empty file), then return unit
	b.WriteString("    (if (i32.eqz (local.get $buf))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (if (i32.ge_s (local.get $fd) (i32.const 0)) (then (drop (call $fd_close (local.get $fd)))))\n")
	b.WriteString("        (return (call $rt_unit))))\n")
	// collect segment index into $D6BUF3: (ptr i32)(len i32) = 8-byte entries
	// scan k = 0..len inclusive: at '\n' emit segment; at k==len emit trailing iff nonempty
	b.WriteString("    (local.set $nseg (i32.const 0))\n")
	b.WriteString("    (local.set $segStart (i32.const 0))\n")
	b.WriteString("    (local.set $k (i32.const 0))\n")
	b.WriteString("    (block $sdb (loop $sdl\n")
	b.WriteString("      (local.set $si (i32.add (global.get $D6BUF3) (i32.shl (local.get $nseg) (i32.const 3))))\n")
	b.WriteString("      (if (i32.ge_s (local.get $k) (local.get $len))\n")
	b.WriteString("        (then\n")
	// trailing segment: add iff segStart < k (non-empty tail, i.e. file did NOT end with '\n')
	b.WriteString("          (if (i32.gt_s (local.get $k) (local.get $segStart))\n")
	b.WriteString("            (then\n")
	b.WriteString("              (i32.store (local.get $si) (i32.add (local.get $buf) (local.get $segStart)))\n")
	b.WriteString("              (i32.store (i32.add (local.get $si) (i32.const 4)) (i32.sub (local.get $k) (local.get $segStart)))\n")
	b.WriteString("              (local.set $nseg (i32.add (local.get $nseg) (i32.const 1)))))\n")
	b.WriteString("          (br $sdb))\n") // exit loop
	b.WriteString("        (else\n")
	b.WriteString("          (if (i32.eq (i32.load8_u (i32.add (local.get $buf) (local.get $k))) (i32.const 10))\n")
	b.WriteString("            (then\n")
	b.WriteString("              (i32.store (local.get $si) (i32.add (local.get $buf) (local.get $segStart)))\n")
	b.WriteString("              (i32.store (i32.add (local.get $si) (i32.const 4)) (i32.sub (local.get $k) (local.get $segStart)))\n")
	b.WriteString("              (local.set $nseg (i32.add (local.get $nseg) (i32.const 1)))\n")
	b.WriteString("              (local.set $segStart (i32.add (local.get $k) (i32.const 1)))))))\n")
	b.WriteString("      (local.set $k (i32.add (local.get $k) (i32.const 1)))\n")
	b.WriteString("      (br $sdl)))\n")
	// drop single trailing empty (file ended with '\n': last segment has len=0)
	b.WriteString("    (if (i32.gt_s (local.get $nseg) (i32.const 0))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (local.set $si (i32.add (global.get $D6BUF3)\n")
	b.WriteString("          (i32.shl (i32.sub (local.get $nseg) (i32.const 1)) (i32.const 3))))\n")
	b.WriteString("        (if (i32.eqz (i32.load (i32.add (local.get $si) (i32.const 4))))\n")
	b.WriteString("          (then (local.set $nseg (i32.sub (local.get $nseg) (i32.const 1)))))))\n")
	// insertion sort: compare segments by $d6_linecmp (bytewise, like Go sort.Strings)
	b.WriteString("    (local.set $k (i32.const 1))\n")
	b.WriteString("    (block $isb (loop $isl\n")
	b.WriteString("      (br_if $isb (i32.ge_s (local.get $k) (local.get $nseg)))\n")
	b.WriteString("      (local.set $si (i32.add (global.get $D6BUF3) (i32.shl (local.get $k) (i32.const 3))))\n")
	b.WriteString("      (local.set $ta (i32.load (local.get $si)))\n")
	b.WriteString("      (local.set $tl (i32.load (i32.add (local.get $si) (i32.const 4))))\n")
	b.WriteString("      (local.set $sj (i32.sub (local.get $k) (i32.const 1)))\n")
	b.WriteString("      (block $ib (loop $il\n")
	b.WriteString("        (br_if $ib (i32.lt_s (local.get $sj) (i32.const 0)))\n")
	b.WriteString("        (local.set $si (i32.add (global.get $D6BUF3) (i32.shl (local.get $sj) (i32.const 3))))\n")
	b.WriteString("        (local.set $pa (i32.load (local.get $si)))\n")
	b.WriteString("        (local.set $pl (i32.load (i32.add (local.get $si) (i32.const 4))))\n")
	b.WriteString("        (br_if $ib (i32.le_s (call $d6_linecmp (local.get $pa) (local.get $pl) (local.get $ta) (local.get $tl)) (i32.const 0)))\n")
	// shift entry[j] to entry[j+1]
	b.WriteString("        (local.set $si (i32.add (global.get $D6BUF3) (i32.shl (i32.add (local.get $sj) (i32.const 1)) (i32.const 3))))\n")
	b.WriteString("        (i32.store (local.get $si) (local.get $pa))\n")
	b.WriteString("        (i32.store (i32.add (local.get $si) (i32.const 4)) (local.get $pl))\n")
	b.WriteString("        (local.set $sj (i32.sub (local.get $sj) (i32.const 1)))\n")
	b.WriteString("        (br $il)))\n")
	// place current element at sj+1
	b.WriteString("      (local.set $si (i32.add (global.get $D6BUF3) (i32.shl (i32.add (local.get $sj) (i32.const 1)) (i32.const 3))))\n")
	b.WriteString("      (i32.store (local.get $si) (local.get $ta))\n")
	b.WriteString("      (i32.store (i32.add (local.get $si) (i32.const 4)) (local.get $tl))\n")
	b.WriteString("      (local.set $k (i32.add (local.get $k) (i32.const 1)))\n")
	b.WriteString("      (br $isl)))\n")
	// write sorted lines to out file (if fd valid)
	b.WriteString("    (if (i32.ge_s (local.get $fd) (i32.const 0))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (local.set $k (i32.const 0))\n")
	b.WriteString("        (block $wb (loop $wl\n")
	b.WriteString("          (br_if $wb (i32.ge_s (local.get $k) (local.get $nseg)))\n")
	b.WriteString("          (local.set $si (i32.add (global.get $D6BUF3) (i32.shl (local.get $k) (i32.const 3))))\n")
	b.WriteString("          (local.set $pa (i32.load (local.get $si)))\n")
	b.WriteString("          (local.set $pl (i32.load (i32.add (local.get $si) (i32.const 4))))\n")
	// fd_write iovec for line bytes
	b.WriteString("          (i32.store (global.get $D6SYS) (local.get $pa))\n")
	b.WriteString("          (i32.store (i32.add (global.get $D6SYS) (i32.const 4)) (local.get $pl))\n")
	b.WriteString("          (drop (call $fd_write (local.get $fd) (global.get $D6SYS) (i32.const 1) (i32.add (global.get $D6SYS) (i32.const 24))))\n")
	// fd_write '\n'
	b.WriteString("          (i32.store8 (i32.add (global.get $D6SYS) (i32.const 12)) (i32.const 10))\n")
	b.WriteString("          (i32.store (global.get $D6SYS) (i32.add (global.get $D6SYS) (i32.const 12)))\n")
	b.WriteString("          (i32.store (i32.add (global.get $D6SYS) (i32.const 4)) (i32.const 1))\n")
	b.WriteString("          (drop (call $fd_write (local.get $fd) (global.get $D6SYS) (i32.const 1) (i32.add (global.get $D6SYS) (i32.const 24))))\n")
	b.WriteString("          (local.set $k (i32.add (local.get $k) (i32.const 1)))\n")
	b.WriteString("          (br $wl)))\n")
	b.WriteString("        (drop (call $fd_close (local.get $fd)))))\n")
	b.WriteString("    (call $rt_unit))\n")
	// c2: arg=out (owned), env=[inp]. Build c3 {inp(retain), out(move)}.
	fmt.Fprintf(b, "  (func $sortFile_c2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", c3)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// c1: arg=inp (owned). Build c2 {inp(move)}.
	fmt.Fprintf(b, "  (func $sortFile_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "sortFile", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitDbApplyWasm bakes `dbApply : Nat -> Nat -> IO Unit` as a DOCUMENTED NO-OP. The WASM
// sandbox (wasmtime preview1) does not provide a subprocess/exec primitive, so dbApply
// CANNOT call `sqlite3 db ".read sql"` from inside the module. The SQL script file is
// already written by the builder's writeChunk/sortFile chain; the actual sqlite3 load
// happens on the HOST side (Task 6). This keeps IO-main programs that call dbApply (e.g.
// ch558/ch559) runnable under wasmtime without crashing -- they simply do not build the .db
// in-sandbox (which is expected: WASM output is checked by the cross-backend lock, not the
// actual sqlite3 content).
//
// ARC: db (env[0]) and sql (env[1]) are MOVED into c3's env by c2; c3 ignores them and
// returns unit. rt_free releases both env slots when c3's closure is freed. World arg immortal.
func (em *wasmEmitter) emitDbApplyWasm(b *strings.Builder) {
	c1 := em.codeRef("dbApply_c1")
	c2 := em.codeRef("dbApply_c2")
	c3 := em.codeRef("dbApply_c3")
	// c3: env=[db,sql], arg=world. WASM sandbox no-op: sqlite3 subprocess not available.
	// The .sql file was written by the builder; the host (Task 6) runs sqlite3.
	b.WriteString("  (func $dbApply_c3 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    ;; dbApply WASM no-op: sqlite3 subprocess unavailable in WASI preview1.\n")
	b.WriteString("    ;; The .sql script is written by writeChunk/sortFile; host Task-6 loads it.\n")
	b.WriteString("    (call $rt_unit))\n")
	// c2: arg=sql (owned), env=[db]. Build c3 {db(retain), sql(move)}.
	fmt.Fprintf(b, "  (func $dbApply_c2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", c3)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// c1: arg=db (owned). Build c2 {db(move)}.
	fmt.Fprintf(b, "  (func $dbApply_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "dbApply", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitPrintStrCodeWasm bakes `printStrCode : Nat -> IO Nat` -- decode the packed String
// and write its RAW bytes (no re-encode; the codec IS the byte-exact representation) plus
// a trailing '\n' to stdout via the runtime's `$puts` helper (the SAME iovec `$show` uses,
// at [16,24)) -- unlike the Task-3/4 file ops, printStrCode must work standalone (ch216
// uses it with no file op present, so it cannot depend on $D6SYS from wasmBibleReadFile).
// 2-step curried IO-closure chain: c1(s->env) -> c2(world->effect).
//
// ARC: s (env[0]) is BORROWED (never released; rt_free reclaims it when c2's closure
// dies). The '\n' byte is written by clobbering $D6BUF[0] AFTER the content write (safe:
// the content bytes have already been consumed by the first $puts call). The returned s
// is RETAINED (env is borrowed) to yield an independent owned reference, matching printNat.
func (em *wasmEmitter) emitPrintStrCodeWasm(b *strings.Builder) {
	c1 := em.codeRef("printStrCode_c1")
	c2 := em.codeRef("printStrCode_c2")
	b.WriteString("  (func $printStrCode_c2 (param $arg i32) (param $env i32) (result i32) (local $s i32) (local $len i32)\n")
	b.WriteString("    (local.set $s (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $d6_s2h (local.get $s))\n")
	b.WriteString("    (local.set $len)\n") // pop len (top)
	b.WriteString("    (drop)\n")           // pop buf ptr (always $D6BUF)
	b.WriteString("    (call $puts (i32.const 1) (global.get $D6BUF) (local.get $len))\n")
	b.WriteString("    (i32.store8 (global.get $D6BUF) (i32.const 10))\n") // '\n'
	b.WriteString("    (call $puts (i32.const 1) (global.get $D6BUF) (i32.const 1))\n")
	b.WriteString("    (call $rt_retain (local.get $s))\n")
	b.WriteString("    (local.get $s))\n")
	fmt.Fprintf(b, "  (func $printStrCode_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "printStrCode", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitGetEnvCodeWasm bakes `getEnvCode : Nat -> IO Nat` -- decode the packed String key,
// look it up via $d6_getenv, and h2s-encode the value (an unset key naturally h2s-encodes
// to the packed-empty sentinel 1, since $d6_getenv returns len=0 on "not found"). 2-step
// curried IO-closure chain: c1(key->env) -> c2(world->effect). Ports rust.go:239.
//
// ARC: key (env[0]) is BORROWED (never released here; rt_free reclaims it when c2's
// closure dies). The h2s result is a fresh owned bignum.
func (em *wasmEmitter) emitGetEnvCodeWasm(b *strings.Builder) {
	c1 := em.codeRef("getEnvCode_c1")
	c2 := em.codeRef("getEnvCode_c2")
	b.WriteString("  (func $getEnvCode_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $klen i32) (local $vbuf i32) (local $vlen i32)\n")
	b.WriteString("    (call $d6_s2h (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $klen)\n") // pop len (top)
	b.WriteString("    (drop)\n")            // pop buf ptr (always $D6BUF)
	b.WriteString("    (call $d6_getenv (global.get $D6BUF) (local.get $klen))\n")
	b.WriteString("    (local.set $vlen)\n") // pop len (top)
	b.WriteString("    (local.set $vbuf)\n") // pop buf ptr
	b.WriteString("    (call $d6_h2s (local.get $vbuf) (local.get $vlen)))\n")
	fmt.Fprintf(b, "  (func $getEnvCode_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "getEnvCode", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitReadFileCodeWasm bakes `readFileCode : Nat -> IO Nat` -- decode the packed String
// path, slurp the file via the Task-3 $d6_readfile, and h2s-encode its bytes. A missing/
// unreadable file ($d6_readfile -> (0,0)) returns the packed-empty sentinel bignum 1
// DIRECTLY (mirrors the C/Rust `if (!fp) return big_from_long(1)` branch, taken before the
// codec is even consulted). 2-step curried IO-closure chain: c1(path->env) -> c2(world->
// effect). Depends on wasmBibleReadFile ($d6_readfile/$D6SYS/$D6SLURP). Ports rust.go:243.
//
// ARC: path (env[0]) is BORROWED. The (0,0)-branch bignum and the h2s result are both
// fresh owned values.
func (em *wasmEmitter) emitReadFileCodeWasm(b *strings.Builder) {
	c1 := em.codeRef("readFileCode_c1")
	c2 := em.codeRef("readFileCode_c2")
	b.WriteString("  (func $readFileCode_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $plen i32) (local $buf i32) (local $len i32)\n")
	b.WriteString("    (call $d6_s2h (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $plen)\n")
	b.WriteString("    (drop)\n")
	b.WriteString("    (call $d6_readfile (global.get $D6BUF) (local.get $plen))\n")
	b.WriteString("    (local.set $len)\n")
	b.WriteString("    (local.set $buf)\n")
	b.WriteString("    (if (i32.eqz (local.get $buf)) (then (return (call $rt_big_from_long (i32.const 1)))))\n")
	b.WriteString("    (call $d6_h2s (local.get $buf) (local.get $len)))\n")
	fmt.Fprintf(b, "  (func $readFileCode_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "readFileCode", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitWriteFileCodeWasm bakes `writeFileCode : Nat -> Nat -> IO Nat` -- decode the packed
// String path, write-open it (O_CREAT|O_TRUNC) via the Task-4 $d6_wopen, fd_write the
// decoded content bytes, close, and return the content code unchanged. 3-step curried
// IO-closure chain: c1(path->env) -> c2(content->env) -> c3(world->effect). Depends on
// wasmBibleReadFile ($D6SYS) + wasmBibleWriteOps ($d6_wopen). Ports rust.go:247.
//
// Both path and content decode through the SAME $D6BUF window sequentially: $d6_wopen's
// path_open call reads the path bytes synchronously before returning, so $D6BUF is safe
// to reuse for the content decode afterward (the sortFile_c3 precedent).
//
// ARC: path (env[0]) and content (env[1]) are BORROWED (never released; rt_free reclaims
// both when c3's closure dies). The returned content is RETAINED (env is borrowed) to
// yield an independent owned reference, matching writeChunk's `h` return.
func (em *wasmEmitter) emitWriteFileCodeWasm(b *strings.Builder) {
	c1 := em.codeRef("writeFileCode_c1")
	c2 := em.codeRef("writeFileCode_c2")
	c3 := em.codeRef("writeFileCode_c3")
	b.WriteString("  (func $writeFileCode_c3 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $plen i32) (local $clen i32) (local $fd i32) (local $c i32)\n")
	b.WriteString("    (local.set $plen (call $d6_s2h_to (call $rt_env (local.get $env) (i32.const 0)) (global.get $D6BUF)))\n")
	b.WriteString("    (local.set $fd (call $d6_wopen (global.get $D6BUF) (local.get $plen)))\n")
	b.WriteString("    (local.set $c (call $rt_env (local.get $env) (i32.const 1)))\n")
	b.WriteString("    (if (i32.ge_s (local.get $fd) (i32.const 0))\n")
	b.WriteString("      (then\n")
	b.WriteString("        (call $d6_s2h (local.get $c))\n")
	b.WriteString("        (local.set $clen)\n")
	b.WriteString("        (drop)\n")
	b.WriteString("        (i32.store (global.get $D6SYS) (global.get $D6BUF))\n")
	b.WriteString("        (i32.store (i32.add (global.get $D6SYS) (i32.const 4)) (local.get $clen))\n")
	b.WriteString("        (drop (call $fd_write (local.get $fd) (global.get $D6SYS) (i32.const 1) (i32.add (global.get $D6SYS) (i32.const 24))))\n")
	b.WriteString("        (drop (call $fd_close (local.get $fd)))))\n")
	b.WriteString("    (call $rt_retain (local.get $c))\n")
	b.WriteString("    (local.get $c))\n")
	fmt.Fprintf(b, "  (func $writeFileCode_c2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", c3)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	fmt.Fprintf(b, "  (func $writeFileCode_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "writeFileCode", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitArgCountCodeWasm bakes `argCountCode : IO Nat` -- the user argv count (WASI argv[0],
// the program name, excluded), via $d6_argcount. World-only arity (no value arg), like
// timeNanos: accessor -> closure(world) -> effect.
func (em *wasmEmitter) emitArgCountCodeWasm(b *strings.Builder) {
	wIdx := em.codeRef("argCountCode_w")
	b.WriteString("  (func $argCountCode_w (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (call $rt_big_from_long (call $d6_argcount)))\n")
	em.emitCachedThunk(b, "argCountCode", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, wIdx)
		return "(local.get " + r + ")"
	})
}

// emitArgAtCodeWasm bakes `argAtCode : Nat -> IO Nat` -- argv[i] (0-based, argv[0] the
// program name skipped internally by $d6_argat) as a packed String code; out of range
// h2s-encodes to the packed-empty sentinel 1 ($d6_argat returns len=0). 2-step curried
// IO-closure chain: c1(i->env) -> c2(world->effect). Ports rust.go:255.
//
// ARC: i (env[0]) is BORROWED; big_nlimbs/big_limb only READ it (no allocation). The h2s
// result is a fresh owned bignum.
func (em *wasmEmitter) emitArgAtCodeWasm(b *strings.Builder) {
	c1 := em.codeRef("argAtCode_c1")
	c2 := em.codeRef("argAtCode_c2")
	b.WriteString("  (func $argAtCode_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $i i32) (local $idx i32) (local $buf i32) (local $len i32)\n")
	b.WriteString("    (local.set $i (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (if (i32.gt_s (call $big_nlimbs (local.get $i)) (i32.const 0))\n")
	b.WriteString("      (then (local.set $idx (call $big_limb (local.get $i) (i32.const 0))))\n")
	b.WriteString("      (else (local.set $idx (i32.const 0))))\n")
	b.WriteString("    (call $d6_argat (local.get $idx))\n")
	b.WriteString("    (local.set $len)\n")
	b.WriteString("    (local.set $buf)\n")
	b.WriteString("    (call $d6_h2s (local.get $buf) (local.get $len)))\n")
	fmt.Fprintf(b, "  (func $argAtCode_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "argAtCode", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitExitWithWasm bakes `exitWith : Nat -> IO Unit` -- terminate the process via the
// WASI `proc_exit` import with the Nat's low limb as the exit status. 2-step curried
// IO-closure chain: c1(n->env) -> c2(world->effect). `proc_exit` never returns (wasmtime
// terminates the module); the trailing `unreachable` satisfies the func's declared
// `(result i32)` type without fabricating a value that is never produced. Ports rust.go:263.
func (em *wasmEmitter) emitExitWithWasm(b *strings.Builder) {
	c1 := em.codeRef("exitWith_c1")
	c2 := em.codeRef("exitWith_c2")
	b.WriteString("  (func $exitWith_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $n i32) (local $code i32)\n")
	b.WriteString("    (local.set $n (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (if (i32.gt_s (call $big_nlimbs (local.get $n)) (i32.const 0))\n")
	b.WriteString("      (then (local.set $code (call $big_limb (local.get $n) (i32.const 0))))\n")
	b.WriteString("      (else (local.set $code (i32.const 0))))\n")
	b.WriteString("    (call $proc_exit (local.get $code))\n")
	b.WriteString("    unreachable)\n")
	fmt.Fprintf(b, "  (func $exitWith_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "exitWith", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitBinEmptyWasm bakes `binEmpty : Bin` -- the empty byte string. Not a function (no
// arg to curry), so it is a plain memoized VALUE thunk over $rt_mkbin(0), exactly the
// nullary-constructor template (emitCtor's arity==0 case): the cached global IS the
// canonical value every reference returns.
func (em *wasmEmitter) emitBinEmptyWasm(b *strings.Builder) {
	em.emitCachedThunk(b, "binEmpty", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkbin (i32.const 0)))\n", r)
		return "(local.get " + r + ")"
	})
}

// emitBinConsWasm bakes `binCons : Nat -> Bin -> Bin` -- prepend the LOW byte of the nat
// (n mod 256, via the 1e9-mod-256 fact: nlimbs==0 -> 0, else limb0 & 255) onto a Bin,
// PURE 2-arg curried chain: accessor -> c1(n->env) -> c2(b->result). c2 borrows both n
// (env, read-only) and b (arg): it copies b's bytes into a FRESH, larger Bin rather than
// mutating or aliasing b, so b is never retained into the result and is released once
// consumed (n is left alone -- owned by the c1-built closure, released when THAT closure
// is torn down by its caller, matching splitOn's env discipline).
func (em *wasmEmitter) emitBinConsWasm(b *strings.Builder) {
	c1 := em.codeRef("binCons_c1")
	c2 := em.codeRef("binCons_c2")
	// c2: env={n}, arg=b (owned). o = fresh Bin of len(b)+1; o[0] = low byte of n;
	// o[1..] = b's bytes. Release b (its bytes are copied, not aliased); return o (fresh
	// owned, no retain needed).
	b.WriteString("  (func $binCons_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $n i32) (local $nl i32) (local $byte i32)\n")
	b.WriteString("    (local $len i32) (local $o i32) (local $i i32)\n")
	b.WriteString("    (local.set $n (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $nl (call $big_nlimbs (local.get $n)))\n")
	b.WriteString("    (if (i32.gt_s (local.get $nl) (i32.const 0))\n")
	b.WriteString("      (then (local.set $byte (i32.and (call $big_limb (local.get $n) (i32.const 0)) (i32.const 255))))\n")
	b.WriteString("      (else (local.set $byte (i32.const 0))))\n")
	b.WriteString("    (local.set $len (call $rt_bin_len (local.get $arg)))\n")
	b.WriteString("    (local.set $o (call $rt_mkbin (i32.add (local.get $len) (i32.const 1))))\n")
	b.WriteString("    (call $rt_bin_set (local.get $o) (i32.const 0) (local.get $byte))\n")
	b.WriteString("    (local.set $i (i32.const 0))\n")
	b.WriteString("    (block $done (loop $lp\n")
	b.WriteString("      (br_if $done (i32.ge_s (local.get $i) (local.get $len)))\n")
	b.WriteString("      (call $rt_bin_set (local.get $o) (i32.add (local.get $i) (i32.const 1))\n")
	b.WriteString("        (call $rt_bin_at (local.get $arg) (local.get $i)))\n")
	b.WriteString("      (local.set $i (i32.add (local.get $i) (i32.const 1)))\n")
	b.WriteString("      (br $lp)))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $o))\n")
	// c1: arg=n (owned). Build c2 {n(move)}.
	fmt.Fprintf(b, "  (func $binCons_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "binCons", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitBinLenWasm bakes `binLen : Bin -> Nat` -- PURE 1-arg (accessor -> c1(b) -> Nat),
// exactly byteLen's template: the owned $arg is consumed (read, then released) since the
// result is a fresh bignum unrelated to b's identity.
func (em *wasmEmitter) emitBinLenWasm(b *strings.Builder) {
	c1 := em.codeRef("binLen_c1")
	b.WriteString("  (func $binLen_c1 (param $arg i32) (param $env i32) (result i32) (local $r i32)\n")
	b.WriteString("    (local.set $r (call $rt_big_from_long (call $rt_bin_len (local.get $arg))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	em.emitCachedThunk(b, "binLen", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitBinAtWasm bakes `binAt : Bin -> Nat -> Nat` -- the byte at an index, 0 if out of
// range. PURE 2-arg curried: accessor -> c1(b->env) -> c2(i->result). The index nat is
// guarded by nlimbs the same way binCons guards its low byte: nlimbs==0 -> index 0 (in
// range iff the Bin is non-empty); nlimbs==1 -> index = limb0 (bounds-checked against
// rt_bin_len); nlimbs>1 -> the value is >= 1e9, unconditionally out of range (no real Bin
// is that long) -- $inRange stays 0 so the oob arm fires. b (env, borrowed) is never
// released here (owned by the c1-built closure); the owned index nat ($arg) is consumed
// (released) since the result is a fresh Nat unrelated to its identity.
func (em *wasmEmitter) emitBinAtWasm(b *strings.Builder) {
	c1 := em.codeRef("binAt_c1")
	c2 := em.codeRef("binAt_c2")
	b.WriteString("  (func $binAt_c2 (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (local $bn i32) (local $nl i32) (local $k i32) (local $inRange i32)\n")
	b.WriteString("    (local $len i32) (local $r i32)\n")
	b.WriteString("    (local.set $bn (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (local.set $nl (call $big_nlimbs (local.get $arg)))\n")
	b.WriteString("    (local.set $inRange (i32.const 0))\n")
	b.WriteString("    (if (i32.eqz (local.get $nl))\n")
	b.WriteString("      (then (local.set $k (i32.const 0)) (local.set $inRange (i32.const 1)))\n")
	b.WriteString("      (else (if (i32.eq (local.get $nl) (i32.const 1))\n")
	b.WriteString("        (then\n")
	b.WriteString("          (local.set $k (call $big_limb (local.get $arg) (i32.const 0)))\n")
	b.WriteString("          (local.set $inRange (i32.const 1))))))\n")
	b.WriteString("    (local.set $len (call $rt_bin_len (local.get $bn)))\n")
	b.WriteString("    (if (i32.and (local.get $inRange) (i32.lt_s (local.get $k) (local.get $len)))\n")
	b.WriteString("      (then (local.set $r (call $rt_big_from_long (call $rt_bin_at (local.get $bn) (local.get $k)))))\n")
	b.WriteString("      (else (local.set $r (call $rt_big_from_long (i32.const 0)))))\n")
	b.WriteString("    (call $rt_release (local.get $arg))\n")
	b.WriteString("    (local.get $r))\n")
	// c1: arg=b (owned). Build c2 {b(move)}.
	fmt.Fprintf(b, "  (func $binAt_c1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", c2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "binAt", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, c1)
		return "(local.get " + r + ")"
	})
}

// emitPrintBinWasm bakes `printBin : Bin -> IO Unit` (NOT `IO Bin` -- ch483's foreign
// signature is `Bin -> IO Unit`, mirroring the JS/C bodies which return $unit/mkunit()
// too). 2-step curried IO-closure chain: c1(b->env) -> w(world->effect). The world step
// borrows b from env (read-only: $rt_show_line renders it via the K_BIN `$show` arm, no
// mutation), then returns the immortal $rt_unit -- no retain-and-return of b is needed
// (unlike printNat, which hands its Nat argument back). b is left untouched: it is owned
// by the c1-built closure and is released when that closure is torn down by its caller
// (bindIO's io_bind4 releases the fresh action after running it), exactly as splitOn's
// env-captured sep is never manually released in the body.
func (em *wasmEmitter) emitPrintBinWasm(b *strings.Builder) {
	vIdx := em.codeRef("printBin_v")
	wIdx := em.codeRef("printBin_w")
	b.WriteString("  (func $printBin_w (param $arg i32) (param $env i32) (result i32) (local $bn i32)\n")
	b.WriteString("    (local.set $bn (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_show_line (local.get $bn))\n")
	b.WriteString("    (call $rt_unit))\n")
	fmt.Fprintf(b, "  (func $printBin_v (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", wIdx)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	em.emitCachedThunk(b, "printBin", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, vIdx)
		return "(local.get " + r + ")"
	})
}

// emitPrintNatWasm bakes `printNat : Nat -> IO Nat` -- the FIRST foreign IO op and the
// documented curried template. accessor -> closure(Nat) -> closure(world) -> effect.
// The world step renders the Nat's decimal + newline to stdout (fd 1) via $rt_show_line
// (K_BIG -> emit_big is exactly the decimal), then RETAINS + returns the Nat.
func (em *wasmEmitter) emitPrintNatWasm(b *strings.Builder) {
	vIdx := em.codeRef("printNat_v")
	wIdx := em.codeRef("printNat_w")
	// world step: env={n}. Print n, return it as an owned reference.
	b.WriteString("  (func $printNat_w (param $arg i32) (param $env i32) (result i32) (local $n i32)\n")
	b.WriteString("    (local.set $n (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_show_line (local.get $n))\n")
	b.WriteString("    (call $rt_retain (local.get $n))\n") // yield an independent owned ref
	b.WriteString("    (local.get $n))\n")
	// value step: capture the (owned) Nat arg into the world step's env (MOVE).
	fmt.Fprintf(b, "  (func $printNat_v (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", wIdx)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// accessor thunk $def_printNat -> mkclo(printNat_v, 0).
	em.emitCachedThunk(b, "printNat", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, vIdx)
		return "(local.get " + r + ")"
	})
}

// emitTimeNanosWasm bakes `timeNanos : IO Nat` -- the world-only arity of the template
// (accessor -> closure(world) -> effect; no value arg). The world step reads the WASI
// monotonic clock (clock_time_get, clock_id 1) into the [24,32) scratch cell and builds a
// bignum from the i64 nanosecond count. The result is fresh/owned (no retain needed).
func (em *wasmEmitter) emitTimeNanosWasm(b *strings.Builder) {
	wIdx := em.codeRef("timeNanos_w")
	b.WriteString("  (func $timeNanos_w (param $arg i32) (param $env i32) (result i32)\n")
	b.WriteString("    (drop (call $clock_time_get (i32.const 1) (i64.const 0) (i32.const 24)))\n")
	b.WriteString("    (call $rt_big_from_i64 (i64.load (i32.const 24))))\n")
	em.emitCachedThunk(b, "timeNanos", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, wIdx)
		return "(local.get " + r + ")"
	})
}

// emitPureIOWasm bakes `pureIO A a u = a` (a constant world thunk). accessor -> closure(A)
// -> closure(a) -> closure(world) -> a. The type arg A erases to $rt_unit (immortal). The
// world step RETAINS + returns the captured value as an independent owned reference.
func (em *wasmEmitter) emitPureIOWasm(b *strings.Builder) {
	p0 := em.codeRef("io_pure0")
	p1 := em.codeRef("io_pure1")
	p2 := em.codeRef("io_pure2")
	// io_pure2: env={a}, arg=world. Return a (owned).
	b.WriteString("  (func $io_pure2 (param $arg i32) (param $env i32) (result i32) (local $a i32)\n")
	b.WriteString("    (local.set $a (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_retain (local.get $a))\n")
	b.WriteString("    (local.get $a))\n")
	// io_pure1: arg=a (owned), build io_pure2 capturing a (MOVE).
	fmt.Fprintf(b, "  (func $io_pure1 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", p2)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// io_pure0: arg=A (type/unit, discarded), return mkclo(io_pure1, 0).
	fmt.Fprintf(b, "  (func $io_pure0 (param $arg i32) (param $env i32) (result i32)\n")
	fmt.Fprintf(b, "    (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", p1)
	em.emitCachedThunk(b, "pureIO", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, p0)
		return "(local.get " + r + ")"
	})
}

// emitBindIOWasm bakes `bindIO A B m k u = k (m ()) ()` -- the IO sequencing combinator.
// accessor -> A -> B -> m -> k -> world -> effect. The world step (io_bind4, env={m,k})
// runs m on the world (m borrowed from env), feeds its value to k (borrowed), runs the
// resulting action on the world, RELEASES that fresh intermediate action, and yields its
// value. io_bind3 RETAINS the forwarded m (env slot) into io_bind4 and MOVES the owned k.
func (em *wasmEmitter) emitBindIOWasm(b *strings.Builder) {
	i0 := em.codeRef("io_bind0")
	i1 := em.codeRef("io_bind1")
	i2 := em.codeRef("io_bind2")
	i3 := em.codeRef("io_bind3")
	i4 := em.codeRef("io_bind4")
	// io_bind4: env={m,k}, arg=world. k (m ()) ().
	b.WriteString("  (func $io_bind4 (param $arg i32) (param $env i32) (result i32) (local $v i32) (local $act i32) (local $r i32)\n")
	b.WriteString("    (local.set $v (call $rt_apply (call $rt_env (local.get $env) (i32.const 0)) (call $rt_unit)))\n")
	b.WriteString("    (local.set $act (call $rt_apply (call $rt_env (local.get $env) (i32.const 1)) (local.get $v)))\n")
	b.WriteString("    (local.set $r (call $rt_apply (local.get $act) (call $rt_unit)))\n")
	b.WriteString("    (call $rt_release (local.get $act))\n") // fresh action from k: reclaim
	b.WriteString("    (local.get $r))\n")
	// io_bind3: env={m}, arg=k. Build io_bind4 {m,k}: retain forwarded m, move owned k.
	fmt.Fprintf(b, "  (func $io_bind3 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 2)))\n", i4)
	b.WriteString("    (call $rt_retain (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (call $rt_env (local.get $env) (i32.const 0)))\n")
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 1) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// io_bind2: arg=m (owned), build io_bind3 capturing m (MOVE).
	fmt.Fprintf(b, "  (func $io_bind2 (param $arg i32) (param $env i32) (result i32) (local $c i32)\n")
	fmt.Fprintf(b, "    (local.set $c (call $rt_mkclo (i32.const %d) (i32.const 1)))\n", i3)
	b.WriteString("    (call $rt_clo_set (local.get $c) (i32.const 0) (local.get $arg))\n")
	b.WriteString("    (local.get $c))\n")
	// io_bind1: arg=B (type/unit, discarded), return mkclo(io_bind2, 0).
	fmt.Fprintf(b, "  (func $io_bind1 (param $arg i32) (param $env i32) (result i32)\n")
	fmt.Fprintf(b, "    (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", i2)
	// io_bind0: arg=A (type/unit, discarded), return mkclo(io_bind1, 0).
	fmt.Fprintf(b, "  (func $io_bind0 (param $arg i32) (param $env i32) (result i32)\n")
	fmt.Fprintf(b, "    (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", i1)
	em.emitCachedThunk(b, "bindIO", func(f *wasmFunc, bb *strings.Builder) string {
		r := f.fresh()
		fmt.Fprintf(bb, "    (local.set %s (call $rt_mkclo (i32.const %d) (i32.const 0)))\n", r, i0)
		return "(local.get " + r + ")"
	})
}

// wasmName sanitizes a rune identifier into a WAT-symbol fragment ([A-Za-z0-9_], shared
// with the C/LLVM sanitizer so symbol schemes match).
func wasmName(n string) string { return cName(n) }

// wasmThunkName / wasmCodeName namespace the two emitted symbol kinds, matching the LLVM
// backend's scheme.
func wasmThunkName(n string) string { return "def_" + wasmName(n) }
func wasmCodeName(n string) string  { return "code_" + wasmName(n) }

// prependW pushes a new innermost local (de Bruijn 0) onto the locals stack.
func prependW(v string, locals []string) []string {
	out := make([]string, 0, len(locals)+1)
	out = append(out, v)
	out = append(out, locals...)
	return out
}

// wasmDataStr renders a Go string as a WAT data-segment string literal (each byte as
// \HH for non-printables / quote / backslash), NUL-terminated.
func wasmDataStr(s string) string {
	var b strings.Builder
	b.WriteByte('"')
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c == '\\':
			b.WriteString("\\5c")
		case c == '"':
			b.WriteString("\\22")
		case c < 0x20 || c >= 0x7f:
			fmt.Fprintf(&b, "\\%02x", c)
		default:
			b.WriteByte(c)
		}
	}
	b.WriteString("\\00")
	b.WriteByte('"')
	return b.String()
}
