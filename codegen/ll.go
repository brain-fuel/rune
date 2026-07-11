package codegen

import (
	"fmt"
	"sort"
	"strings"

	"goforge.dev/rune/v3/core"
)

// LL is the SECOND NATIVE backend (telos-2 / M4, the B3+ "fan out"): it emits
// LLVM IR TEXT over the CLOSURE-CONVERTED IR (codegen/closure.go), compiled and
// run via `clang program.ll runtime.c -o exe`. Where the C backend (codegen/c.go,
// the ORACLE) emits portable C that `cc` lowers, this backend emits the program
// LOGIC as genuine LLVM IR — each CodeBlock is an LLVM `define`, MkClosure is an
// alloc + stores, AppClosure loads a code pointer and indirect-calls, CCase is an
// LLVM `switch`, CField is a runtime field load, CLit{LitNat} is an immediate.
//
// RUNTIME STRATEGY (pragmatic, low-risk — R-NATIVE stage 2): the LLVM backend
// REUSES the C runtime. Re-implementing the tagged-word Value rep, the ARC runtime
// (retain/release, mark-sweep retired), and `$show` by hand in LLVM IR would be
// enormous and unforgiving; instead
// the generated CODE is real LLVM IR that CALLS into a small linked C runtime
// (`llRuntimeC`, the external-linkage twin of cRuntime). This is a legitimate LLVM
// backend: the codegen target IS LLVM IR; the runtime is a linked C shim exactly
// as the source backends lean on a runtime blob. `EmitRuntime()` exposes the C
// the harness links alongside the emitted `.ll`.
//
// SUPPORTED FRAGMENT (byte-identical to the C/other backends on this subset):
// builtin-nat arithmetic (zero/succ/NatElim + accel add/mul/monus), constructors +
// eliminators (CCase/CField over ListElim/NatElim/any datatype), curried closures +
// application, dependent pairs (CPair/CFst/CSnd), the erased token (CUnit), and
// native literals (CLit). Quotient/IO builtins are emitted via the shared runtime
// when present. `partial` recursion runs through the memoized thunks, as in C.
//
// VALUE REP: a `Value` is `intptr_t` (R-NATIVE piece 1). In LLVM IR every Rune
// value is an `i64`; the runtime functions take/return `i64`, env is `i64*`, and a
// code pointer is `i64 (i64, i64*)*`. The emitter never inspects the tag itself —
// it defers to the runtime helpers (mkint/mkclo/apply/con_get/…), so the rep stays
// owned by ONE place (cRuntime, shared) and the two native backends cannot drift.
type LL struct{}

func (LL) Target() string { return "ll" }

// Emit closure-converts the program and emits an LLVM IR translation unit: a
// declare of every runtime helper, every lifted code block as an LLVM define, the
// constructor/eliminator/definition thunks, and `rune_main` (called by the linked
// runtime's C main) that evaluates the entry and `$show`s it. The accompanying C
// runtime is EmitRuntime(); compile with `clang program.ll runtime.c -o exe`.
func (LL) Emit(p Program) (TargetSource, error) {
	cp := ClosureConvert(p)
	// Perceus ownership pass: insert CDup/CDrop so the ARC runtime
	// (rt_retain/rt_release) reference-counts heap values. Mirrors c.go:91 (and
	// wasm.go:51-52) -- the pass is backend-portable, so the LLVM backend sees the
	// SAME annotated IR the C ORACLE does and its lowerings mirror c.go's ownership.
	// Under the interim ladder (Task 1's runtime with NO annotations) this was a pure
	// leak with unchanged output; wiring the pass makes releases fire (memory-only).
	cp = Perceus(cp)
	em := &llEmitter{}
	if p.Nat != nil {
		em.natElim = p.Nat.ElimName
		em.accel = p.Nat.Ops
	}
	em.blocks = make(map[string]CodeBlock, len(cp.Blocks))
	for _, blk := range cp.Blocks {
		em.blocks[blk.Name] = blk
	}
	// Index every non-nat constructor for satCtorDispatch (the LL dual of c.go:105
	// and wasm.go:282): a SATURATED ctor application lowers to a direct rt_mkcon
	// instead of currying through the ctorfn blocks (whose intermediate K_CLOs the
	// recognized-bare spine never releases). The builtin-nat data group is excluded
	// exactly as in C/WASM (its zero/succ are the native bignum reps).
	em.ctorByName = map[string]CtorSpec{}
	for _, d := range cp.Datas {
		if cp.Nat != nil && d.ElimName == cp.Nat.ElimName {
			continue
		}
		for _, c := range d.Ctors {
			em.ctorByName[c.Name] = c
		}
	}

	// The function bodies are written to `body` first so the string-constant
	// globals they reference (collected into em.strs) can be flushed to module
	// scope BEFORE them — LLVM requires a string global be a module-scope
	// definition, not an inline constant.
	var body strings.Builder

	// Constructor + eliminator code blocks and thunks.
	for _, d := range cp.Datas {
		if cp.Nat != nil && d.ElimName == cp.Nat.ElimName {
			em.emitNatLL(&body, *cp.Nat)
			continue
		}
		for _, c := range d.Ctors {
			em.emitCtorLL(&body, c)
		}
	}
	// Per-name memoizing wrapper thunks for host-linked foreign accessors: @fc_<name>()
	// caches the first @<name>() read as one never-released borrowed root (Decision 4),
	// so a foreign read inside a fold does not leak a fresh closure per iteration. The LL
	// dual of c.go's `_fc_<name>` per-call-site static; CForeign lowers to @fc_<name>().
	for _, fn := range foreignNames(p) {
		nm := llName(fn)
		em.emitNamedThunk(&body, "fc_"+nm, "fc_"+nm, func(f *llFunc) string {
			r := f.fresh()
			fmt.Fprintf(&body, "  %s = call i64 @%s()\n", r, nm)
			return r
		})
	}
	// The lifted code blocks (definitions' and eliminators' bodies).
	for _, blk := range cp.Blocks {
		em.emitBlock(&body, blk)
	}
	// Definition thunks (eliminators converted as CDefSpec, then user defs). A
	// `partial` is split into a _step body + a public trampoline driver (T2).
	for _, def := range cp.Defs {
		if cp.Partials[def.Name] {
			em.emitPartialLL(&body, def.Name, def.Arity, def.Body)
			continue
		}
		em.emitDefThunk(&body, def.Name, def.Body)
	}

	// rune_main: evaluate the entry thunk and show it. Called by the runtime's C
	// `main` (which seeds UNIT first; under ARC there is no GC stack bottom to
	// capture). An IO main is a deferred world thunk (unit -> A), forced by applying $unit.
	body.WriteString("define void @rune_main() {\nentry:\n")
	if p.Main != "" {
		f := &llFunc{em: em}
		mainV := f.emit(&body, CGlobal{Name: p.Main}, nil)
		if p.IOMain {
			unit := f.fresh()
			fmt.Fprintf(&body, "  %s = load i64, i64* @UNIT\n", unit)
			r := f.fresh()
			fmt.Fprintf(&body, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", r, mainV, unit)
			mainV = r
		}
		fmt.Fprintf(&body, "  call void @rt_show_line(i64 %s)\n", mainV)
	}
	body.WriteString("  ret void\n}\n")

	// Assemble the module: header, preamble, runtime declares, string globals, then
	// the function bodies.
	var b strings.Builder
	b.WriteString("; Generated by rune emit (llvm backend) — the erased shadow, native via clang. Do not edit.\n")
	b.WriteString("; Link with the C runtime from LL{}.EmitRuntime(): clang program.ll runtime.c -o exe\n\n")
	b.WriteString(llPreamble)
	b.WriteString(llDeclares)
	if usesQuot(p) {
		b.WriteString(llQuotDeclares)
	}
	if usesIO(p) {
		b.WriteString(llIODeclares)
	}
	// Host-linked foreign accessors (R-FFI / B4): an `IForeign` lowers to a call to
	// the zero-arg `@<name>()` the host supplies at link time. LLVM IR requires the
	// callee be declared in the module, so emit a `declare` for each foreign symbol
	// the program references (the source backends need no analogue — their callee is
	// resolved by name at the host's runtime). The definition lives in the linked
	// runtime.c the host provides; here we only promise its signature.
	if fns := foreignNames(p); len(fns) > 0 {
		b.WriteString("; Host-linked foreign accessors (R-FFI; defined in the linked host runtime).\n")
		for _, fn := range fns {
			fmt.Fprintf(&b, "declare i64 @%s()\n", llName(fn))
		}
	}
	b.WriteString("\n")
	// String-constant globals (constructor names + string literals), in stable
	// first-seen order for deterministic output.
	for _, s := range em.strOrder {
		bytes, n := llStrBytes(s)
		fmt.Fprintf(&b, "%s = private unnamed_addr constant [%d x i8] c\"%s\"\n", em.strs[s], n, bytes)
	}
	b.WriteString("\n")
	b.WriteString(body.String())
	return TargetSource(b.String()), nil
}

// foreignNames returns the distinct host-linked foreign symbols the program
// references (R-FFI / B4), in stable first-seen order. The LLVM backend needs
// each one `declare`d in the module so the emitted `call @<name>()` verifies; the
// host supplies the definition in the linked runtime.c. Mirrors usesIO/usesQuot's
// walk but over every IR node kind so a foreign nested in a case arm or pair is
// found.
func foreignNames(p Program) []string {
	seen := map[string]bool{}
	var out []string
	var walk func(Ir)
	walk = func(t Ir) {
		switch x := t.(type) {
		case IForeign:
			pn := primName(x.Name)
			if !seen[pn] {
				seen[pn] = true
				out = append(out, pn)
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
		case IBounce:
			walk(x.Call)
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
	return out
}

// EmitRuntime returns the C runtime the emitted LLVM IR links against (external-
// linkage twin of cRuntime + the C `main` that seeds UNIT and calls `rune_main`;
// under ARC there is no GC stack bottom to capture). The harness writes this beside the `.ll` and
// compiles both with clang. The quotient/IO groups are always included here (they
// are tiny and harmless when the `.ll` never references them).
func (LL) EmitRuntime() string {
	var b strings.Builder
	b.WriteString(llRuntimeC)
	b.WriteString(llRuntimeQuot)
	b.WriteString(llRuntimeIO)
	b.WriteString(llRuntimeMain)
	return b.String()
}

// EmitRuntimeFor is EmitRuntime plus the D3 machine-float (f64) + OpenBLAS dot host
// bodies the program references (R-FFI). The base EmitRuntime() omits them so the
// generic corpus links without -lopenblas; a float/BLAS program uses THIS runtime and
// links -lopenblas. The accessors are EXTERNAL (the .ll calls @fromNat/@dot2/… which
// foreignNames auto-declares); the closure code blocks are static internals.
func (LL) EmitRuntimeFor(p Program) string {
	var b strings.Builder
	b.WriteString(llRuntimeC)
	b.WriteString(llRuntimeQuot)
	b.WriteString(llRuntimeIO)
	emitFloatPrimsLL(&b, p)
	emitStreamPrimsLL(&b, p)
	// Float IO ops (parseFloat/getFloat/printFloat) must follow emitStreamPrimsLL
	// because parseFloat calls d6_s2h (the packed-string decoder defined above).
	emitFloatIOPrimsLL(&b, p)
	emitBinPrimsLL(&b, p)
	emitNetPrimsLL(&b, p)
	emitFSPrimsLL(&b, p)
	emitProcPrimsLL(&b, p)
	emitCryptoPrimsLL(&b, p)
	emitTLSPrimsLL(&b, p)
	// When argCountCode/argAtCode are used, main must accept argc/argv and populate
	// the rune_argc/rune_argv globals that were declared in emitStreamPrimsLL above.
	// When float IO prims are used, main pins LC_NUMERIC="C" so the emitted
	// __fmtf/strtod formatter is locale-independent; locale.h was #included above
	// by emitFloatIOPrimsLL.
	floatIO := usesForeign(p, "parseFloat") || usesForeign(p, "getFloat") || usesForeign(p, "printFloat")
	argv := usesForeign(p, "argCountCode") || usesForeign(p, "argAtCode")
	switch {
	case argv && floatIO:
		b.WriteString(llRuntimeMainFloatArgv)
	case argv:
		b.WriteString(llRuntimeMainArgv)
	case floatIO:
		b.WriteString(llRuntimeMainFloat)
	default:
		b.WriteString(llRuntimeMain)
	}
	return b.String()
}

// emitTLSPrimsLL bakes the Phase-3 TLS client into the linked LLVM runtime — the LL
// twin of emitTLSPrimsC. The TLS state machine is the vendored rune_tls.c shim
// (compiled alongside); this body marshals Bin<->C and calls rune_tls_get. The
// accessor is EXTERNAL (the .ll calls @tlsGet).
func emitTLSPrimsLL(b *strings.Builder, p Program) {
	if !usesTLS(p) {
		return
	}
	b.WriteString(`extern int rune_tls_get(const char* host, int port, const char* path, unsigned char** out, unsigned long* outlen);
static Value tlsGet_c4(Value u, Value* env) { (void)u; Value hb = env[0]; Value pb = env[1]; Value pa = env[2]; int hn = bytes_len(hb); char* host = (char*)malloc(hn + 1); for (int i = 0; i < hn; i++) host[i] = (char) bytes_at(hb, i); host[hn] = 0; int pn = bytes_len(pa); char* path = (char*)malloc(pn + 1); for (int i = 0; i < pn; i++) path[i] = (char) bytes_at(pa, i); path[pn] = 0; int port = (big_nlimbs(pb) == 0 ? 0 : (int) big_limb(pb, 0)); unsigned char* outp = 0; unsigned long on = 0; int rc = rune_tls_get(host, port, path, &outp, &on); free(host); free(path); if (rc != 0) { if (outp) free(outp); return mkbytes(0); } Value r = mkbytes((int) on); for (unsigned long i = 0; i < on; i++) bytes_set(r, (int) i, outp[i]); free(outp); return r; }
static Value tlsGet_c3(Value path, Value* env) { Value c = rt_mkclo(&tlsGet_c4, 3); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_clo_set(c, 2, path); return c; }
static Value tlsGet_c2(Value port, Value* env) { Value c = rt_mkclo(&tlsGet_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, port); return c; }
static Value tlsGet_c1(Value host, Value* env) { (void)env; Value c = rt_mkclo(&tlsGet_c2, 1); rt_clo_set(c, 0, host); return c; }
Value tlsGet(void) { return rt_mkclo(&tlsGet_c1, 0); }
`)
}

// emitCryptoPrimsLL bakes the Phase-3 BearSSL sha256 host body into the linked LLVM
// runtime — the LL twin of emitCryptoPrimsC. The accessor is EXTERNAL (the .ll calls
// @sha256); the program links the vendored libbearssl.a. Fast real digest on a
// backend that ships none; the pure-wootz ch514 stays the in-language oracle.
func emitCryptoPrimsLL(b *strings.Builder, p Program) {
	if !usesCrypto(p) {
		return
	}
	b.WriteString(`#include <bearssl.h>
static Value sha256_c1(Value data, Value* env) { (void)env; int n = bytes_len(data); unsigned char* buf = (unsigned char*)malloc(n > 0 ? n : 1); for (int i = 0; i < n; i++) buf[i] = (unsigned char) bytes_at(data, i); unsigned char out[32]; br_sha256_context ctx; br_sha256_init(&ctx); br_sha256_update(&ctx, buf, n); br_sha256_out(&ctx, out); free(buf); rt_release(data); Value r = mkbytes(32); for (int i = 0; i < 32; i++) bytes_set(r, i, out[i]); return r; }
Value sha256(void) { return rt_mkclo(&sha256_c1, 0); }
`)
}

// emitNetPrimsLL bakes the Phase-1 POSIX socket host bodies into the linked LLVM
// runtime — the LL twin of emitNetPrimsC. Accessors are EXTERNAL (the .ll calls
// @sockConnect/…); handles are the fd in a builtin-nat; payloads are K_BYTES.
func emitNetPrimsLL(b *strings.Builder, p Program) {
	if !usesNet(p) {
		return
	}
	b.WriteString(`#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
static char* _bin_cstr(Value b) { int n = bytes_len(b); char* s = (char*)malloc(n + 1); for (int i = 0; i < n; i++) s[i] = (char) bytes_at(b, i); s[n] = 0; return s; }
static int _ptr_fd(Value c) { return (int)(big_nlimbs(c) == 0 ? 0 : big_limb(c, 0)); }
static Value sockConnect_c3(Value u, Value* env) { (void)u; char* h = _bin_cstr(env[0]); long p = (big_nlimbs(env[1]) == 0 ? 0 : big_limb(env[1], 0)); int fd = socket(AF_INET, SOCK_STREAM, 0); struct sockaddr_in a; memset(&a, 0, sizeof a); a.sin_family = AF_INET; a.sin_port = htons((unsigned short) p); a.sin_addr.s_addr = inet_addr(h); free(h); if (connect(fd, (struct sockaddr*)&a, sizeof a) < 0) { close(fd); return rt_big_from_long(0); } return rt_big_from_long(fd); }
static Value sockConnect_c2(Value port, Value* env) { Value c = rt_mkclo(&sockConnect_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, port); return c; }
static Value sockConnect_c1(Value host, Value* env) { (void)env; Value c = rt_mkclo(&sockConnect_c2, 1); rt_clo_set(c, 0, host); return c; }
Value sockConnect(void) { return rt_mkclo(&sockConnect_c1, 0); }
static Value sockWrite_c3(Value u, Value* env) { (void)u; int fd = _ptr_fd(env[0]); Value data = env[1]; int n = bytes_len(data); char* buf = (char*)malloc(n > 0 ? n : 1); for (int i = 0; i < n; i++) buf[i] = (char) bytes_at(data, i); long w = write(fd, buf, n); free(buf); return rt_big_from_long(w < 0 ? 0 : w); }
static Value sockWrite_c2(Value data, Value* env) { Value c = rt_mkclo(&sockWrite_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, data); return c; }
static Value sockWrite_c1(Value conn, Value* env) { (void)env; Value c = rt_mkclo(&sockWrite_c2, 1); rt_clo_set(c, 0, conn); return c; }
Value sockWrite(void) { return rt_mkclo(&sockWrite_c1, 0); }
static Value sockRead_c3(Value u, Value* env) { (void)u; int fd = _ptr_fd(env[0]); long k = (big_nlimbs(env[1]) == 0 ? 0 : big_limb(env[1], 0)); char* buf = (char*)malloc(k > 0 ? k : 1); long m = read(fd, buf, k); if (m < 0) m = 0; Value r = mkbytes((int) m); for (int i = 0; i < m; i++) bytes_set(r, i, (unsigned char) buf[i]); free(buf); return r; }
static Value sockRead_c2(Value n, Value* env) { Value c = rt_mkclo(&sockRead_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, n); return c; }
static Value sockRead_c1(Value conn, Value* env) { (void)env; Value c = rt_mkclo(&sockRead_c2, 1); rt_clo_set(c, 0, conn); return c; }
Value sockRead(void) { return rt_mkclo(&sockRead_c1, 0); }
static Value sockClose_c2(Value u, Value* env) { (void)u; close(_ptr_fd(env[0])); return mkunit(); }
static Value sockClose_c1(Value conn, Value* env) { (void)env; Value c = rt_mkclo(&sockClose_c2, 1); rt_clo_set(c, 0, conn); return c; }
Value sockClose(void) { return rt_mkclo(&sockClose_c1, 0); }
static Value sockListen_c2(Value u, Value* env) { (void)u; long p = (big_nlimbs(env[0]) == 0 ? 0 : big_limb(env[0], 0)); int fd = socket(AF_INET, SOCK_STREAM, 0); int one = 1; setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one); struct sockaddr_in a; memset(&a, 0, sizeof a); a.sin_family = AF_INET; a.sin_port = htons((unsigned short) p); a.sin_addr.s_addr = inet_addr("127.0.0.1"); if (bind(fd, (struct sockaddr*)&a, sizeof a) < 0 || listen(fd, 16) < 0) { close(fd); return rt_big_from_long(0); } return rt_big_from_long(fd); }
static Value sockListen_c1(Value port, Value* env) { (void)env; Value c = rt_mkclo(&sockListen_c2, 1); rt_clo_set(c, 0, port); return c; }
Value sockListen(void) { return rt_mkclo(&sockListen_c1, 0); }
static Value sockAccept_c2(Value u, Value* env) { (void)u; int cf = accept(_ptr_fd(env[0]), 0, 0); return rt_big_from_long(cf < 0 ? 0 : cf); }
static Value sockAccept_c1(Value lis, Value* env) { (void)env; Value c = rt_mkclo(&sockAccept_c2, 1); rt_clo_set(c, 0, lis); return c; }
Value sockAccept(void) { return rt_mkclo(&sockAccept_c1, 0); }
`)
}

// emitProcPrimsLL bakes Phase-1 os/exec on the LLVM backend (the LL twin of
// emitProcPrimsC): popen `program arg`, slurp stdout into a K_BYTES.
func emitProcPrimsLL(b *strings.Builder, p Program) {
	if !usesProc(p) {
		return
	}
	b.WriteString(`static char* _proc_cstr(Value b) { int n = bytes_len(b); char* s = (char*)malloc(n + 1); for (int i = 0; i < n; i++) s[i] = (char) bytes_at(b, i); s[n] = 0; return s; }
static Value procRun_c3(Value u, Value* env) { (void)u; char* prog = _proc_cstr(env[0]); char* arg = _proc_cstr(env[1]); size_t cl = strlen(prog) + strlen(arg) + 2; char* cmd = (char*)malloc(cl); snprintf(cmd, cl, "%s %s", prog, arg); FILE* f = popen(cmd, "r"); free(prog); free(arg); free(cmd); if (!f) return mkbytes(0); size_t cap = 256, len = 0; unsigned char* buf = (unsigned char*)malloc(cap); int ch; while ((ch = fgetc(f)) != EOF) { if (len == cap) { cap *= 2; buf = (unsigned char*)realloc(buf, cap); } buf[len++] = (unsigned char) ch; } pclose(f); Value r = mkbytes((int) len); for (size_t i = 0; i < len; i++) bytes_set(r, (int) i, buf[i]); free(buf); return r; }
static Value procRun_c2(Value arg, Value* env) { Value c = rt_mkclo(&procRun_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, arg); return c; }
static Value procRun_c1(Value prog, Value* env) { (void)env; Value c = rt_mkclo(&procRun_c2, 1); rt_clo_set(c, 0, prog); return c; }
Value procRun(void) { return rt_mkclo(&procRun_c1, 0); }
`)
}

// emitFSPrimsLL bakes the Phase-1 OS/filesystem host bodies on the LLVM backend (the
// LL twin of emitFSPrimsC), external accessors over POSIX stdio + stat.
func emitFSPrimsLL(b *strings.Builder, p Program) {
	if !usesFS(p) {
		return
	}
	b.WriteString(`#include <sys/stat.h>
#include <unistd.h>
static char* _fs_cstr(Value b) { int n = bytes_len(b); char* s = (char*)malloc(n + 1); for (int i = 0; i < n; i++) s[i] = (char) bytes_at(b, i); s[n] = 0; return s; }
static Value fsWrite_c3(Value u, Value* env) { (void)u; char* path = _fs_cstr(env[0]); Value data = env[1]; int n = bytes_len(data); FILE* f = fopen(path, "wb"); free(path); if (!f) return rt_big_from_long(0); for (int i = 0; i < n; i++) { unsigned char c = (unsigned char) bytes_at(data, i); fwrite(&c, 1, 1, f); } fclose(f); return rt_big_from_long(n); }
static Value fsWrite_c2(Value data, Value* env) { Value c = rt_mkclo(&fsWrite_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, data); return c; }
static Value fsWrite_c1(Value path, Value* env) { (void)env; Value c = rt_mkclo(&fsWrite_c2, 1); rt_clo_set(c, 0, path); return c; }
Value fsWrite(void) { return rt_mkclo(&fsWrite_c1, 0); }
static Value fsRead_c2(Value u, Value* env) { (void)u; char* path = _fs_cstr(env[0]); FILE* f = fopen(path, "rb"); free(path); if (!f) return mkbytes(0); fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET); if (sz < 0) sz = 0; Value r = mkbytes((int) sz); for (long i = 0; i < sz; i++) { int ch = fgetc(f); bytes_set(r, (int) i, ch < 0 ? 0 : ch); } fclose(f); return r; }
static Value fsRead_c1(Value path, Value* env) { (void)env; Value c = rt_mkclo(&fsRead_c2, 1); rt_clo_set(c, 0, path); return c; }
Value fsRead(void) { return rt_mkclo(&fsRead_c1, 0); }
static Value fsExists_c2(Value u, Value* env) { (void)u; char* path = _fs_cstr(env[0]); int ok = access(path, F_OK) == 0; free(path); return rt_big_from_long(ok ? 1 : 0); }
static Value fsExists_c1(Value path, Value* env) { (void)env; Value c = rt_mkclo(&fsExists_c2, 1); rt_clo_set(c, 0, path); return c; }
Value fsExists(void) { return rt_mkclo(&fsExists_c1, 0); }
static Value fsRemove_c2(Value u, Value* env) { (void)u; char* path = _fs_cstr(env[0]); int ok = remove(path) == 0; free(path); return rt_big_from_long(ok ? 1 : 0); }
static Value fsRemove_c1(Value path, Value* env) { (void)env; Value c = rt_mkclo(&fsRemove_c2, 1); rt_clo_set(c, 0, path); return c; }
Value fsRemove(void) { return rt_mkclo(&fsRemove_c1, 0); }
static Value fsMkdir_c2(Value u, Value* env) { (void)u; char* path = _fs_cstr(env[0]); int ok = mkdir(path, 0755) == 0; free(path); return rt_big_from_long(ok ? 1 : 0); }
static Value fsMkdir_c1(Value path, Value* env) { (void)env; Value c = rt_mkclo(&fsMkdir_c2, 1); rt_clo_set(c, 0, path); return c; }
Value fsMkdir(void) { return rt_mkclo(&fsMkdir_c1, 0); }
`)
}

// emitStreamPrimsLL bakes the packed-String codec (d6_s2h/d6_h2s) and the 4 pure
// bible ops (byteLen/splitOn/jsonStrField/sqlQuote) into the linked LLVM runtime —
// the LL twin of emitStreamPrimsC. Accessors have EXTERNAL linkage (no static) so
// the .ll's auto-declared @name() resolves. Internal step functions stay static.
// The C↔LLVM delta rule: mkclo→rt_mkclo, clo_set→rt_clo_set, mkcon→rt_mkcon,
// con_set→rt_con_set, big_from_long→rt_big_from_long; big_cmp/big_add/big_mul/
// big_divmod/big_nlimbs/big_limb/UNIT are unchanged (static internals/globals).
func emitStreamPrimsLL(b *strings.Builder, p Program) {
	if usesNativeStrCodec(p) {
		// ARC: `code` is BORROWED; the codec creates bignum temporaries (one/d256/rem +
		// per-iteration quotients) it does NOT return -- each owned K_BIG must be RELEASED
		// (== c.go's d6_s2h/d6_h2s sweep). d6_h2s is called ONCE PER LINE by foldLines, so
		// its per-limb temps would otherwise scale the retained heap with input length.
		b.WriteString("static unsigned char* d6_s2h(Value code, size_t* outlen) { Value one = rt_big_from_long(1); Value d256 = rt_big_from_long(256); size_t cap = 16, n = 0; unsigned char* buf = (unsigned char*)malloc(cap); Value bv = code; while (big_cmp(bv, one) > 0) { Value rem; Value q = big_divmod(bv, d256, &rem); int byte = (big_nlimbs(rem) == 0) ? 0 : (int)(big_limb(rem, 0) & 255); rt_release(rem); if (n == cap) { cap *= 2; buf = (unsigned char*)realloc(buf, cap); } buf[n++] = (unsigned char)byte; if (bv != code) rt_release(bv); bv = q; } if (bv != code) rt_release(bv); rt_release(one); rt_release(d256); *outlen = n; return buf; }\n")
		b.WriteString("static Value d6_h2s(const unsigned char* s, size_t len) { Value n = rt_big_from_long(1); Value m = rt_big_from_long(256); size_t i = len; while (i-- > 0) { Value nm = big_mul(n, m); Value d = rt_big_from_long((long)s[i]); Value nn = big_add(nm, d); rt_release(nm); rt_release(d); rt_release(n); n = nn; } rt_release(m); return n; }\n")
	}
	if usesForeign(p, "byteLen") {
		// ARC (terminal pure op): rt_apply CONSUMES its arg, so a terminal body OWNS its
		// direct arg and RELEASES it when it neither stores nor returns it (== c.go's sweep;
		// WASM byteLen_c1). env[i] slots stay BORROWED -- only the direct arg is owned.
		b.WriteString("static Value byteLen_c1(Value c, Value* env) { (void)env; size_t len; unsigned char* buf = d6_s2h(c, &len); free(buf); rt_release(c); return rt_big_from_long((long)len); }\n")
		b.WriteString("Value byteLen(void) { return rt_mkclo(&byteLen_c1, 0); }\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("static Value splitOn_c2(Value c, Value* env) { long sb = big_nlimbs(env[0]) == 0 ? 0 : (big_limb(env[0], 0) & 255); size_t len; unsigned char* data = d6_s2h(c, &len); Value lst = rt_mkcon(0, \"nil\", 1); rt_con_set(lst, 0, UNIT); size_t segstart = 0; size_t nsegs = 0; size_t* starts = (size_t*)malloc(sizeof(size_t) * (len + 2)); size_t* ends = (size_t*)malloc(sizeof(size_t) * (len + 2)); for (size_t i = 0; i <= len; i++) { if (i == len || data[i] == (unsigned char)sb) { starts[nsegs] = segstart; ends[nsegs] = i; nsegs++; segstart = i + 1; } } for (size_t s = nsegs; s-- > 0; ) { Value part = d6_h2s(data + starts[s], ends[s] - starts[s]); Value cons = rt_mkcon(1, \"cons\", 3); rt_con_set(cons, 0, UNIT); rt_con_set(cons, 1, part); rt_con_set(cons, 2, lst); lst = cons; } free(starts); free(ends); free(data); rt_release(c); return lst; }\n")
		b.WriteString("static Value splitOn_c1(Value sep, Value* env) { (void)env; Value f = rt_mkclo(&splitOn_c2, 1); rt_clo_set(f, 0, sep); return f; }\n")
		b.WriteString("Value splitOn(void) { return rt_mkclo(&splitOn_c1, 0); }\n")
	}
	if usesForeign(p, "jsonStrField") {
		b.WriteString("static Value jsonStrField_c2(Value doc, Value* env) { size_t fl; unsigned char* fnb = d6_s2h(env[0], &fl); size_t dl; unsigned char* ds = d6_s2h(doc, &dl); size_t nl = fl + 2; unsigned char* needle = (unsigned char*)malloc(nl); needle[0] = '\"'; memcpy(needle + 1, fnb, fl); needle[fl + 1] = '\"'; Value none = rt_mkcon(0, \"none\", 1); rt_con_set(none, 0, UNIT); Value result = none; long found = -1; if (nl <= dl) { for (size_t i = 0; i + nl <= dl; i++) { if (memcmp(ds + i, needle, nl) == 0) { found = (long)i; break; } } } if (found >= 0) { size_t j = (size_t)found + nl; while (j < dl && (ds[j] == ' ' || ds[j] == '\\t' || ds[j] == ':')) j++; if (j < dl && ds[j] == '\"') { j++; size_t k = j; while (k < dl && ds[k] != '\"') k++; Value val = d6_h2s(ds + j, k - j); Value some = rt_mkcon(1, \"some\", 2); rt_con_set(some, 0, UNIT); rt_con_set(some, 1, val); result = some; } } free(fnb); free(ds); free(needle); if (result != none) rt_release(none); rt_release(doc); return result; }\n")
		b.WriteString("static Value jsonStrField_c1(Value field, Value* env) { (void)env; Value f = rt_mkclo(&jsonStrField_c2, 1); rt_clo_set(f, 0, field); return f; }\n")
		b.WriteString("Value jsonStrField(void) { return rt_mkclo(&jsonStrField_c1, 0); }\n")
	}
	if usesForeign(p, "sqlQuote") {
		b.WriteString("static Value sqlQuote_c1(Value s, Value* env) { (void)env; size_t len; unsigned char* in = d6_s2h(s, &len); unsigned char* out = (unsigned char*)malloc(len * 2 + 2); size_t o = 0; out[o++] = '\\''; for (size_t i = 0; i < len; i++) { if (in[i] == '\\'') out[o++] = '\\''; out[o++] = in[i]; } out[o++] = '\\''; Value r = d6_h2s(out, o); free(in); free(out); rt_release(s); return r; }\n")
		b.WriteString("Value sqlQuote(void) { return rt_mkclo(&sqlQuote_c1, 0); }\n")
	}
	// D6 / R-EFFECT: the standard OS vocabulary on native LLVM (env / file / argv /
	// process-exit). Accessors have EXTERNAL linkage (no static) so the .ll's
	// auto-declared @name() resolves. Internal step functions stay static.
	// The C↔LLVM delta: mkclo→rt_mkclo, clo_set→rt_clo_set, big_from_long→rt_big_from_long.
	if usesForeign(p, "printStrCode") {
		b.WriteString("static Value printStrCode_c2(Value u, Value* env) { (void)u; size_t len; unsigned char* buf = d6_s2h(env[0], &len); fwrite(buf, 1, len, stdout); putchar('\\n'); free(buf); rt_retain(env[0]); return env[0]; }\n")
		b.WriteString("static Value printStrCode_c1(Value c, Value* env) { (void)env; Value f = rt_mkclo(&printStrCode_c2, 1); rt_clo_set(f, 0, c); return f; }\n")
		b.WriteString("Value printStrCode(void) { return rt_mkclo(&printStrCode_c1, 0); }\n")
	}
	if usesForeign(p, "getEnvCode") {
		b.WriteString("static Value getEnvCode_c2(Value u, Value* env) { (void)u; size_t kl; unsigned char* kb = d6_s2h(env[0], &kl); char* key = (char*)malloc(kl + 1); memcpy(key, kb, kl); key[kl] = 0; const char* val = getenv(key); if (!val) val = \"\"; Value r = d6_h2s((const unsigned char*)val, strlen(val)); free(kb); free(key); return r; }\n")
		b.WriteString("static Value getEnvCode_c1(Value c, Value* env) { (void)env; Value f = rt_mkclo(&getEnvCode_c2, 1); rt_clo_set(f, 0, c); return f; }\n")
		b.WriteString("Value getEnvCode(void) { return rt_mkclo(&getEnvCode_c1, 0); }\n")
	}
	if usesForeign(p, "readFileCode") {
		b.WriteString("static Value readFileCode_c2(Value u, Value* env) { (void)u; size_t pl; unsigned char* pb = d6_s2h(env[0], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; FILE* fp = fopen(path, \"rb\"); free(pb); free(path); if (!fp) return rt_big_from_long(1); size_t cap = 1024, n = 0; unsigned char* data = (unsigned char*)malloc(cap); int ch; while ((ch = fgetc(fp)) != EOF) { if (n == cap) { cap *= 2; data = (unsigned char*)realloc(data, cap); } data[n++] = (unsigned char)ch; } fclose(fp); Value r = d6_h2s(data, n); free(data); return r; }\n")
		b.WriteString("static Value readFileCode_c1(Value c, Value* env) { (void)env; Value f = rt_mkclo(&readFileCode_c2, 1); rt_clo_set(f, 0, c); return f; }\n")
		b.WriteString("Value readFileCode(void) { return rt_mkclo(&readFileCode_c1, 0); }\n")
	}
	if usesForeign(p, "writeFileCode") {
		b.WriteString("static Value writeFileCode_c3(Value u, Value* env) { (void)u; size_t pl; unsigned char* pb = d6_s2h(env[0], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; size_t dl; unsigned char* data = d6_s2h(env[1], &dl); FILE* fp = fopen(path, \"wb\"); if (fp) { fwrite(data, 1, dl, fp); fclose(fp); } free(pb); free(path); free(data); rt_retain(env[1]); return env[1]; }\n")
		b.WriteString("static Value writeFileCode_c2(Value c, Value* env) { Value f = rt_mkclo(&writeFileCode_c3, 2); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, c); return f; }\n")
		b.WriteString("static Value writeFileCode_c1(Value pth, Value* env) { (void)env; Value f = rt_mkclo(&writeFileCode_c2, 1); rt_clo_set(f, 0, pth); return f; }\n")
		b.WriteString("Value writeFileCode(void) { return rt_mkclo(&writeFileCode_c1, 0); }\n")
	}
	// argCountCode + argAtCode need the rune_argc/rune_argv globals set by main.
	if usesForeign(p, "argCountCode") || usesForeign(p, "argAtCode") {
		b.WriteString("static int rune_argc = 0;\nstatic char** rune_argv = NULL;\n")
	}
	if usesForeign(p, "argCountCode") {
		b.WriteString("static Value argCountCode_c1(Value u, Value* env) { (void)u; (void)env; long n = rune_argc > 1 ? rune_argc - 1 : 0; return rt_big_from_long(n); }\n")
		b.WriteString("Value argCountCode(void) { return rt_mkclo(&argCountCode_c1, 0); }\n")
	}
	if usesForeign(p, "argAtCode") {
		b.WriteString("static Value argAtCode_c2(Value u, Value* env) { (void)u; long idx = (big_nlimbs(env[0]) == 0 ? 0 : (long)big_limb(env[0], 0)) + 1; if (idx >= 1 && idx < rune_argc) { const char* a = rune_argv[idx]; return d6_h2s((const unsigned char*)a, strlen(a)); } return rt_big_from_long(1); }\n")
		b.WriteString("static Value argAtCode_c1(Value i, Value* env) { (void)env; Value f = rt_mkclo(&argAtCode_c2, 1); rt_clo_set(f, 0, i); return f; }\n")
		b.WriteString("Value argAtCode(void) { return rt_mkclo(&argAtCode_c1, 0); }\n")
	}
	if usesForeign(p, "exitWith") {
		b.WriteString("static Value exitWith_c2(Value u, Value* env) { (void)u; long code = (big_nlimbs(env[0]) == 0 ? 0 : (long)big_limb(env[0], 0)); exit((int)code); }\n")
		b.WriteString("static Value exitWith_c1(Value n, Value* env) { (void)env; Value f = rt_mkclo(&exitWith_c2, 1); rt_clo_set(f, 0, n); return f; }\n")
		b.WriteString("Value exitWith(void) { return rt_mkclo(&exitWith_c1, 0); }\n")
	}
	// Higher-order stream ops: LLVM twin of emitStreamPrimsC foldLines/foldDir.
	// Delta rule: apply->rt_apply, mkclo->rt_mkclo, clo_set->rt_clo_set,
	// big_from_long->rt_big_from_long; d6_s2h/d6_h2s/UNIT/big_* unchanged.
	// Accessor thunks (foldLines/foldDir) are NON-static (external linkage) so the
	// .ll's auto-declared @foldLines/@foldDir resolves. Helpers + step functions stay static.
	if usesForeign(p, "foldLines") {
		// ARC (== c.go's foldLines sweep, WASM foldLines_c5 twin): env slots step/s0/path
		// are BORROWED (never released). The accumulator s starts as env[1] (s0) RETAINED so
		// it is an owned reference the fold consumes; on a missing file it is returned owned.
		// rt_apply borrows its closure operand + consumes its arg -- so `step` stays borrowed
		// (applied directly), the fresh `line` is owned+consumed, and each intermediate
		// apply-closure ca/cb is OWNED here + RELEASED after use (else one leak per line).
		b.WriteString("static Value foldLines_c5(Value u, Value* env) { (void)u; Value step = env[0]; size_t pl; unsigned char* pb = d6_s2h(env[2], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; free(pb); Value s = env[1]; rt_retain(s); FILE* fp = fopen(path, \"rb\"); free(path); if (!fp) return s; size_t cap = 1024, n = 0; unsigned char* data = (unsigned char*)malloc(cap); int ch; while ((ch = fgetc(fp)) != EOF) { if (n == cap) { cap *= 2; data = (unsigned char*)realloc(data, cap); } data[n++] = (unsigned char)ch; } fclose(fp); size_t nseg = 0, seg = 0; size_t* st = (size_t*)malloc(sizeof(size_t)*(n+2)); size_t* en = (size_t*)malloc(sizeof(size_t)*(n+2)); for (size_t k = 0; k <= n; k++) { if (k == n || data[k] == '\\n') { st[nseg]=seg; en[nseg]=k; nseg++; seg=k+1; } } if (nseg > 0 && en[nseg-1] == st[nseg-1]) nseg--; for (size_t k = 0; k < nseg; k++) { Value line = d6_h2s(data + st[k], en[k]-st[k]); Value ca = rt_apply(step, s); Value cb = rt_apply(ca, line); rt_release(ca); s = rt_apply(cb, UNIT); rt_release(cb); } free(st); free(en); free(data); return s; }\n")
		b.WriteString("static Value foldLines_c4(Value s0, Value* env) { Value f = rt_mkclo(&foldLines_c5, 3); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, s0); rt_retain(env[1]); rt_clo_set(f, 2, env[1]); return f; }\n")
		b.WriteString("static Value foldLines_c3(Value step, Value* env) { Value f = rt_mkclo(&foldLines_c4, 2); rt_clo_set(f, 0, step); rt_retain(env[0]); rt_clo_set(f, 1, env[0]); return f; }\n")
		b.WriteString("static Value foldLines_c2(Value path, Value* env) { (void)env; Value f = rt_mkclo(&foldLines_c3, 1); rt_clo_set(f, 0, path); return f; }\n")
		b.WriteString("static Value foldLines_c1(Value _s, Value* env) { (void)_s; (void)env; return rt_mkclo(&foldLines_c2, 0); }\n")
		b.WriteString("Value foldLines(void) { return rt_mkclo(&foldLines_c1, 0); }\n")
	}
	// foldDir LLVM twin: same d6_namecmp + d6_foldwalk helpers (static) + curried chain
	// (static inner steps, external accessor). Needs <dirent.h> + <sys/stat.h>.
	if usesForeign(p, "foldDir") {
		b.WriteString("#include <dirent.h>\n")
		b.WriteString("#include <sys/stat.h>\n")
		b.WriteString("static int d6_namecmp(const void* a, const void* b) { return strcmp(*(const char* const*)a, *(const char* const*)b); }\n")
		b.WriteString("static Value d6_foldwalk(const char* dir, const unsigned char* sfx, size_t sfxlen, Value step, Value s) { DIR* dp = opendir(dir); if (!dp) return s; char** names = NULL; size_t cnt = 0, cap = 0; struct dirent* de; while ((de = readdir(dp)) != NULL) { if (strcmp(de->d_name, \".\") == 0 || strcmp(de->d_name, \"..\") == 0) continue; if (cnt == cap) { cap = cap ? cap * 2 : 8; names = (char**)realloc(names, cap * sizeof(char*)); } names[cnt++] = strdup(de->d_name); } closedir(dp); qsort(names, cnt, sizeof(char*), d6_namecmp); for (size_t i = 0; i < cnt; i++) { size_t dl = strlen(dir), nl = strlen(names[i]); char* full = (char*)malloc(dl + nl + 2); memcpy(full, dir, dl); full[dl] = '/'; memcpy(full + dl + 1, names[i], nl + 1); struct stat sb; if (stat(full, &sb) == 0 && S_ISDIR(sb.st_mode)) { s = d6_foldwalk(full, sfx, sfxlen, step, s); } else { size_t fl = strlen(full); if (fl >= sfxlen && memcmp(full + fl - sfxlen, sfx, sfxlen) == 0) { FILE* fp = fopen(full, \"rb\"); if (fp) { size_t bc = 1024, bn = 0; unsigned char* bd = (unsigned char*)malloc(bc); int ch; while ((ch = fgetc(fp)) != EOF) { if (bn == bc) { bc *= 2; bd = (unsigned char*)realloc(bd, bc); } bd[bn++] = (unsigned char)ch; } fclose(fp); Value content = d6_h2s(bd, bn); Value ca = rt_apply(step, s); Value cb = rt_apply(ca, content); rt_release(ca); s = rt_apply(cb, UNIT); rt_release(cb); free(bd); } } } free(full); free(names[i]); } free(names); return s; }\n")
		// ARC: step (env[0]) BORROWED; s0 (env[1]) RETAINED before it enters d6_foldwalk (which
		// consumes it and threads/returns an owned accumulator). Curry blocks retain each
		// forwarded env slot (builders take ownership) + MOVE the direct owned arg (== c.go).
		b.WriteString("static Value foldDir_c6(Value u, Value* env) { (void)u; Value step = env[0]; Value s0 = env[1]; rt_retain(s0); size_t dl; unsigned char* db = d6_s2h(env[2], &dl); char* dir = (char*)malloc(dl + 1); memcpy(dir, db, dl); dir[dl] = 0; free(db); size_t sl; unsigned char* sfx = d6_s2h(env[3], &sl); Value r = d6_foldwalk(dir, sfx, sl, step, s0); free(dir); free(sfx); return r; }\n")
		b.WriteString("static Value foldDir_c5(Value s0, Value* env) { Value f = rt_mkclo(&foldDir_c6, 4); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, s0); rt_retain(env[1]); rt_clo_set(f, 2, env[1]); rt_retain(env[2]); rt_clo_set(f, 3, env[2]); return f; }\n")
		b.WriteString("static Value foldDir_c4(Value step, Value* env) { Value f = rt_mkclo(&foldDir_c5, 3); rt_clo_set(f, 0, step); rt_retain(env[0]); rt_clo_set(f, 1, env[0]); rt_retain(env[1]); rt_clo_set(f, 2, env[1]); return f; }\n")
		b.WriteString("static Value foldDir_c3(Value suf, Value* env) { Value f = rt_mkclo(&foldDir_c4, 2); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, suf); return f; }\n")
		b.WriteString("static Value foldDir_c2(Value dirc, Value* env) { (void)env; Value f = rt_mkclo(&foldDir_c3, 1); rt_clo_set(f, 0, dirc); return f; }\n")
		b.WriteString("static Value foldDir_c1(Value _s, Value* env) { (void)_s; (void)env; return rt_mkclo(&foldDir_c2, 0); }\n")
		b.WriteString("Value foldDir(void) { return rt_mkclo(&foldDir_c1, 0); }\n")
	}
	// WRITE-STREAM ops: LLVM twin of the C equivalents. Same bodies with the delta:
	// rt_mkclo/rt_clo_set/rt_big_from_long (rt_ prefix); big_nlimbs/big_limb/UNIT stay as-is.
	// Accessor thunks (Handle/openWrite/writeChunk/closeWrite/sortFile/dbApply) are NON-static
	// (external linkage) so the .ll's auto-declared foreign names resolve. Internal _c1/_c2/_c3
	// steps + d6_sortsegs + the handle-table statics remain static.
	if usesForeign(p, "openWrite") || usesForeign(p, "writeChunk") || usesForeign(p, "closeWrite") {
		b.WriteString("#define D6_WH_MAX 256\nstatic FILE* d6_wh[D6_WH_MAX];\nstatic long d6_whid = 0;\n")
	}
	if usesForeign(p, "Handle") {
		b.WriteString("Value Handle(void) { return UNIT; }\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("static Value openWrite_c2(Value u, Value* env) { (void)u; size_t pl; unsigned char* pb = d6_s2h(env[0], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; free(pb); FILE* fp = fopen(path, \"wb\"); free(path); if (!fp || d6_whid + 1 >= D6_WH_MAX) { if (fp) fclose(fp); return rt_big_from_long(0); } long id = ++d6_whid; d6_wh[id] = fp; return rt_big_from_long(id); }\n")
		b.WriteString("static Value openWrite_c1(Value path, Value* env) { (void)env; Value f = rt_mkclo(&openWrite_c2, 1); rt_clo_set(f, 0, path); return f; }\n")
		b.WriteString("Value openWrite(void) { return rt_mkclo(&openWrite_c1, 0); }\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("static Value writeChunk_c3(Value u, Value* env) { (void)u; Value h = env[0]; long id = (big_nlimbs(h) == 0) ? 0 : big_limb(h, 0); size_t cl; unsigned char* cb = d6_s2h(env[1], &cl); if (id > 0 && id < D6_WH_MAX && d6_wh[id]) { fwrite(cb, 1, cl, d6_wh[id]); putc('\\n', d6_wh[id]); } free(cb); rt_retain(h); return h; }\n")
		b.WriteString("static Value writeChunk_c2(Value c, Value* env) { Value f = rt_mkclo(&writeChunk_c3, 2); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, c); return f; }\n")
		b.WriteString("static Value writeChunk_c1(Value h, Value* env) { (void)env; Value f = rt_mkclo(&writeChunk_c2, 1); rt_clo_set(f, 0, h); return f; }\n")
		b.WriteString("Value writeChunk(void) { return rt_mkclo(&writeChunk_c1, 0); }\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("static Value closeWrite_c2(Value u, Value* env) { (void)u; Value h = env[0]; long id = (big_nlimbs(h) == 0) ? 0 : big_limb(h, 0); if (id > 0 && id < D6_WH_MAX && d6_wh[id]) { fclose(d6_wh[id]); d6_wh[id] = NULL; } return UNIT; }\n")
		b.WriteString("static Value closeWrite_c1(Value h, Value* env) { (void)env; Value f = rt_mkclo(&closeWrite_c2, 1); rt_clo_set(f, 0, h); return f; }\n")
		b.WriteString("Value closeWrite(void) { return rt_mkclo(&closeWrite_c1, 0); }\n")
	}
	if usesForeign(p, "sortFile") {
		// d6_sortsegs: insertion sort on parallel (st,ln) arrays comparing data+st[i] bytewise.
		b.WriteString("static void d6_sortsegs(unsigned char* data, size_t* st, size_t* ln, size_t nseg) { for (size_t i = 1; i < nseg; i++) { size_t si = st[i], li = ln[i]; size_t j = i; while (j > 0) { size_t sp = st[j-1], lp = ln[j-1]; size_t m = lp < li ? lp : li; int c = memcmp(data + sp, data + si, m); if (c > 0 || (c == 0 && lp > li)) { st[j] = sp; ln[j] = lp; j--; } else break; } st[j] = si; ln[j] = li; } }\n")
		b.WriteString("static Value sortFile_c3(Value u, Value* env) { (void)u; size_t il; unsigned char* ib = d6_s2h(env[0], &il); char* ip = (char*)malloc(il+1); memcpy(ip, ib, il); ip[il]=0; free(ib); size_t ol; unsigned char* ob = d6_s2h(env[1], &ol); char* op = (char*)malloc(ol+1); memcpy(op, ob, ol); op[ol]=0; free(ob); FILE* fp = fopen(ip, \"rb\"); free(ip); if (!fp) { FILE* o = fopen(op, \"wb\"); if (o) fclose(o); free(op); return UNIT; } size_t cap=1024,n=0; unsigned char* data=(unsigned char*)malloc(cap); int ch; while((ch=fgetc(fp))!=EOF){ if(n==cap){cap*=2;data=(unsigned char*)realloc(data,cap);} data[n++]=(unsigned char)ch; } fclose(fp); size_t* st=(size_t*)malloc(sizeof(size_t)*(n+2)); size_t* ln=(size_t*)malloc(sizeof(size_t)*(n+2)); size_t nseg=0, seg=0; for(size_t k=0;k<=n;k++){ if(k==n||data[k]=='\\n'){ st[nseg]=seg; ln[nseg]=k-seg; nseg++; seg=k+1; } } if(nseg>0 && ln[nseg-1]==0) nseg--; d6_sortsegs(data, st, ln, nseg); FILE* o=fopen(op,\"wb\"); if(o){ for(size_t k=0;k<nseg;k++){ fwrite(data+st[k],1,ln[k],o); putc('\\n',o); } fclose(o); } free(st); free(ln); free(data); free(op); return UNIT; }\n")
		b.WriteString("static Value sortFile_c2(Value outp, Value* env) { Value f = rt_mkclo(&sortFile_c3, 2); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, outp); return f; }\n")
		b.WriteString("static Value sortFile_c1(Value inp, Value* env) { (void)env; Value f = rt_mkclo(&sortFile_c2, 1); rt_clo_set(f, 0, inp); return f; }\n")
		b.WriteString("Value sortFile(void) { return rt_mkclo(&sortFile_c1, 0); }\n")
	}
	if usesForeign(p, "dbApply") {
		b.WriteString("static Value dbApply_c3(Value u, Value* env) { (void)u; size_t dl; unsigned char* db = d6_s2h(env[0], &dl); char* dbp = (char*)malloc(dl+1); memcpy(dbp, db, dl); dbp[dl]=0; free(db); size_t sl; unsigned char* sb = d6_s2h(env[1], &sl); char* sqp = (char*)malloc(sl+1); memcpy(sqp, sb, sl); sqp[sl]=0; free(sb); size_t clen = dl + sl + 32; char* cmd = (char*)malloc(clen); snprintf(cmd, clen, \"sqlite3 '%s' '.read %s'\", dbp, sqp); int rc = system(cmd); (void)rc; free(dbp); free(sqp); free(cmd); return UNIT; }\n")
		b.WriteString("static Value dbApply_c2(Value sql, Value* env) { Value f = rt_mkclo(&dbApply_c3, 2); rt_retain(env[0]); rt_clo_set(f, 0, env[0]); rt_clo_set(f, 1, sql); return f; }\n")
		b.WriteString("static Value dbApply_c1(Value db, Value* env) { (void)env; Value f = rt_mkclo(&dbApply_c2, 1); rt_clo_set(f, 0, db); return f; }\n")
		b.WriteString("Value dbApply(void) { return rt_mkclo(&dbApply_c1, 0); }\n")
	}
}

// emitFloatIOPrimsLL bakes parseFloat/getFloat/printFloat into the LLVM runtime.c.
// Emitted AFTER emitStreamPrimsLL because parseFloat calls d6_s2h. Mirrors
// emitFloatIOPrimsC with rt_-prefixed mkclo/clo_set/mkcon/con_set and external
// linkage on thunk entry points (the .ll auto-declares them via foreignNames).
// printFloat_c2 formats via __fmtf (ECMAScript Number::toString shortest
// round-trip), which now lives in the always-emitted base runtime (llRuntimeC),
// not here, so show()'s K_FLOAT case can reach it too (Track B Task 2, the LL
// mirror of Task 1's c.go move). d6_validate_float is a DFA for the float
// literal grammar used by parseFloat.
func emitFloatIOPrimsLL(b *strings.Builder, p Program) {
	usesFloatIO := usesForeign(p, "parseFloat") || usesForeign(p, "getFloat") || usesForeign(p, "printFloat")
	if !usesFloatIO {
		return
	}
	// locale.h required for setlocale(LC_NUMERIC, "C") called from the float main.
	b.WriteString("#include <locale.h>\n")
	b.WriteString("static int d6_validate_float(const unsigned char* s, size_t len) { if (len == 0) return 0; size_t i = 0; if (s[i] == '+' || s[i] == '-') i++; int has_digit = 0; while (i < len && s[i] >= '0' && s[i] <= '9') { has_digit = 1; i++; } if (i < len && s[i] == '.') { i++; while (i < len && s[i] >= '0' && s[i] <= '9') { has_digit = 1; i++; } } if (!has_digit) return 0; if (i < len && (s[i] == 'e' || s[i] == 'E')) { i++; if (i < len && (s[i] == '+' || s[i] == '-')) i++; int hd = 0; while (i < len && s[i] >= '0' && s[i] <= '9') { hd = 1; i++; } if (!hd) return 0; } return i == (size_t)len; }\n")
	if usesForeign(p, "parseFloat") {
		b.WriteString("static Value parseFloat_c(Value code, Value* env) { (void)env; size_t len; unsigned char* buf = d6_s2h(code, &len); if (!d6_validate_float(buf, len)) { free(buf); rt_release(code); Value none = rt_mkcon(0, \"none\", 1); rt_con_set(none, 0, UNIT); return none; } char* tmp = (char*)malloc(len + 1); memcpy(tmp, buf, len); tmp[len] = 0; double d = strtod(tmp, NULL); free(tmp); free(buf); rt_release(code); Value some = rt_mkcon(1, \"some\", 2); rt_con_set(some, 0, UNIT); rt_con_set(some, 1, mkfloat(d)); return some; }\n")
		b.WriteString("Value parseFloat(void) { return rt_mkclo(&parseFloat_c, 0); }\n")
	}
	if usesForeign(p, "getFloat") {
		// Contract: read one '\n'-terminated line (max 255 chars; longer lines are
		// garbage -> 0.0), strip trailing '\n' then all trailing '\r', validate the
		// whole remainder against the float DFA, strtod only on pass. EOF -> 0.0.
		b.WriteString("static Value getFloat_c1(Value u, Value* env) { (void)u; (void)env; char buf[256]; if (!fgets(buf, sizeof buf, stdin)) return mkfloat(0.0); size_t n = strlen(buf); if (n > 0 && buf[n-1] == '\\n') { n--; buf[n] = '\\0'; } while (n > 0 && buf[n-1] == '\\r') { n--; buf[n] = '\\0'; } if (!d6_validate_float((const unsigned char*)buf, n)) return mkfloat(0.0); return mkfloat(strtod(buf, NULL)); }\n")
		b.WriteString("Value getFloat(void) { return rt_mkclo(&getFloat_c1, 0); }\n")
	}
	if usesForeign(p, "printFloat") {
		b.WriteString("static Value printFloat_c2(Value u, Value* env) { (void)u; char buf[64]; __fmtf(float_val(env[0]), buf); printf(\"%s\\n\", buf); rt_retain(env[0]); return env[0]; }\n")
		b.WriteString("static Value printFloat_c1(Value f, Value* env) { (void)env; Value c = rt_mkclo(&printFloat_c2, 1); rt_clo_set(c, 0, f); return c; }\n")
		b.WriteString("Value printFloat(void) { return rt_mkclo(&printFloat_c1, 0); }\n")
	}
}

// emitBinPrimsLL bakes the Phase-0 real-byte-string (Bin) host bodies into the
// linked LLVM runtime — the LL twin of emitBinPrimsC. Accessors are EXTERNAL (the
// .ll calls @binEmpty/@binCons/… which foreignNames auto-declares); the closure
// code blocks are static internals over the runtime's rt_*/bytes_*/big_* helpers.
func emitBinPrimsLL(b *strings.Builder, p Program) {
	if !usesBin(p) {
		return
	}
	b.WriteString("Value binEmpty(void) { return mkbytes(0); }\n")
	b.WriteString(`static Value binCons_c2(Value bb, Value* env) { Value c = env[0]; int bv = (big_nlimbs(c) == 0 ? 0 : (int)big_limb(c, 0)) & 255; int n = bytes_len(bb); Value r = mkbytes(n + 1); bytes_set(r, 0, bv); for (int i = 0; i < n; i++) bytes_set(r, i + 1, bytes_at(bb, i)); rt_release(bb); return r; }
static Value binCons_c1(Value c, Value* env) { (void)env; Value k = rt_mkclo(&binCons_c2, 1); rt_clo_set(k, 0, c); return k; }
Value binCons(void) { return rt_mkclo(&binCons_c1, 0); }
static Value binLen_c1(Value bb, Value* env) { (void)env; Value r = rt_big_from_long(bytes_len(bb)); rt_release(bb); return r; }
Value binLen(void) { return rt_mkclo(&binLen_c1, 0); }
static Value binAt_c2(Value i, Value* env) { Value bb = env[0]; long k = (big_nlimbs(i) == 0 ? 0 : big_limb(i, 0)); rt_release(i); int n = bytes_len(bb); if (k >= 0 && k < n) return rt_big_from_long(bytes_at(bb, (int)k)); return rt_big_from_long(0); }
static Value binAt_c1(Value bb, Value* env) { (void)env; Value k = rt_mkclo(&binAt_c2, 1); rt_clo_set(k, 0, bb); return k; }
Value binAt(void) { return rt_mkclo(&binAt_c1, 0); }
static Value printBin_c2(Value w, Value* env) { (void)w; Value bb = env[0]; int n = bytes_len(bb); putchar('"'); for (int i = 0; i < n; i++) { int x = bytes_at(bb, i); if (x >= 0x20 && x < 0x7f && x != 0x22 && x != 0x5c) putchar(x); else printf("\\x%02x", x); } putchar('"'); putchar('\n'); return mkunit(); }
static Value printBin_c1(Value bb, Value* env) { (void)env; Value c = rt_mkclo(&printBin_c2, 1); rt_clo_set(c, 0, bb); return c; }
Value printBin(void) { return rt_mkclo(&printBin_c1, 0); }
`)
}

// emitFloatPrimsLL bakes the float/BLAS host bodies into the LINKED LLVM runtime,
// with EXTERNAL linkage on the accessors (called from the .ll) and the runtime's
// rt_* closure/bignum helpers. The bodies mirror emitFloatPrimsC; dot2 swaps in
// cblas_ddot (link -lopenblas).
func emitFloatPrimsLL(b *strings.Builder, p Program) {
	if usesForeign(p, "printNat") {
		b.WriteString("static Value printNat_c2(Value w, Value* env) { (void)w; Value n = env[0]; if (IS_INT(n)) printf(\"%ld\\n\", INT_VAL(n)); else { big_print(n); putchar('\\n'); } rt_retain(n); return n; }\n")
		b.WriteString("static Value printNat_c1(Value n, Value* env) { (void)env; Value c = rt_mkclo(&printNat_c2, 1); rt_clo_set(c, 0, n); return c; }\n")
		b.WriteString("Value printNat(void) { return rt_mkclo(&printNat_c1, 0); }\n")
	}
	if usesForeign(p, "getNat") {
		b.WriteString("static Value getNat_c1(Value u, Value* env) { (void)u; (void)env; long x = 0; if (scanf(\"%ld\", &x) != 1) x = 0; return rt_big_from_long(x); }\n")
		b.WriteString("Value getNat(void) { return rt_mkclo(&getNat_c1, 0); }\n")
	}
	if usesForeign(p, "Float") {
		b.WriteString("Value Float(void) { return UNIT; }\n")
	}
	if usesForeign(p, "fromNat") {
		b.WriteString("static Value fromNat_c(Value a, Value* env) { (void)env; Value r = mkfloat(big_to_double(a)); rt_release(a); return r; }\n")
		b.WriteString("Value fromNat(void) { return rt_mkclo(&fromNat_c, 0); }\n")
	}
	if usesForeign(p, "floatToNat") {
		b.WriteString("static Value floatToNat_c(Value x, Value* env) { (void)env; Value r = rt_big_from_long((long)float_val(x)); rt_release(x); return r; }\n")
		b.WriteString("Value floatToNat(void) { return rt_mkclo(&floatToNat_c, 0); }\n")
	}
	bin := func(name, op string) {
		if usesForeign(p, name) {
			fmt.Fprintf(b, "static Value %s_c2(Value y, Value* env) { Value r = mkfloat(float_val(env[0]) %s float_val(y)); rt_release(y); return r; }\n", name, op)
			fmt.Fprintf(b, "static Value %s_c1(Value x, Value* env) { (void)env; Value c = rt_mkclo(&%s_c2, 1); rt_clo_set(c, 0, x); return c; }\n", name, name)
			fmt.Fprintf(b, "Value %s(void) { return rt_mkclo(&%s_c1, 0); }\n", name, name)
		}
	}
	bin("fadd", "+")
	bin("fsub", "-")
	bin("fmul", "*")
	bin("fdiv", "/")
	if usesForeign(p, "fleqN") {
		b.WriteString("static Value fleqN_c2(Value y, Value* env) { Value r = float_val(env[0]) <= float_val(y) ? rt_big_from_long(1) : rt_big_from_long(0); rt_release(y); return r; }\n")
		b.WriteString("static Value fleqN_c1(Value x, Value* env) { (void)env; Value c = rt_mkclo(&fleqN_c2, 1); rt_clo_set(c, 0, x); return c; }\n")
		b.WriteString("Value fleqN(void) { return rt_mkclo(&fleqN_c1, 0); }\n")
	}
	if usesForeign(p, "fsqrt") {
		b.WriteString("#include <math.h>\n")
		b.WriteString("static Value fsqrt_c(Value x, Value* env) { (void)env; Value r = mkfloat(sqrt(float_val(x))); rt_release(x); return r; }\n")
		b.WriteString("Value fsqrt(void) { return rt_mkclo(&fsqrt_c, 0); }\n")
	}
	if usesForeign(p, "fpow") {
		if !usesForeign(p, "fsqrt") {
			b.WriteString("#include <math.h>\n")
		}
		b.WriteString("static Value fpow_c2(Value e, Value* env) { Value r = mkfloat(pow(float_val(env[0]), float_val(e))); rt_release(e); return r; }\n")
		b.WriteString("static Value fpow_c1(Value bs, Value* env) { (void)env; Value c = rt_mkclo(&fpow_c2, 1); rt_clo_set(c, 0, bs); return c; }\n")
		b.WriteString("Value fpow(void) { return rt_mkclo(&fpow_c1, 0); }\n")
	}
	if usesForeign(p, "fabsP") {
		b.WriteString("static Value fabsP_c(Value x, Value* env) { (void)env; double d = float_val(x); rt_release(x); return mkfloat(d < 0 ? -d : d); }\n")
		b.WriteString("Value fabsP(void) { return rt_mkclo(&fabsP_c, 0); }\n")
	}
	if usesForeign(p, "dot2") || usesForeign(p, "dotList") || usesForeign(p, "gemmSum") || usesForeign(p, "npDot") || usesForeign(p, "npMatSum") {
		b.WriteString("#include <cblas.h>\n")
	}
	// shared FList marshaller (vectors + flat matrices), used by dotList/npDot and gemmSum/npMatSum
	// and the numpy-embed prims (pyNpSum/pyNpScale/pyNpMatVec).
	if usesForeign(p, "dotList") || usesForeign(p, "gemmSum") || usesForeign(p, "npDot") || usesForeign(p, "npMatSum") || usesForeign(p, "pyNpSum") || usesForeign(p, "pyNpScale") || usesForeign(p, "pyNpMatVec") {
		b.WriteString("static void fl_fill(Value lst, double* a) { int i = 0; while (!IS_INT(lst) && obj(lst)->kind == K_CON && obj(lst)->tag == 1) { a[i++] = float_val(obj(lst)->slots[0]); lst = obj(lst)->slots[1]; } }\n")
	}
	if usesForeign(p, "dotList") || usesForeign(p, "npDot") || usesForeign(p, "pyNpSum") || usesForeign(p, "pyNpScale") || usesForeign(p, "pyNpMatVec") {
		b.WriteString("static int fl_len(Value lst) { int n = 0; while (!IS_INT(lst) && obj(lst)->kind == K_CON && obj(lst)->tag == 1) { n++; lst = obj(lst)->slots[1]; } return n; }\n")
	}
	if usesForeign(p, "dot2") {
		b.WriteString("static Value dot2_c4(Value b1, Value* env) { double X[2] = { float_val(env[0]), float_val(env[1]) }; double Y[2] = { float_val(env[2]), float_val(b1) }; rt_release(b1); return mkfloat(cblas_ddot(2, X, 1, Y, 1)); }\n")
		b.WriteString("static Value dot2_c3(Value b0, Value* env) { Value c = rt_mkclo(&dot2_c4, 3); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_clo_set(c, 2, b0); return c; }\n")
		b.WriteString("static Value dot2_c2(Value a1, Value* env) { Value c = rt_mkclo(&dot2_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, a1); return c; }\n")
		b.WriteString("static Value dot2_c1(Value a0, Value* env) { (void)env; Value c = rt_mkclo(&dot2_c2, 1); rt_clo_set(c, 0, a0); return c; }\n")
		b.WriteString("Value dot2(void) { return rt_mkclo(&dot2_c1, 0); }\n")
	}
	// arbitrary-length ddot: marshal a Rune FList into a C double[], then cblas_ddot.
	if usesForeign(p, "dotList") {
		b.WriteString("static Value dotList_c2(Value ys, Value* env) { Value xs = env[0]; int n = fl_len(xs), m = fl_len(ys); int k = n < m ? n : m; double* X = (double*)malloc(sizeof(double) * (k > 0 ? k : 1)); double* Y = (double*)malloc(sizeof(double) * (k > 0 ? k : 1)); fl_fill(xs, X); fl_fill(ys, Y); double r = cblas_ddot(k, X, 1, Y, 1); free(X); free(Y); rt_release(ys); return mkfloat(r); }\n")
		b.WriteString("static Value dotList_c1(Value xs, Value* env) { (void)env; Value c = rt_mkclo(&dotList_c2, 1); rt_clo_set(c, 0, xs); return c; }\n")
		b.WriteString("Value dotList(void) { return rt_mkclo(&dotList_c1, 0); }\n")
	}
	// D4 interop: npDot on the native backends is the OpenBLAS gift (cblas_ddot), the
	// same uniform capability the py backend serves with NumPy.
	if usesForeign(p, "npDot") {
		b.WriteString("static Value npDot_c2(Value ys, Value* env) { Value xs = env[0]; int n = fl_len(xs), m = fl_len(ys); int k = n < m ? n : m; double* X = (double*)malloc(sizeof(double) * (k > 0 ? k : 1)); double* Y = (double*)malloc(sizeof(double) * (k > 0 ? k : 1)); fl_fill(xs, X); fl_fill(ys, Y); double r = cblas_ddot(k, X, 1, Y, 1); free(X); free(Y); rt_release(ys); return mkfloat(r); }\n")
		b.WriteString("static Value npDot_c1(Value xs, Value* env) { (void)env; Value c = rt_mkclo(&npDot_c2, 1); rt_clo_set(c, 0, xs); return c; }\n")
		b.WriteString("Value npDot(void) { return rt_mkclo(&npDot_c1, 0); }\n")
	}
	// D4 interop: npMean — hand sum/count (no BLAS); py serves real numpy.mean.
	if usesForeign(p, "npMean") {
		b.WriteString("static Value npMean_c(Value xs, Value* env) { (void)env; Value h = xs; double s = 0; int n = 0; while (!IS_INT(xs) && obj(xs)->kind == K_CON && obj(xs)->tag == 1) { s += float_val(obj(xs)->slots[0]); n++; xs = obj(xs)->slots[1]; } rt_release(h); return mkfloat(n > 0 ? s / n : 0); }\n")
		b.WriteString("Value npMean(void) { return rt_mkclo(&npMean_c, 0); }\n")
	}
	// D4 interop: npVar — 2-pass hand body (no BLAS); py serves real numpy.var.
	if usesForeign(p, "npVar") {
		b.WriteString("static Value npVar_c(Value xs, Value* env) { (void)env; double s = 0; int n = 0; Value t = xs; while (!IS_INT(t) && obj(t)->kind == K_CON && obj(t)->tag == 1) { s += float_val(obj(t)->slots[0]); n++; t = obj(t)->slots[1]; } double m = n > 0 ? s / n : 0; double v = 0; Value u = xs; while (!IS_INT(u) && obj(u)->kind == K_CON && obj(u)->tag == 1) { double d = float_val(obj(u)->slots[0]) - m; v += d * d; u = obj(u)->slots[1]; } rt_release(xs); return mkfloat(n > 0 ? v / n : 0); }\n")
		b.WriteString("Value npVar(void) { return rt_mkclo(&npVar_c, 0); }\n")
	}
	// D4 interop: npMax — fold-max hand body (no BLAS); py serves real numpy.max.
	if usesForeign(p, "npMax") {
		b.WriteString("static Value npMax_c(Value xs, Value* env) { (void)env; Value h = xs; double m = 0; int first = 1; while (!IS_INT(xs) && obj(xs)->kind == K_CON && obj(xs)->tag == 1) { double x = float_val(obj(xs)->slots[0]); if (first || x > m) { m = x; first = 0; } xs = obj(xs)->slots[1]; } rt_release(h); return mkfloat(m); }\n")
		b.WriteString("Value npMax(void) { return rt_mkclo(&npMax_c, 0); }\n")
	}
	// D4 interop: npNorm — sqrt(sum of squares) hand body (math.h, -lm); py serves numpy.linalg.norm.
	if usesForeign(p, "npNorm") {
		if !usesForeign(p, "fsqrt") {
			b.WriteString("#include <math.h>\n")
		}
		b.WriteString("static Value npNorm_c(Value xs, Value* env) { (void)env; Value h = xs; double s = 0; while (!IS_INT(xs) && obj(xs)->kind == K_CON && obj(xs)->tag == 1) { double x = float_val(obj(xs)->slots[0]); s += x * x; xs = obj(xs)->slots[1]; } rt_release(h); return mkfloat(sqrt(s)); }\n")
		b.WriteString("Value npNorm(void) { return rt_mkclo(&npNorm_c, 0); }\n")
	}
	// MATRIX BLAS: gemmSum m k n A B — cblas_dgemm over two flat row-major FLists,
	// returning the SUM of all product entries (scalar observable; no host→Rune matrix).
	if usesForeign(p, "gemmSum") {
		b.WriteString("static Value gemmSum_c5(Value Bm, Value* env) { int m = (int)big_to_double(env[0]), k = (int)big_to_double(env[1]), n = (int)big_to_double(env[2]); Value A = env[3]; int al = m*k, bl = k*n, cl = m*n; double* AA = (double*)malloc(sizeof(double)*(al>0?al:1)); double* BB = (double*)malloc(sizeof(double)*(bl>0?bl:1)); double* CC = (double*)malloc(sizeof(double)*(cl>0?cl:1)); fl_fill(A, AA); fl_fill(Bm, BB); cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, m, n, k, 1.0, AA, k, BB, n, 0.0, CC, n); double s = 0; for (int i = 0; i < cl; i++) s += CC[i]; free(AA); free(BB); free(CC); rt_release(Bm); return mkfloat(s); }\n")
		b.WriteString("static Value gemmSum_c4(Value A, Value* env) { Value c = rt_mkclo(&gemmSum_c5, 4); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_retain(env[2]); rt_clo_set(c, 2, env[2]); rt_clo_set(c, 3, A); return c; }\n")
		b.WriteString("static Value gemmSum_c3(Value n, Value* env) { Value c = rt_mkclo(&gemmSum_c4, 3); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_clo_set(c, 2, n); return c; }\n")
		b.WriteString("static Value gemmSum_c2(Value k, Value* env) { Value c = rt_mkclo(&gemmSum_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, k); return c; }\n")
		b.WriteString("static Value gemmSum_c1(Value m, Value* env) { (void)env; Value c = rt_mkclo(&gemmSum_c2, 1); rt_clo_set(c, 0, m); return c; }\n")
		b.WriteString("Value gemmSum(void) { return rt_mkclo(&gemmSum_c1, 0); }\n")
	}
	// D4 interop: npMatSum on the native backends is the OpenBLAS gift (cblas_dgemm).
	if usesForeign(p, "npMatSum") {
		b.WriteString("static Value npMatSum_c5(Value Bm, Value* env) { int m = (int)big_to_double(env[0]), k = (int)big_to_double(env[1]), n = (int)big_to_double(env[2]); Value A = env[3]; int al = m*k, bl = k*n, cl = m*n; double* AA = (double*)malloc(sizeof(double)*(al>0?al:1)); double* BB = (double*)malloc(sizeof(double)*(bl>0?bl:1)); double* CC = (double*)malloc(sizeof(double)*(cl>0?cl:1)); fl_fill(A, AA); fl_fill(Bm, BB); cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, m, n, k, 1.0, AA, k, BB, n, 0.0, CC, n); double s = 0; for (int i = 0; i < cl; i++) s += CC[i]; free(AA); free(BB); free(CC); rt_release(Bm); return mkfloat(s); }\n")
		b.WriteString("static Value npMatSum_c4(Value A, Value* env) { Value c = rt_mkclo(&npMatSum_c5, 4); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_retain(env[2]); rt_clo_set(c, 2, env[2]); rt_clo_set(c, 3, A); return c; }\n")
		b.WriteString("static Value npMatSum_c3(Value n, Value* env) { Value c = rt_mkclo(&npMatSum_c4, 3); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_clo_set(c, 2, n); return c; }\n")
		b.WriteString("static Value npMatSum_c2(Value k, Value* env) { Value c = rt_mkclo(&npMatSum_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, k); return c; }\n")
		b.WriteString("static Value npMatSum_c1(Value m, Value* env) { (void)env; Value c = rt_mkclo(&npMatSum_c2, 1); rt_clo_set(c, 0, m); return c; }\n")
		b.WriteString("Value npMatSum(void) { return rt_mkclo(&npMatSum_c1, 0); }\n")
	}
	// D4 / R-INTEROP: the CPython EMBED on the LLVM native backend too — the scalar prims
	// (pow / sqrt / factorial), mirroring codegen/c.go but with the LLVM runtime's rt_-prefixed
	// closure/bignum helpers and EXTERNAL-linkage thunks the .ll calls. Cross-backend parity:
	// the embed runs on BOTH native backends (C and LLVM). Link: python3-config --embed.
	if usesForeign(p, "pyPow") || usesForeign(p, "pySqrt") || usesForeign(p, "pyFactorial") || usesForeign(p, "pyNpSum") || usesForeign(p, "pyNpScale") || usesForeign(p, "pyNpMatVec") {
		b.WriteString("#include <Python.h>\n")
		b.WriteString("static int rune_py_inited = 0;\n")
		b.WriteString("static void rune_py_ensure(void) { if (!rune_py_inited) { Py_Initialize(); rune_py_inited = 1; } }\n")
	}
	if usesForeign(p, "pyPow") {
		b.WriteString("static Value pyPow_c2(Value bb, Value* env) { Value aa = env[0]; long a = IS_INT(aa) ? INT_VAL(aa) : (long)big_to_double(aa); long b = IS_INT(bb) ? INT_VAL(bb) : (long)big_to_double(bb); rune_py_ensure(); char buf[80]; snprintf(buf, sizeof buf, \"pow(%ld,%ld)\", a, b); PyObject* m = PyImport_AddModule(\"__main__\"); PyObject* g = PyModule_GetDict(m); PyObject* r = PyRun_String(buf, Py_eval_input, g, g); long res = 0; if (r) { res = PyLong_AsLong(r); Py_XDECREF(r); } else { PyErr_Clear(); } rt_release(bb); return rt_big_from_long(res); }\n")
		b.WriteString("static Value pyPow_c1(Value a, Value* env) { (void)env; Value c = rt_mkclo(&pyPow_c2, 1); rt_clo_set(c, 0, a); return c; }\n")
		b.WriteString("Value pyPow(void) { return rt_mkclo(&pyPow_c1, 0); }\n")
	}
	if usesForeign(p, "pySqrt") {
		b.WriteString("static Value pySqrt_c(Value aa, Value* env) { (void)env; long a = IS_INT(aa) ? INT_VAL(aa) : (long)big_to_double(aa); rune_py_ensure(); char buf[80]; snprintf(buf, sizeof buf, \"__import__('math').sqrt(%ld)\", a); PyObject* m = PyImport_AddModule(\"__main__\"); PyObject* g = PyModule_GetDict(m); PyObject* r = PyRun_String(buf, Py_eval_input, g, g); double d = 0.0; if (r) { d = PyFloat_AsDouble(r); Py_XDECREF(r); } else { PyErr_Clear(); } rt_release(aa); return mkfloat(d); }\n")
		b.WriteString("Value pySqrt(void) { return rt_mkclo(&pySqrt_c, 0); }\n")
	}
	if usesForeign(p, "pyFactorial") {
		b.WriteString("static void big_to_decstr(Value v, char* out, int outsz) { int n = big_nlimbs(v); if (n == 0) { snprintf(out, outsz, \"0\"); return; } int off = snprintf(out, outsz, \"%ld\", big_limb(v, n - 1)); for (int i = n - 2; i >= 0 && off < outsz; i--) off += snprintf(out + off, outsz - off, \"%09ld\", big_limb(v, i)); }\n")
		b.WriteString("static Value big_from_decstr(const char* s) { Value n = mkbig(0); Value ten = rt_big_from_long(10); for (; *s; s++) { if (*s < '0' || *s > '9') continue; Value nm = big_mul(n, ten); Value d = rt_big_from_long(*s - '0'); Value nn = big_add(nm, d); rt_release(nm); rt_release(d); rt_release(n); n = nn; } rt_release(ten); return n; }\n")
		b.WriteString("static Value pyFactorial_c(Value aa, Value* env) { (void)env; char inbuf[512]; if (IS_INT(aa)) snprintf(inbuf, sizeof inbuf, \"%ld\", INT_VAL(aa)); else big_to_decstr(aa, inbuf, sizeof inbuf); rune_py_ensure(); char buf[600]; snprintf(buf, sizeof buf, \"str(__import__('math').factorial(%s))\", inbuf); PyObject* m = PyImport_AddModule(\"__main__\"); PyObject* g = PyModule_GetDict(m); PyObject* r = PyRun_String(buf, Py_eval_input, g, g); Value res = rt_big_from_long(0); if (r) { const char* s = PyUnicode_AsUTF8(r); if (s) { rt_release(res); res = big_from_decstr(s); } Py_XDECREF(r); } else { PyErr_Clear(); } rt_release(aa); return res; }\n")
		b.WriteString("Value pyFactorial(void) { return rt_mkclo(&pyFactorial_c, 0); }\n")
	}
	// the numpy-embed prims on LLVM (mirror codegen/c.go with rt_-prefixed mkclo/clo_set/mkcon/
	// con_set/big_from_long; fl_fill/fl_len/mkfloat/big_to_double are the runtime's plain names).
	if usesForeign(p, "pyNpSum") {
		b.WriteString("static Value pyNpSum_c(Value xs, Value* env) { (void)env; int n = fl_len(xs); double* X = (double*)malloc(sizeof(double) * (n > 0 ? n : 1)); fl_fill(xs, X); rune_py_ensure(); PyObject* lst = PyList_New(n); for (int i = 0; i < n; i++) PyList_SetItem(lst, i, PyFloat_FromDouble(X[i])); free(X); PyObject* m = PyImport_AddModule(\"__main__\"); PyObject* g = PyModule_GetDict(m); PyObject* np = PyImport_ImportModule(\"numpy\"); double d = 0.0; if (np) { PyDict_SetItemString(g, \"np\", np); PyDict_SetItemString(g, \"xs\", lst); PyObject* r = PyRun_String(\"float(np.array(xs).sum())\", Py_eval_input, g, g); if (r) { d = PyFloat_AsDouble(r); Py_XDECREF(r); } else { PyErr_Clear(); } Py_XDECREF(np); } else { PyErr_Clear(); } Py_XDECREF(lst); rt_release(xs); return mkfloat(d); }\n")
		b.WriteString("Value pyNpSum(void) { return rt_mkclo(&pyNpSum_c, 0); }\n")
	}
	if usesForeign(p, "pyNpScale") {
		b.WriteString("static Value pyNpScale_c2(Value kk, Value* env) { Value xs = env[0]; long k = IS_INT(kk) ? INT_VAL(kk) : (long)big_to_double(kk); int n = fl_len(xs); double* X = (double*)malloc(sizeof(double) * (n > 0 ? n : 1)); fl_fill(xs, X); rune_py_ensure(); PyObject* lst = PyList_New(n); for (int i = 0; i < n; i++) PyList_SetItem(lst, i, PyFloat_FromDouble(X[i])); free(X); PyObject* m = PyImport_AddModule(\"__main__\"); PyObject* g = PyModule_GetDict(m); PyObject* np = PyImport_ImportModule(\"numpy\"); Value res = rt_mkcon(0, \"fnil\", 0); if (np) { PyDict_SetItemString(g, \"np\", np); PyDict_SetItemString(g, \"xs\", lst); char buf[80]; snprintf(buf, sizeof buf, \"(np.array(xs) * %ld).tolist()\", k); PyObject* r = PyRun_String(buf, Py_eval_input, g, g); if (r) { int rn = (int)PyList_Size(r); Value acc = rt_mkcon(0, \"fnil\", 0); for (int i = rn - 1; i >= 0; i--) { double d = PyFloat_AsDouble(PyList_GetItem(r, i)); Value c = rt_mkcon(1, \"fcons\", 2); rt_con_set(c, 0, mkfloat(d)); rt_con_set(c, 1, acc); acc = c; } rt_release(res); res = acc; Py_XDECREF(r); } else { PyErr_Clear(); } Py_XDECREF(np); } else { PyErr_Clear(); } Py_XDECREF(lst); rt_release(kk); return res; }\n")
		b.WriteString("static Value pyNpScale_c1(Value xs, Value* env) { (void)env; Value c = rt_mkclo(&pyNpScale_c2, 1); rt_clo_set(c, 0, xs); return c; }\n")
		b.WriteString("Value pyNpScale(void) { return rt_mkclo(&pyNpScale_c1, 0); }\n")
	}
	if usesForeign(p, "pyNpMatVec") {
		b.WriteString("static Value pyNpMatVec_c4(Value v, Value* env) { Value A = env[0]; long m = IS_INT(env[1]) ? INT_VAL(env[1]) : (long)big_to_double(env[1]); long k = IS_INT(env[2]) ? INT_VAL(env[2]) : (long)big_to_double(env[2]); int an = fl_len(A), vn = fl_len(v); double* AA = (double*)malloc(sizeof(double) * (an > 0 ? an : 1)); fl_fill(A, AA); double* VV = (double*)malloc(sizeof(double) * (vn > 0 ? vn : 1)); fl_fill(v, VV); rune_py_ensure(); PyObject* pa = PyList_New(an); for (int i = 0; i < an; i++) PyList_SetItem(pa, i, PyFloat_FromDouble(AA[i])); PyObject* pv = PyList_New(vn); for (int i = 0; i < vn; i++) PyList_SetItem(pv, i, PyFloat_FromDouble(VV[i])); free(AA); free(VV); PyObject* mm = PyImport_AddModule(\"__main__\"); PyObject* g = PyModule_GetDict(mm); PyObject* np = PyImport_ImportModule(\"numpy\"); Value res = rt_mkcon(0, \"fnil\", 0); if (np) { PyDict_SetItemString(g, \"np\", np); PyDict_SetItemString(g, \"A\", pa); PyDict_SetItemString(g, \"v\", pv); char buf[120]; snprintf(buf, sizeof buf, \"(np.array(A).reshape(%ld,%ld) @ np.array(v)).tolist()\", m, k); PyObject* r = PyRun_String(buf, Py_eval_input, g, g); if (r) { int rn = (int)PyList_Size(r); Value acc = rt_mkcon(0, \"fnil\", 0); for (int i = rn - 1; i >= 0; i--) { double d = PyFloat_AsDouble(PyList_GetItem(r, i)); Value c = rt_mkcon(1, \"fcons\", 2); rt_con_set(c, 0, mkfloat(d)); rt_con_set(c, 1, acc); acc = c; } rt_release(res); res = acc; Py_XDECREF(r); } else { PyErr_Clear(); } Py_XDECREF(np); } else { PyErr_Clear(); } Py_XDECREF(pa); Py_XDECREF(pv); rt_release(v); return res; }\n")
		b.WriteString("static Value pyNpMatVec_c3(Value k, Value* env) { Value c = rt_mkclo(&pyNpMatVec_c4, 3); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_retain(env[1]); rt_clo_set(c, 1, env[1]); rt_clo_set(c, 2, k); return c; }\n")
		b.WriteString("static Value pyNpMatVec_c2(Value m, Value* env) { Value c = rt_mkclo(&pyNpMatVec_c3, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, m); return c; }\n")
		b.WriteString("static Value pyNpMatVec_c1(Value A, Value* env) { (void)env; Value c = rt_mkclo(&pyNpMatVec_c2, 1); rt_clo_set(c, 0, A); return c; }\n")
		b.WriteString("Value pyNpMatVec(void) { return rt_mkclo(&pyNpMatVec_c1, 0); }\n")
	}
}

// llEmitter renders CIr terms to LLVM IR. Like cEmitter it knows the nat
// eliminator's name + the accel table for native arithmetic. It also accumulates
// the string-constant globals (constructor names + string literals) so they can be
// flushed to module scope before the functions that reference them.
type llEmitter struct {
	natElim    string
	accel      map[string]core.NatOp
	blocks     map[string]CodeBlock // code-block lookup for the IH-ignoring nat dispatch
	ctorByName map[string]CtorSpec  // non-nat constructors, for satCtorDispatch
	strs       map[string]string    // string content -> its global symbol (@.str.N)
	strOrder   []string             // first-seen order, for deterministic output
}

// intern returns (and registers, on first sight) the module-scope symbol for a
// string constant. The symbol embeds a counter so distinct strings never collide;
// identical strings share one global.
func (em *llEmitter) intern(s string) string {
	if em.strs == nil {
		em.strs = map[string]string{}
	}
	if sym, ok := em.strs[s]; ok {
		return sym
	}
	sym := fmt.Sprintf("@.str.%d", len(em.strOrder))
	em.strs[s] = sym
	em.strOrder = append(em.strOrder, s)
	return sym
}

// llFunc tracks per-function SSA state: a fresh-register counter and a fresh-label
// counter. LLVM IR is SSA + statement-oriented (unlike the C emitter's expression
// form), so each CIr node lowers to a sequence of instructions written to the
// builder and returns the NAME of the register holding its i64 value.
type llFunc struct {
	em    *llEmitter
	reg   int
	label int
}

func (f *llFunc) fresh() string { f.reg++; return fmt.Sprintf("%%v%d", f.reg) }
func (f *llFunc) freshLabel(s string) string {
	f.label++
	return fmt.Sprintf("%s%d", s, f.label)
}

// emitDefThunk emits a memoized thunk for a top-level definition. The first call
// computes the body and caches it (a private global cache + done flag), so
// recursion + the eliminators' self-reference terminate and run once. Under ARC the
// cache holds one never-released reference for the process lifetime — that IS the
// root (no root registration needed), exactly as the C backend does.
func (em *llEmitter) emitDefThunk(b *strings.Builder, name string, body CIr) {
	cache := "@cache_" + llName(name)
	done := "@done_" + llName(name)
	fmt.Fprintf(b, "%s = internal global i64 0\n", cache)
	fmt.Fprintf(b, "%s = internal global i1 0\n", done)
	fmt.Fprintf(b, "define i64 @%s() {\nentry:\n", llThunkName(name))
	f := &llFunc{em: em}
	d := f.fresh()
	fmt.Fprintf(b, "  %s = load i1, i1* %s\n", d, done)
	lret := f.freshLabel("ret")
	lcomp := f.freshLabel("comp")
	fmt.Fprintf(b, "  br i1 %s, label %%%s, label %%%s\n", d, lret, lcomp)
	fmt.Fprintf(b, "%s:\n", lret)
	c0 := f.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", c0, cache)
	fmt.Fprintf(b, "  ret i64 %s\n", c0)
	fmt.Fprintf(b, "%s:\n", lcomp)
	fmt.Fprintf(b, "  store i1 1, i1* %s\n", done)
	v := f.emit(b, body, nil)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", v, cache)
	fmt.Fprintf(b, "  ret i64 %s\n", v)
	b.WriteString("}\n")
}

// emitBlock emits one lifted code block as an LLVM function. The body refers to
// its argument as CVar{0} (the `%arg` parameter, shifted by inner CLet binders) and
// to captured variables via CEnv{slot} (a GEP+load of `%env`). Locals are tracked
// as a stack of register names (innermost = index 0); the seed is `%arg`.
func (em *llEmitter) emitBlock(b *strings.Builder, blk CodeBlock) {
	fmt.Fprintf(b, "define i64 @%s(i64 %%arg, i64* %%env) {\nentry:\n", llCodeName(blk.Name))
	f := &llFunc{em: em}
	v := f.emitIn(b, blk.Body, []string{"%arg"})
	fmt.Fprintf(b, "  ret i64 %s\n", v)
	b.WriteString("}\n")
}

// emitCtorLL emits a constructor as a curried closure, mirroring emitCtorC: a
// nullary constructor is the {tag} object; an arity-k constructor builds k code
// blocks, each capturing the prefix and the last building the {tag,fields} object.
func (em *llEmitter) emitCtorLL(b *strings.Builder, c CtorSpec) {
	if c.Arity == 0 {
		em.emitCachedThunk(b, c.Name, func(f *llFunc) string {
			r := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @rt_mkcon(i32 %d, i8* %s, i32 0)\n", r, c.Tag, em.cStr(c.Name))
			return r
		})
		return
	}
	base := "ctorfn_" + llName(c.Name)
	for i := 0; i < c.Arity; i++ {
		fmt.Fprintf(b, "define i64 @%s_%d(i64 %%arg, i64* %%env) {\nentry:\n", base, i)
		f := &llFunc{em: em}
		if i == c.Arity-1 {
			o := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @rt_mkcon(i32 %d, i8* %s, i32 %d)\n", o, c.Tag, em.cStr(c.Name), c.Arity)
			for k := 0; k < i; k++ {
				ev := f.loadEnv(b, k)
				fmt.Fprintf(b, "  call void @rt_con_set(i64 %s, i32 %d, i64 %s)\n", o, k, ev)
			}
			fmt.Fprintf(b, "  call void @rt_con_set(i64 %s, i32 %d, i64 %%arg)\n", o, i)
			fmt.Fprintf(b, "  ret i64 %s\n}\n", o)
		} else {
			cl := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s_%d, i32 %d)\n", cl, base, i+1, i+1)
			for k := 0; k < i; k++ {
				ev := f.loadEnv(b, k)
				fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %s)\n", cl, k, ev)
			}
			fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %%arg)\n", cl, i)
			fmt.Fprintf(b, "  ret i64 %s\n}\n", cl)
		}
	}
	em.emitCachedThunk(b, c.Name, func(f *llFunc) string {
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s_0, i32 0)\n", r, base)
		return r
	})
}

// emitCachedThunk emits the standard memoized-thunk wrapper around a body that
// produces a Value (a constructor object or its entry closure). Shared by the
// constructor and nat thunks.
func (em *llEmitter) emitCachedThunk(b *strings.Builder, name string, body func(*llFunc) string) {
	cache := "@cache_" + llName(name)
	done := "@done_" + llName(name)
	fmt.Fprintf(b, "%s = internal global i64 0\n", cache)
	fmt.Fprintf(b, "%s = internal global i1 0\n", done)
	fmt.Fprintf(b, "define i64 @%s() {\nentry:\n", llThunkName(name))
	f := &llFunc{em: em}
	d := f.fresh()
	fmt.Fprintf(b, "  %s = load i1, i1* %s\n", d, done)
	lret := f.freshLabel("ret")
	lcomp := f.freshLabel("comp")
	fmt.Fprintf(b, "  br i1 %s, label %%%s, label %%%s\n", d, lret, lcomp)
	fmt.Fprintf(b, "%s:\n", lret)
	c0 := f.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", c0, cache)
	fmt.Fprintf(b, "  ret i64 %s\n", c0)
	fmt.Fprintf(b, "%s:\n", lcomp)
	fmt.Fprintf(b, "  store i1 1, i1* %s\n", done)
	v := body(f)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", v, cache)
	fmt.Fprintf(b, "  ret i64 %s\n}\n", v)
}

// emitNatLL emits the builtin-nat group natively, mirroring emitNatC: zero is the
// immediate 0, succ a +1 closure, the eliminator a 4-block curried fold loop.
func (em *llEmitter) emitNatLL(b *strings.Builder, n NatSpec) {
	// zero: the empty bignum
	em.emitCachedThunk(b, n.Zero, func(f *llFunc) string {
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_big_from_long(i64 0)\n", r)
		return r
	})
	// succ code: x -> x+1 (bignum increment). rt_big_succ BORROW-reads $arg and returns
	// a fresh K_BIG; under PATH B the caller hands the closure an OWNED arg (Perceus dups
	// a borrowed operand at the owning position), so succ must RELEASE its arg like any
	// user closure code block -- else a succ-chain leaks every intermediate K_BIG. The LL
	// dual of emitNatC's succ_code release (c.go:379) and wasm.go's succ_code.
	sc := llName(n.Succ) + "_code"
	fmt.Fprintf(b, "define i64 @%s(i64 %%arg, i64* %%env) {\nentry:\n", sc)
	f := &llFunc{em: em}
	r := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_big_succ(i64 %%arg)\n", r)
	fmt.Fprintf(b, "  call void @rt_release(i64 %%arg)\n")
	fmt.Fprintf(b, "  ret i64 %s\n}\n", r)
	em.emitCachedThunk(b, n.Succ, func(f *llFunc) string {
		rr := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s, i32 0)\n", rr, sc)
		return rr
	})
	// eliminator: m -> c0 -> c1 -> x -> fold (m erased/ignored). env order {m,c0,c1}.
	base := llName(n.ElimName)
	// b3: the fold. env = {m, c0, c1}, arg = x.
	fmt.Fprintf(b, "define i64 @%s_b3(i64 %%arg, i64* %%env) {\nentry:\n", base)
	ff := &llFunc{em: em}
	// arg is the scrutinee x, a BIGNUM (BORROWED -- env {m, c0, c1} borrowed too); the
	// counter k is a bignum, compared against x via rt_big_cmp and stepped by
	// rt_big_succ. acc + k live in allocas across the allocating apply/succ calls. The
	// ARC protocol is the LL dual of emitNatC's b3 (c.go:398-410, ported from WASM's
	// emitNatFold): $acc starts as a BORROWED alias of env[1] (c0) so RETAIN it (the
	// loop owns its own reference; a 0-iteration fold returns acc directly). env[2] (c1,
	// the step) and $arg (x) stay BORROWED. Each iteration RETAINS k before the step
	// apply (the step consumes its own copy), RELEASES the fresh $step closure and the
	// old k, and the final k after the loop. (x's one release is deferred to
	// satElimDispatch, matching c.go -- a constant per-fold, not per-iteration.)
	c0 := ff.loadEnv(b, 1)
	c1 := ff.loadEnv(b, 2)
	fmt.Fprintf(b, "  call void @rt_retain(i64 %s)\n", c0)
	accSlot := ff.fresh()
	fmt.Fprintf(b, "  %s = alloca i64\n", accSlot)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", c0, accSlot)
	kSlot := ff.fresh()
	fmt.Fprintf(b, "  %s = alloca i64\n", kSlot)
	k0 := ff.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_big_from_long(i64 0)\n", k0)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", k0, kSlot)
	lcond := ff.freshLabel("cond")
	lbody := ff.freshLabel("body")
	ldone := ff.freshLabel("done")
	fmt.Fprintf(b, "  br label %%%s\n", lcond)
	fmt.Fprintf(b, "%s:\n", lcond)
	kv := ff.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", kv, kSlot)
	cmpv := ff.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_big_cmp(i64 %s, i64 %%arg)\n", cmpv, kv)
	cmp := ff.fresh()
	fmt.Fprintf(b, "  %s = icmp slt i64 %s, 0\n", cmp, cmpv)
	fmt.Fprintf(b, "  br i1 %s, label %%%s, label %%%s\n", cmp, lbody, ldone)
	fmt.Fprintf(b, "%s:\n", lbody)
	fmt.Fprintf(b, "  call void @rt_retain(i64 %s)\n", kv)
	step := ff.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", step, c1, kv)
	accv := ff.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", accv, accSlot)
	nacc := ff.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", nacc, step, accv)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", nacc, accSlot)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", step)
	k1 := ff.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_big_succ(i64 %s)\n", k1, kv)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", kv)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", k1, kSlot)
	fmt.Fprintf(b, "  br label %%%s\n", lcond)
	fmt.Fprintf(b, "%s:\n", ldone)
	kfin := ff.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", kfin, kSlot)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", kfin)
	res := ff.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", res, accSlot)
	fmt.Fprintf(b, "  ret i64 %s\n}\n", res)
	// b2: arg=c1, env={m,c0}; build b3 closure capturing {m,c0,c1}.
	em.emitCurryBlock(b, base+"_b2", base+"_b3", 3, []int{0, 1}, 2)
	// b1: arg=c0, env={m}; build b2 closure capturing {m,c0}.
	em.emitCurryBlock(b, base+"_b1", base+"_b2", 2, []int{0}, 1)
	// b0: arg=m, env={}; build b1 closure capturing {m}.
	em.emitCurryBlock(b, base+"_b0", base+"_b1", 1, []int{}, 0)
	em.emitCachedThunk(b, n.ElimName, func(f *llFunc) string {
		rr := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s_b0, i32 0)\n", rr, base)
		return rr
	})
	// One-peel CASE form for an IH-ignoring eliminator (see StepIgnoresIH), the LL
	// dual of c.go's `<base>_case`: zero -> c0, succ k -> c1 k unit. Emitted instead
	// of the b3 fold when the step is provably IH-independent, turning the
	// super-exponential `beqNat`-shape into linear.
	// ARC ownership (LL dual of c.go's `_case`, c.go:430-436): c0/c1/x arrive OWNED
	// (natDispatch fires only on a recognized NatElimSpine, so annotateBareSpine
	// consumeOwning'd each leaf). This helper consumes each exactly once. The zero
	// comparand `z` is an owned temp we discard -- released after the compare. zero:
	// return c0 (transferred out), release the dead c1/x. succ: apply c1 to (x-1) then
	// unit, releasing the intermediate step closure + the dead c0 + the borrow-read x;
	// the internal `one` temp is freed (rt_nat_monus BORROW-reads both operands).
	fmt.Fprintf(b, "define i64 @%s_case(i64 %%c0, i64 %%c1, i64 %%x) {\nentry:\n", base)
	b.WriteString("  %z = call i64 @rt_big_from_long(i64 0)\n")
	b.WriteString("  %c = call i64 @rt_big_cmp(i64 %x, i64 %z)\n")
	b.WriteString("  call void @rt_release(i64 %z)\n")
	b.WriteString("  %iz = icmp eq i64 %c, 0\n")
	b.WriteString("  br i1 %iz, label %zero, label %succ\n")
	b.WriteString("zero:\n")
	b.WriteString("  call void @rt_release(i64 %c1)\n")
	b.WriteString("  call void @rt_release(i64 %x)\n")
	b.WriteString("  ret i64 %c0\n")
	b.WriteString("succ:\n")
	b.WriteString("  %one = call i64 @rt_big_from_long(i64 1)\n")
	b.WriteString("  %p = call i64 @rt_nat_monus(i64 %x, i64 %one)\n")
	b.WriteString("  call void @rt_release(i64 %one)\n")
	b.WriteString("  %s = call i64 @rt_apply(i64 %c1, i64 %p)\n")
	b.WriteString("  %u = load i64, i64* @UNIT\n")
	b.WriteString("  %r = call i64 @rt_apply(i64 %s, i64 %u)\n")
	b.WriteString("  call void @rt_release(i64 %s)\n")
	b.WriteString("  call void @rt_release(i64 %c1)\n")
	b.WriteString("  call void @rt_release(i64 %c0)\n")
	b.WriteString("  call void @rt_release(i64 %x)\n")
	b.WriteString("  ret i64 %r\n}\n")
}

// natDispatch emits the constant-time one-peel CASE for a saturated `NatElim m c0
// c1 x [extra...]` whose step ignores its IH, the LL dual of the c.go optimization
// (and the source backends' $natD). Without it the native b3 fold is
// super-exponential on the decidable-equality shape.
func (f *llFunc) natDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	args, ok := NatElimSpine(f.em.natElim, app)
	if !ok || !StepIgnoresIH(f.em.blocks, args[2]) {
		return "", false
	}
	// Evaluate the four spine operands in source order (motive, c0, c1, x). The MOTIVE
	// (args[0]) is erased -- `_case` never reads it -- but annotateBareSpine
	// consumeOwning'd every leaf of the recognized nat-elim spine, so it arrives OWNED:
	// evaluate it (running its consumeOwning side effects) and RELEASE it, matching
	// c.go's natDispatch (c.go:454-460) and WASM satElimDispatch (wasm.go:1008-1009).
	// c0/c1/x are then consumed by `_case` (each freed exactly once inside it).
	mot := f.emitIn(b, args[0], locals)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", mot)
	c0 := f.emitIn(b, args[1], locals)
	c1 := f.emitIn(b, args[2], locals)
	xv := f.emitIn(b, args[3], locals)
	r := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @%s_case(i64 %s, i64 %s, i64 %s)\n", r, llName(f.em.natElim), c0, c1, xv)
	for _, extra := range args[4:] {
		e := f.emitIn(b, extra, locals)
		nr := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", nr, r, e)
		r = nr
	}
	return r, true
}

// satCtorDispatch recognizes a SATURATED constructor application (an AppClosure spine
// headed by a CGlobal naming a non-nat constructor with EXACTLY Arity args) and emits
// the K_CON DIRECTLY: rt_mkcon(tag, name, arity) + one rt_con_set per arg (each arg
// evaluated once into a stable register, then MOVED into its field slot). The LL dual
// of c.go's satCtorDispatch (c.go:479) and wasm.go's (:937). Perceus keeps a recognized
// ctor spine's backbone BARE (no releases on the Clo chain) on the assumption the
// emitter builds the K_CON directly; without this the generic apply path allocates
// arity-1 intermediate curried K_CLOs per runtime ctor application that NOTHING
// releases -- a per-application leak scaling in any structure-building loop. Field
// order + the tag/name/arity triple match ctorfn_<name>_<k> exactly, so the printed
// K_CON is byte-identical to the curried path. A PARTIAL ctor falls through unchanged.
func (f *llFunc) satCtorDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	var args []CIr
	t := CIr(app)
	for {
		ap, ok := t.(AppClosure)
		if !ok {
			break
		}
		args = append([]CIr{ap.Arg}, args...)
		t = ap.Clo
	}
	g, ok := t.(CGlobal)
	if !ok {
		return "", false
	}
	c, isCtor := f.em.ctorByName[g.Name]
	if !isCtor || len(args) != c.Arity {
		return "", false
	}
	// Evaluate each arg into a stable register FIRST (a nested allocating arg must not
	// interleave with the con_set fills), then build + fill. Args arrive OWNED and are
	// MOVED into the field slots.
	vals := make([]string, len(args))
	for i, a := range args {
		vals[i] = f.emitIn(b, a, locals)
	}
	o := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_mkcon(i32 %d, i8* %s, i32 %d)\n", o, c.Tag, f.em.cStr(c.Name), c.Arity)
	for i, v := range vals {
		fmt.Fprintf(b, "  call void @rt_con_set(i64 %s, i32 %d, i64 %s)\n", o, i, v)
	}
	return o, true
}

// satElimDispatch recognizes a SATURATED builtin-nat eliminator application whose step
// USES its IH (natDispatch already handled the IH-ignoring one-peel) and runs the b3
// fold DIRECTLY: build ONLY the b3 closure (env {unit, c0, c1}) and apply it to x,
// instead of currying through the cached eliminator thunk's b0->b1->b2 chain. The LL
// dual of c.go's satElimDispatch (c.go:533) and wasm.go's (:998). Perceus keeps a
// recognized nat-elim spine BARE, so the generic path's three intermediate
// partial-application K_CLOs were never released -- a per-evaluation leak that scaled
// when the fold sat inside an outer loop. The erased motive is evaluated (for its
// consumeOwning side effects) and released -- the fold never reads env[0], which gets
// UNIT (uncounted). c0/c1 are MOVED into the b3 env (freed when the b3 closure is
// released after the call; b3 retains its acc so the base survives). x is borrow-read
// by the fold and released HERE afterward. All four spine args arrive OWNED.
func (f *llFunc) satElimDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	if f.em.natElim == "" {
		return "", false
	}
	args, ok := NatElimSpine(f.em.natElim, app)
	if !ok || len(args) != 4 {
		return "", false
	}
	base := llName(f.em.natElim)
	mot := f.emitIn(b, args[0], locals)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", mot)
	c0 := f.emitIn(b, args[1], locals)
	c1 := f.emitIn(b, args[2], locals)
	xv := f.emitIn(b, args[3], locals)
	u := f.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* @UNIT\n", u)
	cl := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s_b3, i32 3)\n", cl, base)
	fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 0, i64 %s)\n", cl, u)
	fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 1, i64 %s)\n", cl, c0)
	fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 2, i64 %s)\n", cl, c1)
	r := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", r, cl, xv)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", cl)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", xv)
	return r, true
}

// emitCurryBlock emits one currying block of a primitive: it builds the next
// closure of `nslots` slots over code `@next`, copying env slots `copyEnv` (by env
// index, into the same target slots) then placing %arg at slot `argSlot`.
func (em *llEmitter) emitCurryBlock(b *strings.Builder, name, next string, nslots int, copyEnv []int, argSlot int) {
	fmt.Fprintf(b, "define i64 @%s(i64 %%arg, i64* %%env) {\nentry:\n", name)
	f := &llFunc{em: em}
	cl := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s, i32 %d)\n", cl, next, nslots)
	for _, k := range copyEnv {
		ev := f.loadEnv(b, k)
		fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %s)\n", cl, k, ev)
	}
	fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %%arg)\n", cl, argSlot)
	fmt.Fprintf(b, "  ret i64 %s\n}\n", cl)
}

// loadEnv emits a GEP+load reading env slot i, returning the register name.
func (f *llFunc) loadEnv(b *strings.Builder, i int) string {
	p := f.fresh()
	fmt.Fprintf(b, "  %s = getelementptr i64, i64* %%env, i64 %d\n", p, i)
	v := f.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", v, p)
	return v
}

// emit lowers a CIr in the empty-locals context (def-thunk top level).
func (f *llFunc) emit(b *strings.Builder, t CIr, locals []string) string {
	return f.emitIn(b, t, locals)
}

// emitIn lowers a CIr in a context of `locals` (register names of binders visible
// by de Bruijn index, innermost = 0), writing instructions to b and returning the
// register holding the i64 result. CEnv reads the enclosing block's %env.
func (f *llFunc) emitIn(b *strings.Builder, t CIr, locals []string) string {
	switch x := t.(type) {
	case CVar:
		if x.Idx < len(locals) {
			return locals[x.Idx]
		}
		// Closed program invariant: should not happen; emit UNIT as a safe marker.
		r := f.fresh()
		fmt.Fprintf(b, "  %s = load i64, i64* @UNIT\n", r)
		return r
	case CEnv:
		return f.loadEnv(b, x.Idx)
	case CGlobal:
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @%s()\n", r, llThunkName(x.Name))
		return r
	case CForeign:
		// Host-linked accessor: the host defines `Value <name>(void)`. Spec Decision 4:
		// a CForeign accessor read is BORROWED (a cached root the pass never releases;
		// callers dup on consumption). The host accessor allocates a fresh closure per
		// call, so an unmemoized read leaks one closure PER READ (scaling inside a fold
		// loop -- the byteLen-per-line leak c.go's per-call-site static fixes). Route the
		// read through a per-name memoizing wrapper thunk (@fc_<name>, emitted once by
		// Emit) -- the LL dual of c.go's `_fc_<name>` block-scope static: one never-
		// released cached root per foreign for the process lifetime.
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @fc_%s()\n", r, llName(x.Name))
		return r
	case CUnit:
		r := f.fresh()
		fmt.Fprintf(b, "  %s = load i64, i64* @UNIT\n", r)
		return r
	case CLit:
		return f.emitLit(b, x)
	case MkClosure:
		return f.emitMkClosure(b, x, locals)
	case AppClosure:
		if out, ok := f.natDispatch(b, x, locals); ok {
			return out
		}
		if out, ok := f.accelDispatch(b, x, locals); ok {
			return out
		}
		if out, ok := f.satCtorDispatch(b, x, locals); ok {
			return out
		}
		if out, ok := f.satElimDispatch(b, x, locals); ok {
			return out
		}
		clo := f.emitIn(b, x.Clo, locals)
		arg := f.emitIn(b, x.Arg, locals)
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", r, clo, arg)
		return r
	case CLet:
		v := f.emitIn(b, x.Val, locals)
		return f.emitIn(b, x.Body, prepend(v, locals))
	case CPair:
		a := f.emitIn(b, x.A, locals)
		bb := f.emitIn(b, x.B, locals)
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_mkpair(i64 %s, i64 %s)\n", r, a, bb)
		return r
	case CFst:
		p := f.emitIn(b, x.P, locals)
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_pair_fst(i64 %s)\n", r, p)
		return r
	case CSnd:
		p := f.emitIn(b, x.P, locals)
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_pair_snd(i64 %s)\n", r, p)
		return r
	case CField:
		s := f.emitIn(b, x.Scrut, locals)
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_con_get(i64 %s, i32 %d)\n", r, s, x.Index)
		return r
	case CCase:
		return f.emitCase(b, x, locals)
	case CBounce:
		return f.emitBounce(b, x.Call, locals)
	case CDup:
		// Retain V (a CVar/CEnv -- a pure NAME, no side effect) then evaluate K. SSA
		// form of the C statement-expression twin (c.go:642) / the WASM emitIn CDup arm
		// (wasm.go:808-812): the retain precedes the continuation's instructions.
		v := f.emitIn(b, x.V, locals)
		fmt.Fprintf(b, "  call void @rt_retain(i64 %s)\n", v)
		return f.emitIn(b, x.K, locals)
	case CDrop:
		// Release V then evaluate K (c.go:647 / wasm.go:813-817). V is a CVar/CEnv the
		// pass proved in scope at this point; emitIn renders it to the visible register.
		v := f.emitIn(b, x.V, locals)
		fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", v)
		return f.emitIn(b, x.K, locals)
	default:
		panic(fmt.Sprintf("codegen(ll): unknown CIr node %T", t))
	}
}

// emitLit lowers a native literal, deferring to the runtime constructors.
func (f *llFunc) emitLit(b *strings.Builder, x CLit) string {
	r := f.fresh()
	switch x.Kind {
	case LitNat:
		// A compressed numeral parses to a bignum from its decimal magnitude
		// (arbitrary precision, interchangeable with a succ-chain since zero/succ
		// share the bignum rep). No i64 cap.
		fmt.Fprintf(b, "  %s = call i64 @rt_big_parse(i8* %s)\n", r, f.em.cStr(x.Nat))
	case LitStr:
		fmt.Fprintf(b, "  %s = call i64 @rt_mkstr(i8* %s)\n", r, f.em.cStr(x.Str))
	case LitPtr:
		fmt.Fprintf(b, "  %s = call i64 @rt_mkptr(i64 %d)\n", r, x.Int)
	default: // LitInt
		fmt.Fprintf(b, "  %s = call i64 @rt_mkint(i64 %d)\n", r, x.Int)
	}
	return r
}

// emitMkClosure allocates a closure and fills its env slots (each evaluated in the
// enclosing context).
func (f *llFunc) emitMkClosure(b *strings.Builder, x MkClosure, locals []string) string {
	cl := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s, i32 %d)\n", cl, llCodeName(x.Code), len(x.Env))
	for i, e := range x.Env {
		ev := f.emitIn(b, e, locals)
		fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %s)\n", cl, i, ev)
	}
	return cl
}

// emitCase lowers an eliminator's tag dispatch to an LLVM `switch` over the
// scrutinee's constructor tag. Each arm computes its body into a result alloca
// (an alloca avoids hand-rolling phi nodes across arms that may themselves branch),
// then branches to a join block that loads the result. Coverage is by construction;
// an unmatched tag aborts via the runtime.
func (f *llFunc) emitCase(b *strings.Builder, x CCase, locals []string) string {
	s := f.emitIn(b, x.Scrut, locals)
	tag := f.fresh()
	fmt.Fprintf(b, "  %s = call i32 @rt_con_tag(i64 %s)\n", tag, s)
	slot := f.fresh()
	fmt.Fprintf(b, "  %s = alloca i64\n", slot)
	ldef := f.freshLabel("casedefault")
	ljoin := f.freshLabel("casejoin")
	armLabels := make([]string, len(x.Arms))
	for i := range x.Arms {
		armLabels[i] = f.freshLabel("casearm")
	}
	fmt.Fprintf(b, "  switch i32 %s, label %%%s [\n", tag, ldef)
	for i, arm := range x.Arms {
		fmt.Fprintf(b, "    i32 %d, label %%%s\n", arm.Tag, armLabels[i])
	}
	b.WriteString("  ]\n")
	for i, arm := range x.Arms {
		fmt.Fprintf(b, "%s:\n", armLabels[i])
		v := f.emitIn(b, arm.Body, locals)
		fmt.Fprintf(b, "  store i64 %s, i64* %s\n", v, slot)
		fmt.Fprintf(b, "  br label %%%s\n", ljoin)
	}
	fmt.Fprintf(b, "%s:\n", ldef)
	b.WriteString("  call void @rt_abort()\n")
	b.WriteString("  unreachable\n")
	fmt.Fprintf(b, "%s:\n", ljoin)
	r := f.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", r, slot)
	return r
}

// emitBounce lowers a CBounce (a marked tail call) to a K_BOUNCE construction: it
// calls the head partial's _step thunk, builds the bounce object, and fills the
// (evaluated) arg slots. Forcing the bounce (in the public driver via rt_tramp)
// advances ONE iteration without nesting a native frame.
func (f *llFunc) emitBounce(b *strings.Builder, call CIr, locals []string) string {
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
		return f.emitIn(b, call, locals) // not a recognizable spine; emit the call directly
	}
	step := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @%s_step()\n", step, llThunkName(g.Name))
	bnc := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @rt_mkbounce(i64 %s, i32 %d)\n", bnc, step, len(args))
	for i, a := range args {
		av := f.emitIn(b, a, locals)
		fmt.Fprintf(b, "  call void @rt_bounce_set(i64 %s, i32 %d, i64 %s)\n", bnc, i+1, av)
	}
	return bnc
}

// emitPartialLL emits a `partial` def's native trampoline split (the LL twin of
// emitPartialC): a memoized _step thunk (the body, where a marked tail call is a
// K_BOUNCE), `arity` curried driver code blocks collecting the args, and the public
// thunk returning the first driver closure. The last driver block saturates _step
// and drives the bounce chain (rt_tramp).
func (em *llEmitter) emitPartialLL(b *strings.Builder, name string, arity int, body CIr) {
	stepSym := llThunkName(name) + "_step"
	em.emitNamedThunk(b, stepSym, llName(name)+"_step", func(f *llFunc) string {
		return f.emit(b, body, nil)
	})
	if arity == 0 {
		// No args to collect: the public entry just drives the (bounce-free) step.
		em.emitNamedThunk(b, llThunkName(name), llName(name), func(f *llFunc) string {
			s := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @%s()\n", s, stepSym)
			r := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @rt_tramp(i64 %s)\n", r, s)
			return r
		})
		return
	}
	drv := func(i int) string { return fmt.Sprintf("drvfn_%s_%d", llName(name), i) }
	for i := 0; i < arity; i++ {
		if i < arity-1 {
			// Collect: build the next driver closure capturing env[0..i-1] + arg. Unlike
			// the ctor/nat curry blocks (recognized-bare spines whose intermediates are
			// never released), a DRIVER intermediate IS released by the generic apply
			// path, so each forwarded env value must be RETAINED into the child (the child
			// takes its own reference; the parent's later release is then balanced). The
			// fresh $arg is owned by this block, so it MOVES. The LL dual of emitPartialC's
			// collect driver (c.go:756-761) and WASM emitPartialDriverBlock (wasm.go:530-534).
			fmt.Fprintf(b, "define i64 @%s(i64 %%arg, i64* %%env) {\nentry:\n", drv(i))
			f := &llFunc{em: em}
			cl := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s, i32 %d)\n", cl, drv(i+1), i+1)
			for k := 0; k < i; k++ {
				ev := f.loadEnv(b, k)
				fmt.Fprintf(b, "  call void @rt_retain(i64 %s)\n", ev)
				fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %s)\n", cl, k, ev)
			}
			fmt.Fprintf(b, "  call void @rt_clo_set(i64 %s, i32 %d, i64 %%arg)\n", cl, i)
			fmt.Fprintf(b, "  ret i64 %s\n}\n", cl)
			continue
		}
		// Last block: saturate _step with env[0..i-1] + arg, then drive once. The driver
		// closure OWNS its captured env (released by the caller after this returns), so
		// each env value is RETAINED before the apply that consumes it (the _step
		// machinery takes its own reference; the driver keeps its own). The fresh $arg is
		// owned by this block, so it MOVES. step() is a BORROWED cached root (not released
		// by apply). Mirrors emitPartialC's last block (c.go:769-773).
		fmt.Fprintf(b, "define i64 @%s(i64 %%arg, i64* %%env) {\nentry:\n", drv(i))
		f := &llFunc{em: em}
		fv := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @%s()\n", fv, stepSym)
		for k := 0; k < i; k++ {
			ev := f.loadEnv(b, k)
			fmt.Fprintf(b, "  call void @rt_retain(i64 %s)\n", ev)
			nf := f.fresh()
			fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %s)\n", nf, fv, ev)
			fv = nf
		}
		nf := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_apply(i64 %s, i64 %%arg)\n", nf, fv)
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_tramp(i64 %s)\n", r, nf)
		fmt.Fprintf(b, "  ret i64 %s\n}\n", r)
	}
	// The public thunk: a closure entering the first driver block (no captures).
	em.emitNamedThunk(b, llThunkName(name), llName(name), func(f *llFunc) string {
		r := f.fresh()
		fmt.Fprintf(b, "  %s = call i64 @rt_mkclo(i64 (i64, i64*)* @%s, i32 0)\n", r, drv(0))
		return r
	})
}

// emitNamedThunk is emitCachedThunk with an explicit thunk symbol + a distinct
// global tag (so a partial's _step and public thunks get separate cache/done
// globals). The body callback emits the value to memoize.
func (em *llEmitter) emitNamedThunk(b *strings.Builder, thunkSym, tag string, body func(*llFunc) string) {
	cache := "@cache_" + tag
	done := "@done_" + tag
	fmt.Fprintf(b, "%s = internal global i64 0\n", cache)
	fmt.Fprintf(b, "%s = internal global i1 0\n", done)
	fmt.Fprintf(b, "define i64 @%s() {\nentry:\n", thunkSym)
	f := &llFunc{em: em}
	d := f.fresh()
	fmt.Fprintf(b, "  %s = load i1, i1* %s\n", d, done)
	lret := f.freshLabel("ret")
	lcomp := f.freshLabel("comp")
	fmt.Fprintf(b, "  br i1 %s, label %%%s, label %%%s\n", d, lret, lcomp)
	fmt.Fprintf(b, "%s:\n", lret)
	c0 := f.fresh()
	fmt.Fprintf(b, "  %s = load i64, i64* %s\n", c0, cache)
	fmt.Fprintf(b, "  ret i64 %s\n", c0)
	fmt.Fprintf(b, "%s:\n", lcomp)
	fmt.Fprintf(b, "  store i1 1, i1* %s\n", done)
	v := body(f)
	fmt.Fprintf(b, "  store i64 %s, i64* %s\n", v, cache)
	fmt.Fprintf(b, "  ret i64 %s\n", v)
	b.WriteString("}\n")
}

// accelDispatch emits a registered accel-op def on two args as native integer
// arithmetic (C7 / R-NUM Decision 4 — the runtime helpers nat_add/nat_mul/
// nat_monus, identical to the C backend's), O(1), interchangeable with the loop.
func (f *llFunc) accelDispatch(b *strings.Builder, app AppClosure, locals []string) (string, bool) {
	op, a, bb, ok := accelMatchC(app, f.em.accel)
	if !ok {
		return "", false
	}
	var fn string
	switch op {
	case core.NatOpAdd:
		fn = "rt_nat_add"
	case core.NatOpMul:
		fn = "rt_nat_mul"
	case core.NatOpMonus:
		fn = "rt_nat_monus"
	case core.NatOpDiv:
		fn = "rt_nat_div"
	case core.NatOpMod:
		fn = "rt_nat_mod"
	default:
		return "", false
	}
	// The accel op BORROW-reads both operands (nat_* read the inputs and return a
	// fresh K_BIG), so both are dead after. Every operand reaching here is a PRIVATE
	// OWNED reference (annotateBareSpine consumeOwning'd each accel leaf; a shared
	// owned local is dup'd once), so the op must RELEASE both -- the LL dual of c.go's
	// accelDispatch (c.go:660-671) and the WASM twin (wasm.go:1069-1070).
	ea := f.emitIn(b, a, locals)
	eb := f.emitIn(b, bb, locals)
	r := f.fresh()
	fmt.Fprintf(b, "  %s = call i64 @%s(i64 %s, i64 %s)\n", r, fn, ea, eb)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", ea)
	fmt.Fprintf(b, "  call void @rt_release(i64 %s)\n", eb)
	return r, true
}

// llName sanitizes a rune identifier into an LLVM-safe symbol fragment. LLVM
// global names may contain a wide set of characters when quoted, but we keep it to
// [A-Za-z0-9_] (reusing the C sanitizer) so symbols are plain and never need
// quoting; the operator-name table is shared with the C backend.
func llName(n string) string { return cName(n) }

// llThunkName / llCodeName namespace the two kinds of emitted symbol (a def thunk
// vs. a lifted code block), matching the C backend's scheme.
func llThunkName(n string) string { return "def_" + llName(n) }
func llCodeName(n string) string  { return "code_" + llName(n) }

// cStr renders a string constant as an i8* the emitted code can pass to the
// runtime: it interns the string (registering its module-scope global) and returns
// a getelementptr to the first byte of that global. The `[N x i8]*` length must
// match the global's, so it is computed from the same byte count.
func (em *llEmitter) cStr(s string) string {
	sym := em.intern(s)
	n := len(s) + 1 // + NUL terminator
	return fmt.Sprintf("getelementptr inbounds ([%d x i8], [%d x i8]* %s, i64 0, i64 0)", n, n, sym)
}

// llStrBytes renders a Go string as the BODY of an LLVM `c"..."` byte array
// (NUL-terminated, every non-printable/quote/backslash byte as `\HH`), returning
// the escaped text and the total byte count (including the NUL).
func llStrBytes(s string) (string, int) {
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c == '\\':
			b.WriteString("\\5C")
		case c == '"':
			b.WriteString("\\22")
		case c < 0x20 || c >= 0x7f:
			fmt.Fprintf(&b, "\\%02X", c)
		default:
			b.WriteByte(c)
		}
	}
	b.WriteString("\\00")
	return b.String(), len(s) + 1
}

var _ = sort.Strings
