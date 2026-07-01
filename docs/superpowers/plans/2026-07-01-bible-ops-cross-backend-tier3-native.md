# Bible Ops Cross-Backend Tier 3 (Native C + LLVM) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port the packed-String↔bytes codec, the D6 file/env/argv string layer, and all 11 "bible" host ops to the native C and LLVM-IR backends, then fold C+LLVM into the cross-backend divergence-lock so the bible builders are byte-identical across EIGHT backends.

**Architecture:** The native runtimes represent a Rune `String` (= `Bytes` = a packed base-256 `Nat` code) as a `K_BIG` bignum. Both runtimes already ship a hand-rolled base-1e9 bignum (`big_divmod`/`big_mul`/`big_add`/`big_cmp`/`big_from_long`) — so the codec (`d6_s2h` bignum→bytes, `d6_h2s` bytes→bignum) is a short loop mirroring the Rust `_s2h`/`_h2s`, with NO GMP and NO external library. Byte-exactness is FREE on native: raw `unsigned char` buffers, no UTF-16 trap. Each host op is baked into the runtime, gated by `usesForeign(p,"name")`, following the existing `emitFloatPrimsC` / `emitFSPrimsLL` precedent. C bodies are `static`; the LLVM bodies are byte-identical C except the accessor thunk has external linkage (no `static`) so the emitted `.ll`'s auto-declared `@name()` resolves.

**Tech Stack:** Go (the compiler); emitted C (compiled with `cc`) and LLVM-IR (compiled with `clang <prog>.ll <runtime>.c`); `sqlite3` CLI for `dbApply`; the `harness/bible_conformance_test.go` divergence-lock as the consumer.

## Global Constraints

- **Kernel FROZEN.** Touch only `codegen/c.go`, `codegen/ll.go`, `codegen/ll_runtime.go` (if a shared helper must live there), `harness/bible_conformance_test.go`, and (if a new demo listing is genuinely needed) `listings/`. NO changes to `core/`, `store/`, `elaborate/`. NO hash-format bump (no new core constructor).
- **BYTE-EXACT.** The corpus is non-ASCII Greek/Hebrew. Every String↔bytes boundary uses raw `unsigned char` buffers via the codec; file reads/writes use `fread`/`fwrite`/binary `fopen` (never a text-mode transform); `printStrCode` writes raw bytes to stdout (NOT a re-encoding print — this is why native avoids the JS `printStrCode` double-encode bug).
- **Codec is the shared contract.** `d6_s2h`/`d6_h2s` must be byte-identical in semantics to the Rust `_s2h`/`_h2s` (and thus to Go/JS/py/beam/jvm): decode by repeated divmod-by-256 until the quotient equals the `1` sentinel; encode by `n = 1; for i in rev(bytes): n = n*256 + bytes[i]`.
- **All six backends already agree** on the op semantics (js/go/py/rust/erl/jvm are byte-identical, Tier 1+2). The Rust bodies in `codegen/rust.go:186-262` are the semantic reference for every op in this plan — a native body must compute the identical result.
- **C↔LLVM delta rule** (applies to every op): the op's computational body is the SAME C for both backends. The differences are exactly two — (a) the zero-arg accessor thunk is `static Value name(void)` in C but `Value name(void)` (external linkage) in LLVM; (b) inside the LLVM runtime.c bodies, call helpers by the spelling the EXISTING `ll_runtime.go` bodies use (see `io_bind4` at `ll_runtime.go:530` and `emitFSPrimsLL` at `ll.go:312` for the exact `rt_apply`/`mkcon`/`big_*` spellings). `c.go` uses the unprefixed names (`apply`, `mkcon`, `con_set`, `big_from_long`, `big_divmod`, `big_cmp`, `big_add`, `big_mul`, `big_nlimbs`, `big_limb`, `mkclo`, `clo_set`, `UNIT`). Confirm the LLVM spelling once in Task 1 Step 1 and reuse.

## Reference: exact native runtime API (confirmed against c.go)

Copy these verbatim; they are the primitives every op body uses.

```c
/* value rep */
typedef intptr_t Value;                 /* immediate int (low bit 1) or Obj* */
enum { K_CLO=0,K_CON=1,K_PAIR=2,K_STR=3,K_PTR=4,K_UNIT=5,K_BIG=6,K_FLOAT=7,K_BYTES=8,K_BOUNCE=9 };
static Value UNIT;                       /* K_UNIT global, set in main */

/* closures / apply */
static Value mkclo(Value (*code)(Value,Value*), int nenv);   /* env-capturing closure */
static void  clo_set(Value clo, int i, Value v);             /* fill env slot i */
static Value apply(Value clo, Value arg);                    /* clo->code(arg, clo->slots) */

/* constructors */
static Value mkcon(int tag, const char* name, int nfield);
static void  con_set(Value o, int i, Value v);
/* (list nil = tag 0 "nil" nfield 1; cons = tag 1 "cons" nfield 3; slot 0 = erased type = UNIT.
    option none = tag 0 "none" nfield 1; some = tag 1 "some" nfield 2; slot 0 = UNIT.) */

/* bignum (base-1e9 limbs, little-endian; nfield = logical limb count; zero = nfield 0) */
static Value big_from_long(long n);
static int   big_cmp(Value a, Value b);            /* -1 / 0 / 1 by magnitude */
static Value big_add(Value a, Value b);
static Value big_mul(Value a, Value b);
static Value big_divmod(Value a, Value b, Value* rem);   /* quotient; *rem = remainder */
static int   big_nlimbs(Value v);
static long  big_limb(Value v, int i);             /* limb i (0 = least significant) */
```

Foreign accessors are wired by the IR: `CForeign{Name:"byteLen"}` lowers to the C text `byteLen()` (c.go:392), so each op MUST emit a `Value name(void)` returning its (curried) closure. On LLVM, `foreignNames(p)` auto-emits `declare i64 @name()` in the `.ll` (ll.go:128), so the accessor just needs external linkage in runtime.c.

## The codec (built once in Task 1, used by every later op)

```c
/* d6_s2h: decode a packed-String bignum `code` into a malloc'd byte buffer.
   *outlen receives the length. Caller frees. Mirrors Rust _s2h: divmod-by-256
   until the quotient == the 1 sentinel; each remainder is one byte, LSB first. */
static unsigned char* d6_s2h(Value code, size_t* outlen) {
  Value one  = big_from_long(1);
  Value d256 = big_from_long(256);
  size_t cap = 16, n = 0;
  unsigned char* buf = (unsigned char*)malloc(cap);
  Value b = code;
  while (big_cmp(b, one) > 0) {
    Value rem;
    Value q = big_divmod(b, d256, &rem);
    int byte = (big_nlimbs(rem) == 0) ? 0 : (int)(big_limb(rem, 0) & 255);
    if (n == cap) { cap *= 2; buf = (unsigned char*)realloc(buf, cap); }
    buf[n++] = (unsigned char)byte;
    b = q;
  }
  *outlen = n;
  return buf;
}

/* d6_h2s: fold a raw byte buffer into a packed-String bignum.
   Mirrors Rust _h2s: n = 1; for i from len-1 downto 0: n = n*256 + bytes[i]. */
static Value d6_h2s(const unsigned char* s, size_t len) {
  Value n = big_from_long(1);
  Value m = big_from_long(256);
  size_t i = len;
  while (i-- > 0) {
    n = big_add(big_mul(n, m), big_from_long((long)s[i]));
  }
  return n;
}
```

GC note (confirmed from the runtime map): `malloc`'d scratch that is freed before return is invisible to the GC and safe; `mkcon`/`big_*` allocations are GC-tracked automatically; `Value` locals on the C stack are covered by the conservative stack scan across `apply`/`big_*` calls, so no rooting is needed. Accumulate incremental results into a stack `Value`. `d6_s2h` is O(len · divmod) and divmod is O(limbs²·log) — slow for very long lines (the "codec-on-files slow" gotcha), but native is compiled and the real-data gate samples 1500 entries, so it is acceptable; do not micro-optimize (Standing Rule 1).

---

### Task 1: Native packed-String codec + the 4 pure bible ops (byteLen/splitOn/jsonStrField/sqlQuote), C + LLVM

De-risks the codec in isolation: the pure ops are the simplest codec consumers (no IO, no files, no `apply`).

**Files:**
- Modify: `codegen/c.go` (new `emitStreamPrimsC` + call it from the emit path near `emitFloatPrimsC`, c.go:91)
- Modify: `codegen/ll.go` (new `emitStreamPrimsLL` + call it from `EmitRuntimeFor`, ll.go:214)
- Test: `harness/bible_conformance_test.go` (new native-direct gate `TestBibleNativePure`)

**Interfaces:**
- Produces: `d6_s2h`/`d6_h2s` codec + `byteLen`/`splitOn`/`jsonStrField`/`sqlQuote` on both native backends, computing identically to `codegen/rust.go:186-197`.
- Consumes: the runtime API above (all confirmed present in c.go / ll_runtime.go).

- [ ] **Step 1: Confirm the C↔LLVM helper-spelling rule**

Read `codegen/ll.go:312-337` (`emitFSPrimsLL`) and `codegen/ll_runtime.go:520-548` (`io_bind4`, the `main`/`UNIT` wiring). Confirm the exact spelling the LLVM runtime.c bodies use for `apply`, `mkclo`/`clo_set`, `mkcon`/`con_set`, `big_from_long`/`big_divmod`/`big_cmp`/`big_add`/`big_mul`/`big_nlimbs`/`big_limb`, and `UNIT`. Record the mapping (e.g. `apply`→`rt_apply` or `apply`; `UNIT`→`UNIT` or a load). Use `c.go`'s unprefixed names for the C bodies verbatim; apply the confirmed spelling for the LLVM bodies. This mapping is fixed for all of Tasks 1-4.

- [ ] **Step 2: Add `emitStreamPrimsC` to c.go with the codec + pure ops**

Add a new function `emitStreamPrimsC(b *strings.Builder, p Program)` and call it from the emit path right after `emitFloatPrimsC(&b, p)` (c.go:91). Emit the codec once (gated so it appears whenever any stream OR file/env op is present), then the 4 pure ops. The codec gate predicate — reuse/define a helper mirroring rust.go:856:

```go
func usesNativeStrCodec(p Program) bool {
	return usesStream(p) || usesForeign(p, "printStrCode") || usesForeign(p, "getEnvCode") ||
		usesForeign(p, "readFileCode") || usesForeign(p, "writeFileCode") || usesForeign(p, "argAtCode")
}
```

Body of `emitStreamPrimsC`:

```go
func emitStreamPrimsC(b *strings.Builder, p Program) {
	if usesNativeStrCodec(p) {
		b.WriteString("static unsigned char* d6_s2h(Value code, size_t* outlen) { Value one = big_from_long(1); Value d256 = big_from_long(256); size_t cap = 16, n = 0; unsigned char* buf = (unsigned char*)malloc(cap); Value b = code; while (big_cmp(b, one) > 0) { Value rem; Value q = big_divmod(b, d256, &rem); int byte = (big_nlimbs(rem) == 0) ? 0 : (int)(big_limb(rem, 0) & 255); if (n == cap) { cap *= 2; buf = (unsigned char*)realloc(buf, cap); } buf[n++] = (unsigned char)byte; b = q; } *outlen = n; return buf; }\n")
		b.WriteString("static Value d6_h2s(const unsigned char* s, size_t len) { Value n = big_from_long(1); Value m = big_from_long(256); size_t i = len; while (i-- > 0) { n = big_add(big_mul(n, m), big_from_long((long)s[i])); } return n; }\n")
	}
	if usesForeign(p, "byteLen") {
		b.WriteString("static Value byteLen_c1(Value c, Value* env) { (void)env; size_t len; unsigned char* buf = d6_s2h(c, &len); free(buf); return big_from_long((long)len); }\n")
		b.WriteString("static Value byteLen(void) { return mkclo(&byteLen_c1, 0); }\n")
	}
	if usesForeign(p, "splitOn") {
		b.WriteString("static Value splitOn_c2(Value c, Value* env) { long sb = big_nlimbs(env[0]) == 0 ? 0 : (big_limb(env[0], 0) & 255); size_t len; unsigned char* data = d6_s2h(c, &len); Value lst = mkcon(0, \"nil\", 1); con_set(lst, 0, UNIT); size_t segstart = 0; size_t nsegs = 0; size_t* starts = (size_t*)malloc(sizeof(size_t) * (len + 2)); size_t* ends = (size_t*)malloc(sizeof(size_t) * (len + 2)); for (size_t i = 0; i <= len; i++) { if (i == len || data[i] == (unsigned char)sb) { starts[nsegs] = segstart; ends[nsegs] = i; nsegs++; segstart = i + 1; } } for (size_t s = nsegs; s-- > 0; ) { Value part = d6_h2s(data + starts[s], ends[s] - starts[s]); Value cons = mkcon(1, \"cons\", 3); con_set(cons, 0, UNIT); con_set(cons, 1, part); con_set(cons, 2, lst); lst = cons; } free(starts); free(ends); free(data); return lst; }\n")
		b.WriteString("static Value splitOn_c1(Value sep, Value* env) { (void)env; Value f = mkclo(&splitOn_c2, 1); clo_set(f, 0, sep); return f; }\n")
		b.WriteString("static Value splitOn(void) { return mkclo(&splitOn_c1, 0); }\n")
	}
	if usesForeign(p, "jsonStrField") {
		b.WriteString("static Value jsonStrField_c2(Value doc, Value* env) { size_t fl; unsigned char* fnb = d6_s2h(env[0], &fl); size_t dl; unsigned char* ds = d6_s2h(doc, &dl); size_t nl = fl + 2; unsigned char* needle = (unsigned char*)malloc(nl); needle[0] = '\"'; memcpy(needle + 1, fnb, fl); needle[fl + 1] = '\"'; Value none = mkcon(0, \"none\", 1); con_set(none, 0, UNIT); Value result = none; long found = -1; if (nl <= dl) { for (size_t i = 0; i + nl <= dl; i++) { if (memcmp(ds + i, needle, nl) == 0) { found = (long)i; break; } } } if (found >= 0) { size_t j = (size_t)found + nl; while (j < dl && (ds[j] == ' ' || ds[j] == '\\t' || ds[j] == ':')) j++; if (j < dl && ds[j] == '\"') { j++; size_t k = j; while (k < dl && ds[k] != '\"') k++; Value val = d6_h2s(ds + j, k - j); Value some = mkcon(1, \"some\", 2); con_set(some, 0, UNIT); con_set(some, 1, val); result = some; } } free(fnb); free(ds); free(needle); return result; }\n")
		b.WriteString("static Value jsonStrField_c1(Value field, Value* env) { (void)env; Value f = mkclo(&jsonStrField_c2, 1); clo_set(f, 0, field); return f; }\n")
		b.WriteString("static Value jsonStrField(void) { return mkclo(&jsonStrField_c1, 0); }\n")
	}
	if usesForeign(p, "sqlQuote") {
		b.WriteString("static Value sqlQuote_c1(Value s, Value* env) { (void)env; size_t len; unsigned char* in = d6_s2h(s, &len); unsigned char* out = (unsigned char*)malloc(len * 2 + 2); size_t o = 0; out[o++] = '\\''; for (size_t i = 0; i < len; i++) { if (in[i] == '\\'') out[o++] = '\\''; out[o++] = in[i]; } out[o++] = '\\''; Value r = d6_h2s(out, o); free(in); free(out); return r; }\n")
		b.WriteString("static Value sqlQuote(void) { return mkclo(&sqlQuote_c1, 0); }\n")
	}
}
```

(`splitOn` builds the cons list in reverse from `nil`, matching the Rust `.rev()` loop — segment order preserved. `jsonStrField`'s whitespace/colon skip + quote scan is a direct port of rust.go:194. `sqlQuote` doubles `'` and wraps — rust.go:197. All strictly byte-level, so non-ASCII is exact.)

- [ ] **Step 3: Add `emitStreamPrimsLL` to ll.go**

Add `emitStreamPrimsLL(b *strings.Builder, p Program)` and call it from `EmitRuntimeFor` (ll.go:214), after `emitFloatPrimsLL`. The bodies are the SAME C as Task 1 Step 2 with the two deltas from Step 1: (a) each accessor `Value name(void)` has NO `static`; (b) helper calls use the confirmed LLVM spelling. Worked example for `byteLen`:

```go
	if usesForeign(p, "byteLen") {
		b.WriteString("static Value byteLen_c1(Value c, Value* env) { (void)env; size_t len; unsigned char* buf = d6_s2h(c, &len); free(buf); return big_from_long((long)len); }\n")
		b.WriteString("Value byteLen(void) { return mkclo(&byteLen_c1, 0); }\n")   /* NO static on the accessor */
	}
```

(The internal `_c1`/`_c2` step functions stay `static`; only the public zero-arg accessor loses `static`. If Step 1 found the LLVM runtime uses `rt_`-prefixed helpers, apply that prefix to `mkclo`/`clo_set`/`mkcon`/`con_set`/`apply`/`big_*` inside these bodies AND in the codec — emit the codec into the LLVM runtime too, gated by `usesNativeStrCodec`, non-`static` not required for `d6_s2h`/`d6_h2s` since they are called only within runtime.c.)

- [ ] **Step 4: Verify the pure ops on C and LLVM via direct emit→compile→run**

```bash
go build -o /tmp/runeT ./cmd/rune
CC=$(command -v cc); CLANG=$(command -v clang)
runc()  { D=$(mktemp -d); /tmp/runeT emit "$1" "$2" --target c  > "$D/m.c"  && "$CC" -o "$D/m" "$D/m.c" && ( cd "${3:-$D}" && "$D/m" ); }
runll() { D=$(mktemp -d); /tmp/runeT emit "$1" "$2" --target ll > "$D/m.ll" && /tmp/runeT emit "$1" "$2" --target ll --runtime > "$D/rt.c" 2>/dev/null; }
```
(If `--runtime` is not a real flag, obtain the runtime the way `ll_test.go`'s `runLL` does — `codegen.LL{}.EmitRuntimeFor(p)`; the test harness in Step 5 is the authoritative runner. For a quick manual check, use the Step-5 test.) Expected values (same as the JVM/source gates):
```
strongLen   (ch551_json_field.rune)  -> 5
quoteEmbedded (ch557_sql_quote.rune) -> 6
```
A mismatch is a real codec/cell bug — debug it (most likely the divmod remainder-byte extraction or the cons/nil `con_set` slot order).

- [ ] **Step 5: Add the native-direct pure-op gate**

Append to `harness/bible_conformance_test.go`. Add a helper `runNativeListing(t, backend, listing, main, cwd)` (backend = "c" or "ll") that emits, compiles (`cc -o bin main.c` for C; `clang prog.ll runtime.c -o bin` for LLVM, using `codegen.LL{}.EmitRuntimeFor(p)` for the runtime), runs from `cwd`, returns trimmed stdout + an `ok` flag (false → toolchain absent, skip). Model it on `ll_test.go:22` (`runLL`) and the C compile in `io_os_test.go:1709`. Then:

```go
func TestBibleNativePure(t *testing.T) {
	cases := []struct{ listing, main, cwd, want string }{
		{"ch551_json_field.rune", "strongLen", "", "5"},
		{"ch557_sql_quote.rune", "quoteEmbedded", "", "6"},
	}
	for _, be := range []string{"c", "ll"} {
		ran := false
		for _, c := range cases {
			got, ok := runNativeListing(t, be, c.listing, c.main, c.cwd)
			if !ok {
				break
			}
			ran = true
			if got != c.want {
				t.Errorf("%s %s/%s = %q, want %q", be, c.listing, c.main, got, c.want)
			}
		}
		if !ran {
			t.Logf("%s toolchain absent -- skipped", be)
		}
	}
}
```

- [ ] **Step 6: Run the gate + commit**

```
go test ./harness/ -run 'TestBibleNativePure' -count=1 -v          # PASS (c + ll = 5, 6)
go test ./codegen/ -run 'TestC|TestLL' -count=1                    # existing native tests unaffected
```
```bash
git add codegen/c.go codegen/ll.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): packed-String codec + pure bible ops (byteLen/splitOn/jsonStrField/sqlQuote) on native C+LLVM\n\nd6_s2h/d6_h2s decode/encode the base-256 String bignum via the existing hand-rolled\nbignum (no GMP); byte-exact on raw char buffers (no UTF-16 trap). The 4 pure ops\nport rust.go:186-197. TestBibleNativePure verifies 5/6 on cc+clang. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 2: Native D6 file/env/argv layer (getEnvCode/readFileCode/writeFileCode/printStrCode/argAtCode/argCountCode/exitWith), C + LLVM

Closes native D6 string parity — after this, ALL backends reach D6. Reuses the Task-1 codec. Verified by running the existing D6 conformance listings ch215/ch216 on native.

**Files:**
- Modify: `codegen/c.go` (extend `emitStreamPrimsC`, or a sibling `emitD6PrimsC`, with the 7 ops)
- Modify: `codegen/ll.go` (mirror in `emitStreamPrimsLL` / `emitD6PrimsLL`)
- Test: `harness/bible_conformance_test.go` (new `TestD6NativeFileEnv` gate) — or extend `harness/io_os_test.go` if the D6 conformance gates live there; follow the file that holds `TestIOFileEnvConformance` / `TestIOArgvExitConformance`.

**Interfaces:**
- Consumes: `d6_s2h`/`d6_h2s` (Task 1). Produces: the 7 D6 ops on C+LLVM, identical to `codegen/rust.go:237-262`.

- [ ] **Step 1: Locate the D6 conformance gates**

`grep -rn 'TestIOFileEnvConformance\|TestIOArgvExitConformance\|argCountCode\|getEnvCode' harness/*.go`. Note which backends each currently runs (js/py/go/erl + rust) and how ch215/ch216 are invoked (cwd + env var `RUNE_D6` for ch215; argv + expected exit status for ch216). The native gate mirrors that.

- [ ] **Step 2: Emit the 7 D6 ops on C** (in `emitStreamPrimsC` after the pure ops)

Direct ports of rust.go:237-262. `printStrCode` writes RAW bytes then a newline (byte-exact, no re-encode). The empty/error sentinel is the packed bignum `1` = `big_from_long(1)` = `d6_h2s(NULL,0)`.

```go
	if usesForeign(p, "printStrCode") {
		b.WriteString("static Value printStrCode_c2(Value u, Value* env) { (void)u; size_t len; unsigned char* buf = d6_s2h(env[0], &len); fwrite(buf, 1, len, stdout); putchar('\\n'); free(buf); return env[0]; }\n")
		b.WriteString("static Value printStrCode_c1(Value c, Value* env) { (void)env; Value f = mkclo(&printStrCode_c2, 1); clo_set(f, 0, c); return f; }\n")
		b.WriteString("static Value printStrCode(void) { return mkclo(&printStrCode_c1, 0); }\n")
	}
	if usesForeign(p, "getEnvCode") {
		b.WriteString("static Value getEnvCode_c2(Value u, Value* env) { (void)u; size_t kl; unsigned char* kb = d6_s2h(env[0], &kl); char* key = (char*)malloc(kl + 1); memcpy(key, kb, kl); key[kl] = 0; const char* val = getenv(key); if (!val) val = \"\"; Value r = d6_h2s((const unsigned char*)val, strlen(val)); free(kb); free(key); return r; }\n")
		b.WriteString("static Value getEnvCode_c1(Value c, Value* env) { (void)env; Value f = mkclo(&getEnvCode_c2, 1); clo_set(f, 0, c); return f; }\n")
		b.WriteString("static Value getEnvCode(void) { return mkclo(&getEnvCode_c1, 0); }\n")
	}
	if usesForeign(p, "readFileCode") {
		b.WriteString("static Value readFileCode_c2(Value u, Value* env) { (void)u; size_t pl; unsigned char* pb = d6_s2h(env[0], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; FILE* fp = fopen(path, \"rb\"); free(pb); free(path); if (!fp) return big_from_long(1); size_t cap = 1024, n = 0; unsigned char* data = (unsigned char*)malloc(cap); int ch; while ((ch = fgetc(fp)) != EOF) { if (n == cap) { cap *= 2; data = (unsigned char*)realloc(data, cap); } data[n++] = (unsigned char)ch; } fclose(fp); Value r = d6_h2s(data, n); free(data); return r; }\n")
		b.WriteString("static Value readFileCode_c1(Value c, Value* env) { (void)env; Value f = mkclo(&readFileCode_c2, 1); clo_set(f, 0, c); return f; }\n")
		b.WriteString("static Value readFileCode(void) { return mkclo(&readFileCode_c1, 0); }\n")
	}
	if usesForeign(p, "writeFileCode") {
		b.WriteString("static Value writeFileCode_c3(Value u, Value* env) { (void)u; size_t pl; unsigned char* pb = d6_s2h(env[0], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; size_t dl; unsigned char* data = d6_s2h(env[1], &dl); FILE* fp = fopen(path, \"wb\"); if (fp) { fwrite(data, 1, dl, fp); fclose(fp); } free(pb); free(path); free(data); return env[1]; }\n")
		b.WriteString("static Value writeFileCode_c2(Value c, Value* env) { Value f = mkclo(&writeFileCode_c3, 2); clo_set(f, 0, env[0]); clo_set(f, 1, c); return f; }\n")
		b.WriteString("static Value writeFileCode_c1(Value pth, Value* env) { (void)env; Value f = mkclo(&writeFileCode_c2, 1); clo_set(f, 0, pth); return f; }\n")
		b.WriteString("static Value writeFileCode(void) { return mkclo(&writeFileCode_c1, 0); }\n")
	}
	if usesForeign(p, "argCountCode") {
		b.WriteString("static Value argCountCode_c1(Value u, Value* env) { (void)u; (void)env; long n = rune_argc > 1 ? rune_argc - 1 : 0; return big_from_long(n); }\n")
		b.WriteString("static Value argCountCode(void) { return mkclo(&argCountCode_c1, 0); }\n")
	}
	if usesForeign(p, "argAtCode") {
		b.WriteString("static Value argAtCode_c2(Value u, Value* env) { (void)u; long idx = (big_nlimbs(env[0]) == 0 ? 0 : big_limb(env[0], 0)) + 1; if (idx >= 1 && idx < rune_argc) { const char* a = rune_argv[idx]; return d6_h2s((const unsigned char*)a, strlen(a)); } return big_from_long(1); }\n")
		b.WriteString("static Value argAtCode_c1(Value i, Value* env) { (void)env; Value f = mkclo(&argAtCode_c2, 1); clo_set(f, 0, i); return f; }\n")
		b.WriteString("static Value argAtCode(void) { return mkclo(&argAtCode_c1, 0); }\n")
	}
	if usesForeign(p, "exitWith") {
		b.WriteString("static Value exitWith_c2(Value u, Value* env) { (void)u; long code = (big_nlimbs(env[0]) == 0 ? 0 : big_limb(env[0], 0)); exit((int)code); }\n")
		b.WriteString("static Value exitWith_c1(Value n, Value* env) { (void)env; Value f = mkclo(&exitWith_c2, 1); clo_set(f, 0, n); return f; }\n")
		b.WriteString("static Value exitWith(void) { return mkclo(&exitWith_c1, 0); }\n")
	}
```

`argCountCode`/`argAtCode` need argv. Confirm how the C runtime captures `argc`/`argv` in `main` (read c.go's `main` emission near c.go:160-180). If globals `rune_argc`/`rune_argv` do not exist, add `static int rune_argc; static char** rune_argv;` to the runtime and set them at the top of `main` (`rune_argc = argc; rune_argv = argv;`) — a one-line addition, gated so it only appears when `argCountCode`/`argAtCode`/... are used, OR unconditionally (harmless). Note the exact idx convention: rust adds 1 (skips argv[0], the program name), matching go/py; `argAtCode 0` → the first user arg = `argv[1]`. For LLVM, capture argv the same way in the runtime's `main`.

- [ ] **Step 3: Mirror on LLVM** (`emitStreamPrimsLL` / `emitD6PrimsLL`)

Same 7 bodies, applying the Step-1 C↔LLVM delta rule (accessor non-`static`; confirmed helper spelling). Ensure the LLVM runtime `main` captures `argc`/`argv` into the same globals.

- [ ] **Step 4: Verify ch215 (file/env) + ch216 (argv/exit) on C+LLVM**

```bash
# ch215 writes a file, reads it back, prints it, prints $RUNE_D6 -> "hello, wootz\nok\nunit"
D=$(mktemp -d); /tmp/runeT emit listings/ch215_files_env.rune main --target c > "$D/m.c" && cc -o "$D/m" "$D/m.c" && ( cd "$D" && RUNE_D6=unit ./m )
# ch216 prints argc + argv[0]/argv[1], exits with status = argc -> "2\nalpha\nbeta", exit 2
D=$(mktemp -d); /tmp/runeT emit listings/ch216_argv_exit.rune main --target c > "$D/m.c" && cc -o "$D/m" "$D/m.c" && ( cd "$D" && ./m alpha beta; echo "exit=$?" )
```
(Confirm the exact ch215/ch216 listing filenames via `ls listings/ | grep -E 'ch215|ch216'` — names may differ. Expected strings match the source-backend gates.) Repeat with `--target ll` compiled via `clang`.

- [ ] **Step 5: Add the native D6 conformance gate**

Add `TestD6NativeFileEnv` (and argv/exit) mirroring the source-backend `TestIOFileEnvConformance` / `TestIOArgvExitConformance` located in Step 1, but running the "c" and "ll" backends via `runNativeListing` (set `cmd.Dir` to a temp cwd and the `RUNE_D6` env for ch215; pass argv + assert exit status for ch216). Skip cleanly if `cc`/`clang` absent.

- [ ] **Step 6: Run + commit**

```
go test ./harness/ -run 'TestD6Native' -count=1 -v    # PASS on c + ll
```
```bash
git add codegen/c.go codegen/ll.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): D6 file/env/argv string layer on native C+LLVM (parity with rust)\n\ngetEnvCode/readFileCode/writeFileCode/printStrCode/argAtCode/argCountCode/exitWith\nover the Task-1 codec; printStrCode writes raw bytes (byte-exact, no re-encode).\nCloses native D6 string parity -- ch215/ch216 now run on cc+clang. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 3: Native bible higher-order ops (foldLines/foldDir), C + LLVM

The `apply`-ABI + file-read + recursive dir-walk. Ports rust.go:202-214.

**Files:**
- Modify: `codegen/c.go` (`emitStreamPrimsC` — the two higher-order ops + `#include <dirent.h>` gate)
- Modify: `codegen/ll.go` (mirror)
- Test: `harness/bible_conformance_test.go` (extend a native-direct gate `TestBibleNativeFold`)

**Interfaces:**
- Consumes: codec (Task 1), `apply`, `mkcon`. Produces: `foldLines`/`foldDir` on C+LLVM.

- [ ] **Step 1: Add `#include <dirent.h>` when foldDir is used**

In c.go, where headers are conditionally added (the `math.h`/`cblas.h` precedent at c.go:895/912), add: when `usesForeign(p,"foldDir")`, emit `#include <dirent.h>`. For LLVM, add the same include to the runtime.c preamble in `emitStreamPrimsLL` (or via the existing include mechanism in ll.go, cf. `emitFSPrimsLL` at ll.go:312 which already includes `<sys/stat.h>`/`<unistd.h>`).

- [ ] **Step 2: Emit foldLines + foldDir on C**

```go
	if usesForeign(p, "foldLines") {
		b.WriteString("static Value foldLines_c5(Value u, Value* env) { (void)u; Value step = env[0]; Value s0 = env[1]; size_t pl; unsigned char* pb = d6_s2h(env[2], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; free(pb); FILE* fp = fopen(path, \"rb\"); free(path); if (!fp) return s0; size_t cap = 1024, n = 0; unsigned char* data = (unsigned char*)malloc(cap); int ch; while ((ch = fgetc(fp)) != EOF) { if (n == cap) { cap *= 2; data = (unsigned char*)realloc(data, cap); } data[n++] = (unsigned char)ch; } fclose(fp); Value s = s0; size_t start = 0; size_t nlines = 0; for (size_t i = 0; i <= n; i++) { if (i == n) { if (i > start || (n > 0 && data[n-1] == '\\n')) { /* trailing empty handled below */ } } } size_t i = 0, ls = 0; size_t count = 0; /* first pass: count lines with trailing-empty drop */ (void)count; /* iterate splitting on \\n, drop a single trailing empty */ size_t p2 = 0; for (;;) { size_t e = p2; while (e < n && data[e] != '\\n') e++; int islast = (e >= n); if (islast && e == p2 && p2 != 0 && data[p2-1] == '\\n') { break; } Value line = d6_h2s(data + p2, e - p2); s = apply(apply(apply(step, s), line), UNIT); if (islast) break; p2 = e + 1; if (p2 > n) break; } free(data); (void)i; (void)ls; (void)start; (void)nlines; return s; }\n")
		b.WriteString("static Value foldLines_c4(Value s0, Value* env) { Value f = mkclo(&foldLines_c5, 3); clo_set(f, 0, env[0]); clo_set(f, 1, s0); clo_set(f, 2, env[1]); return f; }\n")
		b.WriteString("static Value foldLines_c3(Value step, Value* env) { Value f = mkclo(&foldLines_c4, 2); clo_set(f, 0, step); clo_set(f, 1, env[0]); return f; }\n")
		b.WriteString("static Value foldLines_c2(Value path, Value* env) { (void)env; Value f = mkclo(&foldLines_c3, 1); clo_set(f, 0, path); return f; }\n")
		b.WriteString("static Value foldLines_c1(Value _s, Value* env) { (void)_s; (void)env; return mkclo(&foldLines_c2, 0); }\n")
		b.WriteString("static Value foldLines(void) { return mkclo(&foldLines_c1, 0); }\n")
	}
```

**NOTE to implementer:** the line-splitting loop above is written defensively but is convoluted — REPLACE it with the clean two-step form used everywhere else in this plan (collect `(start,end)` segment bounds splitting on `\n`, then drop a single trailing-empty segment iff the data ends in `\n`, then `apply` per segment in order):

```c
/* clean foldLines_c5 core (use THIS): */
size_t cap2 = 16, nseg = 0;
size_t* st = malloc(sizeof(size_t)*(n+2)); size_t* en = malloc(sizeof(size_t)*(n+2)); (void)cap2;
size_t seg = 0;
for (size_t k = 0; k <= n; k++) { if (k == n || data[k] == '\n') { st[nseg]=seg; en[nseg]=k; nseg++; seg=k+1; } }
if (nseg > 0 && en[nseg-1] == st[nseg-1]) nseg--;   /* drop the single trailing empty from a final '\n' */
Value s = s0;
for (size_t k = 0; k < nseg; k++) { Value line = d6_h2s(data + st[k], en[k]-st[k]); s = apply(apply(apply(step, s), line), UNIT); }
free(st); free(en); free(data);
return s;
```

This matches Rust's `data.split(b'\n')` + drop-trailing-empty exactly (splitting on `\n` KEEPS `\r`, the Tier-2 uniform contract). Use the clean form in both C and LLVM.

```go
	if usesForeign(p, "foldDir") {
		b.WriteString("static int d6_namecmp(const void* a, const void* b) { return strcmp(*(const char* const*)a, *(const char* const*)b); }\n")
		b.WriteString("static Value d6_foldwalk(const char* dir, const unsigned char* sfx, size_t sfxlen, Value step, Value s) { DIR* dp = opendir(dir); if (!dp) return s; char** names = NULL; size_t cnt = 0, cap = 0; struct dirent* de; while ((de = readdir(dp)) != NULL) { if (strcmp(de->d_name, \".\") == 0 || strcmp(de->d_name, \"..\") == 0) continue; if (cnt == cap) { cap = cap ? cap * 2 : 8; names = (char**)realloc(names, cap * sizeof(char*)); } names[cnt++] = strdup(de->d_name); } closedir(dp); qsort(names, cnt, sizeof(char*), d6_namecmp); for (size_t i = 0; i < cnt; i++) { size_t dl = strlen(dir), nl = strlen(names[i]); char* full = (char*)malloc(dl + nl + 2); memcpy(full, dir, dl); full[dl] = '/'; memcpy(full + dl + 1, names[i], nl + 1); struct stat sb; if (stat(full, &sb) == 0 && S_ISDIR(sb.st_mode)) { s = d6_foldwalk(full, sfx, sfxlen, step, s); } else { size_t fl = strlen(full); if (fl >= sfxlen && memcmp(full + fl - sfxlen, sfx, sfxlen) == 0) { FILE* fp = fopen(full, \"rb\"); if (fp) { size_t bc = 1024, bn = 0; unsigned char* bd = (unsigned char*)malloc(bc); int ch; while ((ch = fgetc(fp)) != EOF) { if (bn == bc) { bc *= 2; bd = (unsigned char*)realloc(bd, bc); } bd[bn++] = (unsigned char)ch; } fclose(fp); Value content = d6_h2s(bd, bn); s = apply(apply(apply(step, s), content), UNIT); free(bd); } } } free(full); free(names[i]); } free(names); return s; }\n")
		b.WriteString("static Value foldDir_c6(Value u, Value* env) { (void)u; Value step = env[0]; Value s0 = env[1]; size_t dl; unsigned char* db = d6_s2h(env[2], &dl); char* dir = (char*)malloc(dl + 1); memcpy(dir, db, dl); dir[dl] = 0; free(db); size_t sl; unsigned char* sfx = d6_s2h(env[3], &sl); Value r = d6_foldwalk(dir, sfx, sl, step, s0); free(dir); free(sfx); return r; }\n")
		b.WriteString("static Value foldDir_c5(Value s0, Value* env) { Value f = mkclo(&foldDir_c6, 4); clo_set(f, 0, env[0]); clo_set(f, 1, s0); clo_set(f, 2, env[1]); clo_set(f, 3, env[2]); return f; }\n")
		b.WriteString("static Value foldDir_c4(Value step, Value* env) { Value f = mkclo(&foldDir_c5, 3); clo_set(f, 0, step); clo_set(f, 1, env[0]); clo_set(f, 2, env[1]); return f; }\n")
		b.WriteString("static Value foldDir_c3(Value suf, Value* env) { Value f = mkclo(&foldDir_c4, 2); clo_set(f, 0, env[0]); clo_set(f, 1, suf); return f; }\n")
		b.WriteString("static Value foldDir_c2(Value dirc, Value* env) { (void)env; Value f = mkclo(&foldDir_c3, 1); clo_set(f, 0, dirc); return f; }\n")
		b.WriteString("static Value foldDir_c1(Value _s, Value* env) { (void)_s; (void)env; return mkclo(&foldDir_c2, 0); }\n")
		b.WriteString("static Value foldDir(void) { return mkclo(&foldDir_c1, 0); }\n")
	}
```

**Ordering equivalence (critical):** `d6_foldwalk` sorts each directory's entries by name with `strcmp` (byte order) and recurses depth-first pre-order — matching Go `filepath.WalkDir` (per-dir lexical) and the Rust/JVM `_foldwalk`. `stat` + `S_ISDIR` need `<sys/stat.h>` (add the include like `<dirent.h>`). Filenames are ASCII in the corpus, so `strcmp` = byte order = the other backends. (Non-ASCII filenames would diverge — parked, no consumer; note it in the report.)

- [ ] **Step 3: Mirror on LLVM** — same two bodies + the `d6_foldwalk`/`d6_namecmp` helpers, applying the delta rule; add `<dirent.h>`/`<sys/stat.h>` to the LLVM runtime.

- [ ] **Step 4: Verify on C+LLVM**

```
foldLines: ch549_conllu_count.rune main  (cwd harness/testdata) -> 11\n11
foldDir:   ch554_fold_dir.rune     main  (cwd harness/testdata) -> 3\n3
CRLF lock: ch560_crlf_lines.rune   main  (cwd harness/testdata) -> 16\n16
```

- [ ] **Step 5: Extend the native gate + commit**

```go
func TestBibleNativeFold(t *testing.T) {
	cases := []struct{ listing, main, cwd, want string }{
		{"ch549_conllu_count.rune", "main", "testdata", "11\n11"},
		{"ch554_fold_dir.rune", "main", "testdata", "3\n3"},
		{"ch560_crlf_lines.rune", "main", "testdata", "16\n16"},
	}
	for _, be := range []string{"c", "ll"} {
		for _, c := range cases {
			got, ok := runNativeListing(t, be, c.listing, c.main, c.cwd)
			if !ok { break }
			if got != c.want { t.Errorf("%s %s = %q, want %q", be, c.listing, got, c.want) }
		}
	}
}
```
```bash
go test ./harness/ -run 'TestBibleNativeFold' -count=1 -v
git add codegen/c.go codegen/ll.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible higher-order ops (foldLines/foldDir) on native C+LLVM\n\nfoldLines slurps + splits on newline only (keeps CR, the Tier-2 uniform contract);\nfoldDir walks readdir sorted by name, depth-first, suffix-filtered, matching\nfilepath.WalkDir order. apply(apply(apply(step,s),line),UNIT) drives the erased\nRune step. Verifies 11/3/16 on cc+clang. No core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 4: Native bible write-stream (Handle/openWrite/writeChunk/closeWrite/sortFile) + dbApply, C + LLVM

Ports rust.go:216-235. Opaque write handle = a small-int token into a static `FILE*` table (the `__socks` precedent, but for files).

**Files:**
- Modify: `codegen/c.go`, `codegen/ll.go` (the write-stream + dbApply ops + the handle table)
- Test: `harness/bible_conformance_test.go` (`TestBibleNativeWriteStreamDb`)

**Interfaces:**
- Consumes: codec (Task 1). Produces: `Handle` (foreign type, erases to `UNIT`), `openWrite`/`writeChunk`/`closeWrite`/`sortFile`/`dbApply` on C+LLVM.

- [ ] **Step 1: Emit the handle table + write-stream + sortFile + dbApply on C**

```go
	if usesForeign(p, "openWrite") || usesForeign(p, "writeChunk") || usesForeign(p, "closeWrite") {
		b.WriteString("#define D6_WH_MAX 256\nstatic FILE* d6_wh[D6_WH_MAX];\nstatic long d6_whid = 0;\n")
	}
	if usesForeign(p, "Handle") {
		b.WriteString("static Value Handle(void) { return UNIT; }\n")
	}
	if usesForeign(p, "openWrite") {
		b.WriteString("static Value openWrite_c2(Value u, Value* env) { (void)u; size_t pl; unsigned char* pb = d6_s2h(env[0], &pl); char* path = (char*)malloc(pl + 1); memcpy(path, pb, pl); path[pl] = 0; free(pb); FILE* fp = fopen(path, \"wb\"); free(path); if (!fp || d6_whid + 1 >= D6_WH_MAX) { if (fp) fclose(fp); return big_from_long(0); } long id = ++d6_whid; d6_wh[id] = fp; return big_from_long(id); }\n")
		b.WriteString("static Value openWrite_c1(Value path, Value* env) { (void)env; Value f = mkclo(&openWrite_c2, 1); clo_set(f, 0, path); return f; }\n")
		b.WriteString("static Value openWrite(void) { return mkclo(&openWrite_c1, 0); }\n")
	}
	if usesForeign(p, "writeChunk") {
		b.WriteString("static Value writeChunk_c3(Value u, Value* env) { (void)u; Value h = env[0]; long id = (big_nlimbs(h) == 0) ? 0 : big_limb(h, 0); size_t cl; unsigned char* cb = d6_s2h(env[1], &cl); if (id > 0 && id < D6_WH_MAX && d6_wh[id]) { fwrite(cb, 1, cl, d6_wh[id]); putc('\\n', d6_wh[id]); } free(cb); return h; }\n")
		b.WriteString("static Value writeChunk_c2(Value c, Value* env) { Value f = mkclo(&writeChunk_c3, 2); clo_set(f, 0, env[0]); clo_set(f, 1, c); return f; }\n")
		b.WriteString("static Value writeChunk_c1(Value h, Value* env) { (void)env; Value f = mkclo(&writeChunk_c2, 1); clo_set(f, 0, h); return f; }\n")
		b.WriteString("static Value writeChunk(void) { return mkclo(&writeChunk_c1, 0); }\n")
	}
	if usesForeign(p, "closeWrite") {
		b.WriteString("static Value closeWrite_c2(Value u, Value* env) { (void)u; Value h = env[0]; long id = (big_nlimbs(h) == 0) ? 0 : big_limb(h, 0); if (id > 0 && id < D6_WH_MAX && d6_wh[id]) { fclose(d6_wh[id]); d6_wh[id] = NULL; } return UNIT; }\n")
		b.WriteString("static Value closeWrite_c1(Value h, Value* env) { (void)env; Value f = mkclo(&closeWrite_c2, 1); clo_set(f, 0, h); return f; }\n")
		b.WriteString("static Value closeWrite(void) { return mkclo(&closeWrite_c1, 0); }\n")
	}
	if usesForeign(p, "sortFile") {
		b.WriteString("static int d6_linecmp(const void* a, const void* b) { const unsigned char* x = *(const unsigned char* const*)a; const unsigned char* y = *(const unsigned char* const*)b; size_t xl = *(const size_t*)(x - sizeof(size_t)); (void)xl; (void)y; return 0; }\n")
		// NOTE: the packed-pointer linecmp above is a stub -- IMPLEMENT sortFile with the clean
		// approach below (parallel arrays of (ptr,len), a comparator that does a bytewise
		// memcmp with tie-break on length == C strcmp semantics == Go sort.Strings on 0-255 bytes).
		b.WriteString("static Value sortFile_c3(Value u, Value* env) { (void)u; size_t il; unsigned char* ib = d6_s2h(env[0], &il); char* ip = (char*)malloc(il+1); memcpy(ip, ib, il); ip[il]=0; free(ib); size_t ol; unsigned char* ob = d6_s2h(env[1], &ol); char* op = (char*)malloc(ol+1); memcpy(op, ob, ol); op[ol]=0; free(ob); FILE* fp = fopen(ip, \"rb\"); free(ip); if (!fp) { FILE* o = fopen(op, \"wb\"); if (o) fclose(o); free(op); return UNIT; } size_t cap=1024,n=0; unsigned char* data=(unsigned char*)malloc(cap); int ch; while((ch=fgetc(fp))!=EOF){ if(n==cap){cap*=2;data=(unsigned char*)realloc(data,cap);} data[n++]=(unsigned char)ch; } fclose(fp); /* split into (start,len) segments on \\n, drop trailing empty */ size_t* st=(size_t*)malloc(sizeof(size_t)*(n+2)); size_t* ln=(size_t*)malloc(sizeof(size_t)*(n+2)); size_t nseg=0, seg=0; for(size_t k=0;k<=n;k++){ if(k==n||data[k]=='\\n'){ st[nseg]=seg; ln[nseg]=k-seg; nseg++; seg=k+1; } } if(nseg>0 && ln[nseg-1]==0) nseg--; d6_sortsegs(data, st, ln, nseg); FILE* o=fopen(op,\"wb\"); if(o){ for(size_t k=0;k<nseg;k++){ fwrite(data+st[k],1,ln[k],o); putc('\\n',o); } fclose(o); } free(st); free(ln); free(data); free(op); return UNIT; }\n")
		b.WriteString("static Value sortFile_c2(Value outp, Value* env) { Value f = mkclo(&sortFile_c3, 2); clo_set(f, 0, env[0]); clo_set(f, 1, outp); return f; }\n")
		b.WriteString("static Value sortFile_c1(Value inp, Value* env) { (void)env; Value f = mkclo(&sortFile_c2, 1); clo_set(f, 0, inp); return f; }\n")
		b.WriteString("static Value sortFile(void) { return mkclo(&sortFile_c1, 0); }\n")
	}
	if usesForeign(p, "dbApply") {
		b.WriteString("static Value dbApply_c3(Value u, Value* env) { (void)u; size_t dl; unsigned char* db = d6_s2h(env[0], &dl); char* dbp = (char*)malloc(dl+1); memcpy(dbp, db, dl); dbp[dl]=0; free(db); size_t sl; unsigned char* sb = d6_s2h(env[1], &sl); char* sqp = (char*)malloc(sl+1); memcpy(sqp, sb, sl); sqp[sl]=0; free(sb); size_t clen = dl + sl + 32; char* cmd = (char*)malloc(clen); snprintf(cmd, clen, \"sqlite3 '%s' '.read %s'\", dbp, sqp); int rc = system(cmd); (void)rc; free(dbp); free(sqp); free(cmd); return UNIT; }\n")
		b.WriteString("static Value dbApply_c2(Value sql, Value* env) { Value f = mkclo(&dbApply_c3, 2); clo_set(f, 0, env[0]); clo_set(f, 1, sql); return f; }\n")
		b.WriteString("static Value dbApply_c1(Value db, Value* env) { (void)env; Value f = mkclo(&dbApply_c2, 1); clo_set(f, 0, db); return f; }\n")
		b.WriteString("static Value dbApply(void) { return mkclo(&dbApply_c1, 0); }\n")
	}
```

**IMPLEMENTER — sortFile comparator (do NOT ship the stub `d6_linecmp`):** implement bytewise line sort matching Go `sort.Strings` / Rust `lines.sort()` (unsigned byte lexicographic, shorter-is-less on a prefix tie). Since segments are `(offset,len)` into one buffer, sort an index array. Use a helper `d6_sortsegs(unsigned char* data, size_t* st, size_t* ln, size_t nseg)` that sorts the parallel `st`/`ln` arrays by comparing `data+st[i]` (len `ln[i]`) against `data+st[j]` bytewise:

```c
static void d6_sortsegs(unsigned char* data, size_t* st, size_t* ln, size_t nseg) {
  for (size_t i = 1; i < nseg; i++) {            /* simple insertion sort (stable, small n) */
    size_t si = st[i], li = ln[i];
    size_t j = i;
    while (j > 0) {
      size_t sp = st[j-1], lp = ln[j-1];
      size_t m = lp < li ? lp : li;
      int c = memcmp(data + sp, data + si, m);
      if (c > 0 || (c == 0 && lp > li)) { st[j] = sp; ln[j] = lp; j--; } else break;
    }
    st[j] = si; ln[j] = li;
  }
}
```
Emit `d6_sortsegs` before `sortFile_c3` and delete the `d6_linecmp` stub. (Insertion sort is fine — lexicon line counts are modest; Standing Rule 1, do not reach for qsort-with-context unless a perf consumer appears.) `memcmp` on `unsigned char` = byte order = Go `sort.Strings`, byte-exact for Greek/Hebrew.

**dbApply shell-quoting caveat:** `system("sqlite3 'db' '.read sql'")` runs via `/bin/sh`. The db/sql paths are test temp paths (no quotes/metacharacters), so this is safe here; the source backends (go/jvm) use argv (no shell). Note this in the report — a path containing a `'` would break, but there is no such consumer. (Acceptable per Standing Rule 1; if ever a consumer needs it, switch to `posix_spawn`/`fork`+`execvp`.)

- [ ] **Step 2: Mirror on LLVM** — same bodies + the handle table + `d6_sortsegs`, applying the delta rule.

- [ ] **Step 3: Verify on C+LLVM**

```
openWrite/writeChunk/closeWrite: ch552_write_stream.rune main (temp cwd) -> 2\n2
sortFile:                        ch553_sort_file.rune   main (temp cwd) -> 5\n5
dbApply: build ch558_db_apply.rune main (temp cwd), then `sqlite3 ch558.db "SELECT count(*) FROM t"` -> 2
```

- [ ] **Step 4: Add the native write-stream/db gate + commit**

```go
func TestBibleNativeWriteStreamDb(t *testing.T) {
	for _, be := range []string{"c", "ll"} {
		for _, c := range []struct{ listing, want string }{
			{"ch552_write_stream.rune", "2\n2"},
			{"ch553_sort_file.rune", "5\n5"},
		} {
			got, ok := runNativeListing(t, be, c.listing, "main", t.TempDir())
			if !ok { break }
			if got != c.want { t.Errorf("%s %s = %q, want %q", be, c.listing, got, c.want) }
		}
		// ch558 dbApply: build in a temp cwd, query count(*) -> 2 (skip if sqlite3 absent).
		// Mirror TestBibleJVMWriteStreamDb's dbApply block, compiling via the "be" backend.
	}
}
```
```bash
go test ./harness/ -run 'TestBibleNativeWriteStreamDb' -count=1 -v
git add codegen/c.go codegen/ll.go harness/bible_conformance_test.go
git commit -m "$(printf 'feat(codegen): bible write-stream + sortFile + dbApply on native C+LLVM\n\nHandle = a small-int token into a static FILE* table (the __socks precedent);\nwriteChunk appends chunk+newline raw; sortFile bytewise-sorts lines (memcmp ==\nsort.Strings); dbApply shells to sqlite3. Verifies 2/5 + db count 2 on cc+clang.\nNo core change.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

### Task 5: Add C+LLVM to the divergence-lock (8-way) + native D6 gates + real-data + full regression

Folds C and LLVM into the shared `bibleBackends()` so EVERY conformance gate asserts byte-identity across EIGHT backends (js/go/py/rust/erl/jvm/c/ll).

**Files:**
- Modify: `harness/bible_conformance_test.go` (`bibleBackends()` appends c + ll conditionally)

**Interfaces:**
- Consumes: every native op (Tasks 1-4). Produces: c + ll in `bibleBackends()` → all shared gates (Pure/Fold/WriteStream/DbApply/Builders/CRLF/RealData) run 8-way.

- [ ] **Step 1: Append C + LLVM to `bibleBackends()`**

Follow the JVM precedent (Tier 2, the conditional append with a `compile` hook). The C entry: `bin="cc"`, `emit=codegen.C{}.Emit`, `compile=cc -o out src`, `run=exec.Command(bin)`. The LLVM entry needs BOTH the `.ll` and the `EmitRuntimeFor(p)` runtime.c compiled together — confirm the `bibleBackend` struct's `emit`/`compile` fields can express a two-file compile; if not, extend the struct minimally (the cleanest: the `emit` writes the `.ll`, and the `compile` hook writes the runtime.c next to it and runs `clang prog.ll runtime.c -o out`). Read the current `bibleBackend` struct + `runBibleBackend`/`buildBibleFile` (bible_conformance_test.go) to see how rust's `compile` hook works, and whether a second emitted artifact (the runtime) needs a new field or can be produced inside the `compile` closure from the Program.

```go
	if cc, err := exec.LookPath("cc"); err == nil {
		bks = append(bks, bibleBackend{"c", cc, "c",
			func(p codegen.Program) (codegen.TargetSource, error) { return codegen.C{}.Emit(p) },
			func(bin string) *exec.Cmd { return exec.Command(bin) },
			func(src, out string) *exec.Cmd { return exec.Command(cc, "-o", out, src) }})
	}
	if clang, err := exec.LookPath("clang"); err == nil {
		// LLVM needs the program .ll + the program-specific runtime.c. If bibleBackend cannot
		// carry a second artifact, produce the runtime inside the compile closure via
		// codegen.LL{}.EmitRuntimeFor(p) written beside src, then clang both. See Step 1 note.
		bks = append(bks, bibleBackend{"ll", clang, "ll", /* emit .ll */ /* run bin */ /* compile: clang src+runtime -o out */})
	}
```

Because the LLVM compile needs the `Program` (for `EmitRuntimeFor`), the cleanest wiring is to have the LLVM `emit` closure stash the runtime.c into the same temp dir, or extend `bibleBackend` with an optional `runtime func(codegen.Program) string`. Choose the minimal change; document it in the report.

- [ ] **Step 2: Verify all conformance gates run 8-way**

```
go test ./harness/ -run 'TestBibleConformance' -count=1 -v
```
`TestBibleConformanceBuilders` now compares c + ll `shared-root.out` + `lexicon.sql` against the other six. They MUST be byte-identical. **A native divergence here is a real bug in a Task 1-4 body** (codec, cell, sort, or line-split) — do NOT weaken the assertion or drop native to pass. If it diverges, capture which builder + which bytes differ and fix the responsible native op body.

- [ ] **Step 3: Independent 8-way builder byte-check + sampled real-data**

```bash
go test ./harness/ -run 'TestBibleConformanceBuilders' -count=1 -v   # 8-way sha256 identical
BIBLE_REPO="$HOME/matt/bible" go test ./harness/ -run 'TestBibleConformanceRealData' -count=1 -v -timeout 1200s   # 8 backends, sampled Greek/Hebrew
```
(Native adds compile time; the sampled 1500-entry gate may need a longer timeout than the 6-way run. If `$HOME/matt/bible` is absent the gate SKIPs — note it.)

- [ ] **Step 4: Full regression + commit**

```
go test ./harness/ -run 'TestBible|TestD6Native' -count=1
go test ./harness/ -run 'TestListingsElaborateAndCheck' -count=1
go test ./harness/ -run 'TestBackendConformance' -count=1     # existing 8-backend generic gate unaffected
go test ./codegen/ -count=1                                    # native codegen tests green
```
```bash
git add harness/bible_conformance_test.go
git commit -m "$(printf 'test(harness): add native C+LLVM to the bible divergence-lock (8-way)\n\nC and LLVM join bibleBackends() so every conformance gate -- pure/fold/write-stream/\ndbApply/builders/CRLF/real-data -- now asserts byte-identity across EIGHT backends\n(js/go/py/rust/erl/jvm/c/ll). Tier 3 complete: the bible builders are byte-identical\non all eight, incl real Greek/Hebrew data. Native D6 string parity closed.\n\nCo-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>')"
```

---

## Self-Review

**1. Spec coverage.** The user chose "Bible + native D6 parity." Codec → Task 1 (built once, de-risked by the 4 pure ops). Native D6 file/env/argv layer (getEnvCode/readFileCode/writeFileCode/printStrCode/argAtCode/argCountCode/exitWith) → Task 2, closing native D6 parity (ch215/ch216 run native). The 11 bible ops → Task 1 (4 pure) + Task 3 (foldLines/foldDir) + Task 4 (Handle + write trio + sortFile + dbApply). Task 5 folds C+LLVM into the shared `bibleBackends()` so all gates + the builder divergence-lock + sampled real-data go 8-way. Byte-exactness is native-free (raw char buffers) and enforced at every boundary; `printStrCode` writes raw bytes (dodging the JS double-encode bug).

**2. Placeholder scan.** Every op has a complete C body ported from the confirmed Rust reference (rust.go:186-262) using the confirmed c.go runtime API. The two DELIBERATE non-final snippets are flagged loudly for the implementer to replace with the clean form given inline: (a) the convoluted `foldLines_c5` line-loop → the clean segment-collect form; (b) the `d6_linecmp` stub → the `d6_sortsegs` insertion-sort. Both replacements are given in full in the same step, so there is no missing code — only a "use THIS one" instruction. The LLVM bodies are specified as the SAME C under a precise, deterministic two-part delta rule (accessor non-static + confirmed helper spelling) with a worked `byteLen` example; this is a complete transform, not a placeholder. Runtime-name confirmations (LLVM helper spelling; `rune_argc`/`rune_argv` capture; the `bibleBackend` two-artifact wiring for LLVM) are Step-1 verification points, proven by the compile + gates.

**3. Type consistency.** Foreign accessors are `Value name(void)` called as `name()` (matching `CForeign` lowering, c.go:392); pure ops carry no world token; IO ops carry a trailing `_u` arg and return `UNIT` or the handle/String; the apply ABI is `apply(apply(apply(step, s), line), UNIT)`; cells are `mkcon(tag,name,nfield)` + `con_set(o,0,UNIT)` for the erased type slot (nil=0/cons=1, none=0/some=1). The codec gate `usesNativeStrCodec` mirrors rust.go:856. The write handle is a `big_from_long(id)` token into `d6_wh[]` (the `__socks` precedent). Listings reused: ch215/ch216 (D6), ch549/551/552/553/554/557/558/559 (bible) + ch560 (CRLF) + testdata fixtures + `$HOME/matt/bible` real-data — all already exist from Tiers 1-2. `runNativeListing` is the new shared native runner; `bibleBackends()` gains c + ll conditionally, exactly as it gained jvm in Tier 2.
