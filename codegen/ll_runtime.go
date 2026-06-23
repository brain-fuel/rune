package codegen

// ll_runtime.go — the LLVM backend's module preamble + runtime-helper declares,
// and the linkable C runtime (the external-linkage twin of cRuntime in c.go). The
// emitted `.ll` is genuine LLVM IR for the program LOGIC; the tagged-word Value
// rep, the mark-sweep GC, `apply`, and `$show` live in this C shim, compiled and
// linked by clang alongside the `.ll` (`clang program.ll runtime.c -o exe`). The C
// backend (c.go) is UNTOUCHED — this is a parallel, external-linkage copy so the
// two native backends share the rep without either depending on the other's
// `static` symbols.

// llPreamble is the module-level data the emitted functions reference: the boxed
// UNIT singleton (a global i64 the runtime initializes in main before rune_main).
const llPreamble = `; UNIT is the boxed unit singleton, set by the runtime's main before rune_main.
@UNIT = external global i64

`

// llDeclares declares every core runtime helper the emitted IR may call. They are
// defined (external linkage) in the linked runtime.c (EmitRuntime / llRuntimeC).
const llDeclares = `; Core runtime helpers (defined in the linked runtime.c).
declare i64 @rt_mkint(i64)
declare i64 @rt_as_int(i64)
declare i64 @rt_mkclo(i64 (i64, i64*)*, i32)
declare void @rt_clo_set(i64, i32, i64)
declare i64 @rt_mkcon(i32, i8*, i32)
declare void @rt_con_set(i64, i32, i64)
declare i64 @rt_con_get(i64, i32)
declare i32 @rt_con_tag(i64)
declare i64 @rt_mkpair(i64, i64)
declare i64 @rt_pair_fst(i64)
declare i64 @rt_pair_snd(i64)
declare i64 @rt_mkstr(i8*)
declare i64 @rt_mkptr(i64)
declare i64 @rt_apply(i64, i64)
declare i64 @rt_nat_add(i64, i64)
declare i64 @rt_nat_mul(i64, i64)
declare i64 @rt_nat_monus(i64, i64)
declare i64 @rt_nat_div(i64, i64)
declare i64 @rt_nat_mod(i64, i64)
declare i64 @rt_big_from_long(i64)
declare i64 @rt_big_parse(i8*)
declare i64 @rt_big_succ(i64)
declare i64 @rt_big_cmp(i64, i64)
declare void @rt_abort()
declare void @rt_add_root(i64*)
declare void @rt_show_line(i64)
`

// llQuotDeclares declares the erased quotient builtin accessors (referenced via
// CGlobal as def_qin/def_qlift/def_qsound/def_qind when the program uses quotients).
const llQuotDeclares = `; Quotient builtins (erased; defined in runtime.c).
declare i64 @def_qin()
declare i64 @def_qlift()
declare i64 @def_qsound()
declare i64 @def_qind()
`

// llIODeclares declares the erased IO monad builtin accessors (referenced via
// CGlobal as def_pureIO/def_bindIO when the program uses IO).
const llIODeclares = `; IO monad builtins (erased; defined in runtime.c).
declare i64 @def_pureIO()
declare i64 @def_bindIO()
`

// llRuntimeC is the linkable C runtime: the SAME tagged-word Value rep + mark-sweep
// GC + apply/show as cRuntime, but with EXTERNAL `rt_*` wrappers the emitted LLVM IR
// calls, and an external UNIT global. Kept byte-compatible with the C backend's rep
// so $show is byte-identical. (Distinct symbol namespace `rt_*` so it never clashes
// with c.go's `static` runtime if both ever shared a TU — they do not.)
const llRuntimeC = `/* rune llvm-backend runtime — the external-linkage twin of the C backend's
   embedded runtime. Compiled and linked by clang alongside the emitted .ll. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <setjmp.h>

typedef intptr_t Value;

enum { K_CLO = 0, K_CON = 1, K_PAIR = 2, K_STR = 3, K_PTR = 4, K_UNIT = 5, K_BIG = 6, K_FLOAT = 7, K_BYTES = 8 };

typedef struct Obj {
  int kind;
  struct Obj* gc_next;
  int gc_mark;
  Value (*code)(Value, Value*);
  int nenv;
  int tag;
  const char* name;
  int nfield;
  const char* str;
  long handle;
  double dval; /* D3 machine float (f64): a boxed double, kind K_FLOAT. */
  Value slots[1];
} Obj;

#define INT_TAG(n)  (((Value)(n) << 1) | 1)
#define IS_INT(v)   ((v) & 1)
#define INT_VAL(v)  ((long)((v) >> 1))

/* ---- GC: conservative-roots / precise-slots non-moving mark-sweep (== c.go) ---- */
static Obj* gc_objs = 0;
static size_t gc_live_bytes = 0;
static size_t gc_threshold = 0;
#ifndef RUNE_GC_THRESHOLD
#define RUNE_GC_THRESHOLD (8u * 1024u * 1024u)
#endif
#define GC_MAX_STATIC_ROOTS 4096
static Value* gc_static_roots[GC_MAX_STATIC_ROOTS];
static int gc_n_static_roots = 0;
static void* volatile gc_stack_bottom = 0;
static long gc_n_collections = 0;
static void gc_collect(void);

void rt_add_root(Value* slot) {
  if (gc_n_static_roots >= GC_MAX_STATIC_ROOTS) {
    fprintf(stderr, "rune-ll: too many GC static roots\n"); exit(1);
  }
  gc_static_roots[gc_n_static_roots++] = slot;
}

static void* gc_alloc(size_t n) {
  size_t a = sizeof(void*);
  n = (n + a - 1) & ~(a - 1);
  if (gc_threshold == 0) gc_threshold = RUNE_GC_THRESHOLD;
  if (gc_live_bytes + n > gc_threshold && gc_stack_bottom != 0) {
    gc_collect();
    while (gc_live_bytes + n > gc_threshold) gc_threshold *= 2;
  }
  Obj* o = (Obj*)calloc(1, n);
  if (!o) { fprintf(stderr, "rune-ll: out of memory\n"); exit(1); }
  o->gc_next = gc_objs; o->gc_mark = 0;
  gc_objs = o; gc_live_bytes += n;
  return o;
}

static size_t gc_obj_size(Obj* o) {
  size_t base = sizeof(Obj);
  int extra = 0;
  switch (o->kind) {
    case K_CLO:  extra = o->nenv   > 0 ? o->nenv   - 1 : 0; break;
    case K_CON:  extra = o->nfield > 0 ? o->nfield - 1 : 0; break;
    case K_PAIR: extra = 1; break;
    case K_BIG:  extra = o->nenv   > 0 ? o->nenv   - 1 : 0; break; /* nenv = limb capacity */
    case K_BYTES: extra = o->nenv  > 0 ? o->nenv   - 1 : 0; break; /* Phase 0 Bin: 1 byte/slot, nenv = count */
    default:     extra = 0; break;
  }
  size_t sz = base + (size_t)extra * sizeof(Value);
  size_t a = sizeof(void*);
  return (sz + a - 1) & ~(a - 1);
}

static Obj* gc_find_obj(Value v) {
  if (v == 0 || (v & 1)) return 0;
  if ((v & (Value)(sizeof(void*) - 1)) != 0) return 0;
  char* q = (char*)v;
  for (Obj* o = gc_objs; o; o = o->gc_next) {
    char* lo = (char*)o;
    char* hi = lo + gc_obj_size(o);
    if (q >= lo && q < hi) return o;
  }
  return 0;
}

static void gc_mark_value(Value v);
static void gc_mark_obj(Obj* o) {
  if (o->gc_mark) return;
  o->gc_mark = 1;
  switch (o->kind) {
    case K_CLO:  for (int i = 0; i < o->nenv;   i++) gc_mark_value(o->slots[i]); break;
    case K_CON:  for (int i = 0; i < o->nfield; i++) gc_mark_value(o->slots[i]); break;
    case K_PAIR: gc_mark_value(o->slots[0]); gc_mark_value(o->slots[1]); break;
    default: break;
  }
}
static void gc_mark_value(Value v) {
  if (v == 0 || (v & 1)) return;
  gc_mark_obj((Obj*)v);
}

static void gc_scan_range(char* lo, char* hi) {
  if (lo > hi) { char* t = lo; lo = hi; hi = t; }
  while (((uintptr_t)lo % sizeof(void*)) != 0) lo++;
  for (char* p = lo; p + sizeof(Value) <= hi; p += sizeof(void*)) {
    Value v = *(Value*)p;
    Obj* o = gc_find_obj(v);
    if (o) gc_mark_obj(o);
  }
}

static void gc_collect(void) {
  gc_n_collections++;
  jmp_buf regs;
  if (setjmp(regs)) return;
  (void)regs;
  for (int i = 0; i < gc_n_static_roots; i++) gc_mark_value(*gc_static_roots[i]);
  char* sp = (char*)__builtin_frame_address(0);
  char* bottom = (char*)gc_stack_bottom;
  gc_scan_range(sp, bottom);
  gc_scan_range((char*)&regs, (char*)&regs + sizeof(regs));
  Obj** link = &gc_objs;
  while (*link) {
    Obj* o = *link;
    if (o->gc_mark) { o->gc_mark = 0; link = &o->gc_next; }
    else { *link = o->gc_next; gc_live_bytes -= gc_obj_size(o); free(o); }
  }
}

/* ---- the Value rep + the rt_* helpers the emitted LLVM IR calls ---- */
Value UNIT;
static Obj* obj(Value v) { return (Obj*)v; }

Value rt_mkint(long n) { return INT_TAG(n); }
long rt_as_int(Value v) {
  if (IS_INT(v)) return INT_VAL(v);
  fprintf(stderr, "rune-ll: as_int on non-int\n"); exit(1);
}

Value rt_mkclo(Value (*code)(Value, Value*), int nenv) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj) + (nenv > 0 ? (nenv - 1) : 0) * sizeof(Value));
  o->kind = K_CLO; o->code = code; o->nenv = nenv;
  return (Value)o;
}
void rt_clo_set(Value c, int i, Value x) { obj(c)->slots[i] = x; }

Value rt_mkcon(int tag, const char* name, int nfield) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj) + (nfield > 0 ? (nfield - 1) : 0) * sizeof(Value));
  o->kind = K_CON; o->tag = tag; o->name = name; o->nfield = nfield;
  return (Value)o;
}
void rt_con_set(Value c, int i, Value x) { obj(c)->slots[i] = x; }
Value rt_con_get(Value c, int i) { return obj(c)->slots[i]; }
int rt_con_tag(Value c) { return obj(c)->tag; }

Value rt_mkpair(Value a, Value b) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj) + sizeof(Value));
  o->kind = K_PAIR; o->slots[0] = a; o->slots[1] = b;
  return (Value)o;
}
Value rt_pair_fst(Value p) { return obj(p)->slots[0]; }
Value rt_pair_snd(Value p) { return obj(p)->slots[1]; }

Value rt_mkstr(const char* s) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj));
  o->kind = K_STR; o->str = s;
  return (Value)o;
}
Value rt_mkptr(long h) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj));
  o->kind = K_PTR; o->handle = h;
  return (Value)o;
}
/* D3 machine float (f64): box/unbox a double (kind K_FLOAT), shared by the baked
   float/BLAS host bodies appended by EmitRuntimeFor. */
static Value mkfloat(double d) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj));
  o->kind = K_FLOAT; o->dval = d;
  return (Value)o;
}
static double float_val(Value v) { return obj(v)->dval; }
static Value mkunit(void) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj));
  o->kind = K_UNIT;
  return (Value)o;
}

Value rt_apply(Value clo, Value arg) {
  if (IS_INT(clo) || obj(clo)->kind != K_CLO) {
    fprintf(stderr, "rune-ll: apply of non-closure\n"); exit(1);
  }
  Obj* o = obj(clo);
  return o->code(arg, o->slots);
}

/* ---- naive arbitrary-precision integers (builtin-nat); twin of c.go's bignum.
   A nat is a K_BIG object: base-1e9 limbs (little-endian), nfield = logical limb
   count, nenv = allocated capacity (sizes the object). Zero is empty. Naive cut:
   every nat boxed, no immediate-int fast path (later); FFI LitInt stays IS_INT. */
#define BIG_BASE 1000000000L

static Value mkbig(int nlimbs) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj) + (nlimbs > 0 ? (nlimbs - 1) : 0) * sizeof(Value));
  o->kind = K_BIG; o->tag = 0; o->nfield = nlimbs; o->nenv = nlimbs;
  return (Value)o;
}
static int  big_nlimbs(Value v) { return obj(v)->nfield; }
static long big_limb(Value v, int i) { return (long)obj(v)->slots[i]; }
static void big_setlimb(Value v, int i, long x) { obj(v)->slots[i] = (Value)x; }
/* Phase 0 real byte strings (Bin): K_BYTES stores one byte per Value slot,
   nfield = nenv = the byte count (mirrors K_BIG; raw bytes are never traced). */
static Value mkbytes(int n) {
  Obj* o = (Obj*)gc_alloc(sizeof(Obj) + (n > 0 ? (n - 1) : 0) * sizeof(Value));
  o->kind = K_BYTES; o->nfield = n; o->nenv = n;
  return (Value)o;
}
static int  bytes_len(Value v) { return obj(v)->nfield; }
static int  bytes_at(Value v, int i) { return (int)((long)obj(v)->slots[i] & 255); }
static void bytes_set(Value v, int i, int x) { obj(v)->slots[i] = (Value)(long)(x & 255); }
static Value big_norm(Value v) {
  Obj* o = obj(v);
  while (o->nfield > 0 && o->slots[o->nfield - 1] == 0) o->nfield--;
  return v;
}
static int big_cmp(Value a, Value b) {
  int na = big_nlimbs(a), nb = big_nlimbs(b);
  if (na != nb) return na < nb ? -1 : 1;
  for (int i = na - 1; i >= 0; i--) {
    long x = big_limb(a, i), y = big_limb(b, i);
    if (x != y) return x < y ? -1 : 1;
  }
  return 0;
}
static Value big_add(Value a, Value b) {
  int na = big_nlimbs(a), nb = big_nlimbs(b);
  int n = (na > nb ? na : nb) + 1;
  Value r = mkbig(n);
  long carry = 0;
  for (int i = 0; i < n; i++) {
    long s = carry;
    if (i < na) s += big_limb(a, i);
    if (i < nb) s += big_limb(b, i);
    big_setlimb(r, i, s % BIG_BASE);
    carry = s / BIG_BASE;
  }
  return big_norm(r);
}
static Value big_mul(Value a, Value b) {
  int na = big_nlimbs(a), nb = big_nlimbs(b);
  if (na == 0 || nb == 0) return mkbig(0);
  Value r = mkbig(na + nb);
  for (int i = 0; i < na; i++) {
    long carry = 0, ai = big_limb(a, i);
    for (int j = 0; j < nb; j++) {
      long cur = big_limb(r, i + j) + ai * big_limb(b, j) + carry;
      big_setlimb(r, i + j, cur % BIG_BASE);
      carry = cur / BIG_BASE;
    }
    big_setlimb(r, i + nb, big_limb(r, i + nb) + carry);
  }
  return big_norm(r);
}
static Value big_monus(Value a, Value b) {
  if (big_cmp(a, b) <= 0) return mkbig(0);
  int na = big_nlimbs(a), nb = big_nlimbs(b);
  Value r = mkbig(na);
  long borrow = 0;
  for (int i = 0; i < na; i++) {
    long d = big_limb(a, i) - borrow - (i < nb ? big_limb(b, i) : 0);
    if (d < 0) { d += BIG_BASE; borrow = 1; } else borrow = 0;
    big_setlimb(r, i, d);
  }
  return big_norm(r);
}
static void big_print(Value v) {
  int n = big_nlimbs(v);
  if (n == 0) { putchar('0'); return; }
  printf("%ld", big_limb(v, n - 1));
  for (int i = n - 2; i >= 0; i--) printf("%09ld", big_limb(v, i));
}

/* D3: a builtin-nat magnitude as a double (for fromNat: Nat -> Float). */
static double big_to_double(Value v) {
  double d = 0.0;
  for (int i = big_nlimbs(v) - 1; i >= 0; i--) d = d * (double)BIG_BASE + (double)big_limb(v, i);
  return d;
}

/* rt_* entry points the emitted IR calls. */
Value rt_big_from_long(long n) {
  if (n <= 0) return mkbig(0);
  int k = 0; long t = n;
  while (t > 0) { k++; t /= BIG_BASE; }
  Value v = mkbig(k);
  for (int i = 0; i < k; i++) { big_setlimb(v, i, n % BIG_BASE); n /= BIG_BASE; }
  return v;
}
Value rt_big_parse(const char* s) {
  int len = (int)strlen(s);
  int k = (len + 8) / 9;
  Value v = mkbig(k);
  int idx = 0, end = len;
  while (end > 0) {
    int start = end - 9; if (start < 0) start = 0;
    long limb = 0;
    for (int i = start; i < end; i++) limb = limb * 10 + (s[i] - '0');
    big_setlimb(v, idx++, limb);
    end = start;
  }
  return big_norm(v);
}
/* Schoolbook long division (base-1e9), MS-limb first with per-limb binary search.
   Returns quotient; *rem receives remainder. Div by zero: q=0, r=a. */
static Value big_divmod(Value a, Value b, Value* rem) {
  if (big_nlimbs(b) == 0) { *rem = a; return rt_big_from_long(0); }
  if (big_cmp(a, b) < 0) { *rem = a; return rt_big_from_long(0); }
  int n = big_nlimbs(a);
  Value q = mkbig(n);
  Value r = rt_big_from_long(0);
  Value base = rt_big_from_long(BIG_BASE);
  for (int i = n - 1; i >= 0; i--) {
    r = big_add(big_mul(r, base), rt_big_from_long(big_limb(a, i)));
    long lo = 0, hi = BIG_BASE - 1, d = 0;
    while (lo <= hi) {
      long mid = lo + (hi - lo) / 2;
      if (big_cmp(big_mul(b, rt_big_from_long(mid)), r) <= 0) { d = mid; lo = mid + 1; }
      else hi = mid - 1;
    }
    big_setlimb(q, i, d);
    r = big_monus(r, big_mul(b, rt_big_from_long(d)));
  }
  *rem = big_norm(r);
  return big_norm(q);
}
static Value big_div(Value a, Value b) { Value r; return big_divmod(a, b, &r); }
static Value big_mod(Value a, Value b) { Value r; big_divmod(a, b, &r); return r; }
Value rt_big_succ(Value a) { return big_add(a, rt_big_from_long(1)); }
long  rt_big_cmp(Value a, Value b) { return big_cmp(a, b); }

Value rt_nat_add(Value a, Value b) { return big_add(a, b); }
Value rt_nat_mul(Value a, Value b) { return big_mul(a, b); }
Value rt_nat_monus(Value a, Value b) { return big_monus(a, b); }
Value rt_nat_div(Value a, Value b) { return big_div(a, b); }
Value rt_nat_mod(Value a, Value b) { return big_mod(a, b); }

void rt_abort(void) { fprintf(stderr, "rune-ll: impossible (unmatched constructor tag)\n"); exit(1); }

/* show: byte-identical to the C backend's show (same surface rendering). */
static void show(Value v);
static void show_paren(Value v) {
  if (!IS_INT(v) && obj(v)->kind == K_CON && obj(v)->nfield > 0) {
    Obj* o = obj(v); int shown = 0;
    for (int i = 0; i < o->nfield; i++) if (o->slots[i] != UNIT) shown++;
    if (shown > 0) { putchar('('); show(v); putchar(')'); return; }
  }
  show(v);
}
static void show(Value v) {
  if (v == UNIT) { printf("()"); return; }
  if (IS_INT(v)) { printf("%ld", INT_VAL(v)); return; }
  Obj* o = obj(v);
  switch (o->kind) {
    case K_BIG: big_print(v); return;
    case K_BYTES: { int bn = bytes_len(v); putchar('"'); for (int bi = 0; bi < bn; bi++) { int bx = bytes_at(v, bi); if (bx >= 0x20 && bx < 0x7f && bx != 0x22 && bx != 0x5c) putchar(bx); else printf("\\x%02x", bx); } putchar('"'); return; }
    case K_FLOAT: printf("%g", o->dval); return;
    case K_CLO: printf("<function>"); return;
    case K_PTR: printf("<ptr>"); return;
    case K_STR: printf("%s", o->str); return;
    case K_PAIR: {
      putchar('('); show(o->slots[0]); printf(", "); show(o->slots[1]); putchar(')');
      return;
    }
    case K_CON: {
      printf("%s", o->name);
      for (int i = 0; i < o->nfield; i++) {
        if (o->slots[i] == UNIT) continue;
        putchar(' ');
        show_paren(o->slots[i]);
      }
      return;
    }
    default: printf("()"); return;
  }
}
void rt_show_line(Value v) { show(v); putchar('\n'); }
`

// llRuntimeQuot is the erased quotient builtin group with EXTERNAL linkage (the
// twin of cQuotRuntime). Always linked; harmless when unreferenced.
const llRuntimeQuot = `
/* quotient builtins (erased), external linkage. */
static Value quot_qin2(Value arg, Value* env) { (void)env; return arg; }
static Value quot_qin1(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qin2, 0); }
static Value quot_qin0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qin1, 0); }
Value def_qin(void) { return rt_mkclo(&quot_qin0, 0); }
static Value quot_qlift5(Value arg, Value* env) { return rt_apply(env[0], arg); }
static Value quot_qlift4(Value arg, Value* env) { (void)arg; Value c = rt_mkclo(&quot_qlift5, 1); rt_clo_set(c, 0, env[0]); return c; }
static Value quot_qlift3(Value arg, Value* env) { (void)env; Value c = rt_mkclo(&quot_qlift4, 1); rt_clo_set(c, 0, arg); return c; }
static Value quot_qlift2(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qlift3, 0); }
static Value quot_qlift1(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qlift2, 0); }
static Value quot_qlift0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qlift1, 0); }
Value def_qlift(void) { return rt_mkclo(&quot_qlift0, 0); }
static Value quot_unit5(Value arg, Value* env) { (void)arg; (void)env; return UNIT; }
static Value quot_qsound4(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_unit5, 0); }
static Value quot_qsound3(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qsound4, 0); }
static Value quot_qsound2(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qsound3, 0); }
static Value quot_qsound1(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qsound2, 0); }
static Value quot_qsound0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qsound1, 0); }
Value def_qsound(void) { return rt_mkclo(&quot_qsound0, 0); }
static Value quot_qind4(Value arg, Value* env) { (void)arg; (void)env; return UNIT; }
static Value quot_qind3(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qind4, 0); }
static Value quot_qind2(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qind3, 0); }
static Value quot_qind1(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qind2, 0); }
static Value quot_qind0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&quot_qind1, 0); }
Value def_qind(void) { return rt_mkclo(&quot_qind0, 0); }
`

// llRuntimeIO is the erased IO monad group with EXTERNAL linkage (twin of
// cIORuntime). Always linked; harmless when unreferenced.
const llRuntimeIO = `
/* IO monad builtins (erased), external linkage. */
static Value io_pure2(Value arg, Value* env) { (void)arg; return env[0]; }
static Value io_pure1(Value arg, Value* env) { (void)env; Value c = rt_mkclo(&io_pure2, 1); rt_clo_set(c, 0, arg); return c; }
static Value io_pure0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&io_pure1, 0); }
Value def_pureIO(void) { return rt_mkclo(&io_pure0, 0); }
static Value io_bind4(Value arg, Value* env) { (void)arg; return rt_apply(rt_apply(env[1], rt_apply(env[0], UNIT)), UNIT); }
static Value io_bind3(Value arg, Value* env) { Value c = rt_mkclo(&io_bind4, 2); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, arg); return c; }
static Value io_bind2(Value arg, Value* env) { (void)env; Value c = rt_mkclo(&io_bind3, 1); rt_clo_set(c, 0, arg); return c; }
static Value io_bind1(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&io_bind2, 0); }
static Value io_bind0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&io_bind1, 0); }
Value def_bindIO(void) { return rt_mkclo(&io_bind0, 0); }
`

// llRuntimeMain is the C entrypoint the emitted module's rune_main is called from:
// it captures the GC stack bottom, seeds + roots UNIT, then calls rune_main (which
// the .ll defines). Optional GC stats on stderr (-DRUNE_GC_STATS) mirror c.go so a
// test can confirm the collector fired.
const llRuntimeMain = `
extern void rune_main(void);
int main(void) {
  void* gc_anchor = 0; gc_stack_bottom = (void*)&gc_anchor;
  UNIT = mkunit();
  rt_add_root(&UNIT);
  rune_main();
#ifdef RUNE_GC_STATS
  fprintf(stderr, "rune-ll: gc collections=%ld\n", gc_n_collections);
#else
  (void)gc_n_collections;
#endif
  return 0;
}
`
