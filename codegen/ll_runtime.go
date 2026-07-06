package codegen

// ll_runtime.go — the LLVM backend's module preamble + runtime-helper declares,
// and the linkable C runtime (the external-linkage twin of cRuntime in c.go). The
// emitted `.ll` is genuine LLVM IR for the program LOGIC; the tagged-word Value
// rep, the ARC runtime (retain/release), `apply`, and `$show` live in this C shim, compiled and
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
declare i64 @rt_mkbounce(i64, i32)
declare void @rt_bounce_set(i64, i32, i64)
declare i64 @rt_tramp(i64)
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
declare void @rt_retain(i64)
declare void @rt_release(i64)
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

// llRuntimeC is the linkable C runtime: the SAME tagged-word Value rep + ARC
// (Perceus-style reference counting) + apply/show as cRuntime, but with EXTERNAL
// `rt_*` wrappers the emitted LLVM IR calls, and an external UNIT global. Kept
// byte-compatible with the C backend's rep so $show is byte-identical. (Distinct
// symbol namespace `rt_*` so it never clashes with c.go's `static` runtime if both
// ever shared a TU — they do not.)
const llRuntimeC = `/* rune llvm-backend runtime — the external-linkage twin of the C backend's
   embedded runtime. Compiled and linked by clang alongside the emitted .ll. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef intptr_t Value;

enum { K_CLO = 0, K_CON = 1, K_PAIR = 2, K_STR = 3, K_PTR = 4, K_UNIT = 5, K_BIG = 6, K_FLOAT = 7, K_BYTES = 8, K_BOUNCE = 9 };

typedef struct Obj {
  int kind;
  /* ARC reference count; 1 at birth (arc_alloc). rt_retain increments,
     rt_release decrements and frees the block (per-kind free walker) at zero. */
  long rc;
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

/* ===========================================================================
   ARC — Perceus-style reference counting (== c.go's cRuntime ARC core; the
   spec's docs/superpowers/specs/2026-07-05-native-arc-design.md Decision 3/4).
   THE TWIN'S LINKAGE: rt_retain / rt_release / rt_live are EXTERN here (the
   emitted .ll calls them), where c.go's are static; arc_alloc / rt_counted /
   arc_report stay static (only the C runtime calls them). Ownership rules
   (PATH B): apply() consumes both operands; the mk* builders take ownership of
   their slot arguments; thunk-cache reads (CGlobal) and foreign accessors are
   BORROWED; foreign prim bodies receive borrowed args and return owned results.
   rune heap values are immutable and acyclic, so counting is complete — no
   cycle collector.
   =========================================================================== */
long rt_live = 0;

#ifdef RUNE_ARC_REPORT
static void arc_report(void) { fprintf(stderr, "rt_live=%ld\n", rt_live); }
#endif

static void* arc_alloc(size_t n) {
  /* align to pointer size so the int low-bit tag is never set on a real pointer */
  size_t a = sizeof(void*);
  n = (n + a - 1) & ~(a - 1);
  /* ZERO the whole object: the emitted code allocates an object then fills its
     slots one at a time; a zeroed header/slot reads as null, so a half-built
     object never exposes a wild pointer to the free walker. */
  Obj* o = (Obj*)calloc(1, n);
  if (!o) { fprintf(stderr, "rune-ll: out of memory\n"); exit(1); }
  o->rc = 1;
  rt_live++;
  return o;
}

Value UNIT;                  /* the boxed singleton made in main (extern: the .ll references @UNIT) */

static Obj* obj(Value v) { return (Obj*)v; }

/* rt_counted: is v a counted heap object? Immediates, null, and the UNIT
   singleton (a never-released global) are not. */
static int rt_counted(Value v) {
  if (v == 0 || IS_INT(v) || v == UNIT) return 0;
  return 1;
}

void rt_retain(Value v) {
  if (!rt_counted(v)) return;
  obj(v)->rc++;
}

void rt_release(Value v) {
  if (!rt_counted(v)) return;
  Obj* o = obj(v);
  if (--o->rc > 0) return;
  /* rc hit zero: release children per kind, then free the block. K_STR (->str is a
     literal / emitter buffer we never own), K_PTR, K_UNIT, K_FLOAT are leaves; K_BIG /
     K_BYTES hold limbs / bytes INLINE in slots[] (raw words, never Values), freed with
     the block. */
  switch (o->kind) {
    case K_CLO:
      for (int i = 0; i < o->nenv; i++) rt_release(o->slots[i]);
      break;
    case K_CON:
      for (int i = 0; i < o->nfield; i++) rt_release(o->slots[i]);
      break;
    case K_PAIR:
      rt_release(o->slots[0]); rt_release(o->slots[1]);
      break;
    case K_BOUNCE:
      /* slot[0] is the BORROWED cached _step root (mkbounce stores it WITHOUT a retain,
         matching WASM $rt_mkbounce). Release only the OWNED args (slots[1..nfield-1]);
         the WASM $rt_free K_BOUNCE arm skips the step likewise (base = arg slots). A
         bounce forced through tramp is shell-freed (args moved) and never reaches here;
         one dropped as a dead value (e.g. an unused IH bound to a bounce) frees its
         args but must leave the borrowed step alone. */
      for (int i = 1; i < o->nfield; i++) rt_release(o->slots[i]);
      break;
    default: break;
  }
  rt_live--;
  free(o);
}

Value rt_mkint(long n) { return INT_TAG(n); }
long rt_as_int(Value v) {
  if (IS_INT(v)) return INT_VAL(v);
  fprintf(stderr, "rune-ll: as_int on non-int\n"); exit(1);
}

Value rt_mkclo(Value (*code)(Value, Value*), int nenv) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj) + (nenv > 0 ? (nenv - 1) : 0) * sizeof(Value));
  o->kind = K_CLO; o->code = code; o->nenv = nenv;
  return (Value)o;
}
void rt_clo_set(Value c, int i, Value x) { obj(c)->slots[i] = x; }

Value rt_mkcon(int tag, const char* name, int nfield) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj) + (nfield > 0 ? (nfield - 1) : 0) * sizeof(Value));
  o->kind = K_CON; o->tag = tag; o->name = name; o->nfield = nfield;
  return (Value)o;
}
void rt_con_set(Value c, int i, Value x) { obj(c)->slots[i] = x; }
Value rt_con_get(Value c, int i) { return obj(c)->slots[i]; }
int rt_con_tag(Value c) { return obj(c)->tag; }

Value rt_mkpair(Value a, Value b) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj) + sizeof(Value));
  o->kind = K_PAIR; o->slots[0] = a; o->slots[1] = b;
  return (Value)o;
}
Value rt_pair_fst(Value p) { return obj(p)->slots[0]; }
Value rt_pair_snd(Value p) { return obj(p)->slots[1]; }

Value rt_mkstr(const char* s) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj));
  o->kind = K_STR; o->str = s;
  return (Value)o;
}
Value rt_mkptr(long h) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj));
  o->kind = K_PTR; o->handle = h;
  return (Value)o;
}
/* D3 machine float (f64): box/unbox a double (kind K_FLOAT), shared by the baked
   float/BLAS host bodies appended by EmitRuntimeFor. */
static Value mkfloat(double d) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj));
  o->kind = K_FLOAT; o->dval = d;
  return (Value)o;
}
static double float_val(Value v) { return obj(v)->dval; }
static Value mkunit(void) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj));
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

/* T2 trampoline (twin of c.go's mkbounce/tramp). A partial's marked tail call
   builds a K_BOUNCE {step, args...}; the public driver forces the chain so deep
   tail recursion runs in O(1) native stack. */
Value rt_mkbounce(Value step, int nargs) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj) + (size_t)nargs * sizeof(Value));
  o->kind = K_BOUNCE; o->tag = nargs; o->nfield = nargs + 1; o->slots[0] = step;
  return (Value)o;
}
void rt_bounce_set(Value bnc, int i, Value x) { obj(bnc)->slots[i] = x; }
/* shell-free: reclaim the bounce object WITHOUT releasing the args (moved into the
   step applies) or the step (slots[0], borrowed). == c.go's bounce_free_shell (the
   dual of WASM's $rt_bounce_free_shell). */
static void bounce_free_shell(Value bnc) { Obj* o = obj(bnc); rt_live--; free(o); }
/* tramp: force the bounce chain under ARC (== c.go's tramp; WASM's $rt_tramp). Re-apply
   the BORROWED step (slots[0]) to each OWNED arg (slots[1..tag], MOVED into the applies).
   Each apply but the first builds an owned intermediate closure that nothing else frees --
   RELEASE the previous intermediate before the next apply (i>1; the step at i==1 is
   borrowed, not released). Then shell-free the spent bounce (args moved, step borrowed)
   and continue. O(1) native stack. */
Value rt_tramp(Value v) {
  while (!IS_INT(v) && obj(v)->kind == K_BOUNCE) {
    Obj* o = obj(v);
    Value f = o->slots[0];
    for (int i = 1; i <= o->tag; i++) {
      Value next = rt_apply(f, o->slots[i]);
      if (i > 1) rt_release(f); /* release the previous intermediate (not the borrowed step) */
      f = next;
    }
    bounce_free_shell(v);
    v = f;
  }
  return v;
}

/* ---- naive arbitrary-precision integers (builtin-nat); twin of c.go's bignum.
   A nat is a K_BIG object: base-1e9 limbs (little-endian), nfield = logical limb
   count, nenv = allocated capacity (sizes the object). Zero is empty. Naive cut:
   every nat boxed, no immediate-int fast path (later); FFI LitInt stays IS_INT. */
#define BIG_BASE 1000000000L

static Value mkbig(int nlimbs) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj) + (nlimbs > 0 ? (nlimbs - 1) : 0) * sizeof(Value));
  o->kind = K_BIG; o->tag = 0; o->nfield = nlimbs; o->nenv = nlimbs;
  return (Value)o;
}
static int  big_nlimbs(Value v) { return obj(v)->nfield; }
static long big_limb(Value v, int i) { return (long)obj(v)->slots[i]; }
static void big_setlimb(Value v, int i, long x) { obj(v)->slots[i] = (Value)x; }
/* Phase 0 real byte strings (Bin): K_BYTES stores one byte per Value slot,
   nfield = nenv = the byte count (mirrors K_BIG; raw bytes are never traced). */
static Value mkbytes(int n) {
  Obj* o = (Obj*)arc_alloc(sizeof(Obj) + (n > 0 ? (n - 1) : 0) * sizeof(Value));
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
/* an independent (fresh, owned) copy of a normed bignum's magnitude. */
static Value big_copy(Value a) {
  int n = big_nlimbs(a);
  Value r = mkbig(n);
  for (int i = 0; i < n; i++) big_setlimb(r, i, big_limb(a, i));
  return r;
}
/* Small-divisor fast path (== c.go's big_divmod_small / wasm $big_divmod_small):
   ONE descending-limb pass with an i64 running remainder -- cur = rem*1e9 + limb;
   q_limb = cur/d; rem = cur%d -- instead of the general path's per-limb binary
   search over allocating big_mul/big_cmp temporaries. d is a single base-1e9 limb
   (< 1e9), so cur fits an unsigned 64-bit word. Returns the quotient (fresh, rc=1,
   big_norm'd, no temps); *rem_out receives the scalar remainder (< d) to box. */
static Value big_divmod_small(Value a, long d, long* rem_out) {
  int n = big_nlimbs(a);
  Value q = mkbig(n);                      /* arc_alloc zero-inits the limbs */
  unsigned long rem = 0, dd = (unsigned long)d;
  for (int i = n - 1; i >= 0; i--) {
    unsigned long cur = rem * (unsigned long)BIG_BASE + (unsigned long)big_limb(a, i);
    big_setlimb(q, i, (long)(cur / dd));
    rem = cur % dd;
  }
  *rem_out = (long)rem;
  return big_norm(q);
}
/* ARC discipline (== c.go's big_divmod): q and *rem are FRESH owned results that never
   alias an input, and every internal K_BIG temporary is released. The pre-ARC version
   set *rem = a in the short-circuit arms (aliasing the input) and never freed its loop
   temps -- under ARC that made big_mod x y (x<y) return the OPERAND, which the discard
   release in big_div/big_mod then freed out from under the result (a use-after-free),
   plus a per-call temp leak. */
static Value big_divmod(Value a, Value b, Value* rem) {
  if (big_nlimbs(b) == 0) { *rem = big_copy(a); return rt_big_from_long(0); }
  /* Small-divisor fast path (== c.go): one descending-limb pass, no temps. The
     general binary-search path below serves multi-limb divisors only. Subsumes the
     a<b short-circuit for single-limb b (that pass yields q=0, rem=a). */
  if (big_nlimbs(b) <= 1) {
    long rr; Value q = big_divmod_small(a, big_limb(b, 0), &rr);
    *rem = rt_big_from_long(rr);
    return q;
  }
  if (big_cmp(a, b) < 0) { *rem = big_copy(a); return rt_big_from_long(0); }
  int n = big_nlimbs(a);
  Value q = mkbig(n);
  Value r = rt_big_from_long(0);
  Value base = rt_big_from_long(BIG_BASE);
  for (int i = n - 1; i >= 0; i--) {
    Value t1 = big_mul(r, base), t2 = rt_big_from_long(big_limb(a, i)), rn = big_add(t1, t2);
    rt_release(t1); rt_release(t2); rt_release(r); r = rn;
    long lo = 0, hi = BIG_BASE - 1, d = 0;
    while (lo <= hi) {
      long mid = lo + (hi - lo) / 2;
      Value m1 = rt_big_from_long(mid), m2 = big_mul(b, m1);
      int cmp = big_cmp(m2, r);
      rt_release(m1); rt_release(m2);
      if (cmp <= 0) { d = mid; lo = mid + 1; } else hi = mid - 1;
    }
    big_setlimb(q, i, d);
    Value dd = rt_big_from_long(d), bd = big_mul(b, dd), rr = big_monus(r, bd);
    rt_release(dd); rt_release(bd); rt_release(r); r = rr;
  }
  rt_release(base);
  *rem = big_norm(r);
  return big_norm(q);
}
/* big_div returns the quotient and DISCARDS the (now fresh, owned) remainder -- release
   it. big_mod returns the remainder and discards the quotient -- release that. */
static Value big_div(Value a, Value b) { Value r; Value q = big_divmod(a, b, &r); rt_release(r); return q; }
static Value big_mod(Value a, Value b) { Value r; Value q = big_divmod(a, b, &r); rt_release(q); return r; }
/* big_succ: a+1. big_add reads both inputs and returns a fresh K_BIG; the "1" we
   allocate is OURS to own, so release it once big_add has consumed it (== c.go). */
Value rt_big_succ(Value a) { Value one = rt_big_from_long(1); Value r = big_add(a, one); rt_release(one); return r; }
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
static Value quot_qlift4(Value arg, Value* env) { (void)arg; Value c = rt_mkclo(&quot_qlift5, 1); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); return c; }
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
/* IO monad builtins (erased), external linkage. ARC-aligned with c.go's cIORuntime /
   the WASM twins: the curry intermediates (io_pure1, io_bind1..3) are ORDINARY closures
   the generic apply path RELEASES after each call, so a forwarded env capture must be
   RETAINED into the child; def_pureIO/def_bindIO are MEMOIZED cached roots (borrowed by
   the pass). io_pure2 RETAINS + returns its captured value as an independent owned
   reference (io_bind4 consumes it); io_bind4 runs m, feeds its value to k, runs the
   resulting action act, RELEASES act (reclaim its env), and yields r -- valid because
   every print-and-return prim body / fold accumulator RETAINS its result (Task 3 sweep),
   so r is independent. */
static Value io_pure2(Value arg, Value* env) { (void)arg; rt_retain(env[0]); return env[0]; }
static Value io_pure1(Value arg, Value* env) { (void)env; Value c = rt_mkclo(&io_pure2, 1); rt_clo_set(c, 0, arg); return c; }
static Value io_pure0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&io_pure1, 0); }
Value def_pureIO(void) { static int done=0; static Value cache=0; if(done) return cache; done=1; cache=rt_mkclo(&io_pure0, 0); return cache; }
static Value io_bind4(Value arg, Value* env) { (void)arg; /* env = {m, k} */
  Value v = rt_apply(env[0], UNIT);
  Value act = rt_apply(env[1], v);
  Value r = rt_apply(act, UNIT);
  rt_release(act);
  return r;
}
static Value io_bind3(Value arg, Value* env) { Value c = rt_mkclo(&io_bind4, 2); rt_retain(env[0]); rt_clo_set(c, 0, env[0]); rt_clo_set(c, 1, arg); return c; }
static Value io_bind2(Value arg, Value* env) { (void)env; Value c = rt_mkclo(&io_bind3, 1); rt_clo_set(c, 0, arg); return c; }
static Value io_bind1(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&io_bind2, 0); }
static Value io_bind0(Value arg, Value* env) { (void)arg; (void)env; return rt_mkclo(&io_bind1, 0); }
Value def_bindIO(void) { static int done=0; static Value cache=0; if(done) return cache; done=1; cache=rt_mkclo(&io_bind0, 0); return cache; }
`

// llRuntimeMain is the C entrypoint the emitted module's rune_main is called from:
// it seeds UNIT, then calls rune_main (which the .ll defines). Under ARC there is no
// GC stack bottom or root registration; the optional ARC leak report (-DRUNE_ARC_REPORT)
// registers arc_report as an atexit hook, mirroring c.go's emitted main.
const llRuntimeMain = `
extern void rune_main(void);
int main(void) {
  UNIT = mkunit();
#ifdef RUNE_ARC_REPORT
  atexit(arc_report);
#endif
  rune_main();
  return 0;
}
`

// llRuntimeMainArgv is the argc/argv variant of llRuntimeMain, used when the
// program references argCountCode or argAtCode. The rune_argc/rune_argv globals
// are declared (as static) by emitStreamPrimsLL before this main, so we just
// assign them here. The rest of the body is identical to llRuntimeMain.
const llRuntimeMainArgv = `
extern void rune_main(void);
int main(int argc, char** argv) {
  rune_argc = argc; rune_argv = argv;
  UNIT = mkunit();
#ifdef RUNE_ARC_REPORT
  atexit(arc_report);
#endif
  rune_main();
  return 0;
}
`

// llRuntimeMainFloat is llRuntimeMain with setlocale(LC_NUMERIC, "C") pinned
// at the top of main. Emitted instead of llRuntimeMain when the program uses
// float IO prims (parseFloat/getFloat/printFloat); locale.h is already
// #included by emitFloatIOPrimsLL which appears earlier in the same runtime.c.
const llRuntimeMainFloat = `
extern void rune_main(void);
int main(void) {
  setlocale(LC_NUMERIC, "C");
  UNIT = mkunit();
#ifdef RUNE_ARC_REPORT
  atexit(arc_report);
#endif
  rune_main();
  return 0;
}
`

// llRuntimeMainFloatArgv is the argc/argv+float variant (both setlocale and
// argc/argv capture), for programs that use float IO prims AND argCountCode/
// argAtCode.
const llRuntimeMainFloatArgv = `
extern void rune_main(void);
int main(int argc, char** argv) {
  rune_argc = argc; rune_argv = argv;
  setlocale(LC_NUMERIC, "C");
  UNIT = mkunit();
#ifdef RUNE_ARC_REPORT
  atexit(arc_report);
#endif
  rune_main();
  return 0;
}
`
