# Task 3 Report: Native bible higher-order ops (foldLines/foldDir) on C + LLVM

## Status: DONE

## What was implemented

### Files modified

- `codegen/c.go` -- `emitStreamPrimsC`: added foldLines + foldDir bodies (C static closures + apply ABI)
- `codegen/ll.go` -- `emitStreamPrimsLL`: added LLVM twins with rt_ prefix and external accessor linkage
- `harness/bible_conformance_test.go`: added `TestBibleNativeFold`; fixed `runNativeListing` cwd semantics

---

## foldLines segment logic (clean form used)

The brief flagged the convoluted stub in its draft and said to use the clean form. The
implementation in both backends (C and LLVM) is:

```c
/* slurp file into data[0..n-1] via fgetc loop */
size_t nseg = 0, seg = 0;
size_t* st = (size_t*)malloc(sizeof(size_t)*(n+2));
size_t* en = (size_t*)malloc(sizeof(size_t)*(n+2));
for (size_t k = 0; k <= n; k++) {
    if (k == n || data[k] == '\n') { st[nseg]=seg; en[nseg]=k; nseg++; seg=k+1; }
}
if (nseg > 0 && en[nseg-1] == st[nseg-1]) nseg--;   /* drop single trailing empty */
Value s = s0;
for (size_t k = 0; k < nseg; k++) {
    Value line = d6_h2s(data + st[k], en[k]-st[k]);
    s = apply(apply(apply(step, s), line), UNIT);   /* rt_apply in LLVM */
}
free(st); free(en); free(data);
return s;
```

Splits on `\n` ONLY. `\r` is kept. A single trailing empty segment is dropped iff the
last character was `\n`. Matches `data.split(b'\n')` + drop-trailing-empty in rust.go:207.
Verified with ch560: "alpha\r"(6) + "beta\r"(5) + "gamma"(5) = 16.

---

## d6_foldwalk structure

```
d6_namecmp: strcmp on char** elements (byte order, ASCII corpus)
d6_foldwalk(dir, sfx, sfxlen, step, s):
  opendir(dir) -- return s on failure
  collect names via readdir, skip "." and ".."
  strdup each into growable char** array
  closedir(dp)
  qsort by d6_namecmp (strcmp byte order = filepath.WalkDir on ASCII)
  for each name (sorted):
    build full = "dir/name" (malloc dl+nl+2, memcpy+slash+memcpy)
    stat(full, &sb)
    if S_ISDIR: recurse d6_foldwalk(full, ...) [depth-first pre-order]
    else if suffix match (memcmp last sfxlen bytes):
      slurp file -> content via fgetc loop
      s = apply(apply(apply(step, s), content), UNIT)
      free(bd)
    free(full); free(names[i])
  free(names); return s
```

Walk order matches `filepath.WalkDir` (per-dir lexical, depth-first). Non-ASCII filenames
would diverge (strcmp byte order vs UTF-8 NFC) -- parked, no consumer.

---

## Includes added

### C backend (`emitStreamPrimsC`, inside `if usesForeign(p, "foldDir")` block)

```c
#include <dirent.h>
#include <sys/stat.h>
```

Emitted inline following the math.h/cblas.h precedent (c.go:986/1004). Header guards
make the `<sys/stat.h>` repeat from `emitFSPrimsC` idempotent.

### LLVM backend (`emitStreamPrimsLL`, inside `if usesForeign(p, "foldDir")` block)

Same two includes appended to runtime.c. `emitStreamPrimsLL` runs before `emitFSPrimsLL`
(ll.go:220/223); if the Bin-based FS ops also appear, `<sys/stat.h>` is emitted twice --
idempotent via header guards. The base llRuntimeC (ll_runtime.go) already has
stdio.h/stdlib.h/string.h; dirent.h is new.

---

## C/LLVM delta rule applied

| C backend | LLVM backend |
|---|---|
| `mkclo` | `rt_mkclo` |
| `clo_set` | `rt_clo_set` |
| `apply(clo, arg)` | `rt_apply(clo, arg)` |
| `static Value foldLines(void)` | `Value foldLines(void)` (external linkage) |
| `static Value foldDir(void)` | `Value foldDir(void)` (external linkage) |
| d6_namecmp, d6_foldwalk, _c1.._c5/_c1.._c6 | static in both |
| d6_s2h, d6_h2s | static in both (emitted by codec block) |
| UNIT, big_cmp, big_add, big_mul, big_divmod, big_nlimbs, big_limb | unchanged in both |

---

## Verification runs

### foldLines -- ch549_conllu_count.rune main (cwd=testdata)
```
[c]  -> 11\n11  PASS
[ll] -> 11\n11  PASS
```

### foldDir -- ch554_fold_dir.rune main (cwd=testdata)
```
[c]  -> 3\n3    PASS
[ll] -> 3\n3    PASS
```

### CRLF lock -- ch560_crlf_lines.rune main (cwd=testdata)
```
[c]  -> 16\n16  PASS
[ll] -> 16\n16  PASS
```

TestBibleNativeFold: PASS (2.3s, both c + ll, all 3 cases).

### Regression check

TestBibleConformanceFold, TestBibleConformanceCRLF, TestBibleJVMPureFold, TestBibleNativePure:
all PASS (16.3s total).

---

## Gate result

`go build ./...` clean. `go test ./harness/ -run TestBibleNativeFold` PASS.

---

## Concerns

1. **runNativeListing cwd fix**: The brief specified `cwd: "testdata"` in the test struct,
   but the Task-1 implementation of `runNativeListing` joined "testdata" with the cwd via
   `filepath.Join("testdata", cwd)`, giving "testdata/testdata" which does not exist.
   Changed the function to use `cwd` directly (consistent with `runJVMListing`). Updated
   the comment. No existing caller used a non-empty cwd, so no behavior change for
   existing tests.

2. **Non-ASCII filenames in foldDir**: `strcmp` byte order diverges from Go's UTF-8 NFC
   lexicographic order for non-ASCII filenames. The Bible corpus uses ASCII-only filenames
   so this is moot. Noted in the code comment.

3. **Double include of `<sys/stat.h>`**: When a program uses both foldDir and Bin-based
   FS ops (fsWrite/fsRead etc.), `<sys/stat.h>` is emitted twice in the output. C header
   guards make this harmless.
