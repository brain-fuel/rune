# R-INTEROP — Math/Stats/AI/ML/Plotting interop (bind, don't reimplement)

> Roadmap node D4 (`D4 [I] Math/Stats/AI/ML/Plotting INTEROP (bind, contract-guard)
> ⇐ R-INTEROP, B4, D3`). The clause of telos 3 — "parity with … Python's
> numeric/ML/plotting reach (via interop)" — and the interop research tail R-FFI
> explicitly parks (`ref_docs/wootz/R-FFI.md:222,504,575`). It builds *directly* on
> R-FFI's three machinery: the `store/foreign.go` bodiless-axiom group, the
> three-tier discharge/assume/guard policy, and the assumption-tracking `A`-set
> cert key. It adds **one new thing R-FFI deliberately left as research**: how a
> NumPy/BLAS *array* — a heap of foreign numbers with a shape, a dtype, and an
> owner — is **typed in Rune**, marshalled across the four backend classes
> (BEAM/native-C/JVM/WASM), and **contract-guarded** (shape/dtype/finiteness/
> ownership) without reimplementing the math. The teachable artifact is a small ML
> + plotting pipeline (load → standardize → least-squares fit → scatter+line PNG)
> that runs through bound NumPy/BLAS/Matplotlib with every foreign edge carrying a
> Rune contract.

## Build status — first slice LANDED (D4 opened, v3.29.0–v3.31.0, 2026-06-20)

The full design below (the `Array dt sh` shape-indexed handle, the `CArray`
`CRepr`, embed-vs-port CPython hosting) remains the target. What has actually
**shipped** is the *contract-guarded interop recipe* on the substrate that exists
today — the proof that "bind, don't reimplement, and know what you trust" works
end to end, before the heavier `Array` machinery lands:

- **The capability shape.** A numeric op is ONE Rune `foreign` axiom with
  **per-backend impls**: the *native gift* where a real library is present, a
  *portable reference floor* everywhere else. Implemented:
  - `npDot` (ch221): **real NumPy** (`numpy.dot`) on the **py** backend, **OpenBLAS**
    (`cblas_ddot`) on **C/LLVM**, a reference loop on js/go/erl/rust.
  - `npMean` (ch222): **`numpy.mean`** on py; a hand sum/count floor elsewhere (no BLAS).
  - `npMatSum` (ch223): **`numpy` matmul** `(A@B).sum()` on py (2-D reshape — the
    both-ways marshalling boundary), `cblas_dgemm` on C/LLVM, a triple loop elsewhere.
- **The trust discipline, realized.** Each is bound behind a **tolerance contract**
  written in-language: run the foreign impl AND an in-language reference fold, accept
  the library result only `withinTol eps` of the reference, else **blame** it
  (`Result Float Blame`). This is R-FFI's **guard tier** with the postcondition
  written explicitly (the `with post guard` *surface sugar* is still unbuilt — D3
  tail). The library is **assumed**, but never trusted blind: a divergent result is
  caught at the boundary, not propagated. (Lambert.)
- **Parity of the CONTRACT, not the bits.** All three are byte-identical across the
  seven backends (`TestD4NumpyInterop`/`Mean`/`Matmul`), because the observable is the
  *guarded verdict*, not the raw float — NumPy, OpenBLAS, and the reference loop all
  satisfy the same contract. This is the parity model the design assumes.
- **The recipe (Savage on-ramp).** Adding a third-party function is now uniform:
  *pick the op, write its in-language reference, guard it.* ch221→222→223 walk that
  recipe over 1-D, reduction, and 2-D ops.

**Divergence from the full design, honestly.** The shipped slice marshals a **flat
`FList` of `Float`** (the ch219 array boundary) + explicit `Nat` dims, NOT the
`Array dt sh` shape-indexed handle — so shapes are **guarded** (the reference is
recomputed and compared), not yet **discharged** by dependent types. There is no
`CArray` `CRepr`, no `DType`/`Shape` kit, no embedded/port CPython host model (py
interop is the emitted-source calling `import numpy` in-process; native is the C-ABI
`cblas` only, no CPython). Those remain the design's targets below. What the slice
proves is that the **contract-and-blame spine** is sound and teachable on the current
substrate; the `Array` machinery is the next, heavier increment.

## Build status UPDATE — the embed host model + the FIRST shape-DISCHARGED rung (2026-06-21/22)

Two of the divergences above are now partly closed:

- **Embedded CPython host model LANDED (ch462/ch463, v3.328.25–.32).** The native C and
  LLVM backends link `libpython` and call REAL CPython through the C-API: scalars
  (`pyPow`/`pySqrt`), arbitrary-precision bignum via decimal-string marshalling
  (`pyFactorial`), and — the structured rung — `pyNpSum`/`pyNpScale`/`pyNpMatVec` run real
  **numpy** in the embedded interpreter, BIDIRECTIONALLY: a Rune `FList` marshals to a
  Python list, numpy reduces/reshapes, and an array result walks BACK into a Rune `FList`
  (`mkcon` `fcons`/`fnil`, tail-first). So "embed-vs-port CPython hosting" is no longer
  absent — the EMBED is built, cross-backend (C+LLVM), and the Rune↔numpy bridge round-trips
  1-D and 2-D. Still flat `FList` + `Nat` dims, not the opaque `Array` handle.

- **The first SHAPE-DISCHARGED foreign call LANDED (ch467, v3.328.39).** The headline of
  this design — *shapes discharged by dependent types, not guarded at runtime* — is now
  realized across the FFI boundary, on the flat substrate, BEFORE the opaque handle.
  `safeNpMatVec` wraps the embed's `pyNpMatVec` with TYPED preconditions tying the flat
  array's length to its declared shape:

  ```
  safeNpMatVec : (m k : Nat) -> (A : FList) -> (v : FList)
     -> Eq Nat (flen A) (natMul m k)   -- the (m×k) matrix's flat length IS m*k
     -> Eq Nat (flen v) k              -- the vector's length IS k
     -> FList
  ```

  A caller with a mismatched flat array CANNOT supply `refl` (verified: a `k=3` witness is
  rejected with "refl does not prove the equation"), so a malformed numpy call is a COMPILE
  error, not a runtime shape check. The proofs erase; the foreign body is the unchanged
  `pyNpMatVec`. The well-shaped witness runs real numpy through the embed:
  `[[1,2],[3,4]]·[5,6] = [17,39]`, summed to 56 (`TestD4ShapeDischargedArray`, native C). This
  reuses ch446/ch447's in-language `Eq Nat` shape-contract idiom but points it at a FOREIGN
  call — exactly the "shape conformance discharged from the typed shapes" the Chosen-approach
  section specifies, with `flen`/`natMul` standing in for the not-yet-built `Shape` kit.

### The staged path to the general `Array dt sh` handle

ch467 is rung 1 of a staged climb that keeps each step shippable (Standing Rule 1) and the
kernel fixed (Thompson):

1. **Guarded flat arrays** (ch221–228) — flat `FList` + `Nat` dims, shapes recomputed and
   compared at the boundary. DONE.
2. **Shape-DISCHARGED flat arrays** (ch467 + ch470) — the same flat `FList`, but the dim↔length
   conformance is a TYPED precondition, so a malformed array is a compile error. DONE: ch467
   (1-D vector + 2-D matvec, hand-written `Eq Nat (flen A) (natMul m k)`) AND ch470 (v3.328.42 —
   ARBITRARY RANK: a `Shape = List Nat` value + `flatLen : Shape -> Nat` (the product, `flatLen []
   = 1`, `flatLen (d::ds) = d * flatLen ds`), so the precondition reads `Eq Nat (flen A)(flatLen
   sh)` at ANY rank — a rank-3 [2,2,2] tensor discharges, a length-7 array against it is rejected
   at `refl`, and the bridge `flatLen [m,k] ≡ natMul m k` recovers ch467's rank-2 call, which
   runs real numpy via the embed → 56; TestD4ShapeFlatLen). Rung 2 is COMPLETE.
3. **A `DType` kit** — a closed `data DType is f64 | f32 | i64 | ... end` + `elemTy : DType ->
   U` + `dtypeStr : DType -> String`, so the element type is a value the boundary marshaller
   reads (today everything is `Float`/f64). Rune library, no core.
4. **The opaque `Array dt sh` handle + `CArray` CRepr** — replace the flat `FList` with a
   bodiless rigid `Array : DType -> Shape -> U` whose CRepr mirrors numpy's
   `__array_interface__` (data ptr, dtype, ndim, shape, strides). This is the one piece that
   needs a codegen CRepr extension (beyond scalar/String/Ptr) and a `store/foreign.go` entry.
   The bidirectional marshalling (ch463) and the shape-discharge (ch467) become its `peek`/
   `poke`/`reshape` operations, now O(1) handle passing instead of element-wise FList walks.

Rung 1 landed (ch467); rung 2 COMPLETE (ch470 — arbitrary-rank `Shape`/`flatLen`); 3 is a pure
library; only rung 4 touches codegen. The design below is the rung-4 target; ch467/ch470 prove
the shape-discharge semantics it will inherit. NEXT: rung 3, the `DType` kit (a closed enum +
`elemTy`/`dtypeStr`), also a pure library — no core, no cloud.

## Problem (what's stuck/absent today, with file:line)

R-FFI (B4) makes a *scalar* foreign call typed and contract-guarded: `c_sqrt :
(x : Double) -> { y : Double | leq 0.0 y }` lowers to a bodiless rigid axiom
(`store/foreign.go`, the `store/io.go`/`store/fib.go` pattern), the post is
guarded/assumed/discharged, and the assumed axiom is logged into the `A`-set cert
key (`ref_docs/wootz/R-FFI.md:253-290`). That is the whole *boundary mechanism*.
What is **absent** is everything specific to *numeric arrays and the Python data
ecosystem*:

1. **There is no array type and no array `CRepr`.** R-FFI's marshalling table
   (`ref_docs/wootz/R-FFI.md:300-310`) and R-IR's `Repr` table
   (`ref_docs/wootz/R-IR.md:244-252`) are **scalar + String + opaque Ptr only**.
   R-FFI says so plainly: "the array `CRepr` (research)"
   (`ref_docs/wootz/R-FFI.md:579`), "structs are a later `CRepr` extension"
   (`ref_docs/wootz/R-FFI.md:510`). A NumPy `ndarray` is *not* a scalar and *not*
   an opaque `Ptr` we can leave uninspected — it is **(data pointer, dtype, ndim,
   shape[], strides[], owner)**, and the contract surface (does this matmul's LHS
   width equal the RHS height?) is *exactly* a statement about that shape vector.
   The current substrate has nowhere to put shape.

2. **The numeric tower is exact, the foreign world is floating-point.** The
   shipped tower is `Nat`/`Int`/`Rat` — *exact* arithmetic over quotients
   (`ref_docs/rune-numeric-tower.md:117-150`), with **no `Double`/`Float64` and
   no `/` at Nat/Int by design** (`rune-numeric-tower.md:69`). BLAS/NumPy/Torch
   are IEEE-754 `f64`/`f32` (and `bf16`/`f16`). D3 (`reals/floats +
   linear-algebra interfaces`, the DAG) is the node that *introduces* a `Double`
   type and the LA interface; R-INTEROP is the node that *binds the
   implementations* behind it. The gap: there is no `Double`, no `Array dt sh`,
   and therefore nothing for a BLAS contract to be *about*.

3. **The deploy story is single-process JS; the Python ecosystem is a separate
   runtime.** Today `rune run` is `node` on a self-contained `.js`
   (`CLAUDE.md` Phase 7). NumPy/BLAS/Matplotlib live in **CPython + a C/Fortran
   BLAS + a GUI/Agg renderer**. R-FFI's backend classes
   (`ref_docs/wootz/R-FFI.md:320-339`) cover *C symbols* (LLVM/WASM-machine
   direct, BEAM NIF/port, JVM Panama, JS N-API) but say nothing about *how an
   entire foreign interpreter (CPython) is hosted*, where the GIL sits, or
   whether the bridge is in-process (NIF/embed, fast, can crash the VM) or
   out-of-process (port/subprocess, Lambert-safe). The roadmap's spirit demands
   **BEAM first** (`humble-humming-elephant.md:79-82`), and BEAM's idiom for
   "large foreign compute that must not crash the scheduler" is a *port or a
   dirty-NIF*, not a naive NIF — a decision the scalar R-FFI never had to make.

4. **Nothing constrains a plot.** "Plotting" in the node title is the hardest
   *contract* target: a plot is an effect (writes a file / opens a window) whose
   "correctness" is mostly *not* a refinement on a return value — it is "the
   series I handed it had matching x/y lengths and finite values." That is a
   **precondition on array shape/finiteness** (R-FFI's QTT-0 precondition,
   `ref_docs/wootz/R-FFI.md:194-198`) plus an R-EFFECT `IO` for the write — but it
   needs the array type to *carry the length in its type* for the precondition to
   be checkable at all.

The net: R-FFI gave us a typed, tracked, three-tier boundary for *scalars*.
R-INTEROP must add (a) a Rune **type for a foreign numeric array** that carries
enough shape/dtype to state contracts, (b) the **array `CRepr`** across the four
backend classes (the zero-copy story per host), (c) a **CPython/BLAS hosting
model** (in-process vs out-of-process, GIL, BEAM-first), and (d) the **numeric
contract vocabulary** (shape conformance, dtype match, finiteness, ownership) —
all behind R-FFI's existing `foreign`/`A`-set machinery, with **no outer-core
growth and no hash-format bump** (CLAUDE.md: a bump is only for a new *core*
constructor).

## Prior art (what the literature/other systems do; cite)

- **NumPy's `__array_interface__` / buffer protocol — the wire format to bind
  to.** Every NumPy-compatible array exposes `(shape, typestr/dtype, data pointer,
  strides, version)`; the C-level buffer protocol (`Py_buffer`) and Cython's
  `__Pyx_TypeInfo` validate dtype/contiguity at the boundary and produce precise
  errors on mismatch
  ([NumPy array interface](https://numpy.org/doc/stable/reference/arrays.interface.html);
  [Cython buffer/NumPy integration](https://deepwiki.com/cython/cython/4.2-buffer-protocol-and-numpy-integration)).
  The lesson: the array's *self-description* — pointer + dtype + shape + strides +
  owner — is **already a standardized struct**; R-INTEROP's `CRepr` for arrays is
  a Rune-side mirror of `__array_interface__`, and the dtype/contiguity check is
  exactly a guard-tier boundary check.

- **Futhark / "Towards size-dependent types for array programming" — the shape
  type story for a *practical* array language.** Futhark extends HM inference with
  **size types**: `[n]f64`, functions whose types force compatible sizes, the
  checker rejecting programs whose sizes can't be statically reconciled; sizes
  that can't be inferred become *existential* (`?[n].`)
  ([Henriksen, ARRAY 2021](https://futhark-lang.org/publications/array21.pdf);
  [Futhark](https://en.wikipedia.org/wiki/Futhark_(programming_language))). The
  lesson and the *exact* fit for this substrate: Rune is *fully* dependent, so a
  shape is just a **`Vec n Nat`** value in the type `Array dt sh` — no special
  size-type machinery needed, and a "shape unknown until runtime" array is an
  **existential `Σ (sh : Shape). Array dt sh`** (R-SUM Sigma), Futhark's `?[n].`
  for free.

- **Dependently-typed tensor / "array programs don't go wrong" (Xi & Pfenning;
  `safe-tensor`).** Dependent indices make inconsistent tensor ops *unrepresentable*
  — matmul's inner dimensions must be propositionally equal, transpose permutes the
  shape vector, broadcasting is a relation on shapes
  ([safe-tensor](https://hackage.haskell.org/package/safe-tensor);
  [size-dependent types](https://dl.acm.org/doi/pdf/10.1145/3460944.3464310)). The
  lesson: the LA *interface* (D3) states its laws as **shape equalities in the Rune
  type**; the BLAS *binding* (R-INTEROP) is a foreign axiom that *satisfies* that
  interface, with the shape precondition either discharged (proven from the typed
  shapes) or guarded (checked against the runtime `shape[]`).

- **Erlang NIFs + resource objects — the BEAM hosting model.** `enif_alloc_resource`
  / `enif_make_resource` return a GC-managed *safe handle* to a native struct; a
  binary is a contiguous read-only block with `{data, size}` directly usable as an
  array buffer; **dirty NIFs** run long C work off the scheduler so a BLAS `dgemm`
  doesn't stall the VM, and a **port** isolates a crash-prone library in a separate
  OS process
  ([erl_nif](https://www.erlang.org/doc/apps/erts/erl_nif.html);
  [NIFs guide](https://www.erlang.org/doc/system/nif.html)). The lesson — and the
  BEAM-first realization: a NumPy array on BEAM is **either a resource handle to a
  contiguous binary (zero-copy in-process) or a message to a port (isolated)**;
  R-FFI's existing per-decl NIF-vs-port attribute (`ref_docs/wootz/R-FFI.md:332`)
  is exactly the affine/safety knob, lifted to arrays.

- **CPython embedding, the GIL, and out-of-process numeric compute.** Embedding
  CPython means owning the GIL (`PyGILState_Ensure`/`Release`), and the GIL
  serializes the interpreter; the *officially recommended* parallel path is
  **subprocesses**, with **shared-memory** array transfer (`pyarraypool`,
  `multiprocessing.shared_memory`) to avoid copy overhead; PyO3 models GIL
  acquisition/release as a scoped token
  ([Python GIL C-API](https://docs.python.org/3/c-api/threads.html);
  [PEP 703 — optional GIL](https://peps.python.org/pep-0703/);
  [Real Python — parallelism](https://realpython.com/python-parallel-processing/);
  [PyO3 free-threading](https://pyo3.rs/v0.28.3/free-threading)). The lesson: there
  are **two Python bridges**, and R-INTEROP must offer both as a *backend-keyed
  target choice* (R-FFI's per-backend target table,
  `ref_docs/wootz/R-FFI.md:430-434`): **embed** (in-process, GIL-held, fast,
  fate-shared) and **port/RPC** (out-of-process CPython worker, shared-memory
  arrays, Lambert-safe). BEAM strongly prefers the port; LLVM/native can embed.

- **VeriFFI (the discharge ceiling), F\*/Idris (`%foreign`, `C_Types`, `assume`),
  soft contracts + blame (the guard tier).** All inherited *unchanged* from R-FFI
  (`ref_docs/wootz/R-FFI.md:71-138`). R-INTEROP adds *array-shaped* instances of
  the same three tiers: a BLAS `dgemm` contract is **assumed** (BLAS is trusted),
  the **shape conformance** clause is **discharged** (proven from the typed
  shapes — no runtime cost, the whole point of dependent shapes), and **dtype/
  finiteness/contiguity** at the foreign boundary is **guarded** (checked against
  `__array_interface__`, blame the foreign side / the caller).

Cross-cutting, three smiths: NumPy's array-interface + Futhark's size types show
the array type is a **mirror of an existing standard, not core growth** (Thompson).
NIF resources + CPython-out-of-process show the binding must **face the real GIL
and the real VM scheduler** (Lambert). The three tiers applied to a dgemm —
*prove the shapes, guard the dtype, assume the math* — make a pipeline a learner
can build incrementally (Savage: scatter the data with a guarded plot, then prove
the shape, then trust the BLAS).

## Chosen approach for THIS substrate (concrete; respects containment)

**Thesis: an array is a *typed opaque foreign handle whose Rune type carries its
shape and dtype as ordinary type indices* — `Array dt sh` where `sh : Shape` is a
`Vec ndim Nat` value — bound by R-FFI `foreign` axioms, with shape conformance
**discharged**, dtype/finiteness/contiguity **guarded** at the boundary, and the
numeric library (BLAS/NumPy) **assumed** (logged in the `A`-set).** The handle's
*payload* is R-FFI's opaque `Ptr` (the `CRepr` already exists); R-INTEROP's new
work is (i) the *index* on top of the handle (`dt`, `sh`) so contracts can talk
about shape, (ii) the **array `CRepr`** = the `__array_interface__` struct per
backend, and (iii) the **host model** (embed vs port). **No outer-core
constructor, no hash-format bump**; everything lands in `store/foreign.go`
(R-FFI), the codegen `CRepr`/`Shim` (R-IR), and a *tiered-typed Rune library*
(`Array`, `Tensor`, BLAS/NumPy/plot bindings) — the D-track's library layer.

Six pieces: (A) the array type; (B) the dtype kit; (C) the array `CRepr` =
`__array_interface__` per backend; (D) the CPython/BLAS host model (embed vs
port, BEAM-first); (E) the numeric contract vocabulary mapped to R-FFI's three
tiers; (F) the library surface (BLAS / NumPy / plotting bindings).

### (A) The array type — a shape-and-dtype-indexed opaque handle

An array is **not** a Rune inductive type whose cells are Rune values — that would
*reimplement* NumPy (forbidden) and force every element across the boundary. It is
an **opaque foreign handle** (R-FFI's `Ptr A`, `ref_docs/wootz/R-FFI.md:308`)
*refined by a Rune-level shape and dtype index*:

```
-- dt : DType         the element type tag (a small inductive enum, below)
-- sh : Shape         the shape, a Vec ndim Nat (rank in the length, dims in cells)
Array : DType -> Shape -> U          -- a fibred opaque handle; cells live in C, not Rune
```

`Array dt sh` is a *postulated abstract type* (a bodiless rigid head in
`store/foreign.go`, exactly the R-FFI axiom pattern): you cannot pattern-match
into it (no eliminator), its **only** introductions/eliminations are *foreign
operations* (load, fill, `at`, the BLAS/NumPy ops). This is the
**`builtin nat`/`Ptr` discipline**: the *representation* is foreign, the *type
discipline* is Rune. Crucially, the **shape lives in the type** (`sh`, a real
`Vec n Nat`), so:

- **matmul** has the dependent type that makes mismatch unrepresentable:
  ```
  matmul : (m n k : Nat) -> Array f64 [m, k] -> Array f64 [k, n] -> Array f64 [m, n]
  ```
  the shared `k` is the conformance contract, *discharged by type-checking* (zero
  runtime cost — Futhark/safe-tensor's whole point).
- **runtime-shaped data** (e.g. "load a CSV, shape unknown till read") is the
  **existential** `Σ (sh : Shape). Array dt sh` (R-SUM Sigma, C1) — Futhark's
  `?[n].`; the consumer pattern-matches the shape *value* out and proceeds with it
  in scope. This is why R-INTEROP **lists R-SUM (C1) as a hard dependency** for the
  shaped-load surface (honest: the *fixed-shape* surface needs no Sigma).

`DType` and `Shape` are *tiny ordinary inductive types* in the Rune library (Track
D, not core): `Shape = Vec Nat` (a list with its length), `DType` an enum. They
cost the core nothing — they are data declarations (Phase 4, done).

### (B) The dtype kit — a closed enum bridging Rune scalars to NumPy dtypes

```
data DType : U is
  | f64 : DType  | f32 : DType  | i64 : DType  | i32 : DType
  | bool_ : DType                                  -- + f16/bf16 later, behind a consumer
end
elemTy   : DType -> U            -- f64 -> Double, i64 -> Int, bool_ -> Bool, ...
dtypeStr : DType -> String       -- f64 -> "<f8", i64 -> "<i8" — the __array_interface__ typestr
```

`elemTy dt` is the Rune scalar a *single* extracted cell crosses as (R-FFI's
scalar `CRepr`s — `Double`/`Int`/`Bool` already exist post-D3). `dtypeStr` is the
NumPy `typestr` the guard-tier dtype check compares against at the boundary.
**This is the only place R-INTEROP touches the numeric tower**: it requires D3's
`Double` (IEEE-754 `f64`, the foreign float), and reuses the exact tower
(`Int`/`Nat`) for integer dtypes (`ref_docs/rune-numeric-tower.md`).

### (C) The array `CRepr` — `__array_interface__` per backend

Grow R-FFI's `CRepr` (`ref_docs/wootz/R-FFI.md:399`) with **one** new case,
`CArray`, whose wire form is NumPy's array-interface struct
([NumPy array interface](https://numpy.org/doc/stable/reference/arrays.interface.html)):

```
CArray = { data: void*          -- contiguous buffer (the cells, dtype-encoded)
         , dtype: typestr       -- "<f8" etc., checked against dtypeStr dt
         , ndim: int            -- = length of sh (checked)
         , shape: int64[ndim]   -- = the cells of sh (checked)
         , strides: int64[ndim] -- contiguity check (guard tier)
         , owner: ownership }   -- borrow (Rune never frees) | owned (finalizer)
```

Per backend (extending R-FFI's table, `ref_docs/wootz/R-FFI.md:300-310`, and
R-IR's `Repr`, `ref_docs/wootz/R-IR.md:244`):

| Backend            | `CArray` realization                                   | zero-copy? |
|--------------------|--------------------------------------------------------|------------|
| Native-C (LLVM/Cranelift/WASM-machine) | a C `struct {void* data; ...}` by System-V/wasm ABI; calls into libBLAS/NumPy-C directly | yes (shared buffer) |
| **BEAM (first)**   | a **resource handle** to a contiguous **binary** (`enif_alloc_resource`); dirty-NIF for BLAS, **or** a **port** msg carrying the binary | yes (NIF) / copy (port) |
| JVM                | a `DoubleBuffer`/`MemorySegment` (Panama FFM) over off-heap memory | yes |
| JS / WASM-DOM      | a `Float64Array` over a `WebAssembly.Memory` / `ArrayBuffer`; numeric-WASM (numpy-wasm/pyodide) or a JS shim | yes (typed array) |

The **ownership** field is R-FFI's affine obligation (`ref_docs/wootz/R-FFI.md:311-318`)
applied to the buffer: `borrow` arrays are not freed by Rune (the default,
ready-to-build everywhere); `owned` results need a backend finalizer (BEAM
resource GC, JVM cleaner, WASM explicit free) or the decl is unsupported there
(clean `Emit` error). The **dtype/ndim/shape/strides checks** are the **guard
tier**: when an array crosses *in*, the boundary verifies `dtype == dtypeStr dt`,
`ndim == length sh`, `shape == sh`, contiguous strides — and **blames the foreign
side** on mismatch (Cython's `__Pyx_TypeInfo` validation, made a Rune contract).

### (D) The CPython / BLAS host model — embed vs port, BEAM-first

R-FFI's three FFI classes (`ref_docs/wootz/R-FFI.md:320-339`) cover *C symbols*.
NumPy/Matplotlib are *CPython*, so R-INTEROP adds a **host axis** to the
per-backend target table — two realizations of "call into Python," chosen per
backend / per decl, never in core:

1. **Embed (in-process CPython).** Link `libpython`; each foreign Python op
   acquires the GIL (`PyGILState_Ensure`), marshals `CArray` ↔ NumPy via the
   buffer protocol (zero-copy: NumPy wraps the borrowed buffer), calls, releases.
   Fast, fate-shared (a NumPy segfault takes the process). The default for
   **native-C backends** (LLVM/WASM-machine) where there is no scheduler to
   protect. *BLAS without Python* (D3's pure `dgemm`) is the even-simpler case: a
   direct C-ABI call, no interpreter — the **first thing to ship**.

2. **Port / RPC (out-of-process CPython worker).** A long-lived CPython
   subprocess; arrays move by **shared memory** (`multiprocessing.shared_memory`
   / `pyarraypool`, [Real Python](https://realpython.com/python-parallel-processing/))
   so only the *handle* (name + shape + dtype) is messaged, not the cells; the op
   is an RPC. Survives a Python crash (Lambert: "no abstraction that shatters on a
   page fault"); the supervisor restarts the worker. The default for **BEAM**
   (an embedded `libpython` NIF that segfaults crashes the whole VM — unacceptable),
   realized as a **port** or a **dirty-NIF over a fixed BLAS** for the hot path.

The choice is a **decl/target attribute** (R-FFI's NIF-vs-port knob generalized,
`ref_docs/wootz/R-FFI.md:332`):

```
foreign np_std : (n : Nat) -> Array f64 [n] -> Array f64 [n]
  is { c = "embed:numpy.std", beam = "port:numpy.std", llvm = "embed:numpy.std" }
  with arg finite        -- guard: cells are finite (no NaN/Inf) at the boundary
  with res shape [n]      -- discharge: result shape proven equal to input shape
end
```

**BEAM-first concretely:** the M0/B3 slice binds **BLAS via a dirty-NIF** (pure C,
no Python, contiguous binary as the buffer — the cleanest possible array
boundary) and **NumPy/Matplotlib via a port** (out-of-process, shared-memory). No
`libpython` in the BEAM address space. This is the Lambert-correct BEAM idiom and
matches the roadmap's "BEAM to genuine working FIRST" (`humble-humming-elephant.md:80`).

### (E) Numeric contract vocabulary → R-FFI's three tiers

Every foreign numeric edge gets contract clauses, each landing in exactly one
R-FFI tier (`ref_docs/wootz/R-FFI.md:212-251`):

| Clause                | What it asserts                              | Tier (default)             |
|-----------------------|----------------------------------------------|----------------------------|
| **shape conformance** | inner dims match / result shape relation     | **discharge** (from typed `sh` — zero cost) |
| **dtype match**       | buffer `typestr == dtypeStr dt`              | **guard** (boundary check, blame) |
| **contiguity**        | C-contiguous strides (BLAS needs it)         | **guard** (boundary check) |
| **finiteness**        | no NaN/Inf in the buffer                      | **guard** (opt-in; D1 `Dec`) |
| **ownership**         | borrow vs owned (no UAF/leak)                | **affine** (R-FFI ownership) |
| **the math itself**   | `dgemm` computes C = αAB + βC                 | **assume** (logged in `A`) |

The headline: **shape is discharged, not guarded.** Because `sh` is a *type
index* (`Vec n Nat`), `matmul`'s `k = k` is settled by the checker; the foreign
call carries the dimensions but *no runtime shape assertion is needed* — the
dependent type already proved it (Futhark/safe-tensor, realized on a *fully*
dependent substrate). dtype/contiguity/finiteness are *runtime* facts about the
foreign buffer, so they are **guarded** (the on-ramp; promote-to-assume drops the
check). The numeric *result* (the BLAS math) is **assumed** and shows in `rune
assumptions <pipeline>` — exactly the user's "contract-guard": you *see* that your
ML result rests on `cblas_dgemm`'s correctness, and `rune check --safe` refuses to
deploy a "proven" module that secretly trusts BLAS unless you say so.

### (F) The library surface — BLAS / NumPy / plotting bindings (Track D)

A *tiered-typed Rune library* (telos 3's "typed leaves … property-tested"), all
`foreign` decls over the types above — **no proofs required for a leaf**, contracts
guard the boundary:

- **`Linalg`** (over D3's LA interface): `matmul`, `solve`, `dot`, `transp`,
  `qr`/`svd` — BLAS/LAPACK axioms with discharged shapes, assumed math.
- **`Stats`**: `mean`, `std`, `corr`, `lstsq` — NumPy/SciPy, finiteness guarded.
- **`ML`**: a thin `fit`/`predict` over scikit-learn / a tensor lib (Torch/JAX as
  a later target), shapes typed.
- **`Plot`**: `scatter`, `line`, `savefig` — Matplotlib (Agg, headless), an
  R-EFFECT `IO` (it writes a PNG); the **x/y length match is a typed
  precondition** (`Array f64 [n] -> Array f64 [n] -> IO Unit`), so a ragged plot
  is a *type error*, not a runtime stack trace.

**Containment audit.** No `core/` term, no `store/` hashing change, no
`equality/`, no hash-format bump. New: `Array`/`DType`/`Shape` are a *Rune
library* (`Array` as a `store/foreign.go` abstract axiom, `DType`/`Shape` as
ordinary data decls); `CArray` is one new `CRepr` case (codegen, R-IR's `Repr`);
the embed/port host model is per-backend `Shim` + target metadata (non-hashed,
R-FFI's discipline). The numeric *contracts* reuse R-FFI's tiers and `A`-set
verbatim. Reality lives in codegen + a contained library — where the strata thesis
puts it.

## Interfaces & signatures to add (Go + Rune surface as relevant)

Go (codegen, extending R-FFI/R-IR; **no core/store-hash change**):

```go
// codegen/ — one new CRepr case + its descriptor (grows R-FFI's CRepr,
// ref_docs/wootz/R-FFI.md:399, and R-IR's Repr, ref_docs/wootz/R-IR.md:244).
const CArray CRepr = /* next */     // a NumPy-array-interface buffer
type ArrayRepr struct {
    Dtype    string   // "<f8" etc. (the __array_interface__ typestr)
    Ndim     int      // checked == len(shape index)
    Owner    Ownership // Borrow (default) | Owned (needs backend finalizer)
}
// Per-op host realization (extends R-FFI's per-backend target table; NON-hashed):
type Host int          // HEmbed (in-process CPython/BLAS) | HPort (out-of-proc worker)
type ForeignSig struct {  // grows R-FFI's ForeignSig (R-FFI.md:400)
    // ...R-FFI fields (Arity, ArgRepr, ResRepr, Clauses, Targets)...
    Host    Host             // embed vs port, per backend
    ArrayArgs map[int]ArrayRepr // which args/result are CArray + their descriptors
}

// codegen/shim — each backend's Shim (R-IR.md:326) gains array marshalling +
// (for HEmbed) a CPython/BLAS bridge, (for HPort) a worker-protocol client.
//   BEAM Shim: enif_alloc_resource over a binary (dirty-NIF) / port codec.
//   Native Shim: __array_interface__ struct <-> libBLAS / embedded CPython.
//   JS Shim: Float64Array over WASM memory / pyodide bridge.

// The guard-tier array checks are IR `assert`s the boundary emits (R-FFI guard
// tier, R-FFI.md:232-237): dtype/ndim/shape/contiguity, with blame on the decl.
```

Rune surface (a *library*, built on R-FFI's `foreign` + R-EFFECT's `IO`; **not
core**):

```
-- the abstract array type (store/foreign.go abstract axiom) + dtype/shape kit
Array : DType -> Shape -> U
data DType : U is f64 | f32 | i64 | i32 | bool_ end
Shape : U is Vec Nat                       -- rank in the length, dims in the cells

-- BLAS: shape discharged (k=k by checking), math assumed, dtype guarded
foreign matmul : (m n k : Nat) -> Array f64 [m,k] -> Array f64 [k,n] -> Array f64 [m,n]
  is { c = "embed:cblas_dgemm", beam = "nif:blas_dgemm", llvm = "embed:cblas_dgemm" }
  with arg dtype guard          -- check buffers are <f8 at the boundary, blame
end

-- runtime-shaped load: existential shape (R-SUM Sigma), Futhark's ?[n].
foreign loadCSV : String -> IO (Sigma Shape (fn sh is Array f64 sh end))
  is { c = "embed:numpy.loadtxt", beam = "port:numpy.loadtxt" } end

-- plotting: x/y length match is a TYPED precondition (ragged plot = type error)
foreign scatter : (n : Nat) -> Array f64 [n] -> Array f64 [n] -> String -> IO Unit
  is { c = "embed:mpl.scatter", beam = "port:mpl.scatter" } end
```

CLI: unchanged from R-FFI — `rune assumptions <pipeline>` now lists the *numeric*
axioms (`cblas_dgemm`, `numpy.std`); `rune check --safe` refuses a deployed
pipeline whose `A` includes an *un*discharged numeric axiom; `rune run`/`emit`
emit the dtype/contiguity guards.

## Worked micro-example (the teachable artifact)

`fit.rune` — load points, standardize, least-squares fit a line, plot scatter +
fit. Every foreign edge carries a contract; the three tiers are visible.

```
-- shape comes back at runtime: existential (R-SUM)
main : IO Unit is
  seq
    -- 1. LOAD: shape unknown till read -> existential Sigma (Futhark ?[n].)
    let xy = loadCSV "points.csv"        -- IO (Σ sh. Array f64 sh)
    let (sh, a) = unpack xy              -- sh : Shape, a : Array f64 sh in scope
    -- assume sh = [n, 2] (guarded at the boundary: ndim=2, cols=2, blame on mismatch)
    let n  = nrows sh
    let xs = col 0 n a                   -- Array f64 [n]   (slice; shape discharged)
    let ys = col 1 n a                   -- Array f64 [n]
    -- 2. STANDARDIZE: numpy.std/mean, finiteness GUARDED, shape DISCHARGED
    let zx = np_standardize n xs         -- Array f64 [n]; result shape [n] proven
    -- 3. FIT: least squares via BLAS (lstsq) — shape conformance DISCHARGED,
    --    the math ASSUMED (shows in `rune assumptions main`)
    let (slope, intercept) = lstsq n zx ys
    -- 4. PLOT: x/y length match is a TYPED precondition; ragged = type error
    let yhat = lineEval n slope intercept zx
    seq
      scatter n zx ys "fit.png"          -- IO Unit; x,y both [n] by type
      lineOver n zx yhat "fit.png"
    end
  end
```

What the learner sees and can verify (the teaching beats):

1. **Shape is proven, not checked.** `scatter n zx ys` type-checks *only because*
   `zx` and `ys` both have type `Array f64 [n]` — a ragged plot (`scatter n zx
   ys'` with `ys' : Array f64 [m]`) is a **compile error**, not a runtime crash.
   This is Futhark/safe-tensor on a fully-dependent substrate: no runtime shape
   assertion is emitted for the conformance — the `n = n` is settled by the
   checker. (Savage: "the dimensions can't disagree because the *type* says so.")

2. **dtype/finiteness is guarded at the door.** `loadCSV` and `np_standardize`
   carry `with arg dtype guard` / `with arg finite`: if the CSV had a string
   column (wrong dtype) or a NaN, the boundary halts with `contract violation:
   np_standardize arg (finite) — blame loadCSV` (Cython's `__Pyx_TypeInfo` check,
   as a Rune blame). Lambert: "the data lied, caught at the door."

3. **The math is assumed and *visible*.** `rune assumptions main` lists
   `cblas_dgemm` / `numpy.lstsq` under **assumed axioms** (logged in the `A`-set,
   `ref_docs/wootz/R-FFI.md:253-290`): the fit's correctness rests on BLAS being
   BLAS, and the proof cache *says so*. `rune check --safe main` **fails** —
   you're trusting LAPACK — until you either accept it or discharge a model.
   (Thompson/F\*: trust is explicit and tracked, never silent.)

4. **One source, four backends.** The same `fit.rune` runs on **BEAM** (BLAS via
   dirty-NIF on a binary, NumPy/Matplotlib via an out-of-process port +
   shared-memory — the M0 slice), on **native** (embedded CPython + libBLAS,
   zero-copy buffers), and degrades cleanly on **WASM-DOM** (pyodide or
   "unsupported plot target" — a clean `Emit` error, R-IR's `IInner`-ban shape).
   The guard/blame behavior is identical where supported (M4 conformance).

The single teaching sentence: *a NumPy array is a foreign buffer whose shape and
dtype live in its Rune type — so the shapes are **proven** (no mismatch can
compile), the dtype and finiteness are **guarded** at the boundary (blame the
data), and the math is **assumed** and shown in `Print Assumptions` — bind the
ecosystem, don't reimplement it, and know exactly what you're trusting.*

## Risks / open sub-questions

- **Existential shapes need R-SUM (C1).** Runtime-shaped loads (`loadCSV`) are
  `Σ Shape (Array dt)`, which needs outer Sigma. *Fixed-shape* binds (a known
  `[3,3]` kernel, a typed `matmul` over already-typed shapes) need **no** Sigma
  and are ready the moment B4/R-FFI lands. **Status: fixed-shape ready-to-build;
  shaped-load is C1-gated** (same dependency R-FFI already names,
  `ref_docs/wootz/R-FFI.md:490-494`).

- **`Double`/`f64` is D3's, not ours.** R-INTEROP *consumes* an IEEE-754 `Double`
  and the LA interface; it does not define them (the exact tower has no `Double`,
  `rune-numeric-tower.md:69`). **Status: blocked on D3 for the float element type
  and the LA interface signatures; the array *machinery* (handle, `CRepr`, host
  model) is independent of D3 and ready once R-FFI lands.**

- **Embedded CPython is a fate-sharing hazard (Lambert).** `libpython` in-process
  means a NumPy/native-extension segfault kills the host; on BEAM that is the
  whole VM. *Mitigation, and the design's spine:* **BEAM defaults to port/RPC**
  (out-of-process, shared-memory), embed is opt-in for native backends with no
  scheduler to protect. The port protocol + shared-memory codec is **engineering,
  ready-to-build** (pyarraypool is the reference); the *embedded* path is
  per-backend and partly research (GIL re-entrancy under R-OTP actors).

- **Zero-copy vs ownership across the boundary.** A borrowed buffer NumPy wraps
  must outlive the call; an `owned` result needs a finalizer. *Mitigation:* default
  `borrow`, the buffer is pinned for the call's duration (R-FFI's affine
  obligation); `owned` arrays require a backend with a finalizer (BEAM resource GC,
  JVM cleaner) or are unsupported there. **Status: borrow ready-to-build; owned is
  per-backend, partly research** (inherits R-FFI's ownership risk,
  `ref_docs/wootz/R-FFI.md:511-516`).

- **dtype/contiguity guard needs decidability for the predicate.** A guard is a
  `Dec P` (R-FFI guard tier, `ref_docs/wootz/R-FFI.md:498`). dtype-equality and
  ndim/shape-equality are trivially decidable (`Nat`/enum equality, D1);
  **finiteness** is a decidable scan of the buffer (a foreign `isfinite` reduction,
  itself a small guarded op). **Status: dtype/shape guard ready-to-build on D1;
  finiteness is a foreign-reduction guard, ready once the scan op is bound.**

- **VeriFFI-grade discharge of the *math* is research.** Proving `cblas_dgemm`
  actually computes the matrix product (vs assuming it) is the deep tail R-FFI
  parks here (`ref_docs/wootz/R-FFI.md:502-504`). R-INTEROP ships **assume** for
  the math (the honest default) and **discharge** only for *shape* (which it gets
  for free from dependent types). Proving the numeric kernels is **research,
  parked** — and is where a future "furnace property-test the BLAS against a Rune
  reference model" lives (the Savage on-ramp: property-test the kernel, then
  trust it).

- **Strided / non-contiguous / broadcasting.** BLAS wants C-contiguous; NumPy
  views can be strided; broadcasting is a *relation* on shapes (safe-tensor). First
  cut: **C-contiguous only**, guard-check it, copy-to-contiguous on the foreign
  side if not (a cost, blamed). Strided `CRepr` and a broadcasting shape relation
  are a **labelled extension** (research, per the size-dependent-types literature).

- **Higher-rank tensors / autodiff (Torch/JAX).** The `Array dt sh` type scales to
  arbitrary rank (`sh : Vec ndim Nat`), so a tensor lib is *more of the same
  bindings*; **autodiff** (differentiable foreign ops) is a genuinely separate
  research problem (the foreign graph is opaque to Rune's logic) — **labelled
  research, out of scope** for D4's first cut.

## Test/gate plan

- **Containment gates (Thompson, must pass):** no diff to `core/term.go`,
  `core/hash.go`, `defFormatVersion`/`hashFormatVersion`; the `Array` axiom +
  `DType`/`Shape` data decls + the `CArray` `CRepr` reproduce every existing pure
  def's hash and cert key byte-for-bit (empty `A`, no new core constructor — the
  R-FFI `foreign` group already proves own-hash-space). Mirror
  `store/foreign_test.go`.
- **Shape conformance is discharged (the headline property):** a `matmul` /
  `scatter` with mismatched dimensions is a **type error** at elaboration (no
  runtime check emitted); a conforming one type-checks and emits **no** shape
  assertion in the IR. Property-test over generated shape pairs: well-typed iff
  dims reconcile, and the emitted IR contains zero shape guards for the discharged
  clause.
- **dtype/contiguity/finiteness guard + blame (Lambert):** an array crossing in
  with the wrong `typestr`, wrong ndim, non-contiguous strides, or a NaN halts
  with a blame error naming the decl and clause; a conforming array passes. Run on
  JS/native (M0-era) and BEAM (B3).
- **Array marshalling round-trip:** a `CArray` round-trips Rune→C→Rune (shape,
  dtype, cells) observably unchanged on each backend; zero-copy paths verify the
  buffer is *shared* (mutation visible) where the host model claims it
  (NIF/Panama/typed-array), copied where it claims a copy (port).
- **Assumption tracking for numerics:** `rune assumptions <pipeline>` lists exactly
  the assumed numeric axioms (`cblas_dgemm`, `numpy.std`) in the closure, no more,
  no fewer; `rune check --safe` fails a deployed pipeline with an undischarged
  numeric `A` and passes when shapes are discharged + math explicitly accepted.
  (Inherits R-FFI's soundness property, `ref_docs/wootz/R-FFI.md:539-543`.)
- **Cross-backend conformance (M4):** the `fit.rune` pipeline (or a deterministic
  numeric subset — fixed seed, exact arithmetic where possible) produces equal
  observable results and identical guard/blame on every supported backend; an
  unsupported target (plot on bare WASM) yields a clean `Emit` error.
- **BEAM-first host model (Lambert):** a BLAS dirty-NIF does not stall the
  scheduler under a concurrent workload; a NumPy *port* worker crash is survived
  (supervisor restart), the Rune side sees a typed `IOError`, not a VM death.

## Unblocks (which implement nodes, and what they still need)

- **D4 (this node, Math/Stats/AI/ML/Plotting interop):** **ready-to-build** for
  the *fixed-shape array type, the `CArray` `CRepr`, the BLAS dirty-NIF / native
  C-ABI binding, the dtype/contiguity/finiteness guards, shape-discharge, and the
  assume-tracked numeric math* — all contained repeats of R-FFI's `foreign` +
  three-tier + `A`-set pattern plus one new `CRepr`. *Still needs:* **B4/R-FFI**
  (the whole boundary mechanism this rides), **D3** (the `Double`/`f64` element
  type and the LA interface signatures), **C1/R-SUM** (existential shapes for
  runtime-shaped loads), **D1** (decidable dtype/shape equality for the guards),
  and **B3/BEAM + B2** (the IR + BEAM `Shim` for the dirty-NIF/port realization).
- **D3 (reals/floats + linear-algebra interfaces over BLAS):** **co-developed** —
  D3 states the LA *interface* (shape laws as Rune types); R-INTEROP supplies the
  *BLAS binding* that satisfies it. D3 still needs its `Double` + the typed shape
  laws; R-INTEROP plugs the implementations in behind them.
- **D5 (OTP-class concurrency) / R-OTP:** **aided** — a numeric port worker is a
  supervised process; the port host model is a natural OTP citizen (a `gen_server`
  wrapping CPython). *Still needs* R-OTP for the supervision tree types.
- **M6 (tiered stdlib smoke programs):** **directly delivers** the roadmap's
  "a numeric/ML pipeline via interop … build and run on ≥1 backend"
  (`humble-humming-elephant.md:293-294`) — `fit.rune` *is* that smoke program.
- **The furnace (Savage pedagogy):** **seeds** the polyglot property-test corpus —
  "property-test a bound BLAS/NumPy op against a Rune reference model, then promote
  the guard to assume to discharge" is the furnace on-ramp made concrete for
  numerics. *Still needs* the furnace harness proper.

---

**Status: ready-to-build** for the *fixed-shape* array type (`Array dt sh` as an
R-FFI abstract axiom with shape/dtype indices), the **`CArray` `CRepr`** mirroring
NumPy's `__array_interface__`, the **BLAS dirty-NIF / native C-ABI binding** (no
Python, the cleanest array boundary — the BEAM-first slice), the **dtype/
contiguity/finiteness guards with blame**, **shape conformance discharged by
dependent types** (Futhark/safe-tensor, zero runtime cost), and the **assume-
tracked numeric math** (BLAS/NumPy axioms in the `A`-set, visible in `rune
assumptions`) — all on R-FFI's `foreign` + three-tier + `A`-set machinery with
**one new `CRepr` case, no outer-core growth, and no hash-format bump**.
**Research/parked, clearly labelled:** existential runtime shapes (needs
R-SUM/C1), the `Double`/LA interface (D3), embedded-CPython fate-sharing (BEAM
defaults to out-of-process port/RPC), `owned`-array finalizers per backend,
strided/broadcasting shape relations, VeriFFI-grade discharge of the *numeric
kernels* (vs assume), and autodiff over foreign tensor graphs.

Sources:
[NumPy array interface protocol](https://numpy.org/doc/stable/reference/arrays.interface.html) ·
[Cython buffer protocol & NumPy integration](https://deepwiki.com/cython/cython/4.2-buffer-protocol-and-numpy-integration) ·
[Henriksen, Towards Size-Dependent Types for Array Programming (ARRAY 2021)](https://futhark-lang.org/publications/array21.pdf) ·
[Futhark (Wikipedia)](https://en.wikipedia.org/wiki/Futhark_(programming_language)) ·
[safe-tensor: dependently typed tensor algebra](https://hackage.haskell.org/package/safe-tensor) ·
[Towards size-dependent types (ACM PDF)](https://dl.acm.org/doi/pdf/10.1145/3460944.3464310) ·
[erl_nif (OTP)](https://www.erlang.org/doc/apps/erts/erl_nif.html) ·
[Erlang NIFs guide](https://www.erlang.org/doc/system/nif.html) ·
[Python C-API GIL / threads](https://docs.python.org/3/c-api/threads.html) ·
[PEP 703 — making the GIL optional](https://peps.python.org/pep-0703/) ·
[Real Python — parallel processing / shared-memory arrays](https://realpython.com/python-parallel-processing/) ·
[PyO3 free-threading](https://pyo3.rs/v0.28.3/free-threading)
