# Control Catalog Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A small blessed set of architecture controls (in-region, encrypted-in-transit, least-privilege-IAM, CRDT-convergence-under-all-schedules) PROVEN over a demo model in one self-contained listing, plus a guarded/postulated-tail framework; each control a content-hashed proposition the Assurance Ledger labels; a deliberately over-broad IAM policy rejected at elaboration; a Go-side catalog registry that plugs the flagship set into the ledger gate and (for Plan 4) the CALM emit.

**Architecture:** The catalog is a single self-contained rune listing `listings/ch538_control_catalog.rune` carrying (a) the architecture model as plain datatypes (services with regions, transport edges with a TLS flag, IAM access sets as code lists), (b) the demo model instance (web / relay / store, the two-tab CRDT app), (c) four flagship controls each a `proposition : Eq ... / Conv ...` discharged by a real proof term, and (d) a postulated tail control (the guarded-tail framework, via Plan 2's `postulate ... because ... end`). It elaborates under the existing auto-sweep `TestListingsElaborateAndCheck`, so the proofs are machine-checked for free. A new pure-Go `control/` package names the flagship controls and maps each to its CALM model element; the existing `ledger/` package (Plan 2) reads the listing and labels each control's tier; the existing `ledger.Gate` (Plan 2) enforces that the flagships stay `proven`. The over-broad-IAM rejection is a Go test that loads a mutated source and asserts elaboration fails. Zero kernel change; the convergence proof reuses the verified CvRDT corpus (ch453) verbatim.

**Tech Stack:** The rune surface (datatypes, eliminators, `Eq`/`refl`/`subst`, `postulate` from Plan 2), the `listings/` corpus + its `harness` test sweep, the `ledger/` package (Plan 2), a new `control/` Go package, the Go test harness. Go stdlib only.

## Global Constraints

- **Kernel frozen.** No outer-core changes. Hash-format stays `0x06`. No new `core` constructor. The catalog is a listing plus read-side Go tooling; it adds no kernel machinery and hashes nothing new (a control's proposition identity is `core.HashTerm(Ty)`, its proof identity is `core.HashTerm(Body)`, both already computed by Plan 2's ledger).
- **Reuse, do not rebuild.** The convergence control reuses ch453's proven max-register semilattice algebra (`natMax`, `maxComm`/`maxIdem`/`maxAssoc`, `Reg`/`val`/`merge`, `mergeComm`/`mergeIdem`/`mergeAssoc`) verbatim. Tier labeling reuses `ledger.Build` / `ledger.Tier` (Plan 2). Postulate-tail reuses Plan 2's `postulate ... because "..." end` keyword. The architecture model mirrors the agnostic kinds in `infra/infra.go` (`iam`, `kv`, `object`, `compute`) but is a wootz-side datatype, not a Go `Resource`.
- **The proof IS the gate.** Every flagship control's proposition is discharged by `refl` (or a packaged proof term) that only type-checks when the claim holds. An over-broad IAM policy makes `eqCodes granted needed` reduce to `false`, so `Eq Bool ... true` has no `refl` and elaboration fails. The teeth are definitional, not a runtime check.
- **Process standards.** Contextual keywords only (Plan 2's `postulate`/`because` are already contextual; this plan adds NO new keyword). NO em or en dashes in any code, comment, or doc (use `,` `(` `)` or rewrite). Conventional Commits. Verify before claiming done. Run the FULL `go test ./...` before tagging (a new listing must elaborate under the auto-sweep, and the new keyword from Plan 2 must already be green).
- **Backends unaffected.** The catalog is proofs plus tooling, not codegen, so `harness/backend_conformance_test.go` must stay green unchanged. The listing's optional `main` runs under the existing run sweep, byte-identical, on the source backends only.
- **Depends on Plan 2.** This plan assumes the `ledger/` package and the `postulate ... because "..." end` surface form are landed and green (Plan 2, `2026-06-26-assurance-ledger.md`, marked DONE at v3.332.0). If `postulate` is not present, Task 5 and the postulate-tier assertions in Task 8 cannot be implemented; stop and reconcile.

---

### Task 1: The architecture model + the in-region control

Create the catalog listing with the model datatypes, the demo instance, the decidable kit, and the first flagship control (in-region). The whole file must elaborate (the auto-sweep checks it) and the control's fold must compute to `true` so `refl` discharges the proposition.

**Files:**
- Create: `listings/ch538_control_catalog.rune`
- Create: `harness/control_catalog_test.go`

**Interfaces:**
- Consumes: the rune prelude builtins (`Eq`, `refl`, `NatElim`, `BoolElim`, `builtin nat`); the `loadListing`/`normalizesTo` helpers in `harness/listings_test.go` (same `harness` package).
- Produces (rune defs later tasks and the ledger rely on by these exact names): `usEast : Nat`, `demoServices : SvcList`, `allInRegion : Nat -> SvcList -> Bool`, and the flagship control `inRegionProof : Eq Bool (allInRegion usEast demoServices) true`.

- [ ] **Step 1: Write the listing**

Create `listings/ch538_control_catalog.rune`:

```
-- Chapter 538 - the Wavelet control catalog (Plan 3 of the Wavelet beta).
--
-- A small blessed set of architecture controls, PROVEN over a demo model: the
-- two-tab collaborative-CRDT app, whose CALM nodes are {web, relay, store}. Each
-- control is a content-hashed PROPOSITION (a type) discharged by a real proof
-- term; the Assurance Ledger (rune ledger) reads this file and labels each one
-- proven. Flagship controls landed in this chapter:
--   in-region, encrypted-in-transit, least-privilege-IAM, convergence.
-- A postulated TAIL control (the guarded-tail framework) is added in Task 5.
--
-- The proofs are the gate: each fold computes to `true` (or the merge laws hold),
-- so `refl` type-checks only when the claim holds. An over-broad IAM policy makes
-- the IAM fold reduce to `false`, so its `refl` fails to elaborate (a compile
-- error, not a runtime check) - see harness/control_catalog_test.go.

data Unit : U is unit : Unit end
data Bool : U is false : Bool | true : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ

-- ===== decidable kit =====
-- Boolean Nat equality (the ch75 pattern): eqNat a b = true iff a and b are the
-- same numeral, by nested NatElim.
eqNat : Nat -> Nat -> Bool is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat -> Bool end)
      (fn (m : Nat) is
         NatElim (fn (y : Nat) is Bool end) true (fn (j : Nat) (ih : Bool) is false end) m end)
      (fn (k : Nat) (eqK : Nat -> Bool) is
         fn (m : Nat) is
           NatElim (fn (y : Nat) is Bool end) false (fn (j : Nat) (ih : Bool) is eqK j end) m end end)
      a b
  end
end

-- andB a b = if a then b else false (BoolElim: motive, false-case, true-case, scrutinee).
andB : Bool -> Bool -> Bool is
  fn (a : Bool) (b : Bool) is BoolElim (fn (w : Bool) is Bool end) false b a end
end

-- ===== the architecture model: services pinned to a region =====
-- A service is an id paired with the region it is deployed in.
data Svc : U is svc : Nat -> Nat -> Svc end
data SvcList : U is snil : SvcList | scons : Svc -> SvcList -> SvcList end

region : Svc -> Nat is
  fn (s : Svc) is SvcElim (fn (w : Svc) is Nat end) (fn (id : Nat) (r : Nat) is r end) s end
end

-- allInRegion pin xs = true iff every service in xs is deployed in region `pin`.
allInRegion : Nat -> SvcList -> Bool is
  fn (pin : Nat) (xs : SvcList) is
    SvcListElim (fn (w : SvcList) is Bool end)
      true
      (fn (h : Svc) (t : SvcList) (ih : Bool) is andB (eqNat (region h) pin) ih end)
      xs
  end
end

-- ===== the demo model instance =====
-- Region ids: us-east = 0. The three demo services (web=0, relay=1, store=2) are
-- all pinned to us-east.
usEast : Nat is zero end
web   : Svc is svc 0 usEast end
relay : Svc is svc 1 usEast end
store : Svc is svc 2 usEast end
demoServices : SvcList is scons web (scons relay (scons store snil)) end

-- ===== flagship control: IN-REGION =====
-- PROPOSITION: every demo service is in the pinned region (us-east).
-- PROOF: the fold computes to `true`, so refl discharges it.
inRegionProof : Eq Bool (allInRegion usEast demoServices) true is refl end
```

- [ ] **Step 2: Write the failing smoke test**

Create `harness/control_catalog_test.go`:

```go
package harness

import "testing"

// TestControlCatalogElaborates pins that the catalog listing loads and that the
// in-region control's fold computes to true (so its refl is a real proof).
func TestControlCatalogElaborates(t *testing.T) {
	s := loadListing(t, "ch538_control_catalog.rune")
	normalizesTo(t, s, `allInRegion usEast demoServices`, "true")
}
```

- [ ] **Step 3: Run the elaborate gate + the smoke**

Run: `go test -run 'TestListingsElaborateAndCheck/ch538|TestControlCatalogElaborates' ./harness/`
Expected: PASS. (If `allInRegion usEast demoServices` does not normalize to `true`, a service region or the fold is wrong; if elaboration fails, read the diagnostic and fix the offending def. The `inRegionProof : ... is refl end` only type-checks because the fold reduces to `true`.)

- [ ] **Step 4: Run the full suite (a new listing must not break the sweep)**

Run: `go test ./harness/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add listings/ch538_control_catalog.rune harness/control_catalog_test.go
git commit -m "feat(control): catalog listing + in-region flagship control"
```

---

### Task 2: The encrypted-in-transit control

Add transport edges (a TLS flag per edge) and the second flagship control: every edge in the demo model is encrypted.

**Files:**
- Modify: `listings/ch538_control_catalog.rune` (append the edge model + control)
- Modify: `harness/control_catalog_test.go` (extend the smoke)

**Interfaces:**
- Consumes: `Bool`, `andB` (Task 1).
- Produces: `demoEdges : EdgeList`, `allEncrypted : EdgeList -> Bool`, and the flagship `encryptedProof : Eq Bool (allEncrypted demoEdges) true`.

- [ ] **Step 1: Append the edge model + control to the listing**

Append to `listings/ch538_control_catalog.rune` (after the in-region section):

```
-- ===== the model: transport edges carry a TLS flag =====
-- An edge connects two service ids and records whether the link is encrypted.
data Edge : U is edge : Nat -> Nat -> Bool -> Edge end
data EdgeList : U is enil : EdgeList | econs : Edge -> EdgeList -> EdgeList end

tls : Edge -> Bool is
  fn (e : Edge) is EdgeElim (fn (w : Edge) is Bool end) (fn (f : Nat) (t : Nat) (s : Bool) is s end) e end
end

-- allEncrypted xs = true iff every edge in xs is TLS-protected.
allEncrypted : EdgeList -> Bool is
  fn (xs : EdgeList) is
    EdgeListElim (fn (w : EdgeList) is Bool end)
      true
      (fn (h : Edge) (t : EdgeList) (ih : Bool) is andB (tls h) ih end)
      xs
  end
end

-- demo edges: web -> relay (WebRTC/DTLS) and relay -> store (TLS), both encrypted.
demoEdges : EdgeList is econs (edge 0 1 true) (econs (edge 1 2 true) enil) end

-- ===== flagship control: ENCRYPTED-IN-TRANSIT =====
-- PROPOSITION: every transport edge in the demo model is encrypted.
-- PROOF: the fold computes to `true`.
encryptedProof : Eq Bool (allEncrypted demoEdges) true is refl end
```

- [ ] **Step 2: Extend the smoke test**

Add to `harness/control_catalog_test.go`, inside `TestControlCatalogElaborates` after the existing assertion:

```go
	normalizesTo(t, s, `allEncrypted demoEdges`, "true")
```

- [ ] **Step 3: Run the elaborate gate + the smoke**

Run: `go test -run 'TestListingsElaborateAndCheck/ch538|TestControlCatalogElaborates' ./harness/`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add listings/ch538_control_catalog.rune harness/control_catalog_test.go
git commit -m "feat(control): encrypted-in-transit flagship control"
```

---

### Task 3: The least-privilege-IAM control

Model IAM access as a set of grant codes and prove least privilege as set-equality of the granted policy and the access the workload actually needs. This is the control whose teeth Task 6 demonstrates.

**Files:**
- Modify: `listings/ch538_control_catalog.rune` (append the IAM model + control)
- Modify: `harness/control_catalog_test.go` (extend the smoke)

**Interfaces:**
- Consumes: `Nat`, `Bool`, `andB`, `eqNat` (Task 1).
- Produces: `needed : CodeList`, `granted : CodeList`, `eqCodes : CodeList -> CodeList -> Bool`, and the flagship `leastPrivProof : Eq Bool (eqCodes granted needed) true`.

- [ ] **Step 1: Append the IAM model + control to the listing**

Append to `listings/ch538_control_catalog.rune`:

```
-- ===== the model: IAM access as a set of grant codes =====
-- A grant code names one (action, resource) capability the relay's erased code
-- can perform; e.g. 10 = kv:get, 11 = kv:set. An access set is the list of codes.
data CodeList : U is cnil : CodeList | ccons : Nat -> CodeList -> CodeList end

-- eqCodes xs ys = true iff the two access sets are positionally equal (the demo
-- model keeps both in the same canonical order, so set-equality is list-equality).
eqCodes : CodeList -> CodeList -> Bool is
  fn (xs : CodeList) is
    CodeListElim (fn (w : CodeList) is CodeList -> Bool end)
      (fn (ys : CodeList) is
         CodeListElim (fn (w : CodeList) is Bool end)
           true
           (fn (h : Nat) (t : CodeList) (ih : Bool) is false end)
           ys end)
      (fn (x : Nat) (xt : CodeList) (eqXt : CodeList -> Bool) is
         fn (ys : CodeList) is
           CodeListElim (fn (w : CodeList) is Bool end)
             false
             (fn (y : Nat) (yt : CodeList) (ih : Bool) is andB (eqNat x y) (eqXt yt) end)
             ys end end)
      xs
  end
end

-- `needed` is the access the relay's erased code actually performs: kv:get, kv:set.
-- `granted` is the IAM policy attached to the relay's identity. LEAST PRIVILEGE is
-- granted == needed: no capability is granted that the code does not use (no excess)
-- and the code is not denied a capability it needs (no deficit).
needed  : CodeList is ccons 10 (ccons 11 cnil) end
granted : CodeList is ccons 10 (ccons 11 cnil) end

-- ===== flagship control: LEAST-PRIVILEGE-IAM =====
-- PROPOSITION: the granted policy is exactly the access the workload needs.
-- PROOF: the decision computes to `true`. An over-broad `granted` (an extra code)
-- makes eqCodes reduce to `false`, so this refl would fail to elaborate (Task 6).
leastPrivProof : Eq Bool (eqCodes granted needed) true is refl end
```

- [ ] **Step 2: Extend the smoke test**

Add to `TestControlCatalogElaborates`:

```go
	normalizesTo(t, s, `eqCodes granted needed`, "true")
```

- [ ] **Step 3: Run the elaborate gate + the smoke**

Run: `go test -run 'TestListingsElaborateAndCheck/ch538|TestControlCatalogElaborates' ./harness/`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add listings/ch538_control_catalog.rune harness/control_catalog_test.go
git commit -m "feat(control): least-privilege-IAM flagship control (set-equality)"
```

---

### Task 4: The convergence-under-all-schedules control + a runnable witness

Add the fourth flagship by reusing ch453's verified max-register CvRDT algebra verbatim, then package its three semilattice laws into one content-hashed control. Add a `main` that prints the convergence witness so the listing also RUNS (the Lambert spirit gate).

**Files:**
- Modify: `listings/ch538_control_catalog.rune` (append the CvRDT algebra + the `Conv` bundle + `main`)
- Modify: `harness/control_catalog_test.go` (assert the witness normalizes)

**Interfaces:**
- Consumes: `Nat`, `Eq`, `subst`, `refl`, `NatElim`, `Unit`, `pureIO`/`bindIO`/`IO` (prelude), `printNat` (foreign).
- Produces: `Reg`, `merge : Reg -> Reg -> Reg`, `mergeComm`/`mergeIdem`/`mergeAssoc`, and the flagship `convergesProof : Conv Reg merge`.

- [ ] **Step 1: Append the verified CvRDT algebra (copied from ch453)**

Append to `listings/ch538_control_catalog.rune`. Copy the bodies below verbatim from the proven `listings/ch453_max_register_crdt.rune` (lines 17-146); they are reproduced here in full so this task is self-contained:

```
-- ===== convergence: the verified max-register CvRDT (reused from ch453) =====
cong : (A : U) -> (B : U) -> (f : A -> B) -> (x : A) -> (y : A) -> Eq A x y -> Eq B (f x) (f y) is
  fn (A : U) (B : U) (f : A -> B) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq B (f x) (f z) end) refl
  end
end
symEq : (A : U) -> (x : A) -> (y : A) -> Eq A x y -> Eq A y x is
  fn (A : U) (x : A) (y : A) (p : Eq A x y) is
    subst A x y p (fn (z : A) is Eq A z x end) refl
  end
end

natMax : Nat -> Nat -> Nat is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat -> Nat end)
      (fn (m : Nat) is m end)
      (fn (k : Nat) (maxK : Nat -> Nat) is
         fn (m : Nat) is
           NatElim (fn (y : Nat) is Nat end) (succ k) (fn (j : Nat) (ih2 : Nat) is succ (maxK j) end) m
         end end)
      a b
  end
end
maxZeroR : (a : Nat) -> Eq Nat (natMax a zero) a is
  fn (a : Nat) is
    NatElim (fn (x : Nat) is Eq Nat (natMax x zero) x end) refl (fn (k : Nat) (ih : Eq Nat (natMax k zero) k) is refl end) a
  end
end
maxComm : (a : Nat) -> (b : Nat) -> Eq Nat (natMax a b) (natMax b a) is
  fn (a : Nat) is
    NatElim (fn (x : Nat) is (b : Nat) -> Eq Nat (natMax x b) (natMax b x) end)
      (fn (b : Nat) is symEq Nat (natMax b zero) b (maxZeroR b) end)
      (fn (k : Nat) (ih : (b : Nat) -> Eq Nat (natMax k b) (natMax b k)) is
         fn (b : Nat) is
           NatElim (fn (y : Nat) is Eq Nat (natMax (succ k) y) (natMax y (succ k)) end)
             refl
             (fn (j : Nat) (ihj : Eq Nat (natMax (succ k) j) (natMax j (succ k))) is
                cong Nat Nat succ (natMax k j) (natMax j k) (ih j) end)
             b
         end
       end)
      a
  end
end
maxIdem : (a : Nat) -> Eq Nat (natMax a a) a is
  fn (a : Nat) is
    NatElim (fn (x : Nat) is Eq Nat (natMax x x) x end)
      refl
      (fn (k : Nat) (ih : Eq Nat (natMax k k) k) is cong Nat Nat succ (natMax k k) k ih end)
      a
  end
end
maxAssoc : (a : Nat) -> (b : Nat) -> (c : Nat) ->
           Eq Nat (natMax (natMax a b) c) (natMax a (natMax b c)) is
  fn (a : Nat) is
    NatElim (fn (x : Nat) is (b : Nat) -> (c : Nat) -> Eq Nat (natMax (natMax x b) c) (natMax x (natMax b c)) end)
      (fn (b : Nat) (c : Nat) is refl end)
      (fn (k : Nat) (ih : (b : Nat) -> (c : Nat) -> Eq Nat (natMax (natMax k b) c) (natMax k (natMax b c))) is
         fn (b : Nat) is
           NatElim (fn (y : Nat) is (c : Nat) -> Eq Nat (natMax (natMax (succ k) y) c) (natMax (succ k) (natMax y c)) end)
             (fn (c : Nat) is refl end)
             (fn (j : Nat) (ihb : (c : Nat) -> Eq Nat (natMax (natMax (succ k) j) c) (natMax (succ k) (natMax j c))) is
                fn (c : Nat) is
                  NatElim (fn (z : Nat) is Eq Nat (natMax (natMax (succ k) (succ j)) z) (natMax (succ k) (natMax (succ j) z)) end)
                    refl
                    (fn (i : Nat) (ihc : Eq Nat (natMax (natMax (succ k) (succ j)) i) (natMax (succ k) (natMax (succ j) i))) is
                       cong Nat Nat succ (natMax (natMax k j) i) (natMax k (natMax j i)) (ih j i) end)
                    c
                end
              end)
             b
         end
       end)
      a
  end
end

data Reg : U is reg : Nat -> Reg end
val : Reg -> Nat is fn (r : Reg) is RegElim (fn (w : Reg) is Nat end) (fn (n : Nat) is n end) r end end
merge : Reg -> Reg -> Reg is fn (x : Reg) (y : Reg) is reg (natMax (val x) (val y)) end end

mergeComm : (x : Reg) -> (y : Reg) -> Eq Reg (merge x y) (merge y x) is
  fn (x : Reg) is
    RegElim (fn (wx : Reg) is (y : Reg) -> Eq Reg (merge wx y) (merge y wx) end)
      (fn (a : Nat) is
         fn (y : Reg) is
           RegElim (fn (wy : Reg) is Eq Reg (merge (reg a) wy) (merge wy (reg a)) end)
             (fn (b : Nat) is cong Nat Reg reg (natMax a b) (natMax b a) (maxComm a b) end)
             y
         end end)
      x
  end
end
mergeIdem : (x : Reg) -> Eq Reg (merge x x) x is
  fn (x : Reg) is
    RegElim (fn (wx : Reg) is Eq Reg (merge wx wx) wx end)
      (fn (a : Nat) is cong Nat Reg reg (natMax a a) a (maxIdem a) end)
      x
  end
end
mergeAssoc : (x : Reg) -> (y : Reg) -> (z : Reg) ->
             Eq Reg (merge (merge x y) z) (merge x (merge y z)) is
  fn (x : Reg) is
    RegElim (fn (wx : Reg) is (y : Reg) -> (z : Reg) ->
        Eq Reg (merge (merge wx y) z) (merge wx (merge y z)) end)
      (fn (a : Nat) is
         fn (y : Reg) is
           RegElim (fn (wy : Reg) is (z : Reg) ->
               Eq Reg (merge (merge (reg a) wy) z) (merge (reg a) (merge wy z)) end)
             (fn (b : Nat) is
                fn (z : Reg) is
                  RegElim (fn (wz : Reg) is
                      Eq Reg (merge (merge (reg a) (reg b)) wz) (merge (reg a) (merge (reg b) wz)) end)
                    (fn (c : Nat) is
                       cong Nat Reg reg (natMax (natMax a b) c) (natMax a (natMax b c)) (maxAssoc a b c) end)
                    z
                end end)
             y
         end end)
      x
  end
end
```

- [ ] **Step 2: Append the `Conv` bundle + the runnable witness**

Append to `listings/ch538_control_catalog.rune`:

```
-- ===== flagship control: CONVERGENCE-UNDER-ALL-SCHEDULES =====
-- The three semilattice laws on `merge` (commutative + idempotent + associative)
-- ARE the CvRDT convergence criterion: replicas reach the same state regardless of
-- gossip order, duplication, or grouping. Conv packages them into ONE content-hashed
-- proposition so the ledger shows a single `convergesProof : proven` control.
data Conv : (R : U) -> (m : R -> R -> R) -> U is
  conv : (R : U) -> (m : R -> R -> R)
      -> ((x : R) -> (y : R) -> Eq R (m x y) (m y x))
      -> ((x : R) -> Eq R (m x x) x)
      -> ((x : R) -> (y : R) -> (z : R) -> Eq R (m (m x y) z) (m x (m y z)))
      -> Conv R m
end

convergesProof : Conv Reg merge is conv Reg merge mergeComm mergeIdem mergeAssoc end

-- ===== a runnable witness (the Lambert spirit gate: it deploys + runs) =====
-- Two replicas hold 3 and 7; either merge order converges to 7 = max(3, 7).
foreign printNat : Nat -> IO Nat end
r3 : Reg is reg (succ (succ (succ zero))) end
r7 : Reg is reg (succ (succ (succ (succ (succ (succ (succ zero))))))) end

main : IO Unit is
  bindIO Nat Unit (printNat (val (merge r3 r7))) (fn (u1 : Nat) is
    bindIO Nat Unit (printNat (val (merge r7 r3))) (fn (u2 : Nat) is
      pureIO Unit unit
    end)
  end)
end
```

- [ ] **Step 3: Assert the witness normalizes**

Add to `TestControlCatalogElaborates`:

```go
	// either merge order converges to max(3, 7) = 7
	normalizesTo(t, s, `val (merge r3 r7)`, "7")
	normalizesTo(t, s, `val (merge r7 r3)`, "7")
```

- [ ] **Step 4: Run the elaborate gate + the smoke**

Run: `go test -run 'TestListingsElaborateAndCheck/ch538|TestControlCatalogElaborates' ./harness/`
Expected: PASS. (If `Conv` is rejected, check the strict-positivity / uniform-parameter shape: `R` and `m` are uniform parameters, the constructor arguments are non-recursive function types returning `Eq`, so it is strictly positive. If `conv ...` mismatches, the law argument types must read exactly as the `Conv` constructor declares them.)

- [ ] **Step 5: Run the full suite**

Run: `go test ./harness/`
Expected: PASS (the listing also runs under the existing run/emit sweeps via `main`).

- [ ] **Step 6: Commit**

```bash
git add listings/ch538_control_catalog.rune harness/control_catalog_test.go
git commit -m "feat(control): convergence flagship (reuses ch453 CvRDT) + runnable witness"
```

---

### Task 5: The guarded-tail framework (a postulated tail control)

Not every control can be proven statically. The guarded-tail framework states such a control as a `postulate ... because "..." end` (Plan 2): a labeled, attributable, upgradeable debt. Add one tail control to the catalog. The ledger will label it `postulate` while the four flagships stay `proven`.

**Files:**
- Modify: `listings/ch538_control_catalog.rune` (append a `foreign` live reading + the postulated control)
- Modify: `harness/control_catalog_test.go` (assert it still elaborates)

**Interfaces:**
- Consumes: Plan 2's `postulate NAME : TYPE because "REASON" end` surface form; `Nat`, `Eq`, `usEast` (Task 1).
- Produces: `foreign liveRegion : Nat`, and the tail control `liveInRegion : Eq Nat liveRegion usEast` (bodiless, postulated).

- [ ] **Step 1: Append the tail control to the listing**

Append to `listings/ch538_control_catalog.rune`:

```
-- ===== the guarded-tail framework: a postulated control (a labeled debt) =====
-- The in-region control above is proven against the STATIC model. The matching
-- claim about the LIVE deployment - that the region the cloud actually placed the
-- service in equals the pinned region - cannot be proven yet (the provider region
-- API is not modeled). It is stated as a POSTULATE: an axiom with a reason, which
-- the Assurance Ledger labels `postulate`, attributes via git blame, and tracks for
-- upgrade. When the live region read is modeled, a proof of the SAME proposition
-- hash upgrades it postulated -> proven (Plan 2's upgrade detector).
foreign liveRegion : Nat end
postulate liveInRegion : Eq Nat liveRegion usEast because "live cloud region read not yet modeled (provider API); attested out of band" end
```

- [ ] **Step 2: Assert the listing still elaborates**

The existing `TestControlCatalogElaborates` already loads the file; a postulate is a bodiless assumed definition, so the load must still succeed. No new normalization assertion is needed (the proposition has no proof to compute). Confirm the sweep:

Run: `go test -run 'TestListingsElaborateAndCheck/ch538|TestControlCatalogElaborates' ./harness/`
Expected: PASS. (If `postulate` parses as an identifier, Plan 2 is not landed in this build - stop and reconcile per the Global Constraints.)

- [ ] **Step 3: Run the full suite (the keyword must already be contextual)**

Run: `go test ./...`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add listings/ch538_control_catalog.rune harness/control_catalog_test.go
git commit -m "feat(control): postulated tail control (guarded-tail framework)"
```

---

### Task 6: The over-broad IAM policy is rejected at elaboration

Prove the least-privilege control has teeth: a deliberately over-broad `granted` set makes `eqCodes granted needed` reduce to `false`, so `leastPrivProof : Eq Bool ... true is refl end` fails to elaborate. This cannot live in `listings/` (it would fail the auto-sweep), so it is a Go test that loads a mutated source string and asserts the load errors.

**Files:**
- Modify: `harness/control_catalog_test.go` (add the negative test)

**Interfaces:**
- Consumes: `session.New()`, the session loader (`LoadString`/`LoadSource` - match the name the other `harness` tests use; see how `loadListing` loads source in `harness/listings_test.go`).
- Produces: `TestOverBroadIAMRejected`.

- [ ] **Step 1: Write the negative test**

Add to `harness/control_catalog_test.go` (and add `"goforge.dev/rune/v3/internal/session"` plus `"strings"` to the imports):

```go
// TestOverBroadIAMRejected proves the least-privilege control has teeth: an
// over-broad granted policy (an extra grant code 99 the workload never uses)
// makes eqCodes granted needed reduce to false, so a refl proof of
// `Eq Bool (eqCodes granted needed) true` does NOT type-check. The over-broad
// policy is REJECTED at elaboration - a compile error, not a runtime check.
func TestOverBroadIAMRejected(t *testing.T) {
	const src = `
data Unit : U is unit : Unit end
data Bool : U is false : Bool | true : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
eqNat : Nat -> Nat -> Bool is
  fn (a : Nat) (b : Nat) is
    NatElim (fn (x : Nat) is Nat -> Bool end)
      (fn (m : Nat) is NatElim (fn (y : Nat) is Bool end) true (fn (j : Nat) (ih : Bool) is false end) m end)
      (fn (k : Nat) (eqK : Nat -> Bool) is fn (m : Nat) is NatElim (fn (y : Nat) is Bool end) false (fn (j : Nat) (ih : Bool) is eqK j end) m end end)
      a b
  end
end
andB : Bool -> Bool -> Bool is fn (a : Bool) (b : Bool) is BoolElim (fn (w : Bool) is Bool end) false b a end end
data CodeList : U is cnil : CodeList | ccons : Nat -> CodeList -> CodeList end
eqCodes : CodeList -> CodeList -> Bool is
  fn (xs : CodeList) is
    CodeListElim (fn (w : CodeList) is CodeList -> Bool end)
      (fn (ys : CodeList) is CodeListElim (fn (w : CodeList) is Bool end) true (fn (h : Nat) (t : CodeList) (ih : Bool) is false end) ys end)
      (fn (x : Nat) (xt : CodeList) (eqXt : CodeList -> Bool) is fn (ys : CodeList) is CodeListElim (fn (w : CodeList) is Bool end) false (fn (y : Nat) (yt : CodeList) (ih : Bool) is andB (eqNat x y) (eqXt yt) end) ys end end)
      xs
  end
end
needed  : CodeList is ccons 10 (ccons 11 cnil) end
granted : CodeList is ccons 10 (ccons 11 (ccons 99 cnil)) end
leastPrivProof : Eq Bool (eqCodes granted needed) true is refl end
`
	s := session.New()
	if _, err := s.LoadSource(src); err == nil {
		t.Fatalf("an over-broad IAM policy must be rejected: eqCodes reduces to false, so the refl proof should not type-check")
	}
}
```

(Use the exact session loader the rest of `harness` uses. If `loadListing` calls `session.LoadString(name, src)` or a package helper rather than `s.LoadSource(src)`, mirror that call here. The assertion is only that loading the over-broad source returns a non-nil error.)

- [ ] **Step 2: Run the negative test**

Run: `go test -run TestOverBroadIAMRejected ./harness/`
Expected: PASS (the load returns an error; the diagnostic mentions that `refl` does not prove `Eq Bool false true`, or the conversion mismatch).

- [ ] **Step 3: Commit**

```bash
git add harness/control_catalog_test.go
git commit -m "test(control): over-broad IAM policy rejected at elaboration"
```

---

### Task 7: The `control/` registry (flagship set + CALM element map)

A pure-Go package that names the flagship controls and maps each to the CALM model element it attaches to. `ledger.Gate` consumes the flagship names; Plan 4 (CALM emit) consumes the element map. One source of truth so the gate, the catalog listing, and the CALM projection cannot drift.

**Files:**
- Create: `control/control.go`
- Test: `control/control_test.go`

**Interfaces:**
- Consumes: nothing (a static registry; the def names must match the catalog listing's control defs from Tasks 1-5).
- Produces:
  - `type Control struct { Name string; Kind string; Element string }` - `Name` is the rune def name, `Kind` is the control class, `Element` is the CALM node/relationship id.
  - `func Catalog() []Control`
  - `func Flagships() []string` - the names whose ledger tier must stay `proven`.
  - `func AllowedPostulates() []string` - the tail controls signed off as debts.

- [ ] **Step 1: Write the failing test**

```go
// control/control_test.go
package control

import "testing"

func TestFlagshipsMatchCatalog(t *testing.T) {
	cat := Catalog()
	if len(cat) == 0 {
		t.Fatal("catalog is empty")
	}
	// every flagship name must be a proven control in the catalog
	provenNames := map[string]bool{}
	for _, c := range cat {
		if c.Kind != "tail" {
			provenNames[c.Name] = true
		}
	}
	for _, f := range Flagships() {
		if !provenNames[f] {
			t.Fatalf("flagship %q is not a non-tail control in the catalog", f)
		}
	}
	// the four flagship classes are present and named correctly
	want := map[string]string{
		"inRegionProof":   "in-region",
		"encryptedProof":  "encrypted-in-transit",
		"leastPrivProof":  "least-privilege-iam",
		"convergesProof":  "convergence",
	}
	got := map[string]string{}
	for _, c := range cat {
		got[c.Name] = c.Kind
	}
	for name, kind := range want {
		if got[name] != kind {
			t.Fatalf("control %q: want kind %q, got %q", name, kind, got[name])
		}
	}
}

func TestAllowedPostulatesAreTailControls(t *testing.T) {
	tail := map[string]bool{}
	for _, c := range Catalog() {
		if c.Kind == "tail" {
			tail[c.Name] = true
		}
	}
	for _, p := range AllowedPostulates() {
		if !tail[p] {
			t.Fatalf("allowed postulate %q is not a tail control", p)
		}
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test ./control/`
Expected: FAIL (package `control` does not exist).

- [ ] **Step 3: Implement the registry**

```go
// control/control.go
// Package control is the Wavelet control catalog registry: the single source of
// truth tying each blessed control's rune definition name to its control class
// and the CALM model element it attaches to. The Assurance Ledger gate (Plan 2)
// consumes Flagships()/AllowedPostulates() to enforce the assurance policy; the
// CALM emit (Plan 4) consumes Catalog() to attach each control to its node or
// relationship. The def names here MUST match the controls in
// listings/ch538_control_catalog.rune.
package control

// Control names one blessed control: its rune definition, its class, and the CALM
// model element (a node or a relationship id) it is an assurance about.
type Control struct {
	Name    string // the rune definition name in the catalog listing
	Kind    string // the control class ("in-region", "tail", ...)
	Element string // the CALM node/relationship id this control attaches to
}

// Catalog is the blessed control set proven (or postulated) on the demo model.
func Catalog() []Control {
	return []Control{
		{Name: "inRegionProof", Kind: "in-region", Element: "store"},
		{Name: "encryptedProof", Kind: "encrypted-in-transit", Element: "web->relay"},
		{Name: "leastPrivProof", Kind: "least-privilege-iam", Element: "relay"},
		{Name: "convergesProof", Kind: "convergence", Element: "store"},
		{Name: "liveInRegion", Kind: "tail", Element: "store"},
	}
}

// Flagships are the controls whose ledger tier must stay `proven` or CI fails.
func Flagships() []string {
	out := []string{}
	for _, c := range Catalog() {
		if c.Kind != "tail" {
			out = append(out, c.Name)
		}
	}
	return out
}

// AllowedPostulates are the tail controls signed off as labeled debts (allowed to
// remain `postulate` for now; the ledger still surfaces them honestly).
func AllowedPostulates() []string {
	out := []string{}
	for _, c := range Catalog() {
		if c.Kind == "tail" {
			out = append(out, c.Name)
		}
	}
	return out
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test ./control/`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add control/control.go control/control_test.go
git commit -m "feat(control): catalog registry (flagship set + CALM element map)"
```

---

### Task 8: The catalog plugs into the Assurance Ledger and its gate

Prove the criteria end to end: load the catalog listing, build the ledger (Plan 2), assert the four flagships are `proven` and the tail is `postulate`, run `ledger.Gate` with the flagship set and pass, then demote a flagship and assert the gate fails.

**Files:**
- Modify: `harness/control_catalog_test.go` (add the ledger integration test)

**Interfaces:**
- Consumes: `ledger.Build(s)` / `ledger.Tier` / `ledger.Entry` / `ledger.Gate` / `ledger.GateConfig` (Plan 2, `ledger/`); `control.Flagships()` / `control.AllowedPostulates()` (Task 7); `loadListing` (harness); `session.New()` + the loader.
- Produces: `TestCatalogLedgerTiers`, `TestCatalogGatePassesAndFails`.

- [ ] **Step 1: Write the integration tests**

Add to `harness/control_catalog_test.go` (add `"goforge.dev/rune/v3/ledger"` and `"goforge.dev/rune/v3/control"` to the imports):

```go
// findEntry returns the ledger entry for a control name.
func findEntry(es []ledger.Entry, name string) (ledger.Entry, bool) {
	for _, e := range es {
		if e.Name == name {
			return e, true
		}
	}
	return ledger.Entry{}, false
}

// TestCatalogLedgerTiers: the four flagships are proven, the tail is postulate.
func TestCatalogLedgerTiers(t *testing.T) {
	s := loadListing(t, "ch538_control_catalog.rune")
	es := ledger.Build(s)
	for _, name := range control.Flagships() {
		e, ok := findEntry(es, name)
		if !ok {
			t.Fatalf("flagship %q absent from the ledger", name)
		}
		if e.Tier != ledger.Proven {
			t.Fatalf("flagship %q want proven, got %v", name, e.Tier)
		}
	}
	for _, name := range control.AllowedPostulates() {
		e, ok := findEntry(es, name)
		if !ok {
			t.Fatalf("tail control %q absent from the ledger", name)
		}
		if e.Tier != ledger.Postulate {
			t.Fatalf("tail control %q want postulate, got %v", name, e.Tier)
		}
	}
}

// TestCatalogGatePassesAndFails: the catalog passes the assurance gate; demoting a
// flagship to a postulate (the over-broad / unproven case) fails it.
func TestCatalogGatePassesAndFails(t *testing.T) {
	cfg := ledger.GateConfig{
		Flagships:         control.Flagships(),
		AllowedPostulates: control.AllowedPostulates(),
	}

	s := loadListing(t, "ch538_control_catalog.rune")
	es := ledger.Build(s)
	if errs := ledger.Gate(es, nil, cfg); len(errs) != 0 {
		t.Fatalf("the catalog must pass its own gate, got %v", errs)
	}

	// Demote a flagship: replace convergesProof's proof with a postulate of the
	// same proposition (a flagship leaving `proven`). The gate must fail.
	const demoted = `
data Unit : U is unit : Unit end
data Bool : U is false : Bool | true : Bool end
data Nat : U is zero : Nat | succ : Nat -> Nat end
builtin nat Nat zero succ
data Reg : U is reg : Nat -> Reg end
merge : Reg -> Reg -> Reg is fn (x : Reg) (y : Reg) is x end end
data Conv : (R : U) -> (m : R -> R -> R) -> U is
  conv : (R : U) -> (m : R -> R -> R)
      -> ((x : R) -> (y : R) -> Eq R (m x y) (m y x))
      -> ((x : R) -> Eq R (m x x) x)
      -> ((x : R) -> (y : R) -> (z : R) -> Eq R (m (m x y) z) (m x (m y z)))
      -> Conv R m
end
postulate convergesProof : Conv Reg merge because "demoted for the gate test" end
`
	ds := session.New()
	if _, err := ds.LoadSource(demoted); err != nil {
		t.Fatalf("demoted source must still load (it is well-typed): %v", err)
	}
	des := ledger.Build(ds)
	if errs := ledger.Gate(des, nil, cfg); len(errs) == 0 {
		t.Fatalf("a flagship left at postulate must fail the gate")
	}
}
```

(`ledger.Proven` / `ledger.Postulate` / `ledger.Tier` / `ledger.Entry` / `ledger.Gate` / `ledger.GateConfig` are the Plan 2 API in `ledger/ledger.go` and `ledger/gate.go`. If `ledger.Build` has a different name or signature in this build, read `ledger/ledger.go` and match it. The demoted snippet declares only enough to make `convergesProof` a well-typed postulate of `Conv Reg merge`; `merge` here is a trivial well-typed stub since the postulate needs no proof.)

- [ ] **Step 2: Run the integration tests**

Run: `go test -run 'TestCatalogLedgerTiers|TestCatalogGatePassesAndFails' ./harness/`
Expected: PASS.

- [ ] **Step 3: Run the full suite**

Run: `go test ./...`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add harness/control_catalog_test.go
git commit -m "feat(control): catalog plugs into the assurance ledger + gate"
```

---

## Self-Review

**Spec coverage (against `2026-06-25-wavelet-beta-design.md` Section 1 + 3 and Plan 3 in `00-INDEX.md`):**
- Blessed set fully proven: in-region (Task 1), encrypted-in-transit (Task 2), least-privilege-IAM (Task 3), CRDT-convergence-under-all-schedules (Task 4). Covered.
- Each control a content-hashed proposition over the architecture model: every control is a rune `proposition : Type` whose identity is `core.HashTerm(Ty)` via the Plan 2 ledger; the model is the `Svc`/`Edge`/`CodeList`/`Reg` datatypes plus the demo instance. Covered.
- IAM control is a set-equality (or subset) proof over the access relation: Task 3 proves `eqCodes granted needed = true` (set-equality of granted and needed access sets). Covered.
- Convergence reuses the CvRDT corpus: Task 4 copies ch453's verified algebra verbatim and bundles its three laws into `Conv`. Covered.
- Tail controls guarded with explicit labels via Plan 2: Task 5 states a tail control as `postulate ... because "..." end`, labeled `postulate` by the ledger. Covered.
- Each flagship proven on the demo model: Tasks 1-4 discharge each on the demo instance; Task 8 asserts the ledger tiers are `proven`. Covered.
- A deliberately over-broad policy is rejected (proof does not go through): Task 6 (`TestOverBroadIAMRejected`). Covered.
- The catalog plugs into the Ledger and the CALM emit: Task 8 (ledger tiers + gate) and Task 7 (`control.Catalog()` element map for Plan 4's CALM emit). Covered.

**Placeholder scan:** The rune bodies in Tasks 1-5 are complete and self-contained; Task 4's CvRDT algebra is reproduced in full (copied from the proven `ch453_max_register_crdt.rune`, lines 17-146), not referenced. The only "match the existing symbol" instructions are the session loader name (`LoadSource` vs `LoadString`, Tasks 6/8) and the `ledger.Build` signature (Task 8), each with the exact file to read (`harness/listings_test.go`, `ledger/ledger.go`) - these cannot be guessed because they are private-to-this-build choices made in Plan 2 / the harness. Every Go step shows real, compilable code against the confirmed Plan 2 API.

**Type consistency:** The rune control def names (`inRegionProof`, `encryptedProof`, `leastPrivProof`, `convergesProof`, `liveInRegion`) are introduced in Tasks 1-5 and referenced identically by `control.Catalog()` (Task 7) and the ledger tests (Task 8). `control.Flagships()` returns exactly the four non-tail names; `control.AllowedPostulates()` returns `liveInRegion`; both are consumed by `ledger.GateConfig` in Task 8. The `Conv` datatype's constructor argument types in Task 4 match the `Conv Reg merge` proposition reused in Task 8's demoted snippet. `ledger.Proven`/`ledger.Postulate`/`ledger.Tier`/`ledger.Entry`/`ledger.Gate`/`ledger.GateConfig` are the Plan 2 names.

**One scope note:** The catalog attaches each control to a CALM element id (`control.Control.Element`) but does not itself emit CALM - that is Plan 4, which consumes `control.Catalog()`. The element ids here (`store`, `web->relay`, `relay`) are the demo model's nodes/relationships and must agree with the CALM node ids Plan 4 chooses; if Plan 4 renames a node, update `Catalog()` to match (a one-line change, caught by Plan 4's round-trip test).

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-06-26-control-catalog.md`. Two execution options:

1. **Subagent-Driven (recommended)** - I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** - Execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
