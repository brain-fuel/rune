# R-FILL — transporting HIT path constructors (the meridian filler)

*Design for the genuine-frontier piece: varying-parameter transport of a HIT's
PATH constructor, the keystone the rest of §F's HIT transport is gated on.*

## Problem

Point-constructor varying-parameter transport landed (commit 7732c59):
`transp (λi. Susp (A i)) (north (A i0)) ~> north (A i1)`, and the quotient's
`qinc` transports its carried element. The PATH constructors stayed stuck:

```
transp (λi. Susp (A i)) ⊥ (merid (A i0) a @ k)   -- meridian point, stuck
transp (λi. Quotient (A i) (R i)) ⊥ (qrel … r @ k) -- relation point, stuck
```

A path-constructor APPLICATION `merid a @ k` is a genuine point of the HIT for
interior `k`, so transport must "move the path" with the parameter.

## The two cases differ in difficulty

CHM's HIT transport schema transports a path-constructor application by applying
the constructor to the transported arguments, THEN reconciling the boundary
against the transported endpoints with a `comp`. The reconciliation `comp`
**vanishes exactly when the path constructor's endpoints do not depend on its
arguments** — because then the transported endpoints already equal the new
constructor's endpoints on the nose.

- **Suspension `merid a : north = south`.** Endpoints `north`/`south` are nullary
  point constructors — they carry NO element of the meridian argument `a`. So the
  reconciliation is definitional and the transport is just the meridian of the
  transported argument:
  ```
  transp (λi. Susp (A i)) φ (merid (A i0) a @ k)
    ~> merid (A i1) (transp (λi. A i) φ a) @ k
  ```
  Boundary-coherent on the nose: at `k=i0` both sides are `north (A i1)`
  (the point-ctor rule), at `k=i1` both are `south (A i1)`. The interior is the
  meridian of `transp a`. This is the clean case — no comp needed.

- **Quotient `qrel a b r : qinc a = qinc b`.** Endpoints `qinc a`/`qinc b` DEPEND
  on the transported arguments `a`,`b`. After transporting `a`,`b` the new
  relation path's endpoints are `qinc (a')`/`qinc (b')`, but the *expected*
  boundary is `transp (qinc a)`/`transp (qinc b)` — which equal `qinc (a')` /
  `qinc (b')` by the qinc point rule, so for the fibrant quotient the endpoints
  ALSO reconcile definitionally:
  ```
  transp (λi. Quotient (A i) (R i)) φ (qrel … a b r @ k)
    ~> qrel (A i1) (R i1) (transp a) (transp b) (transp_R r) @ k
  ```
  where `transp a = transp (λi. A i) φ a`, and `transp_R r` transports the witness
  along the relation line `λi. R i (ã i) (b̃ i)` (ã, b̃ the fills of a, b). The
  endpoint coherence holds because `transp (qinc A R a) = qinc (A i1)(R i1)(transp a)`
  is exactly the point rule — so the naive constructor-at-transported-args is
  boundary-correct here too. The only subtlety is the witness line for `r`.

So both reduce to "apply the path constructor to the transported arguments," with
the endpoint reconciliation discharged by the already-landed point-ctor rule.
This is the suspension/quotient-specific simplification of the general CHM comp
formula (which is needed only when endpoints mix argument data with recursion).

## Implementation (additive, in transpG)

Extend the two HIT arms already in `transpG` (core/eval.go): when the input `a0`
is a `papp … (merid …) k` (resp. `papp … (qrel …) k`), rebuild the path
constructor at the transported arguments and re-apply `papp` at the same `k`.

- **Suspension** (suspFormer arm): detect `a0 = papp (Susp (A i0)) (north (A i0))
  (south (A i0)) (merid (A i0) a) k`. Build
  `aT = vTranspG(λi. A i, φ, a)`, `merid' = merid (A i1) aT`, and return
  `papp (Susp (A i1)) (north (A i1)) (south (A i1)) merid' k`.

- **Quotient** (quotFormer arm): detect `a0 = papp (Quotient (A i0)(R i0))
  (qinc … a)(qinc … b) (qrel … a b r) k`. Build `aT`, `bT` along `λi. A i`, and
  `rT` along `λi. R i (transpFill a i)(transpFill b i)` (the relation line over the
  fills); return `papp (Quotient (A i1)(R i1)) (qinc (A i1)(R i1) aT)(qinc … bT)
  (qrel (A i1)(R i1) aT bT rT) k`.

## Verification

- **Boundary pins (refl):** `papp … (transported merid) i0 ≡ north (A i1)` and
  `… i1 ≡ south (A i1)`; quotient analogues. These reuse the path boundary rule and
  the landed point-ctor transport, so they hold definitionally.
- **Canonicity:** a closed varying-parameter transport of a meridian/relation point
  reduces to a `papp (…) (path-ctor …) k` form (a canonical path-constructor
  application), never a stuck transp.
- **Regression:** the meridian-stuck test flips to a computing test; constant-line
  transport unchanged (regularity).

## Honest residual

The general CHM comp-correction (path constructors whose endpoints mix argument
data with RECURSIVE occurrences, and the formal hcomp-cell transport
`transp-of-hcomp = comp`) stays labelled. Suspension and the fibrant quotient are
the cases where the correction is definitional; a HIT with recursive path-ctor
endpoints would need the full comp. The hcomp-cell varying transport (comp over
the HIT line) is the remaining filler, tracked separately.
