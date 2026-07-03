# scribe Rune port L2: exact-Q geometry (ch562)

> Executor plan (structural; body written during execution, harness green per
> increment). Spec: scribe repo docs/superpowers/specs/2026-07-02-rune-port-spec.md.
> ch561 lessons apply: Bool-collision dodge, no neutral-vs-big-literal
> arithmetic, exactly ONE syntactic ih per fuel level.

**Goal:** `listings/ch562_scribe_geom.rune`: signed exact rationals, corner-grid
Point/Affine, the kappa and PaintCode corner tables as EXACT rationals, the path
builders (rect / circle / roundrect both styles), and adaptive cubic flattening,
with refl pins for the laws the Go geometry only asserts within epsilon.

## Design pinned up front

- **Qn**: sign-magnitude record `qn : Bool -> Nat -> Nat -> Qn` (neg, num, den;
  den >= 1 by smart constructors; zero normalized to non-neg). No quotient, no
  field laws: cross-multiplication equality `qeq` and order `qle` are the
  observation. Ops: qadd/qsub/qmul/qdiv/qneg/qhalf; no reduction (bignums carry;
  optional gcd-reduce only if pin runtimes demand it).
- **Exact table constants**: kappa = 5522847498307936 / 10^16; contExtent =
  152866483 / 10^8; the full PaintCode table with denominator 10^8. The
  continuous-corner extent pin becomes an EXACT qeq, not a 1e-9 epsilon.
- **Pt / Aff**: records over Qn; apply/compose mirroring Go geom.Affine
  (m.Mul(n) applies n first), pinned on concrete points.
- **Path**: `PVerb = pMoveTo Pt | pLineTo Pt | pCubicTo Pt Pt Pt | pClose`,
  Path = List PVerb. Builders mirror Go path/shapes.go incl radius clamp
  (needs qdiv + qmin).
- **Flatten**: WORKLIST, not tree recursion (one ih): a stack of pending cubic
  segments; each fuel step pops one, either emits its endpoint (flat: both
  control points within tol of the chord, the exact Go cubicFlat test in Q:
  cross^2 <= tol^2 * len2, squared distances only, no sqrt) or pushes its two
  de Casteljau halves (halving is exact: qhalf). Fuel 4096. Closed/open
  polyline split mirrors Go Flatten.
- **Pins**: Q arithmetic identities; affine compose semantics; roundrect
  bounds EXACT at both styles incl clamp; continuous extent exact; circular
  corner flatten points within the kappa radial bound (exact squared-form
  comparison); a flattened-point-count `answer` for the emit gate (kernel refl
  pins the same count, so kernel and compiled agree on the geometry).
- **Gates**: elaborate/check (pins), emit row `answer`, REPL test evaluating a
  flatten count in-session.

## Tasks

1. Preamble + Qn ops + pins (green per chunk).
2. Pt/Aff + pins.
3. Corner tables + Path builders + exact bounds/extent pins.
4. Worklist flatten + accuracy pins + answer.
5. Harness row + REPL test + full sweep (-timeout 30m) + finish
   (rebase if main moved, next free tag, push).
