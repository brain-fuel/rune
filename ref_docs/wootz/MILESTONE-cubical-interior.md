# MILESTONE — cubical interior (M1+) BANKED

The milestone boundary, banked per explicit direction. The cubical inner stratum
now **computes end to end** for everything expressible without a face-restricted
reduction relation. This is a stable, green checkpoint; M2 (univalence *computes*,
retiring the `ua` postulate) is the next milestone and is gated on the one
remaining foundational node (see [FACE-RESTRICTED-EVAL-DESIGN.md](FACE-RESTRICTED-EVAL-DESIGN.md)).

## What is banked (all green, no hash-format bump, conversion soundness intact)

Twelve verified commits, `feat/cubical-stratum-phase3`:

| Node | What computes | Listing / test |
|---|---|---|
| **A1** | transp/comp over piF (incl. varying domain) | ch23 |
| **A2** | hcomp ⊥/⊤ (empty/total system) | — |
| **A3** | transp **and** comp over pathF lines (endpoints definitional, interior symbolic) | ch47 |
| **A4** | **path induction on general paths** (pathJ-via-comp) + path-η in conversion | ch49 |
| **A5** | inner Σ former + computing projections | ch24 |
| **A5b** | transp/hcomp/comp over **dependent** sigmaF (via hfill/compFill) | ch48 |
| **A6 (ready slice)** | Glue former + glue/unglue + 3 boundary ι-rules + ⊤-degeneracy | ch45 |
| **A6 (Kan)** | **hcomp/comp over Glue (re-glue via unglue/unglueT)**; **transp over Glue for a `glue` INTRO over a φ-constant line** (`transpGlueIntro`, CCHM compGlue specialized) | ch50 |
| **R-BOX** | pathF proper-face hcomp (any face, overlap-free) | ch46 |

**Additive primitives created** (each its own builtin group / sound conversion
edit, no kernel-format change, all property/refl-verified):
- `fsplit` — the `holds (for φ ψ)` eliminator (face dispatch); the unlock for A3.
- `hfill` / `compFill` — homogeneous/heterogeneous filler forms.
- `forallF` — the `∀i.φ` cofibration.
- the real `Equiv` contractible-fibre tower (`fiber`/`isContr`/`isEquiv`/`Equiv`/
  `equivFun`/`equivProof`, Σf.isEquiv with computing projections).
- **path-η** in conversion (`pabs A f ≡ q` iff `f@fresh ≡ q@fresh`).

## What is NOT banked (the M2 gate) — UPDATED

The substrate has since advanced past most of this section. The value
face-restriction that was "no analog for" is now **landed** as `RestrictIv`
(`core/quote.go:19`), the contractibility filler as `extend` (`core/eval.go:2717`),
and the type-level path abstraction as `pabsU` (`store/pathu.go`). The
hcomp/comp-over-Glue re-glue and the `glue`-intro transp-over-Glue
(`transpGlueIntro`) are banked (see the table above). The M2 residue is now exactly:

- **G1 — the general transp-over-Glue arm** for a **neutral input** or a **φ varying
  in i** (`core/eval.go:2913-2917` still returns `nil,false`). This is the genuine
  consumer of `RestrictIv` + `extend`: wire them through the full cubicaltt
  `compGlue`. The riskiest remaining step.
- **G2 / A7 — retire the postulated `ua` head**: re-type/re-body `FRoleUa`
  (`store/fib.go`) as the Glue line `pabsU (λi. Glue …)`, delete the baked-in
  `castU`-over-`ua` shortcut (`core/eval.go:2290-2291`) so it reduces *through* the
  `pabsU → transpG → transp-over-Glue` route. The one deliberate hash event (`fib`
  group digest + ch10), no `defFormatVersion` bump.
- **G3 — X2 canonicity + X1/R-FRAME exactness** on the new G1 arm.

`fsplit` (N-way nested dispatch, ch52) already discharges C-OVERLAP, so the
`elaborate/system.go` "sys smart-constructor" the companion docs proposed is
**superseded — do not build it**. The canonical M2 thesis already computes:
`castU (pabsU (λi. Glue…)) (glue… t a) ~> glue…`
(`internal/session/structural_test.go:TestUnivalenceComputesViaGlue`), additively,
with the postulate still standing — G2 makes it the *only* path.

## Discipline held throughout

No unsound/unverifiable code entered the proof kernel. Every shipped ι-rule and
the one conversion edit (path-η) is refl-pinned or property-tested; the full suite
is green at every commit; `defFormatVersion` is unchanged (no new core
constructor). The deep remaining node is deferred to a reviewed project rather
than committed under pressure.
