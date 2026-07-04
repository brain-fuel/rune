# Five-outputs integration (Wavelet beta success criterion 2): Design

Date: 2026-07-04
Status: Approved by the author in-session (design presented and approved
2026-07-04; the one amendment: manifest files carry the `.rune` extension until
the Plan 7 rename, at which point they become `.wvl` - no `.wav` files remain).
Seed: main @ v3.365.0 (6f two-tab demo shipped v3.363.0; script ergonomics
v3.364.0; explain verb + scribe corpus on main).

## Premise

Beta success criterion 2: "One source produces all five outputs, and the
emitted CALM doc re-validates 1:1." All five outputs run today, but from four
loose sources with no binding gate:

1. Ledger: `rune ledger listings/ch538_control_catalog.rune`
2. Fault sim: `rune simulate FILE N` (convention: `init`/`op0`/`op1`/`merge`/
   `value` - the two-tab `counter.rune` has `initGC`/`bump`, wrong names)
3. 3-cloud Terraform: `rune deploy --manifest examples/wavelet_deploy.wav
   --backend aws|azure|gcp`
4. Running app: `examples/twotab/` (built from `counter.rune`)
5. CALM: `rune calm emit|validate --manifest examples/wavelet_demo.wav
   --listing listings/ch538_control_catalog.rune`

The load-bearing gap: ch538's `convergesProof` is over `Reg` (the ch453
max-register), NOT the GC the two-tab app runs. The criterion's "same content
hashes" binding does not hold today. Also two overlapping manifests exist
(demo = web/relay/store; deploy = relay/store/iam).

## Decision 1: one manifest, `.rune` extension

Unify the two manifests into ONE file, `examples/wavelet_demo.rune`, carrying
all four rows:

    paas    web
    compute relay
    kv      store
    iam     relay_role grants=kv:Get,kv:Set

Both `rune deploy` and `rune calm` consume it (calm.go's default manifest path
changes accordingly). `examples/wavelet_deploy.wav` is DELETED (Rule 5 -
superseded by the unified manifest); its Go-test consumers migrate. The CALM
doc gains a `relay_role` node - honest (it IS part of the architecture) and
still validates 1:1 since emit and validate read the same source.

Extension policy (the author's amendment): every manifest file in the repo
uses the `.rune` extension from this work onward - `examples/app.wav` ->
`examples/app.rune`, `examples/serving.wav` -> `examples/serving.rune`,
`examples/wavelet_demo.wav` -> `examples/wavelet_demo.rune`. Go code and
LIVING docs (README.md, examples/README.md, examples/WALKTHROUGH.md,
PARKING-LOT.md, ref_docs/wootz/R-INFRA.md, pitch/) migrate their references.
Historical records (docs/superpowers/plans/*, specs) are not rewritten. At the
Plan 7 rename these become `.wvl`.

Rejected: keeping two manifests plus a doc note ("the same architecture,
described twice") - it concedes exactly the "one source" claim the criterion
makes.

## Decision 2: the hash binding (the criterion's core)

ch538's convergence control is re-proven over the app's actual protocol:

- Copy the GC shapes from the runnable demo source - `data GC`, `initGC`,
  `bump`, `value`, `merge` (pointwise max) - into ch538 verbatim-in-structure.
  Content addressing does the binding: structurally identical definitions hash
  EQUAL, so the ledger/CALM proof hashes and the deployed app's hashes are the
  same values, literally.
- `convergesProof : Conv GC merge` discharged by `mergeComm`/`mergeIdem`/
  `mergeAssoc` over GC (the ch72/ch565 proof shapes).
- The `Reg` max-register copy in ch538 is DELETED (Rule 5): ch453 still
  carries the max-register CvRDT as a listing; ch538 only ever needed A
  convergence control, and the demo's is the right one. `control.Catalog()`
  is unchanged (same `convergesProof` name, same `store` element).
- No committed ledger baseline exists, so the hash change has no baseline
  churn.

The in-region/encrypted/least-priv/tail controls are untouched.

## Decision 3: simulate the app source directly

`examples/twotab/counter.rune` gains three thin aliases at the bottom:

    init : GC is initGC end
    op0 : GC -> GC is bump end
    op1 : GC -> GC is bump end

Then `rune simulate examples/twotab/counter.rune 2` folds the EXACT source the
app deploys under the partition-and-heal schedule and the CvRDT law linter.
ch565 (the proven twin) is untouched - the aliases are runnable-demo surface,
not proof material. The twotab build path is unaffected (extra exported defs
are inert; the library exports are explicit `--export` flags).

## Decision 4: the gate - `TestFiveOutputs`

One test in `cmd/rune` (it drives CLI verbs; the package already tests deploy/
calm/simulate) that produces all five outputs from the one source set
{`examples/twotab/counter.rune`, `examples/wavelet_demo.rune`,
`listings/ch538_control_catalog.rune`} and asserts:

1. **Ledger**: `runLedger --json` on ch538 renders; the `convergesProof` entry
   is tier `proven`.
2. **Sim**: `runSimulate` on counter.rune (2 replicas) reports a converging
   verdict (the CvRDT laws pass and the healed round agrees).
3. **Terraform**: `runDeploy --manifest examples/wavelet_demo.rune --backend
   X` for X in aws/azure/gcp emits fmt-clean HCL (the existing fmt-check
   helper), each naming the relay_role IAM grants.
4. **App**: the twotab build artifacts exist or build (node+wabt-gated
   sub-step, skips cleanly like the existing TestTwoTabBuild; the other four
   sub-steps are always-on).
5. **CALM**: `runCalm emit` then `runCalm validate` on the emitted doc
   round-trips ("validates against the source 1:1").

Plus the criterion-2 pin, the reason this gate exists: load counter.rune and
ch538 in two sessions and assert **hash(merge in counter.rune) == hash(merge
in ch538)** (and the same for the GC group). Any future drift between the
proven control and the deployed merge is a CI failure, not a footnote.

## Docs

`examples/WALKTHROUGH.md` gains a five-outputs section: the five commands, the
one source set, and the binding sentence ("the convergence control in the CALM
doc and the merge the browser tabs run are the same content hash - shown by
`rune hash`").

## Non-goals

- 6g (the GTM script) and the rename (Plan 7).
- Billed-account apply (cloud-gated, separate item).
- CALM authoring-ingest (post-beta), note-CRDT variant, any new infra kinds.
- No changes to codegen/, the kernel, ch565's proofs, or the twotab app/glue.

## Consumers

- 6g scripts directly off the gate's five commands.
- The funding pitch's "prove it, run it" demo: criterion 2 becomes checkable.
- The Plan 7 rename inherits one manifest and one extension rule.

## Testing summary

New: `TestFiveOutputs` (five sub-steps + the hash-binding pin). Regression:
existing deploy/calm/ledger/simulate/twotab gates stay green under the
manifest rename + ch538 convergence swap; listings gate re-checks ch538;
full `go test -timeout 30m ./...` before merge.
