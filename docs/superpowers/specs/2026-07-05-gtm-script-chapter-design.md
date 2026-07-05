# GTM script + chapter (Wavelet beta item 6g): Design

Date: 2026-07-05
Status: Approved by the author in-session (format: both script and chapter,
one spine; audience: funder-facing, both in pitch/).
Seed: main @ v3.368.0 (five-outputs integration shipped; criterion 2 closed).
Naming: everything says `rune` - per the 2026-07-04 sequencing decision the
project stays Rune through three major version bumps before any Wavelet beta
cut; the rename is NOT part of 6g.

## Premise

Beta item 6g: the go-to-market artifact, scripted. The beta design names two
moments that must land - the proven-minimal-IAM diff and the Assurance Ledger
view - and success criterion 1 names the effect: a skeptical technical reader
changes posture. The existing pitch/01-DEMO.md and pitch/06-PITCH-SCRIPT.md
predate 6f and the five-outputs integration, so the two named moments, the
two-tab browser demo, and the hash-binding story appear nowhere in the GTM
material.

## Decision 1: two artifacts, one spine, both in pitch/

- `pitch/01-DEMO.md` is REWRITTEN IN PLACE to the beta-complete demo spine
  (below): the recordable script - a ~3-minute video shot list plus the
  10-minute in-room walk, exact commands, expected output, say-over lines.
- `pitch/11-DEMO-CHAPTER.md` is NEW: the long-form chapter (~3000 words),
  the same spine in prose for the funder's technical-diligence read. Each
  section: a run-it-yourself block, what it means, what it does NOT claim.

Rejected: a second standalone script beside 01-DEMO (two demo scripts drift);
one combined doc (loses the recordable shot-list separation).

## Decision 2: the spine (both artifacts, this order)

1. **Cold open - the artifact.** Two browser tabs (examples/twotab), bump in
   each, the counter converges live over a real RTCDataChannel. The page's
   honest-claims footer read aloud: which layers are machine-checked (state,
   merge convergence, wire codec) and which are trusted glue (WebRTC, DOM,
   signaling).
2. **One source, five outputs.** The criterion-2 command set, with output
   captured from real runs: `rune ledger listings/ch538_control_catalog.rune`,
   `rune simulate examples/twotab/counter.rune 2`, `rune deploy --manifest
   examples/wavelet_demo.rune --backend aws|azure|gcp`, `(cd examples/twotab
   && node build.mjs)`, `rune calm emit | rune calm validate`.
3. **Moment 1 - the proven-minimal-IAM diff.** The manifest carries
   `grants=kv:Get,kv:Set`; show the scoped policy in the aws vs gcp HCL
   (concrete resources differ, logical set identical). Then the sabotage:
   widen ch538's modeled policy to over-broad and the LISTING FAILS TO LOAD -
   the `refl` no longer elaborates, a compile error (the same sabotage
   harness/control_catalog_test.go runs in CI). Say-over: you physically
   cannot ship the over-broad role.
4. **Moment 2 - the Assurance Ledger view.** `rune ledger` on ch538: four
   flagship controls `proven`, and `liveInRegion: POSTULATE` with its stated
   reason, git provenance, and upgrade path. The honesty IS the pitch: the
   tier is on the page, not in a footnote.
5. **Schedule luck (kept).** gcounter vs lww under `rune simulate`: the
   proven join converges under any schedule; the LWW counter passes one
   schedule and the law linter calls it "schedule luck, not a property."
6. **The hash binding.** `rune hash` shows the `merge` in the control
   catalog and the `merge` the browser tabs run are the SAME content hash;
   the emitted CALM doc re-validates 1:1. Say-over: the control in the
   compliance doc and the code in the browser are one value.
7. **Close.** `go test ./...` - every arrow above is a gate; a reader can
   clone and reproduce. Then the what-NOT-to-claim list, updated: HCL is
   emitted and format/schema-checked but billed-account apply is still open
   (exactly what funding buys), stdlib depth vs breadth, single builder.

## Decision 3: writing rules

- Every command transcript is pasted from a REAL run in the repo at the
  implementation commit; no invented output. Where output is long (HCL,
  ledger table), excerpt honestly and say what was elided.
- Marketing discipline: Marketing-Is-An-Argument / Five Lightbulbs ordering
  stays in 06-PITCH-SCRIPT (untouched except demo-sequence references);
  argue with dignity, no urgency/scarcity/FOMO manufactured anywhere.
- NO em-dashes or en-dashes in new prose (ASCII hyphen-minus only).
- All verbs `rune ...`; no `wvl`, no "Wavelet CLI" phrasing. Product naming
  in prose may still say Wavelet for the infra track where the existing
  pitch docs already do.
- Consistency pass: other pitch/ docs that reference the old demo sequence
  (00-README.md, 02-COMPARISON.md, 06-PITCH-SCRIPT.md slide 0) get their
  references updated to the new spine, minimal edits only.

## Decision 4: verification

No new test harness (YAGNI - the five-outputs gate already pins every
command's behavior in CI). The implementation plan requires the implementer
to run each command and capture output; the reviewer re-runs a sample. The
two-tab cold open's "expected on screen" is described from RUN.md manual
acceptance plus the existing puppeteer gate's assertions.

## Non-goals

- No video recording, no slides, no visual assets (the script IS the
  deliverable; recording is the author's act).
- No rename, no `wvl` (three major bumps precede it).
- No new demo functionality; if a spine step needs code that does not exist,
  that is a spec bug to surface, not to build silently.
- No rewrite of 02-COMPARISON/03-AI-WORKFLOW/04..10 beyond demo-sequence
  reference touch-ups.

## Consumers

- The author records the video and runs the in-room demo from 01-DEMO.md.
- The funder's technical read is 11-DEMO-CHAPTER.md.
- The beta checklist item 7 (6g) closes on merge.

## Testing summary

Manual: every transcript reproduced at implementation time; reviewer
re-runs a sample of the commands. CI: unchanged (five-outputs gate already
covers the commands). Prose: no-dash rule, honesty list present in both
artifacts.
