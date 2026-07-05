# GTM Script + Chapter (6g) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rewrite pitch/01-DEMO.md as the beta-complete recordable demo script and write pitch/11-DEMO-CHAPTER.md, the long-form funder chapter, both on one spine with real captured transcripts.

**Architecture:** Two writing tasks sharing a fixed 7-step spine (spec Decision 2), plus a consistency pass over the pitch docs that reference the demo. Every command transcript is captured from a real run at the implementation commit; no invented output.

**Tech Stack:** Markdown prose; `go run ./cmd/rune` for command capture; node+wabt for the twotab build; terraform for HCL checks (present locally).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-gtm-script-chapter-design.md`. The spine order in Decision 2 is FIXED: cold open two-tab, five outputs, IAM diff + sabotage, Ledger view, schedule luck, hash binding, close + what-NOT-to-claim.
- All verbs `rune ...`; NO `wvl`, no rename references. "Wavelet" as the infra-product name in prose is fine where existing pitch docs already use it.
- NO em-dashes or en-dashes anywhere in new/edited prose (ASCII hyphen-minus only). Check with `grep -P '\x{2013}|\x{2014}'` before every commit.
- Every command transcript pasted from a REAL run in this repo; long output excerpted honestly with an explicit "elided" note.
- No urgency/scarcity/FOMO language; argue with dignity. The Five Lightbulbs structure lives in 06-PITCH-SCRIPT and is not duplicated.
- The what-NOT-to-claim list appears in BOTH artifacts and includes at minimum: billed-account apply still open (what funding buys), stdlib deep-not-broad, single builder.
- No code changes anywhere in this plan. If a spine step cannot be reproduced with existing code, STOP and report (spec bug), do not build around it.
- Commit with explicit pathspecs; Conventional Commits.

## Reference facts (for all tasks)

- Run commands from the repo root. The CLI is `go run ./cmd/rune <verb> ...` (write transcripts as `rune <verb> ...` in the docs, the reader has it installed or aliased).
- The five-outputs command set and expected shapes are gated by `cmd/rune/five_outputs_test.go` (TestFiveOutputs*): ledger tier proven, sim "verdict: CONVERGED ... join laws hold", 3-cloud HCL with kv:Get/kv:Set, twotab build artifacts, CALM "validates against the source 1:1".
- IAM sabotage (spine step 3): in `listings/ch538_control_catalog.rune`, `needed : CodeList is ccons 10 (ccons 11 cnil) end` (~line 136) models kv:get/kv:set and `leastPrivProof : Eq Bool (eqCodes granted needed) true is refl end` (~line 143). Widening `granted` with an extra grant (e.g. `ccons 10 (ccons 11 (ccons 99 cnil))`) makes the file FAIL TO LOAD (refl rejected at elaboration). Capture the real error, then REVERT the edit (git checkout the file). CI twin: harness/control_catalog_test.go TestOverBroadIAMRejected.
- Hash binding (spine step 6): `rune hash examples/twotab/counter.rune | grep ' merge '` vs `rune hash listings/ch538_control_catalog.rune | grep ' merge '` show the identical hash (exact grep/format: check `runHash` output shape first; adjust the pipe so the two merge lines are shown side by side honestly). CI twin: TestFiveOutputsHashBinding.
- Two-tab cold open (spine step 1): build with `(cd examples/twotab && node build.mjs)`, serve with `python3 -m http.server` per examples/twotab/RUN.md; on-screen behavior description comes from RUN.md manual acceptance + the TestTwoTabDemo assertions (two tabs pair, bumps converge both sides). The honest-claims footer text is verbatim in examples/twotab/index.html.
- Schedule luck (spine step 5): `rune simulate examples/gcounter.rune 2` (CONVERGED) vs `rune simulate examples/lww.rune 2` (linter: not commutative, "schedule luck, not a property").
- Ledger view (spine step 4): `rune ledger listings/ch538_control_catalog.rune` text table; the postulate row is `liveInRegion` with reason "live cloud region read not yet modeled (provider API); attested out of band".

---

### Task 1: Rewrite pitch/01-DEMO.md (the recordable script)

**Files:**
- Modify: `pitch/01-DEMO.md` (full rewrite, keep the filename)

**Interfaces:**
- Produces: the 7-step spine with captured transcripts that Task 2 quotes; section anchors `## The 3-minute version` and `## The 10-minute version` and `## What NOT to claim in the demo` that Task 3's references point at.

- [ ] **Step 1: Capture the transcripts**

From the repo root, run and save output for each (scratch file, not committed):

```bash
go run ./cmd/rune ledger listings/ch538_control_catalog.rune
go run ./cmd/rune simulate examples/twotab/counter.rune 2
go run ./cmd/rune simulate examples/gcounter.rune 2
go run ./cmd/rune simulate examples/lww.rune 2
go run ./cmd/rune deploy --manifest examples/wavelet_demo.rune --backend aws | head -60
go run ./cmd/rune deploy --manifest examples/wavelet_demo.rune --backend gcp | head -60
(cd examples/twotab && node build.mjs)
go run ./cmd/rune calm emit --manifest examples/wavelet_demo.rune --listing listings/ch538_control_catalog.rune > /tmp/demo.calm.json
go run ./cmd/rune calm validate /tmp/demo.calm.json --manifest examples/wavelet_demo.rune --listing listings/ch538_control_catalog.rune
go run ./cmd/rune hash examples/twotab/counter.rune | grep -i merge
go run ./cmd/rune hash listings/ch538_control_catalog.rune | grep -i merge
```

Then the sabotage capture: edit `listings/ch538_control_catalog.rune` line ~137, widen `granted` to `ccons 10 (ccons 11 (ccons 99 cnil))`, run `go run ./cmd/rune ledger listings/ch538_control_catalog.rune` to capture the load FAILURE error text, then `git checkout listings/ch538_control_catalog.rune` and re-run to confirm clean again.

- [ ] **Step 2: Rewrite the file on the spine**

Structure (replace the whole file):

1. Header: "The demo is the whole pitch" framing (keep the current opening sentiment: a technical funder believes what runs; every arrow is a test in `go test ./...`; source of truth examples/WALKTHROUGH.md).
2. `## The 3-minute version (record this as a video)` - the 7 spine steps as numbered shots. Each shot: the on-screen action (exact command or browser action), the expected output (from Step 1 captures, excerpted with "(elided)" notes where long), and a SAY line. Draft say lines to refine, not replace in spirit:
   - Cold open: "Two browser tabs, one counter, no server in the data path. The merge that just reconciled them is a machine-checked theorem. The footer tells you exactly which parts are proven and which parts are trusted glue - honesty is part of the product."
   - Five outputs: "One source. Five outputs: the assurance ledger, a fault simulation, Terraform for three clouds, the app you just watched, and a CALM architecture doc that re-validates against the source one to one."
   - IAM diff: "The role ships with exactly the two capabilities the code performs. Watch what happens when I widen it - the file no longer compiles. You physically cannot ship the over-broad role."
   - Ledger: "Four controls proven. One postulate - and it says so, with the reason and the upgrade path. The tier is on the page, not in a footnote."
   - Schedule luck: "These two files look almost identical. One is proven to converge under any schedule. The other passed this run by luck, and the linter says so."
   - Hash binding: "Same hash. The control in the compliance doc and the merge running in your browser are one value. That is what content addressing buys."
   - Close: "Every arrow you just saw is a passing test. Clone it and run go test."
3. `## The 10-minute version (the in-room demo)` - same spine, expanded: open counter.rune and lww.rune side by side (keep this beat from the current doc), the sabotage beat with the real error on screen and the revert, the aws-vs-gcp HCL diff beat ("SQS vs Pub/Sub, same logical set" - adapt to what the kv/compute/iam rows actually emit, from the Step 1 captures), the ledger walk with the liveInRegion postulate read aloud.
4. `## Why this demo is unfalsifiable in the way that matters` - keep, updated: name TestFiveOutputs* and TestTwoTabDemo as the gates behind the new moments.
5. `## What NOT to claim in the demo` - keep the three honesty items, updated wording: HCL emitted + fmt/schema-checked, billed apply is open and is what funding buys; stdlib deep-not-broad; single builder.

- [ ] **Step 3: Verify prose rules**

```bash
grep -P '\x{2013}|\x{2014}' pitch/01-DEMO.md; echo "dash-exit:$?"   # expect 1 (no hits)
grep -n "wvl" pitch/01-DEMO.md; echo "wvl-exit:$?"                  # expect 1
grep -c "NOT to claim" pitch/01-DEMO.md                              # expect >= 1
```

- [ ] **Step 4: Commit**

```bash
git add pitch/01-DEMO.md
git commit -m "docs(pitch): rewrite demo script on the beta-complete spine (6g)" -- pitch/01-DEMO.md
```

---

### Task 2: Write pitch/11-DEMO-CHAPTER.md (the long-form chapter)

**Files:**
- Create: `pitch/11-DEMO-CHAPTER.md`
- Read (source material, do not modify): `pitch/01-DEMO.md` (Task 1's transcripts), `examples/WALKTHROUGH.md`, `examples/twotab/RUN.md`, `docs/superpowers/specs/2026-06-25-wavelet-beta-design.md` (the Assurance Ledger section for the tier vocabulary)

**Interfaces:**
- Consumes: Task 1's captured transcripts (quote them; re-run any command where more of the output is needed than 01-DEMO excerpts).
- Produces: `pitch/11-DEMO-CHAPTER.md` with a title line Task 3 links from 00-README.

- [ ] **Step 1: Write the chapter**

~3000 words, funder technical-diligence reader, prose (not a shot list). Same 7-step spine, one section per step. EVERY section has three parts:
- a run-it-yourself block (exact commands, real output excerpts),
- "What this means" (the argument: why this step is impossible for config-shaped tools; tie to the mechanism - proofs as compile errors, content hashes as identity, the simulator's law linter),
- "What this does not claim" (the honest edge: e.g. the two-tab signaling is a same-browser shortcut a production app replaces; the Bin edge of the codec is runtime-gated, not kernel-proven; the postulate tier exists and is labeled).

Opening: the one-paragraph thesis - infrastructure tools check the shape of config, this checks the behavior of the system, and as AI writes more infrastructure the difference compounds (align with 06-PITCH-SCRIPT slide 1's argument, do not duplicate its deck structure).

Closing: the reproduce-it-all paragraph (clone, `go test ./...`, every claim is a gate) + the same what-NOT-to-claim list as 01-DEMO (same three items, prose form).

- [ ] **Step 2: Verify prose rules**

```bash
grep -P '\x{2013}|\x{2014}' pitch/11-DEMO-CHAPTER.md; echo "dash-exit:$?"  # expect 1
grep -n "wvl" pitch/11-DEMO-CHAPTER.md; echo "wvl-exit:$?"                 # expect 1
wc -w pitch/11-DEMO-CHAPTER.md                                              # expect ~2500-4000
```

- [ ] **Step 3: Commit**

```bash
git add pitch/11-DEMO-CHAPTER.md
git commit -m "docs(pitch): long-form demo chapter (6g)" -- pitch/11-DEMO-CHAPTER.md
```

---

### Task 3: Consistency pass + checklist close

**Files:**
- Modify: `pitch/00-README.md` (add 11-DEMO-CHAPTER.md to the doc list; update any demo-sequence summary), `pitch/02-COMPARISON.md` and `pitch/06-PITCH-SCRIPT.md` (ONLY where they describe the old demo sequence: slide 0's described sequence should mention the two-tab cold open and the ledger/IAM moments; minimal edits), `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (strike item 7, 6g, DONE, matching the existing strike style)

**Interfaces:**
- Consumes: Task 1's section anchors and Task 2's filename.

- [ ] **Step 1: Survey the references**

```bash
grep -n "90-second\|two-tab\|demo" pitch/00-README.md pitch/02-COMPARISON.md pitch/06-PITCH-SCRIPT.md | head -30
```

Update ONLY sentences that describe the demo's contents/sequence so they match the new spine (cold open two-tab, IAM diff, ledger view; "3-minute" not "90-second" where the length is named). Do not restructure either doc.

- [ ] **Step 2: Strike beta item 7**

In `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md`, mark item 7 (6g) DONE in the file's existing strike style, noting: script pitch/01-DEMO.md rewritten + chapter pitch/11-DEMO-CHAPTER.md, rune verbs per the 2026-07-04 sequencing decision.

- [ ] **Step 3: Verify and commit**

```bash
grep -P '\x{2013}|\x{2014}' pitch/00-README.md pitch/02-COMPARISON.md pitch/06-PITCH-SCRIPT.md | grep -v "^\S*:[0-9]*:.*[a-z]" | head   # inspect: no NEW dashes introduced by your edits (pre-existing ones elsewhere in untouched lines are fine)
git add pitch/00-README.md pitch/02-COMPARISON.md pitch/06-PITCH-SCRIPT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
git commit -m "docs(pitch): demo-sequence references updated to the 6g spine; beta item 7 done" -- pitch/00-README.md pitch/02-COMPARISON.md pitch/06-PITCH-SCRIPT.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md
```
