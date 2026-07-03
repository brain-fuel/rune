# Two-tab CRDT browser demo (Wavelet beta item 6f): Design

Date: 2026-07-03
Status: Draft, pending author review. Two AUTHOR-CONFIRM decisions taken on
recommendation while the author was AFK, flagged inline.
Seed: main @ v3.362.0 (6d browser-library shim shipped: LibraryBackend +
glue.js ABI + ch565 proven codec + two-instance node convergence gate)

## Premise

The beta's headline artifact: a shared counter CRDT in two browser tabs
converging live over WebRTC, where the merge logic, the state, and the wire
bytes are all machine-checked rune running as WASM. Everything below the
browser is done (6d): the library module, the glue, the proven codec, and a
node gate that already drives the exact flow two instances will perform. 6f
adds only the browser skin: WebRTC signaling, the DOM, and the page that
wires them to glue.js verbatim.

## Decision 1: static demo, no tooling

`examples/twotab/` holds the whole demo as static files:

- `index.html`: the page. Two elements per the beta spec's "two divs"
  framing: the counter value and a peer-status line, plus one Bump button.
  Inline minimal CSS; no framework, no bundler.
- `app.js`: an ES module consuming `glue.js` verbatim (import, load, call,
  mkBin/readBin/readNat per the 6d ABI + ownership contract). Owns the
  event loop: button click = `bump` + broadcast; incoming bytes = `mkBin` +
  `gcFromBin` + `merge`; render = `value` + `readNat` into the DOM.
- `counter.rune`: the demo program (re-exports ch565's shapes: initGC, bump,
  value, merge, gcToBin, gcFromBin; a copy of the ch565 definitions or a
  thin listing-style file, whichever the build finds simpler; the PROOFS
  stay in ch565, the demo program only needs the runnable defs).
- `build.mjs`: one node script: runs `rune build counter.rune --kind library
  --target wasm --export ...` then assembles the emitted `.wat` to
  `counter.wasm` via the harness wabt (browsers cannot parse WAT; the
  assembly happens once at build time, nothing ships a compiler). Outputs
  land beside the page: `counter.wasm`, `counter.glue.js`.
- `RUN.md`: two commands (build; `python3 -m http.server` or any static
  server) + what to observe. file:// does not serve ES modules/wasm
  reliably; a static server is the documented path.

Rejected: any bundler/framework (two divs); assembling WAT in-browser
(ships a compiler to run a counter, slow first paint).

## Decision 2: signaling via BroadcastChannel, data via real WebRTC
(AUTHOR-CONFIRM: recommended option taken while AFK; alternatives were
manual SDP copy-paste, or a websocket relay server)

Two tabs of the same origin discover each other and exchange SDP
offer/answer/ICE through the browser's built-in `BroadcastChannel` (a
same-browser bus; ~30 lines). The CRDT bytes then flow over a genuine
`RTCDataChannel`. Honest framing, stated in RUN.md and the page footer: the
DATA PLANE is real WebRTC; only peer DISCOVERY uses the same-browser
shortcut, which is exactly the part a production app replaces with its own
signaling. Roles: first tab to broadcast a hello becomes the offerer;
simple collision rule (lower random session id offers) handles the race.

On channel open, each side sends its current encoded state once (initial
sync), then sends state after every local bump. Receiving state always
merges (idempotent, commutative, proven: duplicates and reordering are
harmless BY THEOREM, which the page footer says).

## Decision 3: the demo's honest-claims footer

The page displays, under the counter: which layers are machine-checked
(state, merge convergence via ch72's mergeComm/Idem/Assoc, wire codec via
ch565's round-trip theorem) and which are trusted glue (WebRTC transport,
DOM, signaling). This is the assurance-ledger spirit at demo scale and
feeds 6g's script directly. Static text, two sentences, no ledger tooling
in scope.

## Decision 4: verification (AUTHOR-CONFIRM: recommended option taken while
AFK; alternatives were manual-only, or playwright)

Two layers:

1. Manual: RUN.md's steps are the demo acceptance (open two tabs, bump in
   each, watch both converge; kill a tab, reopen, re-sync works because
   state re-sends on channel open).
2. Automated, optional: `TestTwoTabDemo` in the harness behind a puppeteer
   dependency (npm, harness-local beside wabt, a setup.sh section; the gate
   skips when node/puppeteer/the built artifacts are absent). Headless
   chrome, two pages against a stdlib static server started by the test,
   real BroadcastChannel + real RTCDataChannel inside one browser process:
   click bump twice in tab A, once in tab B, poll both DOMs until both show
   3 (bounded wait), assert equality and the expected total. This pins the
   ACTUAL browser claim, not just the node simulation.

The existing 6d node gate remains the fast always-on check of the same
logic; the puppeteer gate is the slow optional end-to-end pin.

## Non-goals

- No cross-machine demo (BroadcastChannel is same-browser; the RUN.md notes
  what a production signaling swap looks like, one sentence).
- No note/MV-Register variant, no text input (counter only; the note CRDT
  remains the documented stretch with its own codec + proofs when chosen).
- No five-outputs integration and no GTM script (the next beta items; this
  demo is output 4 of 5).
- No styling beyond legibility (the demo's beauty is the theorem, not CSS).
- No changes to codegen/, the glue, or the ABI (6f consumes 6d as shipped;
  any gap it finds in the ABI is a 6d bug to fix there, not to patch around
  in app.js).

## Consumers

- 6g: scripts this artifact (the bump-converge moment, the honest-claims
  footer, the proven-minimal-IAM diff beside it).
- The five-outputs integration: this page is the "running app" output.
- The funding pitch demo: the live half of "prove it, run it".

## Testing summary

Always-on: the existing 6d node convergence gate (unchanged). Optional:
TestTwoTabDemo puppeteer two-tab convergence. Manual: RUN.md acceptance.
Regression: listings + WASM suites untouched (no codegen change).
