# Two-Tab CRDT Browser Demo (6f) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The beta's headline artifact: two browser tabs converging a proven CRDT counter live over a real WebRTC data channel, consuming 6d's glue.js verbatim.

**Architecture:** Four tasks. (1) The demo program + build script: `examples/twotab/counter.rune` built to `counter.wasm` + `counter.glue.js` via LibraryBackend + one-time wabt assembly, with a node load-smoke. (2) The sync protocol module (`sync.js`, channel-injectable, node-unit-tested with fakes) + the page (`index.html`, `app.js`, `RUN.md`). (3) The optional puppeteer gate: headless chrome, two pages, real BroadcastChannel + RTCDataChannel, both DOMs converge. (4) Docs.

**Tech Stack:** rune (the demo program), JavaScript ES modules (sync.js/app.js), WebRTC (RTCPeerConnection/RTCDataChannel), BroadcastChannel, node (build.mjs + unit tests), wabt (build-time WAT assembly), puppeteer (optional gate), Go harness.

**Design source:** `docs/superpowers/specs/2026-07-03-twotab-demo-design.md`.

## Global Constraints

- **No changes to codegen/, the glue template, or the ABI.** 6f consumes 6d as shipped; an ABI gap found here is a 6d bug fixed in its own commit with its own test, not patched around in app.js.
- **Work in a fresh git worktree off main; explicit-pathspec commits.** Concurrent agents are active in this repo (feat/script-ergonomics in flight).
- **The glue ABI (from the shipped `codegen/wasm_library.go` template):** `load(bytes)` instantiates with stub WASI + runs init and returns a handle `h` (raw exports at `h.exports`); `call(h, name, ...ptrs)`; `mkBin(h, uint8array) -> ptr`; `readBin(h, ptr) -> Uint8Array` (copies); `readNat(h, ptr) -> number` (saturating u32); `retain(h, ptr)`/`release(h, ptr)`. Ownership: values returned to JS are owned by JS (release when done); call args are consumed; def accessors are internal to call. READ the generated glue once at implementation start and correct any signature drift here against reality; the ABI is frozen, the wording here is not authoritative over the code.
- **ch565 runnable surface** (listings/ch565_gc_codec.rune): `initGC : GC`, `bump : GC -> GC`, `value : GC -> Nat`, `merge : GC -> GC -> GC`, `gcToBin : GC -> Bin`, `gcFromBin : Bin -> GC`. The demo program re-declares these (proofs stay in ch565).
- **Honest-claims footer text (spec Decision 3), verbatim on the page:** "Machine-checked: the counter state, the merge (convergence proven: commutative, idempotent, associative), and the wire codec (round-trip theorem). Trusted glue: WebRTC transport, signaling, and this page's JavaScript."
- **Skips, not failures**, when node/wabt/puppeteer/built artifacts are absent.
- NO em or en dashes anywhere. Conventional Commits. `go build ./... && go vet ./harness/` clean before each commit that touches Go.

## Reference: verified anchors (main @ 60f89b3)

- Build CLI: `rune build FILE [name] [--kind app|library] [--export R[:H]] [--out dir]` (cmd/rune/build.go); LibraryBackend emits `<name>.wat` + `<name>.glue.js`.
- wabt assembly pattern (harness/browserlib/driver.mjs): `const wabt = await (await import("wabt")).default(); const mod = wabt.parseWat("m.wat", watText); const { buffer } = mod.toBinary({});` wabt lives in `harness/node_modules` (setup.sh section 7); build.mjs must import it from there (relative path or createRequire; the driver's resolution notes are in .superpowers/sdd history, simplest: run build.mjs with cwd=harness or resolve via `new URL("../../harness/node_modules/wabt/index.js", import.meta.url)`; pick one and document).
- The 6d node gate (harness/wasm_library_test.go) shows how a Go test builds the library artifacts in-process; the puppeteer gate reuses that or shells `build.mjs`, implementer's call, stated in the test comment.
- setup.sh idiom: section 7 (wabt) is the template for the puppeteer section; CHECK_ONLY gating per the file's convention.
- Static serving from a Go test: `net/http.FileServer` on `httptest.NewServer` or a `http.Server` on :0; puppeteer pages navigate to the served URL. BroadcastChannel + WebRTC work headless in one chrome process across two pages of the same browser context WITHOUT special flags (verify empirically; if RTC needs flags, `--no-sandbox` and nothing else is acceptable in a test).

---

### Task 1: The demo program + build script + load smoke

**Files:**
- Create: `examples/twotab/counter.rune`
- Create: `examples/twotab/build.mjs`
- Create: `examples/twotab/.gitignore` (the built `counter.wasm`, `counter.wat`, `counter.glue.js` are artifacts, not sources)
- Test: `examples/twotab/smoke.mjs` (node script asserting the built module loads + value(initGC)=0 through the glue) and a `TestTwoTabBuild` gate in `harness/wasm_library_test.go` that runs build.mjs + smoke.mjs when node+wabt present

**Interfaces:**
- Consumes: ch565's def shapes; `rune build --kind library --target wasm`; wabt.
- Produces: `examples/twotab/counter.{wasm,glue.js}` on disk after `node build.mjs`; exports named exactly `initGC,bump,value,merge,gcToBin,gcFromBin` (Tasks 2-3 call these through the glue).

- [ ] **Step 1: counter.rune**

Copy the RUNNABLE defs from listings/ch565_gc_codec.rune (data GC, NList, max, initGC, bump, value, merge, append, encodeNat/decodeNatFrame or the actual frame helpers, encodeGC, decodeGC, the foreign Bin decls, gcToBin, gcFromBin) WITHOUT the theorem section (proofs live in ch565; the demo program only runs). Header comment: "The two-tab demo program. Runnable twin of ch565 (the proofs live there); rebuild with build.mjs."

- [ ] **Step 2: build.mjs**

```js
// build.mjs: rune build (library, wasm) then wabt-assemble WAT -> wasm.
// Usage: node build.mjs   (from examples/twotab/; rune binary from PATH or ../../rune)
import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdtempSync } from "node:fs";
import { fileURLToPath } from "node:url";
import path from "node:path";
const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, "../..");
const exportsList = ["initGC", "bump", "value", "merge", "gcToBin", "gcFromBin"];
const args = ["build", path.join(here, "counter.rune"), "--kind", "library",
  "--target", "wasm", "--out", here, ...exportsList.flatMap(e => ["--export", e])];
const rune = process.env.RUNE_BIN || "go";
if (rune === "go") execFileSync("go", ["run", "./cmd/rune", ...args], { cwd: repo, stdio: "inherit" });
else execFileSync(rune, args, { stdio: "inherit" });
const wabt = await (await import(path.join(repo, "harness/node_modules/wabt/index.js"))).default();
const wat = readFileSync(path.join(here, "counter.wat"), "utf8");
const mod = wabt.parseWat("counter.wat", wat);
writeFileSync(path.join(here, "counter.wasm"), Buffer.from(mod.toBinary({}).buffer));
console.log("built counter.wasm + counter.glue.js");
```

Adjust the artifact names/paths to what `rune build --out` actually writes (read cmd/rune/build.go + golang_library conventions; if the wat lands under a subdir or a different stem, fix the script, not the backend).

- [ ] **Step 3: smoke.mjs**

```js
// smoke.mjs: the built artifacts load in node exactly as a browser would.
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import path from "node:path";
const here = path.dirname(fileURLToPath(import.meta.url));
const glue = await import(pathToFileURL(path.join(here, "counter.glue.js")));
const h = await glue.load(readFileSync(path.join(here, "counter.wasm")));
const s0 = glue.call(h, "initGC");
const v = glue.call(h, "value", s0);
const n = glue.readNat(h, v);
if (n !== 0) { console.error(`value(initGC) = ${n}, want 0`); process.exit(1); }
console.log("smoke ok");
```

- [ ] **Step 4: the Go gate**

```go
// TestTwoTabBuild: the demo builds and its artifacts load (node + wabt gated).
func TestTwoTabBuild(t *testing.T) {
	requireBrowserLib(t)
	dir := repoPath(t, "examples/twotab") // add a tiny helper or inline filepath.Join from the module root
	run := func(script string) {
		cmd := exec.Command("node", script)
		cmd.Dir = dir
		out, err := cmd.CombinedOutput()
		if err != nil { t.Fatalf("%s: %v\n%s", script, err, out) }
	}
	run("build.mjs")
	run("smoke.mjs")
}
```

Run: `go test -run TestTwoTabBuild ./harness/` after `bash bin/setup.sh`. Expected: PASS ("built", "smoke ok"). Confirm the .gitignore keeps artifacts out of `git status`.

- [ ] **Step 5: Commit**

```bash
git add examples/twotab/counter.rune examples/twotab/build.mjs examples/twotab/smoke.mjs examples/twotab/.gitignore harness/wasm_library_test.go
git commit -m "feat(examples): two-tab demo program + build script (library wasm + wabt assembly)" -- examples/twotab harness/wasm_library_test.go
```

---

### Task 2: sync.js (channel-injectable protocol) + the page

**Files:**
- Create: `examples/twotab/sync.js`
- Create: `examples/twotab/sync_test.mjs` (node unit test with fake channels)
- Create: `examples/twotab/app.js`
- Create: `examples/twotab/index.html`
- Create: `examples/twotab/RUN.md`
- Test: `TestTwoTabSyncUnit` in `harness/wasm_library_test.go` (runs `node sync_test.mjs`, node-gated)

**Interfaces:**
- Consumes: Task 1's artifacts + glue ABI.
- Produces: `createSync({ signal, rtcFactory, onPeerState, onStatus }) -> { start(), sendState(bytes) }` in sync.js, where `signal` is a BroadcastChannel-shaped object (`postMessage`, `onmessage`), `rtcFactory()` returns an RTCPeerConnection-shaped object, `onPeerState(Uint8Array)` fires per received state, `onStatus(string)` fires on connection-state changes. app.js wires the real browser objects; sync_test.mjs wires fakes.

- [ ] **Step 1: sync.js**

```js
// sync.js: two-tab WebRTC sync. Signaling over an injected channel (the real
// app passes a BroadcastChannel), data over an injected RTCPeerConnection
// factory. Roles: both tabs broadcast hello with a random session id; the
// LOWER id becomes the offerer (deterministic collision rule). On data-channel
// open each side sends its current state once (initial sync); every local
// bump sends again. Receiving always merges: duplicates and reordering are
// harmless by theorem (ch72 mergeComm/Idem/Assoc + ch565 round-trip).
export function createSync({ signal, rtcFactory, onPeerState, onStatus, getState }) {
  const id = Math.random().toString(36).slice(2);
  let pc = null, dc = null, offerer = false, peerId = null;
  const say = (m) => signal.postMessage({ ...m, from: id });
  const wire = (channel) => {
    dc = channel;
    dc.binaryType = "arraybuffer";
    dc.onopen = () => { onStatus("connected"); sendState(getState()); };
    dc.onmessage = (e) => onPeerState(new Uint8Array(e.data));
    dc.onclose = () => onStatus("peer left");
  };
  const start = () => {
    signal.onmessage = async ({ data: m }) => {
      if (m.from === id) return;
      if (m.t === "hello") {
        if (peerId) return;           // already paired
        peerId = m.from;
        say({ t: "hello2" });         // answer the hello so BOTH sides learn the pair
        if (id < m.from) await makeOffer();
      } else if (m.t === "hello2") {
        if (peerId) return;
        peerId = m.from;
        if (id < m.from) await makeOffer();
      } else if (m.t === "offer" && m.to === id) {
        pc = rtcFactory();
        pc.ondatachannel = (e) => wire(e.channel);
        pc.onicecandidate = (e) => e.candidate && say({ t: "ice", to: peerId, c: e.candidate });
        await pc.setRemoteDescription(m.sdp);
        const ans = await pc.createAnswer();
        await pc.setLocalDescription(ans);
        say({ t: "answer", to: peerId, sdp: pc.localDescription });
      } else if (m.t === "answer" && m.to === id) {
        await pc.setRemoteDescription(m.sdp);
      } else if (m.t === "ice" && m.to === id && pc) {
        await pc.addIceCandidate(m.c);
      }
    };
    say({ t: "hello" });
    onStatus("waiting for peer");
  };
  const makeOffer = async () => {
    offerer = true;
    pc = rtcFactory();
    wire(pc.createDataChannel("crdt"));
    pc.onicecandidate = (e) => e.candidate && say({ t: "ice", to: peerId, c: e.candidate });
    const off = await pc.createOffer();
    await pc.setLocalDescription(off);
    say({ t: "offer", to: peerId, sdp: pc.localDescription });
  };
  const sendState = (bytes) => { if (dc && dc.readyState === "open") dc.send(bytes); };
  return { start, sendState };
}
```

(The exact hello/hello2 pairing and role rule are the contract; the implementer may simplify if a variant proves cleaner under the unit test, keeping determinism: exactly one offerer per pair, re-broadcast tolerated.)

- [ ] **Step 2: sync_test.mjs with fakes**

Fake signal = two objects sharing an array bus with manual pump; fake RTC = a pair factory whose data channels are directly cross-wired (send on A fires onmessage on B). Test scenarios: (a) two syncs started, exactly one becomes offerer, channel opens, both statuses hit "connected"; (b) initial sync: each side's getState called and delivered to the peer's onPeerState; (c) sendState after "bump" delivers bytes; (d) duplicate delivery of the same bytes is fine (protocol layer does not dedupe; just assert delivery, the merge idempotence is the app's theorem). Assert with plain `assert` from node:assert; exit nonzero on failure. Print "sync unit ok".

- [ ] **Step 3: app.js + index.html + RUN.md**

app.js:

```js
import { createSync } from "./sync.js";
const glue = await import("./counter.glue.js");
const h = await glue.load(await (await fetch("./counter.wasm")).arrayBuffer());
let state = glue.call(h, "initGC");
const $ = (q) => document.querySelector(q);
const render = () => {
  const v = glue.call(h, "value", state);
  $("#count").textContent = glue.readNat(h, v);
  glue.release(h, v);
};
const encode = () => {
  const b = glue.call(h, "gcToBin", state);      // note: gcToBin consumes? NO:
  // call() consumes its ARGUMENTS; state is passed, so retain first:
  // (the ownership contract: args are moved; we keep using state after)
  const bytes = glue.readBin(h, b);
  glue.release(h, b);
  return bytes;
};
// IMPLEMENTER NOTE: every glue.call that passes `state` (value, gcToBin,
// merge) MOVES it per the ABI. The app must glue.retain(h, state) before any
// such call whose result is not the new state. Get this right against the
// ownership contract in the glue header and PIN it with the puppeteer gate's
// repeated bumps (a UAF here shows as wrong counts or a trap on the Nth bump).
const sync = createSync({
  signal: new BroadcastChannel("twotab-crdt"),
  rtcFactory: () => new RTCPeerConnection(),
  getState: encode,
  onPeerState: (bytes) => {
    const rb = glue.mkBin(h, bytes);
    const remote = glue.call(h, "gcFromBin", rb);
    state = glue.call(h, "merge", state, remote);
    render();
  },
  onStatus: (s) => { $("#status").textContent = s; },
});
$("#bump").onclick = () => {
  state = glue.call(h, "bump", state);
  render();
  sync.sendState(encode());
};
render();
sync.start();
```

index.html: title, `#count` div (large), `#bump` button, `#status` div, the verbatim honest-claims footer from Global Constraints, `<script type="module" src="./app.js">`. Minimal inline CSS (centered column, large count).

RUN.md: `bash ../../bin/setup.sh` (once, for wabt), `node build.mjs`, `python3 -m http.server 8000`, open `http://localhost:8000/` twice, bump in each, watch both converge; kill a tab and reopen to see re-sync; one sentence on the BroadcastChannel-signaling shortcut and what production replaces it with.

- [ ] **Step 4: Go unit gate + run**

```go
func TestTwoTabSyncUnit(t *testing.T) {
	if _, err := exec.LookPath("node"); err != nil { t.Skip("node not in PATH") }
	cmd := exec.Command("node", "sync_test.mjs")
	cmd.Dir = repoPath(t, "examples/twotab")
	out, err := cmd.CombinedOutput()
	if err != nil { t.Fatalf("sync unit: %v\n%s", err, out) }
}
```

Run: `go test -run 'TestTwoTabSyncUnit|TestTwoTabBuild' ./harness/`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add examples/twotab/sync.js examples/twotab/sync_test.mjs examples/twotab/app.js examples/twotab/index.html examples/twotab/RUN.md harness/wasm_library_test.go
git commit -m "feat(examples): two-tab page (sync protocol + DOM + RUN.md)" -- examples/twotab harness/wasm_library_test.go
```

---

### Task 3: The puppeteer gate

**Files:**
- Modify: `bin/setup.sh` (section 8: puppeteer, mirroring section 7's idiom incl CHECK_ONLY; pin the version npm reports; note the chrome download size)
- Create: `examples/twotab/e2e.mjs` (the puppeteer driver)
- Test: `TestTwoTabDemo` in `harness/wasm_library_test.go`

**Interfaces:**
- Consumes: Tasks 1-2 (built artifacts + the page); puppeteer from harness/node_modules.
- Produces: the automated two-tab convergence pin.

- [ ] **Step 1: setup.sh section 8**

Mirror section 7 exactly (have node; presence-check harness/node_modules/puppeteer; install when not CHECK_ONLY; ok/warn lines; note "downloads a headless chrome, ~120MB" in the warn text; same refresh-nit comment).

- [ ] **Step 2: e2e.mjs**

```js
// e2e.mjs <url>: two pages, one browser (BroadcastChannel + WebRTC work
// within a single browser context). Bump twice in A, once in B, poll both
// DOMs until both show 3 or 20s elapses. Prints "e2e ok" or exits 1.
import puppeteer from "puppeteer";
const url = process.argv[2];
const browser = await puppeteer.launch({ args: ["--no-sandbox"] });
try {
  const A = await browser.newPage(); await A.goto(url);
  const B = await browser.newPage(); await B.goto(url);
  const status = (p) => p.$eval("#status", (e) => e.textContent);
  const deadline = Date.now() + 20000;
  while (Date.now() < deadline && !((await status(A)).includes("connected") && (await status(B)).includes("connected")))
    await new Promise((r) => setTimeout(r, 200));
  await A.click("#bump"); await A.click("#bump");
  await B.click("#bump");
  const count = (p) => p.$eval("#count", (e) => e.textContent);
  while (Date.now() < deadline && !((await count(A)) === "3" && (await count(B)) === "3"))
    await new Promise((r) => setTimeout(r, 200));
  const [a, b] = [await count(A), await count(B)];
  if (a !== "3" || b !== "3") { console.error(`no convergence: A=${a} B=${b}`); process.exit(1); }
  console.log("e2e ok");
} finally { await browser.close(); }
```

(Module resolution: run with cwd=harness like driver.mjs, page URL absolute; or place e2e.mjs so `import "puppeteer"` resolves from harness/node_modules per the Task-3 report of 6d; verify empirically.)

- [ ] **Step 3: TestTwoTabDemo**

```go
// TestTwoTabDemo: the real two-tab claim. Serves examples/twotab statically,
// drives two headless-chrome pages, asserts both DOMs converge to 3.
// Skips: node, puppeteer, or the built artifacts absent (run TestTwoTabBuild
// or build.mjs first; this gate builds them itself when missing? NO: it
// invokes build.mjs itself so the gate is self-contained, node+wabt gated).
func TestTwoTabDemo(t *testing.T) {
	requireBrowserLib(t)
	if _, err := os.Stat(repoPath(t, "harness/node_modules/puppeteer")); err != nil {
		t.Skip("puppeteer not installed (bin/setup.sh)")
	}
	dir := repoPath(t, "examples/twotab")
	build := exec.Command("node", "build.mjs"); build.Dir = dir
	if out, err := build.CombinedOutput(); err != nil { t.Fatalf("build: %v\n%s", err, out) }
	srv := httptest.NewServer(http.FileServer(http.Dir(dir)))
	defer srv.Close()
	e2e := exec.Command("node", "e2e.mjs", srv.URL+"/index.html")
	e2e.Dir = dir
	out, err := e2e.CombinedOutput()
	if err != nil { t.Fatalf("e2e: %v\n%s", err, out) }
	if !strings.Contains(string(out), "e2e ok") { t.Fatalf("no e2e ok: %s", out) }
}
```

(Adjust puppeteer import resolution per Step 2's note; httptest server binds localhost which headless chrome reaches fine.)

- [ ] **Step 4: Run everything**

Run: `bash bin/setup.sh` then `go test -run 'TestTwoTab' ./harness/ -timeout 10m -v`
Expected: build, sync-unit, demo all PASS; demo prints e2e ok. Also run the demo MANUALLY once per RUN.md and confirm by eye (this is the artifact; the human check is part of acceptance). Record what you saw in the report.

- [ ] **Step 5: Commit**

```bash
git add bin/setup.sh examples/twotab/e2e.mjs harness/wasm_library_test.go
git commit -m "test(harness): puppeteer two-tab convergence gate (the 6f browser pin)" -- bin/setup.sh examples/twotab/e2e.mjs harness/wasm_library_test.go
```

---

### Task 4: Docs

**Files:**
- Modify: `docs/superpowers/plans/00-INDEX.md` (Plan 6 decomposition: 6f DONE with commit range + the one-line surface)
- Modify: `docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md` (Tier A item 5 DONE, item-2/3/4 style; the remaining Tier A = five-outputs integration + 6g)
- Modify: `ref_docs/wootz/R-ARC.md` (Plan-6 downstream list: 6f landed)
- Modify: `README.md` IF it references the demo as future (check; only touch if stale)

**Interfaces:** consumes the real commit hashes from this branch's log.

- [ ] **Step 1: Edits per the file list**, dash check on added lines (`git diff --cached | grep '^+' | grep -P '[\x{2013}\x{2014}]'` empty).

- [ ] **Step 2: Full gate + commit**

Run: `go build ./... && go vet ./harness/ && go test ./harness/ -run 'TwoTab|WasmBrowserLibrary|GCCodec|Bytes' -timeout 15m`
Expected: green.

```bash
git add docs/superpowers/plans/00-INDEX.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md ref_docs/wootz/R-ARC.md
git commit -m "docs: 6f two-tab demo landed" -- docs/superpowers/plans/00-INDEX.md docs/superpowers/plans/2026-07-01-wavelet-beta-remaining.md ref_docs/wootz/R-ARC.md
```

---

## Self-Review

**Spec coverage:** Decision 1 (static demo files) -> Tasks 1-2; Decision 2 (BroadcastChannel signaling + real RTCDataChannel + initial-sync-on-open + always-merge) -> Task 2's sync.js contract + app.js; Decision 3 (honest-claims footer, verbatim text pinned in Global Constraints) -> Task 2 index.html; Decision 4 (manual RUN.md + optional puppeteer gate) -> Task 2 RUN.md + Task 3; non-goals respected (no codegen change: the ABI-gap rule is in Global Constraints; no cross-machine; counter only).
**Placeholder scan:** the sync.js and app.js listings are complete contracts with two flagged implementer-judgment zones (pairing-rule simplification under test; the state-retain discipline pinned by the puppeteer gate). The build-artifact naming is read-the-real-CLI, anchored.
**Type consistency:** createSync's option names identical in sync.js/sync_test/app.js; export list identical Task 1-3; repoPath helper named once and reused; gate names TestTwoTabBuild/SyncUnit/Demo consistent.
**Honest notes:** (1) The app.js ownership discipline (retain before consuming calls) is the likeliest bug site; the puppeteer gate's repeated bumps are the designed net, and the note says exactly where to look. (2) WebRTC-under-headless is verified empirically in Task 3; the fallback flag is pinned to --no-sandbox only. (3) build.mjs's `go run ./cmd/rune` default keeps the demo buildable without an installed rune binary; RUNE_BIN overrides.
