// ownership_check.mjs: empirical proof that app.js's ownership discipline is
// correct against the REAL glue (not a fake) -- run this after `node
// build.mjs`. It replays the exact call pattern app.js uses: initGC, then
// several bumps (each `state = glue.call(h, "bump", state)`, mirroring the
// click handler), a `render`-shaped read (retain state, call "value", read,
// release), an `encode`-shaped read (retain state, call "gcToBin", readBin,
// release), and an `onPeerState`-shaped merge (mkBin a decoded copy of
// state's own bytes, gcFromBin it, then `state = glue.call(h, "merge",
// state, remote)`, mirroring the incoming-sync handler).
//
// Every glue.call that passes `state` as an argument MOVES it (rt_apply
// consumes its args, per the glue header's ownership contract); a call whose
// RESULT does not become the new `state` (value, gcToBin) must retain(state)
// first so the moved reference is a throwaway duplicate, not the only one.
// A call whose result DOES become the new `state` (bump, merge) needs no
// retain -- the old state is meant to be consumed, that is exactly what "the
// new state replaces the old one" means under move semantics.
//
// Balance discipline: run the identical sequence TWICE and compare
// rt_live() deltas (this repo's own steady-harness convention, e.g.
// harness/browserlib/driver.mjs's warm-up-round + baseline pattern) rather
// than asserting an absolute rt_live() of zero. The FIRST run pays every
// one-time cost the runtime allocates on first touch (each def_<name>
// accessor's memoized-cache cell, and similar first-use fixed costs); the
// SECOND run's marginal live-count growth over the first is the true
// per-session cost of this exact call pattern, and it must be exactly zero
// -- any nonzero growth is a leak (a retain this script's ownership
// reasoning failed to match with a release) and any negative growth would be
// a double-release / use-after-free signature.
import { readFileSync } from "node:fs";
import { fileURLToPath, pathToFileURL } from "node:url";
import path from "node:path";
import assert from "node:assert";

const here = path.dirname(fileURLToPath(import.meta.url));
const glue = await import(pathToFileURL(path.join(here, "counter.glue.js")));
const h = await glue.load(readFileSync(path.join(here, "counter.wasm")));

// -- app.js's render()/encode() shapes, reproduced exactly -----------------
function renderValue(state) {
  glue.retain(h, state); // state is passed to `call`, which MOVES it; keep our copy alive
  const v = glue.call(h, "value", state);
  const n = glue.readNat(h, v);
  glue.release(h, v); // v is owned by us (call's result); release when done
  return n;
}

function encode(state) {
  glue.retain(h, state); // same reasoning as renderValue: gcToBin's result != new state
  const b = glue.call(h, "gcToBin", state);
  const bytes = glue.readBin(h, b); // copies bytes out of linear memory
  glue.release(h, b);
  return bytes;
}

function mergeRemoteBytes(state, bytes) {
  const rb = glue.mkBin(h, bytes); // owned by us
  const remote = glue.call(h, "gcFromBin", rb); // consumes rb, returns owned remote
  // merge's result BECOMES the new state -- both `state` and `remote` are
  // meant to be consumed here, no retain needed (mirrors app.js's
  // `state = glue.call(h, "merge", state, remote);`).
  return glue.call(h, "merge", state, remote);
}

const N_BUMPS = 5;

// The full scripted sequence app.js's session performs: init, N bumps (each
// rendered and encoded, the click-handler pattern), a self-merge (the
// onPeerState pattern, fed the state's own just-encoded bytes -- ch72's
// mergeIdem says this must be a no-op), and a merge with a fresh zero state
// (the join-semilattice bottom, also a no-op). Every value is asserted at
// every step; the final held `state` reference is released before return so
// a caller comparing rt_live() before/after sees the true marginal cost.
function runFullSequence() {
  let state = glue.call(h, "initGC");
  assert.strictEqual(renderValue(state), 0, "value(initGC) = 0");

  let lastBytes;
  for (let i = 1; i <= N_BUMPS; i++) {
    state = glue.call(h, "bump", state); // consumes the old state, returns the new one
    assert.strictEqual(renderValue(state), i, `value after ${i} bump(s) = ${i}`);
    lastBytes = encode(state); // sync.sendState(encode()) shape, every bump
  }
  assert.strictEqual(renderValue(state), N_BUMPS, `value after ${N_BUMPS} bumps = ${N_BUMPS}`);

  // onPeerState shape: merge with a decoded copy of OUR OWN current bytes.
  // Convergence is idempotent (ch72 mergeIdem), so the value must be unchanged.
  state = mergeRemoteBytes(state, lastBytes);
  assert.strictEqual(renderValue(state), N_BUMPS, "merge with a decoded copy of self is idempotent");

  // Merge with an all-zero remote (initGC's bytes) must also be a no-op
  // (merge is the pointwise max join; zero is the join-semilattice bottom).
  const zeroState = glue.call(h, "initGC"); // owned; encode() retains its own copy internally
  const zeroBytes = encode(zeroState);
  glue.release(h, zeroState);
  state = mergeRemoteBytes(state, zeroBytes);
  assert.strictEqual(renderValue(state), N_BUMPS, "merge with the zero state is a no-op");

  glue.release(h, state); // the one reference this whole run still owns at the end
}

runFullSequence(); // warm-up: pays every one-time allocation cost
const baseline = h.exports.rt_live();
runFullSequence(); // steady-state: identical call pattern, nothing new should be live
const delta = h.exports.rt_live() - baseline;
assert.strictEqual(delta, 0, `rt_live() must be unchanged after an identical second run, got delta ${delta}`);

console.log("ownership check ok");
