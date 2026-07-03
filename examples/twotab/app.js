// app.js: the two-tab demo page's DOM wiring. Loads counter.glue.js (the
// generated glue for counter.wasm, 6f Task 1), drives the G-Counter CRDT
// through it, and syncs state with the other tab via sync.js.
//
// OWNERSHIP DISCIPLINE (glue ABI, see counter.glue.js's header): `glue.call`
// MOVES every argument it is given (rt_apply consumes its args); a value
// `glue.call` RETURNS is owned by the caller (release it when done). `state`
// is held across many calls in this file, so every call site below is one of
// two shapes:
//   - the call's RESULT becomes the new `state` (bump, merge): no retain
//     needed -- the old state is meant to be consumed, that is exactly what
//     "replaced by the new state" means under move semantics.
//   - the call's result does NOT become the new `state` (value, gcToBin):
//     `glue.retain(h, state)` FIRST, so the moved reference is a throwaway
//     duplicate and `state` itself survives for later use. Skipping this
//     retain is a use-after-free that shows up as wrong counts or a trap on
//     a later bump, not an immediate crash (examples/twotab/ownership_check.mjs
//     pins this against the real glue, including that exact failure mode).
import { createSync } from "./sync.js";

const glue = await import("./counter.glue.js");
const h = await glue.load(await (await fetch("./counter.wasm")).arrayBuffer());

let state = glue.call(h, "initGC");

const $ = (q) => document.querySelector(q);

const render = () => {
  glue.retain(h, state); // "value"'s result is not the new state; keep state alive
  const v = glue.call(h, "value", state);
  $("#count").textContent = glue.readNat(h, v);
  glue.release(h, v);
};

const encode = () => {
  glue.retain(h, state); // "gcToBin"'s result is not the new state; keep state alive
  const b = glue.call(h, "gcToBin", state);
  const bytes = glue.readBin(h, b);
  glue.release(h, b);
  return bytes;
};

const sync = createSync({
  signal: new BroadcastChannel("twotab-crdt"),
  rtcFactory: () => new RTCPeerConnection(),
  getState: encode,
  onPeerState: (bytes) => {
    const rb = glue.mkBin(h, bytes);
    const remote = glue.call(h, "gcFromBin", rb); // consumes rb
    // "merge"'s result BECOMES the new state: both `state` and `remote` are
    // meant to be consumed here, no retain needed (mirrors the click
    // handler's `state = glue.call(h, "bump", state)` below).
    state = glue.call(h, "merge", state, remote);
    render();
  },
  onStatus: (s) => {
    $("#status").textContent = s;
  },
});

$("#bump").onclick = () => {
  state = glue.call(h, "bump", state); // consumes the old state, returns the new one
  render();
  sync.sendState(encode());
};

render();
sync.start();
