// sync_test.mjs: node-only unit test for sync.js's pairing/transport protocol,
// driven entirely by fakes -- no BroadcastChannel, no RTCPeerConnection, so it
// runs under plain node (no browser, no puppeteer). The fake signal is an
// N-endpoint broadcast bus with a manual pump (models BroadcastChannel:
// postMessage never re-delivers to the sender, only to every other endpoint;
// endpoints can be removed to model a killed tab); the fake RTC factory
// cross-wires each consecutive PAIR of created peer connections -- the
// offerer's createDataChannel channel to a synthesized remote channel fired
// through the answerer's ondatachannel -- so `send` on one side fires
// `onmessage` on the other without any real WebRTC negotiation.
//
// Usage: node sync_test.mjs   (exits nonzero + throws on the first failed
// assertion; prints "sync unit ok" on success).
import assert from "node:assert";
import { createSync } from "./sync.js";

// ============================================================================
// Fake signaling: a broadcast bus. join() returns a BroadcastChannel-shaped
// endpoint; postMessage queues delivery to every OTHER joined endpoint (and
// learns the endpoint's session id from its outgoing traffic, for filters);
// pump() drains the queue; remove() models a killed tab (queued and future
// deliveries to it are dropped); setFilter() installs a per-delivery predicate
// (used by scenario (e) to force the hello2 pairing path).
// ============================================================================
function makeBus() {
  const endpoints = [];
  const queue = [];
  let filter = null;
  const join = () => {
    const ep = {
      onmessage: null,
      _id: null,
      postMessage(m) {
        ep._id = m.from; // every sync.js message carries its sender's session id
        for (const other of endpoints) {
          if (other !== ep) queue.push({ to: other, m });
        }
      },
    };
    endpoints.push(ep);
    return ep;
  };
  const remove = (ep) => {
    const i = endpoints.indexOf(ep);
    if (i >= 0) endpoints.splice(i, 1);
  };
  const pump = () => {
    while (queue.length) {
      const { to, m } = queue.shift();
      if (!endpoints.includes(to)) continue; // killed tab: drop
      if (filter && !filter(to, m)) continue;
      if (to.onmessage) to.onmessage({ data: m });
    }
  };
  return { join, remove, pump, setFilter: (f) => { filter = f; } };
}

// ============================================================================
// Fake RTC: a factory whose peer connections get cross-wired in consecutive
// PAIRS by creation order (0-1, 2-3, ...): within a pair, the side that called
// createDataChannel is the offerer; the other side's ondatachannel receives a
// synthesized remote channel. The two creation points are separated by several
// awaits in the real protocol, so wiring retries on a microtask until both
// sides are ready. closePair(k) fires onclose on both channels of the k-th
// wired pair (models the data channel dying when a tab is killed).
// ============================================================================
function makeFakeChannel(label) {
  const ch = {
    label,
    binaryType: null,
    readyState: "connecting",
    onopen: null,
    onmessage: null,
    onclose: null,
    _peer: null,
    send(data) {
      if (ch._peer && ch._peer.readyState === "open" && ch._peer.onmessage) {
        ch._peer.onmessage({ data });
      }
    },
  };
  return ch;
}

function makeRTCFactory() {
  const pcs = [];
  const wiredPairs = [];
  let dataChannelsCreated = 0;

  const wirePair = (k, attempts) => {
    const a = pcs[2 * k];
    const b = pcs[2 * k + 1];
    if (!a || !b) return; // second pc of the pair not created yet; its factory call reschedules
    const offerer = a._localChannel ? a : b._localChannel ? b : null;
    const answerer = offerer === a ? b : a;
    if (!offerer || !answerer.ondatachannel) {
      if (attempts < 1000) queueMicrotask(() => wirePair(k, attempts + 1));
      return;
    }
    if (offerer._localChannel._peer) return; // already wired
    const local = offerer._localChannel;
    const remote = makeFakeChannel(local.label);
    local._peer = remote;
    remote._peer = local;
    local.readyState = "open";
    remote.readyState = "open";
    wiredPairs.push([local, remote]);
    answerer.ondatachannel({ channel: remote });
    // Fire the "open" event on both sides now that both are wired, mirroring
    // a real RTCDataChannel's open event after negotiation completes.
    if (local.onopen) local.onopen();
    if (remote.onopen) remote.onopen();
  };

  const factory = () => {
    const pc = {
      localDescription: null,
      ondatachannel: null,
      onicecandidate: null,
      _localChannel: null,
      createDataChannel(label) {
        dataChannelsCreated++;
        const ch = makeFakeChannel(label);
        pc._localChannel = ch;
        return ch;
      },
      createOffer: async () => ({ type: "offer" }),
      createAnswer: async () => ({ type: "answer" }),
      setLocalDescription: async (desc) => { pc.localDescription = desc; },
      setRemoteDescription: async (_desc) => {},
      addIceCandidate: async (_c) => {},
    };
    pcs.push(pc);
    const k = Math.floor((pcs.length - 1) / 2);
    queueMicrotask(() => wirePair(k, 0));
    return pc;
  };

  const closePair = (k) => {
    for (const ch of wiredPairs[k]) {
      ch.readyState = "closed";
      if (ch.onclose) ch.onclose();
    }
  };

  return { factory, offererCount: () => dataChannelsCreated, closePair };
}

// Drains both the signal bus and any pending microtask-scheduled RTC wiring
// across several rounds; sync.js's handlers are async (await through fake
// promises), so a single synchronous pump misses work that only becomes
// ready after a microtask tick.
const tick = () => new Promise((resolve) => setImmediate(resolve));
async function settle(pump, rounds = 30) {
  for (let i = 0; i < rounds; i++) {
    pump();
    await tick();
  }
}

function enc(s) {
  return new TextEncoder().encode(s);
}
function dec(u8) {
  return new TextDecoder().decode(u8);
}

// makePeer: one fake tab -- a bus endpoint + a createSync over it, with its
// received states and statuses captured for assertions.
function makePeer(bus, rtc, name) {
  const signal = bus.join();
  const received = [];
  const statuses = [];
  const sync = createSync({
    signal,
    rtcFactory: rtc.factory,
    getState: () => enc(`state-${name}`),
    onPeerState: (bytes) => received.push(dec(bytes)),
    onStatus: (s) => statuses.push(s),
  });
  return { signal, sync, received, statuses };
}

// ============================================================================
// Scenario (a): two syncs started, exactly one becomes offerer, the channel
// opens, and both statuses hit "connected".
// ============================================================================
async function scenarioA() {
  const bus = makeBus();
  const rtc = makeRTCFactory();
  const a = makePeer(bus, rtc, "A");
  const b = makePeer(bus, rtc, "B");

  a.sync.start();
  b.sync.start();
  await settle(bus.pump);

  assert.ok(a.statuses.includes("waiting for peer"), "A should announce waiting for peer");
  assert.ok(b.statuses.includes("waiting for peer"), "B should announce waiting for peer");
  assert.ok(a.statuses.includes("connected"), "A should reach connected");
  assert.ok(b.statuses.includes("connected"), "B should reach connected");
  assert.strictEqual(rtc.offererCount(), 1, "exactly one side should create the data channel (be the offerer)");
}

// ============================================================================
// Scenario (b): initial sync -- each side's getState is called and delivered
// to the peer's onPeerState once the channel opens.
// ============================================================================
async function scenarioB() {
  const bus = makeBus();
  const rtc = makeRTCFactory();
  const a = makePeer(bus, rtc, "A");
  const b = makePeer(bus, rtc, "B");

  a.sync.start();
  b.sync.start();
  await settle(bus.pump);

  assert.deepStrictEqual(a.received, ["state-B"], "A should receive B's initial getState once");
  assert.deepStrictEqual(b.received, ["state-A"], "B should receive A's initial getState once");

  return { bus, a, b };
}

// ============================================================================
// Scenario (c): sendState after a local "bump" delivers the new bytes to the
// peer.
// ============================================================================
async function scenarioC() {
  const { bus, a, b } = await scenarioB();

  a.sync.sendState(enc("bumped-A-1"));
  await settle(bus.pump);

  assert.deepStrictEqual(b.received, ["state-A", "bumped-A-1"], "B should receive A's post-bump state");
  return { bus, a, b };
}

// ============================================================================
// Scenario (d): duplicate delivery of the same bytes is fine -- the protocol
// layer does not dedupe; both deliveries must arrive (merge idempotence is the
// app's theorem to rely on, not this layer's job).
// ============================================================================
async function scenarioD() {
  const { bus, a, b } = await scenarioC();

  const dupBytes = enc("dup-state");
  a.sync.sendState(dupBytes);
  a.sync.sendState(dupBytes);
  await settle(bus.pump);

  assert.deepStrictEqual(
    b.received,
    ["state-A", "bumped-A-1", "dup-state", "dup-state"],
    "duplicate sendState calls must both be delivered, undeduplicated"
  );
}

// ============================================================================
// Scenario (e): asymmetric start -- the hello2-triggers-offer path. A bus
// filter drops any "hello" addressed to the LOWER-id endpoint, so the lower
// side (the offerer under the deterministic rule) only ever sees the higher
// side's hello2 reply and must make its offer from THAT branch. Still exactly
// one offerer; the channel still opens; initial states still cross.
// ============================================================================
async function scenarioE() {
  const bus = makeBus();
  const rtc = makeRTCFactory();
  bus.setFilter((to, m) => !(m.t === "hello" && to._id !== null && to._id < m.from));
  const a = makePeer(bus, rtc, "A");
  const b = makePeer(bus, rtc, "B");

  a.sync.start();
  b.sync.start(); // both ids are learned by the bus before the first pump
  await settle(bus.pump);

  assert.strictEqual(rtc.offererCount(), 1, "hello2 path: still exactly one offerer");
  assert.ok(a.statuses.includes("connected"), "A should reach connected via the asymmetric start");
  assert.ok(b.statuses.includes("connected"), "B should reach connected via the asymmetric start");
  assert.deepStrictEqual(a.received, ["state-B"], "initial sync still crosses (A side)");
  assert.deepStrictEqual(b.received, ["state-A"], "initial sync still crosses (B side)");
}

// ============================================================================
// Scenario (f): reopen -- kill-a-tab re-pairing. Pair A and B; close the data
// channel (both sides' onclose fires, as when a tab dies) and remove B from
// the bus (the dead tab hears nothing); then a THIRD fresh peer C broadcasts
// hello. The survivor A must re-pair with C: onclose reset its pairing state,
// so C's hello is accepted, a second (single-offerer) negotiation runs, the
// channel opens, and A's current state is delivered to the newcomer.
// ============================================================================
async function scenarioF() {
  const bus = makeBus();
  const rtc = makeRTCFactory();
  const a = makePeer(bus, rtc, "A");
  const b = makePeer(bus, rtc, "B");

  a.sync.start();
  b.sync.start();
  await settle(bus.pump);
  assert.ok(a.statuses.includes("connected"), "precondition: first pairing connected");
  assert.strictEqual(rtc.offererCount(), 1, "precondition: one offerer in the first pairing");

  // Kill B: its signal endpoint leaves the bus and the data channel closes.
  bus.remove(b.signal);
  rtc.closePair(0);
  await settle(bus.pump);
  assert.ok(a.statuses.includes("peer left"), "survivor should observe the peer leaving");

  // A third fresh tab arrives and broadcasts hello.
  const c = makePeer(bus, rtc, "C");
  c.sync.start();
  await settle(bus.pump);

  assert.strictEqual(rtc.offererCount(), 2, "re-pairing runs exactly one more offer");
  assert.ok(c.statuses.includes("connected"), "newcomer should reach connected");
  assert.ok(
    a.statuses.lastIndexOf("connected") > a.statuses.indexOf("peer left"),
    "survivor should reconnect AFTER the peer left"
  );
  assert.ok(c.received.includes("state-A"), "survivor's current state is delivered to the newcomer");
  assert.ok(a.received.includes("state-C"), "newcomer's state is delivered to the survivor");
}

async function main() {
  await scenarioA();
  await scenarioB();
  await scenarioC();
  await scenarioD();
  await scenarioE();
  await scenarioF();
  console.log("sync unit ok");
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
