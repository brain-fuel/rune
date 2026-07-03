// sync_test.mjs: node-only unit test for sync.js's pairing/transport protocol,
// driven entirely by fakes -- no BroadcastChannel, no RTCPeerConnection, so it
// runs under plain node (no browser, no puppeteer). Two fake signal endpoints
// share a manually-pumped message bus (models BroadcastChannel: postMessage on
// one side never re-delivers to itself, only to the other); a fake RTC pair
// factory cross-wires the offerer's local data channel to a synthesized remote
// channel on the answerer's peer connection so `send` on one fires `onmessage`
// on the other, without any real WebRTC negotiation.
//
// Usage: node sync_test.mjs   (exits nonzero + throws on the first failed
// assertion; prints "sync unit ok" on success).
import assert from "node:assert";
import { createSync } from "./sync.js";

// ============================================================================
// Fake signaling: two BroadcastChannel-shaped endpoints sharing two one-way
// queues (A -> B and B -> A), pumped manually so the test controls scheduling.
// ============================================================================
function makeSignalPair() {
  const toA = [];
  const toB = [];
  const a = { postMessage: (m) => toB.push(m), onmessage: null };
  const b = { postMessage: (m) => toA.push(m), onmessage: null };
  const pump = () => {
    let progressed = true;
    while (progressed) {
      progressed = false;
      while (toA.length) {
        const m = toA.shift();
        progressed = true;
        if (a.onmessage) a.onmessage({ data: m });
      }
      while (toB.length) {
        const m = toB.shift();
        progressed = true;
        if (b.onmessage) b.onmessage({ data: m });
      }
    }
  };
  return { a, b, pump };
}

// ============================================================================
// Fake RTC: a pair factory whose two RTCPeerConnection-shaped objects get
// their data channels cross-wired once the offerer has created a local
// channel (via createDataChannel) and the answerer has registered
// ondatachannel (via the "offer" message handler in sync.js). Both events
// happen at different, async-separated points in the real protocol, so
// wiring is retried on a microtask until both sides are ready.
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
      if (ch._peer && ch._peer.onmessage) ch._peer.onmessage({ data });
    },
  };
  return ch;
}

function makeRTCPairFactory() {
  const pcs = [];
  let dataChannelsCreated = 0;

  const wireIfReady = () => {
    if (pcs.length < 2) return;
    const [offererPc, answererPc] = pcs;
    if (!offererPc._localChannel || !answererPc.ondatachannel) {
      queueMicrotask(wireIfReady);
      return;
    }
    if (offererPc._localChannel._peer) return; // already wired
    const remote = makeFakeChannel(offererPc._localChannel.label);
    offererPc._localChannel._peer = remote;
    remote._peer = offererPc._localChannel;
    offererPc._localChannel.readyState = "open";
    remote.readyState = "open";
    answererPc.ondatachannel({ channel: remote });
    // Fire the "open" event on both sides now that both are wired, mirroring
    // a real RTCDataChannel's open event after negotiation completes.
    if (offererPc._localChannel.onopen) offererPc._localChannel.onopen();
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
    if (pcs.length === 2) queueMicrotask(wireIfReady);
    return pc;
  };

  return { factory, offererCount: () => dataChannelsCreated };
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

// ============================================================================
// Scenario (a): two syncs started, exactly one becomes offerer, the channel
// opens, and both statuses hit "connected".
// ============================================================================
async function scenarioA() {
  const { a: sigA, b: sigB, pump } = makeSignalPair();
  const rtc = makeRTCPairFactory();
  const statusesA = [];
  const statusesB = [];

  const syncA = createSync({
    signal: sigA, rtcFactory: rtc.factory,
    getState: () => enc("state-A"),
    onPeerState: () => {},
    onStatus: (s) => statusesA.push(s),
  });
  const syncB = createSync({
    signal: sigB, rtcFactory: rtc.factory,
    getState: () => enc("state-B"),
    onPeerState: () => {},
    onStatus: (s) => statusesB.push(s),
  });

  syncA.start();
  syncB.start();
  await settle(pump);

  assert.ok(statusesA.includes("waiting for peer"), "A should announce waiting for peer");
  assert.ok(statusesB.includes("waiting for peer"), "B should announce waiting for peer");
  assert.ok(statusesA.includes("connected"), "A should reach connected");
  assert.ok(statusesB.includes("connected"), "B should reach connected");
  assert.strictEqual(rtc.offererCount(), 1, "exactly one side should create the data channel (be the offerer)");

  return { sigA, sigB, pump, rtc };
}

// ============================================================================
// Scenario (b): initial sync -- each side's getState is called and delivered
// to the peer's onPeerState once the channel opens.
// ============================================================================
async function scenarioB() {
  const { a: sigA, b: sigB, pump } = makeSignalPair();
  const rtc = makeRTCPairFactory();
  const receivedA = [];
  const receivedB = [];

  const syncA = createSync({
    signal: sigA, rtcFactory: rtc.factory,
    getState: () => enc("initial-A"),
    onPeerState: (bytes) => receivedA.push(dec(bytes)),
    onStatus: () => {},
  });
  const syncB = createSync({
    signal: sigB, rtcFactory: rtc.factory,
    getState: () => enc("initial-B"),
    onPeerState: (bytes) => receivedB.push(dec(bytes)),
    onStatus: () => {},
  });

  syncA.start();
  syncB.start();
  await settle(pump);

  assert.deepStrictEqual(receivedA, ["initial-B"], "A should receive B's initial getState once");
  assert.deepStrictEqual(receivedB, ["initial-A"], "B should receive A's initial getState once");

  return { syncA, syncB, pump, receivedA, receivedB };
}

// ============================================================================
// Scenario (c): sendState after a local "bump" delivers the new bytes to the
// peer.
// ============================================================================
async function scenarioC() {
  const { syncA, syncB, pump, receivedB } = await scenarioB();

  syncA.sendState(enc("bumped-A-1"));
  await settle(pump);

  assert.deepStrictEqual(receivedB, ["initial-A", "bumped-A-1"], "B should receive A's post-bump state");
  return { syncA, syncB, pump, receivedB };
}

// ============================================================================
// Scenario (d): duplicate delivery of the same bytes is fine -- the protocol
// layer does not dedupe; both deliveries must arrive (merge idempotence is the
// app's theorem to rely on, not this layer's job).
// ============================================================================
async function scenarioD() {
  const { syncA, pump, receivedB } = await scenarioC();

  const dupBytes = enc("dup-state");
  syncA.sendState(dupBytes);
  syncA.sendState(dupBytes);
  await settle(pump);

  assert.deepStrictEqual(
    receivedB,
    ["initial-A", "bumped-A-1", "dup-state", "dup-state"],
    "duplicate sendState calls must both be delivered, undeduplicated"
  );
}

async function main() {
  await scenarioA();
  await scenarioB();
  await scenarioC();
  await scenarioD();
  console.log("sync unit ok");
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
