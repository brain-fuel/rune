// sync.js: two-tab WebRTC sync. Signaling over an injected channel (the real
// app passes a BroadcastChannel), data over an injected RTCPeerConnection
// factory. Roles: both tabs broadcast hello with a random session id; the
// LOWER id becomes the offerer (deterministic collision rule). On data-channel
// open each side sends its current state once (initial sync); every local
// bump sends again. Receiving always merges: duplicates and reordering are
// harmless by theorem (ch72 mergeComm/Idem/Assoc + ch565 round-trip).
//
// This module touches no browser global directly -- every browser-only object
// (BroadcastChannel, RTCPeerConnection) is passed in by the caller, so this
// file is testable under plain node with fakes (sync_test.mjs) and the real
// page (app.js) supplies the genuine objects.
export function createSync({ signal, rtcFactory, onPeerState, onStatus, getState }) {
  const id = Math.random().toString(36).slice(2);
  let pc = null, dc = null, peerId = null;
  const say = (m) => signal.postMessage({ ...m, from: id });
  const wire = (channel) => {
    dc = channel;
    dc.binaryType = "arraybuffer";
    dc.onopen = () => { onStatus("connected"); sendState(getState()); };
    dc.onmessage = (e) => onPeerState(new Uint8Array(e.data));
    // On close, RESET the pairing state, not just the status: the survivor of
    // a killed tab must accept the reopened tab's fresh hello (otherwise the
    // `if (peerId) return` guards below would ignore it forever and the
    // kill-reopen re-sync RUN.md promises would never happen). The newcomer
    // always broadcasts hello on start, so the survivor only needs to become
    // pairable again, not to re-announce itself.
    dc.onclose = () => { onStatus("peer left"); peerId = null; pc = null; dc = null; };
  };
  const makeOffer = async () => {
    pc = rtcFactory();
    wire(pc.createDataChannel("crdt"));
    pc.onicecandidate = (e) => e.candidate && say({ t: "ice", to: peerId, c: e.candidate });
    const off = await pc.createOffer();
    await pc.setLocalDescription(off);
    say({ t: "offer", to: peerId, sdp: pc.localDescription });
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
  const sendState = (bytes) => { if (dc && dc.readyState === "open") dc.send(bytes); };
  return { start, sendState };
}
