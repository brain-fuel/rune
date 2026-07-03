# Running the two-tab CRDT demo

From the repo root, once:

```
bash bin/setup.sh
```

(installs wabt into `harness/node_modules`, used to assemble the WAT the
`rune build` step below emits into `counter.wasm`.)

From `examples/twotab/`:

```
node build.mjs
python3 -m http.server 8000
```

Open `http://localhost:8000/` in two browser tabs. Click Bump in either tab
and watch the count in BOTH tabs converge to the same total (the CvRDT
merge, proven commutative/idempotent/associative, applied over a real
WebRTC data channel). Close one tab and reopen it: the fresh tab re-syncs to
the current total automatically (each side re-sends its current state the
moment the data channel opens).

Peer discovery in this demo goes over the browser's `BroadcastChannel`,
which only works between tabs of the same browser/origin; a production
deployment keeps the real `RTCDataChannel` for the counter bytes but swaps
`BroadcastChannel` for a real signaling channel (a websocket relay, or
manual SDP exchange) so the two peers do not need to share a browser.
