// e2e.mjs <url>: the real two-tab claim -- two pages, ONE headless-chrome browser
// (BroadcastChannel + RTCDataChannel both work fine within a single browser
// context/process; confirmed empirically, no chrome flags beyond --no-sandbox
// needed). Waits for both pages to report "connected", bumps twice in A and once
// in B, then polls both DOMs until both show 3 or 20s elapses. Prints "e2e ok" and
// exits 0 on convergence; prints a diagnostic and exits 1 otherwise.
//
// Module resolution: puppeteer lives in harness/node_modules (installed by
// bin/setup.sh section 8), and this script lives under examples/twotab/, not
// under harness/ -- so a bare `import "puppeteer"` would fail the same way a bare
// `import "wabt"` would from build.mjs (node's ESM loader resolves a bare
// specifier by walking up from the IMPORTING FILE's own location, and
// examples/twotab is not an ancestor of harness/node_modules). Fixed the same way
// build.mjs resolves wabt: an absolute path to puppeteer's own entry file
// (package.json "main": "./lib/puppeteer/puppeteer.js"), computed from this
// file's location so it needs no cwd assumption.
//
// Click method: `ElementHandle.click()` (puppeteer's synthetic mouse click, which
// waits on a bounding-box/visibility check tied to the page actually producing
// render frames) was empirically found to HANG INDEFINITELY on both pages in this
// headless-chrome setup, even though the page's JS thread stays fully responsive
// the entire time (confirmed: `page.evaluate(() => 1+1)` resolves instantly while
// a concurrent `.click()` call is stuck). A direct DOM click dispatched from
// inside the page via `page.evaluate(() => el.click())` has none of that
// frame-visibility dependency and resolves in low single-digit milliseconds, so
// it is used here instead -- not a chrome flag or a timeout change, just a
// different (still first-party) puppeteer API for the same user action.
import { fileURLToPath } from "node:url";
import path from "node:path";

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, "../..");
const puppeteer = (await import(path.join(repo, "harness", "node_modules", "puppeteer", "lib", "puppeteer", "puppeteer.js"))).default;

const url = process.argv[2];
if (!url) {
  console.error("usage: node e2e.mjs <url>");
  process.exit(1);
}

const clickBump = (page) => page.evaluate(() => document.querySelector("#bump").click());

const browser = await puppeteer.launch({ args: ["--no-sandbox"] });
try {
  const A = await browser.newPage();
  await A.goto(url);
  const B = await browser.newPage();
  await B.goto(url);

  const status = (p) => p.$eval("#status", (e) => e.textContent);
  const deadline = Date.now() + 20000;
  while (Date.now() < deadline && !((await status(A)).includes("connected") && (await status(B)).includes("connected"))) {
    await new Promise((r) => setTimeout(r, 200));
  }
  const [sa, sb] = [await status(A), await status(B)];
  if (!sa.includes("connected") || !sb.includes("connected")) {
    console.error(`no pairing: A status=${sa} B status=${sb}`);
    process.exit(1);
  }

  await clickBump(A);
  await clickBump(A);
  await clickBump(B);

  const count = (p) => p.$eval("#count", (e) => e.textContent);
  const deadline2 = Date.now() + 20000;
  while (Date.now() < deadline2 && !((await count(A)) === "3" && (await count(B)) === "3")) {
    await new Promise((r) => setTimeout(r, 200));
  }
  const [a, b] = [await count(A), await count(B)];
  if (a !== "3" || b !== "3") {
    console.error(`no convergence: A=${a} B=${b}`);
    process.exit(1);
  }
  console.log("e2e ok");
} finally {
  await browser.close();
}
