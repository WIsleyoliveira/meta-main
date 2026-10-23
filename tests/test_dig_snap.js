// Snapping AI tick positions onto the tick marks of a figure, and finding the real curve colour (www/dig_ai.js).
// Pure functions on synthetic RGBA images: run with `node tests/test_dig_snap.js` (no browser).
const { snapTick, nearestColour, hexToRgb } = require('../www/dig_ai.js');
let failed = 0;
const ok = (cond, name, extra) => { if (cond) console.log('ok:', name); else { failed++; console.log('FAIL:', name, extra || ''); } };

function image(W, H, opts) {
  opts = opts || {};
  const data = new Uint8ClampedArray(W * H * 4).fill(255);
  const set = (x, y, c) => { if (x < 0 || y < 0 || x >= W || y >= H) return; const i = (y * W + x) * 4; data[i] = c[0]; data[i + 1] = c[1]; data[i + 2] = c[2]; data[i + 3] = 255; };
  const rect = (x0, y0, x1, y1, c) => { for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) set(x, y, c); };
  const black = [20, 20, 20], grey = [221, 221, 221];
  const xt = [], yt = [];
  for (let k = 0; k <= 5; k++) xt.push(100 + k * 172);
  for (let k = 0; k <= 5; k++) yt.push(600 - k * 112);
  if (opts.grid) { xt.forEach(x => rect(x, 40, x, 599, grey)); yt.forEach(y => rect(101, y, 960, y, grey)); }
  if (opts.curve) {   // a survival-like curve in colour, reaching down to the axis on the right
    for (let x = 101; x <= 960; x++) { const y = Math.round(40 + (x - 100) / 860 * 556); rect(x, y - 1, x, y + 1, opts.curve); }
  }
  rect(100, 600, 960, 601, black);   // x axis line (2 px)
  rect(99, 40, 100, 601, black);     // y axis line (2 px)
  if (opts.thin) {   // 1 px anti-aliased ticks, as R's axis() draws them at 100 dpi: grey, not black
    const g = [122, 122, 122];
    xt.forEach(x => rect(x, 602, x, 609, g)); yt.forEach(y => rect(91, y, 98, y, g));
  } else if (!opts.noTicks) {
    xt.forEach(x => rect(x, 602, x + 1, 608, black));     // x ticks: 2 px wide, 7 px long, centre x + 0.5
    yt.forEach(y => rect(92, y, 98, y + 1, black));       // y ticks: 2 px tall, 7 px long, centre y + 0.5
  }
  xt.forEach(x => rect(x - 8, 615, x + 8, 626, black));   // tick labels as text blobs, below the ticks with a gap
  yt.forEach(y => rect(60, y - 5, 84, y + 6, black));
  return { img: { width: W, height: H, data }, xt, yt };
}

// ---- x and y ticks from noisy proposals ----
{
  const { img, xt, yt } = image(1000, 700);
  let worst = 0;
  for (const [i, d] of [[0, 10], [2, -10], [5, 7], [3, -4]]) {
    const r = snapTick(img, 'x', xt[i] + d, 600 + 5);
    worst = Math.max(worst, Math.abs(r.px - (xt[i] + 0.5)), Math.abs(r.py - 600.5));
    ok(r.snapped, 'snap x tick ' + i + ' from ' + d + ' px');
  }
  ok(worst <= 1, 'snap x within 1 px', 'worst ' + worst);
  worst = 0;
  for (const [i, d] of [[0, -9], [4, 9], [5, 3]]) {
    const r = snapTick(img, 'y', 100 - 6, yt[i] + d);
    worst = Math.max(worst, Math.abs(r.py - (yt[i] + 0.5)), Math.abs(r.px - 99.5));
    ok(r.snapped, 'snap y tick ' + i + ' from ' + d + ' px');
  }
  ok(worst <= 1, 'snap y within 1 px', 'worst ' + worst);
}

// ---- thin grey (anti-aliased) tick marks ----
{
  const { img, xt, yt } = image(1000, 700, { thin: true });
  const r = snapTick(img, 'x', xt[3] + 9, 612), q = snapTick(img, 'y', 88, yt[2] - 8);
  ok(r.snapped && Math.abs(r.px - xt[3]) <= 1, 'thin grey x ticks', JSON.stringify(r));
  ok(q.snapped && Math.abs(q.py - yt[2]) <= 1, 'thin grey y ticks', JSON.stringify(q));
}

// ---- no tick marks: keep the proposal, not snapped ----
{
  const { img, xt } = image(1000, 700, { noTicks: true });
  const r = snapTick(img, 'x', xt[2] + 6, 606);
  ok(!r.snapped && r.px === xt[2] + 6, 'no tick marks -> snapped false');
}

// ---- gridlines at every tick do not capture the snap (Review Focus 1) ----
{
  const { img, xt, yt } = image(1000, 700, { grid: true });
  const r = snapTick(img, 'x', xt[1] - 8, 604);
  ok(r.snapped && Math.abs(r.px - (xt[1] + 0.5)) <= 1 && Math.abs(r.py - 600.5) <= 1, 'gridlines do not capture (x)', JSON.stringify(r));
  const s = snapTick(img, 'y', 95, yt[3] + 8);
  ok(s.snapped && Math.abs(s.py - (yt[3] + 0.5)) <= 1 && Math.abs(s.px - 99.5) <= 1, 'gridlines do not capture (y)', JSON.stringify(s));
}

// ---- a coloured curve near the axis does not capture the snap ----
{
  const { img, xt } = image(1000, 700, { curve: [47, 93, 138] });
  const r = snapTick(img, 'x', xt[5] - 10, 603);
  ok(r.snapped && Math.abs(r.px - (xt[5] + 0.5)) <= 1, 'curve does not capture', JSON.stringify(r));
}

// ---- the real curve colour from a lighter legend hex (Review Focus 2) ----
{
  const curve = [47, 93, 138];
  const { img } = image(1000, 700, { curve, grid: true });
  const c = nearestColour(img, '#3366A0', { x0: 101, y0: 40, x1: 960, y1: 599 });
  const d = Math.hypot(c[0] - curve[0], c[1] - curve[1], c[2] - curve[2]);
  ok(d < 6, 'nearestColour from a lighter legend hex', JSON.stringify(c));
  ok(JSON.stringify(hexToRgb('#B23A48')) === JSON.stringify([178, 58, 72]), 'hexToRgb');
}

if (failed) { console.log(failed + ' FAILED'); process.exit(1); }
console.log('DIG SNAP TESTS PASSED');
