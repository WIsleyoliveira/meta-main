// Snapping AI tick positions onto the tick marks of a figure, and finding the real curve colour (www/dig_ai.js).
// Pure functions on synthetic RGBA images: run with `node tests/test_dig_snap.js` (no browser).
const { snapTick, snapAxis, axisTicksUsable, nearestColour, hexToRgb } = require('../www/dig_ai.js');
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
  if (opts.minor) for (let x = 100 + 43; x < 960; x += 43) if ((x - 100) % 172) rect(x, 602, x, 605, black);   // minor ticks, 4 px
  if (opts.aaEnds) { rect(99, 40, 100, 42, [150, 150, 150]); rect(91, 40, 98, 40, black); }   // grey anti-aliased end of the y axis, top tick at its end
  if (opts.cornerLabel) rect(70, 594, 84, 607, black);   // the "0" of the y label "0.0" crossing the x axis row
  if (opts.blackOnAxis) rect(700, 597, 960, 599, black); // a black curve that reached 0 lies on the x axis
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

// ---- review fixes ----
{ // a tick-label digit crossing the axis row is not a tick (it lies outside the axis line)
  const { img, xt } = image(1000, 700, { cornerLabel: true });
  const r = snapTick(img, 'x', xt[0] - 15, 604);
  ok(!r.snapped || Math.abs(r.px - (xt[0] + 0.5)) <= 1, 'corner label digit does not capture', JSON.stringify(r));
}
{ // the tick at the anti-aliased end of an axis line still snaps
  const { img } = image(1000, 700, { aaEnds: true });
  const r = snapTick(img, 'y', 88, 30);
  ok(r.snapped && Math.abs(r.py - 40) <= 1, 'tick at a grey (anti-aliased) axis end', JSON.stringify(r));
}
{ // a black curve lying on the axis: no wrong snap
  const { img, xt } = image(1000, 700, { blackOnAxis: true });
  const r = snapTick(img, 'x', xt[5] - 12, 604), q = snapTick(img, 'x', xt[4] + 10, 604);
  ok(!r.snapped || Math.abs(r.px - (xt[5] + 0.5)) <= 1, 'black curve on the axis (last tick)', JSON.stringify(r));
  ok(!q.snapped || Math.abs(q.px - (xt[4] + 0.5)) <= 1, 'black curve on the axis (tick inside)', JSON.stringify(q));
}
{ // minor ticks: a proposal 25 px off lands on a minor tick; the axis check flags it
  const { img, xt } = image(1000, 700, { minor: true });
  const ticks = [0, 12, 24, 36, 48, 60].map((v, i) => ({ value: v, px: xt[i] + (i === 5 ? -25 : 3), py: 604 }));
  const ax = snapAxis(img, 'x', ticks, 'linear');
  ok(ax.points.length === 2 && ax.points[0].value === 0 && ax.points[1].value === 60, 'snapAxis keeps the farthest ticks');
  const last = ax.points[1];
  ok(!last.snapped || Math.abs(last.px - (xt[5] + 0.5)) <= 1, 'minor tick capture is not reported as matched', JSON.stringify(last));
  const good = snapAxis(img, 'x', [0, 12, 24, 36, 48, 60].map((v, i) => ({ value: v, px: xt[i] + 4, py: 604 })), 'linear');
  ok(good.points.every(p => p.snapped) && Math.abs(good.points[1].px - (xt[5] + 0.5)) <= 1, 'consistent ticks stay matched');
}
{ // two arms of similar colours: each arm gets its own colour
  const W = 1000, H = 700, data = new Uint8ClampedArray(W * H * 4).fill(255);
  const put = (x0, x1, y, c) => { for (let x = x0; x <= x1; x++) for (let d = -1; d <= 1; d++) { const i = ((y + d) * W + x) * 4; data[i] = c[0]; data[i + 1] = c[1]; data[i + 2] = c[2]; } };
  put(120, 400, 200, [31, 119, 180]); put(120, 900, 300, [23, 190, 207]);   // blue (short) and cyan (long)
  const img = { width: W, height: H, data };
  const c = nearestColour(img, '#1f77b4', null, ['#17becf']);
  ok(Math.hypot(c[0] - 31, c[1] - 119, c[2] - 180) < 6, 'nearestColour keeps the arm apart from a similar longer curve', JSON.stringify(c));
}
{ // an axis read with fewer than two distinct values cannot be calibrated
  ok(!axisTicksUsable([{ value: 0 }, { value: 0 }]) && !axisTicksUsable([{ value: 1 }]) && axisTicksUsable([{ value: 0 }, { value: 60 }]), 'axisTicksUsable');
}

if (failed) { console.log(failed + ' FAILED'); process.exit(1); }
console.log('DIG SNAP TESTS PASSED');
