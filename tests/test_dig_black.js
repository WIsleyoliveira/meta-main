// Digitizer on a black-and-white figure (solid curve and dashed curve that cross, grey grid lines).
// Usage: start the app on port 4760, then: node tests/test_dig_black.js http://127.0.0.1:4760
const { chromium } = require(process.env.PLAYWRIGHT_PATH || '/opt/npm-tools/node_modules/playwright');
const fs = require('fs');
const AXIS_IN = Number(process.env.AXIS_IN || 0);   // put the y = 0 calibration this many pixels below the x axis, so the axis line lies inside the traced area
const url = process.argv[2] || 'http://127.0.0.1:4760';
const FIG = process.env.FIG || 'E';   // E: black curves; F: grey frame, curve starts right of the y axis, curves run past the last tick
const cal = JSON.parse(fs.readFileSync('tests/out/valfig_' + FIG + '_cal.json'));
const truth = fs.readFileSync('tests/out/valfig_' + FIG + '_truth.csv', 'utf8').trim().split('\n').slice(1).map(l => l.split(',').map(Number));
const ok = (c, m) => { console.log((c ? 'OK   ' : 'FAIL ') + m); if (!c) process.exitCode = 1; };
const interp = (x, col) => { for (let i = 1; i < truth.length; i++) if (truth[i][0] >= x) { const a = truth[i - 1], b = truth[i]; return a[col] + (b[col] - a[col]) * (x - a[0]) / (b[0] - a[0] || 1); } return truth[truth.length - 1][col]; };
(async () => {
  const b = await chromium.launch({ executablePath: process.env.CHROME_PATH || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const pg = await b.newPage({ viewport: { width: 1600, height: 1500 } });
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  await pg.goto(url); await pg.waitForTimeout(2000);
  await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(500);
  await pg.setInputFiles('#dig_file', 'tests/out/' + cal.file); await pg.waitForTimeout(1500);
  const at = async (px, py) => pg.evaluate(([px, py]) => { const cv = document.getElementById('dig_canvas'); cv.scrollIntoView({ block: 'center' });
    const r = cv.getBoundingClientRect(), z = window.KMDigitizer.zoom; return { x: r.left + px * z, y: r.top + py * z }; }, [px, py]);
  await pg.fill('#dig_x1v', '0'); await pg.fill('#dig_x2v', '20'); await pg.fill('#dig_y1v', '0'); await pg.fill('#dig_y2v', '1');
  for (const [m, px, py] of [['cal-x1', cal.x0, cal.y0], ['cal-x2', cal.x20, cal.y0], ['cal-y1', cal.x0, cal.y0 + AXIS_IN], ['cal-y2', cal.x0, cal.y1]]) {
    await pg.click(`[data-dig-mode="${m}"]`); const p = await at(px, py); await pg.mouse.click(p.x, p.y);
  }
  const trace = async (COL, t0, name) => {
    await pg.fill('#dig_study', 'Black'); await pg.fill('#dig_arm', name);
    await pg.click('[data-dig-mode="pick"]');
    const p = await at(cal.x0 + (cal.x20 - cal.x0) * t0 / 20, cal.y0 + AXIS_IN + (cal.y1 - cal.y0 - AXIS_IN) * interp(t0, COL)); await pg.mouse.click(p.x, p.y);
    await pg.click('[data-dig-act="trace"]'); await pg.waitForTimeout(300);
    const r = await pg.evaluate(([x0, x20, y0, y1]) => window.KMDigitizer.pts.map(q => [(q.px - x0) * 20 / (x20 - x0), (y0 - q.py) / (y0 - y1)]), [cal.x0, cal.x20, cal.y0, cal.y1]);
    const dev = (col) => { const e = r.filter(q => q[0] > 0.5 && q[0] < (FIG === 'F' ? 22.2 : 19.5)).map(q => Math.abs(q[1] - interp(q[0], col))).sort((a, c) => a - c); return { mean: e.reduce((a, c) => a + c, 0) / e.length, p95: e[Math.floor(.95 * e.length)] }; };
    return { n: r.length, own: dev(COL), other: dev(COL === 1 ? 2 : 1), r };
  };
  // 1. the continuous curve first (nothing to ignore yet)
  const s1 = await trace(1, 1, 'Continuous');
  console.log('start of continuous: x=' + s1.r[0][0].toFixed(2) + ' y=' + s1.r[0][1].toFixed(3) + '  end: x=' + s1.r[s1.r.length - 1][0].toFixed(2));
  console.log('continuous: points', s1.n, 'error vs solid', JSON.stringify(s1.own));
  ok(s1.own.p95 < 0.01, 'the continuous black curve is traced (95% of points within 0.01)');
  await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(400);
  // 2. the dashed curve without the ignore option: it falls onto the continuous curve (this is the problem)
  const s2 = await trace(2, 12, 'Dashed');
  console.log('dashed WITHOUT ignoring: points', s2.n, 'error vs dashed', JSON.stringify(s2.own), 'vs solid', JSON.stringify(s2.other));
  // 3. the dashed curve with "Ignore curves already saved"
  await pg.check('#dig_mask');
  const s3 = await trace(2, 12, 'Dashed');
  console.log('dashed WITH ignoring: points', s3.n, 'error vs dashed', JSON.stringify(s3.own), 'vs solid', JSON.stringify(s3.other));
  console.log(s3.r.filter((q,i)=>i%8==0).map(q=>q[0].toFixed(1)+':'+q[1].toFixed(2)).join(' '));
  const rowShare = (r) => { const c = {}; r.forEach(q => { const k = q[1].toFixed(3); c[k] = (c[k] || 0) + 1; }); return Math.max(...Object.values(c)) / r.length; };
  ok(rowShare(s1.r) < 0.3 && rowShare(s3.r) < 0.3, 'no long run of points along one horizontal line (an axis line is not traced)');
  ok(s3.r.every(q => q[0] > 0.05), 'nothing is placed on the vertical axis');
  // the beginning: points in the first two years (the dashed curve hides under the continuous one there) must exist and be right
  // the very first points are the top of the curve (checked separately), so the error is taken from 0.3 years on
  // where the curve is almost vertical one pixel of horizontal error is several percent, so the error is measured as the distance
  // to the true curve allowing 2 pixels (0.06 years here) in x
  const nearErr = (q, col) => { let m = 9; for (let dx = -0.06; dx <= 0.0601; dx += 0.01) m = Math.min(m, Math.abs(q[1] - interp(q[0] + dx, col))); return m; };
  // before 0.3 years (about 10 pixels) the dashed curve is almost vertical and a gap between two dashes can hide most of it: see the README
  const early = s3.r.filter(q => q[0] > 0.3 && q[0] < 2); const eerr = early.map(q => nearErr(q, 2));
  console.log(s3.r.filter(q => q[0] < 1.2).map(q => q[0].toFixed(2) + ':' + q[1].toFixed(3) + '/' + interp(q[0], 2).toFixed(3)).join(' '));
  { const w = early.map((q, i) => [eerr[i], q]).sort((a, b) => b[0] - a[0]).slice(0, 3); console.log('worst early points', w.map(([e, q]) => 'x=' + q[0].toFixed(2) + ' y=' + q[1].toFixed(3) + ' err=' + e.toFixed(3) + ' truth=' + interp(q[0], 2).toFixed(3)).join(' | ')); }
  console.log('first 2 years: points', early.length, 'max error', Math.max(...eerr).toFixed(4), 'first point at x =', s3.r[0][0].toFixed(2));
  ok(early.length > 15 && Math.max(...eerr) < 0.02, 'the dashed curve starts at the beginning and its hidden start is filled in');
  ok(s3.r[0][0] < 0.3 && Math.abs(s3.r[0][1] - 1) < 0.03, 'the first point is at the start of the curve (top left)');
  if (FIG === 'F') {
    ok(Math.max(...s3.r.map(q => q[0])) > 21.5, 'the trace runs past the last labelled tick, to the end of the curve');
    ok(Math.max(...s1.r.map(q => q[0])) > 21.5, 'the continuous curve too');
    ok(s1.r[0][0] < 0.4 && s1.r[0][1] > 0.93, 'the continuous curve is traced from its very start (not from the axis)');
  }
  // the first year: the dashed curve is clearly below the continuous one here, and the trace must follow the dashed one
  const win = (r, col) => { const e = r.filter(q => q[0] > 0.15 && q[0] < 1.5).map(q => Math.min(...[-0.06, -0.03, 0, 0.03, 0.06].map(dx => Math.abs(q[1] - interp(q[0] + dx, col))))); return e.reduce((a, c) => a + c, 0) / e.length; };
  console.log('first year (0.15 to 1.5): mean error vs dashed', win(s3.r, 2).toFixed(4), ' vs solid', win(s3.r, 1).toFixed(4));
  ok(win(s3.r, 2) < 0.012 && win(s3.r, 2) < win(s3.r, 1), 'in the first year the trace follows the dashed curve, not the continuous one');
  const dense = (r) => r.filter(q => q[0] < 0.6).length;
  console.log('points in the first 0.6 years: continuous', dense(s1.r), ' dashed', dense(s3.r));
  ok(dense(s1.r) >= 14 && dense(s3.r) >= 8, 'the steep early drop is sampled densely (14 or more points on the continuous curve in the first 0.6 years, 8 or more on the dashed one)');
  ok(s1.r[0][1] > 0.99 && s3.r[0][1] > 0.99, 'the first point of each curve is at the top (survival 1.0)');
  ok(s3.n > 30, 'enough dashed-curve points');
  ok(s3.own.p95 < 0.015, 'the dashed curve is followed (95% of points within 0.015 of it)');
  ok(s3.own.mean < s3.other.mean, 'closer to the dashed curve than to the continuous one');
  await b.close();
})();
