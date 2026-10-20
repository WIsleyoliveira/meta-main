// Validation D: the in-browser digitizer, end to end, on synthetic figures with known curves.
// Usage: start the app (shiny::runApp(".", port = 4760)), then: node tests/validation/val_digitizer.js http://127.0.0.1:4760
const { chromium } = require(process.env.PLAYWRIGHT_PATH || '/opt/npm-tools/node_modules/playwright');
const fs = require('fs');
const url = process.argv[2] || 'http://127.0.0.1:4760';
const cal = JSON.parse(fs.readFileSync('tests/out/valfig_cal.json'));
(async () => {
  const b = await chromium.launch({ executablePath: process.env.CHROME_PATH || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const out = [];
  for (const k of Object.keys(cal)) {
    const c = cal[k];
    const pg = await b.newPage({ viewport: { width: 1600, height: 1500 } });
    await pg.goto(url); await pg.waitForTimeout(2000);
    await pg.click('.mode-bar span:has-text("Digitize")'); await pg.waitForTimeout(500);
    await pg.setInputFiles('#dig_file', 'tests/out/' + c.file); await pg.waitForTimeout(1500);
    const at = async (px, py) => pg.evaluate(([px, py]) => { const cv = document.getElementById('dig_canvas'); cv.scrollIntoView({ block: 'center' });
      const r = cv.getBoundingClientRect(), z = window.KMDigitizer.zoom; return { x: r.left + px * z, y: r.top + py * z }; }, [px, py]);
    const isLog = k === 'D';
    if (isLog) await pg.selectOption('#dig_xscale', 'log');
    await pg.fill('#dig_x1v', isLog ? '1' : '0'); await pg.fill('#dig_x2v', isLog ? '1000' : '60'); await pg.fill('#dig_y1v', '0'); await pg.fill('#dig_y2v', '1');
    for (const [m, px, py] of [['cal-x1', c.x0, c.y0], ['cal-x2', c.x60, c.y0], ['cal-y1', c.x0, c.y0], ['cal-y2', c.x0, c.y1]]) {
      await pg.click(`[data-dig-mode="${m}"]`); const p = await at(px, py); await pg.mouse.click(p.x, p.y);
    }
    if (isLog) { // the readout must follow the log scale: x = 100 is two thirds along the axis, y = 0.65 is 65% up
      const q = await at(c.x0 + (c.x60 - c.x0) * 2 / 3, c.y0 + (c.y1 - c.y0) * 0.65); await pg.mouse.move(q.x, q.y); await pg.waitForTimeout(200);
      out.push({ figure: k, file: c.file, arm: 'readout at x=100, y=0.65', readout: await pg.textContent('#dig_tip') }); }
    const curves = isLog ? [['Log-x curve', 0]] : [['Intervention', 0.025], ['Control', 0.045]];
    const truth = (rate, x) => isLog ? 0.9 - 0.25 * Math.log10(x) : Math.exp(-rate * x);
    for (const [arm, rate] of curves) {
      await pg.fill('#dig_study', 'Fig ' + k); await pg.fill('#dig_arm', arm);
      await pg.click('[data-dig-mode="pick"]');
      const fx = isLog ? Math.log10(10) / 3 : 8 / 60, px = c.x0 + (c.x60 - c.x0) * fx, py = c.y0 + (c.y1 - c.y0) * truth(rate, isLog ? 10 : 8);
      const p = await at(px, py); await pg.mouse.click(p.x, p.y);
      await pg.click('[data-dig-act="trace"]'); await pg.waitForTimeout(300);
      const r = await pg.evaluate(([rate, isLog]) => { const S = window.KMDigitizer, cc = S.cal; const errs = [];
        S.pts.forEach(p => { let x, y;
          if (isLog) { const l1 = Math.log10(cc.x1.val), l2 = Math.log10(cc.x2.val); x = Math.pow(10, l1 + (p.px - cc.x1.px) * (l2 - l1) / (cc.x2.px - cc.x1.px)); }
          else x = cc.x1.val + (p.px - cc.x1.px) * (cc.x2.val - cc.x1.val) / (cc.x2.px - cc.x1.px);
          y = cc.y1.val + (p.py - cc.y1.py) * (cc.y2.val - cc.y1.val) / (cc.y2.py - cc.y1.py);
          errs.push(Math.abs(y - (isLog ? 0.9 - 0.25 * Math.log10(x) : Math.exp(-rate * x)))); });
        errs.sort((a, b) => a - b); return { n: errs.length, mean: errs.reduce((a, b) => a + b, 0) / errs.length, p95: errs[Math.floor(.95 * errs.length)], max: errs[errs.length - 1] }; }, [rate, isLog]);
      out.push({ figure: k, file: c.file, arm, points: r.n, mean_abs_error: r.mean, p95_abs_error: r.p95, max_abs_error: r.max });
      await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(400);
    }
    await pg.close();
  }
  fs.writeFileSync('tests/out/val_digitizer.json', JSON.stringify(out, null, 1));
  console.log(JSON.stringify(out, null, 1)); await b.close();
})();
