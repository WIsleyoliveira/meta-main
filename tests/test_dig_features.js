// Browser test: digitize two arms of a figure with known curves, check the overlay, save the project,
// reload the page, open the project, and send everything to the Kaplan-Meier analysis.
const { chromium } = require(process.env.PLAYWRIGHT_PATH || '/opt/npm-tools/node_modules/playwright');
const fs = require('fs');
const url = process.argv[2] || 'http://127.0.0.1:4762';
const cal = JSON.parse(fs.readFileSync('tests/out/valfig_cal.json')).A;
const ok = (c, m) => { if (!c) { console.log('FAIL', m); process.exitCode = 1; } else console.log('OK  ', m); };
(async () => {
  const b = await chromium.launch({ executablePath: process.env.CHROME_PATH || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const ctx = await b.newContext({ acceptDownloads: true, viewport: { width: 1600, height: 1500 } });
  const pg = await ctx.newPage(); const errs = []; pg.on('pageerror', e => errs.push(e.message));
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  const setup = async () => { await pg.goto(url); await pg.waitForTimeout(2000); await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(500);
    await pg.setInputFiles('#dig_file', 'tests/out/' + cal.file); await pg.waitForTimeout(1200); };
  const at = async (px, py) => pg.evaluate(([px, py]) => { const cv = document.getElementById('dig_canvas'); cv.scrollIntoView({ block: 'center' });
    const r = cv.getBoundingClientRect(), z = window.KMDigitizer.zoom; return { x: r.left + px * z, y: r.top + py * z }; }, [px, py]);
  await setup();
  await pg.fill('#dig_x1v', '0'); await pg.fill('#dig_x2v', '60'); await pg.fill('#dig_y1v', '0'); await pg.fill('#dig_y2v', '1');
  for (const [m, px, py] of [['cal-x1', cal.x0, cal.y0], ['cal-x2', cal.x60, cal.y0], ['cal-y1', cal.x0, cal.y0], ['cal-y2', cal.x0, cal.y1]]) {
    await pg.click(`[data-dig-mode="${m}"]`); const p = await at(px, py); await pg.mouse.click(p.x, p.y); }
  const arms = [['Intervention', 0.025, 'treatment', '90', '200, 148, 109, 81, 60, 44'], ['Control', 0.045, 'control', '130', '200, 117, 68, 40, 23, 13']];
  await pg.fill('#dig_study', 'Trial S');
  for (const [arm, rate] of arms) {
    await pg.fill('#dig_arm', arm); await pg.click('[data-dig-mode="pick"]');
    const t = 8, p = await at(cal.x0 + (cal.x60 - cal.x0) * t / 60, cal.y0 + (cal.y1 - cal.y0) * Math.exp(-rate * t)); await pg.mouse.click(p.x, p.y);
    await pg.click('[data-dig-act="trace"]'); await pg.waitForTimeout(250); await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(500);
  }
  for (let i = 0; i < 2; i++) {
    await pg.evaluate(i => $('#dig_pair')[0].selectize.setValue(String(i + 1)), i); await pg.waitForTimeout(300);
    await pg.evaluate(v => $('#dig_group')[0].selectize.setValue(v), arms[i][2]);
    await pg.fill('#dig_ntotal', '200'); await pg.fill('#dig_events', arms[i][3]); await pg.fill('#dig_times', '0, 12, 24, 36, 48, 60'); await pg.fill('#dig_nrisk', arms[i][4]);
    await pg.click('#dig_add_arm'); await pg.waitForTimeout(500);
  }
  // 1. overlay
  await pg.click('#dig_check'); await pg.waitForTimeout(6000);
  const ov = await pg.evaluate(() => window.KMDigitizer.overlay.map(o => ({ arm: o.arm, t: o.time, s: o.surv })));
  ok(ov.length === 2, 'the overlay has both rebuilt curves');
  const rate = { Intervention: 0.025, Control: 0.045 };
  for (const o of ov) { let mx = 0; for (let i = 0; i < o.t.length; i++) { if (o.t[i] > 2 && o.t[i] < 58) mx = Math.max(mx, Math.abs(o.s[i] - Math.exp(-rate[o.arm] * o.t[i]))); }
    ok(mx < 0.04, `rebuilt curve for ${o.arm} stays within 0.04 of the true curve (max ${mx.toFixed(3)})`); }
  await pg.screenshot({ path: 'tests/out/dig_overlay.png', fullPage: false });
  await pg.click('[data-dig-act="clear-overlay"]'); ok((await pg.evaluate(() => window.KMDigitizer.overlay.length)) === 0, 'overlay can be removed');
  // 2. save project
  const [dl] = await Promise.all([pg.waitForEvent('download'), pg.click('#proj_save')]);
  const path = 'tests/out/test_project.json'; await dl.saveAs(path);
  const pj = JSON.parse(fs.readFileSync(path));
  ok(pj.app === 'Ponderis' && pj.digitizer.curve.length > 100 && pj.digitizer.arms.length === 2 && pj.digitizer.risk.length === 12, 'the saved project holds the curves, risk table and arms');
  // 3. reload and open
  await pg.goto(url); await pg.waitForTimeout(2000); await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(500);
  await pg.setInputFiles('#proj_open', path); await pg.waitForTimeout(2500);
  const txt = await pg.textContent('body');
  ok(/Project opened/.test(txt), 'opening the project shows a confirmation');
  await pg.click('#dig_tabs a[data-value="saved"]'); await pg.waitForTimeout(2200);
  ok((await pg.locator('#dig_tbl_arms tbody tr').count()) === 2 && /Trial S/.test(await pg.textContent('#dig_tbl_arms')), 'the arms table is restored');
  ok((await pg.locator('#dig_tbl_risk tbody tr').count()) >= 6, 'the risk table is restored');
  // 4. use in the analysis
  await pg.click('#dig_use'); await pg.waitForTimeout(7000);
  ok(await pg.isVisible('a[data-value=pooled]'), 'the analysis opens with the restored data');
  await pg.click('.results:visible .nav-link:has-text("Hazard ratios")'); await pg.waitForTimeout(1500);
  const hrTxt = (await pg.locator('.results:visible').innerText()).replace(/\s+/g, ' ');
  const m = hrTxt.match(/(\d\.\d+)\s*\((\d\.\d+)\s*(?:to|-|\u2013)\s*(\d\.\d+)\)/);
  console.log('   hazard ratio tab:', hrTxt.slice(0, 220));
  ok(!!m && Math.abs(parseFloat(m[1]) - 0.556) < 0.06, 'pooled HR from the restored project is close to the true 0.556');
  await pg.screenshot({ path: 'tests/out/dig_after_project.png' });
  ok(errs.length === 0, 'no page errors: ' + JSON.stringify(errs));
  await b.close();
})();
