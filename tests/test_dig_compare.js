// Browser test of the library flow: save each curve once with all its details, then build comparisons from the saved curves
// (no digitizing again), check the hazard ratios, the names, the time multiplier and the project file.
const { chromium } = require(process.env.PLAYWRIGHT_PATH || '/opt/npm-tools/node_modules/playwright');
const fs = require('fs');
const url = process.argv[2] || 'http://127.0.0.1:4747';
const cal = JSON.parse(fs.readFileSync('tests/out/valfig_cal.json')).A;
const ok = (c, m) => { if (!c) { console.log('FAIL', m); process.exitCode = 1; } else console.log('OK  ', m); };
(async () => {
  const b = await chromium.launch({ executablePath: process.env.CHROME_PATH || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const ctx = await b.newContext({ acceptDownloads: true, viewport: { width: 1600, height: 1500 } });
  const pg = await ctx.newPage(); const errs = []; pg.on('pageerror', e => errs.push(e.message));
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  await pg.goto(url); await pg.waitForTimeout(2000); await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(500);
  await pg.setInputFiles('#dig_file', 'tests/out/' + cal.file); await pg.waitForTimeout(1200);
  const at = async (px, py) => { await pg.click('#dig_tabs a[data-value="fig"]'); await pg.waitForTimeout(500); await pg.evaluate(() => document.getElementById('dig_canvas').scrollIntoView({ block: 'center', behavior: 'instant' })); await pg.waitForTimeout(450);
    return pg.evaluate(([px, py]) => { const cv = document.getElementById('dig_canvas'), r = cv.getBoundingClientRect(), z = window.KMDigitizer.zoom; return { x: r.left + px * z, y: r.top + py * z }; }, [px, py]); };
  // the figure's time axis runs 0 to 5 YEARS; the analysis wants months: multiplier 12
  await pg.fill('#dig_x1v', '0'); await pg.fill('#dig_x2v', '5'); await pg.fill('#dig_y1v', '0'); await pg.fill('#dig_y2v', '1');
  for (const [m, px, py] of [['cal-x1', cal.x0, cal.y0], ['cal-x2', cal.x60, cal.y0], ['cal-y1', cal.x0, cal.y0], ['cal-y2', cal.x0, cal.y1]]) {
    await pg.click(`[data-dig-mode="${m}"]`); const p = await at(px, py); await pg.mouse.click(p.x, p.y); }
  const arms = [['Intervention', 0.025, 'arm A', '200', '90', '200, 148, 109, 81, 60, 44'], ['Control', 0.045, 'arm B', '200', '130', '200, 117, 68, 40, 23, 13']];
  await pg.fill('#dig_study', 'Trial S'); await pg.fill('#dig_tt', '0, 1, 2, 3, 4, 5'); await pg.fill('#dig_tmult', '12');
  for (const [arm, rate, sub, n, ev, nr] of arms) {
    await pg.fill('#dig_arm', arm); await pg.click('[data-dig-mode="pick"]');
    const t = 8, p = await at(cal.x0 + (cal.x60 - cal.x0) * t / 60, cal.y0 + (cal.y1 - cal.y0) * Math.exp(-rate * t)); await pg.mouse.click(p.x, p.y);
    await pg.click('[data-dig-act="trace"]'); await pg.waitForTimeout(250);
    await pg.fill('#dig_sub', sub); await pg.fill('#dig_n', n); await pg.fill('#dig_ev', ev); await pg.fill('#dig_nr', nr);
    await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(900);
  }
  // 1. one save kept everything
  await pg.click('#dig_tabs a[data-value="saved"]'); await pg.waitForTimeout(2200);
  const tbl = (await pg.textContent('#dig_tbl_curves')).replace(/\s+/g, ' ');
  ok(/Intervention/.test(tbl) && /arm A/.test(tbl) && /arm B/.test(tbl) && /200/.test(tbl), 'one save stored the curves with subgroup and patients');
  ok((await pg.locator('#dig_tbl_arms tbody tr').count()) === 2 && (await pg.locator('#dig_tbl_risk tbody tr').count()) >= 6, 'the arms and the risk table came with it');
  const riskTxt = (await pg.textContent('#dig_tbl_risk')).replace(/\s+/g, ' ');
  console.log('   risk table:', riskTxt.slice(0, 200));
  ok(/Intervention12148/.test(riskTxt) && /Intervention6044/.test(riskTxt), 'the risk times were multiplied by 12 (years to months): 12 and 60 appear');
  const jt = await pg.evaluate(() => window.KMDigitizer.pts.length);   // points were cleared after saving
  ok(jt === 0, 'the points were cleared after saving');
  ok((await pg.inputValue('#dig_n')) === '' && (await pg.inputValue('#dig_tt')) === '0, 1, 2, 3, 4, 5' && (await pg.inputValue('#dig_tmult')) === '12', 'the details were cleared, the times and the multiplier kept');
  // the check overlay is drawn in the time unit of the figure (years), although the saved curves are in months
  await pg.click('#dig_check'); await pg.waitForTimeout(6500);
  const ovT = await pg.evaluate(() => (window.KMDigitizer.overlay || []).map(o => Math.max.apply(null, o.time)));
  ok(ovT.length === 2 && ovT.every(v => v > 3 && v <= 5.2), 'the check overlay is drawn in the figure\'s own time unit: ' + JSON.stringify(ovT));
  // 2. comparison A: Intervention (treatment) against Control
  await pg.click('#dig_tabs a[data-value="saved"]'); await pg.waitForTimeout(2200);
  const row = async (name) => pg.locator('#dig_tbl_curves tbody tr', { hasText: name }).first().click();
  const hr = async () => { await pg.click('.results:visible .nav-link:has-text("Hazard ratios")'); await pg.waitForTimeout(1500);
    const t = (await pg.locator('.results:visible').innerText()).replace(/\s+/g, ' '); const m = t.match(/(\d\.\d+)\s*\((\d\.\d+)\s*(?:to|-|–)\s*(\d\.\d+)\)/); return { m, t }; };
  await pg.fill('#dig_cmp_name', 'Mismatch test'); await pg.fill('#dig_unit', 'Trial S');
  await row('Intervention'); await pg.click('#dig_add_tx'); await pg.waitForTimeout(400);
  await row('Control'); await pg.click('#dig_add_ct'); await pg.waitForTimeout(400);
  const stage = (await pg.textContent('#dig_stage_ui')).replace(/\s+/g, ' ');
  ok(/Treatment side: Intervention - arm A \(Trial S\)/.test(stage) && /Control side: Control - arm B \(Trial S\)/.test(stage), 'the two sides show the curves with their subgroup: ' + stage);
  await pg.click('#dig_add_unit'); await pg.waitForTimeout(700);
  ok((await pg.locator('#dig_tbl_cmp tbody tr').count()) === 2, 'the unit appears in the comparison table');
  await pg.fill('#dig_lab_tx', 'Sex mismatch'); await pg.fill('#dig_lab_ct', 'Sex match');
  await pg.click('#dig_use'); await pg.waitForTimeout(7000);
  ok(await pg.isVisible('a[data-value=pooled]'), 'the analysis opens from the comparison');
  ok((await pg.inputValue('#km_name_tx')) === 'Sex mismatch' && (await pg.inputValue('#km_name_ctrl')) === 'Sex match', 'the names of the two sides reach the analysis');
  let r = await hr(); console.log('   A:', r.t.slice(0, 160));
  ok(!!r.m && Math.abs(parseFloat(r.m[1]) - 0.556) < 0.07, 'comparison A: the HR is close to the true 0.556');
  ok(/Sex mismatch/.test(r.t) || /Sex mismatch/.test(await pg.locator('.results:visible').innerText()), 'the plot text uses the names of the sides');
  // time unit: the last reconstructed time should be near 60 months, not 5
  const dataTab = await pg.textContent('body');
  // 3. comparison B from the same saved curves: the roles reversed, nothing digitized again
  await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(800);
  await pg.fill('#dig_cmp_name', 'Reverse'); await pg.fill('#dig_unit', 'Trial S reversed');
  await row('Control'); await pg.click('#dig_add_tx'); await pg.waitForTimeout(400);
  await row('Intervention'); await pg.click('#dig_add_ct'); await pg.waitForTimeout(400);
  await pg.click('#dig_add_unit'); await pg.waitForTimeout(700);
  await pg.click('#dig_use'); await pg.waitForTimeout(7000);
  console.log('   mode now:', await pg.evaluate(() => $('input[name=mode]:checked').val()), '| notes:', (await pg.locator('.shiny-notification').allInnerTexts()).join(' || ').slice(0, 400)); await pg.screenshot({ path: 'tests/out/dig_cmp_B.png', fullPage: true });
  r = await hr(); console.log('   B:', r.t.slice(0, 160));
  ok(!!r.m && Math.abs(parseFloat(r.m[1]) - 1 / 0.556) < 0.2, 'comparison B (roles reversed): the HR is close to 1/0.556');
  // 3b. the same study and arm name with ANOTHER subgroup is a different curve and must not replace the first one
  await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(800);
  await pg.fill('#dig_arm', 'Control'); await pg.click('[data-dig-mode="pick"]');
  { const t = 8, p = await at(cal.x0 + (cal.x60 - cal.x0) * t / 60, cal.y0 + (cal.y1 - cal.y0) * Math.exp(-0.045 * t)); await pg.mouse.click(p.x, p.y); }
  await pg.click('[data-dig-act="trace"]'); await pg.waitForTimeout(250);
  await pg.fill('#dig_sub', 'arm C'); await pg.fill('#dig_n', '150'); await pg.fill('#dig_nr', '150, 90, 50, 30, 15, 8');
  await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(1200);
  await pg.click('#dig_tabs a[data-value="saved"]'); await pg.waitForTimeout(2200);
  const tb2 = (await pg.textContent('#dig_tbl_curves')).replace(/\s+/g, ' ');
  ok(/arm B/.test(tb2) && /arm C/.test(tb2) && /Control \(arm C\)/.test(tb2) && (await pg.locator('#dig_tbl_curves tbody tr').count()) === 3, 'a curve with the same arm name and another subgroup was kept as a new curve: ' + tb2.slice(0, 160));
  // saving the same curve again, with the same subgroup, replaces it (a re-trace)
  await pg.fill('#dig_arm', 'Control'); await pg.fill('#dig_sub', 'arm C'); await pg.fill('#dig_n', '150'); await pg.fill('#dig_nr', '150, 90, 50, 30, 15, 8');
  await pg.evaluate(() => { const S = window.KMDigitizer; S.pts = S.masks[S.masks.length - 1].map(q => Object.assign({}, q)); });
  await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(1200);
  ok((await pg.locator('#dig_tbl_curves tbody tr').count()) === 3, 'saving the same curve and subgroup again replaces it instead of adding another');
  // 4. both comparisons are kept in the project, and open again
  await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(800);
  const [dl] = await Promise.all([pg.waitForEvent('download'), pg.click('#proj_save')]);
  const path = 'tests/out/test_project_cmp.json'; await dl.saveAs(path);
  const pj = JSON.parse(fs.readFileSync(path));
  ok(pj.digitizer.cmp.length === 4 && new Set(pj.digitizer.cmp.map(x => x.comparison)).size === 2, 'the project holds both comparisons');
  ok(pj.digitizer.arms.every(a => a.subgroup) && pj.digitizer.curve.every(c => +c.survival <= 1), 'the project holds the subgroups and survival proportions');
  await pg.goto(url); await pg.waitForTimeout(2000); await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(500);
  await pg.setInputFiles('#proj_open', path); await pg.waitForTimeout(2500);
  await pg.click('#dig_tabs a[data-value="saved"]'); await pg.waitForTimeout(2200);
  const opts = await pg.evaluate(() => Object.keys($('#dig_cmp_pick')[0].selectize.options));
  ok(opts.includes('Mismatch test') && opts.includes('Reverse'), 'opening the project lists both comparisons: ' + JSON.stringify(opts));
  await pg.evaluate(() => $('#dig_cmp_pick')[0].selectize.setValue('Mismatch test')); await pg.waitForTimeout(800);
  ok((await pg.inputValue('#dig_cmp_name')) === 'Mismatch test' && (await pg.locator('#dig_tbl_cmp tbody tr').count()) === 2, 'choosing one shows its units');
  await pg.screenshot({ path: 'tests/out/dig_compare.png', fullPage: true });
  ok(errs.length === 0, 'no page errors: ' + JSON.stringify(errs));
  await b.close();
})();
