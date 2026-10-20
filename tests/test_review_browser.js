// Browser test: navigation, meta-regression, influence, PRISMA, risk of bias, GRADE, digitizer tabs.
const { chromium } = require('/opt/npm-tools/node_modules/playwright');
const URL = process.argv[2] || 'http://127.0.0.1:4747', SHOTS = process.argv[3] || '';
let fails = 0; const ok = (c, m) => { console.log((c ? 'OK   ' : 'FAIL ') + m); if (!c) fails++; };
(async () => {
  const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const pg = await b.newPage({ viewport: { width: 1440, height: 1000 } });
  const errs = []; pg.on('pageerror', e => errs.push(e.message));
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  await pg.goto(URL); await pg.waitForTimeout(2500);
  const shot = async n => { if (SHOTS) await pg.screenshot({ path: SHOTS + '/r_' + n + '.png', fullPage: true }); };
  const mode = async v => { await pg.click(`.mode-bar input[value="${v}"]`, { force: true }); await pg.waitForTimeout(900); };
  const tab = async (id, v) => { await pg.click(`#${id} a[data-value="${v}"]`); await pg.waitForTimeout(1800); };
  const txt = async sel => ((await pg.textContent(sel).catch(() => '')) || '').replace(/\s+/g, ' ');
  ok((await pg.locator('.mode-bar input[type=radio]').count()) === 6, 'home plus five groups in the top navigation');
  // forest
  await mode('forest'); await pg.click('#load_example2'); await pg.waitForTimeout(4500);
  await tab('tabs', 'metareg');
  ok(await pg.locator('#mr_cov').count() === 1, 'meta-regression offers a covariate');
  const opts = await pg.$$eval('#mr_cov option', o => o.map(x => x.textContent)); console.log('   covariates:', opts.join(' | '));
  ok(opts.some(x => /year/.test(x)), 'year is offered as a covariate');
  await pg.waitForTimeout(1500); await shot('metareg');
  ok((await pg.locator('#mr_plot img').count()) === 1 && /Intercept|year/.test(await txt('#mr_tbl')), 'meta-regression plot and coefficient table appear: ' + (await txt('#mr_tbl')).slice(0, 120));
  await tab('tabs', 'influence'); await shot('influence');
  ok((await pg.locator('#baujat_plot img').count()) === 1 && (await pg.locator('#radial_plot img').count()) === 1, 'Baujat and Galbraith plots appear');
  ok(await pg.locator('#infl_tbl table tbody tr').count() >= 8, 'influence table has one row per study');
  // review
  await mode('review'); await tab('rev_tabs', 'prisma');
  await pg.fill('#pr_dbs', 'PubMed, 700\nEmbase, 500'); await pg.fill('#pr_other', '30'); await pg.fill('#pr_dup', '200'); await pg.fill('#pr_excl', '900'); await pg.fill('#pr_nr', '10');
  await pg.fill('#pr_reasons', 'Wrong population, 12\nNo outcome data, 8'); await pg.click('#pr_from_fit'); await pg.waitForTimeout(1800);
  ok((await pg.inputValue('#pr_ma')) === '8', 'meta-analysis count comes from the forest plot');
  ok((await pg.locator('#pr_plot img').count()) === 1, 'PRISMA diagram drawn'); await shot('prisma');
  await pg.fill('#pr_dup', '5000'); await pg.waitForTimeout(1200);
  ok(/do not add up/.test(await txt('#pr_notice')), 'inconsistent PRISMA numbers are flagged');
  await tab('rev_tabs', 'rob'); await pg.click('#rob_from_fit'); await pg.waitForTimeout(1500);
  ok((await pg.locator('.rob-grid tbody tr').count()) === 8, 'risk-of-bias grid has the 8 studies of the forest plot');
  const sels = await pg.$$('.rob-grid select'); for (const s of sels) await s.selectOption('Low');
  await pg.waitForTimeout(3000);
  await (await pg.$$('.rob-grid select'))[1].selectOption('High'); await pg.waitForTimeout(4000); await shot('rob');
  ok((await pg.locator('#rob_fig img').count()) === 1 && /High/.test(await txt('#rob_tbl')), 'risk-of-bias figure and table appear');
  await pg.fill('#rob_letter', 'B'); await pg.waitForTimeout(2500); await shot('rob_letter'); ok((await pg.locator('#rob_fig img').count()) === 1, 'robvis-style figure drawn with a panel letter');
  await pg.evaluate(() => $('#rob_tool')[0].selectize.setValue('nos')); await pg.waitForTimeout(1500);
  ok((await pg.locator('.rob-grid input[type=number]').count()) === 24, 'Newcastle-Ottawa shows star inputs');
  await tab('rev_tabs', 'grade');
  await pg.fill('#gr_intervention', 'PCI'); await pg.fill('#gr_comparison', 'CABG'); await pg.fill('#gr_population', 'Adults with a chronic total occlusion');
  await pg.click('#gr_from_fit'); await pg.waitForTimeout(2500);
  ok(/RR/.test(await pg.inputValue('#gr_measure_1')) && parseFloat(await pg.inputValue('#gr_est_1')) > 0 && parseInt(await pg.inputValue('#gr_k_1')) > 1, 'GRADE outcome filled from the forest plot: ' + await pg.inputValue('#gr_est_1'));
  ok(/PCI compared to CABG for Adults with a chronic total occlusion/.test(await txt('#gr_sof')), 'table title follows the review question');
  ok(/High/.test(await txt('#gr_sof')) && /per 1,000/.test(await txt('#gr_sof')), 'RCT with no concerns is High, with absolute effects: ' + (await txt('#gr_sof')).slice(0, 80));
  await pg.evaluate(() => { $('#gr_rob_1')[0].selectize.setValue('1'); $('#gr_imp_1')[0].selectize.setValue('2'); }); await pg.waitForTimeout(1500);
  ok(/Low/.test(await txt('#gr_sof')) && /explain every rating/.test(await txt('#gr_issues')), 'downgrades lower the certainty and ask for an explanation');
  await pg.fill('#gr_rob_t_1', 'Unclear allocation concealment.'); await pg.fill('#gr_imp_t_1', 'The 95% CI includes appreciable benefit and harm.'); await pg.waitForTimeout(1500);
  ok(/Downgraded by one level for risk of bias: Unclear allocation concealment\./.test(await txt('#gr_sof')) && /Downgraded by two levels for imprecision/.test(await txt('#gr_sof')), 'footnotes carry the explanations');
  ok(!/explain every rating/.test(await txt('#gr_issues').catch(() => '')), 'no more warning once explained');
  await tab('gr_views', 'ep'); ok(/Certainty assessment/.test(await txt('#gr_profile')), 'evidence profile tab');
  await tab('gr_views', 'txt'); ok(/Cochrane Handbook/.test(await txt('#gr_methods')) && /downgraded by one level for risk of bias/.test(await txt('#gr_results')), 'text for the paper');
  await tab('gr_views', 'sof');
  for (let k = 0; k < 9; k++) { await pg.click('#gr_add'); await pg.waitForTimeout(350); }
  ok((await pg.locator('.gr-block').count()) === 7, 'no more than seven outcomes: ' + await pg.locator('.gr-block').count());
  await shot('grade');
  // digitizer tabs
  await mode('dig'); await shot('dig');
  for (const v of ['saved', 'fig']) { await tab('dig_tabs', v); }
  ok(await pg.locator('#dig_canvas').isVisible(), 'figure tab shows the canvas again');
  ok(errs.length === 0, 'no page errors: ' + JSON.stringify(errs.slice(0, 3)));
  await b.close(); console.log(fails ? fails + ' FAILED' : 'ALL OK'); process.exit(fails ? 1 : 0);
})();
