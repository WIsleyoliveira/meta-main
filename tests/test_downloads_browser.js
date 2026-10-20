// Browser test: every figure of the forest-plot area has its download buttons, they deliver real files, and "How to read this" sits between the figure and the buttons.
const { chromium } = require('/opt/npm-tools/node_modules/playwright');
const URL = process.argv[2] || 'http://127.0.0.1:4747';
let fails = 0; const ok = (c, m) => { console.log((c ? 'OK   ' : 'FAIL ') + m); if (!c) fails++; };
(async () => {
  const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const ctx = await b.newContext({ acceptDownloads: true, viewport: { width: 1440, height: 1000 } });
  const pg = await ctx.newPage(); pg.setDefaultTimeout(10000);
  const errs = []; pg.on('pageerror', e => errs.push(e.message));
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  await pg.goto(URL); await pg.waitForTimeout(2500); await pg.click('.mode-bar span:has-text("Forest plot")'); await pg.waitForTimeout(600);
  await pg.click('#load_example2'); await pg.waitForTimeout(5000);
  const dl = async (id, minBytes = 2000) => {
    const [d] = await Promise.all([pg.waitForEvent('download', { timeout: 30000 }), pg.evaluate(i => document.getElementById(i).click(), id)]);
    const p = await d.path(); const n = require('fs').statSync(p).size; ok(n > minBytes, `${id} -> ${d.suggestedFilename()} (${n} bytes)`);
  };
  const order = async (tabVal, figSel, key, dlId) => {
    await pg.click(`#tabs a[data-value="${tabVal}"]`); await pg.waitForTimeout(3000);
    const pos = await pg.evaluate(([f, k, d]) => { const y = e => e ? e.getBoundingClientRect().top : -1; return [y(document.querySelector(f)), y(document.querySelector(`details.howto[data-key="${k}"]`)), y(document.getElementById(d))]; }, [figSel, key, dlId]);
    ok(pos[0] >= 0 && pos[0] < pos[1] && pos[1] < pos[2], `${tabVal}: figure < how to read < downloads ${pos.map(Math.round)}`);
  };
  await order('forest', '#forest_plot', 'forest', 'dl_png'); await dl('dl_png'); await dl('dl_pdf');
  await order('loo', '#loo_plot', 'loo', 'dl_loo_png'); await dl('dl_loo_png'); await dl('dl_loo_pdf'); await dl('dl_loo_svg');
  await order('funnel', '#funnel_plot', 'funnel', 'dl_funnel_png'); await dl('dl_funnel_png'); await dl('dl_funnel_pdf'); await dl('dl_funnel_svg');
  await order('influence', '#radial_plot', 'influence', 'dl_baujat');
  for (const i of ['dl_baujat', 'dl_baujat_pdf', 'dl_radial', 'dl_radial_pdf', 'dl_infl_png', 'dl_infl_pdf']) await dl(i);
  await order('metareg', '#mr_plot', 'metareg', 'dl_mr_png'); await dl('dl_mr_png'); await dl('dl_mr_pdf');
  ok(errs.length === 0, 'no page errors ' + errs.join('|'));
  await b.close(); console.log(fails ? 'FAILED' : 'DOWNLOADS ALL OK'); process.exit(fails ? 1 : 0);
})();
