// Browser test: hover hints on the Survival curves options and the "How to read this" panels of its result tabs.
const { chromium } = require('/opt/npm-tools/node_modules/playwright');
const URL = process.argv[2] || 'http://127.0.0.1:4747', SHOTS = process.argv[3] || '';
let fails = 0; const ok = (c, m) => { console.log((c ? 'OK   ' : 'FAIL ') + m); if (!c) fails++; };
(async () => {
  const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const pg = await b.newPage({ viewport: { width: 1440, height: 1000 } }); pg.setDefaultTimeout(8000);
  const errs = []; pg.on('pageerror', e => errs.push(e.message));
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  await pg.goto(URL); await pg.waitForTimeout(2500);
  await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(800);
  await pg.click('.subnav-btn[data-mode="km"]'); await pg.waitForTimeout(1200);
  const card = async () => ((await pg.textContent('.hint-card.on').catch(() => '')) || '').replace(/\s+/g, ' ');
  const settle = async sel => { await pg.locator(sel).first().scrollIntoViewIfNeeded(); await pg.mouse.move(5, 5); await pg.waitForTimeout(500); };
  const hoverLabel = async (id) => { const sel = `label[for="${id}"]`; await settle(sel); await pg.hover(sel); await pg.waitForTimeout(1100); };
  // example data first, so the result tabs exist
  await pg.click('#km_load_example'); await pg.waitForTimeout(9000);
  // label hint
  await hoverLabel('km_landmarks'); let c = await card(); ok(/Survival at fixed times/.test(c) && /What it does/.test(c) && /Best practice/.test(c), 'label hint: ' + c.slice(0, 80));
  await pg.mouse.move(5, 5); await pg.waitForTimeout(300);
  ok((await pg.locator('.hint-card.on').count()) === 0, 'hint closes when the mouse leaves');
  // no hint before the delay
  await pg.hover('label[for="km_tau"]'); await pg.waitForTimeout(300);
  ok((await pg.locator('.hint-card.on').count()) === 0, 'no hint after 0.3 s');
  await pg.waitForTimeout(700); ok(/RMST horizon/.test(await card()), 'hint after 0.7 s: RMST horizon');
  // check box
  await pg.mouse.move(5, 5); await pg.waitForTimeout(300);
  await pg.waitForTimeout(400);
  await settle('#km_conf'); await pg.hover('#km_conf'); await pg.waitForTimeout(1100); ok(/confidence bands/.test(await card()), 'check box hint');
  // radio
  await pg.mouse.move(5, 5); await pg.waitForTimeout(300);
  await pg.click('.results a[data-value="hr"]'); await pg.waitForTimeout(1500);
  await settle('.radio:has(input[value="published_first"])'); await pg.hover('.radio:has(input[value="published_first"])'); await pg.waitForTimeout(1100);
  ok(/Published first/.test(await card()), 'radio option hint');
  // selectize option
  await pg.mouse.move(5, 5); await pg.waitForTimeout(300);
  await pg.waitForTimeout(400);
  await pg.click('#km_tv_df + .selectize-control .selectize-input'); await pg.waitForTimeout(400);
  await pg.hover('.selectize-dropdown .option[data-value="4"]'); await pg.waitForTimeout(1100);
  ok(/Flexible \(4\)/.test(await card()), 'list option hint');
  await pg.keyboard.press('Escape'); await pg.mouse.move(5, 5); await pg.waitForTimeout(400);
  // panels
  const tab = async v => { await pg.click(`#km_tabs a[data-value="${v}"]`); await pg.waitForTimeout(2500); };
  for (const [v, key, re] of [['pooled', 'km_pooled', /Drug|Treatment/], ['bystudy', 'km_study', /studies have a hazard ratio/], ['summary', 'km_summary', /events in/],
                              ['hr', 'km_hr', /pooled hazard ratio/], ['nph', 'km_nph', /Schoenfeld/], ['sens', 'km_sens', /Across \d+ analyses/], ['check', 'km_check', /Reconstructed events/]]) {
    await tab(v);
    const t = ((await pg.textContent(`details.howto[data-key="${key}"]`).catch(() => '')) || '').replace(/\s+/g, ' ');
    ok(/Your result/.test(t) && /What you are looking at/.test(t) && re.test(t), `${v}: panel with reading and guide`);
    if (SHOTS) await pg.screenshot({ path: `${SHOTS}/km_howto_${v}.png`, fullPage: true });
  }
  // digitizer
  await pg.click('.subnav-btn[data-mode="dig"]').catch(() => {}); await pg.waitForTimeout(1200);
  const dl = await pg.locator('label[for="dig_nr"]').count();
  if (dl) { await hoverLabel('dig_nr'); ok(/Numbers at risk/.test(await card()), 'digitizer label hint'); }
  ok(errs.length === 0, 'no page errors ' + errs.join('|'));
  await b.close(); console.log(fails ? 'FAILED' : 'KM HOWTO BROWSER ALL OK'); process.exit(fails ? 1 : 0);
})();
