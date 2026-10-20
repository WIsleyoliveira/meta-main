const { chromium } = require('/opt/npm-tools/node_modules/playwright');
(async () => {
  const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' });
  const pg = await b.newPage({ viewport: { width: 1600, height: 1500 } });
  const errs = []; pg.on('pageerror', e => errs.push(e.message)); pg.on('console', m => { if (m.type()==='error') errs.push(m.text()); });
  await pg.addInitScript(() => { setInterval(() => { const r = document.querySelector('input[name=mode]:checked'); const v = r && r.value; if (v !== window.__lm) { window.__lm = v; setTimeout(() => document.querySelectorAll('.step.collapsed .step-head').forEach(h => h.click()), 250); } }, 150); });
  await pg.goto('http://127.0.0.1:4747'); await pg.waitForTimeout(2500);
  await pg.click('.mode-bar span:has-text("Survival curves")'); await pg.waitForTimeout(800);
  await pg.setInputFiles('#dig_file', 'tests/out/synth_km.png'); await pg.waitForTimeout(1500);
  const clickPx = async (px, py) => {
    const r = await pg.evaluate(([px,py]) => { const c=document.getElementById('dig_canvas'), z=window.KMDigitizer.zoom; const b=c.getBoundingClientRect(); c.scrollIntoView({block:'center'}); const b2=c.getBoundingClientRect(); return {x:b2.left+px*z, y:b2.top+py*z}; }, [px,py]);
    await pg.mouse.click(r.x, r.y);
  };
  await pg.fill('#dig_x1v','0'); await pg.fill('#dig_x2v','60'); await pg.fill('#dig_y1v','0'); await pg.fill('#dig_y2v','1');
  for (const [m,px,py] of [['cal-x1',100,600],['cal-x2',960,600],['cal-y1',100,600],['cal-y2',100,40]]) {
    await pg.click(`[data-dig-mode="${m}"]`); await clickPx(px,py);
  }
  console.log('cal', await pg.textContent('#dig_calstate'));
  await pg.fill('#dig_study','Trial S'); 
  const arms = [['Intervention','#2F5D8A',0.025],['Control','#B23A48',0.045]];
  for (const [arm,col,rate] of arms) {
    await pg.fill('#dig_arm',arm);
    await pg.click('[data-dig-mode="pick"]');
    // pick colour at a curve point: t=6
    const px = 100 + 6/60*860, py = 600 - Math.exp(-rate*6)*560;
    await clickPx(px, py);
    console.log('colour', await pg.evaluate(()=>JSON.stringify(window.KMDigitizer.color))); await pg.click('[data-dig-act="trace"]'); await pg.waitForTimeout(300);
    console.log(arm, await pg.textContent('#dig_status'));
    const err = await pg.evaluate((rate) => { const S=window.KMDigitizer,c=S.cal; let mx=0; S.pts.forEach(p=>{ const x=c.x1.val+(p.px-c.x1.px)*(c.x2.val-c.x1.val)/(c.x2.px-c.x1.px), y=c.y1.val+(p.py-c.y1.py)*(c.y2.val-c.y1.val)/(c.y2.py-c.y1.py); const e=Math.abs(y-Math.exp(-rate*x)); if(e>mx){mx=e; window.__w=[x,y,p.px,p.py];} }); return mx+' worst '+JSON.stringify(window.__w); }, rate);
    console.log(arm, 'max abs error', err);
    await pg.click('[data-dig-act="save"]'); await pg.waitForTimeout(800);
  }
  await pg.screenshot({ path: 'tests/out/dig_1.png' });
  // arm form
  for (let i=1;i<=2;i++) {
    await pg.evaluate(i=>$('#dig_pair')[0].selectize.setValue(String(i)), i); await pg.waitForTimeout(300);
    await pg.evaluate(v=>$('#dig_group')[0].selectize.setValue(v), i===1?'treatment':'control');
    await pg.fill('#dig_ntotal','200'); await pg.fill('#dig_events', i===1?'90':'130');
    await pg.fill('#dig_times','0, 12, 24, 36, 48, 60');
    await pg.fill('#dig_nrisk', i===1?'200, 148, 109, 81, 60, 44':'200, 117, 68, 40, 23, 13');
    await pg.click('#dig_add_arm'); await pg.waitForTimeout(800);
  }
  await pg.screenshot({ path: 'tests/out/dig_2.png', fullPage: true });
  await pg.click('#dig_tabs a[data-value="saved"]'); await pg.waitForTimeout(2200);
  await pg.click('#dig_use'); await pg.waitForTimeout(6000);
  await pg.screenshot({ path: 'tests/out/dig_3.png', fullPage: true });
  console.log('mode km visible:', await pg.isVisible('#km_tabs'));
  console.log('errors', JSON.stringify(errs));
  await b.close();
})();
