/* AI reading of a Kaplan-Meier figure in the digitizer.
   The image is sent only when the person presses "Read this figure with AI". The AI's tick positions are snapped onto
   the tick marks of the image here, and the curve colour is taken from the image itself, so the calibration and the
   tracing keep the precision of the existing digitizer. */
(function () {
  'use strict';

  function hexToRgb(hex) {
    var h = String(hex || '').replace('#', '');
    if (h.length === 3) h = h.split('').map(function (c) { return c + c; }).join('');
    var n = parseInt(h, 16);
    return isFinite(n) && h.length === 6 ? [(n >> 16) & 255, (n >> 8) & 255, n & 255] : null;
  }

  // a pixel of an axis line or tick mark: dark enough and without colour (a coloured curve is not an axis).
  // limit: luminance below which a pixel counts; thin anti-aliased ticks come out grey (about 120), light gridlines about 220
  var AXIS_LUM = 140, TICK_LUM = 175;
  function darkAt(img, x, y, limit) {
    if (x < 0 || y < 0 || x >= img.width || y >= img.height) return false;
    var i = (y * img.width + x) * 4, r = img.data[i], g = img.data[i + 1], b = img.data[i + 2];
    var mx = Math.max(r, g, b), mn = Math.min(r, g, b);
    return (0.299 * r + 0.587 * g + 0.114 * b) < (limit || AXIS_LUM) && (mx - mn) < 40;
  }

  // Snap a proposed tick position (px, py) onto the tick mark of the x or y axis.
  // Returns {px, py, snapped: true, dist} at the tick's centre on the axis line, or the proposal with snapped: false.
  function snapTick(img, axis, px, py, opts) {
    opts = opts || {};
    var W = img.width, H = img.height, horiz = axis === 'x';
    var along = horiz ? W : H, across = horiz ? H : W;
    // a: position along the axis, c: across it
    var at = function (a, c) { return horiz ? darkAt(img, a, c, AXIS_LUM) : darkAt(img, c, a, AXIS_LUM); };
    var atT = function (a, c) { return horiz ? darkAt(img, a, c, TICK_LUM) : darkAt(img, c, a, TICK_LUM); };
    var p = horiz ? px : py, q = horiz ? py : px;
    // 1. the axis line: the line across-position near q with the longest dark run along the axis
    var c0 = Math.max(0, Math.round(q - 0.06 * across)), c1 = Math.min(across - 1, Math.round(q + 0.06 * across));
    var runs = [], best = 0;
    for (var c = c0; c <= c1; c++) {
      var run = 0, longest = 0;
      for (var a = 0; a < along; a++) { if (at(a, c)) { run++; if (run > longest) longest = run; } else run = 0; }
      runs.push(longest); if (longest > best) best = longest;
    }
    if (best < 0.3 * along) return { px: px, py: py, snapped: false };
    var band = [];
    runs.forEach(function (r, k) { if (r >= 0.9 * best) band.push(c0 + k); });
    var ca = band[0], cb = band[band.length - 1];
    // keep the contiguous part of the band closest to q
    for (var k = 1; k < band.length; k++) if (band[k] !== band[k - 1] + 1) { if (Math.abs(band[k] - q) < Math.abs(ca - q)) ca = band[k]; cb = band[k]; }
    while (cb + 1 <= c1 && runs[cb + 1 - c0] >= 0.9 * best) cb++;
    var axisPos = (ca + cb) / 2;
    // 2. tick marks: short dark segments perpendicular to the axis, touching it on either side
    var maxLen = Math.max(4, Math.round((opts.maxTick || 0.04) * across));
    var a0 = Math.max(0, Math.round(p - 0.025 * along)), a1 = Math.min(along - 1, Math.round(p + 0.025 * along));
    var isTick = function (a) {
      var lo = 0; while (lo <= maxLen && atT(a, ca - 1 - lo)) lo++;
      var hi = 0; while (hi <= maxLen && atT(a, cb + 1 + hi)) hi++;
      return (lo >= 3 && lo <= maxLen) || (hi >= 3 && hi <= maxLen);
    };
    var clusters = [], cur = null;
    for (var a2 = a0; a2 <= a1; a2++) {
      if (isTick(a2)) { if (cur && cur.b === a2 - 1) cur.b = a2; else { cur = { a: a2, b: a2 }; clusters.push(cur); } }
    }
    if (!clusters.length) return { px: px, py: py, snapped: false };
    var pick = null, dist = Infinity;
    clusters.forEach(function (cl) { var m = (cl.a + cl.b) / 2, d = Math.abs(m - p); if (d < dist) { dist = d; pick = m; } });
    return horiz ? { px: pick, py: axisPos, snapped: true, dist: dist } : { px: axisPos, py: pick, snapped: true, dist: dist };
  }

  // The colour actually drawn in the plot area that is closest to a legend colour: the most frequent colour among
  // the pixels near it (so tracing starts from the curve's true colour, not the AI's estimate).
  function nearestColour(img, hex, box) {
    var t = hexToRgb(hex); if (!t) return null;
    box = box || { x0: 0, y0: 0, x1: img.width - 1, y1: img.height - 1 };
    var bins = {}, bestKey = null, bestN = 0;
    for (var y = Math.max(0, Math.round(box.y0)); y <= Math.min(img.height - 1, Math.round(box.y1)); y++) {
      for (var x = Math.max(0, Math.round(box.x0)); x <= Math.min(img.width - 1, Math.round(box.x1)); x++) {
        var i = (y * img.width + x) * 4, r = img.data[i], g = img.data[i + 1], b = img.data[i + 2];
        if (Math.min(r, g, b) > 230) continue;   // background
        if (Math.hypot(r - t[0], g - t[1], b - t[2]) > 110) continue;
        var key = (r >> 3) + ',' + (g >> 3) + ',' + (b >> 3), e = bins[key] || (bins[key] = { n: 0, r: 0, g: 0, b: 0 });
        e.n++; e.r += r; e.g += g; e.b += b;
        if (e.n > bestN) { bestN = e.n; bestKey = key; }
      }
    }
    if (!bestKey) return t;
    var e2 = bins[bestKey];
    return [e2.r / e2.n, e2.g / e2.n, e2.b / e2.n];
  }

  // ---------------- browser: send the image, apply a reading ----------------
  function browser() {
    var $ = function (id) { return document.getElementById(id); };
    var ready = false;
    var setVal = function (id, v) { var e = $(id); if (e) { e.value = v; e.dispatchEvent(new Event('change', { bubbles: true })); } };
    var dig = function () { return window.KMDigitizer; }, ui = function () { return window.KMDigitizerAPI; };
    var MAX_SIDE = 1568;

    function refreshButton() { var b = $('aifig_read_btn'); if (b) b.disabled = !ready; }
    function sendImage() {
      var S = dig();
      if (!S || !S.img) { ui().status('Load the figure first, then press Read this figure with AI.', 'err'); return; }
      var k = Math.min(1, MAX_SIDE / Math.max(S.img.width, S.img.height));
      var c = document.createElement('canvas'); c.width = Math.round(S.img.width * k); c.height = Math.round(S.img.height * k);
      c.getContext('2d').drawImage(S.img, 0, 0, c.width, c.height);
      var b64 = c.toDataURL('image/png').split(',')[1];
      var study = $('dig_study') ? $('dig_study').value.trim() : '';
      Shiny.setInputValue('aifig_image', { b64: b64, width: c.width, height: c.height, orig_width: S.img.width, orig_height: S.img.height,
                                           study: study, nonce: Math.random() }, { priority: 'event' });
      ui().status('Reading the figure with AI\u2026');
    }

    function applyAxes(m) {
      var S = dig(); if (!S || !S.img) return;
      setVal('dig_xscale', m.xscale); setVal('dig_yscale', m.yscale);   // changing a scale resets the calibration, so do it first
      var cal = { x1: null, x2: null, y1: null, y2: null }, unsure = [];
      [['x', m.x], ['y', m.y]].forEach(function (pair) {
        var axis = pair[0], ticks = pair[1] || [];
        ticks.slice(0, 2).forEach(function (t, j) {
          var key = axis + (j + 1), s = snapTick(S.imgData, axis, t.px, t.py);
          cal[key] = { px: s.px, py: s.py, val: t.value };
          if (!s.snapped) unsure.push(key);
          var inp = $('dig_' + key + 'v'); if (inp) inp.value = t.value;
        });
      });
      S.cal = cal; S.calUnsure = unsure; ui().draw();
      ui().status(unsure.length ? 'Axes set from the AI reading. Check the orange calibration points: they could not be matched to a tick mark (re-set them with the Set buttons if needed).'
                                : 'Axes set from the AI reading and matched to the tick marks. Check the crosses, then trace.', unsure.length ? '' : 'ok');
    }

    function applyArm(m) {
      var S = dig(); if (!S || !S.img) return;
      $('dig_arm').value = m.label || '';
      [['dig_tt', m.times], ['dig_nr', m.nrisk], ['dig_hr', m.hr], ['dig_hrlo', m.hrlo], ['dig_hrhi', m.hrhi]].forEach(function (p) { var e = $(p[0]); if (e) e.value = p[1] || ''; });
      var note = m.note ? ' ' + m.note : '';
      if (!m.hex) { ui().status('Details filled for ' + m.label + '. The AI gave no colour: pick it on the figure, then trace.' + note); return; }
      var box = null, c = S.cal;
      if (ui().calibrated()) {   // the plot area (curves often run a little past the last x tick)
        var xa = Math.min(c.x1.px, c.x2.px), xb = Math.max(c.x1.px, c.x2.px);
        box = { x0: xa, x1: xb + 0.2 * (xb - xa), y0: Math.min(c.y1.py, c.y2.py), y1: Math.max(c.y1.py, c.y2.py) };
      }
      S.color = nearestColour(S.imgData, m.hex, box || undefined);
      var sw = $('dig_swatch'); if (sw) sw.style.background = 'rgb(' + S.color.map(Math.round).join(',') + ')';
      if (!ui().calibrated()) { ui().status('Details and colour set for ' + m.label + '. Calibrate the axes, then press Trace by colour.' + note); return; }
      ui().trace();
      var st = $('dig_status'); if (st && note) st.textContent += note;
    }

    document.addEventListener('click', function (e) { if (e.target && e.target.id === 'aifig_read_btn') sendImage(); });
    document.addEventListener('dig:image', function () { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('aifig_new_image', Math.random()); });
    var handlers = function () {
      Shiny.addCustomMessageHandler('aifig_ready', function (m) { ready = !!m.ready; refreshButton(); });
      Shiny.addCustomMessageHandler('aifig_axes', applyAxes);
      Shiny.addCustomMessageHandler('aifig_arm', applyArm);
    };
    if (window.Shiny && Shiny.addCustomMessageHandler) handlers(); else document.addEventListener('shiny:connected', handlers);
    document.addEventListener('shiny:value', function (e) { if (e.name === 'aifig_panel') setTimeout(refreshButton, 0); });
  }

  var api = { hexToRgb: hexToRgb, snapTick: snapTick, nearestColour: nearestColour };
  if (typeof module !== 'undefined' && module.exports) module.exports = api;
  if (typeof window !== 'undefined') { window.KMDigitizerAI = api; browser(); }
})();
