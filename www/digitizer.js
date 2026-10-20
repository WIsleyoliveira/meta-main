/* In-browser digitizer for Kaplan-Meier figures.
   The image never leaves the browser: it is read locally and drawn on a canvas.
   Only the finished points (time, survival) are sent to the app. */
(function () {
  'use strict';
  var S = { img: null, imgData: null, zoom: 1, mode: 'none', cal: { x1: null, x2: null, y1: null, y2: null },
            pts: [], color: null, box: null, boxFirst: null, name: '', overlay: [] };
  var canvas, ctx, wrap, loupe, lctx, off, octx;
  var $ = function (id) { return document.getElementById(id); };

  function status(msg, kind) {
    var el = $('dig_status'); if (!el) return;
    el.textContent = msg; el.className = 'dig-status' + (kind ? ' ' + kind : '');
  }
  function calibrated() { var c = S.cal; return !!(c.x1 && c.x2 && c.y1 && c.y2); }
  function num(id) { var v = parseFloat(String($(id).value).replace(',', '.')); return isFinite(v) ? v : NaN; }

  // axes may be linear or logarithmic (chosen in step 2); interpolation happens on the log10 scale for a log axis
  function isLog(axis) { var el = $('dig_' + axis + 'scale'); return !!el && el.value === 'log'; }
  function fwd(v, lg) { return lg ? Math.log(v) / Math.LN10 : v; }
  function inv(v, lg) { return lg ? Math.pow(10, v) : v; }
  function toData(px, py) {
    var c = S.cal, lx = isLog('x'), ly = isLog('y');
    return {
      x: inv(fwd(c.x1.val, lx) + (px - c.x1.px) * (fwd(c.x2.val, lx) - fwd(c.x1.val, lx)) / (c.x2.px - c.x1.px), lx),
      y: inv(fwd(c.y1.val, ly) + (py - c.y1.py) * (fwd(c.y2.val, ly) - fwd(c.y1.val, ly)) / (c.y2.py - c.y1.py), ly)
    };
  }
  function fromData(x, y) {
    var c = S.cal, lx = isLog('x'), ly = isLog('y');
    return {
      px: c.x1.px + (fwd(x, lx) - fwd(c.x1.val, lx)) * (c.x2.px - c.x1.px) / (fwd(c.x2.val, lx) - fwd(c.x1.val, lx)),
      py: c.y1.py + (fwd(y, ly) - fwd(c.y1.val, ly)) * (c.y2.py - c.y1.py) / (fwd(c.y2.val, ly) - fwd(c.y1.val, ly))
    };
  }

  // ---- drawing -----------------------------------------------------------------
  function draw() {
    if (!S.img) { return; }
    var z = S.zoom, w = Math.round(S.img.width * z), h = Math.round(S.img.height * z);
    canvas.width = w; canvas.height = h; canvas.style.width = w + 'px'; canvas.style.height = h + 'px';
    ctx.imageSmoothingEnabled = z < 1;
    ctx.drawImage(S.img, 0, 0, w, h);
    ctx.lineWidth = 1.5; ctx.font = '12px sans-serif';
    var c = S.cal;
    function cross(px, py, label, col) {
      ctx.strokeStyle = col; ctx.fillStyle = col;
      ctx.beginPath(); ctx.moveTo(px * z - 9, py * z); ctx.lineTo(px * z + 9, py * z);
      ctx.moveTo(px * z, py * z - 9); ctx.lineTo(px * z, py * z + 9); ctx.stroke();
      ctx.fillText(label, px * z + 6, py * z - 6);
    }
    if (c.x1) cross(c.x1.px, c.x1.py, 'X1=' + c.x1.val, '#0a7d3b');
    if (c.x2) cross(c.x2.px, c.x2.py, 'X2=' + c.x2.val, '#0a7d3b');
    if (c.y1) cross(c.y1.px, c.y1.py, 'Y1=' + c.y1.val, '#7a3db8');
    if (c.y2) cross(c.y2.px, c.y2.py, 'Y2=' + c.y2.val, '#7a3db8');
    if (S.box) {
      ctx.setLineDash([6, 4]); ctx.strokeStyle = '#d98200';
      ctx.strokeRect(S.box.x0 * z, S.box.y0 * z, (S.box.x1 - S.box.x0) * z, (S.box.y1 - S.box.y0) * z);
      ctx.setLineDash([]);
    }
    if (S.overlay && S.overlay.length && calibrated()) {
      var cols = ['#0a7d3b', '#d98200', '#7a3db8', '#00839a'];
      S.overlay.forEach(function (o, k) {
        ctx.strokeStyle = cols[k % cols.length]; ctx.lineWidth = 2; ctx.setLineDash([7, 3]); ctx.beginPath();
        var started = false, prev = null;
        for (var i = 0; i < o.time.length; i++) {
          var q = fromData(o.time[i], o.surv[i]); if (!isFinite(q.px) || !isFinite(q.py)) continue;
          if (!started) { ctx.moveTo(q.px * z, q.py * z); started = true; }
          else { ctx.lineTo(q.px * z, prev.py * z); ctx.lineTo(q.px * z, q.py * z); }   // step function
          prev = q;
        }
        ctx.stroke(); ctx.setLineDash([]);
        ctx.fillStyle = cols[k % cols.length]; ctx.fillText('rebuilt: ' + o.arm, 12, 16 + 15 * k);
      });
    }
    ctx.fillStyle = '#B23A48'; ctx.strokeStyle = '#ffffff'; ctx.lineWidth = 1;
    S.pts.forEach(function (p) {
      ctx.fillStyle = p.filled ? '#e08a00' : '#B23A48';
      ctx.beginPath(); ctx.arc(p.px * z, p.py * z, 3.2, 0, 2 * Math.PI); ctx.fill(); ctx.stroke();
    });
    var n = $('dig_npts'); if (n) n.textContent = S.pts.length;
    var cs = $('dig_calstate');
    if (cs) cs.textContent = ['x1', 'x2', 'y1', 'y2'].map(function (k) { return (c[k] ? '✓ ' : '• ') + k.toUpperCase(); }).join('   ');
  }

  function fit() {
    if (!S.img || !wrap) return;
    var avail = wrap.clientWidth - 4;
    S.zoom = avail > 50 ? Math.min(1.5, avail / S.img.width) : 1;
    draw();
  }

  function loadBlob(blob, name) {
    var url = URL.createObjectURL(blob);
    var img = new Image();
    img.onload = function () {
      S.img = img; S.name = name || '';
      off.width = img.width; off.height = img.height; octx.drawImage(img, 0, 0);
      S.imgData = octx.getImageData(0, 0, img.width, img.height);
      S.cal = { x1: null, x2: null, y1: null, y2: null }; S.pts = []; S.masks = []; S.box = null; S.boxFirst = null; S.color = null;
      var sw = $('dig_swatch'); if (sw) sw.style.background = 'transparent';
      $('dig_empty').style.display = 'none';
      fit();
      status('Image loaded. Next: calibrate the axes (step 2).', 'ok');
    };
    img.onerror = function () { status('That file could not be read as an image. Use PNG, JPG or a screenshot.', 'err'); };
    img.src = url;
  }

  // ---- interaction ----------------------------------------------------------------
  function imgPos(e) {
    var r = canvas.getBoundingClientRect();
    return { px: (e.clientX - r.left) * (canvas.width / r.width) / S.zoom, py: (e.clientY - r.top) * (canvas.height / r.height) / S.zoom };
  }
  function pixelColor(px, py) {
    var d = S.imgData, r = 0, g = 0, b = 0, n = 0;
    for (var dy = -1; dy <= 1; dy++) for (var dx = -1; dx <= 1; dx++) {
      var x = Math.round(px) + dx, y = Math.round(py) + dy;
      if (x < 0 || y < 0 || x >= d.width || y >= d.height) continue;
      var i = (y * d.width + x) * 4; r += d.data[i]; g += d.data[i + 1]; b += d.data[i + 2]; n++;
    }
    return n ? [r / n, g / n, b / n] : [0, 0, 0];
  }

  function onClick(e) {
    if (!S.img) return;
    var p = imgPos(e), m = S.mode;
    if (m.indexOf('cal-') === 0) {
      var k = m.substr(4), isX = k.charAt(0) === 'x';
      var v = num('dig_' + k + 'v');
      if (isNaN(v)) { status('Type the value of this axis point first (box next to the button), then click the figure.', 'err'); return; }
      if (isLog(k.charAt(0)) && v <= 0) { status('A logarithmic axis needs a value above zero.', 'err'); return; }
      S.cal[k] = { px: p.px, py: p.py, val: v };
      setMode('none'); draw();
      status(calibrated() ? 'Axes calibrated. Next: trace the curve (step 3).' : 'Point ' + k.toUpperCase() + ' set.', 'ok');
    } else if (m === 'add') {
      if (!calibrated()) { status('Calibrate the four axis points first (step 2).', 'err'); return; }
      S.pts.push({ px: p.px, py: p.py }); draw();
    } else if (m === 'erase') {
      var best = -1, bd = 12 / S.zoom;
      S.pts.forEach(function (q, i) { var d = Math.hypot(q.px - p.px, q.py - p.py); if (d < bd) { bd = d; best = i; } });
      if (best >= 0) { S.pts.splice(best, 1); draw(); }
    } else if (m === 'pick') {
      S.color = bestColor(p.px, p.py); S.startY = p.py; S.startX = p.px;
      var sw = $('dig_swatch'); sw.style.background = 'rgb(' + S.color.map(Math.round).join(',') + ')';
      setMode('none'); status('Colour picked. Press "Trace by colour".', 'ok');
    } else if (m === 'box') {
      if (!S.boxFirst) { S.boxFirst = p; status('Now click the opposite corner of the plot area.'); }
      else {
        S.box = { x0: Math.min(S.boxFirst.px, p.px), y0: Math.min(S.boxFirst.py, p.py), x1: Math.max(S.boxFirst.px, p.px), y1: Math.max(S.boxFirst.py, p.py) };
        S.boxFirst = null; setMode('none'); draw(); status('Tracing will stay inside the dashed box.', 'ok');
      }
    }
  }

  function onMove(e) {
    if (!S.img) return;
    var p = imgPos(e);
    var sx = Math.max(0, Math.min(S.img.width - 1, p.px)), sy = Math.max(0, Math.min(S.img.height - 1, p.py));
    lctx.imageSmoothingEnabled = false;
    lctx.fillStyle = '#fff'; lctx.fillRect(0, 0, loupe.width, loupe.height);
    lctx.drawImage(off, sx - 10, sy - 10, 20, 20, 0, 0, loupe.width, loupe.height);
    lctx.strokeStyle = '#B23A48'; lctx.lineWidth = 1;
    lctx.beginPath(); lctx.moveTo(loupe.width / 2, 0); lctx.lineTo(loupe.width / 2, loupe.height);
    lctx.moveTo(0, loupe.height / 2); lctx.lineTo(loupe.width, loupe.height / 2); lctx.stroke();
    var ro = $('dig_readout'), d = calibrated() ? toData(p.px, p.py) : null;
    var label = d ? 'X = ' + d.x.toFixed(2) + '   Y = ' + d.y.toFixed(3)
                  : 'pixel ' + Math.round(p.px) + ', ' + Math.round(p.py) + '  (calibrate to see X and Y)';
    if (ro) ro.textContent = label;
    var tip = $('dig_tip');
    if (!tip) { tip = document.createElement('div'); tip.id = 'dig_tip'; tip.className = 'dig-tip'; document.body.appendChild(tip); }
    tip.textContent = label; tip.style.display = 'block';
    var left = e.clientX + 18, top = e.clientY + 18;
    if (left + 220 > window.innerWidth) left = e.clientX - 230;
    tip.style.left = left + 'px'; tip.style.top = top + 'px';
  }
  function onLeave() { var t = $('dig_tip'); if (t) t.style.display = 'none'; }

  function setMode(m) {
    S.mode = m; if (m !== 'box') S.boxFirst = null;
    document.querySelectorAll('[data-dig-mode]').forEach(function (b) {
      b.classList.toggle('active', b.getAttribute('data-dig-mode') === m);
    });
    if (canvas) canvas.style.cursor = m === 'none' ? 'default' : 'crosshair';
    var hints = { 'add': 'Click along the curve. Use Undo or Erase to fix mistakes.', 'erase': 'Click a point to remove it.',
                  'pick': 'Click on the curve line to pick its colour.', 'box': 'Click one corner of the plot area, then the opposite corner.' };
    if (hints[m]) status(hints[m]);
    else if (m.indexOf('cal-') === 0) status('Click the tick for ' + m.substr(4).toUpperCase() + ' on the figure.');
  }

  // the colour of the line itself: the most saturated (or, for grey lines, the darkest) pixel near the click,
  // because the average of a thin line and its white background is a washed-out colour
  function bestColor(px, py) {
    var d = S.imgData, W = d.width, H = d.height, best = null, bs = -1, grey = null, gd = 1e9;
    for (var dy = -4; dy <= 4; dy++) for (var dx = -4; dx <= 4; dx++) {
      var x = Math.round(px) + dx, y = Math.round(py) + dy; if (x < 0 || y < 0 || x >= W || y >= H) continue;
      var i = (y * W + x) * 4, r = d.data[i], g = d.data[i + 1], b = d.data[i + 2];
      var sat = Math.max(r, g, b) - Math.min(r, g, b), lum = r + g + b;
      if (sat > bs) { bs = sat; best = [r, g, b]; }
      if (lum < gd) { gd = lum; grey = [r, g, b]; }
    }
    return bs >= 40 ? best : grey;
  }

  // where an already saved curve lies in column x: the vertical span between the saved points on each side of x
  // (a steep drop covers a long span), and its centre line. Saved points are sorted by x.
  function maskInfo(m, x) {
    var lo = 0, hi = m.length - 1, r = -1;
    while (lo <= hi) { var mid = (lo + hi) >> 1; if (m[mid].px <= x) { r = mid; lo = mid + 1; } else hi = mid - 1; }
    if (r < 0) return m[0].px - x > 6 ? null : { lo: m[0].py, hi: m[0].py, mid: m[0].py };
    if (r === m.length - 1) return x - m[r].px > 6 ? null : { lo: m[r].py, hi: m[r].py, mid: m[r].py };
    var a = m[r], b = m[r + 1];
    if (b.px - a.px > 40) return x - a.px > 6 ? null : { lo: a.py, hi: a.py, mid: a.py };
    var t = (x - a.px) / ((b.px - a.px) || 1);
    return { lo: Math.min(a.py, b.py), hi: Math.max(a.py, b.py), mid: a.py + (b.py - a.py) * t, sl: Math.abs(b.py - a.py) / ((b.px - a.px) || 1) };
  }
  function median(v) { v = v.slice().sort(function (p, q) { return p - q; }); return v.length ? v[Math.floor(v.length / 2)] : NaN; }

  // ---- automatic tracing by colour -----------------------------------------------------
  function trace() {
    if (!S.img) { status('Load an image first.', 'err'); return; }
    if (!calibrated()) { status('Calibrate the four axis points first (step 2).', 'err'); return; }
    if (!S.color) { status('Press "Pick colour", then click on the curve line.', 'err'); return; }
    var tol = num('dig_tol'); if (isNaN(tol)) tol = 60;
    var d = S.imgData, W = d.width, H = d.height, c = S.cal;
    var xa = S.box ? S.box.x0 : Math.min(c.x1.px, c.x2.px), xb = S.box ? S.box.x1 : Math.max(c.x1.px, c.x2.px);
    var ya = S.box ? S.box.y0 : Math.min(c.y1.py, c.y2.py) - 3, yb = S.box ? S.box.y1 : Math.max(c.y1.py, c.y2.py) + 3;
    // survival curves begin at the y axis, which is usually to the left of the first labelled x tick (x = 0 is rarely labelled with a tick that was calibrated)
    // (not further left than time 0, which can lie between the y axis and the first labelled tick; the tick marks on the y axis would be taken for the curve)
    if (!S.box) {
      var yax = Math.min(c.y1.px, c.y2.px), x0px = isLog('x') ? -Infinity : fromData(0, c.y1.val).px, startX = Math.max(yax, x0px - 3);
      if (startX < xa && xa - startX < 0.5 * Math.abs(c.x2.px - c.x1.px)) xa = startX;
    }
    xa = Math.max(0, Math.floor(xa)); xb = Math.min(W - 1, Math.ceil(xb)); ya = Math.max(0, Math.floor(ya)); yb = Math.min(H - 1, Math.ceil(yb));
    // curves usually run past the last labelled tick (end of follow-up): trace up to 20% further right than the last tick
    // (the frame is still detected over the calibrated width only); a drawn box is taken as it is
    var xb0 = xb, xbEnd = S.box ? xb : Math.min(W - 1, Math.ceil(xb + 0.2 * Math.abs(c.x2.px - c.x1.px)));
    var step = Math.max(1, Math.round(num('dig_step') || 3));
    while ((xbEnd - xa) / step > 500) step++;
    var tr = S.color[0], tg = S.color[1], tb = S.color[2];
    function match(x, y) { var i = (y * W + x) * 4, dr = d.data[i] - tr, dg = d.data[i + 1] - tg, db = d.data[i + 2] - tb; return Math.sqrt(dr * dr + dg * dg + db * db) <= tol; }
    // the frame of the plot (x axis, y axis, box lines) is a long straight line of the same dark colour: find those rows and
    // columns (a line that matches in at least 85% of the width or height) and leave them out
    // the frame is often thin and grey (antialiased), so it is detected with a wider colour tolerance, and the rows/columns next to it are left out too
    var nCols = 0, nRows = yb - ya + 1, rowHit = [], colHit = [], tolF = tol * 1.5 + 10;
    function matchF(x, y) { var i = (y * W + x) * 4, dr = d.data[i] - tr, dg = d.data[i + 1] - tg, db = d.data[i + 2] - tb; return Math.sqrt(dr * dr + dg * dg + db * db) <= tolF; }
    for (var y = ya; y <= yb; y++) rowHit[y] = 0;
    for (var x = xa; x <= xb0; x++) { nCols++; colHit[x] = 0; for (var y2 = ya; y2 <= yb; y2++) if (matchF(x, y2)) { colHit[x]++; rowHit[y2]++; } }
    var axisRow = [], axisCol = [];
    for (var y3 = ya; y3 <= yb; y3++) axisRow[y3] = nCols > 20 && rowHit[y3] >= 0.7 * nCols;
    for (var x3 = xa; x3 <= xb0; x3++) axisCol[x3] = nRows > 20 && colHit[x3] >= 0.7 * nRows;
    var ar2 = axisRow.slice(), ac2 = axisCol.slice();
    for (var y4 = ya; y4 <= yb; y4++) if (ar2[y4]) { axisRow[Math.max(ya, y4 - 1)] = true; axisRow[Math.min(yb, y4 + 1)] = true; }
    for (var x4 = xa; x4 <= xb0; x4++) if (ac2[x4]) { axisCol[Math.max(xa, x4 - 1)] = true; axisCol[Math.min(xb0, x4 + 1)] = true; }
    var useMask = $('dig_mask') && $('dig_mask').checked && S.masks && S.masks.length, mhw = num('dig_maskw'); if (isNaN(mhw) || mhw < 0) mhw = 4;
    // where the curve begins: its top (survival curves start at the top of the scale), its bottom (cumulative incidence), or where you clicked
    var sm = $('dig_start') ? $('dig_start').value : 'top', topRow = Math.min(c.y1.py, c.y2.py), botRow = Math.max(c.y1.py, c.y2.py);
    var startPt = sm === 'click' && S.startY !== undefined ? { px: S.startX, py: S.startY } : { px: xa, py: sm === 'bottom' ? botRow : topRow };

    // pass 1: the runs of matching pixels in every column, with the saved curves' pixels (and the plot frame) left out
    var cols = [];
    for (var x = xa; x <= xbEnd; x++) {
      if (axisCol[x]) continue;
      var info = useMask ? S.masks.map(function (m) { return maskInfo(m, x); }).filter(function (v) { return v !== null; }) : [];
      info.forEach(function (v) { v.a = Math.ceil(v.lo - mhw); v.b = Math.floor(v.hi + mhw); });   // masked rows a..b
      var runs = [], cur = null;
      for (var y = ya; y <= yb; y++) {
        if (axisRow[y]) continue;
        if (info.some(function (v) { return y >= v.a && y <= v.b; })) continue;
        if (match(x, y)) { if (cur && y === cur.b + 1) cur.b = y; else { cur = { a: y, b: y }; runs.push(cur); } }
      }
      // a run that touches the masked band is a curve that is partly hidden behind the saved one
      runs.forEach(function (r) { r.cut = info.some(function (v) { return r.a === v.b + 1; }) ? 'top' : (info.some(function (v) { return r.b === v.a - 1; }) ? 'bottom' : null); });
      cols.push({ x: x, runs: runs, mid: info.length ? info[0].mid : null, sl: info.length ? info[0].sl || 0 : 0, msteep: info.some(function (v) { return v.sl > 1.5; }) });
    }
    // typical thickness of the line, from the runs that are not cut by the saved curve (steep stretches excluded)
    var T = median([].concat.apply([], cols.map(function (cc) { return cc.runs.filter(function (r) { return !r.cut && r.b - r.a + 1 <= 12; }).map(function (r) { return r.b - r.a + 1; }); })));
    if (!isFinite(T)) T = 3;
    function centre(r) {   // a partly hidden line: its visible edge is the true edge, the centre is half a line width in
      if (r.cut === 'top' && r.b - r.a + 1 < T) return r.b - (T - 1) / 2;
      if (r.cut === 'bottom' && r.b - r.a + 1 < T) return r.a + (T - 1) / 2;
      return (r.a + r.b) / 2;
    }

    // pass 2: follow the curve from its beginning. Every `step` columns, but column by column where the curve is steep
    // (the first months of a survival curve), so that the early drop is not reduced to two or three points
    var out = [], pending = [], nFilled = 0, last = startPt, lastMid = null, started = false, steep = false;
    var colAt = {}; cols.forEach(function (cc) { colAt[cc.x] = cc; });
    function dist(r, ref) { return ref < r.a ? r.a - ref : (ref > r.b ? ref - r.b : 0); }   // a steep run that contains the expected position is at distance 0
    for (var xx = xa; xx <= xbEnd;) {
      var cc = colAt[xx];
      if (!cc) { xx += 1; continue; }
      var runs = cc.runs, ref, ys = null, accepted = null;
      // while ignoring saved curves, the expected position keeps the same distance from the saved curve as the last point had
      // (a dashed curve can hide behind it for a long stretch)
      if (useMask) ref = (cc.mid !== null && lastMid !== null) ? cc.mid + (last.py - lastMid) : last.py; else ref = last.py;
      // runs too far from the expected position (legend, text, the frame) are refused; the allowance grows while no point is found
      var allowed = started ? Math.min(80, 25 + 3 * Math.max(0, Math.round((cc.x - last.px) / step) - 1)) : 60;
      // where the saved curve is steep, a curve a little to the side of it is far away vertically: allow for that
      if (useMask) allowed += Math.min(100, 8 * cc.sl);
      runs = runs.filter(function (r) { return dist(r, ref) <= allowed; });
      // a thin sliver right next to the saved curve is its anti-aliased edge, not the hidden curve: leave it out; and where the
      // curve is visible apart from the saved one, prefer that over anything that touches the saved curve
      if (useMask) {
        runs = runs.filter(function (r) { return !r.cut || r.b - r.a + 1 >= 0.5 * T; });
        var free = runs.filter(function (r) { return !r.cut; });
        if (free.length) runs = free;
      }
      if (runs.length) {
        accepted = runs.reduce(function (u, v) { var du = dist(u, ref), dv = dist(v, ref); return (dv < du || (dv === du && Math.abs(centre(v) - ref) < Math.abs(centre(u) - ref))) ? v : u; });
        ys = Math.round(centre(accepted));
        // the very first point: the curve begins at the top (or bottom) of its first run, not in the middle of it
        if (!started && sm === 'top' && accepted.a <= ref + T) ys = Math.round(accepted.a + (T - 1) / 2);
        if (!started && sm === 'bottom' && accepted.b >= ref - T) ys = Math.round(accepted.b - (T - 1) / 2);
      }
      if (ys !== null) {
        if (out.length && pending.length && useMask && lastMid !== null && cc.mid !== null && pending.every(function (q) { return q.mid !== null; })) {
          // between two visible pieces the distance to the saved curve changes gradually: interpolate it instead of keeping the old distance
          var off0 = last.py - lastMid, off1 = ys - cc.mid, n = pending.length;
          pending.forEach(function (q, k) { q.py = Math.round(q.mid + off0 + (off1 - off0) * (k + 1) / (n + 1)); });
        }
        if (out.length) pending.forEach(function (q) { out.push({ px: q.px, py: q.py, filled: true }); });   // stretches hidden behind the saved curve or between dashes
        pending = [];
        out.push({ px: cc.x, py: ys });
        steep = (accepted.b - accepted.a + 1) > Math.max(2 * T, 6);
        last = { px: cc.x, py: ys }; lastMid = cc.mid; started = true;
      } else if (useMask && out.length) {
        pending.push({ px: cc.x, py: Math.round(ref), mid: cc.mid, filled: true });
        steep = cc.msteep;   // also column by column where the saved curve (and so the hidden one) is steep
      } else steep = false;
      xx += steep ? 1 : step;
    }
    nFilled = out.filter(function (q) { return q.filled; }).length;
    if (out.length < 5) { status('Too few pixels matched this colour. Pick the colour again on a thicker part of the line, or raise the tolerance.', 'err'); return; }
    // a survival curve never goes up (a cumulative incidence never goes down): where the trace jitters against that, hold the previous value
    if (sm === 'top' || sm === 'bottom') for (var k = 1; k < out.length; k++) { if (sm === 'top' && out[k].py < out[k - 1].py) out[k].py = out[k - 1].py; if (sm === 'bottom' && out[k].py > out[k - 1].py) out[k].py = out[k - 1].py; }
    S.pts = out; draw();
    status(out.length + ' points traced' + (nFilled ? ' (' + nFilled + ' in orange: the curve is hidden there, behind a saved curve or between dashes, so its position was estimated)' : '') +
      '. Check them on the figure; use Erase for stray points (legend, censor marks), then Save this curve.', 'ok');
  }

  // ---- saving ------------------------------------------------------------------------------
  function save() {
    if (!calibrated()) { status('Calibrate the axes first.', 'err'); return; }
    var study = $('dig_study').value.trim(), arm = $('dig_arm').value.trim();
    if (!study || !arm) { status('Type the study name and the arm name (step 1).', 'err'); return; }
    if (S.pts.length < 5) { status('At least 5 points are needed; 20 or more is better.', 'err'); return; }
    var pts = S.pts.map(function (p) { return toData(p.px, p.py); }).filter(function (p) { return isLog('x') || p.x >= -1e-9; }).sort(function (a, b) { return a.x - b.x; });   // no time before 0
    var ymax = Math.max(Math.abs(S.cal.y1.val), Math.abs(S.cal.y2.val));
    var fv = function (id) { var e = $(id); return e ? String(e.value).trim() : ''; };
    var det = { sub: fv('dig_sub'), n: fv('dig_n'), ev: fv('dig_ev'), tt: fv('dig_tt'), nr: fv('dig_nr'), tmult: fv('dig_tmult'),
                hr: fv('dig_hr'), hrlo: fv('dig_hrlo'), hrhi: fv('dig_hrhi') };
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('dig_save', {
        study: study, arm: arm, ymax: ymax, nonce: Math.random(), det: det,
        time: pts.map(function (p) { return Math.round(p.x * 1000) / 1000; }),
        surv: pts.map(function (p) { return Math.round(p.y * 10000) / 10000; })
      }, { priority: 'event' });
    }
    (S.masks = S.masks || []).push(S.pts.slice().sort(function (a, b) { return a.px - b.px; }));
    S.pts = []; draw();
    ['dig_sub', 'dig_n', 'dig_ev', 'dig_nr', 'dig_hr', 'dig_hrlo', 'dig_hrhi'].forEach(function (id) { var e = $(id); if (e) e.value = ''; });   // the times and the multiplier usually stay the same for the next arm
    status('Saved ' + pts.length + ' points for ' + study + ' / ' + arm + '. ' +
      (pts.length < 30 ? 'That is fewer than 30 points, so the hazard ratio will be less accurate. ' : '') +
      'Trace the next arm (change the arm name and the details) or load the next figure.', pts.length < 30 ? '' : 'ok');
  }

  // ---- setup ------------------------------------------------------------------------------------
  function init() {
    canvas = $('dig_canvas'); if (!canvas) return;
    ctx = canvas.getContext('2d'); wrap = $('dig_wrap'); loupe = $('dig_loupe'); lctx = loupe.getContext('2d');
    off = document.createElement('canvas'); octx = off.getContext('2d', { willReadFrequently: true });
    canvas.addEventListener('click', onClick);
    canvas.addEventListener('mousemove', onMove);
    canvas.addEventListener('mouseleave', onLeave);
    $('dig_file').addEventListener('change', function (e) { var f = e.target.files[0]; if (f) loadBlob(f, f.name); });
    document.addEventListener('click', function (e) {
      var b = e.target.closest ? e.target.closest('[data-dig-mode],[data-dig-act]') : null; if (!b) return;
      var m = b.getAttribute('data-dig-mode'), a = b.getAttribute('data-dig-act');
      if (m) setMode(S.mode === m ? 'none' : m);
      if (a === 'undo') { S.pts.pop(); draw(); }
      if (a === 'clear') { S.pts = []; draw(); }
      if (a === 'trace') trace();
      if (a === 'save') save();
      if (a === 'zoom-in') { S.zoom *= 1.25; draw(); }
      if (a === 'zoom-out') { S.zoom /= 1.25; draw(); }
      if (a === 'fit') fit();
      if (a === 'clear-box') { S.box = null; draw(); }
      if (a === 'clear-overlay') { S.overlay = []; draw(); }
      if (a === 'reset-cal') { S.cal = { x1: null, x2: null, y1: null, y2: null }; draw(); }
    });
    document.addEventListener('paste', function (e) {
      var items = (e.clipboardData || {}).items || [];
      for (var i = 0; i < items.length; i++) {
        if (items[i].type.indexOf('image') === 0) { var f = items[i].getAsFile(); if (f) { loadBlob(f, 'pasted image'); e.preventDefault(); } return; }
      }
    });
    ['dragover', 'drop'].forEach(function (ev) {
      wrap.addEventListener(ev, function (e) {
        e.preventDefault();
        if (ev === 'drop' && e.dataTransfer.files.length) loadBlob(e.dataTransfer.files[0], e.dataTransfer.files[0].name);
      });
    });
    ['dig_xscale', 'dig_yscale'].forEach(function (id) {
      var el = $(id); if (el) el.addEventListener('change', function () {
        S.cal = { x1: null, x2: null, y1: null, y2: null }; S.pts = []; S.overlay = []; draw();
        status('Axis type changed. Calibrate the four axis points again.');
      });
    });
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('dig_overlay', function (m) { S.overlay = m.curves || []; draw(); if (S.overlay.length && wrap) wrap.scrollIntoView({ block: 'center', behavior: 'smooth' }); });
    } else document.addEventListener('shiny:connected', function () {
      Shiny.addCustomMessageHandler('dig_overlay', function (m) { S.overlay = m.curves || []; draw(); if (S.overlay.length && wrap) wrap.scrollIntoView({ block: 'center', behavior: 'smooth' }); });
    });
    window.addEventListener('resize', function () { if (S.img) fit(); });
    if (window.ResizeObserver) new ResizeObserver(function () { if (S.img && wrap.clientWidth > 50 && !S.fitted) { S.fitted = true; fit(); } }).observe(wrap);
    draw();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init); else init();
  window.KMDigitizer = S;   // handy for debugging and tests
})();
