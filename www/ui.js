// Interface behaviour: folding steps in the left rail, one-line summaries of what was chosen, workflow bar of Survival curves.
(function () {
  function short(t) {
    t = (t || "").replace(/\s+/g, " ").trim();
    var m = t.match(/\(([^)]{1,6})\)\s*$/);             // keep a short abbreviation such as (RR)
    if (m && /[A-Za-z]/.test(m[1])) return m[1];
    t = t.replace(/\s*\([^)]*\)\s*/g, " ").trim();
    return t.length > 34 ? t.slice(0, 32) + "…" : t;
  }
  function summarize(step) {
    var out = [], body = step.querySelector(".step-body");
    if (!body) return "";
    body.querySelectorAll("select").forEach(function (s) {
      if (out.length >= 3 || s.closest(".adv, .mapping, .shiny-panel-conditional[style*='display: none']")) return;
      var o = s.options[s.selectedIndex];
      if (o && o.text) out.push(short(o.text));
    });
    body.querySelectorAll(".input-group input[type=text][readonly]").forEach(function (f) {
      if (out.length < 3 && f.value) out.push(f.value.length > 28 ? f.value.slice(0, 26) + "…" : f.value);
    });
    var note = body.querySelector("#detect_note .aside, #detect_note .notice");
    if (note && note.textContent.trim() && out.length < 3) { var t = note.textContent.replace(/\s+/g, " ").trim(); out.push(t.length > 70 ? t.slice(0, 68) + "…" : t); }
    return out.join(" · ");
  }
  function setOpen(step, open) {
    step.classList.toggle("collapsed", !open);
    var h = step.querySelector(".step-head"); if (h) h.setAttribute("aria-expanded", open ? "true" : "false");
    var s = step.querySelector(".step-sum"); if (s) s.textContent = open ? "" : summarize(step);
    if (open) setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 50);
  }
  function toggle(e) {
    var h = e.target.closest(".step-head");
    if (!h || e.target.closest(".info-btn")) return;
    if (e.type === "keydown" && e.key !== "Enter" && e.key !== " ") return;
    e.preventDefault();
    var st = h.parentNode; setOpen(st, st.classList.contains("collapsed"));
  }
  document.addEventListener("click", toggle);
  document.addEventListener("keydown", toggle);

  // A step is closed when it is done (data loaded): the server says which ones
  function register() {
    if (!window.Shiny || register.done) return; register.done = true;
    Shiny.addCustomMessageHandler("steps_done", function (ids) {
      [].concat(ids).forEach(function (id) {
        document.querySelectorAll('.step[data-step="' + id + '"]').forEach(function (st) { st.classList.add("done"); setOpen(st, false); });
      });
    });
    Shiny.addCustomMessageHandler("steps_open", function (ids) {
      [].concat(ids).forEach(function (id) {
        document.querySelectorAll('.step[data-step="' + id + '"]').forEach(function (st) { st.classList.remove("done"); setOpen(st, true); });
      });
    });
  }
  document.addEventListener("shiny:connected", register);
  if (window.Shiny && Shiny.addCustomMessageHandler) register();

  // initial summaries of the steps that start folded
  function init() { document.querySelectorAll(".step.collapsed").forEach(function (s) { var e = s.querySelector(".step-sum"); if (e) e.textContent = summarize(s); }); }
  if (document.readyState !== "loading") init(); else document.addEventListener("DOMContentLoaded", init);
  document.addEventListener("shiny:connected", function () { setTimeout(init, 800); });
  // a folded step keeps its summary current when its outputs change (for example the note about where the data came from)
  var rt; if (window.jQuery) jQuery(document).on("shiny:value", function () { clearTimeout(rt); rt = setTimeout(init, 200); });

  // (the Survival curves workflow bar now has two steps; the data-tab hook stays for any step that names a tab)
  document.addEventListener("click", function (e) {
    var b = e.target.closest(".subnav-btn[data-tab]");
    if (!b) return;
    var tab = b.getAttribute("data-tab"), tries = 0;
    (function go() {
      var a = document.querySelector('#km_tabs a[data-value="' + tab + '"]');
      if (a) a.click(); else if (++tries < 15) setTimeout(go, 150);
    })();
  });
})();

// Home: navigation by data-go, the saved-project button and the gradual reveal (Home only).
(function () {
  function goto(mode) {
    var r = document.querySelector('input[name="mode"][value="' + mode + '"]');
    if (r) { r.click(); window.scrollTo(0, 0); }
  }
  document.addEventListener("click", function (e) {
    var g = e.target.closest("[data-go]");
    if (g) { e.preventDefault(); goto(g.getAttribute("data-go")); return; }
    if (e.target.closest("#home_open")) { goto("dig"); var f = document.getElementById("proj_open"); if (f) f.click(); }
  });
  function reveal() {
    var els = document.querySelectorAll(".home [data-rv]");
    if (!("IntersectionObserver" in window) || (window.matchMedia && matchMedia("(prefers-reduced-motion: reduce)").matches)) {
      els.forEach(function (e) { e.classList.add("in"); }); return;
    }
    var io = new IntersectionObserver(function (es) {
      es.forEach(function (en) { if (en.isIntersecting) { en.target.classList.add("in"); io.unobserve(en.target); } });
    }, { threshold: 0.12 });
    els.forEach(function (e) { io.observe(e); });
  }
  if (document.readyState !== "loading") reveal(); else document.addEventListener("DOMContentLoaded", reveal);
})();


// Opening an area from the top bar starts with every step closed.
document.addEventListener("change", function (e) {
  var r = e.target;
  if (!r || r.name !== "mode" || !r.closest(".mode-bar")) return;
  document.querySelectorAll(".step:not(.collapsed)").forEach(function (st) { st.classList.add("collapsed"); var h = st.querySelector(".step-head"); if (h) h.setAttribute("aria-expanded", "false"); var s = st.querySelector(".step-sum"); if (s) s.textContent = ""; });
  setTimeout(function () { document.querySelectorAll(".step.collapsed").forEach(function (st) { var e2 = st.querySelector(".step-sum"); if (e2) e2.dispatchEvent(new Event("x")); }); }, 0);
});
