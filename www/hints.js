// Method hints: hovering an option in the forest plot panel for about 0.7 s shows where it is used and the best practice.
// Entries are keyed "inputId:value" for list options and "inputId" for check boxes.
(function () {
  var H = {
    // ---- Data type
    "type:dich": { t: "Dichotomous data", use: "Two groups, each with the number of patients who had the event and the total. Typical for mortality, stroke or reintervention in trials and cohort comparisons.", best: "Choose it when you have the raw counts, not only a published effect. Then pick RR or OR (RD for absolute differences)." },
    "type:cont": { t: "Continuous data", use: "Two groups with mean, SD and n of a measured outcome, such as blood pressure, LVEF or length of stay.", best: "Use MD when every study has the same unit and SMD when the scales differ. If a study reports median and IQR, convert it first in the Calculator tab." },
    "type:gen": { t: "Pre-calculated effect", use: "Each study already reports an effect with its 95% CI or standard error, for example an adjusted HR from a survival analysis or an OR from a cohort.", best: "Pool the same kind of estimate (never mix HR with OR). Prefer adjusted estimates for observational studies and say which variables were adjusted for. Ratios are pooled on the log scale automatically." },
    "type:prop": { t: "Single proportion", use: "Only one group: the share of patients with an outcome, such as prevalence, incidence or 30-day mortality.", best: "Expect high heterogeneity, so use random effects and report the prediction interval. Do not pool raw proportions when values are close to 0% or 100%." },

    // ---- Effect measure
    "sm:RR": { t: "Risk ratio (RR)", use: "Dichotomous outcomes. It tells how many times more (or less) likely the event is in the treatment group.", best: "The easiest measure to interpret clinically, and usually more consistent across populations than the OR or RD. A good default for trials." },
    "sm:OR": { t: "Odds ratio (OR)", use: "Dichotomous outcomes, case-control studies and results from logistic regression.", best: "It overstates the effect compared with the RR when the outcome is common (above about 10%). Use it for rare events or when the studies only give ORs." },
    "sm:RD": { t: "Risk difference (RD)", use: "Dichotomous outcomes, as an absolute difference in risk. It is the base for the number needed to treat.", best: "It varies a lot with the baseline risk, so heterogeneity is common. Show it next to the RR or OR, not instead of them." },
    "sm:MD": { t: "Mean difference (MD)", use: "Continuous outcomes measured in the same unit in every study.", best: "Preferred whenever the unit is the same, because the result keeps its clinical meaning (for example, mmHg or points of LVEF)." },
    "sm:SMD": { t: "Standardized mean difference (SMD)", use: "Continuous outcomes measured with different scales, such as different questionnaires for the same concept.", best: "Hedges' g corrects the bias of small samples. Read it in standard deviations (about 0.2 small, 0.5 moderate, 0.8 large). If all studies share a unit, MD is easier to interpret." },
    "sm:HR": { t: "Hazard ratio (HR)", use: "Time-to-event outcomes, such as survival or time to reintervention.", best: "Check that hazards are roughly proportional. Take the HR and its CI from the paper, preferably adjusted for observational studies. To rebuild the data from curves, use the Survival curves tab." },
    "sm:Other": { t: "Other effect (difference scale)", use: "Any effect that is a difference and not a ratio, such as a change in a score or percentage points.", best: "Do not use it for RR, OR or HR, which must be pooled on the log scale." },
    "sm:PLOGIT": { t: "Logit transformation", use: "Single proportions, working on the log-odds scale.", best: "A sound default. Studies with 0% or 100% get a continuity correction, so check the result with another transformation if there are many." },
    "sm:PFT": { t: "Freeman-Tukey double arcsine", use: "Single proportions. It stabilises the variance and keeps studies with 0% or 100%.", best: "Handles extremes well, but the pooled value can be misleading when sample sizes differ a lot. Compare it with the logit as a sensitivity analysis." },
    "sm:PAS": { t: "Arcsine transformation", use: "Single proportions on the arcsine square-root scale.", best: "Mostly kept to reproduce older analyses. The logit or Freeman-Tukey are usually preferable." },
    "sm:PRAW": { t: "Raw proportion", use: "Pools the proportions as they are, with no transformation.", best: "Only when all values are far from 0 and 1 (roughly 20% to 80%) and the studies are large. Otherwise the CI can go below 0% or above 100%." },

    // ---- Pooling method and zero cells
    "method:MH": { t: "Mantel-Haenszel", use: "Dichotomous data with RR, OR or RD.", best: "The default choice. It behaves better than inverse variance when events are few or some cells are zero." },
    "method:Inverse": { t: "Inverse variance", use: "A generic method that works for any effect measure.", best: "Fine when every study has a reasonable number of events. With rare events the standard errors are poorly estimated, so prefer Mantel-Haenszel or Peto." },
    "method:Peto": { t: "Peto odds ratio", use: "Odds ratios only, for rare events.", best: "Best with very rare events (below about 1%), balanced arms and small effects. It is biased with unbalanced arms or a large effect. Needs the odds ratio as the measure." },
    "incr:only0": { t: "Correction only where needed", use: "Adds 0.5 to the cells of studies that have a zero cell, and leaves the others as they are.", best: "The usual approach. Run a sensitivity analysis, because the result with zero events can depend on the correction." },
    "incr:all": { t: "Correction in all studies", use: "Adds 0.5 to every study when at least one study has a zero cell.", best: "It slightly distorts studies that had no zeros, so use it mainly to match another analysis. Studies with no events in both arms add no information to RR or OR." },

    // ---- Model and heterogeneity
    "model:random": { t: "Random effects", use: "When studies differ in population, dose, follow-up or design, which is almost always the case in clinical reviews.", best: "The usual primary model. It estimates the average effect across studies, so report the heterogeneity (I², τ²) and a prediction interval." },
    "model:common": { t: "Common effect", use: "When all studies are close to identical, for example centres following the same protocol.", best: "It assumes one true effect, so the CI is too narrow if there is heterogeneity. Use it as a sensitivity analysis unless you justified it in advance." },
    "model:both": { t: "Both models", use: "Shows the common and random-effects results together.", best: "Useful to compare. A large difference between them points to heterogeneity or small-study effects." },
    "tau:REML": { t: "REML", use: "Estimator of the between-study variance (τ²).", best: "Recommended default. It is less biased than DerSimonian-Laird, especially with few studies." },
    "tau:DL": { t: "DerSimonian-Laird", use: "The classic estimator, also used by RevMan.", best: "It underestimates τ² with few studies or high heterogeneity, which makes the CI too narrow. Use it with the Hartung-Knapp adjustment, or to reproduce a RevMan analysis." },
    "tau:PM": { t: "Paule-Mandel", use: "Estimator of τ² that is recommended for both binary and continuous data.", best: "A good alternative to REML. Use it as a sensitivity analysis." },
    "hk": { t: "Hartung-Knapp adjustment", use: "Random-effects models. It widens the CI to account for the uncertainty in τ².", best: "Recommended in most reviews, mainly with fewer than about 20 studies, because it keeps the confidence interval honest. With 2 or 3 studies the interval becomes very wide, which is expected." },
    "pi": { t: "Prediction interval", use: "Random-effects models. It shows the range where the effect of a new study is expected to fall.", best: "Report it whenever you use random effects with at least 3 studies. It shows what heterogeneity means in practice, and the CI of the mean alone hides it." },

    // ---- Style and order
    "layout:meta": { t: "Default style", use: "The standard layout of the meta package.", best: "Clean and good for working. Switch to the journal style when you prepare the figure." },
    "layout:RevMan5": { t: "RevMan 5 style", use: "The Cochrane look, with the groups' events and totals beside the plot.", best: "Use it for Cochrane reviews or when readers expect RevMan figures." },
    "layout:JAMA": { t: "JAMA style", use: "Layout following the JAMA journals.", best: "Choose it when the target journal is JAMA or similar, and check the journal's figure instructions." },
    "layout:BMJ": { t: "BMJ style", use: "Layout following the BMJ.", best: "Choose it when the target journal is BMJ or similar, and check the journal's figure instructions." },
    "sort:none": { t: "As in the file", use: "Keeps the order of the rows in your CSV.", best: "Good when you already ordered the studies on purpose, for example by subgroup." },
    "sort:year": { t: "By year", use: "Oldest to newest.", best: "Shows how the effect changed over time. The usual choice in publications." },
    "sort:effect": { t: "By effect size", use: "From the smallest to the largest effect.", best: "Makes the spread of results easy to see, but the order says nothing about study quality." },
    "sort:weight": { t: "By weight", use: "Heaviest studies first.", best: "Shows which studies drive the pooled result. Say the order in the figure legend." },

    // ---- Publication bias
    "bias:Egger": { t: "Egger's test", use: "Regression test for funnel plot asymmetry (small-study effects).", best: "Use it with at least 10 studies, because power is low otherwise. With odds ratios it can give false signals, so consider Thompson-Sharp. Asymmetry alone does not prove publication bias." },
    "bias:Begg": { t: "Begg's test", use: "Rank-correlation test for funnel plot asymmetry.", best: "Less powerful than Egger's test. Use it as a complement and only with at least 10 studies." },
    "bias:Thompson": { t: "Thompson-Sharp test", use: "Regression test that also accounts for heterogeneity between studies.", best: "A better choice than Egger's test for ratio measures (OR, RR) when there is heterogeneity. Needs at least 10 studies." },
    "trimfill": { t: "Trim-and-fill", use: "Estimates how many studies are missing on one side of the funnel plot and adds them to the analysis.", best: "Use it only as a sensitivity analysis. It assumes that asymmetry comes from publication bias and works poorly with heterogeneity, so do not replace the main result with it." },
    // ---- Survival curves: set-up of the analysis (hover the option, or the name of the field)
    "km_scale:proportion": { l1: "What it does", t: "Proportions (0 to 1)", use: "Tells the app that the survival values in your curve file run from 0 to 1.", best: "Use it when the digitizer exported a 0 to 1 axis. A wrong choice is the commonest cause of a strange curve, so check the first plot." },
    "km_scale:percent": { l1: "What it does", t: "Percent (0 to 100)", use: "Tells the app that the survival values run from 0 to 100, as on an axis labelled in percent. They are divided by 100.", best: "Use it when the published axis shows percent and you digitized it as it appears. Do not mix proportions and percent in the same file." },
    "km_curve_type:survival": { l1: "What it does", t: "Survival (falling curve)", use: "The published figure starts at the top and falls as events happen.", best: "The usual Kaplan-Meier figure. The app reconstructs patients directly from it." },
    "km_curve_type:cumulative": { l1: "What it does", t: "Cumulative incidence (rising curve)", use: "The published figure starts at zero and rises with events. The app converts it to survival (1 minus the value) before reconstructing.", best: "Only for 1 minus Kaplan-Meier curves. If the figure is a competing-risks cumulative incidence, the reconstruction is not valid: it assumes censoring is non-informative." },
    "km_tau": { l1: "What it does", t: "RMST horizon", use: "Restricted mean survival time (RMST) is the average time free of the event up to this time point. It gives a difference in time gained, which stays valid when hazards are not proportional.", best: "Pick a time that every study reaches, ideally set before looking at the data and justified clinically. Left empty, the shortest follow-up among the studies is used." },
    "km_landmarks": { l1: "What it does", t: "Survival at fixed times", use: "Reports survival, with its confidence interval, and the absolute difference between the arms at each time you list.", best: "Use clinically meaningful times (for example 1, 3 and 5 years) that fall inside the follow-up of most studies. Estimates near the end of follow-up rest on few patients." },
    "km_landmark_time": { l1: "What it does", t: "Landmark time", use: "Splits follow-up in two. It gives the hazard ratio before the landmark and, restarting the curves, among patients still alive and under follow-up after it.", best: "Choose the time where the curves seem to change, or a clinical time point, and say that you did. Several tries on the same data inflate false positives." },
    "km_cuts": { l1: "What it does", t: "Cut points for intervals", use: "Estimates one hazard ratio per time interval (for example 0 to 6, 6 to 24 and after 24).", best: "Use a few intervals with enough events in each. Intervals with few events give very wide, unstable hazard ratios." },
    "km_truncs": { l1: "What it does", t: "Shorter follow-up", use: "Repeats the Cox analysis after censoring everybody at the time you type, to see whether the result depends on the late, less reliable part of the curves.", best: "Try one or two earlier times. Late follow-up has few patients at risk and the reconstruction there is the least accurate." },
    "km_tv_df:2": { l1: "What it does", t: "Smooth (2)", use: "Lets the hazard ratio change over time with a very simple curve.", best: "Good when there are few events (under about 100). It cannot show more than a gentle trend." },
    "km_tv_df:3": { l1: "What it does", t: "Moderate (3)", use: "A middle level of flexibility for the hazard ratio over time.", best: "A sensible default. Change it only to check that the shape does not depend on this choice." },
    "km_tv_df:4": { l1: "What it does", t: "Flexible (4)", use: "Allows the hazard ratio to bend more than once.", best: "Needs many events. With few events the curve starts to follow noise, mostly at its ends." },
    "km_tv_df:5": { l1: "What it does", t: "Very flexible (5)", use: "The most flexible shape offered for the hazard ratio over time.", best: "Use only with hundreds of events, and as a check. Do not choose the setting that gives the most interesting curve." },

    // ---- Survival curves: plot
    "km_display:survival": { l1: "What it does", t: "Plot survival", use: "Draws the probability of being event-free, falling from 1.", best: "The standard way. Better when the event is common, so the curves are not squeezed near the top." },
    "km_display:cumulative": { l1: "What it does", t: "Plot cumulative incidence", use: "Draws 1 minus survival, rising from 0.", best: "Easier to read when events are rare, because the differences are not squeezed into the top of the plot. Show the same data in the text." },
    "km_conf": { l1: "What it does", t: "95% confidence bands", use: "Shades the range of values compatible with the data around each curve.", best: "Keep them on. Overlapping bands do not prove there is no difference, and the bands widen where few patients are at risk." },
    "km_marks": { l1: "What it does", t: "Censored patients", use: "Puts a small tick on the curve where patients left follow-up without the event.", best: "Useful to show when censoring happens. In pooled curves of reconstructed data the ticks are crowded, so it is usually left off." },
    "km_risk_table": { l1: "What it does", t: "Numbers at risk", use: "Shows how many patients are still followed at several times under the plot.", best: "Always keep it in a figure for a paper. Reviewers read the tail of a curve by looking at how many patients remain." },
    "km_ypct": { l1: "What it does", t: "Percent axis", use: "Labels the vertical axis in percent instead of 0 to 1.", best: "A matter of the journal style. It does not change any number." },
    "km_send_src:reconstructed": { l1: "What it does", t: "Reconstructed hazard ratios", use: "Pools the hazard ratio that the app calculated from each rebuilt curve.", best: "The same method for every study, but an approximation. Compare with the published ones when they exist." },
    "km_send_src:published_first": { l1: "What it does", t: "Published first", use: "Takes the hazard ratio printed in the paper where you entered it, and the reconstructed one for the other studies.", best: "A good choice for the main analysis: the published adjusted HR is more reliable than a rebuilt one. Say which studies used which." },
    "km_send_src:published_only": { l1: "What it does", t: "Published only", use: "Pools only the hazard ratios printed in the papers.", best: "The reference analysis when every study reports one. Studies without a published HR drop out." },

    // ---- Survival curves: sensitivity
    "km_digit_B": { l1: "What it does", t: "Digitizing-error repeats", use: "How many times the reconstruction is repeated after moving every digitized point by a small random error.", best: "30 is enough to see the spread, and 100 or more gives a steadier estimate. More repeats take longer." },
    "km_digit_noise": { l1: "What it does", t: "Reading error", use: "The size of the random error added to each digitized survival value, in survival units (0.005 is half a percentage point).", best: "Use the error you think your digitizing has: usually 0.005 to 0.01 with a careful, high-resolution figure." },

    // ---- Digitizer
    "dig_ctype:survival": { l1: "What it does", t: "Survival (falling curve)", use: "The figure starts at 100% and falls.", best: "Choose what the figure shows, not what you want to analyse. The pooled analysis converts it for you." },
    "dig_ctype:cumulative": { l1: "What it does", t: "Cumulative incidence (rising curve)", use: "The figure starts at 0% and rises.", best: "If it is a competing-risks cumulative incidence, the reconstruction does not apply: it needs a Kaplan-Meier curve." },
    "dig_xscale": { l1: "What it does", t: "X axis scale", use: "How the time axis is spaced: equal steps (linear) or by powers of ten (logarithmic).", best: "Kaplan-Meier time axes are almost always linear. Calibrate with two ticks far apart." },
    "dig_yscale": { l1: "What it does", t: "Y axis scale", use: "How the survival axis is spaced: linear for a normal Kaplan-Meier figure.", best: "Choose logarithmic only if the ticks are really spaced that way. Values must be above zero." },
    "dig_tol": { l1: "What it does", t: "Colour tolerance", use: "How different from the picked colour a pixel can be and still count as part of the curve.", best: "Raise it for a faint or blurred line, lower it when other curves of a similar colour are caught by mistake. Check the result." },
    "dig_step": { l1: "What it does", t: "Pixels between points", use: "How far apart the traced points are along the time axis.", best: "3 gives a dense trace. Enough points (30 or more per curve) matter more for the accuracy of the hazard ratio than a very fine step." },
    "dig_mask": { l1: "What it does", t: "Ignore saved curves", use: "Skips the pixels where a curve you already saved lies, so a second curve of the same colour can be traced.", best: "Trace and save the continuous curve first, then tick this for the dashed one. Check the orange points the app estimates." },
    "dig_start": { l1: "What it does", t: "Where the curve begins", use: "The starting point the tracing is anchored to.", best: "Leave as the top for survival and the bottom for cumulative incidence. Use 'where I clicked' only for a curve that does not start at 100% or 0%." },
    "dig_tt": { l1: "What it does", t: "Times of the risk table", use: "The time points printed in the table of numbers at risk under the figure.", best: "Type them exactly as printed, in the same order as the numbers at risk below." },
    "dig_nr": { l1: "What it does", t: "Numbers at risk", use: "The patients still at risk at each of those times.", best: "These make the reconstruction much more accurate, because they tell the model how many patients leave through censoring. Always enter them when the paper gives them." },
    "dig_tmult": { l1: "What it does", t: "Multiply the times by", use: "Converts the time axis to the unit of your analysis, for example 12 to turn years into months.", best: "Use one time unit for every study of a comparison. Leave at 1 if the figure is already in that unit." },
    "dig_ev": { l1: "What it does", t: "Total events", use: "The number of events reported for the arm.", best: "Enter it when the paper gives it: the reconstruction is then forced to match, and the Check tab can show how well it did." },
    "dig_n": { l1: "What it does", t: "Patients in the arm", use: "The number of patients randomised or analysed in the arm.", best: "Enter the number the curve is based on. A wrong number changes the censoring pattern of the rebuilt data." }
  };

  var DELAY = 700, SWITCH = 150, card, timer, cur = null;

  function el(tag, cls, text) { var e = document.createElement(tag); if (cls) e.className = cls; if (text != null) e.textContent = text; return e; }
  function getCard() {
    if (!card) { card = el("div", "hint-card"); card.setAttribute("role", "tooltip"); document.body.appendChild(card); }
    return card;
  }
  function hide() { clearTimeout(timer); cur = null; if (card) card.classList.remove("on"); }

  function show(h, anchor) {
    var c = getCard(); c.innerHTML = "";
    c.appendChild(el("div", "hint-title", h.t));
    if (h.paras) {
      h.paras.forEach(function (t) { c.appendChild(el("p", "", t)); });
    } else {
      c.appendChild(el("div", "hint-label", h.l1 || "Where it is used"));
      c.appendChild(el("p", "", h.use));
      c.appendChild(el("div", "hint-label", "Best practice"));
      c.appendChild(el("p", "", h.best));
    }
    c.style.left = "0px"; c.style.top = "0px"; c.classList.add("on");
    var r = anchor.getBoundingClientRect(), w = c.offsetWidth, ch = c.offsetHeight, vw = window.innerWidth, vh = window.innerHeight;
    var x = r.right + 10; if (x + w > vw - 8) x = Math.max(8, r.left - w - 10);
    var y = Math.min(Math.max(8, r.top - 6), Math.max(8, vh - ch - 8));
    c.style.left = x + "px"; c.style.top = y + "px";
  }

  // Finds the hint for the element under the mouse: a list option, a check box, a radio option or the name of a field.
  function lookup(target) {
    var opt = target.closest(".selectize-dropdown .option");
    if (opt) {
      var ctrl = opt.closest(".selectize-control"), sel = ctrl && ctrl.parentNode && ctrl.parentNode.querySelector("select");
      var key = sel && sel.id + ":" + opt.getAttribute("data-value");
      return H[key] ? { h: H[key], a: opt.closest(".selectize-dropdown") || opt, k: key } : null;
    }
    var ib = target.closest(".info-btn[data-help]");
    if (ib) {
      var t = ib.getAttribute("data-help"), hd = ib.closest(".step-head"), nm = hd && hd.querySelector("h3");
      return { h: { t: "What to do here", paras: t.split("\n").filter(Boolean) }, a: ib, k: "info:" + (nm ? nm.textContent : t.slice(0, 30)) };
    }
    var cb = target.closest(".checkbox, label.dig-check");
    if (cb) {
      var inp = cb.querySelector("input[type=checkbox]");
      return inp && H[inp.id] ? { h: H[inp.id], a: cb, k: inp.id } : null;
    }
    var rd = target.closest(".radio");
    if (rd) {
      var r = rd.querySelector("input[type=radio]"), rk = r && r.name + ":" + r.value;
      return rk && H[rk] ? { h: H[rk], a: rd, k: rk } : null;
    }
    var lb = target.closest("label[for]");
    if (lb) {
      var lk = lb.getAttribute("for").replace(/-selectized$/, "");
      return H[lk] ? { h: H[lk], a: lb, k: lk } : null;
    }
    return null;
  }

  document.addEventListener("mouseover", function (e) {
    var f = lookup(e.target);
    if (!f) { if (!e.target.closest(".hint-card")) hide(); return; }
    if (cur === f.k) return;
    var visible = card && card.classList.contains("on");
    clearTimeout(timer); cur = f.k;
    timer = setTimeout(function () { show(f.h, f.a); }, visible ? SWITCH : DELAY);
  });
  document.addEventListener("mouseout", function (e) {
    if (!e.relatedTarget || !e.relatedTarget.closest || !lookup(e.relatedTarget)) hide();
  });
  ["mousedown", "keydown", "scroll", "wheel"].forEach(function (ev) { document.addEventListener(ev, hide, true); });

  // "How to read this" panels: open the first time, then remember what the person chose.
  function initHowto() {
    document.querySelectorAll("details.howto").forEach(function (d) {
      var k = "mf_howto_" + d.getAttribute("data-key"), v = null;
      try { v = localStorage.getItem(k); } catch (e) {}
      if (v === null || v === "open") d.open = true;
      d.addEventListener("toggle", function () { try { localStorage.setItem(k, d.open ? "open" : "closed"); } catch (e) {} });
    });
  }
  if (document.readyState !== "loading") initHowto(); else document.addEventListener("DOMContentLoaded", initHowto);
  document.addEventListener("shiny:connected", initHowto, { once: true });
})();
