# Written Methods and Results for Kaplan-Meier mode, built from the actual analysis objects.
# Everything quoted comes from the numbers on screen; nothing is interpreted beyond the
# direction and statistical significance of an estimate.

km_ref_list <- c(
  "Guyot P, Ades AE, Ouwens MJNM, Welton NJ. Enhanced secondary analysis of survival data: reconstructing the data from published Kaplan-Meier survival curves. BMC Med Res Methodol 2012;12:9.",
  "Liu N, Zhou Y, Lee JJ. IPDfromKM: reconstruct individual patient data from published Kaplan-Meier survival curves. BMC Med Res Methodol 2021;21:111.",
  "Therneau TM, Grambsch PM. Modeling Survival Data: Extending the Cox Model. New York: Springer; 2000.",
  "Royston P, Parmar MKB. Restricted mean survival time: an alternative to the hazard ratio for the design and analysis of randomized trials with a time-to-event outcome. BMC Med Res Methodol 2013;13:152.",
  "Dafni U. Landmark analysis at the 25-year landmark point. Circ Cardiovasc Qual Outcomes 2011;4:363-371.",
  "Balduzzi S, Rucker G, Schwarzer G. How to perform a meta-analysis with R: a practical tutorial. Evid Based Ment Health 2019;22:153-160.",
  "IntHout J, Ioannidis JPA, Borm GF. The Hartung-Knapp-Sidik-Jonkman method for random effects meta-analysis is straightforward and considerably outperforms the standard DerSimonian-Laird method. BMC Med Res Methodol 2014;14:25.",
  "R Core Team. R: A language and environment for statistical computing. Vienna, Austria: R Foundation for Statistical Computing.")

# ---- small formatting helpers ---------------------------------------------------------
.rp_num <- function(x, d = 2) formatC(x, format = "f", digits = d)
.rp_p <- function(p) if (is.na(p)) "P not available" else if (p < 0.001) "P < 0.001" else sprintf("P = %.3f", p)
.rp_hr <- function(h, lo, hi, label = "HR") sprintf("%s %.2f, 95%% CI %.2f to %.2f", label, h, lo, hi)
.rp_join <- function(x) {
  x <- x[nzchar(x)]
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste0(paste(utils::head(x, -1), collapse = ", "), " and ", utils::tail(x, 1))
}
.rp_lead <- function(lo, hi, tx, ct, outcome) {
  if (is.na(lo) || is.na(hi)) return(sprintf("The difference in the hazard of %s between %s and %s could not be estimated", outcome, tx, ct))
  if (lo > 1) sprintf("The hazard of %s was higher with %s than with %s", outcome, tx, ct)
  else if (hi < 1) sprintf("The hazard of %s was lower with %s than with %s", outcome, tx, ct)
  else sprintf("There was no statistically significant difference in the hazard of %s between %s and %s", outcome, tx, ct)
}
`%+%` <- function(a, b) paste0(a, b)
.rp_sig <- function(lo, hi) !is.na(lo) && !is.na(hi) && (lo > 1 || hi < 1)

# ---- Methods ------------------------------------------------------------------------------
# p: km_params(); e: core environment; x: extra environment; v: environment holding variants_tab (or NULL)
km_methods_text <- function(p, e, x, v = NULL) {
  tx <- if (nzchar(p$name_tx)) p$name_tx else "treatment"
  ct <- if (nzchar(p$name_ctrl)) p$name_ctrl else "control"
  outcome <- if (nzchar(p$outcome)) p$outcome else "the outcome of interest"
  u <- if (nzchar(p$time_unit)) p$time_unit else "time units"
  k <- nlevels(e$ipd$study_id)
  n_all <- nrow(e$ipd)
  have_ev <- any(!is.na(e$arms$total_events))
  have_risk <- nrow(e$risk) > 0
  have_pub <- any(!is.na(e$arms$hr_published))

  p1 <- sprintf(paste0(
    "Kaplan-Meier curves comparing %s with %s for %s were available from %d studies (%s patients). ",
    "The published curves were digitized, and individual patient data were reconstructed for each arm from the digitized ",
    "coordinates%s using the iterative algorithm of Guyot and colleagues, as implemented in the IPDfromKM package [1,2]. ",
    "Reconstruction accuracy was checked by comparing the reconstructed with the reported %s and by overlaying the digitized points on the reconstructed curves."),
    tx, ct, outcome, k, format(n_all, big.mark = ","),
    if (have_risk && have_ev) ", the reported numbers at risk and the reported total number of events"
    else if (have_risk) " and the reported numbers at risk" else if (have_ev) " and the reported total number of events" else "",
    if (have_risk && have_ev) "numbers at risk and total events" else if (have_risk) "numbers at risk"
    else if (have_ev) "total events" else "curves")

  p2 <- sprintf(paste0(
    "The reconstructed data of all studies were pooled in a one-stage analysis. Kaplan-Meier curves were estimated for each group from its pooled patients, which describes the combined population and is not adjusted for study, ",
    "and the effect of %s relative to %s was summarised as a hazard ratio (HR) from a Cox proportional hazards model stratified by study, ",
    "with a log-rank test stratified by study [3]. Proportional hazards were assessed with a global test based on scaled Schoenfeld residuals and with log-log plots. ",
    "Median follow-up was estimated with the reverse Kaplan-Meier method. Restricted mean survival time (RMST) up to %s %s, and the difference in RMST between groups, ",
    "were estimated as measures that do not depend on proportional hazards [4]; survival and its absolute difference were also estimated at %s."),
    tx, ct, format(round(e$rmst$tau[1], 1)), u,
    if (length(p$landmarks)) paste(.rp_join(format(p$landmarks)), u) else "prespecified times")

  p3 <- sprintf(paste0(
    "Because the effect of treatment may change over follow-up, we also performed a landmark analysis at %s %s, estimating the hazard ratio from time zero to the landmark and, ",
    "among patients still at risk at the landmark, after it [5]. Hazard ratios were estimated within the intervals %s using patients at risk at the start of each interval. ",
    "In addition, the hazard ratio was modelled as a continuous function of time with a Cox model including a natural-spline time-varying treatment effect (%d degrees of freedom), ",
    "and a Wald test of the time-varying terms was used to test whether the hazard ratio was constant."),
    format(x$lm_time), u, .rp_join(sub("^After", "after", x$piecewise$interval)), p$tv_df)

  trunc_ <- x$truncs
  p4 <- paste0(
    "Sensitivity analyses comprised: a Cox model not stratified by study and a model with study as a gamma frailty term; ",
    "a two-stage meta-analysis of the study-specific hazard ratios using common-effect and random-effects models, the latter with restricted maximum likelihood estimation of the ",
    "between-study variance and the Hartung-Knapp adjustment [6,7]; leave-one-study-out analyses; ",
    if (length(trunc_)) sprintf("restriction of follow-up to %s %s; ", .rp_join(format(trunc_)), u) else "",
    if (have_pub) "comparison of the reconstructed with the published hazard ratios, and a two-stage analysis using published hazard ratios where available; " else "",
    if (!is.null(v) && exists("variants_tab", envir = v, inherits = FALSE)) "and repeating the reconstruction without the reported total events and without the numbers at risk. " else "",
    "These analyses use the same reconstructed patients and therefore assess dependence on modelling choices, not independent replication.")
  p4 <- sub("; These analyses", ". These analyses", p4, fixed = TRUE)
  p4 <- sub(", and repeating", ", and repeating", p4, fixed = TRUE)

  p5 <- sprintf(paste0("All tests were two-sided with a significance level of 0.05. Analyses were performed in %s with the survival, IPDfromKM and meta packages [8]. ",
                       "Reconstructed individual patient data are approximations of the original data and the results should be interpreted accordingly."),
                R.version.string)
  c(p1, p2, p3, p4, p5)
}

# ---- Results ------------------------------------------------------------------------------
km_results_text <- function(p, e, x, v = NULL) {
  tx <- if (nzchar(p$name_tx)) p$name_tx else "the treatment group"
  ct <- if (nzchar(p$name_ctrl)) p$name_ctrl else "the control group"
  tx_s <- if (nzchar(p$name_tx)) p$name_tx else "treatment"
  ct_s <- if (nzchar(p$name_ctrl)) p$name_ctrl else "control"
  outcome <- if (nzchar(p$outcome)) p$outcome else "the outcome"
  u <- if (nzchar(p$time_unit)) p$time_unit else "time units"
  k <- nlevels(e$ipd$study_id)
  a <- x$arm_summary; i_c <- which(a$group == "control"); i_t <- which(a$group == "treatment")
  h <- e$hr_pooled
  out <- character(0)

  # 1. Studies, follow-up, reconstruction quality
  ck <- e$checks
  rel <- abs(ck$events_reconstructed - ck$events_reported) / ck$events_reported
  rel <- rel[is.finite(rel)]
  fu <- function(i) if (is.na(a$median_followup[i])) "not estimable" else sprintf("%.1f %s", a$median_followup[i], u)
  r1 <- sprintf("A total of %d studies with %s patients contributed Kaplan-Meier curves (%s in the %s group and %s in the %s group). The median follow-up was %s in the %s group and %s in the %s group. %s",
    k, format(sum(a$patients), big.mark = ","), format(a$patients[i_t], big.mark = ","), tx_s, format(a$patients[i_c], big.mark = ","), ct_s,
    fu(i_t), tx_s, fu(i_c), ct_s,
    if (length(rel)) sprintf("The reconstructed numbers of events differed from the reported numbers by at most %.1f%% in any arm.", 100 * max(rel))
    else "Total events were not reported, so the reconstructed events could not be compared with published numbers.")
  out <- c(out, r1)

  # 2. Primary comparison
  ph_bad <- !is.na(e$ph_p) && e$ph_p < 0.05
  r2 <- sprintf("In the pooled analysis, %s (%s; %s). The stratified log-rank test gave %s. %s",
    tolower(substring(.rp_lead(h$lo, h$hi, tx_s, ct_s, outcome), 1, 1)) %+% substring(.rp_lead(h$lo, h$hi, tx_s, ct_s, outcome), 2),
    .rp_hr(h$hr, h$lo, h$hi), .rp_p(h$p), .rp_p(e$logrank_p),
    if (is.na(e$ph_p)) ""
    else if (ph_bad) sprintf("The proportional hazards assumption was not supported (global Schoenfeld test, %s), so the single hazard ratio should be read as an average over follow-up.", .rp_p(e$ph_p))
    else sprintf("There was no evidence against proportional hazards (global Schoenfeld test, %s).", .rp_p(e$ph_p)))
  rm <- e$rmst; rd <- e$rmst_diff
  r2 <- paste0(r2, sprintf(" Restricted mean survival time up to %s %s was %.2f %s in the %s group and %.2f %s in the %s group, a difference of %.2f %s (95%% CI %.2f to %.2f).",
    format(round(rm$tau[1], 1)), u, rm$rmst[rm$group == "treatment"], u, tx_s, rm$rmst[rm$group == "control"], u, ct_s,
    rd$diff, u, rd$diff - 1.96 * rd$se, rd$diff + 1.96 * rd$se))
  lt <- e$landmark_tab; rdt <- x$rd_tab
  if (!is.null(lt) && nrow(lt) && !is.null(rdt) && nrow(rdt)) {
    bits <- vapply(utils::head(sort(unique(lt$time)), 3), function(tm) {
      st <- lt$survival[lt$time == tm & lt$group == "treatment"]; sc <- lt$survival[lt$time == tm & lt$group == "control"]
      d <- rdt[rdt$time == tm, ]
      if (!length(st) || !length(sc) || is.na(st) || is.na(sc) || !nrow(d)) return("")
      sprintf("%.1f%% with %s and %.1f%% with %s at %s %s (difference %+.1f percentage points, 95%% CI %.1f to %.1f)",
              100 * st, tx_s, 100 * sc, ct_s, format(tm), u, 100 * d$difference, 100 * d$lower, 100 * d$upper)
    }, "")
    bits <- bits[nzchar(bits)]
    if (length(bits)) r2 <- paste0(r2, " Estimated survival was ", paste(bits, collapse = "; "), ".")
  }
  out <- c(out, r2)

  # 3. Hazards over time
  lm <- x$landmark_hr; pw <- x$piecewise
  fmt_row <- function(d, i) if (is.na(d$hr[i])) "not estimable" else sprintf("%.2f (95%% CI %.2f to %.2f)", d$hr[i], d$lo[i], d$hi[i])
  r3 <- sprintf("In the landmark analysis at %s %s, the hazard ratio was %s from time zero to the landmark and %s thereafter. By time interval, the hazard ratio was %s.",
    format(x$lm_time), u, fmt_row(lm, 1), fmt_row(lm, 2),
    paste(vapply(seq_len(nrow(pw)), function(i) sprintf("%s for %s %s", fmt_row(pw, i), sub("^After", "after", pw$interval[i]), u), ""), collapse = "; "))
  tv <- x$tv
  if (is.null(tv)) {
    r3 <- paste0(r3, " The time-varying model could not be fitted, usually because there were too few events.")
  } else {
    cross <- which(diff(sign(tv$hr - 1)) != 0)
    r3 <- paste0(r3, sprintf(" In the spline model, the estimated hazard ratio changed from %.2f at %.1f %s to %.2f at %.1f %s%s (test of a constant hazard ratio, %s).",
      tv$hr[1], tv$time[1], u, tv$hr[nrow(tv)], tv$time[nrow(tv)], u,
      if (length(cross)) sprintf(", crossing 1 at approximately %.1f %s", tv$time[cross[1]], u) else "", .rp_p(x$tv_p)))
  }
  if (!anyNA(lm$hr) && ((lm$hr[1] < 1 && lm$hr[2] > 1) || (lm$hr[1] > 1 && lm$hr[2] < 1)))
    r3 <- paste0(r3, " The direction of the association therefore differed between early and late follow-up.")
  if (!ph_bad && (is.na(x$tv_p) || x$tv_p >= 0.05) && !is.null(tv))
    r3 <- paste0(r3, " These analyses did not show clear evidence that the hazard ratio changed over time.")
  out <- c(out, r3)

  # 4. Sensitivity
  st <- if (!is.null(v) && exists("sens_all", envir = v, inherits = FALSE)) get("sens_all", envir = v) else x$sens_tab
  mod <- st[st$group == "Pooling model" & is.finite(st$hr), ]
  prim_sig <- .rp_sig(h$lo, h$hi)
  r4 <- character(0)
  if (nrow(mod) > 1) {
    diff_sig <- mod$analysis[vapply(seq_len(nrow(mod)), function(i) .rp_sig(mod$lo[i], mod$hi[i]) != prim_sig || (mod$hr[i] < 1) != (h$hr < 1), TRUE)]
    r4 <- c(r4, sprintf("Alternative pooling models gave %s. %s",
      if (round(min(mod$hr), 2) == round(max(mod$hr), 2)) sprintf("the same hazard ratio (%.2f)", min(mod$hr)) else sprintf("hazard ratios from %.2f to %.2f", min(mod$hr), max(mod$hr)),
      if (length(diff_sig)) sprintf("The following differed from the primary analysis in direction or statistical significance: %s.", .rp_join(sub("^(.)", "\\L\\1", diff_sig, perl = TRUE)))
      else "All were in the same direction and had the same statistical significance as the primary analysis."))
  }
  hg <- x$heterogeneity
  if (!is.null(hg)) r4 <- c(r4, sprintf("Heterogeneity between the study-level hazard ratios was I\u00b2 = %.0f%% (\u03c4\u00b2 = %.3f; Q test, %s)%s.",
      100 * hg$I2, hg$tau2, .rp_p(hg$Q_p),
      if (!is.na(hg$pred_lo)) sprintf(", with a 95%% prediction interval for a new study of %.2f to %.2f", hg$pred_lo, hg$pred_hi) else ""))
  lo_t <- x$loo_tab
  if (!is.null(lo_t) && nrow(lo_t) && any(is.finite(lo_t$hr))) {
    changed <- lo_t$omitted[vapply(seq_len(nrow(lo_t)), function(i) is.finite(lo_t$hr[i]) && .rp_sig(lo_t$lo[i], lo_t$hi[i]) != prim_sig, TRUE)]
    r4 <- c(r4, sprintf("In leave-one-study-out analyses the hazard ratio ranged from %.2f to %.2f%s.", min(lo_t$hr, na.rm = TRUE), max(lo_t$hr, na.rm = TRUE),
      if (length(changed)) sprintf(", and statistical significance changed after omitting %s", .rp_join(changed)) else ", with no change in statistical significance"))
  }
  fu_rows <- st[st$group == "Follow-up" & is.finite(st$hr), ]
  if (nrow(fu_rows)) r4 <- c(r4, sprintf("When follow-up was restricted, the hazard ratio was %s.",
      paste(sprintf("%.2f (95%% CI %.2f to %.2f) at %s", fu_rows$hr, fu_rows$lo, fu_rows$hi, paste(sub("Follow-up cut at ", "", fu_rows$analysis), u)), collapse = "; ")))
  pb <- x$pub_tab; npub <- sum(!is.na(pb$hr_published))
  if (npub > 0) { rr <- range(pb$ratio_rec_to_pub, na.rm = TRUE)
    r4 <- c(r4, sprintf("For the %d %s that reported a hazard ratio, the ratio of the reconstructed to the published hazard ratio was %s.",
      npub, if (npub == 1) "study" else "studies", if (round(rr[1], 2) == round(rr[2], 2)) sprintf("%.2f", rr[1]) else sprintf("between %.2f and %.2f", rr[1], rr[2]))) }
  rec <- st[st$group == "Reconstruction" & is.finite(st$hr), ]
  if (nrow(rec)) r4 <- c(r4, sprintf("Repeating the reconstruction with less information gave a hazard ratio of %s.",
      paste(sprintf("%.2f (95%% CI %.2f to %.2f) %s", rec$hr, rec$lo, rec$hi, tolower(sub("^Reconstruction ", "", rec$analysis))), collapse = "; ")))
  dg <- if (!is.null(v) && exists("digit_tab", envir = v, inherits = FALSE)) get("digit_tab", envir = v) else NULL
  if (!is.null(dg) && nrow(dg)) r4 <- c(r4, sprintf("To assess the effect of digitizing error, the reconstruction was repeated %d times after adding random errors (SD %.3f on the survival scale) to the digitized points; the standard deviation of the log hazard ratio across repetitions was %.3f, which widened the 95%% confidence interval of the pooled hazard ratio from %.2f to %.2f to %.2f to %.2f.",
      dg$runs, dg$noise, dg$sd_loghr, exp(log(dg$hr_base) - 1.96 * dg$se_loghr_base), exp(log(dg$hr_base) + 1.96 * dg$se_loghr_base), dg$ci_total_lo, dg$ci_total_hi))
  if (length(r4)) out <- c(out, paste(r4, collapse = " "))
  out
}

# One piece of text for download: Methods, Results, References
km_report_plain <- function(methods, results) {
  paste(c("METHODS", "", paste(methods, collapse = "\n\n"), "", "RESULTS", "", paste(results, collapse = "\n\n"), "",
          "REFERENCES", "", paste0(seq_along(km_ref_list), ". ", km_ref_list), "",
          "Generated by Ponderis from the analysis on screen. Edit the wording to match your protocol and verify the references before submission."), collapse = "\n")
}

# ---- Forest plot mode: a Results paragraph --------------------------------------------------
results_text <- function(m, p, k) {
  rnd <- p$model %in% c("random", "both")
  bt <- function(v) tryCatch(meta::backtransf(v, sm = m$sm, n = m$n.harmonic.mean), error = function(e) v)
  what <- SM_NAMES[[p$sm]]
  one <- function(label, te, lo, hi, pv) sprintf("%s pooled %s was %.2f (95%% CI %.2f to %.2f; %s)", label, what, bt(te), bt(lo), bt(hi), .rp_p(pv))
  parts <- character(0)
  if (rnd) parts <- c(parts, one("the random-effects", m$TE.random, m$lower.random, m$upper.random, m$pval.random))
  if (p$model %in% c("common", "both")) parts <- c(parts, one("the common-effect", m$TE.common, m$lower.common, m$upper.common, m$pval.common))
  main_sig <- if (rnd) m$pval.random < 0.05 else m$pval.common < 0.05
  het <- sprintf("Heterogeneity was %s (I\u00b2 = %.0f%%, \u03c4\u00b2 = %.3f; Q test, %s).",
                 if (is.na(m$I2)) "not estimable" else if (m$I2 < 0.25) "low" else if (m$I2 < 0.5) "moderate" else if (m$I2 < 0.75) "substantial" else "considerable",
                 100 * m$I2, m$tau2, .rp_p(m$pval.Q))
  paste0(sprintf("Data from %d studies were pooled. ", k),
         paste0(toupper(substring(parts[1], 1, 1)), substring(parts[1], 2)),
         if (length(parts) > 1) paste0(", and ", parts[2]) else "", ", which was ",
         if (isTRUE(main_sig)) "statistically significant" else "not statistically significant", ". ", het,
         if (rnd && p$pi && !is.null(m$lower.predict) && is.finite(m$lower.predict))
           sprintf(" The 95%% prediction interval was %.2f to %.2f.", bt(m$lower.predict), bt(m$upper.predict)) else "")
}
