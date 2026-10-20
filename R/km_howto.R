# Teaching panels ("How to read this") for the Survival curves area.
# Same idea as R/howto.R: a short guide to the elements of the figure or table, and a reading of the user's own numbers.
# The readings are built from the analysis objects; they describe direction and statistical significance and never go further.

# ---- Small helpers ---------------------------------------------------------------------

.kh_names <- function(p) list(
  tx = if (nzchar(p$name_tx)) p$name_tx else "the treatment group",
  ct = if (nzchar(p$name_ctrl)) p$name_ctrl else "the control group",
  u = if (nzchar(p$time_unit)) p$time_unit else "time units")
.kh_p <- function(x) if (is.na(x)) "n/a" else if (x < 0.001) "< 0.001" else sprintf("= %.3f", x)
.kh_pct <- function(x) sprintf("%.1f%%", 100 * x)
.kh_up <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))
.kh_hr_dir <- function(h, lo, hi) {
  if (any(!is.finite(c(h, lo, hi)))) return("could not be estimated")
  if (hi < 1) "is below 1 and its interval excludes 1: the hazard of the event is lower with treatment"
  else if (lo > 1) "is above 1 and its interval excludes 1: the hazard of the event is higher with treatment"
  else "has an interval that includes 1: the data are compatible with no difference in the hazard, and also with a benefit or a harm"
}

# ---- Guides ---------------------------------------------------------------------------

guide_km_pooled <- function() tagList(
  howto_list(
    list("Vertical axis", "The share of patients still free of the event (survival), from 100% at the start. With cumulative incidence it is the share who already had the event, rising from 0."),
    list("Horizontal axis", "Time since the start of follow-up, in the unit you chose. Far to the right, few patients remain, so that part is less reliable."),
    list("Steps", "Each drop is a time when events happened. A long flat stretch means no events (or only censoring) in that period."),
    list("Shaded band", "The 95% confidence interval. It widens to the right because fewer patients are at risk. Bands that overlap do not prove there is no difference."),
    list("Tick marks", "If switched on, patients who left follow-up without the event (censored)."),
    list("Numbers at risk", "Patients still followed at each time, under the plot. Read the end of the curves with these numbers in mind."),
    list("Hazard ratio and log-rank p", "Printed on the plot. They compare the whole curves, with the pooling done by a Cox model stratified by study, so each study is only compared with itself."),
    list("The curves themselves", "Each curve is drawn from all the patients of that group put together, without regard to study. It describes the combined population. If the trials differ in baseline risk and in how many patients each arm contributes, the two curves can differ for that reason alone. Rely on the stratified hazard ratio for the comparison.")),
  tags$p("These curves are rebuilt from the points you digitized (Guyot method). They are close to the real patient data, but they are an approximation and should be reported as such."))

guide_km_study <- function() howto_list(
  list("Curve", "The reconstructed curve of one arm of one study."),
  list("Dots", "The points you digitized. The curve should pass through them; a curve that leaves the dots means the numbers at risk or the total events do not agree with the figure."),
  list("Use", "Look here first when the pooled result looks odd: one study with a very different shape can drive the pooled curve."))

guide_km_summary <- function() howto_list(
  list("Events per 100 person-time units", "Events divided by the total time under follow-up. It is a crude rate that ignores the study each patient came from."),
  list("Median follow-up", "Estimated by the reverse Kaplan-Meier method, treating the event as censoring. It is how long a typical patient was followed, not the time to the event."),
  list("Median survival", "The time when survival crosses 50%. If the curve never reaches 50%, the median is 'not reached' and cannot be compared."),
  list("RMST", "Restricted mean survival time: the average event-free time up to the horizon, the area under the curve. The difference between arms is a gain or loss in time and does not need proportional hazards."),
  list("Survival at fixed times", "The share event-free at each time, with a 95% CI, and the difference between arms in percentage points."))

guide_km_hr <- function() howto_list(
  list("Hazard ratio (HR)", "Compares how fast events happen in the treatment arm and in the control arm. 1 means no difference, below 1 means fewer events per unit of time with treatment, above 1 means more."),
  list("95% CI", "If it includes 1, the data are compatible with no difference. A narrow interval means a more precise estimate."),
  list("Stratified Cox model", "One baseline hazard per study, one common treatment effect. This respects randomisation within each study."),
  list("Reconstructed / published", "Ratio of the HR you rebuilt to the one in the paper. Values close to 1 show that the reconstruction worked. The published adjusted HR is the better estimate when it exists."),
  list("Send to the forest plot", "Sends the study hazard ratios to the Forest plot so you can pool them with a random-effects model and read heterogeneity, prediction interval and bias."))

guide_km_nph <- function() tagList(
  howto_list(
    list("Proportional hazards", "The single hazard ratio is only a good summary when the effect is about the same during the whole follow-up. Curves that cross are a classic sign that it is not."),
    list("Schoenfeld test", "Tests the assumption. A small p suggests the effect changes with time; a large p does not prove it holds, especially with few events."),
    list("Landmark analysis", "Hazard ratio from time zero to the landmark, and then among patients who are still alive and under follow-up at the landmark. If they point in different directions, the effect is not constant."),
    list("Hazard ratio by interval", "One hazard ratio per period. Each uses the patients still at risk at its start."),
    list("Hazard ratio over time", "The solid line is the estimated hazard ratio at each time and the band its 95% CI. If the band covers 1 at some times, there is no clear effect there. The ends are the least reliable."),
    list("Log-log plot", "Plots log(-log survival) against log time. Roughly parallel lines support proportional hazards; lines that cross or diverge do not.")),
  tags$p("When hazards are not proportional, report the restricted mean survival time and the survival difference at fixed times next to the hazard ratio."))

guide_km_sens <- function() howto_list(
  list("Pooling model", "The same hazard ratios pooled in different ways. If the answer stays the same, the result does not depend on that choice."),
  list("Follow-up cuts", "The hazard ratio when everybody is censored at an earlier time. It answers a different question (the effect early on), so a difference is not an error."),
  list("Reconstruction checks", "Repeats the reconstruction without the total events, without the numbers at risk, and with small random errors added to your digitized points. It shows how much the hazard ratio depends on the quality of the digitizing."),
  list("Leave one study out", "The hazard ratio after removing each study in turn. A single study that changes the conclusion needs a closer look."),
  list("Reconstructed against published", "Each study's rebuilt hazard ratio next to the published one. Points that agree show that the method worked for that study."),
  list("Heterogeneity (I²)", "The share of the variation between studies that is beyond chance. With few studies it is very imprecise."),
  list("Prediction interval", "Where the hazard ratio of a new study is expected to lie. It is wider than the confidence interval when studies differ."))

guide_km_check <- function() howto_list(
  list("Events reported / reconstructed", "The rebuilt number of events should be close to the number in the paper (within about 5%)."),
  list("RMSE and max error", "How far the rebuilt curve is from the digitized points. Small values (below about 0.02) are good."),
  list("K-S p", "Compares the rebuilt and original curve. A very small p suggests a poor fit."),
  list("Risk table max diff", "The largest difference between the numbers at risk in the paper and in the rebuilt data, in patients."),
  list("Digitized points", "Fewer than about 30 points per curve make the hazard ratio less accurate."))

# ---- Readings --------------------------------------------------------------------------

# Survival of each group on a grid of times, from the pooled Kaplan-Meier fit
.kh_grid <- function(e) {
  tau <- e$rmst$tau[1]
  tm <- seq(0, tau, length.out = 60)
  s <- summary(e$km_fit, times = tm, extend = TRUE)
  g <- sub("^group=", "", as.character(s$strata))
  list(t = tm, ct = s$surv[g == "control"], tx = s$surv[g == "treatment"], tau = tau)
}

reading_km_pooled <- function(e, p) {
  nm <- .kh_names(p); h <- e$hr_pooled; g <- .kh_grid(e)
  ok <- is.finite(g$tx) & is.finite(g$ct) & g$t > 0
  d <- g$tx[ok] - g$ct[ok]
  above <- mean(d > 0.005); below <- mean(d < -0.005)
  s1 <- if (above > 0.9) sprintf("The curve of %s stays above the curve of %s during almost all of the follow-up, so fewer patients had the event with %s.", nm$tx, nm$ct, nm$tx)
        else if (below > 0.9) sprintf("The curve of %s stays below the curve of %s during almost all of the follow-up, so more patients had the event with %s.", nm$tx, nm$ct, nm$tx)
        else if (above > 0.1 && below > 0.1) "The two curves cross during follow-up. The risk is not always higher in one arm, and a single hazard ratio will hide that."
        else "The two curves are very close during the follow-up."
  if (isTRUE(p$display == "cumulative")) s1 <- paste0(s1, " (The plot shows cumulative incidence, which is the mirror image: the arm with the higher curve has more events.)")
  i <- length(g$t)
  s2 <- sprintf("At %s %s, estimated survival is %s with %s and %s with %s (a difference of %+.1f percentage points).",
                format(signif(g$tau, 4)), nm$u, .kh_pct(g$tx[i]), nm$tx, .kh_pct(g$ct[i]), nm$ct, 100 * (g$tx[i] - g$ct[i]))
  md <- e$medians
  mtx <- md$median[md$group == "treatment"]; mct <- md$median[md$group == "control"]
  s3 <- if (is.finite(mtx) && is.finite(mct)) sprintf("Median survival is %.1f %s with %s and %.1f %s with %s.", mtx, nm$u, nm$tx, mct, nm$u, nm$ct)
        else "At least one curve does not fall to 50% within the follow-up, so its median is not reached and the medians cannot be compared."
  s4 <- sprintf("The pooled hazard ratio is %.2f (95%% CI %.2f to %.2f). It %s.", h$hr, h$lo, h$hi, .kh_hr_dir(h$hr, h$lo, h$hi))
  s5 <- if (!is.na(e$ph_p) && e$ph_p < 0.05) sprintf("The test of proportional hazards is significant (p %s), so read the hazard ratio as an average and look at the Non-proportional hazards tab.", .kh_p(e$ph_p))
        else if (!is.na(e$ph_p)) sprintf("The test of proportional hazards does not show a problem (p %s).", .kh_p(e$ph_p)) else ""
  c(s1, s2, s3, s4, s5)
}

reading_km_study <- function(e, p) {
  nm <- .kh_names(p); hs <- e$hr_study; fd <- e$fidelity
  hs <- hs[is.finite(hs$effect), , drop = FALSE]
  k <- nrow(hs)
  s1 <- if (k == 0) "No study-level hazard ratio could be estimated."
        else sprintf("%d of %d studies have a hazard ratio below 1 (favouring %s), %d above 1. The study hazard ratios range from %.2f (%s) to %.2f (%s).",
                     sum(hs$effect < 1), k, nm$tx, sum(hs$effect >= 1), min(hs$effect), hs$study_id[which.min(hs$effect)], max(hs$effect), hs$study_id[which.max(hs$effect)])
  s2 <- if (nrow(fd) == 0) "" else {
    bad <- fd[fd$mean_abs_diff > 0.02, , drop = FALSE]
    if (nrow(bad)) sprintf("The rebuilt curve is further than 0.02 from your points, on average, for: %s. Look at those panels first (the Check tab has the numbers).", paste(paste(bad$study_id, bad$arm, sep = " / "), collapse = "; "))
    else sprintf("Every rebuilt curve stays within 0.02 of your digitized points on average (the largest average gap is %.3f).", max(fd$mean_abs_diff))
  }
  c(s1, s2)
}

reading_km_summary <- function(e, x, p) {
  nm <- .kh_names(p); a <- x$arm_summary; it <- which(a$group == "treatment"); ic <- which(a$group == "control")
  s1 <- sprintf("%s: %s events in %s patients (%.2f per 100 person-%s of follow-up). %s: %s events in %s patients (%.2f per 100 person-%s).",
                .kh_up(nm$tx), format(a$events[it], big.mark = ","), format(a$patients[it], big.mark = ","), a$rate_per_100[it], nm$u,
                .kh_up(nm$ct), format(a$events[ic], big.mark = ","), format(a$patients[ic], big.mark = ","), a$rate_per_100[ic], nm$u)
  rd <- e$rmst_diff; lo <- rd$diff - 1.96 * rd$se; hi <- rd$diff + 1.96 * rd$se
  s2 <- sprintf("Over the first %s %s, patients had on average %.2f %s %s event-free time with %s than with %s (95%% CI %.2f to %.2f)%s.",
                format(signif(e$rmst$tau[1], 4)), nm$u, abs(rd$diff), nm$u, if (rd$diff >= 0) "more" else "less", nm$tx, nm$ct, lo, hi,
                if (lo > 0 || hi < 0) ", so the interval excludes 0" else ", and the interval includes 0, so a difference of zero is compatible with the data")
  rt <- x$rd_tab
  s3 <- if (!is.null(rt) && nrow(rt)) {
    r <- utils::head(rt, 3)
    paste0("Absolute difference in survival (", nm$tx, " minus ", nm$ct, "): ",
           paste(sprintf("%+.1f points at %s %s (95%% CI %.1f to %.1f)", 100 * r$difference, format(r$time), nm$u, 100 * r$lower, 100 * r$upper), collapse = "; "), ".")
  } else ""
  md <- e$medians
  s4 <- if (all(is.finite(md$median))) "" else "At least one median is 'not reached'; the RMST and the survival at fixed times are then the better ways to compare the arms."
  c(s1, s2, s3, s4)
}

reading_km_hr <- function(e, x, p) {
  nm <- .kh_names(p); h <- e$hr_pooled
  s1 <- sprintf("The pooled hazard ratio for %s versus %s is %.2f (95%% CI %.2f to %.2f, p %s). It %s.", nm$tx, nm$ct, h$hr, h$lo, h$hi, .kh_p(h$p), .kh_hr_dir(h$hr, h$lo, h$hi))
  s1b <- sprintf("In plain words: at any moment, a patient on %s had about %.0f%% %s instantaneous risk of the event than a patient on %s, assuming the ratio is constant over time.",
                 nm$tx, 100 * abs(1 - h$hr), if (h$hr < 1) "lower" else "higher", nm$ct)
  hg <- x$heterogeneity
  s2 <- ""
  if (!is.null(hg)) s2 <- sprintf("Between studies, I² is %.0f%% (Q test p %s)%s.", 100 * hg$I2, .kh_p(hg$Q_p),
         if (!is.na(hg$pred_lo)) sprintf(", and the 95%% prediction interval for a new study is %.2f to %.2f", hg$pred_lo, hg$pred_hi) else "")
  pb <- x$pub_tab; n <- sum(!is.na(pb$hr_published))
  s3 <- if (n == 0) "No published hazard ratio was entered, so the rebuilt ones cannot be compared with the papers."
        else { rr <- range(pb$ratio_rec_to_pub, na.rm = TRUE)
               sprintf("For %d %s with a published hazard ratio, the rebuilt hazard ratio divided by the published one is %s. Values close to 1 mean the reconstruction worked.",
                       n, if (n == 1) "study" else "studies", if (rr[2] - rr[1] < 0.01) sprintf("%.2f", rr[1]) else sprintf("%.2f to %.2f", rr[1], rr[2])) }
  c(s1, s1b, s2, s3)
}

reading_km_nph <- function(x, p) {
  nm <- .kh_names(p)
  php <- if (is.null(x$ph_tab)) NA_real_ else x$ph_tab$p
  s1 <- if (is.na(php)) "The Schoenfeld test could not be calculated."
        else if (php < 0.05) sprintf("The Schoenfeld test is significant (p %s): there is evidence that the effect of treatment changes with time.", .kh_p(php))
        else sprintf("The Schoenfeld test is not significant (p %s). That is not proof that the hazards are proportional, because a test has little power with few events.", .kh_p(php))
  lm <- x$landmark_hr
  s2 <- if (nrow(lm) == 2 && !anyNA(lm$hr))
          sprintf("Landmark at %s %s: the hazard ratio is %.2f (%.2f to %.2f) before it and %.2f (%.2f to %.2f) after it.%s", format(x$lm_time), nm$u,
                  lm$hr[1], lm$lo[1], lm$hi[1], lm$hr[2], lm$lo[2], lm$hi[2],
                  if ((lm$hr[1] < 1) != (lm$hr[2] < 1)) " They fall on opposite sides of 1: the direction of the effect changes." else if (abs(log(lm$hr[1]) - log(lm$hr[2])) > 0.3) " The size of the effect differs between the periods." else " The two periods agree.")
        else "The landmark analysis could not be estimated for both periods."
  pw <- x$piecewise
  s3 <- if (nrow(pw) && !anyNA(pw$hr)) paste0("By interval: ", paste(sprintf("%s %s: %.2f (%.2f to %.2f)", pw$interval, nm$u, pw$hr, pw$lo, pw$hi), collapse = "; "), ".") else ""
  tv <- x$tv
  s4 <- if (is.null(tv)) "The model with the hazard ratio changing over time needs about 30 events and could not be fitted."
        else { cross <- which(diff(sign(tv$hr - 1)) != 0)
               sprintf("In the model with time-varying effect, the hazard ratio goes from %.2f at %s %s to %.2f at %s %s%s. Test that it is constant: p %s.",
                       tv$hr[1], format(round(tv$time[1], 1)), nm$u, tv$hr[nrow(tv)], format(round(tv$time[nrow(tv)], 1)), nm$u,
                       if (length(cross)) sprintf(", crossing 1 at about %.1f %s", tv$time[cross[1]], nm$u) else "", .kh_p(x$tv_p)) }
  differ <- nrow(lm) == 2 && !anyNA(lm$hr) && ((lm$hr[1] < 1) != (lm$hr[2] < 1) || abs(log(lm$hr[1]) - log(lm$hr[2])) > 0.3)
  s5 <- if ((!is.na(php) && php < 0.05) || (!is.na(x$tv_p) && x$tv_p < 0.05)) "Because of this, report the interval or landmark hazard ratios and the RMST next to the single hazard ratio."
        else if (differ) "The tests are not significant, but the estimates in the two periods are not alike. Tests have little power, so look at the plot of the hazard ratio over time and consider reporting the landmark hazard ratios and the RMST as well."
        else "Nothing here argues against reporting a single hazard ratio, but look at the plot of the hazard ratio over time as well."
  c(s1, s2, s3, s4, s5)
}

reading_km_sens <- function(e, x, v, p) {
  h <- e$hr_pooled; a <- v$sens_all; ok <- is.finite(a$hr)
  if (!any(ok)) return(character(0))
  prim <- .rp_sig(h$lo, h$hi)
  same <- vapply(which(ok), function(i) (.rp_sig(a$lo[i], a$hi[i]) == prim) && ((a$hr[i] < 1) == (h$hr < 1)), TRUE)
  s1 <- sprintf("Across %d analyses the hazard ratio goes from %.2f to %.2f. %s", sum(ok), min(a$hr[ok]), max(a$hr[ok]),
                if (all(same)) "Every one has the same direction and the same statistical significance as the main result, so the conclusion does not depend on these choices."
                else sprintf("These differ from the main result in direction or significance: %s. Say so in the paper and explain why.", .rp_join(a$analysis[ok][!same])))
  lo <- x$loo_tab
  s2 <- if (is.null(lo)) "Leave-one-out needs at least three studies."
        else { ch <- lo$omitted[vapply(seq_len(nrow(lo)), function(i) is.finite(lo$hr[i]) && .rp_sig(lo$lo[i], lo$hi[i]) != prim, TRUE)]
               sprintf("Leaving out each study in turn gives hazard ratios from %.2f to %.2f. %s", min(lo$hr, na.rm = TRUE), max(lo$hr, na.rm = TRUE),
                       if (length(ch)) sprintf("Statistical significance changes after leaving out %s, so the result depends on %s.", paste(ch, collapse = " or "), if (length(ch) == 1) "that study" else "those studies") else "Statistical significance never changes.") }
  d <- v$digit_tab
  s3 <- if (is.null(d)) "Press the button above to test how much digitizing error would change the hazard ratio."
        else sprintf("With random reading errors of %.3f added to your points (%d repeats), the hazard ratio stays between %.2f and %.2f. %s", d$noise, d$runs, d$hr_lo_2.5, d$hr_hi_97.5,
                     if (d$hr_lo_2.5 < 1 && d$hr_hi_97.5 > 1 && h$hi < 1) "That range includes 1, so the finding is sensitive to digitizing error." else "")
  c(s1, s2, s3)
}

reading_km_check <- function(e) {
  ck <- e$checks; fd <- e$fidelity
  rel <- abs(ck$events_reconstructed - ck$events_reported) / ck$events_reported; rel <- rel[is.finite(rel)]
  s1 <- if (length(rel)) sprintf("Reconstructed events differ from the reported ones by at most %.1f%% in any of the %d arms%s.", 100 * max(rel), nrow(ck),
                                 if (max(rel) > 0.05) " (above the 5% that is usually accepted: check the points and the numbers at risk)" else " (within the 5% usually accepted)")
        else "No total events were entered, so the reconstructed events cannot be compared with the papers."
  s2 <- sprintf("The rebuilt curves are on average within %.3f of the digitized points (the worst arm is %.3f)%s.", mean(fd$mean_abs_diff), max(fd$mean_abs_diff),
                if (max(fd$mean_abs_diff) > 0.02) ", and some arms are further than 0.02 from the points" else "")
  few <- fd[fd$points < 30, , drop = FALSE]
  s3 <- if (nrow(few)) sprintf("%d %s fewer than 30 digitized points, which makes the hazard ratio less accurate.", nrow(few), if (nrow(few) == 1) "arm has" else "arms have") else ""
  c(s1, s2, s3)
}
