# Kaplan-Meier mode, part 2: summaries by arm, non-proportional hazards (landmark, intervals,
# time-varying hazard ratio) and sensitivity analyses. Like km_helpers.R, the analysis code is
# kept as text so the app and the exported script run exactly the same lines.
#
# Objects expected in the environment (created by km_analysis_code): ipd, arms, curve, risk,
# km_fit, cox_strat, hr_pooled, hr_study, tau, landmarks.

# ---- Part A: arm summaries and non-proportional hazards ----------------------------

km_extra_code <- function(p) {
  code <- r"---(
# Hazard ratio, treatment vs control, from a Cox fit (NA when the model could not be fitted)
km_hr <- function(f) {
  s <- tryCatch(summary(f), error = function(e) NULL)
  na <- c(hr = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, se = NA_real_)
  if (!is.list(s) || is.null(s$conf.int)) return(na)
  cs <- s$coefficients
  if (!is.finite(cs[1, 1])) return(na)
  c(hr = unname(s$conf.int[1, "exp(coef)"]), lo = unname(s$conf.int[1, "lower .95"]),
    hi = unname(s$conf.int[1, "upper .95"]), p = unname(cs[1, ncol(cs)]), se = unname(cs[1, "se(coef)"]))
}
fit_cox <- function(formula, data) tryCatch(suppressWarnings(coxph(formula, data = data)), error = function(e) NULL)

# ---- Summary by arm: patients, events, person-time, rate, median follow-up (reverse Kaplan-Meier)
arm_summary <- data.frame(group = levels(ipd$group),
  patients = as.vector(table(ipd$group)),
  events = as.vector(tapply(ipd$status, ipd$group, sum)),
  person_time = as.vector(tapply(ipd$time, ipd$group, sum)))
arm_summary$rate_per_100 <- 100 * arm_summary$events / arm_summary$person_time
fu_fit <- survfit(Surv(time, 1 - status) ~ group, data = ipd)
arm_summary$median_followup <- unname(summary(fu_fit)$table[, "median"])
rate_ratio <- list(rr = (arm_summary$events[2] / arm_summary$person_time[2]) / (arm_summary$events[1] / arm_summary$person_time[1]),
                   se = sqrt(1 / arm_summary$events[2] + 1 / arm_summary$events[1]))
rate_ratio$lo <- rate_ratio$rr * exp(-1.96 * rate_ratio$se); rate_ratio$hi <- rate_ratio$rr * exp(1.96 * rate_ratio$se)

# ---- Absolute difference in survival at the chosen times
rd_tab <- NULL
if (length(landmarks) > 0) {
  s_ <- summary(km_fit, times = landmarks, extend = TRUE)
  g_ <- sub("^group=", "", as.character(s_$strata))
  rd_tab <- do.call(rbind, lapply(landmarks, function(tm) {
    i0 <- which(g_ == "control" & s_$time == tm); i1 <- which(g_ == "treatment" & s_$time == tm)
    d <- s_$surv[i1] - s_$surv[i0]; se <- sqrt(s_$std.err[i1]^2 + s_$std.err[i0]^2)
    data.frame(time = tm, difference = d, lower = d - 1.96 * se, upper = d + 1.96 * se)
  }))
}

# ---- Proportional hazards: test per model, and the log-log plot uses km_fit
ph_tab <- tryCatch({ z <- cox.zph(cox_strat); data.frame(chisq = z$table["GLOBAL", "chisq"], df = z$table["GLOBAL", "df"], p = z$table["GLOBAL", "p"]) },
                   error = function(e) NULL)
ph_study <- do.call(rbind, lapply(levels(ipd$study_id), function(s) {
  d <- ipd[ipd$study_id == s, ]
  pz <- tryCatch(cox.zph(coxph(Surv(time, status) ~ group, data = d))$table["GLOBAL", "p"], error = function(e) NA_real_)
  data.frame(study_id = s, ph_p = unname(pz))
}))

# ---- Landmark analysis: hazard ratio before and after a time point (curves restart at the landmark)
lm_time <- %%LANDMARK%%
if (is.na(lm_time) || lm_time <= 0 || lm_time >= max(ipd$time)) lm_time <- max(1, round(tau / 4))
early <- ipd; early$t_e <- pmin(early$time, lm_time); early$s_e <- ifelse(early$time > lm_time, 0L, early$status)
late <- ipd[ipd$time > lm_time, ]; late$t_l <- late$time - lm_time
late$study_id <- droplevels(late$study_id)
lm_early <- km_hr(fit_cox(Surv(t_e, s_e) ~ group + strata(study_id), early))
lm_late <- km_hr(fit_cox(Surv(t_l, status) ~ group + strata(study_id), late))
lm_fit_early <- survfit(Surv(t_e, s_e) ~ group, data = early)
lm_fit_late <- tryCatch(survfit(Surv(t_l, status) ~ group, data = late), error = function(e) NULL)
ev_ <- function(st, g) sum(st[g == "control"])
landmark_hr <- data.frame(
  period = c(paste0("0 to ", lm_time), paste0("After ", lm_time)),
  events_control = c(sum(early$s_e[early$group == "control"]), sum(late$status[late$group == "control"])),
  events_treatment = c(sum(early$s_e[early$group == "treatment"]), sum(late$status[late$group == "treatment"])),
  hr = c(lm_early[["hr"]], lm_late[["hr"]]), lo = c(lm_early[["lo"]], lm_late[["lo"]]),
  hi = c(lm_early[["hi"]], lm_late[["hi"]]), p = c(lm_early[["p"]], lm_late[["p"]]))

# ---- Hazard ratio by time interval (patients still at risk at the start of each interval)
cuts <- c(%%CUTS%%)
if (!length(cuts)) cuts <- unique(round(tau * c(1/3, 2/3)))
cuts <- sort(unique(cuts[cuts > 0 & cuts < max(ipd$time)]))
edges <- c(0, cuts, Inf)
piecewise <- do.call(rbind, lapply(seq_len(length(edges) - 1), function(i) {
  a <- edges[i]; b <- edges[i + 1]
  d <- ipd[ipd$time > a, ]
  d$start <- a; d$stop <- pmin(d$time, b); d$ev <- ifelse(d$time > b, 0L, d$status)
  r <- km_hr(fit_cox(Surv(start, stop, ev) ~ group + strata(study_id), d))
  data.frame(interval = if (is.infinite(b)) paste0("After ", a) else paste0(a, " to ", b), from = a, to = b,
             events_control = sum(d$ev[d$group == "control"]), events_treatment = sum(d$ev[d$group == "treatment"]),
             hr = r[["hr"]], lo = r[["lo"]], hi = r[["hi"]], p = r[["p"]])
}))
rownames(piecewise) <- NULL

# ---- Hazard ratio at every time: Cox model with a natural-spline time-varying effect
tv_df <- %%TVDF%%
tv <- NULL; tv_p <- NA_real_
evt <- ipd$time[ipd$status == 1]
if (length(evt) >= 30) {
  bk <- unname(quantile(evt, c(0.02, 0.98)))
  kn <- unname(quantile(evt, seq_len(tv_df - 1) / tv_df))
  ipd$gnum <- as.numeric(ipd$group == "treatment")
  tt_fun <- function(x, t, ...) splines::ns(t, knots = kn, Boundary.knots = bk) * x
  tv_fit <- tryCatch(suppressWarnings(coxph(Surv(time, status) ~ gnum + tt(gnum) + strata(study_id), data = ipd, tt = tt_fun)),
                     error = function(e) NULL)
  if (!is.null(tv_fit) && !anyNA(coef(tv_fit))) {
    b <- coef(tv_fit); V <- vcov(tv_fit)
    tgrid <- seq(bk[1], bk[2], length.out = 100)
    X <- cbind(1, splines::ns(tgrid, knots = kn, Boundary.knots = bk))
    lhr <- drop(X %*% b); se_ <- sqrt(rowSums((X %*% V) * X))
    tv <- data.frame(time = tgrid, hr = exp(lhr), lo = exp(lhr - 1.96 * se_), hi = exp(lhr + 1.96 * se_))
    ix <- 2:length(b)
    tv_p <- pchisq(drop(t(b[ix]) %*% solve(V[ix, ix]) %*% b[ix]), length(ix), lower.tail = FALSE)
  }
}
)---"
  code <- gsub("%%LANDMARK%%", if (is.na(p$landmark_time)) "NA_real_" else format(p$landmark_time), code, fixed = TRUE)
  code <- gsub("%%CUTS%%", paste(format(p$cuts), collapse = ", "), code, fixed = TRUE)
  gsub("%%TVDF%%", format(p$tv_df), code, fixed = TRUE)
}

# ---- Part B: sensitivity analyses -------------------------------------------------

km_sens_code <- function(p) {
  code <- r"---(
sens_rows <- list()
add_sens <- function(analysis, group, r) {
  sens_rows[[length(sens_rows) + 1]] <<- data.frame(analysis = analysis, group = group,
    hr = r[["hr"]], lo = r[["lo"]], hi = r[["hi"]], p = r[["p"]], stringsAsFactors = FALSE)
}
add_sens("Stratified by study (primary)", "Pooling model", c(hr = hr_pooled$hr, lo = hr_pooled$lo, hi = hr_pooled$hi, p = hr_pooled$p))
add_sens("Not stratified (all patients together)", "Pooling model", km_hr(fit_cox(Surv(time, status) ~ group, ipd)))
add_sens("Study as random effect (gamma frailty)", "Pooling model", km_hr(fit_cox(Surv(time, status) ~ group + frailty(study_id), ipd)))

# Two-stage: pool the study hazard ratios with a meta-analysis
two_stage <- function(te, se, lab) {
  ok <- is.finite(te) & is.finite(se)
  if (sum(ok) < 2) return(NULL)
  tryCatch(meta::metagen(TE = te[ok], seTE = se[ok], studlab = lab[ok], sm = "HR", common = TRUE, random = TRUE,
                         method.tau = "REML", method.random.ci = "HK", prediction = sum(ok) >= 3),
           error = function(e) NULL)
}
ts_row <- function(m, which) {
  if (is.null(m)) return(c(hr = NA, lo = NA, hi = NA, p = NA))
  c(hr = exp(m[[paste0("TE.", which)]]), lo = exp(m[[paste0("lower.", which)]]), hi = exp(m[[paste0("upper.", which)]]), p = m[[paste0("pval.", which)]])
}
m_rec <- two_stage(log(hr_study$effect), hr_study$se, hr_study$study_id)
add_sens("Two-stage, common effect (inverse variance)", "Pooling model", ts_row(m_rec, "common"))
add_sens("Two-stage, random effects (REML, Hartung-Knapp)", "Pooling model", ts_row(m_rec, "random"))
heterogeneity <- if (is.null(m_rec)) NULL else data.frame(k = m_rec$k, I2 = m_rec$I2, tau2 = m_rec$tau2, Q_p = m_rec$pval.Q,
  pred_lo = if (m_rec$k >= 3) exp(m_rec$lower.predict) else NA, pred_hi = if (m_rec$k >= 3) exp(m_rec$upper.predict) else NA)

# Reconstructed against published hazard ratios
trt <- arms[arms$group == "treatment", ]
pub_tab <- data.frame(study_id = hr_study$study_id, hr_reconstructed = hr_study$effect, rec_lo = hr_study$ci_lower,
                      rec_hi = hr_study$ci_upper, rec_se = hr_study$se, stringsAsFactors = FALSE)
trt <- trt[!is.na(trt$hr_published), , drop = FALSE]      # a study with several treatment arms: the first one that has a published hazard ratio
mi <- match(pub_tab$study_id, trt$study_id)
pub_tab$hr_published <- trt$hr_published[mi]; pub_tab$pub_lo <- trt$hr_ci_lower[mi]; pub_tab$pub_hi <- trt$hr_ci_upper[mi]
pub_tab$pub_se <- (log(pub_tab$pub_hi) - log(pub_tab$pub_lo)) / (2 * 1.96)
pub_tab$ratio_rec_to_pub <- pub_tab$hr_reconstructed / pub_tab$hr_published
n_pub <- sum(!is.na(pub_tab$hr_published))
if (n_pub > 0) {
  te_mix <- ifelse(is.na(pub_tab$hr_published), log(pub_tab$hr_reconstructed), log(pub_tab$hr_published))
  se_mix <- ifelse(is.na(pub_tab$hr_published), pub_tab$rec_se, pub_tab$pub_se)
  m_mix <- two_stage(te_mix, se_mix, pub_tab$study_id)
  add_sens(sprintf("Two-stage random effects, published HR where available (%d of %d)", n_pub, nrow(pub_tab)), "Source of study hazard ratios", ts_row(m_mix, "random"))
  if (n_pub >= 2) {
    m_pub <- two_stage(log(pub_tab$hr_published), pub_tab$pub_se, pub_tab$study_id)
    add_sens(sprintf("Two-stage random effects, published HR only (%d studies)", n_pub), "Source of study hazard ratios", ts_row(m_pub, "random"))
  }
}

# Shorter follow-up: censor everyone at a chosen time
truncs <- c(%%TRUNCS%%)
if (!length(truncs)) truncs <- unique(round(tau * c(0.5, 0.75)))
truncs <- sort(unique(truncs[truncs > 0 & truncs < max(ipd$time)]))
for (tr in truncs) {
  d <- ipd; d$status <- ifelse(d$time > tr, 0L, d$status); d$time <- pmin(d$time, tr)
  add_sens(paste0("Follow-up cut at ", tr), "Follow-up", km_hr(fit_cox(Surv(time, status) ~ group + strata(study_id), d)))
}
sens_tab <- do.call(rbind, sens_rows)
sens_all <- sens_tab

# Leave one study out (hazard ratio from the stratified Cox model)
loo_tab <- NULL
if (nlevels(ipd$study_id) >= 3) {
  loo_tab <- do.call(rbind, lapply(levels(ipd$study_id), function(s) {
    d <- ipd[ipd$study_id != s, ]; d$study_id <- droplevels(d$study_id)
    r <- km_hr(fit_cox(Surv(time, status) ~ group + strata(study_id), d))
    data.frame(omitted = s, hr = r[["hr"]], lo = r[["lo"]], hi = r[["hi"]], p = r[["p"]])
  }))
}
)---"
  gsub("%%TRUNCS%%", paste(format(p$truncs), collapse = ", "), code, fixed = TRUE)
}

# Re-run the reconstruction with less information, to see how much the result depends on it.
km_variants_code <- function(digit_B = 30, digit_noise = 0.005) {
  code <- r"---(
km_recon_ipd <- function(curve, risk, arms, use_events = TRUE, use_risk = TRUE) {
  out <- list()
  for (i in seq_len(nrow(arms))) {
    s <- arms$study_id[i]; a <- arms$arm[i]; g <- arms$group[i]
    cv <- curve[curve$study_id == s & curve$arm == a, c("time", "survival")]
    cv <- cv[order(cv$time), ]
    if (cv$time[1] > 0) cv <- rbind(data.frame(time = 0, survival = 1), cv)
    cv$survival <- pmin(1, cummin(cv$survival))
    rk <- risk[risk$study_id == s & risk$arm == a, c("time", "n_risk")]
    rk <- rk[order(rk$time), ]
    n <- arms$n_total[i]; ev <- if (use_events) arms$total_events[i] else NA_real_
    if (is.na(n) && nrow(rk) > 0 && rk$time[1] == 0) n <- rk$n_risk[1]
    if (use_risk && nrow(rk) >= 2) {
      if (rk$time[1] > 0 && !is.na(n)) rk <- rbind(data.frame(time = 0, n_risk = n), rk)
      pre <- IPDfromKM::preprocess(dat = cv, trisk = rk$time, nrisk = rk$n_risk, totalpts = if (is.na(n)) NULL else n, maxy = 1)
    } else {
      pre <- IPDfromKM::preprocess(dat = cv, totalpts = if (is.na(n)) NULL else n, maxy = 1)
    }
    rec <- IPDfromKM::getIPD(prep = pre, armID = as.numeric(g == "treatment"), tot.events = if (is.na(ev)) NULL else ev)
    out[[i]] <- data.frame(study_id = s, arm = a, group = g, time = rec$IPD$time, status = rec$IPD$status)
  }
  d <- do.call(rbind, out)
  d$group <- factor(d$group, levels = c("control", "treatment"))
  d$study_id <- factor(d$study_id, levels = unique(arms$study_id))
  d
}
variant_rows <- list()
for (v in list(list("Reconstruction ignoring reported total events", FALSE, TRUE),
               list("Reconstruction from the curve only (no numbers at risk)", TRUE, FALSE))) {
  d <- tryCatch(km_recon_ipd(curve, risk, arms, use_events = v[[2]], use_risk = v[[3]]), error = function(e) NULL)
  r <- if (is.null(d)) c(hr = NA, lo = NA, hi = NA, p = NA) else km_hr(fit_cox(Surv(time, status) ~ group + strata(study_id), d))
  variant_rows[[length(variant_rows) + 1]] <- data.frame(analysis = v[[1]], group = "Reconstruction", hr = r[["hr"]], lo = r[["lo"]], hi = r[["hi"]], p = r[["p"]], stringsAsFactors = FALSE)
}
variants_tab <- do.call(rbind, variant_rows)
sens_all <- rbind(sens_tab, variants_tab)

# Digitizing error: repeat the reconstruction after adding small random reading errors to every digitized survival value
digit_B <- %%DIGIT_B%%; digit_noise <- %%DIGIT_NOISE%%
digit_tab <- NULL
if (digit_B >= 5) {
  set.seed(2024)
  digit_loghr <- rep(NA_real_, digit_B)
  for (b in seq_len(digit_B)) {
    cb <- curve
    cb$survival <- pmin(1, pmax(0, cb$survival + rnorm(nrow(cb), 0, digit_noise)))
    cb$survival[cb$time == 0] <- 1
    d <- tryCatch(km_recon_ipd(cb, risk, arms), error = function(e) NULL)
    if (!is.null(d)) digit_loghr[b] <- tryCatch(unname(coef(coxph(Surv(time, status) ~ group + strata(study_id), data = d))[1]), error = function(e) NA_real_)
  }
  digit_loghr <- digit_loghr[is.finite(digit_loghr)]
  if (length(digit_loghr) >= 5) {
    se_base <- (log(hr_pooled$hi) - log(hr_pooled$lo)) / (2 * 1.96)
    se_total <- sqrt(se_base^2 + var(digit_loghr))
    digit_tab <- data.frame(runs = length(digit_loghr), noise = digit_noise, hr_base = hr_pooled$hr,
      hr_mean = exp(mean(digit_loghr)), sd_loghr = sd(digit_loghr),
      hr_lo_2.5 = unname(exp(quantile(digit_loghr, 0.025))), hr_hi_97.5 = unname(exp(quantile(digit_loghr, 0.975))),
      se_loghr_base = se_base, se_loghr_total = se_total,
      ci_total_lo = exp(log(hr_pooled$hr) - 1.96 * se_total), ci_total_hi = exp(log(hr_pooled$hr) + 1.96 * se_total))
  }
}
)---"
  code <- gsub("%%DIGIT_B%%", format(as.integer(digit_B)), code, fixed = TRUE)
  gsub("%%DIGIT_NOISE%%", format(digit_noise), code, fixed = TRUE)
}

# ---- Plot calls (shared by app and script) -----------------------------------------

km_extra_calls <- function(p) {
  nm_tx <- q(if (nzchar(p$name_tx)) p$name_tx else "Treatment")
  nm_ct <- q(if (nzchar(p$name_ctrl)) p$name_ctrl else "Control")
  unit <- q(if (nzchar(p$time_unit)) p$time_unit else "months")
  list(
    landmark = sprintf("km_plot_landmark(lm_fit_early, lm_fit_late, lm_time, lm_early, lm_late, name_tx = %s, name_ctrl = %s, time_unit = %s, display = %s, conf = %s)",
                       nm_tx, nm_ct, unit, q(p$display), rbool(p$conf)),
    tv = sprintf("km_plot_tvhr(tv, hr_pooled, tv_p, time_unit = %s)", unit),
    loglog = sprintf("km_plot_loglog(km_fit, name_tx = %s, name_ctrl = %s, time_unit = %s)", nm_tx, nm_ct, unit),
    sens = "km_plot_rows(sens_all$analysis, sens_all$hr, sens_all$lo, sens_all$hi, group = sens_all$group, ref = hr_pooled$hr, title = \"Sensitivity of the pooled hazard ratio\")",
    loo = "km_plot_rows(paste(\"Omitting\", loo_tab$omitted), loo_tab$hr, loo_tab$lo, loo_tab$hi, ref = hr_pooled$hr, title = \"Leave one study out\")",
    pub = "km_plot_published(pub_tab)")
}

# Figure height (inches) for the row-style plots
km_rows_height <- function(n_rows, n_groups = 0) 1.9 + 0.3 * (n_rows + n_groups)

# ---- Plot functions (self-contained base graphics) ---------------------------------

km_plot_rows <- function(lab, hr, lo, hi, group = NULL, col = NULL, ref = NULL, title = NULL,
                         xlab = "Hazard ratio, log scale (below 1 favours treatment)") {
  n <- length(lab); idx <- seq_len(n)
  if (is.null(col)) col <- rep("#2F5D8A", n)
  if (is.null(group)) {
    disp <- data.frame(type = rep("row", n), label = lab, i = idx, stringsAsFactors = FALSE)
  } else {
    disp <- data.frame(type = character(0), label = character(0), i = integer(0), stringsAsFactors = FALSE)
    for (g in unique(group)) {
      disp <- rbind(disp, data.frame(type = "header", label = g, i = NA_integer_, stringsAsFactors = FALSE),
                    data.frame(type = "row", label = lab[group == g], i = idx[group == g], stringsAsFactors = FALSE))
    }
  }
  m <- nrow(disp); ypos <- m:1
  vals <- c(lo, hi, hr, ref, 1); vals <- vals[is.finite(vals) & vals > 0]
  lx <- log(c(min(max(min(vals), 0.05), 0.9), max(min(max(vals), 20), 1.1)))
  pad <- 0.06 * diff(lx); xlim <- exp(lx + c(-pad, pad))
  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  graphics::par(mar = c(4.6, min(24, 1 + 0.42 * max(nchar(disp$label))), 2.8, 9.5), las = 1, mgp = c(2.6, 0.7, 0))
  graphics::plot(NA, xlim = xlim, ylim = c(0.4, m + 0.6), log = "x", yaxt = "n", xaxt = "n", xlab = xlab, ylab = "", bty = "n")
  ticks <- c(0.1, 0.2, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3, 4, 5, 10, 20)
  ticks <- ticks[ticks >= xlim[1] & ticks <= xlim[2]]
  if (length(ticks) < 4) ticks <- sort(unique(c(1, signif(exp(seq(log(xlim[1]), log(xlim[2]), length.out = 5)), 2))))
  graphics::axis(1, at = ticks, labels = ticks)
  graphics::abline(v = 1, col = "grey35")
  if (!is.null(ref) && is.finite(ref)) graphics::abline(v = ref, lty = 3, col = "#B23A48")
  for (k in seq_len(m)) {
    if (disp$type[k] == "header") {
      graphics::mtext(disp$label[k], side = 2, at = ypos[k], line = 0.4, adj = 1, font = 2, cex = 0.85, las = 1)
      next
    }
    i <- disp$i[k]
    graphics::mtext(disp$label[k], side = 2, at = ypos[k], line = 0.4, adj = 1, cex = 0.82, las = 1)
    if (is.finite(hr[i])) {
      if (is.finite(lo[i]) && is.finite(hi[i]))
        graphics::segments(max(lo[i], xlim[1]), ypos[k], min(hi[i], xlim[2]), ypos[k], col = col[i], lwd = 2)
      graphics::points(hr[i], ypos[k], pch = 15, col = col[i], cex = 1.2)
      graphics::mtext(sprintf("%.2f (%.2f to %.2f)", hr[i], lo[i], hi[i]), side = 4, at = ypos[k], line = 0.4, adj = 0, cex = 0.8, las = 1)
    } else {
      graphics::mtext("not estimable", side = 4, at = ypos[k], line = 0.4, adj = 0, cex = 0.8, las = 1, col = "grey40")
    }
  }
  graphics::mtext("HR (95% CI)", side = 4, at = m + 0.55, line = 0.4, adj = 0, font = 2, cex = 0.8, las = 1)
  if (!is.null(title)) graphics::mtext(title, side = 3, line = 1.2, adj = 0, font = 2, cex = 0.9)
  invisible(NULL)
}

km_plot_published <- function(pub_tab) {
  d <- pub_tab[!is.na(pub_tab$hr_published), ]
  if (!nrow(d)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "No published hazard ratios were entered.\nAdd hr_published, hr_ci_lower and hr_ci_upper to the arms file (treatment row).", cex = 0.9)
    return(invisible(NULL))
  }
  lab <- c(rbind(rep("Reconstructed", nrow(d)), rep("Published", nrow(d))))
  grp <- rep(d$study_id, each = 2)
  hr <- c(rbind(d$hr_reconstructed, d$hr_published))
  lo <- c(rbind(d$rec_lo, d$pub_lo)); hi <- c(rbind(d$rec_hi, d$pub_hi))
  col <- rep(c("#2F5D8A", "#B23A48"), nrow(d))
  km_plot_rows(lab, hr, lo, hi, group = grp, col = col, title = "Reconstructed against published hazard ratios")
}

km_plot_landmark <- function(fit_e, fit_l, lm_time, hr_e, hr_l, name_tx = "Treatment", name_ctrl = "Control",
                             time_unit = "months", display = "survival", conf = TRUE, xmax = NA) {
  cols <- c(control = "#B23A48", treatment = "#2F5D8A")
  nm <- c(control = name_ctrl, treatment = name_tx)
  flip <- display == "cumulative"
  tf <- function(y) if (flip) 1 - y else y
  steps <- function(t, y) { n <- length(t); list(x = c(t[1], rep(t[-1], each = 2)), y = c(rep(y[-n], each = 2), y[n])) }
  pfmt <- function(p) if (is.na(p)) "p n/a" else if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
  pieces <- function(fit) {
    if (is.null(fit) || is.null(fit$strata)) return(list())
    g <- sub("^group=", "", names(fit$strata)); ends <- cumsum(fit$strata); starts <- c(1, utils::head(ends, -1) + 1)
    stats::setNames(lapply(seq_along(g), function(k) {
      i <- starts[k]:ends[k]
      list(t = c(0, fit$time[i]), s = c(1, fit$surv[i]), lo = c(1, fit$lower[i]), up = c(1, fit$upper[i]))
    }), g)
  }
  pe <- pieces(fit_e); pl <- pieces(fit_l)
  tmax <- if (is.na(xmax)) lm_time + max(c(0, vapply(pl, function(z) max(z$t), 0))) else xmax
  draw <- function(z, off, endt, col) {
    lo <- z$lo; up <- z$up; lo[is.na(lo)] <- z$s[is.na(lo)]; up[is.na(up)] <- z$s[is.na(up)]
    tt <- c(z$t + off, endt); keep <- tt <= tmax; tt <- tt[keep]
    ext <- function(v) { v <- c(v, utils::tail(v, 1)); v[keep] }
    ss <- ext(z$s); lo2 <- ext(lo); up2 <- ext(up)
    if (conf) {
      lw <- if (flip) 1 - up2 else lo2; hg <- if (flip) 1 - lo2 else up2
      a <- steps(tt, lw); b <- steps(tt, hg)
      graphics::polygon(c(b$x, rev(a$x)), c(b$y, rev(a$y)), col = grDevices::adjustcolor(col, 0.15), border = NA)
    }
    s <- steps(tt, tf(ss)); graphics::lines(s$x, s$y, col = col, lwd = 2.6)
  }
  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  graphics::par(mar = c(4.4, 6.5, 4.6, 1.5), las = 1, mgp = c(3.2, 0.8, 0))
  graphics::plot(NA, xlim = c(0, tmax), ylim = c(0, 1), xaxs = "i", bty = "l",
                 xlab = paste0("Time (", time_unit, ")"),
                 ylab = if (flip) "Cumulative incidence (restarts at the landmark)" else "Survival probability (restarts at the landmark)")
  for (g in names(pe)) draw(pe[[g]], 0, lm_time, cols[[g]])
  for (g in names(pl)) draw(pl[[g]], lm_time, tmax, cols[[g]])
  graphics::abline(v = lm_time, lty = 2, col = "grey35")
  graphics::legend(if (flip) "topleft" else "bottomleft", legend = nm[c("treatment", "control")], col = cols[c("treatment", "control")], lwd = 2.6, bty = "n", cex = 0.9)
  lab_e <- sprintf("0 to %s %s: HR %.2f (95%% CI %.2f to %.2f), %s", format(lm_time), time_unit, hr_e[["hr"]], hr_e[["lo"]], hr_e[["hi"]], pfmt(hr_e[["p"]]))
  lab_l <- sprintf("After %s %s: HR %.2f (95%% CI %.2f to %.2f), %s", format(lm_time), time_unit, hr_l[["hr"]], hr_l[["lo"]], hr_l[["hi"]], pfmt(hr_l[["p"]]))
  graphics::mtext(lab_e, side = 3, line = 2.2, adj = 0, font = 2, cex = 0.85)
  graphics::mtext(lab_l, side = 3, line = 0.9, adj = 0, font = 2, cex = 0.85)
  invisible(NULL)
}

km_plot_tvhr <- function(tv, hr_pooled, tv_p, time_unit = "months") {
  if (is.null(tv)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, "The time-varying model could not be fitted.\nIt needs at least 30 events.", cex = 0.9)
    return(invisible(NULL))
  }
  lo <- pmax(tv$lo, 0.05); hi <- pmin(tv$hi, 20)
  yl <- range(c(lo, hi, 1, tv$hr))
  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  graphics::par(mar = c(4.4, 5.2, 3.4, 1.5), las = 1, mgp = c(3, 0.8, 0))
  graphics::plot(NA, xlim = range(tv$time), ylim = yl, log = "y", xaxs = "i", yaxt = "n", bty = "l",
                 xlab = paste0("Time (", time_unit, ")"), ylab = "Hazard ratio (treatment vs control)")
  ticks <- c(0.1, 0.2, 0.25, 0.33, 0.5, 0.67, 1, 1.5, 2, 3, 4, 5, 10, 20); ticks <- ticks[ticks >= yl[1] & ticks <= yl[2]]
  graphics::axis(2, at = ticks, labels = ticks)
  graphics::polygon(c(tv$time, rev(tv$time)), c(hi, rev(lo)), col = grDevices::adjustcolor("#2F5D8A", 0.2), border = NA)
  graphics::abline(h = 1, col = "grey35")
  graphics::abline(h = hr_pooled$hr, lty = 3, col = "#B23A48")
  graphics::lines(tv$time, tv$hr, col = "#2F5D8A", lwd = 2.8)
  graphics::legend("topright", legend = c("Hazard ratio over time", sprintf("Single pooled HR (%.2f)", hr_pooled$hr)),
                   col = c("#2F5D8A", "#B23A48"), lty = c(1, 3), lwd = c(2.8, 1.5), bty = "n", cex = 0.85)
  pt <- if (is.na(tv_p)) "n/a" else if (tv_p < 0.001) "< 0.001" else sprintf("= %.3f", tv_p)
  graphics::mtext(sprintf("Hazard ratio over time. Test that the effect is constant: p %s", pt), side = 3, line = 1, adj = 0, font = 2, cex = 0.85)
  invisible(NULL)
}

km_plot_loglog <- function(fit, name_tx = "Treatment", name_ctrl = "Control", time_unit = "months") {
  cols <- c(control = "#B23A48", treatment = "#2F5D8A")
  nm <- c(control = name_ctrl, treatment = name_tx)
  grp <- sub("^group=", "", names(fit$strata)); ends <- cumsum(fit$strata); starts <- c(1, utils::head(ends, -1) + 1)
  dat <- lapply(seq_along(grp), function(k) {
    i <- starts[k]:ends[k]; ok <- fit$time[i] > 0 & fit$surv[i] > 0 & fit$surv[i] < 1
    list(x = log(fit$time[i][ok]), y = log(-log(fit$surv[i][ok])))
  })
  xr <- range(unlist(lapply(dat, `[[`, "x"))); yr <- range(unlist(lapply(dat, `[[`, "y")))
  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  graphics::par(mar = c(4.4, 5.2, 3.4, 1.5), las = 1, mgp = c(3, 0.8, 0))
  graphics::plot(NA, xlim = xr, ylim = yr, bty = "l", xlab = paste0("log(time), time in ", time_unit), ylab = "log(-log(survival))")
  for (k in seq_along(grp)) graphics::lines(dat[[k]]$x, dat[[k]]$y, type = "s", col = cols[[grp[k]]], lwd = 2.4)
  graphics::legend("bottomright", legend = nm[grp], col = cols[grp], lwd = 2.4, bty = "n", cex = 0.9)
  graphics::mtext("Proportional hazards check: roughly parallel curves support a constant hazard ratio", side = 3, line = 1, adj = 0, font = 2, cex = 0.8)
  invisible(NULL)
}

# Names of every plot function the exported script must contain
KM_PLOT_FUNCTIONS <- c("km_plot", "km_plot_by_study", "km_plot_landmark", "km_plot_tvhr", "km_plot_loglog",
                       "km_plot_rows", "km_plot_published")
