# Kaplan-Meier reconstruction: column rules, templates, validation, the R code
# that does the analysis (run by the app AND exported as a script), and plot functions.
#
# Method: Guyot et al. (2012), implemented in the IPDfromKM package. The pooled
# curve is built from the reconstructed patients of all studies (one-stage), and
# the hazard ratio comes from a Cox model stratified by study.

# ---- File specification ----------------------------------------------------

KM_COLS <- list(
  curve = c("study_id", "arm", "time", "survival"),
  risk  = c("study_id", "arm", "time", "n_risk"),
  arms  = c("study_id", "arm", "group", "n_total", "total_events", "hr_published", "hr_ci_lower", "hr_ci_upper")
)
# Optional columns: a missing column is filled with NA instead of being an error.
KM_OPTIONAL <- list(curve = character(0), risk = character(0),
                    arms = c("total_events", "hr_published", "hr_ci_lower", "hr_ci_upper"))
KM_REQUIRED <- list(
  curve = KM_COLS$curve, risk = KM_COLS$risk,
  arms = c("study_id", "arm", "group", "n_total")
)
KM_TITLES <- c(curve = "Curve points", risk = "Numbers at risk", arms = "Arms")

KM_ALIASES <- list(
  study_id = c("studyid", "study", "studyname", "trial", "trialname"),
  arm = c("arm", "armname", "armlabel", "treatmentarm"),
  time = c("time", "t", "followup", "months", "years", "timemonths", "timeyears"),
  survival = c("survival", "surv", "s", "survivalprobability", "probability", "survivalrate", "st"),
  n_risk = c("nrisk", "atrisk", "numberatrisk", "nar", "risk"),
  group = c("group", "role", "armtype", "groupname"),
  n_total = c("ntotal", "n", "total", "npatients", "patients", "samplesize", "randomized"),
  total_events = c("totalevents", "events", "nevents", "eventstotal", "deaths", "ntotalevents"),
  hr_published = c("hrpublished", "publishedhr", "reportedhr", "hr", "hazardratio"),
  hr_ci_lower = c("hrcilower", "hrlower", "hrlo", "hrlcl", "publishedlower", "cilower"),
  hr_ci_upper = c("hrciupper", "hrupper", "hrhi", "hrucl", "publishedupper", "ciupper")
)

KM_TX_WORDS <- c("treatment", "tx", "experimental", "intervention", "1")
KM_CT_WORDS <- c("control", "ctrl", "comparator", "placebo", "0")

# Header-only CSV, UTF-8 with BOM so Excel shows accents correctly.
km_write_template <- function(file, which, sep = ",") {
  con <- file(file, open = "wb"); on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(paste0(paste(KM_COLS[[which]], collapse = sep), "\r\n")), con)
}

# Match a user's file to the standard column names. Returns list(data, orig) or list(error).
km_standardize <- function(d, which) {
  keys <- KM_COLS[[which]]
  g <- guess_columns(keys, names(d), aliases = KM_ALIASES)
  miss <- KM_REQUIRED[[which]][!nzchar(g[KM_REQUIRED[[which]]])]
  if (length(miss)) {
    return(list(error = sprintf("The %s file is missing: %s. It needs the columns %s.",
                                tolower(KM_TITLES[[which]]), paste(miss, collapse = ", "),
                                paste(KM_REQUIRED[[which]], collapse = ", "))))
  }
  out <- lapply(keys, function(k) if (nzchar(g[[k]])) as.character(d[[g[[k]]]]) else rep(NA_character_, nrow(d)))
  names(out) <- keys
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out$study_id <- trimws(out$study_id); out$arm <- trimws(out$arm)
  list(data = out, orig = g)
}

# ---- Example data (simulated; not real studies) ----------------------------

km_example <- function() {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  set.seed(2026)
  spec <- data.frame(study_id = c("Trial A 2012", "Trial B 2015", "Trial C 2018"),
                     n = c(250, 400, 180), follow = c(48, 60, 36), by = c(12, 12, 6),
                     hr = c(0.75, 0.82, 0.68), scale = c(55, 70, 45), stringsAsFactors = FALSE)
  curve <- risk <- arms <- list(); truth <- list()
  for (i in seq_len(nrow(spec))) {
    both <- list()
    s <- spec[i, ]
    for (g in c("control", "treatment")) {
      h <- if (g == "treatment") s$hr else 1
      u <- runif(s$n); t <- s$scale * (-log(u) / h)^(1 / 1.2)
      cens <- pmin(s$follow, rexp(s$n, 1 / 120))
      tm <- pmin(t, cens); st <- as.integer(t <= cens)
      both[[g]] <- data.frame(time = tm, status = st, tx = as.numeric(g == "treatment"))
      f <- survival::survfit(survival::Surv(tm, st) ~ 1)
      tt <- seq(0, s$follow, length.out = 36)
      sv <- summary(f, times = tt, extend = TRUE)$surv
      arm_name <- if (g == "treatment") "Intervention" else "Standard care"
      curve[[length(curve) + 1]] <- data.frame(study_id = s$study_id, arm = arm_name,
        time = round(tt, 1), survival = round(sv, 3), stringsAsFactors = FALSE)
      rt <- seq(0, s$follow, by = s$by)
      risk[[length(risk) + 1]] <- data.frame(study_id = s$study_id, arm = arm_name, time = rt,
        n_risk = vapply(rt, function(x) sum(tm >= x), numeric(1)), stringsAsFactors = FALSE)
      arms[[length(arms) + 1]] <- data.frame(study_id = s$study_id, arm = arm_name, group = g,
        n_total = s$n, total_events = sum(st), hr_published = NA_real_, hr_ci_lower = NA_real_,
        hr_ci_upper = NA_real_, stringsAsFactors = FALSE)
    }
    bb <- do.call(rbind, both)
    cf <- survival::coxph(survival::Surv(time, status) ~ tx, data = bb)
    truth[[s$study_id]] <- unname(exp(coef(cf)))
    ci <- exp(stats::confint(cf))
    # "Published" hazard ratio: only trials A and C report one (typed on the treatment row)
    if (i != 2) {
      k <- length(arms) ; a <- arms[[k]]
      a$hr_published[a$group == "treatment"] <- round(unname(exp(coef(cf))), 2)
      a$hr_ci_lower[a$group == "treatment"] <- round(ci[1, 1], 2)
      a$hr_ci_upper[a$group == "treatment"] <- round(ci[1, 2], 2)
      arms[[k]] <- a
    }
  }
  out <- list(curve = do.call(rbind, curve), risk = do.call(rbind, risk), arms = do.call(rbind, arms))
  attr(out, "observed_hr") <- truth
  out
}

# ---- Validation -------------------------------------------------------------

km_group_of <- function(x) {
  x <- tolower(trimws(x))
  ifelse(x %in% KM_TX_WORDS, "treatment", ifelse(x %in% KM_CT_WORDS, "control", NA_character_))
}

# curve, risk, arms: standardized character data frames. Returns list(errors, warnings).
km_validate <- function(curve, risk, arms, p) {
  err <- character(0); wrn <- character(0)
  key <- function(d) paste(d$study_id, d$arm, sep = " / ")

  # --- arms file
  if (!nrow(arms)) return(list(errors = "The arms file has no rows.", warnings = wrn))
  grp <- km_group_of(arms$group)
  bad <- which(is.na(grp))
  if (length(bad)) err <- c(err, fmt_rows(bad, key(arms), "Group must be 'treatment' or 'control'"))
  if (anyDuplicated(key(arms))) err <- c(err, paste0("Each study and arm can appear only once in the arms file. Repeated: ",
                                                      paste(unique(key(arms)[duplicated(key(arms))]), collapse = ", "), "."))
  if (!length(bad)) {
    cnt <- table(factor(arms$study_id, levels = unique(arms$study_id)), factor(grp, levels = c("treatment", "control")))
    wrong <- rownames(cnt)[cnt[, "treatment"] < 1 | cnt[, "control"] < 1]
    if (length(wrong)) err <- c(err, paste0("Each study needs at least one treatment arm and one control arm. Check: ",
                                            paste(utils::head(wrong, 5), collapse = ", "), ". For multi-arm trials, keep the arms you want to compare (several arms on one side are pooled)."))
  }
  ntot <- as_num(arms$n_total); ev <- as_num(arms$total_events)
  nn <- which(is.na(ntot) & !is.na(arms$n_total)); if (length(nn)) err <- c(err, fmt_rows(nn, key(arms), "n_total is not a number"))
  ne <- which(is.na(ev) & !is.na(arms$total_events)); if (length(ne)) err <- c(err, fmt_rows(ne, key(arms), "total_events is not a number"))
  i <- which(ntot <= 0); if (length(i)) err <- c(err, fmt_rows(i, key(arms), "n_total must be greater than 0"))
  i <- which(!is.na(ev) & !is.na(ntot) & ev > ntot); if (length(i)) err <- c(err, fmt_rows(i, key(arms), "total_events exceeds n_total"))
  i <- which(!is.na(ev) & ev < 0); if (length(i)) err <- c(err, fmt_rows(i, key(arms), "total_events cannot be negative"))
  hp <- as_num(arms$hr_published); hl <- as_num(arms$hr_ci_lower); hu <- as_num(arms$hr_ci_upper)
  anyh <- which(!is.na(hp) | !is.na(hl) | !is.na(hu))
  if (length(anyh)) {
    inc <- anyh[is.na(hp[anyh]) | is.na(hl[anyh]) | is.na(hu[anyh])]
    if (length(inc)) err <- c(err, fmt_rows(inc, key(arms), "Published hazard ratio needs all three values: hr_published, hr_ci_lower, hr_ci_upper"))
    bad_h <- anyh[!is.na(hp[anyh]) & !is.na(hl[anyh]) & !is.na(hu[anyh]) & (hp[anyh] <= 0 | hl[anyh] <= 0 | hu[anyh] <= 0 | hl[anyh] > hp[anyh] | hu[anyh] < hp[anyh])]
    if (length(bad_h)) err <- c(err, fmt_rows(bad_h, key(arms), "Published hazard ratio must be positive and lie inside its confidence interval"))
    wrong_row <- anyh[!is.na(grp[anyh]) & grp[anyh] != "treatment"]
    if (length(wrong_row)) wrn <- c(wrn, fmt_rows(wrong_row, key(arms), "Published hazard ratio found on a control row; it is read from the treatment row of each study only"))
  }
  if (all(is.na(ev))) wrn <- c(wrn, "No total events given. The reconstruction works without them but is a little less accurate.")
  else if (anyNA(ev)) wrn <- c(wrn, "Some arms have no total events; those reconstructions are a little less accurate.")

  # --- curve file
  tm <- as_num(curve$time); sv <- as_num(curve$survival)
  i <- which((is.na(tm) & !is.na(curve$time)) | (is.na(sv) & !is.na(curve$survival)))
  if (length(i)) err <- c(err, fmt_rows(i, key(curve), "Time or survival is not a number"))
  ok <- !is.na(tm) & !is.na(sv)
  if (any(!ok) && !length(i)) wrn <- c(wrn, fmt_rows(which(!ok), key(curve), "Skipped curve rows with missing values"))
  i <- which(ok & tm < 0); if (length(i)) err <- c(err, fmt_rows(i, key(curve), "Time cannot be negative"))
  sv_use <- if (p$scale == "percent") sv / 100 else sv
  i <- which(ok & (sv_use > 1.0001 | sv_use < -0.0001))
  if (length(i)) err <- c(err, if (p$scale == "proportion")
    "Survival values are above 1. If they are percentages, set 'Survival values' to Percent in step 3."
    else fmt_rows(i, key(curve), "Survival must be between 0 and 100"))
  if (p$scale == "percent" && any(ok) && max(sv[ok]) <= 1)
    wrn <- c(wrn, "Every survival value is 1 or below although the scale is Percent. If they are proportions, switch the scale.")

  if (!length(err)) {
    ak <- key(arms)
    for (k in seq_len(nrow(arms))) {
      idx <- which(key(curve) == ak[k] & ok)
      if (length(idx) < 5) { err <- c(err, sprintf("%s has only %d curve points. At least 5 are needed; 20 or more is better.", ak[k], length(idx))); next }
      if (length(idx) < 30) wrn <- c(wrn, sprintf("%s has %d curve points. With fewer than 30 the reconstructed hazard ratio can be off by several percent (about 6%% with 12 points in our tests, 1 to 3%% with 40).", ak[k], length(idx)))
      o <- idx[order(tm[idx])]; s2 <- sv_use[o]
      rise <- max(c(0, diff(s2)))
      if (rise > 0.03) wrn <- c(wrn, sprintf("%s: survival rises by up to %.1f points along the curve. Check the digitized points.", ak[k], 100 * rise))
      if (tm[o[1]] > 0.02 * max(tm[o])) wrn <- c(wrn, sprintf("%s: the curve does not start at time 0; a point at time 0 with survival 100%% is added.", ak[k]))
    }
    extra <- setdiff(unique(key(curve)), ak)
    if (length(extra)) wrn <- c(wrn, paste0("Curve rows ignored because the arm is not in the arms file: ", paste(utils::head(extra, 4), collapse = ", "), "."))
  }

  # --- risk file
  rt <- as_num(risk$time); rn <- as_num(risk$n_risk)
  i <- which((is.na(rt) & !is.na(risk$time)) | (is.na(rn) & !is.na(risk$n_risk)))
  if (length(i)) err <- c(err, fmt_rows(i, key(risk), "Time or n_risk is not a number"))
  if (!length(err)) {
    ak <- key(arms)
    no_risk <- ak[!(ak %in% key(risk)[!is.na(rt) & !is.na(rn)])]
    if (length(no_risk)) wrn <- c(wrn, paste0("No numbers at risk for: ", paste(utils::head(no_risk, 4), collapse = ", "),
                                              ". Those arms are reconstructed from the curve alone, which is less accurate."))
    for (k in seq_len(nrow(arms))) {
      idx <- which(key(risk) == ak[k] & !is.na(rt) & !is.na(rn))
      if (length(idx) < 2) next
      o <- idx[order(rt[idx])]
      if (any(diff(rn[o]) > 0)) wrn <- c(wrn, sprintf("%s: numbers at risk increase over time. Check the table.", ak[k]))
      if (rt[o[1]] == 0 && !is.na(ntot[k]) && rn[o[1]] != ntot[k])
        wrn <- c(wrn, sprintf("%s: number at risk at time 0 (%s) differs from n_total (%s).", ak[k], rn[o[1]], ntot[k]))
    }
    extra <- setdiff(unique(key(risk)), ak)
    if (length(extra)) wrn <- c(wrn, paste0("Risk-table rows ignored because the arm is not in the arms file: ", paste(utils::head(extra, 4), collapse = ", "), "."))
    miss_n <- which(is.na(ntot))
    for (k in miss_n) {
      idx <- which(key(risk) == ak[k] & !is.na(rt) & !is.na(rn) & rt == 0)
      if (!length(idx)) err <- c(err, sprintf("%s has no n_total and no number at risk at time 0.", ak[k]))
    }
  }
  list(errors = err, warnings = wrn)
}

# ---- Analysis code (shared by the app and the exported script) ---------------

km_params <- function(input) {
  nums <- function(x) {
    v <- suppressWarnings(as.numeric(strsplit(gsub(",", " ", if (length(x) == 1 && !is.na(x)) x else ""), "[[:space:];]+")[[1]]))
    utils::head(unique(v[is.finite(v) & v > 0]), 8)
  }
  num1 <- function(x, lo = -Inf, hi = Inf) { v <- suppressWarnings(as.numeric(x)); if (length(v) != 1 || is.na(v) || v < lo || v > hi) NA_real_ else v }
  list(
    scale = pick(input$scale, c("proportion", "percent")),
    curve_type = pick(input$curve_type, c("survival", "cumulative")),
    display = pick(input$display, c("survival", "cumulative")),
    outcome = txt(input$outcome, 80),
    time_unit = txt(input$time_unit, 20), name_tx = txt(input$name_tx, 40), name_ctrl = txt(input$name_ctrl, 40),
    conf = flag(input$conf, TRUE), marks = flag(input$marks, FALSE),
    risk_table = flag(input$risk_table, TRUE), ypct = flag(input$ypct, FALSE),
    xmax = num1(input$xmax, lo = 0), ymin = { v <- num1(input$ymin, 0, 0.95); if (is.na(v)) 0 else v },
    landmarks = nums(input$landmarks), tau = num1(input$tau, lo = 0),
    landmark_time = num1(input$landmark_time, lo = 0), cuts = nums(input$cuts), truncs = nums(input$truncs),
    tv_df = { v <- num1(input$tv_df, 2, 5); if (is.na(v)) 3 else round(v) }
  )
}

km_prep_code <- function(p) {
  c("num <- function(x) suppressWarnings(as.numeric(gsub(\",\", \".\", as.character(x), fixed = TRUE)))",
    "curve$time <- num(curve$time); curve$survival <- num(curve$survival)",
    "risk$time <- num(risk$time); risk$n_risk <- num(risk$n_risk)",
    "arms$n_total <- num(arms$n_total); arms$total_events <- num(arms$total_events)",
    "arms$hr_published <- num(arms$hr_published); arms$hr_ci_lower <- num(arms$hr_ci_lower); arms$hr_ci_upper <- num(arms$hr_ci_upper)",
    if (p$scale == "percent") "curve$survival <- curve$survival / 100   # percent to proportion",
    if (p$curve_type == "cumulative") "curve$survival <- 1 - curve$survival   # cumulative incidence to survival",
    sprintf("arms$group <- ifelse(tolower(trimws(arms$group)) %%in%% c(%s), \"treatment\", \"control\")",
            paste(vapply(KM_TX_WORDS, q, ""), collapse = ", ")),
    "curve <- curve[complete.cases(curve), ]; risk <- risk[complete.cases(risk), ]")
}

km_analysis_code <- function(p) {
  code <- r"---(
# Reconstruct individual patients, one arm at a time (Guyot et al. 2012, via IPDfromKM)
ipd_list <- list(); dig_list <- list(); chk_list <- list()
for (i in seq_len(nrow(arms))) {
  s <- arms$study_id[i]; a <- arms$arm[i]; g <- arms$group[i]
  cv <- curve[curve$study_id == s & curve$arm == a, c("time", "survival")]
  cv <- cv[order(cv$time), ]
  if (cv$time[1] > 0) cv <- rbind(data.frame(time = 0, survival = 1), cv)
  cv$survival <- pmin(1, cummin(cv$survival))      # remove small upward digitizing noise
  rk <- risk[risk$study_id == s & risk$arm == a, c("time", "n_risk")]
  rk <- rk[order(rk$time), ]
  n <- arms$n_total[i]; ev <- arms$total_events[i]
  if (is.na(n) && nrow(rk) > 0 && rk$time[1] == 0) n <- rk$n_risk[1]
  if (nrow(rk) >= 2) {
    if (rk$time[1] > 0 && !is.na(n)) rk <- rbind(data.frame(time = 0, n_risk = n), rk)
    pre <- IPDfromKM::preprocess(dat = cv, trisk = rk$time, nrisk = rk$n_risk,
                                 totalpts = if (is.na(n)) NULL else n, maxy = 1)
  } else {
    pre <- IPDfromKM::preprocess(dat = cv, totalpts = if (is.na(n)) NULL else n, maxy = 1)
  }
  rec <- IPDfromKM::getIPD(prep = pre, armID = as.numeric(g == "treatment"),
                           tot.events = if (is.na(ev)) NULL else ev)
  rm_ <- rec$riskmat
  ipd_list[[i]] <- data.frame(study_id = s, arm = a, group = g, time = rec$IPD$time, status = rec$IPD$status)
  dig_list[[i]] <- data.frame(study_id = s, arm = a, group = g, cv)
  chk_list[[i]] <- data.frame(study_id = s, arm = a, group = g, n = nrow(rec$IPD),
    events_reported = ev, events_reconstructed = sum(rec$IPD$status),
    rmse = rec$precision[["RMSE"]], max_abs_error = rec$precision[["max_abserror"]],
    ks_p = rec$kstest[["p-value"]],
    risk_table_diff = if (is.null(rm_) || nrow(rm_) == 0) NA else max(abs(rm_$nrisk - rm_$nrisk.hat)))
}
ipd <- do.call(rbind, ipd_list); digitized <- do.call(rbind, dig_list); checks <- do.call(rbind, chk_list)
ipd$group <- factor(ipd$group, levels = c("control", "treatment"))
ipd$study_id <- factor(ipd$study_id, levels = unique(arms$study_id))

# How closely each rebuilt curve follows the points that were digitized (compared at the same times)
fidelity <- do.call(rbind, lapply(seq_len(nrow(arms)), function(i) {
  s <- arms$study_id[i]; a <- arms$arm[i]
  d <- ipd[as.character(ipd$study_id) == s & ipd$arm == a, ]
  dg <- digitized[digitized$study_id == s & digitized$arm == a, ]
  f <- survfit(Surv(time, status) ~ 1, data = d)
  rec_at <- summary(f, times = dg$time, extend = TRUE)$surv
  first_below <- function(t, sv) { j <- which(sv <= 0.5); if (length(j)) t[j[1]] else NA_real_ }
  data.frame(study_id = s, arm = a, points = nrow(dg), mean_abs_diff = mean(abs(rec_at - dg$survival)),
             max_abs_diff = max(abs(rec_at - dg$survival)),
             median_digitized = first_below(dg$time, dg$survival), median_reconstructed = first_below(f$time, f$surv))
}))

# Pooled analysis: one curve per group, Cox model stratified by study
km_fit <- survfit(Surv(time, status) ~ group, data = ipd)
cox_strat <- coxph(Surv(time, status) ~ group + strata(study_id), data = ipd)
cs <- summary(cox_strat)
hr_pooled <- list(hr = unname(cs$conf.int[1, "exp(coef)"]), lo = unname(cs$conf.int[1, "lower .95"]),
                  hi = unname(cs$conf.int[1, "upper .95"]), p = unname(cs$coefficients[1, "Pr(>|z|)"]))
lr <- survdiff(Surv(time, status) ~ group + strata(study_id), data = ipd)
logrank_p <- 1 - pchisq(lr$chisq, sum(lr$n > 0) - 1)
ph_p <- tryCatch(unname(cox.zph(cox_strat)$table["GLOBAL", "p"]), error = function(e) NA_real_)

# Hazard ratio of each study (treatment vs control)
hr_study <- do.call(rbind, lapply(split(ipd, ipd$study_id), function(d) {
  f <- tryCatch(coxph(Surv(time, status) ~ group, data = d), error = function(e) NULL)
  if (is.null(f)) return(data.frame(study_id = as.character(d$study_id[1]), effect = NA, ci_lower = NA, ci_upper = NA, se = NA))
  s <- summary(f)
  data.frame(study_id = as.character(d$study_id[1]), effect = s$conf.int[1, "exp(coef)"],
             ci_lower = s$conf.int[1, "lower .95"], ci_upper = s$conf.int[1, "upper .95"],
             se = s$coefficients[1, "se(coef)"])
}))
rownames(hr_study) <- NULL

# Median survival, restricted mean survival time (RMST) and survival at chosen times
tau <- %%TAU%%
if (is.na(tau)) tau <- min(tapply(ipd$time, list(ipd$study_id, ipd$group), max), na.rm = TRUE)
km_tab <- summary(km_fit, rmean = tau)$table
rmst <- data.frame(group = c("control", "treatment"), rmst = km_tab[, "rmean"], se = km_tab[, "se(rmean)"], tau = tau)
rmst_diff <- list(diff = rmst$rmst[2] - rmst$rmst[1], se = sqrt(rmst$se[1]^2 + rmst$se[2]^2))
medians <- data.frame(group = c("control", "treatment"), median = km_tab[, "median"],
                      lower = km_tab[, "0.95LCL"], upper = km_tab[, "0.95UCL"])
landmarks <- c(%%LANDMARKS%%)
landmark_tab <- NULL
if (length(landmarks) > 0) {
  lm_ <- summary(km_fit, times = landmarks, extend = TRUE)
  landmark_tab <- data.frame(group = sub("^group=", "", as.character(lm_$strata)), time = lm_$time,
                             survival = lm_$surv, lower = lm_$lower, upper = lm_$upper)
}
)---"
  code <- gsub("%%TAU%%", if (is.na(p$tau)) "NA_real_" else format(p$tau), code, fixed = TRUE)
  gsub("%%LANDMARKS%%", paste(format(p$landmarks), collapse = ", "), code, fixed = TRUE)
}

# The part of the analysis that rebuilds patients (no pooling); used to check one arm against its figure
km_reconstruct_code <- function(p) sub("(?s)\n# Pooled analysis.*$", "", km_analysis_code(p), perl = TRUE)

km_plot_call <- function(p) {
  sprintf(paste0("km_plot(km_fit, hr = hr_pooled, lr_p = logrank_p, name_tx = %s, name_ctrl = %s,\n",
                 "        time_unit = %s, display = %s, conf = %s, marks = %s, risk_table = %s,\n",
                 "        xmax = %s, ymin = %s, ypct = %s)"),
          q(if (nzchar(p$name_tx)) p$name_tx else "Treatment"), q(if (nzchar(p$name_ctrl)) p$name_ctrl else "Control"),
          q(if (nzchar(p$time_unit)) p$time_unit else "months"), q(p$display),
          rbool(p$conf), rbool(p$marks), rbool(p$risk_table),
          if (is.na(p$xmax)) "NA" else format(p$xmax), format(p$ymin), rbool(p$ypct))
}

km_by_study_call <- function(p) {
  sprintf("km_plot_by_study(ipd, digitized, hr_study, name_tx = %s, name_ctrl = %s, time_unit = %s, display = %s)",
          q(if (nzchar(p$name_tx)) p$name_tx else "Treatment"), q(if (nzchar(p$name_ctrl)) p$name_ctrl else "Control"),
          q(if (nzchar(p$time_unit)) p$time_unit else "months"), q(p$display))
}

km_build_code <- function(p, digit_B = 30, digit_noise = 0.005) {
  list(prep = paste(km_prep_code(p), collapse = "\n"),
       analysis = km_analysis_code(p),
       plot = km_plot_call(p),
       by_study = km_by_study_call(p),
       extra = km_extra_code(p),
       sens = km_sens_code(p),
       variants = km_variants_code(digit_B, digit_noise),
       calls = km_extra_calls(p))
}

# Standalone script. files: list(curve = list(name, sep, enc, orig), risk = ..., arms = ...)
km_build_script <- function(code, files, w, h, sizes = list(sens = 6, loo = 4, pub = 4)) {
  rd <- function(which) {
    f <- files[[which]]; o <- f$orig; o <- o[nzchar(o)]
    c(sprintf("%s <- read.csv(%s, sep = %s, colClasses = \"character\",", which, q(f$name), q(f$sep)),
      sprintf("                na.strings = c(%s),", paste(vapply(NA_STRINGS, q, ""), collapse = ", ")),
      sprintf("                check.names = FALSE, strip.white = TRUE, encoding = %s)", q(f$enc)),
      sprintf("names(%s)[1] <- sub(\"^(\\ufeff|\\u00ef\\\\.\\\\.)\", \"\", names(%s)[1])  # remove Excel's hidden marker", which, which),
      sprintf("%s <- %s[c(%s)]", which, which, paste(vapply(unname(o), q, ""), collapse = ", ")),
      sprintf("names(%s) <- c(%s)", which, paste(vapply(names(o), q, ""), collapse = ", ")),
      vapply(KM_OPTIONAL[[which]][!nzchar(f$orig[KM_OPTIONAL[[which]]])], function(k) sprintf("%s$%s <- NA", which, k), ""))
  }
  fn <- function(nm) paste0(nm, " <- ", paste(deparse(get(nm)), collapse = "\n"))
  c(sprintf("# Kaplan-Meier reconstruction and pooled curve, generated by the app on %s", format(Sys.Date(), "%Y-%m-%d")),
    "# Needs R packages: survival, IPDfromKM and meta  (install.packages(c(\"survival\", \"IPDfromKM\", \"meta\")))",
    "# Put this script and your three CSV files in the same folder, then run it.",
    "# Reference: Guyot P, et al. BMC Med Res Methodol 2012;12:9.",
    "",
    "library(survival)", "library(IPDfromKM)", "",
    "# 1. Read the three files",
    rd("curve"), "", rd("risk"), "", rd("arms"), "",
    "# 2. Prepare the data", code$prep, "",
    "# 3. Reconstruct patients and run the pooled analysis", code$analysis, "",
    "# 4. Results",
    "print(hr_pooled)          # hazard ratio, treatment vs control (Cox stratified by study)",
    "print(logrank_p)          # stratified log-rank p-value",
    "print(ph_p)               # proportional hazards test (below 0.05: the hazard ratio is a rough summary)",
    "print(hr_study)           # one hazard ratio per study (can be pooled with a forest plot)",
    "print(medians); print(rmst); print(rmst_diff); print(landmark_tab)",
    "print(checks)             # how well each reconstruction matches the published numbers",
    "print(fidelity)           # rebuilt curve against the digitized points, and the two medians", "",
    "# 5. Summary by arm and non-proportional hazards (landmark, intervals, hazard ratio over time)",
    code$extra,
    "print(arm_summary); print(rate_ratio); print(rd_tab)",
    "print(ph_tab); print(ph_study)    # proportional hazards tests",
    "print(landmark_hr); print(piecewise); print(tv_p)", "",
    "# 6. Sensitivity analyses",
    code$sens,
    "print(sens_tab); print(loo_tab); print(pub_tab); print(heterogeneity)", "",
    "# 7. Reconstruction variants (slower: rebuilds the patients with less information)",
    code$variants,
    "print(variants_tab)",
    "print(digit_tab)          # effect of digitizing error on the pooled hazard ratio (see its columns)", "",
    "# 8. Plot functions", paste(vapply(KM_PLOT_FUNCTIONS, fn, ""), collapse = "\n\n"), "",
    "# 9. Save the figures",
    sprintf("png(\"km_pooled.png\", width = %s, height = %s, units = \"in\", res = 300)", w, h), code$plot, "dev.off()",
    sprintf("pdf(\"km_pooled.pdf\", width = %s, height = %s)", w, h), code$plot, "dev.off()",
    svg_device_code("km_pooled.svg", w, h), code$plot, "dev.off()",
    "png(\"km_by_study.png\", width = 11, height = 3.8 * ceiling(nlevels(ipd$study_id) / 3), units = \"in\", res = 300)", code$by_study, "dev.off()",
    "png(\"km_landmark.png\", width = 9.5, height = 5.8, units = \"in\", res = 300)", code$calls$landmark, "dev.off()",
    "png(\"km_hazard_ratio_over_time.png\", width = 8.5, height = 5.4, units = \"in\", res = 300)", code$calls$tv, "dev.off()",
    "png(\"km_loglog.png\", width = 7.5, height = 5.2, units = \"in\", res = 300)", code$calls$loglog, "dev.off()",
    sprintf("png(\"km_sensitivity.png\", width = 10, height = %s, units = \"in\", res = 300)", sizes$sens), code$calls$sens, "dev.off()",
    sprintf("if (!is.null(loo_tab)) { png(\"km_leave_one_out.png\", width = 9, height = %s, units = \"in\", res = 300); %s; dev.off() }", sizes$loo, code$calls$loo),
    sprintf("png(\"km_reconstructed_vs_published.png\", width = 9, height = %s, units = \"in\", res = 300)", sizes$pub), code$calls$pub, "dev.off()", "",
    "# 10. Save the tables",
    "write.csv(ipd[, c(\"study_id\", \"arm\", \"group\", \"time\", \"status\")], \"reconstructed_ipd.csv\", row.names = FALSE)",
    "write.csv(hr_study, \"study_hazard_ratios.csv\", row.names = FALSE)",
    "for (nm in c(\"arm_summary\", \"rd_tab\", \"landmark_hr\", \"piecewise\", \"sens_all\", \"loo_tab\", \"pub_tab\", \"checks\", \"fidelity\", \"digit_tab\"))",
    "  if (!is.null(get(nm))) write.csv(get(nm), paste0(nm, \".csv\"), row.names = FALSE)",
    "", script_versions_code(c("survival", "IPDfromKM", "meta", "metafor", "splines")))
}

# ---- Plot functions (self-contained: base graphics + survival) -----------------

km_plot <- function(fit, hr = NULL, lr_p = NULL, name_tx = "Treatment", name_ctrl = "Control",
                    time_unit = "months", display = "survival", conf = TRUE, marks = FALSE,
                    risk_table = TRUE, xmax = NA, ymin = 0, ypct = FALSE, tick_by = NA) {
  cols <- c(control = "#B23A48", treatment = "#2F5D8A")
  nm <- c(control = name_ctrl, treatment = name_tx)
  grp <- sub("^group=", "", names(fit$strata))
  ends <- cumsum(fit$strata); starts <- c(1, utils::head(ends, -1) + 1)
  tmax <- if (is.na(xmax)) max(fit$time) else xmax
  if (is.na(tick_by)) { pt <- pretty(c(0, tmax), n = 6); tick_by <- pt[2] - pt[1] }
  ticks <- seq(0, tmax, by = tick_by)
  flip <- display == "cumulative"
  tf <- function(y) if (flip) 1 - y else y
  steps <- function(t, y) { n <- length(t); list(x = c(t[1], rep(t[-1], each = 2)), y = c(rep(y[-n], each = 2), y[n])) }
  pfmt <- function(p) if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
  op <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(op))
  if (risk_table) graphics::layout(matrix(1:2, ncol = 1), heights = c(3.6, 0.9 + 0.3 * length(grp)))
  graphics::par(mar = c(if (risk_table) 2.4 else 4.4, 6.5, 3.4, 1.5), las = 1, mgp = c(3.2, 0.8, 0))
  ylim <- if (flip) c(0, 1 - ymin) else c(ymin, 1)
  ylab <- paste0(if (flip) "Cumulative incidence" else "Survival probability", if (ypct) " (%)" else "")
  graphics::plot(NA, xlim = c(0, tmax), ylim = ylim, xaxs = "i", xaxt = "n", yaxt = "n", bty = "l",
                 xlab = if (risk_table) "" else paste0("Time (", time_unit, ")"), ylab = ylab)
  graphics::axis(1, at = ticks)
  yt <- graphics::axTicks(2)
  graphics::axis(2, at = yt, labels = if (ypct) round(100 * yt) else yt)
  counts <- character(length(grp))
  for (k in seq_along(grp)) {
    idx <- starts[k]:ends[k]
    tt <- c(0, fit$time[idx]); ss <- c(1, fit$surv[idx])
    lo <- c(1, fit$lower[idx]); up <- c(1, fit$upper[idx])
    lo[is.na(lo)] <- ss[is.na(lo)]; up[is.na(up)] <- ss[is.na(up)]
    keep <- tt <= tmax
    last <- function(v) c(v[keep], utils::tail(v[keep], 1))
    tt2 <- c(tt[keep], tmax); ss2 <- last(ss); lo2 <- last(lo); up2 <- last(up)
    col <- cols[[grp[k]]]
    if (conf) {
      lw <- if (flip) 1 - up2 else lo2; hg <- if (flip) 1 - lo2 else up2
      a <- steps(tt2, lw); b <- steps(tt2, hg)
      graphics::polygon(c(b$x, rev(a$x)), c(b$y, rev(a$y)), col = grDevices::adjustcolor(col, 0.15), border = NA)
    }
    s <- steps(tt2, tf(ss2)); graphics::lines(s$x, s$y, col = col, lwd = 2.6)
    if (marks) {
      ci <- idx[fit$n.censor[idx] > 0 & fit$time[idx] <= tmax]
      graphics::points(fit$time[ci], tf(fit$surv[ci]), pch = 3, cex = 0.6, col = col)
    }
    counts[k] <- sprintf("%s (n = %s, events = %s)", nm[[grp[k]]], format(fit$n[k], big.mark = ","),
                         format(sum(fit$n.event[idx]), big.mark = ","))
  }
  graphics::legend(if (flip) "topleft" else "bottomleft", legend = counts, col = cols[grp], lwd = 2.6, bty = "n", cex = 0.9)
  if (!is.null(hr)) {
    t1 <- sprintf("%s vs %s: HR %.2f (95%% CI %.2f to %.2f), %s", name_tx, name_ctrl, hr$hr, hr$lo, hr$hi, pfmt(hr$p))
    if (!is.null(lr_p)) t1 <- paste0(t1, "; stratified log-rank ", pfmt(lr_p))
    graphics::mtext(t1, side = 3, line = 0.8, adj = 0, font = 2, cex = 0.85)
  }
  if (risk_table) {
    graphics::par(mar = c(4.4, 6.5, 0.6, 1.5), xpd = FALSE)
    graphics::plot(NA, xlim = c(0, tmax), ylim = c(0.5, length(grp) + 0.5), xaxs = "i", xaxt = "n", yaxt = "n", bty = "n",
                   xlab = paste0("Time (", time_unit, ")"), ylab = "")
    graphics::axis(1, at = ticks)
    sm <- summary(fit, times = ticks, extend = TRUE)
    st <- as.character(sm$strata)
    for (k in seq_along(grp)) {
      row <- length(grp) - k + 1
      graphics::text(ticks, row, sm$n.risk[st == paste0("group=", grp[k])], col = cols[[grp[k]]], cex = 0.9, xpd = NA)
      graphics::mtext(substr(nm[[grp[k]]], 1, 16), side = 2, at = row, line = 2.2, las = 1, adj = 1, cex = 0.85, col = cols[[grp[k]]])
    }
    graphics::mtext("Number at risk (reconstructed)", side = 3, line = -0.6, adj = 0, font = 2, cex = 0.8)
  }
  invisible(NULL)
}

km_plot_by_study <- function(ipd, digitized, hr_study, name_tx = "Treatment", name_ctrl = "Control",
                             time_unit = "months", display = "survival") {
  cols <- c(control = "#B23A48", treatment = "#2F5D8A")
  flip <- display == "cumulative"
  tf <- function(y) if (flip) 1 - y else y
  studies <- unique(as.character(ipd$study_id)); k <- length(studies)
  nc <- min(3, k); nr <- ceiling(k / nc)
  op <- graphics::par(mfrow = c(nr, nc), mar = c(3.6, 4.4, 3.2, 0.8), las = 1, mgp = c(2.4, 0.7, 0))
  on.exit(graphics::par(op))
  for (i in seq_along(studies)) {
    s <- studies[i]
    d <- ipd[as.character(ipd$study_id) == s, ]
    d$group <- factor(as.character(d$group), levels = c("control", "treatment"))
    f <- survival::survfit(survival::Surv(time, status) ~ group, data = d)
    gl <- sub("^group=", "", names(f$strata))
    graphics::plot(f, col = cols[gl], fun = if (flip) "event" else function(x) x, conf.int = FALSE, mark.time = FALSE, lwd = 2.4,
                   xlab = paste0("Time (", time_unit, ")"), ylab = if (flip) "Cumulative incidence" else "Survival",
                   ylim = c(0, 1), xlim = c(0, max(d$time)), bty = "l")
    dg <- digitized[as.character(digitized$study_id) == s, ]
    for (g in c("control", "treatment")) {
      z <- dg[dg$group == g, ]
      graphics::points(z$time, tf(z$survival), pch = 16, cex = 0.5, col = grDevices::adjustcolor(cols[[g]], 0.5))
    }
    h <- hr_study[hr_study$study_id == s, ]
    ttl <- if (nrow(h) && !is.na(h$effect[1])) sprintf("%s\nHR %.2f (%.2f to %.2f)", s, h$effect[1], h$ci_lower[1], h$ci_upper[1]) else s
    graphics::mtext(ttl, side = 3, line = 0.2, adj = 0, font = 2, cex = 0.8)
    if (i == 1) graphics::legend(if (flip) "topleft" else "bottomleft",
                                 legend = c(name_ctrl, name_tx, "digitized points"), bty = "n", cex = 0.75,
                                 col = c(cols, "grey40"), lwd = c(2.4, 2.4, NA), pch = c(NA, NA, 16))
  }
  invisible(NULL)
}

# IPDfromKM relies on dplyr functions without importing them, so the package must be
# ATTACHED (library), not just loaded. Done lazily the first time KM mode runs.
km_available <- function() {
  requireNamespace("IPDfromKM", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)
}
km_attach <- function() {
  for (pkg in c("survival", "IPDfromKM")) {
    if (!paste0("package:", pkg) %in% search())
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(TRUE)
}
