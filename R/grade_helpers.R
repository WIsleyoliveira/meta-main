# GRADE: certainty of the evidence, Summary of findings table and evidence profile, following Chapter 14 of the Cochrane Handbook
# (Schunemann et al., "Completing 'Summary of findings' tables and grading the certainty of the evidence").
#
# Everything here is plain R with no Shiny, so it can be tested alone. The judgements are the person's: the functions that "suggest"
# only read the analysis and say why.

GRADE_LEVELS <- c("Very low", "Low", "Moderate", "High")
GRADE_SYM <- c("Very low" = "⊕◯◯◯", "Low" = "⊕⊕◯◯", "Moderate" = "⊕⊕⊕◯", "High" = "⊕⊕⊕⊕")
GRADE_DEFS <- c(
  "High certainty" = "We are very confident that the true effect lies close to that of the estimate of the effect.",
  "Moderate certainty" = "We are moderately confident in the effect estimate: the true effect is likely to be close to the estimate of the effect, but there is a possibility that it is substantially different.",
  "Low certainty" = "Our confidence in the effect estimate is limited: the true effect may be substantially different from the estimate of the effect.",
  "Very low certainty" = "We have very little confidence in the effect estimate: the true effect is likely to be substantially different from the estimate of effect.")

GRADE_DESIGN_RCT <- "Randomized trials"
GRADE_DESIGN_NRS <- "Observational studies"
GRADE_DESIGN_ROBINS <- "Observational studies (ROBINS-I)"
GRADE_MAX_OUTCOMES <- 7L   # Cochrane Handbook 14.1.2: no more than seven outcomes per table

GRADE_MEASURES <- c("RR", "OR", "HR", "RD", "MD", "SMD", "Other")
GRADE_MEASURE_NAMES <- c(RR = "risk ratio", OR = "odds ratio", HR = "hazard ratio", RD = "risk difference", MD = "mean difference",
                         SMD = "standardised mean difference", Other = "effect")
GRADE_DOMAINS <- c(rob = "risk of bias", inc = "inconsistency", ind = "indirectness", imp = "imprecision", pub = "publication bias")
GRADE_UPS <- c(large = "large effect", dose = "dose-response gradient", conf = "plausible residual confounding that would reduce the effect")

.gn <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) != 1 || is.na(x)) 0 else x }
.gv <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) != 1 || !is.finite(x)) NA_real_ else x }
.gt <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) "" else trimws(as.character(x))
.glevel <- function(n) c("one level", "two levels", "three levels")[n]

# Randomized trials start high. Observational studies start low, or high when they were assessed with ROBINS-I (Handbook 14.2.1);
# the latter should then normally be rated down for confounding and selection bias. Each domain lowers the certainty by one level
# (serious) or two (very serious); risk of bias can lower it by three (critical under ROBINS-I). Rating up applies only to
# observational studies that started low and have no reason to be rated down (large effect, dose-response, opposing confounding).
grade_certainty <- function(design, rob = 0, inconsistency = 0, indirectness = 0, imprecision = 0, pubbias = 0, large = 0, dose = 0, confounding = 0) {
  start <- if (design %in% c(GRADE_DESIGN_RCT, GRADE_DESIGN_ROBINS)) 4 else 2
  down <- .gn(rob) + .gn(inconsistency) + .gn(indirectness) + .gn(imprecision) + .gn(pubbias)
  up <- if (identical(design, GRADE_DESIGN_NRS) && down == 0) .gn(large) + .gn(dose) + .gn(confounding) else 0
  GRADE_LEVELS[max(1, min(4, start - down + up))]
}

# ---- Formatting ---------------------------------------------------------------

.gf <- function(x, d = 2) formatC(x, digits = d, format = "f")
.gint <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
.grisk <- function(x) if (!is.finite(x)) "n/a" else if (abs(x) < 10) sub("\\.0$", "", formatC(abs(x), digits = 1, format = "f")) else .gint(abs(x))
.gdiff <- function(x) if (!is.finite(x)) "n/a" else if (round(x, 1) == 0) "0" else paste(.grisk(x), if (x < 0) "fewer" else "more")

# Corresponding risk in the intervention group (per 1,000) from the assumed comparator risk (Handbook 14.1.5.1).
# RR: control x RR. OR: the odds are multiplied. HR: event-free survival is raised to the power HR. RD is already absolute.
grade_absolute <- function(measure, est, lo, hi, control = NA_real_, per = 1000) {
  est <- .gv(est); lo <- .gv(lo); hi <- .gv(hi); control <- .gv(control)
  if (!is.finite(est) || !is.finite(lo) || !is.finite(hi)) return(NULL)
  if (measure == "RD") {
    d <- per * c(est, lo, hi)
    return(list(kind = "rd", control = control, int = if (is.finite(control)) control + d[1] else NA_real_, diff = d[1], dlo = d[2], dhi = d[3]))
  }
  if (!measure %in% c("RR", "OR", "HR") || !is.finite(control) || control <= 0 || control >= per) return(NULL)
  if (any(c(est, lo, hi) <= 0)) return(NULL)
  R <- control / per
  f <- switch(measure, RR = function(x) R * x, OR = function(x) x * R / (1 - R + x * R), HR = function(x) 1 - (1 - R)^x)
  g <- function(x) min(per, max(0, f(x) * per))
  int <- g(est); l <- g(lo); h <- g(hi)
  list(kind = "risk", control = control, int = int, lo = l, hi = h, diff = int - control, dlo = l - control, dhi = h - control)
}

# "88 per 1,000 (79 to 95)" and "32 fewer per 1,000 (from 41 fewer to 25 fewer)"
grade_abs_text <- function(a) {
  if (is.null(a)) return(NULL)
  rng <- function(l, h) sprintf("from %s to %s", .gdiff(l), .gdiff(h))
  list(control = if (is.finite(a$control)) sprintf("%s per 1,000", .grisk(a$control)) else "",
       int = if (a$kind == "risk") sprintf("%s per 1,000 (%s to %s)", .grisk(a$int), .grisk(a$lo), .grisk(a$hi)) else if (is.finite(a$int)) sprintf("%s per 1,000", .grisk(a$int)) else "",
       diff = sprintf("Risk difference: %s per 1,000 (%s)", .gdiff(a$diff), rng(a$dlo, a$dhi)))
}

# Continuous outcomes: the difference, in words, as the Handbook shows it
grade_continuous_text <- function(measure, est, lo, hi, outcome = "", control_mean = NA_real_, unit = "") {
  est <- .gv(est); lo <- .gv(lo); hi <- .gv(hi); control_mean <- .gv(control_mean)
  if (!is.finite(est) || !is.finite(lo) || !is.finite(hi)) return(NULL)
  unit <- trimws(unit %||% ""); u <- if (nzchar(unit)) paste0(" ", unit) else ""
  dir <- function(x, with_unit = FALSE) if (round(x, 2) == 0) "0" else paste0(.gf(abs(x)), if (with_unit) u else "", " ", if (x < 0) "lower" else "higher")
  out <- if (nzchar(trimws(outcome))) tolower(substr(outcome, 1, 1)) %+% substring(outcome, 2) else "value"
  rng <- sprintf("%s to %s", dir(lo), dir(hi))
  if (measure == "SMD") return(list(control = "", int = sprintf("The SMD in the intervention group was %s SD %s (%s)", .gf(abs(est)), if (est < 0) "lower" else "higher", rng)))
  list(control = if (is.finite(control_mean)) sprintf("The mean %s in the control group was %s%s", out, .gf(control_mean), u) else "",
       int = sprintf("The mean %s in the intervention group was %s (%s)", out, dir(est, TRUE), rng))
}

`%+%` <- function(a, b) paste0(a, b)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Suggestions from the analysis --------------------------------------------

# Optimal information size for a dichotomous outcome: total participants needed to detect a relative risk reduction (alpha 0.05, power 80%)
grade_ois <- function(control_risk, rrr = 0.25, alpha = 0.05, power = 0.8) {
  p1 <- .gv(control_risk); rrr <- .gv(rrr)
  if (!is.finite(p1) || !is.finite(rrr) || p1 <= 0 || p1 >= 1 || rrr <= 0 || rrr >= 1) return(NA_real_)
  p2 <- p1 * (1 - rrr)
  n <- (stats::qnorm(1 - alpha / 2) + stats::qnorm(power))^2 * (p1 * (1 - p1) + p2 * (1 - p2)) / (p1 - p2)^2
  2 * ceiling(n)
}

# From a fitted meta-analysis (m): inconsistency, imprecision and publication bias, each with the sentence that would explain it.
# control: assumed control risk per 1,000 (dichotomous only). They are starting points; the person decides.
grade_suggest <- function(m, bias_p = NA, control = NA_real_, rrr = 25, n_total = NA_real_) {
  rnd <- !is.null(m$TE.random) && !is.na(m$TE.random) && !isTRUE(m$random == FALSE)
  lo <- if (rnd) m$lower.random else m$lower.common; hi <- if (rnd) m$upper.random else m$upper.common
  ratio <- m$sm %in% c("RR", "OR", "HR", "IRR")
  prop <- m$sm %in% c("PLOGIT", "PFT", "PAS", "PRAW")
  bt <- function(v) tryCatch(meta::backtransf(v, sm = m$sm, n = m$n.harmonic.mean), error = function(e) v)
  out <- list(inconsistency = 0, imprecision = 0, pubbias = 0, inc_text = "", imp_text = "", pub_text = "", ois = NA_real_)

  # inconsistency (Handbook 10.10.2 bands for I2; direction of the study estimates)
  k <- m$k; i2 <- 100 * m$I2
  if (k < 2) out$inc_text <- "Only one study, so there is no inconsistency."
  else if (is.na(i2)) out$inc_text <- ""
  else {
    te <- m$TE[is.finite(m$TE)]
    opposite <- !prop && any(te < 0) && any(te > 0)
    band <- if (i2 < 40) "might not be important" else if (i2 < 60) "may be moderate" else if (i2 < 75) "may be substantial" else "may be considerable"
    out$inconsistency <- if (i2 >= 75 && opposite) 2 else if (i2 >= 50) 1 else 0
    rg <- sprintf("estimates range from %s to %s", .gf(bt(min(te))), .gf(bt(max(te))))
    out$inc_text <- if (out$inconsistency == 0) sprintf("I² = %.0f%% (heterogeneity %s); not rated down.", i2, band)
                    else sprintf("I² = %.0f%%, heterogeneity %s; %s%s.", i2, band, rg, if (opposite) ", on both sides of no effect" else "")
  }

  # imprecision: does the CI include no effect, and appreciable benefit or harm? Is the sample below the optimal information size?
  ois <- if (ratio && is.finite(.gv(control))) grade_ois(.gv(control) / 1000, rrr / 100) else NA_real_
  out$ois <- ois
  if (!prop && is.finite(lo) && is.finite(hi)) {
    crosses_null <- lo < 0 && hi > 0
    spans <- ratio && lo < log(0.75) && hi > log(1.25)
    below_ois <- is.finite(ois) && is.finite(.gv(n_total)) && n_total < ois
    out$imprecision <- if (crosses_null && (below_ois || spans)) 2 else if (crosses_null || below_ois) 1 else 0
    bits <- c(if (crosses_null) "The 95% CI includes no effect" else "The 95% CI excludes no effect",
              if (spans) " and both appreciable benefit and appreciable harm (RR 0.75 and 1.25 as a guide)",
              if (below_ois) sprintf("; total participants (%s) are below the optimal information size (%s)", .gint(n_total), .gint(ois))
              else if (is.finite(ois) && is.finite(.gv(n_total))) sprintf("; total participants (%s) reach the optimal information size (%s)", .gint(n_total), .gint(ois)))
    out$imp_text <- paste0(paste(bits, collapse = ""), ".")
  }

  # publication bias: only a funnel-based test with at least 10 studies says anything
  if (k >= 10 && is.finite(.gv(bias_p))) {
    out$pubbias <- if (bias_p < 0.1) 1 else 0
    out$pub_text <- sprintf("Funnel plot test for asymmetry, p = %s (%d studies)%s.", .gf(bias_p, 3), k, if (bias_p < 0.1) "; asymmetry suggests small-study effects" else "")
  } else out$pub_text <- if (k < 10) sprintf("Only %d studies, too few to judge a funnel plot.", k) else ""
  out$note <- paste(c(out$inc_text, out$imp_text, out$pub_text)[nzchar(c(out$inc_text, out$imp_text, out$pub_text))], collapse = " ")
  out
}

# Risk of bias from the weight each study has in the pooled result (Handbook 14.2.2: the judgement follows the studies that carry the information).
# rob: data frame with the study name and the overall judgement of RoB 2 or ROBINS-I.
grade_rob_suggest <- function(m, rob, tool, design = GRADE_DESIGN_RCT, rnd = TRUE) {
  if (is.null(rob) || !nrow(rob) || !tool %in% c("rob2", "robins")) return(NULL)
  w <- if (rnd && !is.null(m$w.random)) m$w.random else m$w.common
  if (is.null(w)) return(NULL)
  key <- function(x) tolower(trimws(as.character(x)))
  idx <- match(key(m$studlab), key(rob$study))
  if (anyNA(idx)) return(list(matched = FALSE, text = "The study names in the risk-of-bias table do not match the forest plot, so the risk of bias could not be read from them."))
  ov <- as.character(rob$overall[idx]); w <- w / sum(w)
  cat_of <- function(x) if (tool == "rob2") c(Low = "low", `Some concerns` = "some", High = "high")[x]
                        else c(Low = "low", Moderate = "some", Serious = "high", Critical = "crit", `No information` = "some")[x]
  cc <- unname(cat_of(ov)); cc[is.na(cc)] <- "some"
  sh <- vapply(c("low", "some", "high", "crit"), function(z) sum(w[cc == z]), numeric(1))
  txt <- sprintf("Weight of the studies by overall risk of bias: %.0f%% low, %.0f%% %s, %.0f%% %s%s.", 100 * sh["low"],
                 100 * sh["some"], if (tool == "rob2") "some concerns" else "moderate", 100 * sh["high"], if (tool == "rob2") "high" else "serious",
                 if (tool == "robins") sprintf(", %.0f%% critical", 100 * sh["crit"]) else "")
  level <- if (design == GRADE_DESIGN_ROBINS) { if (sh["crit"] >= 0.5) 3 else 2 }
           else if (sh["low"] >= 0.5) 0 else 1
  if (design == GRADE_DESIGN_ROBINS) txt <- paste0(txt, " Studies assessed with ROBINS-I are normally rated down two levels for confounding and selection bias.")
  list(matched = TRUE, level = level, shares = sh, text = txt)
}

# ---- Outcome records ----------------------------------------------------------
# An outcome is a list: name, time, importance, design, k, n, measure, est, lo, hi, control, source, cmean, unit, comment, rrr,
# and for each domain d in rob, inc, ind, imp, pub: o[[d]] (level) and o[[paste0(d, "_t")]] (explanation); upgrades large, dose, conf likewise.

grade_cert_of <- function(o) grade_certainty(o$design %||% GRADE_DESIGN_RCT, o$rob, o$inc, o$ind, o$imp, o$pub, o$large, o$dose, o$conf)

# Reasons for rating down or up, in the order of the Handbook, each with the explanation the person typed
grade_reasons <- function(o) {
  out <- list()
  for (d in names(GRADE_DOMAINS)) {
    lv <- .gn(o[[d]]); if (lv <= 0) next
    out[[length(out) + 1]] <- list(domain = d, level = lv, kind = "down",
      note = sprintf("Downgraded by %s for %s: %s", .glevel(lv), GRADE_DOMAINS[[d]], if (nzchar(.gt(o[[paste0(d, "_t")]]))) sub("[.]$", "", .gt(o[[paste0(d, "_t")]])) %+% "." else "explanation needed."))
  }
  if (identical(o$design, GRADE_DESIGN_NRS) && all(vapply(names(GRADE_DOMAINS), function(d) .gn(o[[d]]) == 0, TRUE))) {
    for (d in names(GRADE_UPS)) {
      lv <- .gn(o[[d]]); if (lv <= 0) next
      out[[length(out) + 1]] <- list(domain = d, level = lv, kind = "up",
        note = sprintf("Upgraded by %s for %s: %s", .glevel(lv), GRADE_UPS[[d]], if (nzchar(.gt(o[[paste0(d, "_t")]]))) sub("[.]$", "", .gt(o[[paste0(d, "_t")]])) %+% "." else "explanation needed."))
    }
  }
  out
}

grade_study_label <- function(o) {
  k <- .gv(o$k); lab <- if (identical(o$design, GRADE_DESIGN_RCT)) "RCT" else "observational study"
  kk <- if (is.finite(k)) { if (k == 1) sprintf("1 %s", lab) else sprintf("%s %s", .gint(k), if (lab == "RCT") "RCTs" else "observational studies") } else ""
  n <- .gv(o$n)
  if (is.finite(n) && nzchar(kk)) sprintf("%s (%s)", .gint(n), kk) else if (nzchar(kk)) sprintf("— (%s)", kk) else if (is.finite(n)) .gint(n) else ""
}

grade_effect_text <- function(o) {
  est <- .gv(o$est); lo <- .gv(o$lo); hi <- .gv(o$hi); if (!all(is.finite(c(est, lo, hi)))) return("")
  sprintf("%s %s (%s to %s)", o$measure, .gf(est), .gf(lo), .gf(hi))
}

# Absolute-effect texts for an outcome: list(control, int, diff, wide) where wide = text spanning both columns (continuous outcomes)
grade_abs_for <- function(o) {
  ms <- o$measure %||% "RR"
  if (ms %in% c("MD", "SMD")) {
    ct <- grade_continuous_text(ms, o$est, o$lo, o$hi, o$name, o$cmean, o$unit)
    return(if (is.null(ct)) NULL else list(wide = TRUE, control = ct$control, int = ct$int, diff = ""))
  }
  a <- grade_absolute(ms, o$est, o$lo, o$hi, o$control)
  if (is.null(a)) return(NULL)
  t <- grade_abs_text(a); t$wide <- FALSE; t
}

# Problems the person should fix before using the table
grade_issues <- function(outs) {
  w <- character(0)
  for (i in seq_along(outs)) {
    o <- outs[[i]]; tag <- sprintf("Outcome %d%s", i, if (nzchar(.gt(o$name))) sprintf(" (%s)", .gt(o$name)) else "")
    if (!nzchar(.gt(o$name))) w <- c(w, sprintf("Outcome %d has no name.", i))
    est <- .gv(o$est); lo <- .gv(o$lo); hi <- .gv(o$hi)
    if (!all(is.finite(c(est, lo, hi)))) w <- c(w, sprintf("%s: enter the effect estimate and its 95%% CI.", tag))
    else {
      if (lo > est || est > hi) w <- c(w, sprintf("%s: the estimate lies outside its confidence interval.", tag))
      if ((o$measure %||% "RR") %in% c("RR", "OR", "HR") && lo <= 0) w <- c(w, sprintf("%s: ratio estimates must be greater than 0.", tag))
    }
    rs <- grade_reasons(o)
    miss <- vapply(rs, function(r) !nzchar(.gt(o[[paste0(r$domain, "_t")]])), TRUE)
    if (any(miss)) w <- c(w, sprintf("%s: explain every rating (%s). Each judgement below high certainty needs a footnote.", tag, paste(vapply(rs[miss], function(r) c(GRADE_DOMAINS, GRADE_UPS)[[r$domain]], ""), collapse = ", ")))
    down <- sum(vapply(names(GRADE_DOMAINS), function(d) .gn(o[[d]]), numeric(1)))
    ups <- sum(vapply(names(GRADE_UPS), function(d) .gn(o[[d]]), numeric(1)))
    if (identical(o$design, GRADE_DESIGN_NRS) && ups > 0 && down > 0) w <- c(w, sprintf("%s: rating up is not applied because another domain is rated down.", tag))
    if (identical(o$design, GRADE_DESIGN_ROBINS) && .gn(o$rob) < 2)
      w <- c(w, sprintf("%s: studies assessed with ROBINS-I should normally be rated down two levels for confounding and selection bias. Justify it if you do not.", tag))
    if ((o$measure %||% "RR") %in% c("RR", "OR", "HR") && !is.finite(.gv(o$control))) w <- c(w, sprintf("%s: add an assumed control risk to show the absolute effect.", tag))
    if (down >= 4) w <- c(w, sprintf("%s: the certainty cannot fall below very low.", tag))
  }
  w
}

# ---- Footnotes -------------------------------------------------------------------

grade_footnoter <- function() {
  env <- new.env(); env$txt <- character()
  marks <- c(letters, paste0(letters, letters))
  list(add = function(t) { t <- trimws(t); if (!nzchar(t)) return(""); i <- match(t, env$txt); if (is.na(i)) { env$txt <- c(env$txt, t); i <- length(env$txt) }; marks[i] },
       notes = function() stats::setNames(env$txt, marks[seq_along(env$txt)]))
}

.gsup <- function(m) if (length(m) && any(nzchar(m))) tags$sup(class = "fn", paste(unique(m[nzchar(m)]), collapse = ",")) else NULL

# ---- Summary of findings table (Cochrane layout) ----------------------------------

grade_title <- function(q) {
  a <- .gt(q$intervention); b <- .gt(q$comparison); p <- .gt(q$population)
  if (!nzchar(a) && !nzchar(b) && !nzchar(p)) return("")
  sprintf("%s compared to %s for %s", if (nzchar(a)) a else "[intervention]", if (nzchar(b)) b else "[comparison]", if (nzchar(p)) p else "[health problem]")
}

grade_header_block <- function(q, ncol) {
  tt <- grade_title(q)
  line <- function(lab, v) if (nzchar(.gt(v))) tags$div(tags$b(paste0(lab, ": ")), .gt(v))
  tags$tr(class = "sof-top", tags$td(colspan = ncol,
    if (nzchar(tt)) tags$div(class = "sof-title", tt),
    line("Patient or population", q$population), line("Setting", q$setting), line("Intervention", q$intervention), line("Comparison", q$comparison)))
}

.gabbr <- function(outs) {
  ms <- unique(vapply(outs, function(o) o$measure %||% "RR", ""))
  full <- c(RR = "RR: risk ratio", OR = "OR: odds ratio", HR = "HR: hazard ratio", RD = "RD: risk difference", MD = "MD: mean difference", SMD = "SMD: standardised mean difference")
  paste(c("CI: confidence interval", unname(full[intersect(names(full), ms)])), collapse = "; ")
}

.gdefs <- function() tags$div(class = "sof-defs",
  tags$div(tags$b("GRADE Working Group grades of evidence")),
  lapply(names(GRADE_DEFS), function(n) tags$div(tags$b(paste0(n, ": ")), GRADE_DEFS[[n]])))

.gnotes <- function(fn) {
  n <- fn$notes(); if (!length(n)) return(NULL)
  tags$div(class = "sof-notes", tags$div(tags$b("Explanations")),
    lapply(seq_along(n), function(i) tags$div(tags$sup(class = "fn", names(n)[i]), " ", n[[i]])))
}

grade_sof_html <- function(q, outs) {
  fn <- grade_footnoter()
  comp <- if (nzchar(.gt(q$comparison))) .gt(q$comparison) else "comparator"
  intv <- if (nzchar(.gt(q$intervention))) .gt(q$intervention) else "intervention"
  rows <- lapply(outs, function(o) {
    cert <- grade_cert_of(o); rs <- grade_reasons(o)
    ab <- grade_abs_for(o)
    ctrl_mark <- .gsup(fn$add(.gt(o$source)))
    cert_marks <- vapply(rs, function(r) fn$add(r$note), "")
    cells_abs <- if (is.null(ab)) list(tags$td(colspan = 2, class = "na", "—"))
      else if (isTRUE(ab$wide)) list(tags$td(ab$control), tags$td(ab$int))
      else list(tags$td(ab$control, ctrl_mark),
                tags$td(if (nzchar(ab$int)) tags$div(ab$int), tags$div(class = "sof-diff", ab$diff)))
    tags$tr(
      tags$td(class = "sof-out", tags$b(if (nzchar(.gt(o$name))) .gt(o$name) else "Outcome"), if (nzchar(.gt(o$time))) tags$div(class = "sof-time", .gt(o$time))),
      cells_abs,
      tags$td(grade_effect_text(o)),
      tags$td(grade_study_label(o)),
      tags$td(class = "sof-cert", tags$div(class = "sym", GRADE_SYM[[cert]]), tags$div(cert, .gsup(cert_marks))),
      tags$td(.gt(o$comment)))
  })
  tbl <- tags$table(class = "sof",
    tags$thead(grade_header_block(q, 7),
      tags$tr(tags$th(rowspan = 2, "Outcomes"), tags$th(colspan = 2, "Anticipated absolute effects* (95% CI)"), tags$th(rowspan = 2, "Relative effect (95% CI)"),
              tags$th(rowspan = 2, "№ of participants (studies)"), tags$th(rowspan = 2, "Certainty of the evidence (GRADE)"), tags$th(rowspan = 2, "Comments")),
      tags$tr(tags$th(paste("Risk with", comp)), tags$th(paste("Risk with", intv)))),
    tags$tbody(rows),
    tags$tfoot(tags$tr(tags$td(colspan = 7,
      tags$div(class = "sof-star", "*The risk in the intervention group (and its 95% confidence interval) is based on the assumed risk in the comparison group and the relative effect of the intervention (and its 95% CI)."),
      tags$div(class = "sof-abbr", .gabbr(outs)),
      .gdefs(), .gnotes(fn)))))
  tbl
}

# ---- Evidence profile (one column per domain) ---------------------------------------

grade_profile_html <- function(q, outs) {
  fn <- grade_footnoter()
  lv_txt <- function(lv) c("Not serious", "Serious", "Very serious", "Extremely serious")[lv + 1]
  rows <- lapply(outs, function(o) {
    cert <- grade_cert_of(o); rs <- grade_reasons(o)
    mk <- function(d) { r <- Filter(function(x) x$domain == d, rs); .gsup(vapply(r, function(x) fn$add(x$note), "")) }
    ups <- Filter(function(x) x$kind == "up", rs)
    other <- c(if (.gn(o$pub) > 0) "publication bias strongly suspected", vapply(ups, function(x) c(GRADE_UPS, GRADE_DOMAINS)[[x$domain]], ""))
    ab <- grade_abs_for(o)
    abs_txt <- if (is.null(ab)) "—" else if (isTRUE(ab$wide)) ab$int else ab$diff
    tags$tr(
      tags$td(class = "sof-out", tags$b(if (nzchar(.gt(o$name))) .gt(o$name) else "Outcome"), if (nzchar(.gt(o$time))) tags$div(class = "sof-time", .gt(o$time))),
      tags$td(.gt(if (is.finite(.gv(o$k))) .gint(.gv(o$k)) else "")),
      tags$td(if (identical(o$design, GRADE_DESIGN_RCT)) "Randomised trials" else "Observational studies"),
      tags$td(lv_txt(.gn(o$rob)), mk("rob")), tags$td(lv_txt(.gn(o$inc)), mk("inc")), tags$td(lv_txt(.gn(o$ind)), mk("ind")), tags$td(lv_txt(.gn(o$imp)), mk("imp")),
      tags$td(if (length(other)) tagList(paste(other, collapse = "; "), mk("pub"), .gsup(vapply(ups, function(x) fn$add(x$note), ""))) else "None"),
      tags$td(grade_effect_text(o)), tags$td(abs_txt),
      tags$td(class = "sof-cert", tags$div(class = "sym", GRADE_SYM[[cert]]), tags$div(cert)),
      tags$td(c("Critical", "Important")[match(o$importance %||% "Critical", c("Critical", "Important"))]))
  })
  tags$table(class = "sof ep",
    tags$thead(grade_header_block(q, 12),
      tags$tr(tags$th(rowspan = 2, "Outcome"), tags$th(colspan = 7, "Certainty assessment"), tags$th(colspan = 2, "Effect"), tags$th(rowspan = 2, "Certainty"), tags$th(rowspan = 2, "Importance")),
      tags$tr(tags$th("№ of studies"), tags$th("Study design"), tags$th("Risk of bias"), tags$th("Inconsistency"), tags$th("Indirectness"), tags$th("Imprecision"), tags$th("Other considerations"),
              tags$th("Relative (95% CI)"), tags$th("Absolute (95% CI)"))),
    tags$tbody(rows),
    tags$tfoot(tags$tr(tags$td(colspan = 12, tags$div(class = "sof-abbr", .gabbr(outs)), .gdefs(), .gnotes(fn)))))
}

# ---- Flat table and texts -------------------------------------------------------------

grade_csv <- function(outs) {
  do.call(rbind, lapply(outs, function(o) {
    cert <- grade_cert_of(o); ab <- grade_abs_for(o); rs <- grade_reasons(o)
    data.frame(Outcome = .gt(o$name), `Time frame` = .gt(o$time), Importance = o$importance %||% "Critical", Design = o$design %||% GRADE_DESIGN_RCT,
               `Studies` = .gt(o$k), Participants = .gt(o$n), `Relative effect (95% CI)` = grade_effect_text(o),
               `Risk with comparator` = if (is.null(ab)) "" else ab$control, `Risk with intervention` = if (is.null(ab)) "" else ab$int,
               `Risk difference` = if (is.null(ab)) "" else ab$diff, Certainty = cert, Reasons = paste(vapply(rs, function(r) r$note, ""), collapse = " "),
               Comments = .gt(o$comment), check.names = FALSE, stringsAsFactors = FALSE)
  }))
}

grade_methods_text <- function(outs) {
  nrs <- any(vapply(outs, function(o) !identical(o$design, GRADE_DESIGN_RCT), TRUE))
  robins <- any(vapply(outs, function(o) identical(o$design, GRADE_DESIGN_ROBINS), TRUE))
  paste0("We assessed the certainty of the evidence for each outcome with the GRADE approach, following Chapter 14 of the Cochrane Handbook for Systematic Reviews of Interventions. ",
         "Evidence from randomised trials started at high certainty",
         if (nrs) if (robins) ", and evidence from non-randomised studies assessed with ROBINS-I also started at high certainty and was rated down for confounding and selection bias where appropriate"
                 else ", and evidence from non-randomised studies started at low certainty" else "",
         ". We considered five domains for rating down: risk of bias, inconsistency, indirectness, imprecision and publication bias. ",
         "We rated down by one level when a concern was serious and by two levels when it was very serious",
         if (nrs && !robins) ", and we considered rating up for a large effect, a dose-response gradient, and plausible residual confounding that would reduce the effect" else "",
         ". Certainty was graded as high, moderate, low or very low. We justified every judgement in the footnotes of the summary of findings table. ",
         "Adapt this paragraph to your protocol (for example, who graded the evidence and how disagreements were resolved).")
}

grade_result_sentences <- function(outs) {
  vapply(outs, function(o) {
    cert <- grade_cert_of(o); rs <- grade_reasons(o)
    why <- if (length(rs)) paste0(", ", paste(vapply(rs, function(r) sprintf("%s by %s for %s", if (r$kind == "down") "downgraded" else "upgraded", .glevel(r$level),
                                                                        c(GRADE_DOMAINS, GRADE_UPS)[[r$domain]]), ""), collapse = ", ")) else ""
    nm <- if (nzchar(.gt(o$name))) .gt(o$name) else "this outcome"
    est <- .gv(o$est); lo <- .gv(o$lo); hi <- .gv(o$hi)
    k <- .gv(o$k); n <- .gv(o$n)
    lab <- if (identical(o$design, GRADE_DESIGN_RCT)) "randomised trial" else "observational stud"
    size <- paste(c(if (is.finite(k)) sprintf("%s %s", .gint(k), if (lab == "randomised trial") (if (k == 1) lab else "randomised trials") else (if (k == 1) "observational study" else "observational studies")),
                    if (is.finite(n)) sprintf("%s participants", .gint(n))), collapse = ", ")
    eff <- if (all(is.finite(c(est, lo, hi)))) sprintf(" (%s %s, 95%% CI %s to %s%s)", o$measure %||% "RR", .gf(est), .gf(lo), .gf(hi), if (nzchar(size)) paste0("; ", size) else "") else ""
    sprintf("For %s%s, we judged the certainty of the evidence as %s%s%s.", nm, if (nzchar(.gt(o$time))) paste0(" (", .gt(o$time), ")") else "", tolower(cert), why, eff)
  }, "")
}

grade_refs <- c(
  "Schunemann HJ, Higgins JPT, Vist GE, Glasziou P, Akl EA, Skoetz N, Guyatt GH. Chapter 14: Completing 'Summary of findings' tables and grading the certainty of the evidence. In: Higgins JPT, Thomas J, Chandler J, et al., editors. Cochrane Handbook for Systematic Reviews of Interventions. Cochrane.",
  "Guyatt GH, Oxman AD, Vist GE, et al. GRADE: an emerging consensus on rating quality of evidence and strength of recommendations. BMJ 2008;336:924-926.")

# ---- Style shared by the app and the Word download ------------------------------------
GRADE_TABLE_CSS <- paste(
  "table.sof { border-collapse: collapse; width: 100%; font-size: 12px; background: #fff; color: #111; }",
  "table.sof th, table.sof td { border: 1px solid #8A99A8; padding: 5px 7px; vertical-align: top; text-align: left; }",
  "table.sof thead th { background: #DDE6EF; font-weight: 700; }",
  "table.sof tr.sof-top td { background: #F4F7FA; padding: 8px 10px; }",
  "table.sof .sof-title { font-weight: 700; font-size: 14px; margin-bottom: 4px; }",
  "table.sof .sof-time, table.sof .sof-diff { color: #44525F; font-size: 11px; margin-top: 2px; }",
  "table.sof .sym { font-size: 15px; letter-spacing: 1px; }",
  "table.sof td.sof-cert { white-space: nowrap; }",
  "table.sof tfoot td { font-size: 11px; border-top: 2px solid #8A99A8; }",
  "table.sof tfoot div { margin: 3px 0; }",
  "table.sof .fn { font-weight: 700; color: #1E3A5F; }", sep = "\n")

grade_word_doc <- function(tbl, title = "Summary of findings") {
  html <- as.character(htmltools::renderTags(tbl)$html)
  paste0("<html xmlns:o=\"urn:schemas-microsoft-com:office:office\" xmlns:w=\"urn:schemas-microsoft-com:office:word\" xmlns=\"http://www.w3.org/TR/REC-html40\">",
         "<head><meta charset=\"utf-8\"><title>", title, "</title><style>@page { size: landscape; } body { font-family: Calibri, Arial, sans-serif; }\n", GRADE_TABLE_CSS, "</style></head><body>", html, "</body></html>")
}
