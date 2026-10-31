# The facts sheet: every number the writing assistant may use, taken from the analyses on screen (forest plot,
# pooled Kaplan-Meier, risk of bias, GRADE, PRISMA), each with a short id. And the check of the numbers the AI writes
# against it. Plain functions, no Shiny.

ai_facts_empty <- function() data.frame(id = character(0), group = character(0), label = character(0), value = numeric(0),
                                        text = character(0), stringsAsFactors = FALSE)

.af_p <- function(p) if (is.na(p)) "n/a" else if (p < 0.001) "< 0.001" else sprintf("%.3f", p)
.af_n <- function(x, d = 2) if (is.na(x)) "n/a" else formatC(x, digits = d, format = "f")

ai_facts <- function(forest = NULL, km = NULL, rob = NULL, rob_tool = NULL, grade = NULL, prisma = NULL) {
  out <- list()
  add <- function(id, group, label, value = NA_real_, text = NULL) {
    v <- suppressWarnings(as.numeric(value)); if (length(v) != 1) v <- NA_real_
    out[[length(out) + 1]] <<- data.frame(id = id, group = group, label = label, value = v,
                                          text = if (is.null(text)) .af_n(v) else as.character(text), stringsAsFactors = FALSE)
  }
  # ---- forest plot ----
  if (!is.null(forest) && isTRUE(forest$ok)) {
    m <- forest$env$m; p <- forest$p
    bt <- function(v) tryCatch(meta::backtransf(v, sm = m$sm, n = m$n.harmonic.mean), error = function(e) v)
    rnd <- p$model %in% c("random", "both")
    add("data_type", "forest", "Data type", text = SPEC[[p$type]]$title)
    add("effect_measure", "forest", "Effect measure", text = SM_NAMES[[p$sm]])
    add("model", "forest", "Model", text = c(random = "random effects", common = "common effect", both = "common and random effects")[[p$model]])
    if (rnd) add("tau_method", "forest", "Between-study variance estimator", text = c(REML = "REML", DL = "DerSimonian-Laird", PM = "Paule-Mandel")[[p$tau]])
    if (rnd) add("hartung_knapp", "forest", "Hartung-Knapp adjustment", text = if (isTRUE(p$hk)) "yes" else "no")
    add("k", "forest", "Number of studies", m$k, as.character(m$k))
    nt <- sum(c(m$n.e, m$n.c, if (p$type == "prop") m$n), na.rm = TRUE)
    if (nt > 0) add("n_total", "forest", "Participants", nt, format(nt, big.mark = ","))
    if (p$type == "dich") add("events_total", "forest", "Events", sum(c(m$event.e, m$event.c), na.rm = TRUE), as.character(sum(c(m$event.e, m$event.c), na.rm = TRUE)))
    te <- if (rnd) m$TE.random else m$TE.common; lo <- if (rnd) m$lower.random else m$lower.common
    hi <- if (rnd) m$upper.random else m$upper.common; pv <- if (rnd) m$pval.random else m$pval.common
    add("pooled_est", "forest", paste("Pooled", SM_NAMES[[p$sm]], if (rnd) "(random effects)" else "(common effect)"), bt(te))
    add("pooled_lo", "forest", "Pooled estimate, 95% CI lower", bt(lo)); add("pooled_hi", "forest", "Pooled estimate, 95% CI upper", bt(hi))
    add("pooled_p", "forest", "Pooled estimate, p value", pv, .af_p(pv))
    if (p$model == "both") { add("common_est", "forest", "Pooled estimate (common effect)", bt(m$TE.common))
      add("common_lo", "forest", "Common effect, 95% CI lower", bt(m$lower.common)); add("common_hi", "forest", "Common effect, 95% CI upper", bt(m$upper.common)) }
    if (rnd && isTRUE(p$pi) && is.finite(m$lower.predict %||% NA)) {
      add("pi_lo", "forest", "95% prediction interval, lower", bt(m$lower.predict)); add("pi_hi", "forest", "95% prediction interval, upper", bt(m$upper.predict)) }
    add("i2", "forest", "I² (proportion; write as a percentage)", m$I2, sprintf("%.0f%%", 100 * m$I2))
    add("tau2", "forest", "τ²", m$tau2, .af_n(m$tau2, 3))
    add("q", "forest", "Cochran's Q", m$Q, .af_n(m$Q, 2)); add("q_p", "forest", "Q test, p value", m$pval.Q, .af_p(m$pval.Q))
    w <- if (rnd) m$w.random else m$w.common; w <- 100 * w / sum(w)
    for (i in seq_len(m$k)) {
      add(sprintf("study_%d_label", i), "studies", "Study", text = m$studlab[i])
      add(sprintf("study_%d_est", i), "studies", paste(m$studlab[i], "estimate"), bt(m$TE[i]))
      add(sprintf("study_%d_lo", i), "studies", paste(m$studlab[i], "95% CI lower"), bt(m$lower[i]))
      add(sprintf("study_%d_hi", i), "studies", paste(m$studlab[i], "95% CI upper"), bt(m$upper[i]))
      add(sprintf("study_%d_weight", i), "studies", paste(m$studlab[i], "weight (%)"), w[i], sprintf("%.1f%%", w[i]))
    }
    if (!is.null(m$subgroup) && length(m$subgroup.levels %||% character(0))) {
      lv <- m$subgroup.levels
      for (j in seq_along(lv)) {
        add(sprintf("sg_%d_label", j), "subgroups", "Subgroup", text = lv[j])
        add(sprintf("sg_%d_est", j), "subgroups", paste(lv[j], "pooled estimate"), bt(if (rnd) m$TE.random.w[j] else m$TE.common.w[j]))
        add(sprintf("sg_%d_lo", j), "subgroups", paste(lv[j], "95% CI lower"), bt(if (rnd) m$lower.random.w[j] else m$lower.common.w[j]))
        add(sprintf("sg_%d_hi", j), "subgroups", paste(lv[j], "95% CI upper"), bt(if (rnd) m$upper.random.w[j] else m$upper.common.w[j]))
      }
      pb <- if (rnd) m$pval.Q.b.random else m$pval.Q.b.common
      add("sg_diff_p", "subgroups", "Test for subgroup differences, p value", pb, .af_p(pb))
    }
    inf <- tryCatch(meta::metainf(m, pooled = if (rnd) "random" else "common"), error = function(e) NULL)
    if (!is.null(inf)) {
      est <- bt(inf$TE[seq_len(m$k)]); lab <- sub("^Omitting ", "", inf$studlab[seq_len(m$k)])
      add("loo_min", "sensitivity", paste("Leave-one-out: smallest pooled estimate (omitting", lab[which.min(est)], ")"), min(est))
      add("loo_max", "sensitivity", paste("Leave-one-out: largest pooled estimate (omitting", lab[which.max(est)], ")"), max(est))
    }
    if (m$k >= 3) {
      b <- tryCatch(meta::metabias(m, method.bias = p$bias, k.min = 3), error = function(e) NULL)
      if (!is.null(b) && is.finite(b$pval %||% NA)) { add("bias_test", "sensitivity", "Small-study test", text = p$bias)
        add("bias_p", "sensitivity", paste(p$bias, "test, p value"), b$pval, .af_p(b$pval)) }
    }
    if (isTRUE(p$trimfill)) {
      tf <- tryCatch(meta::trimfill(m), error = function(e) NULL)
      if (!is.null(tf)) { add("tf_added", "sensitivity", "Trim-and-fill: studies added", tf$k0, as.character(tf$k0))
        add("tf_est", "sensitivity", "Trim-and-fill: adjusted pooled estimate", bt(tf$TE.random)) }
    }
  }
  # ---- pooled Kaplan-Meier ----
  if (!is.null(km) && isTRUE(km$core$ok)) {
    e <- km$core$env; x <- if (isTRUE(km$extra$ok)) km$extra$env else NULL; h <- e$hr_pooled
    add("km_k", "survival", "Studies with Kaplan-Meier curves", nlevels(e$ipd$study_id), as.character(nlevels(e$ipd$study_id)))
    add("km_hr", "survival", "Pooled hazard ratio (Cox, stratified by study)", h$hr); add("km_hr_lo", "survival", "Pooled HR, 95% CI lower", h$lo)
    add("km_hr_hi", "survival", "Pooled HR, 95% CI upper", h$hi); add("km_hr_p", "survival", "Pooled HR, p value", h$p, .af_p(h$p))
    if (is.finite(e$logrank_p %||% NA)) add("km_logrank_p", "survival", "Stratified log-rank test, p value", e$logrank_p, .af_p(e$logrank_p))
    if (is.finite(e$ph_p %||% NA)) add("km_ph_p", "survival", "Proportional hazards test, p value", e$ph_p, .af_p(e$ph_p))
    if (!is.null(e$rmst_diff$diff)) add("km_rmst_diff", "survival", "Difference in restricted mean survival time", e$rmst_diff$diff)
    if (!is.null(x$arm_summary)) for (g in c("treatment", "control")) { a <- x$arm_summary[x$arm_summary$group == g, ]
      if (nrow(a)) add(paste0("km_patients_", g), "survival", paste("Patients,", g), a$patients[1], format(a$patients[1], big.mark = ",")) }
  }
  # ---- risk of bias ----
  if (!is.null(rob) && nrow(rob) && !is.null(rob_tool)) {
    ov <- rob_overall(rob, rob_tool)
    add("rob_tool", "risk of bias", "Risk-of-bias tool", text = c(rob2 = "RoB 2", robins = "ROBINS-I", robins2 = "ROBINS-I V2", nos = "Newcastle-Ottawa Scale")[[rob_tool]])
    cnt <- function(levels) sum(ov %in% levels)
    add("rob_n_low", "risk of bias", "Studies at low risk overall", cnt(c("Low", "Good")), as.character(cnt(c("Low", "Good"))))
    add("rob_n_some", "risk of bias", "Studies with some concerns / moderate risk overall", cnt(c("Some concerns", "Moderate", "Fair")), as.character(cnt(c("Some concerns", "Moderate", "Fair"))))
    add("rob_n_high", "risk of bias", "Studies at high / serious risk overall", cnt(c("High", "Serious", "Poor")), as.character(cnt(c("High", "Serious", "Poor"))))
    if (rob_tool %in% c("robins", "robins2")) add("rob_n_critical", "risk of bias", "Studies at critical risk overall", cnt("Critical"), as.character(cnt("Critical")))
    for (i in seq_len(nrow(rob))) add(sprintf("rob_study_%d", i), "risk of bias", "Overall risk of bias", text = paste0(rob$study[i], ": ", ov[i]))
  }
  # ---- GRADE ----
  for (i in seq_along(grade %||% list())) {
    o <- grade[[i]]; if (!nzchar(o$name %||% "")) next
    add(sprintf("grade_%d_outcome", i), "GRADE", "Outcome", text = o$name)
    add(sprintf("grade_%d_certainty", i), "GRADE", paste("Certainty of evidence,", o$name), text = grade_cert_of(o))
    why <- unlist(lapply(names(GRADE_DOMAINS), function(d) if (.gn(o[[d]]) > 0) paste0(GRADE_DOMAINS[[d]], if (.gn(o[[d]]) > 1) " (very serious)" else " (serious)",
                                                                                     if (nzchar(o[[paste0(d, "_t")]] %||% "")) paste0(": ", o[[paste0(d, "_t")]]) else "")))
    add(sprintf("grade_%d_reasons", i), "GRADE", paste("Reasons for rating down,", o$name), text = if (length(why)) paste(why, collapse = "; ") else "none")
  }
  # ---- PRISMA ----
  if (!is.null(prisma) && !length(prisma$errors)) {
    pf <- function(id, label, v) if (!is.na(v %||% NA)) add(id, "PRISMA", label, v, as.character(v))
    pf("prisma_identified", "Records identified", prisma$identified); pf("prisma_duplicates", "Duplicates removed", prisma$dup)
    pf("prisma_screened", "Records screened", prisma$screened); pf("prisma_assessed", "Reports assessed for eligibility", prisma$assessed)
    pf("prisma_included", "Studies included in the review", prisma$included); pf("prisma_in_ma", "Studies in the meta-analysis", prisma$in_ma)
  }
  if (!length(out)) return(ai_facts_empty())
  do.call(rbind, out)
}

# ---- Number check ----------------------------------------------------------------------------

# Numbers in a text, with where they are: decimal point or comma, thousands separators, a minus only as a sign (not in "0.60-0.94")
ai_extract_numbers <- function(text) {
  re <- "(?<![0-9.,])(-(?=[0-9]))?[0-9]{1,3}(?:,[0-9]{3})+(?![0-9])|(?<![0-9.,])(-(?=[0-9]))?[0-9]+(?:[.,][0-9]+)?"
  mm <- gregexpr(re, text, perl = TRUE)[[1]]
  if (mm[1] == -1) return(data.frame(number = character(0), start = integer(0), end = integer(0), value = numeric(0), stringsAsFactors = FALSE))
  s <- as.integer(mm); e <- s + attr(mm, "match.length") - 1L
  num <- substring(text, s, e)
  # a minus right after a digit (a range written with a hyphen) is not a sign
  for (i in seq_along(num)) if (startsWith(num[i], "-") && s[i] > 1 && grepl("[0-9]", substr(text, s[i] - 1, s[i] - 1))) {
    num[i] <- substring(num[i], 2); s[i] <- s[i] + 1L }
  val <- vapply(num, function(n) {
    if (grepl("^-?[1-9][0-9]{0,2}(,[0-9]{3})+$", n)) as.numeric(gsub(",", "", n)) else as.numeric(sub(",", ".", n, fixed = TRUE))
  }, 0)
  data.frame(number = num, start = s, end = e, value = unname(val), stringsAsFactors = FALSE)
}

ai_check_numbers <- function(text, facts) {
  nums <- ai_extract_numbers(text)
  if (!nrow(nums)) return(cbind(nums[, c("number", "start", "end")], matched = logical(0)))
  v <- facts$value[is.finite(facts$value)]
  cand <- unique(c(unlist(lapply(0:3, function(d) round(v, d))), unlist(lapply(0:1, function(d) round(100 * v[abs(v) <= 1], d))),
                   round(abs(v), 2), round(abs(v), 3)))
  # numbers printed in the text facts (study labels with years, subgroup names) and the usual constants
  in_text <- as.numeric(unlist(regmatches(facts$text, gregexpr("[0-9]+(?:\\.[0-9]+)?", facts$text, perl = TRUE))))
  allowed <- c(95, 2020, 1:10, in_text)
  pvals <- facts$value[grepl("(^|_)p$", facts$id) & is.finite(facts$value)]
  matched <- vapply(seq_len(nrow(nums)), function(i) {
    x <- nums$value[i]; if (is.na(x)) return(FALSE)
    if (any(abs(cand - x) < 1e-9) || any(abs(allowed - x) < 1e-9)) return(TRUE)
    before <- substr(text, max(1, nums$start[i] - 6), nums$start[i] - 1)
    if (grepl("p\\s*(<|≤)\\s*$", before, ignore.case = TRUE)) return(any(pvals < x))
    if (grepl("p\\s*(>|≥)\\s*$", before, ignore.case = TRUE)) return(any(pvals > x))
    FALSE
  }, logical(1))
  data.frame(number = nums$number, start = nums$start, end = nums$end, matched = matched, stringsAsFactors = FALSE)
}

# The text as HTML (escaped), with each number not found in the facts marked for the reader
ai_mark_numbers <- function(text, facts) {
  ch <- ai_check_numbers(text, facts)
  esc <- function(x) htmltools::htmlEscape(x)
  if (!nrow(ch) || all(ch$matched)) return(htmltools::HTML(esc(text)))
  out <- character(0); pos <- 1L
  for (i in which(!ch$matched)) {
    out <- c(out, esc(substr(text, pos, ch$start[i] - 1L)),
             sprintf('<span class="aiw-unmatched" title="Not found in your analysis: check it">%s</span>', esc(ch$number[i])))
    pos <- ch$end[i] + 1L
  }
  htmltools::HTML(paste0(c(out, esc(substr(text, pos, nchar(text)))), collapse = ""))
}
