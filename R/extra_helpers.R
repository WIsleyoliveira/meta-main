# Extra analyses for the forest-plot mode: meta-regression, influence diagnostics, Baujat and Galbraith plots.
# Plain functions (no Shiny) so they can be tested on their own. They take the meta object `m` that the forest-plot
# analysis already built, so they always use the same studies, effect measure and model as the forest plot.

# Measures stored on the log scale (shown back-transformed)
EXTRA_RATIO <- c("RR", "OR", "HR", "IRR")

extra_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))

# Which columns can be used as a covariate? Numeric ones (at least 70% of the filled cells read as numbers) and short text ones
# with 2 to 6 distinct values (treated as groups). Columns the analysis itself uses are left out.
mr_candidates <- function(dat, exclude = character(0)) {
  out <- list()
  for (cn in setdiff(names(dat), exclude)) {
    v <- dat[[cn]]; filled <- !is.na(v) & nzchar(trimws(as.character(v)))
    if (sum(filled) < 3) next
    nv <- extra_num(v)
    if (mean(!is.na(nv[filled])) >= 0.7) {
      if (length(unique(nv[!is.na(nv)])) >= 2) out[[cn]] <- "number"
    } else {
      nu <- length(unique(trimws(as.character(v[filled]))))
      if (nu >= 2 && nu <= 6) out[[cn]] <- "group"
    }
  }
  if (!length(out)) return(data.frame(col = character(0), kind = character(0)))
  data.frame(col = names(out), kind = unname(unlist(out)), stringsAsFactors = FALSE)
}

mr_covariate <- function(v, kind) {
  if (kind == "number") extra_num(v) else { f <- trimws(as.character(v)); f[is.na(f) | f == ""] <- NA; factor(f) }
}

# Meta-regression of the study effects on one covariate. Returns list(ok, error, fit, table, text, code, ...)
mr_run <- function(m, x, kind = "number", label = "covariate", tau = "REML", hk = TRUE) {
  y <- m$TE; se <- m$seTE
  keep <- is.finite(y) & is.finite(se) & !is.na(x)
  if (kind == "group") x <- droplevels(x[keep]) else x <- x[keep]
  y <- y[keep]; se <- se[keep]
  k <- length(y)
  if (k < 3) return(list(ok = FALSE, error = "Meta-regression needs at least 3 studies with a value for the covariate."))
  if (kind == "number" && length(unique(x)) < 2) return(list(ok = FALSE, error = "The covariate has the same value in every study."))
  if (kind == "group" && nlevels(x) < 2) return(list(ok = FALSE, error = "The covariate has only one group among the studies."))
  tau <- if (tau %in% c("REML", "DL", "PM", "ML", "HE", "EB", "SJ", "HS")) tau else "REML"
  fit <- tryCatch(suppressWarnings(metafor::rma(yi = y, sei = se, mods = ~ x, method = tau, test = if (isTRUE(hk)) "knha" else "z")),
                  error = function(e) e)
  if (inherits(fit, "error")) return(list(ok = FALSE, error = paste("The meta-regression could not be fitted:", conditionMessage(fit))))
  ratio <- m$sm %in% EXTRA_RATIO
  cf <- data.frame(term = rownames(fit$beta), estimate = as.numeric(fit$beta), se = fit$se, ci_lower = fit$ci.lb, ci_upper = fit$ci.ub,
                   p = fit$pval, stringsAsFactors = FALSE)
  cf$term <- sub("^intrcpt$", "Intercept", cf$term)
  cf$term <- sub("^x", paste0(label, if (kind == "group") ": " else ""), cf$term)
  if (kind == "number") cf$term[cf$term == paste0(label)] <- label
  tab <- cf
  if (ratio) {
    tab$ratio <- exp(cf$estimate); tab$ratio_lower <- exp(cf$ci_lower); tab$ratio_upper <- exp(cf$ci_upper)
  }
  r2 <- if (is.null(fit$R2) || is.na(fit$R2)) NA_real_ else fit$R2
  base <- tryCatch(suppressWarnings(metafor::rma(yi = y, sei = se, method = tau)), error = function(e) NULL)
  no_het <- !is.null(base) && isTRUE(base$tau2 < 1e-8)   # nothing to explain
  qm_p <- fit$QMp
  slope_row <- if (kind == "number") 2 else NA
  fmt_p <- function(p) if (is.na(p)) "n/a" else if (p < 0.001) "< 0.001" else sprintf("= %.3f", p)
  txt <- if (kind == "number") {
    est <- if (ratio) sprintf("ratio per 1-unit increase %.3f (95%% CI %.3f to %.3f)", exp(cf$estimate[2]), exp(cf$ci_lower[2]), exp(cf$ci_upper[2]))
           else sprintf("coefficient %.3f (95%% CI %.3f to %.3f)", cf$estimate[2], cf$ci_lower[2], cf$ci_upper[2])
    sprintf("In a random-effects meta-regression of %d studies, %s was %s (%s; p %s). The covariate %s (residual I² = %.0f%%).",
            k, label, if (cf$p[2] < 0.05) "associated with the effect" else "not clearly associated with the effect", est, fmt_p(cf$p[2]),
            if (no_het) "could not explain heterogeneity because there was none to explain" else if (is.na(r2)) "explained an unknown share of the between-study variance" else sprintf("explained %.0f%% of the between-study variance", r2), fit$I2)
  } else {
    sprintf("In a meta-regression of %d studies, the effect %s %s (test of moderators p %s). The covariate %s (residual I² = %.0f%%).",
            k, if (isTRUE(qm_p < 0.05)) "differed by" else "did not clearly differ by", label, fmt_p(qm_p),
            if (no_het) "could not explain heterogeneity because there was none to explain" else if (is.na(r2)) "explained an unknown share of the between-study variance" else sprintf("explained %.0f%% of the between-study variance", r2), fit$I2)
  }
  if (k < 10) txt <- paste(txt, "With fewer than 10 studies, a meta-regression has little power and its result should be seen as exploratory.")
  list(ok = TRUE, fit = fit, k = k, table = tab, ratio = ratio, kind = kind, label = label, r2 = r2, i2 = fit$I2, tau2 = fit$tau2,
       qm = fit$QM, qm_p = qm_p, text = txt, x = x, y = y, se = se,
       code = c("# Meta-regression (metafor), using the effects and standard errors of the model above",
                sprintf("res <- metafor::rma(yi = m$TE, sei = m$seTE, mods = ~ %s, method = \"%s\", test = \"%s\")", if (kind == "number") label else paste0("factor(", label, ")"), tau, if (isTRUE(hk)) "knha" else "z"),
                "summary(res)",
                sprintf("metafor::regplot(res%s)", if (ratio) ", transf = exp" else "")))
}

mr_plot <- function(r) {
  if (!isTRUE(r$ok)) { plot.new(); text(0.5, 0.5, r$error); return(invisible()) }
  op <- par(mar = c(4.5, 4.5, 2, 1)); on.exit(par(op))
  if (r$kind == "number") {
    metafor::regplot(r$fit, xlab = r$label, ylab = if (r$ratio) "Effect (ratio scale)" else "Effect", transf = if (r$ratio) exp else NULL,
                     shade = TRUE, pch = 19, col = "#2F5D8A", pi = FALSE, bty = "l", las = 1)
  } else {
    ## groups: plot every study by its group, sized by weight
    lv <- levels(r$x); xi <- as.integer(r$x); wt <- 1 / (r$se^2 + r$tau2); cx <- 0.8 + 2 * sqrt(wt / max(wt))
    yy <- if (r$ratio) exp(r$y) else r$y
    plot(xi + stats::runif(length(xi), -0.08, 0.08), yy, xaxt = "n", xlim = c(0.5, length(lv) + 0.5), pch = 19, col = "#2F5D8A", cex = cx,
         xlab = r$label, ylab = if (r$ratio) "Effect (ratio scale)" else "Effect", log = if (r$ratio) "y" else "", las = 1, bty = "l")
    axis(1, at = seq_along(lv), labels = lv)
    if (r$ratio) abline(h = 1, lty = 2, col = "grey50") else abline(h = 0, lty = 2, col = "grey50")
  }
}

# Influence diagnostics (leave-one-out effect on estimate, hat values, Cook's distance...). Returns list(ok, table, fit, inf)
infl_run <- function(m, tau = "REML", hk = TRUE) {
  keep <- is.finite(m$TE) & is.finite(m$seTE)
  if (sum(keep) < 3) return(list(ok = FALSE, error = "Influence diagnostics need at least 3 studies."))
  tau <- if (tau %in% c("REML", "DL", "PM", "ML", "HE", "EB", "SJ", "HS")) tau else "REML"
  fit <- tryCatch(suppressWarnings(metafor::rma(yi = m$TE[keep], sei = m$seTE[keep], method = tau, test = if (isTRUE(hk)) "knha" else "z",
                                                slab = m$studlab[keep])), error = function(e) e)
  if (inherits(fit, "error")) return(list(ok = FALSE, error = paste("The influence analysis could not run:", conditionMessage(fit))))
  inf <- tryCatch(suppressWarnings(stats::influence(fit)), error = function(e) e)
  if (inherits(inf, "error")) return(list(ok = FALSE, error = paste("The influence analysis could not run:", conditionMessage(inf))))
  i <- inf$inf
  tab <- data.frame(study = as.character(m$studlab[keep]), rstudent = i$rstudent, dffits = i$dffits, cook_d = i$cook.d, hat = i$hat,
                    weight_pct = i$weight, tau2_without = i$tau2.del, I2_without = NA_real_, flagged = ifelse(inf$is.infl, "yes", ""),
                    stringsAsFactors = FALSE)
  # I-squared without each study
  tab$I2_without <- vapply(seq_len(nrow(tab)), function(j) {
    f <- tryCatch(suppressWarnings(metafor::rma(yi = m$TE[keep][-j], sei = m$seTE[keep][-j], method = tau)), error = function(e) NULL)
    if (is.null(f)) NA_real_ else f$I2 }, numeric(1))
  list(ok = TRUE, table = tab, fit = fit, inf = inf, k = sum(keep),
       code = c("# Influence diagnostics (metafor)",
                sprintf("res <- metafor::rma(yi = m$TE, sei = m$seTE, method = \"%s\", test = \"%s\", slab = m$studlab)", tau, if (isTRUE(hk)) "knha" else "z"),
                "inf <- influence(res)", "print(inf)", "plot(inf)",
                "meta::baujat(m)", "meta::radial(m)"))
}

infl_plot <- function(r) {
  if (!isTRUE(r$ok)) { plot.new(); text(0.5, 0.5, r$error); return(invisible()) }
  plot(r$inf)
}

baujat_plot <- function(m) {
  tryCatch(meta::baujat(m, pch = 19, col = "#2F5D8A"), error = function(e) { plot.new(); text(0.5, 0.5, paste("Baujat plot not drawn:", conditionMessage(e))) })
}

radial_plot <- function(m) {
  tryCatch(meta::radial(m), error = function(e) { plot.new(); text(0.5, 0.5, paste("Galbraith plot not drawn:", conditionMessage(e))) })
}

# One-line reading of the influence table, for people who do not know the statistics
infl_note <- function(r) {
  if (!isTRUE(r$ok)) return(r$error)
  tb <- r$table; fl <- tb$study[tb$flagged == "yes"]
  if (length(fl)) sprintf("Studies flagged as influential by the usual rules (metafor): %s. Check them for a reason (population, design, outlier), and report the result with and without them.", paste(fl, collapse = ", "))
  else "No study is flagged as influential by the usual rules (metafor)."
}
