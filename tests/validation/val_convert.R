# Validation B: median/quartiles/range -> mean and SD, checked by simulation against the true sample mean and SD.
suppressMessages(source("R/helpers.R")); source("R/conv_skew_null.R"); source("R/conv_helpers.R")
set.seed(2025)
REPS <- as.integer(Sys.getenv("VAL_REPS", "150"))
dists <- list(
  "Normal (mean 50, SD 10)" = function(n) rnorm(n, 50, 10),
  "Lognormal, mild skew (sigma 0.4)" = function(n) rlnorm(n, 3, 0.4),
  "Lognormal, strong skew (sigma 0.8)" = function(n) rlnorm(n, 2.5, 0.8),
  "Exponential (mean 10)" = function(n) rexp(n, 0.1))
sizes <- c(30, 200)
rows <- list()
for (dn in names(dists)) for (n in sizes) {
  acc <- list()
  for (r in seq_len(REPS)) {
    x <- dists[[dn]](n); q <- unname(quantile(x, c(.25, .5, .75), type = 7))
    tm <- mean(x); ts <- sd(x)
    for (sc in c("S1 (range)", "S2 (quartiles)")) {
      est <- if (sc == "S1 (range)") conv_to_meansd(n, min = min(x), med = q[2], max = max(x)) else conv_to_meansd(n, q1 = q[1], med = q[2], q3 = q[3])
      est$rel_mean <- (est$mean - tm) / abs(tm); est$rel_sd <- (est$sd - ts) / ts; est$scenario <- sc
      acc[[length(acc) + 1]] <- est[, c("method", "scenario", "rel_mean", "rel_sd")]
    }
  }
  a <- do.call(rbind, acc)
  agg <- do.call(rbind, lapply(split(a, list(a$method, a$scenario), drop = TRUE), function(g)
    data.frame(distribution = dn, n = n, scenario = g$scenario[1], method = g$method[1],
               bias_mean_pct = 100 * mean(g$rel_mean, na.rm = TRUE), rmse_mean_pct = 100 * sqrt(mean(g$rel_mean^2, na.rm = TRUE)),
               bias_sd_pct = 100 * mean(g$rel_sd, na.rm = TRUE), rmse_sd_pct = 100 * sqrt(mean(g$rel_sd^2, na.rm = TRUE)))))
  rows[[length(rows) + 1]] <- agg
}
conv_sim <- do.call(rbind, rows); rownames(conv_sim) <- NULL
W <- CONV_METHODS[["wan"]]
get <- function(dn, n, sc, meth) conv_sim[conv_sim$distribution == dn & conv_sim$n == n & conv_sim$scenario == sc & conv_sim$method == meth, ]

conv_checks <- list()
chk <- function(name, ok, detail) conv_checks[[length(conv_checks) + 1]] <<- data.frame(check = name, pass = isTRUE(ok), detail = detail, stringsAsFactors = FALSE)
g <- get("Normal (mean 50, SD 10)", 200, "S2 (quartiles)", W)
chk("Normal data, n=200, quartiles: Wan+Luo bias in the mean below 1%", abs(g$bias_mean_pct) < 1, sprintf("bias %.2f%%", g$bias_mean_pct))
chk("Normal data, n=200, quartiles: Wan+Luo bias in the SD below 3%", abs(g$bias_sd_pct) < 3, sprintf("bias %.2f%%", g$bias_sd_pct))
g <- get("Normal (mean 50, SD 10)", 200, "S1 (range)", W)
chk("Normal data, n=200, range: Wan+Luo bias in the SD below 5%", abs(g$bias_sd_pct) < 5, sprintf("bias %.2f%%", g$bias_sd_pct))
w <- get("Lognormal, strong skew (sigma 0.8)", 200, "S2 (quartiles)", W); m <- get("Lognormal, strong skew (sigma 0.8)", 200, "S2 (quartiles)", CONV_METHODS[["mln"]])
b <- get("Lognormal, strong skew (sigma 0.8)", 200, "S2 (quartiles)", CONV_METHODS[["bc"]])
chk("Strong skew, n=200, quartiles: lognormal method has less SD error than Wan+Luo", m$rmse_sd_pct < w$rmse_sd_pct, sprintf("RMSE of SD: lognormal %.1f%% vs Wan+Luo %.1f%%", m$rmse_sd_pct, w$rmse_sd_pct))
chk("Strong skew, n=200, quartiles: Box-Cox has less SD error than Wan+Luo", b$rmse_sd_pct < w$rmse_sd_pct, sprintf("RMSE of SD: Box-Cox %.1f%% vs Wan+Luo %.1f%%", b$rmse_sd_pct, w$rmse_sd_pct))
chk("Strong skew, n=200, quartiles: Wan+Luo underestimates the SD by more than 15% (the reason a skewed method is needed)", w$bias_sd_pct < -15, sprintf("bias %.1f%%", w$bias_sd_pct))

# skewness flag: share of samples flagged, by distribution, sample size and what was reported
set.seed(7)
flag_rate <- function(f, n, sc, reps = 400) mean(replicate(reps, { x <- f(n); q <- unname(quantile(x, c(.25, .5, .75)))
  if (sc == "quartiles") conv_skew(q1 = q[1], med = q[2], q3 = q[3], n = n)$skewed
  else if (sc == "range") conv_skew(min = min(x), med = q[2], max = max(x), n = n)$skewed
  else conv_skew(min = min(x), q1 = q[1], med = q[2], q3 = q[3], max = max(x), n = n)$skewed }))
conv_flag <- do.call(rbind, lapply(c("quartiles", "range", "all five"), function(sc) do.call(rbind, lapply(c(30, 100, 200), function(n)
  data.frame(reported = sc, n = n, normal = 100 * flag_rate(dists[[1]], n, sc), mild_skew = 100 * flag_rate(dists[[2]], n, sc), strong_skew = 100 * flag_rate(dists[[3]], n, sc))))))
fr <- function(sc, n, col) conv_flag[conv_flag$reported == sc & conv_flag$n == n, col]
chk("Skewness flag: false alarms on normal data stay near the intended 10% (quartiles, n=100 and n=200)", all(c(fr("quartiles", 100, "normal"), fr("quartiles", 200, "normal")) > 4 & c(fr("quartiles", 100, "normal"), fr("quartiles", 200, "normal")) < 17), sprintf("%.0f%% and %.0f%%", fr("quartiles", 100, "normal"), fr("quartiles", 200, "normal")))
chk("Skewness flag: false alarms on normal data stay near the intended 10% (range, n=100)", fr("range", 100, "normal") > 4 && fr("range", 100, "normal") < 17, sprintf("%.0f%%", fr("range", 100, "normal")))
chk("Skewness flag: false alarms with all five numbers stay below 15% (n=100)", fr("all five", 100, "normal") < 15, sprintf("%.0f%%", fr("all five", 100, "normal")))
chk("Skewness flag: strong skew detected in at least 85% of samples (quartiles, n=200)", fr("quartiles", 200, "strong_skew") > 85, sprintf("%.0f%%", fr("quartiles", 200, "strong_skew")))
chk("Skewness flag: strong skew detected in at least 60% of samples (quartiles, n=100)", fr("quartiles", 100, "strong_skew") > 60, sprintf("%.0f%%", fr("quartiles", 100, "strong_skew")))

# reverse conversion
r <- conv_to_median(50, 10); chk("Mean/SD to median/IQR, normal: IQR = 1.349 x SD", abs(r$iqr[1] - 1.349 * 10) < 0.01, sprintf("IQR %.3f", r$iqr[1]))
set.seed(3); y <- rlnorm(1e6, 2, .6); rr <- conv_to_median(mean(y), sd(y))
chk("Mean/SD to median/IQR, lognormal: recovers the true median within 1%", abs(rr$median[2] - median(y)) / median(y) < 0.01, sprintf("true %.3f, estimated %.3f", median(y), rr$median[2]))
chk("Mean/SD to median/IQR, lognormal: recovers Q1 and Q3 within 2%", abs(rr$q1[2] / quantile(y, .25) - 1) < 0.02 && abs(rr$q3[2] / quantile(y, .75) - 1) < 0.02, sprintf("Q1 %.2f vs %.2f; Q3 %.2f vs %.2f", rr$q1[2], quantile(y, .25), rr$q3[2], quantile(y, .75)))
conv_checks <- do.call(rbind, conv_checks)
