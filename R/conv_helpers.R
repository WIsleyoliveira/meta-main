# Conversions between median (with quartiles or range) and mean (with SD).
# Methods: Wan 2014 (SD) with Luo 2018 (mean) for roughly normal data; Box-Cox and quantile
# estimation (McGrath 2020) and lognormal (Cai 2021) from the R package estmeansd for skewed data.

CONV_METHODS <- c(wan = "Wan 2014 + Luo 2018 (roughly symmetric data)",
                  bc = "Box-Cox (McGrath 2020, skewed data)",
                  qe = "Quantile estimation (McGrath 2020, skewed data)",
                  mln = "Method of lognormal (Cai 2021, skewed positive data)")

conv_have_estmeansd <- function() requireNamespace("estmeansd", quietly = TRUE)

# Which set of summaries was reported: S1 = min, median, max; S2 = quartiles and median; S3 = all five
conv_scenario <- function(min, q1, med, q3, max) {
  has <- function(x) length(x) == 1 && is.finite(x)
  r <- has(min) && has(max); q <- has(q1) && has(q3)
  if (!has(med)) return(NA_character_)
  if (r && q) "S3" else if (q) "S2" else if (r) "S1" else NA_character_
}

conv_check <- function(n, min = NA, q1 = NA, med = NA, q3 = NA, max = NA) {
  if (!is.finite(n) || n < 3) return("Type the sample size (at least 3).")
  s <- conv_scenario(min, q1, med, q3, max)
  if (is.na(s)) return("Type the median plus either both quartiles (Q1 and Q3) or both extremes (minimum and maximum).")
  v <- c(min, q1, med, q3, max); v <- v[is.finite(v)]
  if (is.unsorted(v)) return("The values must be in order: minimum <= Q1 <= median <= Q3 <= maximum.")
  if (s != "S1" && !(q3 > q1)) return("Q3 must be larger than Q1.")
  NULL
}

# Wan 2014 for the SD, Luo 2018 for the mean
conv_wan <- function(n, min = NA, q1 = NA, med, q3 = NA, max = NA) {
  s <- conv_scenario(min, q1, med, q3, max)
  xi <- 2 * qnorm((n - 0.375) / (n + 0.25)); eta <- 2 * qnorm((0.75 * n - 0.125) / (n + 0.25))
  w <- 2.2 / (2.2 + n^0.75)
  switch(s,
    S1 = list(mean = 4 / (4 + n^0.75) * (min + max) / 2 + n^0.75 / (4 + n^0.75) * med, sd = (max - min) / xi),
    S2 = list(mean = (0.7 + 0.39 / n) * (q1 + q3) / 2 + (0.3 - 0.39 / n) * med, sd = (q3 - q1) / eta),
    S3 = list(mean = w * (min + max) / 2 + (0.7 - 0.72 / n^0.55) * (q1 + q3) / 2 + (0.3 + 0.72 / n^0.55 - w) * med,
              sd = (max - min) / (2 * xi) + (q3 - q1) / (2 * eta)))
}

# All methods side by side. Returns a data frame: method, mean, sd, note
conv_to_meansd <- function(n, min = NA, q1 = NA, med, q3 = NA, max = NA) {
  # the skewed-data methods use simulation: fix the seed so the numbers are reproducible, then restore the caller's random state
  if (exists(".Random.seed", envir = globalenv())) { old_seed <- get(".Random.seed", envir = globalenv()); on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE) }
  else on.exit(if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(2024)
  s <- conv_scenario(min, q1, med, q3, max)
  rows <- list()
  add <- function(key, m, sd, note = "") rows[[length(rows) + 1]] <<- data.frame(method = CONV_METHODS[[key]], mean = m, sd = sd, note = note, stringsAsFactors = FALSE)
  w <- conv_wan(n, min, q1, med, q3, max); add("wan", w$mean, w$sd)
  if (conv_have_estmeansd()) {
    a <- list(min.val = if (s %in% c("S1", "S3")) unname(min), q1.val = if (s %in% c("S2", "S3")) unname(q1), med.val = unname(med),
              q3.val = if (s %in% c("S2", "S3")) unname(q3), max.val = if (s %in% c("S1", "S3")) unname(max), n = unname(n))
    a <- a[!vapply(a, is.null, logical(1))]
    for (k in c("bc", "qe", "mln")) {
      fn <- switch(k, bc = estmeansd::bc.mean.sd, qe = estmeansd::qe.mean.sd, mln = estmeansd::mln.mean.sd)
      r <- tryCatch(suppressWarnings(do.call(fn, a)), error = function(e) NULL)
      if (is.null(r)) add(k, NA_real_, NA_real_, "could not be fitted for these numbers")
      else add(k, unname(r$est.mean), unname(r$est.sd))
    }
  }
  do.call(rbind, rows)
}

# Asymmetry of the reported summaries. Two indices, each compared with its distribution for normal data of the same
# sample size (simulated; tables in conv_skew_null.R), which gives an approximate p-value:
#   Bowley index  (Q1 + Q3 - 2 median) / (Q3 - Q1)    when quartiles are given
#   Range index   (min + max - 2 median) / (max - min) when the extremes are given
# A small p-value means the summaries are unlikely to come from symmetric data. The flag is raised at p < 0.10
# (p < 0.05 for each index when both are available), so about one in ten normal samples is flagged by chance.
conv_null_quantiles <- function(n, tab) {
  ln <- log(pmin(max(SKEW_NULL_N), pmax(min(SKEW_NULL_N), n))); g <- log(SKEW_NULL_N)
  vapply(seq_along(SKEW_NULL_LEVELS), function(k) stats::approx(g, tab[, k], xout = ln, rule = 2)$y, 0)
}
conv_skew_p <- function(index, n, tab) {
  if (!is.finite(index) || !is.finite(n)) return(NA_real_)
  q <- conv_null_quantiles(n, tab); a <- abs(index)
  if (a <= q[1]) return(0.5)
  if (a >= q[length(q)]) return(0.01)
  1 - stats::approx(q, SKEW_NULL_LEVELS, xout = a, ties = "ordered")$y
}
conv_skew <- function(min = NA, q1 = NA, med, q3 = NA, max = NA, n = NA) {
  s <- conv_scenario(min, q1, med, q3, max)
  b <- if (s %in% c("S2", "S3")) (q1 + q3 - 2 * med) / (q3 - q1) else NA_real_
  r <- if (s %in% c("S1", "S3")) (min + max - 2 * med) / (max - min) else NA_real_
  pb <- if (is.finite(n)) conv_skew_p(b, n, SKEW_NULL_BOWLEY) else NA_real_
  pr <- if (is.finite(n)) conv_skew_p(r, n, SKEW_NULL_RANGE) else NA_real_
  cut <- if (s == "S3") 0.05 else 0.10
  skewed <- isTRUE(any(c(pb, pr) < cut, na.rm = TRUE))
  idx <- if (is.finite(b)) b else r
  p <- suppressWarnings(min(c(pb, pr), na.rm = TRUE)); if (!is.finite(p)) p <- NA_real_
  list(index = unname(idx), bowley = unname(b), range = unname(r), p_bowley = pb, p_range = pr, p = p, skewed = skewed, scenario = s)
}

# Mean and SD to median and quartiles: normal assumption, and lognormal for positive skewed data
conv_to_median <- function(mean, sd, n = NA) {
  rows <- list()
  z <- qnorm(0.75)
  rows[[1]] <- data.frame(method = "Normal distribution", median = mean, q1 = mean - z * sd, q3 = mean + z * sd, iqr = 2 * z * sd, stringsAsFactors = FALSE)
  if (mean > 0 && sd > 0) {
    s2 <- log(1 + (sd / mean)^2); mu <- log(mean) - s2 / 2; sg <- sqrt(s2)
    rows[[2]] <- data.frame(method = "Lognormal distribution (positive, right-skewed data)", median = exp(mu),
                            q1 = exp(mu - z * sg), q3 = exp(mu + z * sg), iqr = exp(mu + z * sg) - exp(mu - z * sg), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}


# One row of a table of studies: reported mean and SD are kept; otherwise they are estimated from the median.
# method: "auto" (Wan and Luo unless the summaries look asymmetric, then Box-Cox), "wan", "bc", "qe" or "mln".
conv_row <- function(n, mean = NA, sd = NA, min = NA, q1 = NA, med = NA, q3 = NA, max = NA, method = "auto") {
  out <- function(m, s, src, note = "") data.frame(mean = m, sd = s, source = src, note = note, stringsAsFactors = FALSE)
  if (is.finite(mean) && is.finite(sd)) return(if (sd > 0) out(mean, sd, "reported") else out(NA_real_, NA_real_, "error", "The SD must be above zero."))
  err <- conv_check(n, min, q1, med, q3, max); if (!is.null(err)) return(out(NA_real_, NA_real_, "error", err))
  tab <- conv_to_meansd(n, min, q1, med, q3, max); note <- ""
  key <- method
  if (method == "auto") {
    sk <- conv_skew(min, q1, med, q3, max, n = n)
    key <- if (sk$skewed && conv_have_estmeansd()) "bc" else "wan"
    note <- if (sk$skewed) (if (conv_have_estmeansd()) "summaries look asymmetric: Box-Cox used" else "summaries look asymmetric but estmeansd is not installed: Wan and Luo used") else ""
  }
  nm <- CONV_METHODS[[key]]; r <- tab[tab$method == nm, , drop = FALSE]
  if (!nrow(r) || !is.finite(r$mean) || !is.finite(r$sd)) { r <- tab[1, , drop = FALSE]; nm <- r$method; note <- paste(c(note, "the chosen method could not be fitted: Wan and Luo used"), collapse = "; ") }
  out(r$mean, r$sd, nm, note)
}

# Other conversions (Cochrane Handbook 6.5.2)
conv_se_to_sd <- function(se, n) se * sqrt(n)
conv_ci_to_sd <- function(lower, upper, n, level = 0.95) sqrt(n) * (upper - lower) / (2 * qt((1 + level) / 2, n - 1))
conv_combine <- function(n1, m1, s1, n2, m2, s2) {
  n <- n1 + n2; m <- (n1 * m1 + n2 * m2) / n
  v <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2 + n1 * n2 / n * (m1^2 + m2^2 - 2 * m1 * m2)) / (n - 1)
  list(n = n, mean = m, sd = sqrt(v))
}
