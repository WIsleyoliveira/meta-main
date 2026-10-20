# Validation A: the forest-plot engine against metafor (an independent implementation)
# and against published values for the BCG vaccine data (Colditz 1994, as analysed in the metafor documentation).
suppressMessages({library(meta); library(metafor); library(metadat)})
source("R/helpers.R")

app_fit <- function(df, type, inp = list()) {
  rd <- list(data = df); keys <- names(SPEC[[type]]$cols)
  map <- as.list(guess_columns(c("study", keys), names(df)))
  p <- make_params(inp, type); if (!is.null(inp$sm)) p$sm <- inp$sm
  v <- validate_data(df, type, map, p); if (length(v$errors)) stop(paste(v$errors, collapse = "; "))
  code <- build_code(p, map, v$need, group_col = "", year_col = "")
  env <- new.env(parent = globalenv()); env$dat <- df
  eval(parse(text = code$prepare), env); eval(parse(text = code$model), env); env$m
}
res <- list(); add <- function(case, quantity, app, ref, tol, source) res[[length(res) + 1]] <<- data.frame(
  case = case, quantity = quantity, app = app, reference = ref, abs_diff = abs(app - ref), tolerance = tol,
  pass = abs(app - ref) <= tol, source = source, stringsAsFactors = FALSE)

# ---- 1. BCG vaccine: risk ratio, random effects (REML), no Hartung-Knapp
b <- dat.bcg; d1 <- data.frame(study = paste(b$author, b$year), events_tx = b$tpos, n_tx = b$tpos + b$tneg,
                               events_ctrl = b$cpos, n_ctrl = b$cpos + b$cneg)
m <- app_fit(d1, "dich", list(sm = "RR", method = "Inverse", model = "random", tau = "REML", hk = FALSE, incr = "only0"))
es <- escalc("RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = b); mf <- rma(yi, vi, data = es, method = "REML")
add("BCG, RR, REML", "pooled log RR", m$TE.random, -0.7145, 5e-4, "published (metafor documentation)")
add("BCG, RR, REML", "SE of pooled log RR", m$seTE.random, 0.1798, 5e-4, "published (metafor documentation)")
add("BCG, RR, REML", "tau-squared", m$tau2, 0.3132, 5e-4, "published (metafor documentation)")
add("BCG, RR, REML", "I-squared from Q (%)", 100 * m$I2, 100 * (mf$QE - (mf$k - 1)) / mf$QE, 1e-6, "Higgins formula on metafor Q (published I-squared of 92.22 uses the REML tau-squared instead)")
add("BCG, RR, REML", "pooled log RR", m$TE.random, as.numeric(mf$beta), 1e-6, "metafor, same data")
add("BCG, RR, REML", "tau-squared", m$tau2, mf$tau2, 1e-6, "metafor, same data")

# ---- 2. Same data, Hartung-Knapp
m <- app_fit(d1, "dich", list(sm = "RR", method = "Inverse", model = "random", tau = "REML", hk = TRUE))
mk <- rma(yi, vi, data = es, method = "REML", test = "knha")
add("BCG, RR, REML + Hartung-Knapp", "pooled log RR", m$TE.random, as.numeric(mk$beta), 1e-6, "metafor, same data")
add("BCG, RR, REML + Hartung-Knapp", "lower 95% limit", m$lower.random, mk$ci.lb, 1e-4, "metafor, same data")
add("BCG, RR, REML + Hartung-Knapp", "upper 95% limit", m$upper.random, mk$ci.ub, 1e-4, "metafor, same data")

# ---- 3. Mantel-Haenszel common effect, OR and RR
for (sm in c("OR", "RR")) {
  m <- app_fit(d1, "dich", list(sm = sm, method = "MH", model = "common", incr = "only0"))
  mm <- rma.mh(measure = sm, ai = tpos, bi = tneg, ci = cpos, di = cneg, data = b, add = c(0, 0), to = c("none", "none"))
  add(paste("BCG,", sm, "Mantel-Haenszel"), "pooled log effect", m$TE.common, as.numeric(mm$beta), 1e-4, "metafor rma.mh, same data")
}

# ---- 4. Continuous: MD and SMD (Normand 1999, hospital stay after stroke)
nm <- dat.normand1999; d2 <- data.frame(study = nm$source, mean_tx = nm$m1i, sd_tx = nm$sd1i, n_tx = nm$n1i,
                                        mean_ctrl = nm$m2i, sd_ctrl = nm$sd2i, n_ctrl = nm$n2i)
for (sm in c("MD", "SMD")) {
  m <- app_fit(d2, "cont", list(sm = sm, model = "random", tau = "REML", hk = FALSE))
  e2 <- escalc(sm, m1i = m1i, sd1i = sd1i, n1i = n1i, m2i = m2i, sd2i = sd2i, n2i = n2i, data = nm)
  r2 <- rma(yi, vi, data = e2, method = "REML")
  tl <- if (sm == "SMD") 5e-3 else 1e-4   # the two packages use slightly different variance formulas for Hedges' g
  add(paste("Normand, continuous", sm, "REML"), "pooled effect", m$TE.random, as.numeric(r2$beta), tl, "metafor, same data")
  add(paste("Normand, continuous", sm, "REML"), "tau-squared", m$tau2, r2$tau2, if (sm == "SMD") 5e-3 else 1e-4, "metafor, same data")
}

# ---- 5. Pre-calculated hazard ratios (log scale back-transformation)
yi <- es$yi; se <- sqrt(es$vi)
d3 <- data.frame(study = paste(b$author, b$year), effect = exp(yi), ci_lower = exp(yi - 1.96 * se), ci_upper = exp(yi + 1.96 * se))
m <- app_fit(d3, "gen", list(sm = "HR", model = "random", tau = "REML", hk = FALSE))
r3 <- rma(yi, se^2, method = "REML")
add("Pre-calculated HR (from BCG)", "pooled log HR", m$TE.random, as.numeric(r3$beta), 1e-4, "metafor, same data")

# ---- 6. Single proportion, logit
d4 <- data.frame(study = paste(b$author, b$year), events = b$tpos, n = b$tpos + b$tneg)
m <- app_fit(d4, "prop", list(sm = "PLOGIT", model = "random", tau = "REML", hk = FALSE))
e4 <- escalc("PLO", xi = events, ni = n, data = d4); r4 <- rma(yi, vi, data = e4, method = "REML")
add("Single proportion, logit", "pooled logit proportion", m$TE.random, as.numeric(r4$beta), 1e-3, "metafor, same data")

forest_validation <- do.call(rbind, res)
