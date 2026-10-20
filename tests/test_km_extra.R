# Engine test with KNOWN truth: control hazard constant, treatment hazard 2x before t=6 and 0.5x after.
suppressMessages({library(survival); library(meta)})
source("R/helpers.R"); source("R/km_helpers.R"); source("R/km_extra.R")
ok <- function(c, m) { if (!isTRUE(c)) stop("FAIL: ", m, call. = FALSE); cat("OK  ", m, "\n") }

set.seed(11)
sim_study <- function(id, n) {
  h0 <- 0.03
  tt_c <- rexp(n, h0)
  # treatment: hazard 0.06 until t=6, then 0.015 (piecewise exponential)
  u <- rexp(n, 1); t1 <- u / 0.06
  tt_t <- ifelse(t1 <= 6, t1, 6 + (u - 0.06 * 6) / 0.015)
  cens <- 36
  mk <- function(t, g) data.frame(study_id = id, arm = g, group = g, time = pmin(t, cens), status = as.integer(t <= cens))
  rbind(mk(tt_c, "control"), mk(tt_t, "treatment"))
}
ipd <- rbind(sim_study("S1", 400), sim_study("S2", 500), sim_study("S3", 300))
ipd$group <- factor(ipd$group, levels = c("control", "treatment")); ipd$study_id <- factor(ipd$study_id)
km_fit <- survfit(Surv(time, status) ~ group, data = ipd)
cox_strat <- coxph(Surv(time, status) ~ group + strata(study_id), data = ipd)
cs <- summary(cox_strat)
hr_pooled <- list(hr = cs$conf.int[1, 1], lo = cs$conf.int[1, 3], hi = cs$conf.int[1, 4], p = cs$coefficients[1, 5])
hr_study <- do.call(rbind, lapply(split(ipd, ipd$study_id), function(d) {
  s <- summary(coxph(Surv(time, status) ~ group, data = d))
  data.frame(study_id = as.character(d$study_id[1]), effect = s$conf.int[1, 1], ci_lower = s$conf.int[1, 3], ci_upper = s$conf.int[1, 4], se = s$coefficients[1, 3])
}))
arms <- data.frame(study_id = rep(c("S1", "S2", "S3"), each = 2), arm = rep(c("c", "t"), 3), group = rep(c("control", "treatment"), 3),
                   n_total = c(400, 400, 500, 500, 300, 300), total_events = NA, hr_published = c(NA, 0.9, NA, NA, NA, 0.7),
                   hr_ci_lower = c(NA, 0.7, NA, NA, NA, 0.5), hr_ci_upper = c(NA, 1.2, NA, NA, NA, 0.95))
tau <- 36; landmarks <- c(12, 24)
p <- km_params(list(landmark_time = 6, cuts = "6, 18", truncs = "12, 24", tv_df = 3, scale = "proportion"))

t0 <- Sys.time()
eval(parse(text = km_extra_code(p)))
cat("extra code took", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s\n")
eval(parse(text = km_sens_code(p)))

cat("pooled HR", round(hr_pooled$hr, 2), "\n"); print(landmark_hr); print(piecewise)
ok(landmark_hr$hr[1] > 1.6 && landmark_hr$hr[1] < 2.5, "landmark early HR near 2")
ok(landmark_hr$hr[2] > 0.35 && landmark_hr$hr[2] < 0.7, "landmark late HR near 0.5")
ok(nrow(piecewise) == 3, "three intervals")
ok(piecewise$hr[1] > 1.6 && piecewise$hr[3] < 0.7, "piecewise: harm early, benefit late")
ok(!is.null(tv) && tv$hr[1] > 1.3 && tv$hr[nrow(tv)] < 0.8, "time-varying HR goes from above 1 to below 1")
ok(tv_p < 0.001, "test of constant effect rejects")
ok(!is.na(ph_tab$p) && ph_tab$p < 0.001, "global PH test rejects")
ok(nrow(arm_summary) == 2 && all(arm_summary$events > 0), "arm summary")
ok(!is.null(rd_tab) && nrow(rd_tab) == 2, "risk difference table")
print(sens_tab[, c("analysis", "hr", "lo", "hi")])
ok(nrow(sens_tab) >= 8, "sensitivity table has rows")
ok(all(is.finite(sens_tab$hr)), "all sensitivity estimates finite")
ok(!is.null(loo_tab) && nrow(loo_tab) == 3, "leave-one-out")
ok(sum(!is.na(pub_tab$hr_published)) == 2, "published HRs picked up")
ok(!is.null(heterogeneity), "heterogeneity")

# plots draw
calls <- km_extra_calls(p)
for (f in KM_PLOT_FUNCTIONS[-(1:2)]) assign(f, get(f))
png("tests/out/x_%02d.png", width = 900, height = 560, res = 96, type = "cairo")
for (nm in names(calls)) eval(parse(text = calls[[nm]]))
dev.off()
cat("EXTRA ENGINE TESTS PASSED\n")
