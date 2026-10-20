# Validation C: Kaplan-Meier reconstruction. Simulate trials with known individual patient data,
# draw the published curve and risk table from them, "digitize" with noise, rebuild with the app,
# and compare with what the true data give (Cox hazard ratio, events, survival at fixed times).
suppressMessages({library(survival); library(IPDfromKM); library(meta); library(splines)})
suppressMessages({source("R/helpers.R"); source("R/km_helpers.R"); source("R/km_extra.R")})
set.seed(4242)
REPS <- as.integer(Sys.getenv("VAL_KM_REPS", "20"))

sim_arm <- function(n, rate, shape, cens_rate, maxT) {
  t <- (rexp(n)^(1 / shape)) / rate   # Weibull with hazard rate^shape * shape * t^(shape-1)
  c <- pmin(rexp(n, cens_rate), maxT) ; time <- pmin(t, c); status <- as.integer(t <= c)
  data.frame(time = time, status = status)
}
make_inputs <- function(n, hr, shape, maxT, npts, risk_every, noise, digits, with_risk, with_events) {
  rate0 <- 0.05; a0 <- sim_arm(n, rate0, shape, 0.01, maxT)
  # with a common Weibull shape, a hazard ratio of hr needs the rate multiplied by hr^(1/shape)
  a1 <- sim_arm(n, rate0 * hr^(1 / shape), shape, 0.01, maxT)
  d <- rbind(cbind(a0, g = 0), cbind(a1, g = 1))
  true_cox <- summary(coxph(Surv(time, status) ~ g, data = d))
  tg <- seq(0, maxT, length.out = npts); mk <- function(a, nm) {
    f <- survfit(Surv(time, status) ~ 1, data = a); s <- summary(f, times = tg, extend = TRUE)$surv
    s <- pmin(1, pmax(0, round(s + rnorm(length(s), 0, noise), digits))); s[1] <- 1
    data.frame(study_id = "Sim", arm = nm, time = tg, survival = s)
  }
  rt <- if (with_risk) seq(0, maxT, by = risk_every) else 0
  risk <- function(a, nm) data.frame(study_id = "Sim", arm = nm, time = rt, n_risk = vapply(rt, function(u) sum(a$time >= u), 1))
  arms <- data.frame(study_id = "Sim", arm = c("Control", "Treatment"), group = c("control", "treatment"), n_total = n,
                     total_events = if (with_events) c(sum(a0$status), sum(a1$status)) else NA)
  list(curve = rbind(mk(a0, "Control"), mk(a1, "Treatment")), risk = rbind(risk(a0, "Control"), risk(a1, "Treatment")), arms = arms,
       truth = list(hr = true_cox$conf.int[1, 1], loghr = log(true_cox$conf.int[1, 1]), events = c(sum(a0$status), sum(a1$status)),
                    s_ctrl = summary(survfit(Surv(time, status) ~ 1, data = a0), times = maxT / 2, extend = TRUE)$surv,
                    s_trt = summary(survfit(Surv(time, status) ~ 1, data = a1), times = maxT / 2, extend = TRUE)$surv))
}
rebuild <- function(inp, maxT) {
  chr <- lapply(inp[c("curve", "risk", "arms")], function(d) { d[] <- lapply(d, as.character); d })
  p <- km_params(list()); env <- new.env(parent = globalenv())
  for (w in names(KM_COLS)) env[[w]] <- km_standardize(chr[[w]], w)$data
  code <- km_build_code(p); eval(parse(text = code$prep), env); eval(parse(text = code$analysis), env)
  env
}
scen <- list(
  list(name = "Proportional hazards, full risk table, 40 points", n = 150, hr = 0.65, shape = 1, npts = 40, risk = TRUE, ev = TRUE),
  list(name = "Proportional hazards, full risk table, only 12 points", n = 150, hr = 0.65, shape = 1, npts = 12, risk = TRUE, ev = TRUE),
  list(name = "Only the initial number at risk, total events given", n = 150, hr = 0.65, shape = 1, npts = 40, risk = FALSE, ev = TRUE),
  list(name = "Only the initial number at risk, no total events", n = 150, hr = 0.65, shape = 1, npts = 40, risk = FALSE, ev = FALSE),
  list(name = "Weibull (shape 1.5), full risk table, 40 points", n = 150, hr = 0.60, shape = 1.5, npts = 40, risk = TRUE, ev = TRUE),
  list(name = "Large trial (n=600 per arm), full risk table", n = 600, hr = 0.80, shape = 1, npts = 40, risk = TRUE, ev = TRUE))
maxT <- 60; rows <- list()
for (sc in scen) for (r in seq_len(REPS)) {
  inp <- make_inputs(sc$n, sc$hr, sc$shape, maxT, sc$npts, 12, 0.002, 3, sc$risk, sc$ev)
  e <- tryCatch(rebuild(inp, maxT), error = function(err) NULL); if (is.null(e)) next
  hs <- e$hr_study; h <- hs$effect[1]
  rows[[length(rows) + 1]] <- data.frame(scenario = sc$name, rep = r, true_hr = inp$truth$hr, rec_hr = h,
    err_log = log(h) - inp$truth$loghr, ci_covers_true = (hs$ci_lower[1] <= inp$truth$hr) && (hs$ci_upper[1] >= inp$truth$hr))
}
km_val <- do.call(rbind, rows)
km_summary <- do.call(rbind, lapply(split(km_val, km_val$scenario), function(g)
  data.frame(scenario = g$scenario[1], reps = nrow(g), mean_true_hr = mean(g$true_hr), mean_rec_hr = mean(g$rec_hr),
             bias_loghr = mean(g$err_log), mae_loghr = mean(abs(g$err_log)), max_abs_loghr = max(abs(g$err_log)),
             within_5pct = mean(abs(exp(g$err_log) - 1) < 0.05), ci_cover = mean(g$ci_covers_true))))
km_summary <- km_summary[match(vapply(scen, function(s) s$name, ""), km_summary$scenario), ]; rownames(km_summary) <- NULL
