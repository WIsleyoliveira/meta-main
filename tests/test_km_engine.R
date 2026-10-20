# Engine tests for KM reconstruction: example data, exported script, conversions,
# step-corner curves, validation messages, and every plot option.
suppressMessages({library(survival); library(IPDfromKM)})
source("R/helpers.R"); source("R/km_helpers.R"); source("R/km_extra.R")
out <- Sys.getenv("OUT_DIR", "tests/out"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
ok <- function(msg) cat("OK  ", msg, "\n")
chr <- function(l) lapply(l, function(d) { d[] <- lapply(d, as.character); d })

base_params <- function(...) { p <- km_params(list(...)); p }

run_analysis <- function(ex, p) {
  ex <- chr(ex)
  env <- new.env(parent = globalenv())
  for (w in names(KM_COLS)) env[[w]] <- km_standardize(ex[[w]], w)$data
  code <- km_build_code(p)
  eval(parse(text = code$prep), env); eval(parse(text = code$analysis), env)
  env$km_plot <- km_plot; env$km_plot_by_study <- km_plot_by_study
  list(env = env, code = code)
}

ex <- km_example(); obs <- attr(ex, "observed_hr")
p <- base_params(landmarks = "12, 24, 36")
ex_chr <- chr(ex)
std <- lapply(setNames(names(KM_COLS), names(KM_COLS)), function(w) km_standardize(ex_chr[[w]], w))
v <- km_validate(std$curve$data, std$risk$data, std$arms$data, p)
cat("validation errors:", length(v$errors), " warnings:", length(v$warnings), "\n"); print(v$warnings)
stopifnot(!length(v$errors)); ok("example data validates")

r <- run_analysis(ex, p); e <- r$env
print(round(e$hr_study[, -1], 3)); print(unlist(obs))
err <- abs(e$hr_study$effect - unlist(obs)[e$hr_study$study_id]); cat("abs HR error by study:", round(err, 3), "\n")
stopifnot(all(err < 0.03)); ok("reconstructed study HRs match the observed HRs of the simulated trials")
cat("pooled HR:", round(unlist(e$hr_pooled), 3), " PH p:", round(e$ph_p, 3), "\n")
stopifnot(e$hr_pooled$lo < e$hr_pooled$hr, e$hr_pooled$hr < e$hr_pooled$hi, e$hr_pooled$hr < 1)
stopifnot(all(abs(e$checks$events_reconstructed - e$checks$events_reported) / e$checks$events_reported < 0.05))
ok("pooled HR sane, reconstructed events within 5% of reported")
stopifnot(is.data.frame(e$fidelity), nrow(e$fidelity) == 6, all(e$fidelity$mean_abs_diff < 0.03), all(e$fidelity$points > 0))
ok("rebuilt curves follow the digitized points (fidelity table)")
few <- chr(ex); few$curve <- few$curve[as.numeric(few$curve$time) %in% c(0, 6, 12, 18, 24, 36, 48), ]
vf <- km_validate(km_standardize(few$curve, "curve")$data, std$risk$data, std$arms$data, p)
stopifnot(any(grepl("fewer than 30", unlist(vf$warnings)) | grepl("curve points. With fewer", unlist(vf$warnings)))); ok("warning when a curve has fewer than 30 points")
stopifnot(nrow(e$landmark_tab) == 6, !is.null(e$rmst_diff$diff)); ok("landmarks and RMST computed")

# percent scale + cumulative incidence give the same answer
ex2 <- ex; ex2$curve$survival <- 100 * (1 - ex$curve$survival)
r2 <- run_analysis(ex2, base_params(scale = "percent", curve_type = "cumulative", landmarks = "12, 24, 36"))
stopifnot(isTRUE(all.equal(r2$env$hr_pooled$hr, e$hr_pooled$hr, tolerance = 5e-3)))
ok("percent + cumulative incidence input equals survival proportion input")

# step corners (duplicate times, as a digitizer gives on vertical drops)
ex3 <- ex; cv <- ex$curve; keep <- list()
for (k in unique(paste(cv$study_id, cv$arm))) {
  d <- cv[paste(cv$study_id, cv$arm) == k, ]
  corners <- do.call(rbind, lapply(seq_len(nrow(d) - 1), function(i) data.frame(study_id = d$study_id[1], arm = d$arm[1],
                          time = c(d$time[i + 1], d$time[i + 1]), survival = c(d$survival[i], d$survival[i + 1]))))
  keep[[k]] <- rbind(d[1, ], corners)
}
ex3$curve <- do.call(rbind, keep)
r3 <- run_analysis(ex3, p)
cat("step-corner HR:", round(r3$env$hr_pooled$hr, 3), "vs", round(e$hr_pooled$hr, 3), "\n")
stopifnot(abs(r3$env$hr_pooled$hr - e$hr_pooled$hr) < 0.03); ok("curves with duplicated times (step corners) work")

# no risk table, no total events
ex4 <- ex; ex4$risk <- ex$risk[0, ]; ex4$arms$total_events <- NA
r4 <- run_analysis(ex4, p); cat("no risk/events HR:", round(r4$env$hr_pooled$hr, 3), "\n")
stopifnot(abs(r4$env$hr_pooled$hr - e$hr_pooled$hr) < 0.06); ok("works without risk table and total events")

# exported script runs on its own and matches the app (semicolon CSV, original column names)
dd <- file.path(out, "kmscript"); unlink(dd, recursive = TRUE); dir.create(dd)
ren <- list(curve = c(study_id = "Study", arm = "Arm", time = "Time (months)", survival = "Survival (%)"),
            risk = c(study_id = "Study", arm = "Arm", time = "Time", n_risk = "No. at risk"),
            arms = c(study_id = "Study", arm = "Arm", group = "Role", n_total = "N", total_events = "Events", hr_published = "HR (paper)", hr_ci_lower = "HR low", hr_ci_upper = "HR high"))
files <- list()
for (w in names(KM_COLS)) {
  d <- ex[[w]]; if (w == "curve") d$survival <- 100 * d$survival
  names(d) <- ren[[w]][names(d)]
  fn <- paste0(w, ".csv"); write.table(format(d, decimal.mark = ","), file.path(dd, fn), sep = ";", row.names = FALSE, quote = FALSE, dec = ",")
  files[[w]] <- list(name = fn, sep = ";", enc = "UTF-8", orig = setNames(ren[[w]][KM_COLS[[w]]], KM_COLS[[w]]))
}
pp <- base_params(scale = "percent", landmarks = "12, 24, 36")
code <- km_build_code(pp)
writeLines(km_build_script(code, files, 9, 7), file.path(dd, "analysis.R"))
old <- setwd(dd); res <- system2("Rscript", "analysis.R", stdout = TRUE, stderr = TRUE); setwd(old)
st <- attr(res, "status"); if (!is.null(st) && st != 0) { print(tail(res, 20)); stop("exported KM script failed") }
sh <- read.csv(file.path(dd, "study_hazard_ratios.csv"))
stopifnot(all(file.exists(file.path(dd, c("km_pooled.png", "km_pooled.pdf", "km_pooled.svg", "reconstructed_ipd.csv", "km_landmark.png", "km_hazard_ratio_over_time.png", "km_sensitivity.png", "piecewise.csv", "sens_all.csv")))))
cat("script HRs:", round(sh$effect, 3), " app HRs:", round(e$hr_study$effect, 3), "\n")
stopifnot(all(abs(sh$effect - e$hr_study$effect) < 0.01))
ok("exported script (Excel-style file: ';' and decimal commas, % scale, renamed columns) reproduces the app")

# validation messages
chk <- function(label, mod, pattern, p = base_params()) {
  e2 <- mod(chr(ex))
  s2 <- lapply(setNames(names(KM_COLS), names(KM_COLS)), function(w) km_standardize(e2[[w]], w))
  m <- km_validate(s2$curve$data, s2$risk$data, s2$arms$data, p)
  hit <- any(grepl(pattern, c(m$errors, m$warnings), ignore.case = TRUE))
  if (!hit) { cat("MISSED:", label, "\n"); print(c(m$errors, m$warnings)); stop("validation did not flag: ", label) }
}
chk("bad group", function(d) { d$arms$group[1] <- "banana"; d }, "treatment' or 'control")
chk("missing arm", function(d) { d$arms <- d$arms[-1, ]; d }, "at least one treatment arm and one control arm")
chk("text value", function(d) { d$curve$survival[3] <- "x"; d }, "not a number")
chk("wrong scale", function(d) { d$curve$survival <- as.character(100 * as.numeric(d$curve$survival)); d }, "above 1")
chk("too many events", function(d) { d$arms$total_events[1] <- "9999"; d }, "exceeds n_total")
chk("few points", function(d) { cv <- d$curve; d$curve <- cv[!(cv$study_id == "Trial A 2012" & cv$arm == "Intervention" & as.numeric(cv$time) > 3), ]; d }, "curve points")
chk("no risk table", function(d) { d$risk <- d$risk[d$risk$study_id != "Trial B 2015", ]; d }, "no numbers at risk")
chk("rising curve", function(d) { i <- which(d$curve$study_id == "Trial C 2018" & d$curve$arm == "Standard care")[10]; d$curve$survival[i] <- "0.99"; d }, "survival rises")
chk("no n", function(d) { d$arms$n_total[1] <- NA; r <- d$risk; d$risk <- r[!(r$study_id == d$arms$study_id[1] & r$arm == d$arms$arm[1] & r$time == "0"), ]; d }, "no n_total")
chk("risk at 0 differs", function(d) { d$risk$n_risk[1] <- "999"; d }, "differs from n_total")
chk("risk increases", function(d) { d$risk$n_risk[2] <- "9999"; d }, "increase over time")
ok("validation flags: bad group, missing arm, text value, wrong scale, too many events, few points, no risk table, rising curve, no n, risk mismatches")
st5 <- km_standardize(data.frame(a = "x", b = "y"), "curve"); stopifnot(!is.null(st5$error)); print(st5$error); ok("file with wrong columns gives a clear message")

# plots, all options
pdf(NULL)
for (disp in c("survival", "cumulative")) for (rt in c(TRUE, FALSE)) for (cf in c(TRUE, FALSE)) {
  pr <- base_params(display = disp, risk_table = rt, conf = cf, marks = !cf, ypct = rt, ymin = if (rt) 0.3 else 0, xmax = 40, name_tx = "PCI", name_ctrl = "CABG")
  rr <- run_analysis(ex, pr); eval(parse(text = rr$code$plot), rr$env); eval(parse(text = rr$code$by_study), rr$env)
}
dev.off(); ok("pooled and by-study plots draw for every option combination")
png(file.path(out, "km_pooled.png"), width = 9, height = 7, units = "in", res = 110)
eval(parse(text = r$code$plot), r$env); dev.off()
png(file.path(out, "km_by_study.png"), width = 10, height = 3.6, units = "in", res = 110)
eval(parse(text = r$code$by_study), r$env); dev.off()
cat("KM ENGINE TESTS PASSED\n")
