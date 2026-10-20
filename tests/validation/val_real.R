# Validation D: real patient data from R datasets (survival package). The Kaplan-Meier curve and the numbers at risk
# are read from the real individual data as a paper would print them (curve rounded to 3 digits, small reading
# error), the app rebuilds the patients, and its hazard ratio is compared with the Cox model on the real data.
suppressMessages({library(survival); library(IPDfromKM)})
suppressMessages({source("R/helpers.R"); source("R/km_helpers.R"); source("R/km_extra.R")})
set.seed(777)
real_sets <- list(
  list(name = "veteran: lung cancer, standard vs test chemotherapy", d = transform(veteran, g = as.integer(trt == 2)), nm = c("Standard", "Test")),
  list(name = "lung: advanced lung cancer, men vs women (months)", d = transform(lung, time = time / 30.44, status = status - 1, g = as.integer(sex == 2)), nm = c("Men", "Women")),
  list(name = "ovarian: ovarian cancer, cyclophosphamide vs +adriamycin", d = transform(ovarian, time = futime / 30.44, status = fustat, g = as.integer(rx == 2)), nm = c("Cyclophosphamide", "Cyclo + adria")),
  list(name = "colon (death): chemotherapy Obs vs Lev+5FU", d = {x <- subset(colon, etype == 2 & rx != "Lev"); transform(x, time = time / 365.25 * 12, g = as.integer(rx == "Lev+5FU"))}, nm = c("Observation", "Lev+5FU")))
rows <- list()
for (NPTS in c(30, 100)) for (rs in real_sets) {
  d <- rs$d[stats::complete.cases(rs$d[, c("time", "status", "g")]), ]
  maxT <- max(d$time); tg <- seq(0, maxT * 0.98, length.out = NPTS)
  rt <- seq(0, maxT * 0.9, length.out = 6)
  curve <- do.call(rbind, lapply(0:1, function(g) { a <- d[d$g == g, ]
    s <- summary(survfit(Surv(time, status) ~ 1, data = a), times = tg, extend = TRUE)$surv
    s <- pmin(1, pmax(0, round(s + rnorm(length(s), 0, 0.002), 3))); s[1] <- 1
    data.frame(study_id = "Real", arm = rs$nm[g + 1], time = round(tg, 3), survival = s) }))
  risk <- do.call(rbind, lapply(0:1, function(g) { a <- d[d$g == g, ]
    data.frame(study_id = "Real", arm = rs$nm[g + 1], time = round(rt, 3), n_risk = vapply(rt, function(u) sum(a$time >= u), 1)) }))
  arms <- data.frame(study_id = "Real", arm = rs$nm, group = c("control", "treatment"), n_total = as.integer(table(d$g)),
                     total_events = vapply(0:1, function(g) sum(d$status[d$g == g]), 1))
  chr <- lapply(list(curve = curve, risk = risk, arms = arms), function(x) { x[] <- lapply(x, as.character); x })
  p <- km_params(list()); env <- new.env(parent = globalenv())
  for (w in names(KM_COLS)) env[[w]] <- km_standardize(chr[[w]], w)$data
  code <- km_build_code(p)
  ok <- tryCatch({ eval(parse(text = code$prep), env); eval(parse(text = code$analysis), env); TRUE }, error = function(e) { message(rs$name, ": ", conditionMessage(e)); FALSE })
  if (!ok) next
  truth <- summary(coxph(Surv(time, status) ~ g, data = d))$conf.int
  h <- env$hr_study
  mt <- tryCatch(unname(summary(survfit(Surv(time, status) ~ g, data = d))$table[, "median"]), error = function(e) c(NA, NA))
  rows[[length(rows) + 1]] <- data.frame(dataset = rs$name, points = NPTS, n = nrow(d), true_hr = truth[1, 1], app_hr = h$effect[1],
    ratio = h$effect[1] / truth[1, 1], true_ci = sprintf("%.2f-%.2f", truth[1, 3], truth[1, 4]), app_ci = sprintf("%.2f-%.2f", h$ci_lower[1], h$ci_upper[1]),
    ci_covers_true = h$ci_lower[1] <= truth[1, 1] && h$ci_upper[1] >= truth[1, 1])
}
real_validation <- do.call(rbind, rows)
# tolerance: the rebuilt HR within 10% of the real-data HR and its CI covering the real HR
real_validation$pass <- abs(real_validation$ratio - 1) < 0.10 & real_validation$ci_covers_true
print(real_validation, row.names = FALSE)
