# Comparisons built from the saved library: units, pooled sides, warnings, errors, and the result against the true patients.
suppressMessages({library(survival); library(IPDfromKM); library(shiny)})
source("R/helpers.R"); source("R/km_helpers.R"); source("R/km_extra.R"); source("R/dig_module.R")
ok <- function(msg) cat("OK  ", msg, "\n")
chr <- function(l) lapply(l, function(d) { d[] <- lapply(d, as.character); d })
set.seed(11)
sim <- function(n, scale, shape = 1.1, follow = 120) {
  t <- scale * (-log(runif(n)))^(1 / shape); cens <- pmin(follow, rexp(n, 1 / 200))
  data.frame(time = pmin(t, cens), status = as.integer(t <= cens))
}
G <- list(MM = list(n = 900, sc = 110), FM = list(n = 700, sc = 95), FF = list(n = 500, sc = 105), MF = list(n = 750, sc = 80))
ipd <- lapply(G, function(g) sim(g$n, g$sc))
curve <- risk <- arms <- list()
for (a in names(G)) {
  d <- ipd[[a]]; f <- survfit(Surv(time, status) ~ 1, data = d)
  tt <- seq(0, 120, length.out = 40); sv <- summary(f, times = tt, extend = TRUE)$surv
  curve[[a]] <- data.frame(study_id = "Kemna 2016", arm = a, time = as.character(round(tt, 2)), survival = as.character(round(sv, 4)))
  rt <- seq(0, 120, by = 24)
  risk[[a]] <- data.frame(study_id = "Kemna 2016", arm = a, time = as.character(rt), n_risk = as.character(vapply(rt, function(x) sum(d$time >= x), 1)))
  arms[[a]] <- data.frame(study_id = "Kemna 2016", arm = a, group = "", n_total = as.character(nrow(d)), total_events = as.character(sum(d$status)),
                          hr_published = NA_character_, hr_ci_lower = NA_character_, hr_ci_upper = NA_character_,
                          subgroup = c(MM = "male recipient", FM = "male recipient", FF = "female recipient", MF = "female recipient")[[a]])
}
curve <- do.call(rbind, curve); risk <- do.call(rbind, risk); arms <- do.call(rbind, arms); rownames(curve) <- rownames(risk) <- rownames(arms) <- NULL
stopifnot(identical(DIG_COLS$curve, KM_COLS$curve), identical(DIG_COLS$risk, KM_COLS$risk), identical(DIG_COLS$arms[1:8], KM_COLS$arms), identical(names(arms), DIG_COLS$arms), identical(names(dig_empty("arms")), DIG_COLS$arms))
ok("the library has the subgroup column")

cmp <- function(rows) { d <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE); names(d) <- c("unit", "group", "arm"); cbind(comparison = "C", d[, 1:2], study_id = "Kemna 2016", arm = d$arm) }
run <- function(b) {
  ex <- chr(list(curve = b$curve, risk = b$risk, arms = b$arms)); p <- km_params(list())
  env <- new.env(parent = globalenv())
  for (w in names(KM_COLS)) env[[w]] <- km_standardize(ex[[w]], w)$data
  v <- km_validate(env$curve, env$risk, env$arms, p); stopifnot(!length(v$errors))
  code <- km_build_code(p); eval(parse(text = code$prep), env); eval(parse(text = code$analysis), env); env
}
truth <- function(units) {   # units: list(unit = list(tx = c(arms), ct = c(arms)))
  d <- do.call(rbind, lapply(names(units), function(u) rbind(
    data.frame(unit = u, g = 1, do.call(rbind, ipd[units[[u]]$tx])), data.frame(unit = u, g = 0, do.call(rbind, ipd[units[[u]]$ct])))))
  s <- summary(coxph(Surv(time, status) ~ g + strata(unit), data = d))
  unname(s$conf.int[1, "exp(coef)"])
}

# 1. Two units: male recipients (FM vs MM) and female recipients (MF vs FF)
b1 <- dig_build_comparison(cmp(list(c("Men", "treatment", "FM"), c("Men", "control", "MM"), c("Women", "treatment", "MF"), c("Women", "control", "FF"))), curve, risk, arms)
stopifnot(is.null(b1$error), !length(b1$warnings), identical(sort(unique(b1$arms$study_id)), c("Men", "Women")), nrow(b1$arms) == 4)
stopifnot(all(names(b1$arms) == KM_COLS$arms), all(names(b1$curve) == KM_COLS$curve), all(names(b1$risk) == KM_COLS$risk))
e1 <- run(b1); t1 <- truth(list(Men = list(tx = "FM", ct = "MM"), Women = list(tx = "MF", ct = "FF")))
cat("two units: pooled HR", round(e1$hr_pooled$hr, 3), " true", round(t1, 3), "\n")
stopifnot(abs(log(e1$hr_pooled$hr) - log(t1)) < 0.04, nrow(e1$hr_study) == 2)
ok("two units (one per recipient sex): the pooled hazard ratio matches the true patients")

# 2. One unit, each side pooling two curves (mismatch = FM + MF, match = MM + FF)
b2 <- dig_build_comparison(cmp(list(c("Mismatch vs match", "treatment", "FM"), c("Mismatch vs match", "treatment", "MF"),
                                    c("Mismatch vs match", "control", "MM"), c("Mismatch vs match", "control", "FF"))), curve, risk, arms)
stopifnot(is.null(b2$error), nrow(b2$arms) == 4, length(unique(b2$arms$study_id)) == 1)
e2 <- run(b2); t2 <- truth(list(u = list(tx = c("FM", "MF"), ct = c("MM", "FF"))))
cat("pooled sides: HR", round(e2$hr_pooled$hr, 3), " true", round(t2, 3), "\n")
stopifnot(abs(log(e2$hr_pooled$hr) - log(t2)) < 0.04)
stopifnot(nrow(e2$ipd) == sum(vapply(ipd, nrow, 1)), all(table(e2$ipd$group) > 0), length(unique(e2$ipd$arm)) == 4)
ok("pooled sides: the HR matches the true pooled patients, and every patient is counted once")

# 3. The same saved curves, another question (only the two mismatch groups against each other)
b3 <- dig_build_comparison(cmp(list(c("Only mismatch", "treatment", "MF"), c("Only mismatch", "control", "FM"))), curve, risk, arms)
e3 <- run(b3); t3 <- truth(list(u = list(tx = "MF", ct = "FM")))
stopifnot(is.null(b3$error), abs(log(e3$hr_pooled$hr) - log(t3)) < 0.05)
ok("a new comparison reuses the saved curves without digitizing again")

# 4. Warnings and errors
b4 <- dig_build_comparison(cmp(list(c("Men", "treatment", "FM"), c("Men", "control", "MM"), c("Other", "treatment", "MF"), c("Other", "control", "MM"))), curve, risk, arms)
stopifnot(is.null(b4$error), length(b4$warnings) == 1, grepl("more than one unit", b4$warnings))
ok("a curve used in two units gives a warning")
stopifnot(!is.null(dig_build_comparison(cmp(list(c("Men", "treatment", "FM"))), curve, risk, arms)$error))
ok("a unit with only one side is refused")
stopifnot(grepl("same curve twice", dig_build_comparison(cmp(list(c("U", "treatment", "FM"), c("U", "control", "FM"))), curve, risk, arms)$error))
ok("the same curve on both sides of a unit is refused")
stopifnot(grepl("missing the patients", dig_build_comparison(cmp(list(c("U", "treatment", "FM"), c("U", "control", "MM"))), curve, risk, arms[arms$arm != "MM", ])$error))
ok("a curve without its number of patients is refused with a clear message")
stopifnot(!is.null(dig_build_comparison(dig_cmp_empty(), curve, risk, arms)$error))
ok("an empty comparison is refused")
# two curves with the same arm name from different studies in one unit are told apart
c2 <- curve; r2 <- risk; a2 <- arms
c2b <- c2[c2$arm == "MM", ]; c2b$study_id <- "Other 2020"; r2b <- r2[r2$arm == "MM", ]; r2b$study_id <- "Other 2020"; a2b <- a2[a2$arm == "MM", ]; a2b$study_id <- "Other 2020"
cm5 <- data.frame(comparison = "C", unit = "U", group = c("treatment", "control", "control"), study_id = c("Kemna 2016", "Kemna 2016", "Other 2020"), arm = c("FM", "MM", "MM"))
b5 <- dig_build_comparison(cm5, rbind(c2, c2b), rbind(r2, r2b), rbind(a2, a2b))
stopifnot(is.null(b5$error), anyDuplicated(paste(b5$arms$study_id, b5$arms$arm)) == 0, sum(grepl("\\[", b5$arms$arm)) == 2)
ok("equal arm names from different studies in one unit stay distinct")
cat("COMPARE TESTS PASSED\n")
