# Runs every validation and writes tests/validation/VALIDATION_REPORT.md
# Usage from the app folder:  Rscript tests/validation/run_validation.R
# Needs: meta, metafor, metadat, survival, IPDfromKM, estmeansd. The digitizer part is read from
# tests/out/val_digitizer.json when present (made by tests/validation/val_digitizer.js).
suppressMessages(library(jsonlite))
t0 <- Sys.time()
source("tests/validation/val_forest.R"); source("tests/validation/val_convert.R")
Sys.setenv(VAL_KM_REPS = Sys.getenv("VAL_KM_REPS", "40")); source("tests/validation/val_km.R"); source("tests/validation/val_real.R")

md_table <- function(d, digits = 3) {
  f <- function(x) if (is.numeric(x)) ifelse(is.na(x), "", ifelse(x != 0 & abs(x) < 1e-6, "<0.000001", formatC(x, digits = digits, format = "fg", flag = "#"))) else if (is.logical(x)) ifelse(is.na(x), "info", ifelse(x, "PASS", "FAIL")) else as.character(x)
  d[] <- lapply(d, f); d[] <- lapply(d, function(x) sub("\\.$", "", x))
  c(paste0("| ", paste(names(d), collapse = " | "), " |"), paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
    apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

km_summary$rule <- ifelse(grepl("12 points", km_summary$scenario), "informational (coarse digitizing)",
                          ifelse(grepl("Only the initial", km_summary$scenario), "mean abs error of log HR < 0.05", "mean abs error of log HR < 0.03"))
km_summary$pass <- ifelse(grepl("12 points", km_summary$scenario), NA,
                          km_summary$mae_loghr < ifelse(grepl("Only the initial", km_summary$scenario), 0.05, 0.03) & km_summary$ci_cover >= 0.9)

dig <- if (file.exists("tests/out/val_digitizer.json")) fromJSON("tests/out/val_digitizer.json") else NULL
if (!is.null(dig)) {
  dig$pass <- dig$mean_abs_error < 0.01 & dig$p95_abs_error < 0.02
  # the log-axis readout row has no error columns: its text must show X within 2% of 100 and Y within 0.01 of 0.65
  ro <- !is.na(dig$readout)
  if (any(ro)) dig$pass[ro] <- vapply(dig$readout[ro], function(tx) {
    x <- as.numeric(sub(".*X = ([0-9.]+).*", "\\1", tx)); y <- as.numeric(sub(".*Y = ([0-9.]+).*", "\\1", tx))
    isTRUE(abs(x / 100 - 1) < 0.02 && abs(y - 0.65) < 0.01) }, TRUE)
  for (nm in c("points", "mean_abs_error", "p95_abs_error", "max_abs_error", "readout")) if (!nm %in% names(dig)) dig[[nm]] <- NA
}

n_forest <- nrow(forest_validation); p_forest <- sum(forest_validation$pass)
n_conv <- nrow(conv_checks); p_conv <- sum(conv_checks$pass)
km_rules <- km_summary[!is.na(km_summary$pass), ]; n_km <- nrow(km_rules); p_km <- sum(km_rules$pass)
n_dig <- if (is.null(dig)) 0 else nrow(dig); p_dig <- if (is.null(dig)) 0 else sum(dig$pass)
n_real <- nrow(real_validation); p_real <- sum(real_validation$pass)
all_ok <- p_real == n_real && p_forest == n_forest && p_conv == n_conv && p_km == n_km && p_dig == n_dig

L <- c("# Ponderis: validation report", "",
  sprintf("Run on %s with R %s (meta %s, metafor %s, IPDfromKM %s%s). Total time %.0f seconds.", format(Sys.Date()), getRversion(),
          packageVersion("meta"), packageVersion("metafor"), packageVersion("IPDfromKM"),
          if (conv_have_estmeansd()) paste0(", estmeansd ", packageVersion("estmeansd")) else ", estmeansd not installed", as.numeric(difftime(Sys.time(), t0, units = "secs"))), "",
  "## Summary", "",
  "| Part | What was checked | Result |", "|---|---|---|",
  sprintf("| A. Forest-plot engine | Pooled estimates, heterogeneity and confidence limits against the independent package metafor and against published BCG-vaccine values | %d of %d checks pass |", p_forest, n_forest),
  sprintf("| B. Median/IQR to mean/SD | Simulated samples with known mean and SD, all methods, normal and skewed data | %d of %d checks pass |", p_conv, n_conv),
  sprintf("| C. Kaplan-Meier reconstruction | Simulated trials with known patient data: reconstructed hazard ratio against the true Cox hazard ratio | %d of %d scenarios pass (plus one informational) |", p_km, n_km),
  sprintf("| D. Digitizer | Points read from synthetic figures (clean, small, JPEG with censor marks) against the true curves | %s |", if (is.null(dig)) "not run" else sprintf("%d of %d curves pass", p_dig, n_dig)), "",
  if (all_ok) "**Overall: all checks pass.**" else "**Overall: at least one check failed. See the tables below.**", "",
  "## A. Forest-plot engine", "",
  "Each row compares a number produced by the app (through the same code the app runs and exports) with a reference value. Tolerances are shown. The BCG vaccine trials (13 studies) are the standard example in the metafor documentation, and the published values come from there.", "",
  md_table(forest_validation[, c("case", "quantity", "app", "reference", "abs_diff", "tolerance", "pass", "source")], 4), "",
  "Notes: for Hedges' g (SMD) the two packages use slightly different variance formulas, so a small difference (about 0.001 in the pooled effect) is expected and the tolerance is wider. The I-squared in the metafor documentation (92.22%) is based on the REML tau-squared; the meta package uses the Q-based formula (92.12% here), so the check compares against that formula.", "",
  "## B. Median/IQR to mean/SD", "",
  sprintf("Simulation: %d samples per cell. Bias and root-mean-square error (RMSE) are relative to the true sample mean and SD, in percent.", REPS), "",
  md_table(conv_checks[, c("check", "pass", "detail")]), "",
  "Skewness flag, share of samples flagged (n = 200):", "", md_table(conv_flag, 3), "",
  "Full simulation results (n = 200):", "",
  md_table(conv_sim[conv_sim$n == 200, c("distribution", "scenario", "method", "bias_mean_pct", "rmse_mean_pct", "bias_sd_pct", "rmse_sd_pct")], 3), "",
  "Reading it: on symmetric data every method is close. On skewed data Wan and Luo underestimate the SD badly, which is why the app highlights a skewed-data method when the reported values look asymmetric. The skewness flag compares the reported values with what normal data would give (an approximate p-value, flagged below 0.10, about 10% false alarms on normal data). It misses mild skew, especially in small samples; use what you know about the variable as well.", "",
  "## C. Kaplan-Meier reconstruction", "",
  sprintf("Each scenario simulates %d trials (150 or 600 patients per arm, 60 months, censoring), builds the published curve and risk table from the true data, adds digitizing noise (SD 0.002, rounded to 3 decimals), rebuilds individual patient data with the app, and compares the hazard ratio with the Cox hazard ratio of the true data.", max(km_summary$reps)), "",
  md_table(km_summary[, c("scenario", "reps", "mean_true_hr", "mean_rec_hr", "bias_loghr", "mae_loghr", "max_abs_loghr", "within_5pct", "ci_cover", "rule", "pass")], 3), "",
  "mae_loghr is the mean absolute error of the log hazard ratio (0.03 is about 3%). within_5pct is the share of trials with a reconstructed HR within 5% of the true one. ci_cover is how often the reconstructed 95% interval contains the true HR.", "",
  "Reading it: with a risk table and enough points the reconstructed HR is within about 1 to 3% of the truth. With only 12 digitized points the error grows to about 6%; with only the initial number at risk it is about 2%. Digitize 30 or more points per curve.", "",
  "## D. Digitizer", "")
if (!is.null(dig)) L <- c(L, "The digitizer ran in a real browser (headless Chromium). Calibration and tracing were done through the interface, by colour. Error is the vertical difference between each traced point and the true curve, in survival units (0 to 1).", "",
  md_table(dig[, c("figure", "file", "arm", "points", "mean_abs_error", "p95_abs_error", "max_abs_error", "readout", "pass")], 3), "",
  "Figures: A is clean, B is a small image (500 x 350), C is a JPEG (quality 60) with censoring tick marks drawn on the curves, D has a logarithmic x axis (the readout row checks the cursor coordinates at x = 100, y = 0.65).", "") else L <- c(L, "Not run. Start the app and run `node tests/validation/val_digitizer.js`.", "")
L <- c(L, "## E. Real patient data", "",
  "The survival package datasets contain real individual patient data. For each, the curve is read at 30 or 100 time points from the real data (rounded to 3 decimals, reading error SD 0.002) with a six-point risk table and the total events, the app rebuilds the patients, and the hazard ratio is compared with the Cox model on the real data. A pass means the rebuilt hazard ratio is within 10% of the real one and its confidence interval contains the real hazard ratio.", "",
  md_table(real_validation[, c("dataset", "points", "n", "true_hr", "app_hr", "ratio", "true_ci", "app_ci", "ci_covers_true", "pass")], 3), "",
  "Reading it: with 100 points the rebuilt hazard ratio is within about 7% of the real one in every dataset, and usually within 2 to 4%; with 30 points the error is larger (the lung data differ by 6%). More digitized points help. These are not the data of your own meta-analysis: validate that one against its published result before submitting.", "",
  "## What this does and does not show", "",
  "- It shows the app reproduces trusted software and recovers known truth in simulation. That is evidence the code is correct.",
  "- It does not replace checking an actual meta-analysis from your field against its published result. Do that for any analysis you publish.",
  "- Reconstructed data from published curves are estimates. Their accuracy depends on figure quality, the number of points read, and whether the risk table and total events are available.",
  "- The conversion of medians to means is an approximation. Report the method and, when many studies need it, run a sensitivity analysis without them.")
writeLines(L, "tests/validation/VALIDATION_REPORT.md")
cat(sprintf("A %d/%d  B %d/%d  C %d/%d  D %d/%d  E %d/%d  overall %s\n", p_forest, n_forest, p_conv, n_conv, p_km, n_km, p_dig, n_dig, p_real, n_real, if (all_ok) "OK" else "CHECK"))
quit(status = if (all_ok) 0 else 1)
