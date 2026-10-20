# "How to read this" panels of the Survival curves area: every reading renders, with real text, and the sentences follow the numbers.
suppressMessages({library(shiny); library(bslib); library(DT); library(meta); library(IPDfromKM); library(survival)})
app <- shinyAppDir(".")
ok <- function(c, m) { cat(if (isTRUE(c)) "OK   " else "FAIL ", m, "\n"); if (!isTRUE(c)) quit(status = 1) }
testServer(app, {
  session$setInputs(mode = "km", type = "dich", km_tpl_sep = ",", km_scale = "proportion", km_curve_type = "survival",
    km_display = "survival", km_time_unit = "months", km_name_tx = "Drug", km_name_ctrl = "Placebo",
    km_conf = TRUE, km_marks = FALSE, km_risk_table = TRUE, km_ypct = FALSE, km_xmax = NA, km_ymin = 0,
    km_landmarks = "12, 24", km_tau = NA, km_landmark_time = NA, km_cuts = "", km_truncs = "", km_tv_df = "3",
    km_send_src = "reconstructed", km_outcome = "mortality", km_digit_B = 30, km_digit_noise = 0.005)
  session$setInputs(km_load_example = 1)
  session$elapse(2000)
  txt <- function(id) gsub("<[^>]+>", " ", paste(as.character(output[[id]]$html), collapse = " "))
  for (id in c("hw_km_pooled","hw_km_study","hw_km_summary","hw_km_hr","hw_km_nph","hw_km_sens","hw_km_check")) {
    t <- txt(id); ok(nchar(trimws(t)) > 80, paste(id, ":", substr(gsub("\\s+", " ", trimws(t)), 1, 150)))
  }
  for (id in c("hw_km_sens","hw_km_hr","hw_km_nph","hw_km_summary")) cat("\n", id, ":", gsub("\\s+", " ", txt(id)), "\n")
  ok(grepl("Drug", txt("hw_km_pooled")) && grepl("Placebo", txt("hw_km_pooled")), "arm names used")
  ok(grepl("pooled hazard ratio for Drug versus Placebo is 0\\.", txt("hw_km_hr")), "HR sentence")
})
cat("KM HOWTO TESTS PASSED\n")
