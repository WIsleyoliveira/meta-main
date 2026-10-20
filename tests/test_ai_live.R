# One real call to the Claude API (costs about one cent). Runs only when asked:
#   PONDERIS_AI_LIVE=1 ANTHROPIC_API_KEY=sk-ant-... Rscript tests/test_ai_live.R
source("R/helpers.R")
source("R/ai_client.R")
source("R/ai_extract.R")

key <- Sys.getenv("ANTHROPIC_API_KEY")
if (Sys.getenv("PONDERIS_AI_LIVE") != "1" || !nzchar(key)) {
  cat("SKIPPED (set PONDERIS_AI_LIVE=1 and ANTHROPIC_API_KEY)\n")
  quit(save = "no", status = 0)
}

f <- tempfile(fileext = ".pdf")
grDevices::pdf(f, width = 8.5, height = 11)
plot.new()
lines_txt <- c("Trial Z: a randomized trial of drug A versus placebo (2021)",
               "Methods. We randomized 298 adults with sepsis to drug A or placebo.",
               "Results. At 30 days, 12 of 150 patients died in the treatment group",
               "and 20 of 148 in the control group.")
for (i in seq_along(lines_txt)) text(0, 1 - i * 0.06, lines_txt[i], adj = 0, cex = 0.9)
grDevices::dev.off()

r <- ai_run_job(list(path = f, name = "trial_z.pdf", type = "dich", outcome = "30-day mortality",
                     arms = "drug A vs placebo", notes = "", key = key, model = "claude-sonnet-5-5"))
if (!isTRUE(r$ok)) stop("live call failed: ", r$message)
cl <- r$cells
val <- function(k) cl$value[cl$row == 1 & cl$col == k]
sts <- function(k) cl$status[cl$row == 1 & cl$col == k]
print(cl[cl$row == 1, c("col", "value", "page", "status")])
stopifnot(val("events_tx") == "12", val("n_tx") == "150", val("events_ctrl") == "20", val("n_ctrl") == "148")
stopifnot(all(vapply(c("events_tx", "n_tx", "events_ctrl", "n_ctrl"), sts, "") == "verified"))
cat(sprintf("tokens: %d in, %d out, about $%.3f\n", r$usage$input_tokens, r$usage$output_tokens, ai_cost(r$usage, "claude-sonnet-5-5")))
cat("AI LIVE TEST PASSED\n")
