# Three real calls to the Claude API (a few cents): PDF extraction, a RoB 2 assessment and a figure reading. Runs only when asked:
#   PONDERIS_AI_LIVE=1 ANTHROPIC_API_KEY=sk-ant-... Rscript tests/test_ai_live.R
source("R/helpers.R")
source("R/ai_client.R")
source("R/ai_extract.R")
source("R/review_helpers.R"); source("R/rob_questions.R"); source("R/rob_algorithms.R"); source("R/ai_rob.R"); source("R/ai_figure.R")

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

# RoB 2: a made-up report in which allocation alternated -> Domain 1 High
g <- tempfile(fileext = ".pdf")
grDevices::pdf(g, width = 8.5, height = 11); plot.new()
rob_lines <- c("Trial Q: drug B versus usual care (2022)",
               "Methods. Patients were allocated alternately to drug B or usual care in order of admission.",
               "Outcome assessors were not told which treatment patients received.",
               "Results. All 210 allocated patients were followed up and analysed in the group to which they were allocated.")
for (i in seq_along(rob_lines)) text(0, 1 - i * 0.06, rob_lines[i], adj = 0, cex = 0.8)
grDevices::dev.off()
rr <- ai_rob_one(list(path = g, pdf_name = "trial_q.pdf", study = "Trial Q 2022", tool = "rob2", variant = "assignment",
                      settings = list(outcome = "mortality", effect = "assignment"), key = key, model = "claude-sonnet-5-5"))
if (!isTRUE(rr$ok)) stop("live RoB call failed: ", rr$message)
jj <- rob_judge("rob2", "assignment", setNames(rr$answers$answer, rr$answers$id))
print(rr$answers[rr$answers$domain == 1, c("id", "answer", "status")])
stopifnot(rr$answers$answer[rr$answers$id == "1.1"] %in% c("N", "PN"), jj$domains[["D1"]] == "High")

# Figure: a Kaplan-Meier plot with a numbers-at-risk table -> the table read exactly
h <- tempfile(fileext = ".png")
ragg::agg_png(h, width = 1000, height = 760, res = 100)
par(mar = c(9, 5, 2, 2))
tt <- seq(0, 36, 0.25)
plot(NA, xlim = c(0, 36), ylim = c(0, 1), xaxs = "i", yaxs = "i", axes = FALSE, xlab = "Months", ylab = "Overall survival")
axis(1, at = seq(0, 36, 12)); axis(2, at = seq(0, 1, 0.2), las = 1); box()
lines(tt, exp(-0.02 * tt), type = "s", col = "#2F5D8A", lwd = 3); lines(tt, exp(-0.04 * tt), type = "s", col = "#B23A48", lwd = 3)
legend("topright", c("Drug B", "Usual care"), col = c("#2F5D8A", "#B23A48"), lwd = 3, bty = "n")
text(30, 0.9, "HR 0.52 (95% CI 0.38-0.71)")
mtext("No. at risk", side = 1, line = 4, adj = 0)
mtext("Drug B", side = 1, line = 5, at = -6, adj = 0); mtext(c(210, 165, 128, 97), side = 1, line = 5, at = seq(0, 36, 12))
mtext("Usual care", side = 1, line = 6, at = -6, adj = 0); mtext(c(205, 130, 82, 49), side = 1, line = 6, at = seq(0, 36, 12))
grDevices::dev.off()
rf <- ai_figure_read(jsonlite::base64_enc(readBin(h, "raw", file.size(h))), 1000, 760, "Trial Q 2022", key, "claude-sonnet-5-5")
if (!isTRUE(rf$ok)) stop("live figure call failed: ", rf$message)
rows <- rf$reading$risk_table$rows; nums <- function(lbl) unlist(Filter(function(r) grepl(lbl, r$label), rows)[[1]]$n)
print(rf$warnings)
stopifnot(all(unlist(rf$reading$risk_table$times) == c(0, 12, 24, 36)), all(nums("Drug") == c(210, 165, 128, 97)), all(nums("Usual") == c(205, 130, 82, 49)),
          length(rf$reading$arms) == 2, isTRUE(all.equal(rf$reading$annotations$hr, 0.52)))
cat("AI LIVE TEST PASSED\n")
