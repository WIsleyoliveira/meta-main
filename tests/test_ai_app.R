# AI panel and PDF extraction in the Shiny server (shiny::testServer), with a fake transport:
# no network, no key, no cost.
suppressMessages({library(shiny); library(meta)})
source("R/helpers.R")
ok <- function(name) cat("ok:", name, "\n")
app <- shinyAppDir(".")
Sys.unsetenv(c("ANTHROPIC_API_KEY", "PONDERIS_AI_MAX_CALLS"))
html <- function(x) paste(as.character(x$html %||% x), collapse = "")

# ---------------------------- AI panel ----------------------------
testServer(app, {
  stopifnot(!ai$ready(), grepl("key", ai$reason(), ignore.case = TRUE))
  ok("not ready by default")

  session$setInputs(ai_key = "sk-ant-x", ai_save_key = 1)
  stopifnot(!ai$ready(), grepl("consent|agree", ai$reason(), ignore.case = TRUE))
  ok("consent required")

  session$setInputs(ai_consent = TRUE)
  stopifnot(ai$ready(), is.infinite(ai$calls_left()), ai$key() == "sk-ant-x", ai$model() == "claude-opus-5-5")
  ok("user key + consent")

  ai$record_usage(list(input_tokens = 1e6, output_tokens = 0), "claude-opus-5-5")
  session$flushReact()
  stopifnot(grepl("$4.00", output$ai_usage, fixed = TRUE))
  ok("usage")

  all_out <- paste(output$ai_usage, html(output$ai_status), html(output$ai_badge))
  stopifnot(!grepl("sk-ant-x", all_out, fixed = TRUE))
  ok("key never rendered")

  session$setInputs(ai_model = "claude-sonnet-5-5")
  stopifnot(ai$model() == "claude-sonnet-5-5")
  session$setInputs(ai_model = "rm -rf")
  stopifnot(ai$model() == "claude-opus-5-5")
  ok("model whitelisted")

  session$setInputs(ai_forget_key = 1)
  stopifnot(!ai$ready(), identical(ai$key(), ""))
  ok("forget key")
})

Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-server", PONDERIS_AI_MAX_CALLS = "2")
testServer(app, {
  session$setInputs(ai_consent = TRUE)
  stopifnot(ai$ready(), ai$calls_left() == 2, ai$key() == "sk-ant-server")
  ai$record_usage(list(input_tokens = 10, output_tokens = 10), "claude-opus-5-5")
  ai$record_usage(NULL, "claude-opus-5-5")
  stopifnot(ai$calls_left() == 0, !ai$ready(), grepl("limit", ai$reason()))
  stopifnot(!grepl("sk-ant-server", html(output$ai_status), fixed = TRUE))
  ok("server key and cap")
})
Sys.unsetenv(c("ANTHROPIC_API_KEY", "PONDERIS_AI_MAX_CALLS"))

cat("AI PANEL TESTS PASSED\n")
