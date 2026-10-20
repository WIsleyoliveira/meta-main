# AI panel and PDF extraction in the Shiny server (shiny::testServer), with a fake transport:
# no network, no key, no cost.
suppressMessages({library(shiny); library(meta)})
source("R/helpers.R"); source("R/ai_client.R"); source("R/ai_extract.R")
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

# ---------------------------- PDF extraction ----------------------------
fx <- function(f) paste(readLines(file.path("tests/fixtures/ai", f), warn = FALSE), collapse = "\n")
mk_pdf <- function(txt) { f <- tempfile(fileext = ".pdf"); grDevices::pdf(f); plot.new(); text(0.5, 0.5, txt); grDevices::dev.off(); f }
pdfs <- function(n) { f <- vapply(seq_len(n), function(i) mk_pdf(paste("Article", i)), "")
  data.frame(name = paste0("article", seq_len(n), ".pdf"), size = file.size(f), type = "application/pdf", datapath = f, stringsAsFactors = FALSE) }
# fake API: answers in order; a number is an HTTP status, a string a fixture file
seq_transport <- function(...) { answers <- list(...); n <- 0
  f <- function(body, key) { n <<- n + 1; a <- answers[[min(n, length(answers))]]
    if (is.numeric(a)) list(status = as.integer(a), body_text = "") else list(status = 200L, body_text = fx(a)) }
  environment(f)$count <- function() n; f }
n_calls <- function(tr) environment(tr)$count()
forest_inputs <- function(session) session$setInputs(type = "dich", tpl_sep = ",", model = "random", tau = "REML", hk = TRUE, pi = FALSE,
  layout = "meta", sort = "none", weights = TRUE, het = TRUE, overall_p = TRUE, details = FALSE,
  arm_e = "Treatment", arm_c = "Control", label_left = "Favours treatment", label_right = "Favours control", smlab = "",
  bias = "Egger", trimfill = FALSE, method = "MH", sm = "RR")
ready_ai <- function(session) session$setInputs(ai_key = "sk-ant-x", ai_save_key = 1, ai_consent = TRUE)
options(ponderis.ai_runner = "sync")

tr <- seq_transport("ok_dich.json", 529)
options(ponderis.ai_transport = tr)
testServer(app, {
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(2), ai_outcome = "30-day mortality", ai_run = 1)
  stopifnot(n_calls(tr) == 2, nrow(ai_x$table()) == 2)
  e <- html(output$ai_errors)
  stopifnot(grepl("article2.pdf", e), grepl("busy", e), grepl("ai_retry", e), !grepl("article1.pdf", e))
  stopifnot(grepl("30 days", html(output$ai_ambig)), !grepl(" :", html(output$ai_ambig), fixed = TRUE))
  ok("two pdfs one fails")

  st <- ai_x$cells()
  stopifnot(all(!is.na(st$status)), "events_tx" %in% st$col)
  session$setInputs(ai_table_cells_selected = matrix(c(1, 2), ncol = 2))
  ev <- html(output$ai_evidence)
  stopifnot(grepl("12 of 150", ev), grepl("page 3", ev, ignore.case = TRUE), grepl("Treatment: events", ev), !grepl("events_tx", ev))
  ok("evidence box")

  session$setInputs(ai_use = 1)
  # the subgroup column is empty in every row, so it is left out (an empty column would show as a subgroup "Not specified")
  stopifnot(raw()$name == "ai_extracted.csv", identical(names(raw()$data), setdiff(template_cols("dich"), "subgroup")), nrow(raw()$data) == 2)
  d <- raw()$data; g <- guess_columns(c("study", "year", "subgroup", names(SPEC$dich$cols)), names(d))
  do.call(session$setInputs, c(list(map_study = g[["study"]], map_year = g[["year"]], map_group = g[["subgroup"]]),
                               setNames(as.list(g[names(SPEC$dich$cols)]), paste0("map_", names(SPEC$dich$cols)))))
  Sys.sleep(0.6); session$elapse(1000); session$flushReact()
  stopifnot(isTRUE(fit()$ok), grepl("large language model", html(output$methods_txt)), is.null(fit()$env$grp) || length(unique(fit()$env$grp)) <= 1)
  ok("use rows feeds forest")

  f <- output$ai_dl_csv
  back <- read_user_csv(f)$data
  stopifnot(identical(names(back), template_cols("dich")), back$n_tx[1] == "150", nrow(back) == 2)
  ok("csv download")

  session$setInputs(ai_table_cell_edit = data.frame(row = 1, col = 2, value = "12a"))
  stopifnot(ai_x$cells()$status[ai_x$cells()$col == "events_tx"][1] == "edited", ai_x$table()$events_tx[1] == "12a")
  session$setInputs(ai_use = 2); Sys.sleep(0.6); session$elapse(1000); session$flushReact()
  f <- fit(); stopifnot(identical(f$ok, FALSE), length(f$errors) >= 1)
  ok("edited non-number")
})

tr <- seq_transport("zero_rows.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(1), ai_run = 1)
  e <- html(output$ai_errors)
  stopifnot(grepl("No study rows found in this PDF", e), grepl("Review article", e), nrow(ai_x$table()) == 0)
  rv <- html(output$ai_review)
  stopifnot(!grepl("ai_use", rv), grepl("Discard", rv), !grepl(" :", e, fixed = TRUE))
  ok("zero rows")
})

tr <- seq_transport("ok_dich.json", "ok_dich.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(2), ai_run = 1)
  stopifnot(nrow(ai_x$table()) == 4, grepl("appears in 2 PDFs", html(output$ai_warnings)))
  ok("duplicate study")
})

Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-server", PONDERIS_AI_MAX_CALLS = "1")
tr <- seq_transport("ok_dich.json", "ok_dich.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  forest_inputs(session); session$setInputs(ai_consent = TRUE)
  session$setInputs(ai_pdfs = pdfs(2), ai_run = 1)
  e <- html(output$ai_errors)
  stopifnot(n_calls(tr) == 1, nrow(ai_x$table()) == 2, grepl("article2.pdf", e), grepl("Not run:", e), grepl("limit", e))
  ok("stopped mid-run")
})
Sys.unsetenv(c("ANTHROPIC_API_KEY", "PONDERIS_AI_MAX_CALLS"))

tr <- seq_transport("ok_dich.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  forest_inputs(session)
  session$setInputs(ai_pdfs = pdfs(1), ai_run = 1)
  stopifnot(n_calls(tr) == 0, grepl("API key", html(output$ai_extract_reason)))
  ok("disabled when not ready")
})

tr <- seq_transport(529, "ok_dich.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(1), ai_run = 1)
  stopifnot(nrow(ai_x$table()) == 0)
  session$setInputs(ai_retry = 1)
  stopifnot(nrow(ai_x$table()) == 2, !grepl("article1.pdf", html(output$ai_errors)))
  ok("retry")
})

# review fixes
pending <- list()
options(ponderis.ai_runner = function(job, done) pending[[length(pending) + 1]] <<- list(job = job, done = done))
testServer(app, {
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(2), ai_run = 1)
  stopifnot(length(pending) == 1)
  session$setInputs(ai_discard = 1)
  late <- list(ok = TRUE, name = "article1.pdf", cells = ai_cells_long(list(), "dich"), ambiguities = character(0),
               usage = list(input_tokens = 1, output_tokens = 1), called = TRUE)
  pending[[1]]$done(late)
  session$flushReact()
  stopifnot(length(ai_x$results()) == 0, length(pending) == 1)
  session$setInputs(ai_run = 2)
  stopifnot(length(pending) == 2)
  ok("discard during a run")
})
options(ponderis.ai_runner = "sync")

testServer(app, {
  # the panel is rebuilt each time it opens; it must show the consent already given, or opening it would withdraw it
  session$setInputs(ai_consent = TRUE)
  m <- as.character(ai$panel())
  stopifnot(grepl('id="ai_consent" checked', m, fixed = TRUE) || grepl('checked="checked"', m, fixed = TRUE))
  session$setInputs(ai_consent = FALSE)
  stopifnot(!grepl("checked", as.character(ai$panel()), fixed = TRUE))
  ok("reopening the panel keeps consent")
})

# minor review fixes
options(ponderis.ai_transport = seq_transport("total_dich.json"))
testServer(app, {
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(1), ai_run = 1)
  stopifnot(grepl("a total of 400", html(output$ai_warnings)))
  ok("warning: arms far from the total in the quote")
})

testServer(app, {
  # the button itself is disabled while the AI is not ready (not only the reason shown)
  sent <- list()
  session$sendInputMessage <- function(inputId, message) sent[[length(sent) + 1]] <<- c(list(id = inputId), message)
  run_disabled <- function() { m <- Filter(function(x) identical(x$id, "ai_run") && !is.null(x$disabled), sent)
    if (length(m)) m[[length(m)]]$disabled else NA }
  forest_inputs(session); ready_ai(session)
  stopifnot(identical(run_disabled(), FALSE))
  session$setInputs(ai_forget_key = 1)
  stopifnot(identical(run_disabled(), TRUE))
  ok("extract button disabled when not ready")
})
source("R/ai_extract_module.R")
run_btn <- regmatches(u <- as.character(ai_extract_ui()), regexpr('<button[^>]*id="ai_run"[^>]*>', u))
stopifnot(length(run_btn) == 1, grepl(" disabled", run_btn))
ok("extract button starts disabled")

options(ponderis.ai_transport = seq_transport("ok_dich.json"))
testServer(app, {
  # a cell edit replaces the table's data in the browser instead of drawing the table again (scroll and selection stay)
  reg <- 0L; msgs <- list()
  session$registerDataObj <- function(name, data, filterFunc) { reg <<- reg + 1L; "data-url" }
  session$sendCustomMessage <- function(type, message) msgs[[length(msgs) + 1]] <<- list(type = type, message = message)
  forest_inputs(session); ready_ai(session)
  session$setInputs(ai_pdfs = pdfs(1), ai_run = 1)
  invisible(output$ai_table); n0 <- reg
  session$setInputs(ai_table_cells_selected = matrix(c(1, 3), ncol = 2))
  session$setInputs(ai_table_cell_edit = data.frame(row = 1, col = 3, value = "151"))
  session$flushReact()
  reload <- Filter(function(m) identical(m$type, "datatable-calls") && identical(m$message$call$method, "reloadData"), msgs)
  stopifnot(ai_x$table()$n_tx[1] == "151", length(reload) == 1, isFALSE(reload[[1]]$message$call$args[[1]]),
            identical(reload[[1]]$message$call$args[[2]], "none"), reg == n0 + 1L)
  ok("cell edit keeps the table (replaceData)")
})

cat("AI EXTRACTION TESTS PASSED\n")
