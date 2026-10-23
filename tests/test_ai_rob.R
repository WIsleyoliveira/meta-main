# Risk-of-bias assistant without network: schema, prompt, parsing, applicability and quote check
# (plain functions), then the Shiny flow with a fake transport.
source("R/helpers.R"); source("R/review_helpers.R")
source("R/ai_client.R"); source("R/ai_extract.R")
source("R/rob_questions.R"); source("R/rob_algorithms.R"); source("R/ai_rob.R")
ok <- function(name) cat("ok:", name, "\n")
fx <- function(f) paste(readLines(file.path("tests/fixtures/ai", f), warn = FALSE), collapse = "\n")
fake <- function(status, body_text = "") { n <- 0; f <- function(body, key) { n <<- n + 1; environment(f)$last <- body; list(status = as.integer(status), body_text = body_text) }; f }
pdf_with <- function(lines) {
  f <- tempfile(fileext = ".pdf"); grDevices::pdf(f, width = 8.5, height = 11)
  plot.new(); text(0.5, 0.5, "Methods page one"); plot.new()
  for (i in seq_along(lines)) text(0, 1 - i * 0.05, lines[i], adj = 0, cex = 0.8)
  grDevices::dev.off(); f
}

# ---- schema ----
walk <- function(x, f) { f(x); if (is.list(x)) for (el in x) walk(el, f) }
for (tv in list(c("rob2", "assignment"), c("rob2", "adherence"), c("robins2", "A"), c("robins2", "B"))) {
  sc <- ai_rob_schema(tv[1], tv[2]); bank <- ai_rob_bank(tv[1], tv[2])
  walk(sc, function(x) if (is.list(x) && identical(x$type, "object")) stopifnot(identical(x$additionalProperties, FALSE), setequal(unlist(x$required), names(x$properties))))
  bad <- character(0); walk(sc, function(x) if (is.list(x)) bad <<- c(bad, intersect(names(x), c("minimum", "maximum", "minLength", "maxLength"))))
  stopifnot(!length(bad), identical(names(sc$properties$answers$properties), vapply(bank, `[[`, "", "id")))
  for (q in bank) stopifnot(setequal(unlist(sc$properties$answers$properties[[q$id]]$properties$answer$enum),
                                     if (is.null(q$when)) q$answers else union(q$answers, "NA")))
}
ok("schema strict for every tool and variant")

sys <- ai_rob_system("rob2", "assignment")
stopifnot(grepl("Was the allocation sequence random?", sys, fixed = TRUE), grepl("If Y/PY/NI to 2.1 or 2.2", sys, fixed = TRUE),
          grepl("never decide", sys, ignore.case = TRUE), grepl("verbatim", sys))
stopifnot(grepl("Strong yes", ai_rob_system("robins2", "A")))
ut <- ai_rob_user_text(list(outcome = "30-day mortality", result = "RR 0.80", effect = "assignment"), "Smith 2019")
stopifnot(grepl("Smith 2019", ut), grepl("30-day mortality", ut), grepl("RR 0.80", ut), grepl("assignment", ut))
ut <- ai_rob_user_text(list(outcome = "stroke", confounders = "age; diabetes", cointerventions = "statins"), "Lee 2020", tool = "robins2")
stopifnot(grepl("age; diabetes", ut), grepl("statins", ut))
stopifnot(grepl("intention-to-treat", ai_rob_user_text(list(effect = "A"), "Lee 2020", tool = "robins2")),
          grepl("per-protocol", ai_rob_user_text(list(effect = "B"), "Lee 2020", tool = "robins2")))
stopifnot(ai_rob_hex("Smith 2019") != ai_rob_hex("Smith-2019"), grepl("^[0-9a-f]+$", ai_rob_hex("Smith 2019 \u00e9")))
ok("prompt names questions, settings and study")

# ---- one study ----
key <- "sk-ant-test"
f <- pdf_with(c("Patients were allocated alternately to the two groups.", "All 300 randomized patients were included in the analysis."))
job <- list(path = f, pdf_name = "smith.pdf", study = "Smith 2019", tool = "rob2", variant = "assignment",
            settings = list(outcome = "mortality", effect = "assignment"), key = key, model = "claude-opus-5-5")
tr <- fake(200, fx("rob2_alternation.json"))
r <- ai_rob_one(job, transport = tr)
a <- r$answers
stopifnot(isTRUE(r$ok), r$study == "Smith 2019", nrow(a) == 22, identical(a$id, vapply(ROB2_Q$assignment, `[[`, "", "id")),
          a$answer[a$id == "1.1"] == "N", isTRUE(r$called), length(r$notes) == 1)
stopifnot(tr(NULL, NULL)$status == 200)  # transport callable
ok("parse one study")

# Review Focus 3: 2.3 and 2.4 do not apply when nobody was aware (2.1 = 2.2 = N): forced to NA whatever the AI said
stopifnot(a$answer[a$id == "2.3"] == "NA", a$answer[a$id == "2.4"] == "NA", isTRUE(a$forced[a$id == "2.3"]))
j <- rob_judge("rob2", "assignment", setNames(a$answer, a$id))
stopifnot(j$domains[["D1"]] == "High", j$domains[["D2"]] == "Low", j$overall == "High")
ok("non-applicable answers forced to NA; Domain 1 High from alternation")

stopifnot(a$status[a$id == "1.1"] == "verified", a$status[a$id == "3.1"] == "verified",
          a$status[a$id == "1.2"] == "not_found", a$status[a$id == "5.1"] == "empty")
ok("quote statuses")

scan <- tempfile(fileext = ".pdf"); grDevices::pdf(scan); plot.new(); grDevices::dev.off()
r2 <- ai_rob_one(modifyList(job, list(path = scan)), transport = fake(200, fx("rob2_alternation.json")))
stopifnot(r2$answers$status[r2$answers$id == "1.1"] == "unverifiable")
ok("scanned pdf unverifiable")

r3 <- ai_rob_one(job, transport = fake(529))
stopifnot(identical(r3$ok, FALSE), r3$error == "overloaded", isTRUE(r3$called))
big <- tempfile(fileext = ".pdf"); writeBin(raw(33 * 1024^2), big)
r4 <- ai_rob_one(modifyList(job, list(path = big)), transport = fake(200, fx("rob2_alternation.json")))
stopifnot(r4$error == "too_large", identical(r4$called, FALSE))
ok("errors")

tr <- fake(200, fx("rob2_alternation.json"))
ai_rob_one(job, transport = tr)
body <- environment(tr)$last
stopifnot(body$messages[[1]]$content[[1]]$type == "document", grepl("Smith 2019", body$messages[[1]]$content[[2]]$text),
          identical(body$output_config$format$schema, ai_rob_schema("rob2", "assignment")))
ok("request carries the PDF, the study and the schema")

# ROBINS-I: the AI's C4 against the variant the reviewer chose
w <- ai_rob_variant_warning("robins2", "A", c(C4 = "Y"))
stopifnot(nzchar(w), grepl("per-protocol", w), !nzchar(ai_rob_variant_warning("robins2", "A", c(C4 = "N"))),
          !nzchar(ai_rob_variant_warning("rob2", "assignment", c())))
ok("robins variant warning")

cat("AI ROB PARSING TESTS PASSED\n")

# ==================== the Risk of bias tab, in the Shiny server ====================
suppressMessages({ library(shiny); library(meta) })
app <- shinyAppDir(".")
html <- function(x) paste(as.character(x$html %||% x), collapse = "")
mk_pdf <- function(txt) { f <- tempfile(fileext = ".pdf"); grDevices::pdf(f); plot.new(); text(0.5, 0.5, txt); grDevices::dev.off(); f }
pdfs <- function(n) { f <- vapply(seq_len(n), function(i) mk_pdf(paste("Patients were allocated alternately to the two groups.", i)), "")
  data.frame(name = paste0("trial", seq_len(n), ".pdf"), size = file.size(f), type = "application/pdf", datapath = f, stringsAsFactors = FALSE) }
seq_transport <- function(...) { answers <- list(...); n <- 0; bodies <- list()
  f <- function(body, key) { n <<- n + 1; bodies[[n]] <<- body; a <- answers[[min(n, length(answers))]]
    if (is.numeric(a)) list(status = as.integer(a), body_text = "") else list(status = 200L, body_text = fx(a)) }
  environment(f)$count <- function() n; environment(f)$sent <- function() bodies; f }
n_calls <- function(tr) environment(tr)$count()
pdf_in <- function(study) paste0("airob_pdf_", ai_rob_hex(study))
link <- function(session, ...) { x <- list(...); do.call(session$setInputs, setNames(unname(x), vapply(names(x), pdf_in, ""))) }
Sys.unsetenv(c("ANTHROPIC_API_KEY", "PONDERIS_AI_MAX_CALLS"))
ready_ai <- function(session) session$setInputs(ai_key = "sk-ant-x", ai_save_key = 1, ai_consent = TRUE)
options(ponderis.ai_runner = "sync")
start <- function(session, lib, studies = "Smith 2019\nJones 2020", n_pdf = 2) {
  ready_ai(session)
  session$setInputs(rob_tool = "rob2", rob_studies = studies, airob_outcome = "mortality", airob_effect = "assignment", airob_pdfs = pdfs(n_pdf))
  vapply(lib$entries(), `[[`, "", "id")
}

tr <- seq_transport("rob2_alternation.json", 529)
options(ponderis.ai_transport = tr)
testServer(app, {
  ids <- start(session, ai_lib)
  link(session, `Smith 2019` = ids[1], `Jones 2020` = ids[2]); session$setInputs(airob_run = 1)
  as <- rev$airob$assessments()
  stopifnot(n_calls(tr) == 2, isTRUE(as[["rob2|Smith 2019"]]$ok), identical(as[["rob2|Jones 2020"]]$ok, FALSE))
  e <- html(output$airob_errors); cards <- html(output$airob_cards)
  stopifnot(grepl("Jones 2020", e), grepl("busy", e), grepl("airob_retry", e), grepl("Smith 2019", cards),
            grepl("Was the allocation sequence random?", cards, fixed = TRUE), grepl("Suggested by the RoB 2 algorithm", cards))
  ok("assess two studies, one fails")

  j <- rev$airob$judgement("rob2|Smith 2019")
  stopifnot(j$domains[["D1"]] == "High", j$overall == "High")
  session$setInputs(airob_answer = list(study = "rob2|Smith 2019", id = "2.1", answer = "Y"))
  j <- rev$airob$judgement("rob2|Smith 2019")
  stopifnot(j$answers[["2.3"]] == "NI", j$domains[["D2"]] == "Some concerns",
            rev$airob$assessments()[["rob2|Smith 2019"]]$answers$edited[rev$airob$assessments()[["rob2|Smith 2019"]]$answers$id == "2.1"])
  session$setInputs(airob_answer = list(study = "rob2|Smith 2019", id = "1.2", answer = "Y"))
  stopifnot(rev$airob$judgement("rob2|Smith 2019")$domains[["D1"]] == "Some concerns")
  ok("edit answer recomputes")

  session$setInputs(airob_override = list(study = "rob2|Smith 2019", domain = "D5", level = "Low", reason = ""))
  stopifnot(is.null(rev$airob$assessments()[["rob2|Smith 2019"]]$overrides$D5))
  session$setInputs(airob_override = list(study = "rob2|Smith 2019", domain = "D5", level = "High", reason = "Outcome switched from the registry."))
  stopifnot(rev$airob$final("rob2|Smith 2019")[["D5"]] == "High", grepl("Outcome switched", html(output$airob_cards)))
  ok("override needs a reason")

  session$setInputs(airob_apply = 1)
  d <- rev$rob_data()
  stopifnot(d$D1[d$study == "Smith 2019"] == "Some concerns", d$D5[d$study == "Smith 2019"] == "High", is.na(d$D1[d$study == "Jones 2020"]))
  ok("apply to the risk-of-bias table")

  session$setInputs(rob_studies = "Smith 2019a\nJones 2020", airob_apply = 2)
  stopifnot(grepl("Smith 2019", html(output$airob_apply_note)), grepl("not in the study list", html(output$airob_apply_note)))
  ok("renamed study is not applied to another row")

  csv <- read.csv(output$dl_airob_csv, stringsAsFactors = FALSE)
  stopifnot(nrow(csv) == 22, all(c("study", "domain", "question_id", "question", "answer", "ai_answer", "edited", "page", "quote", "rationale", "status") %in% names(csv)))
  jd <- read.csv(output$dl_airob_judgements, stringsAsFactors = FALSE)
  stopifnot(nrow(jd) == 6, jd$source[jd$domain == "D5"] == "override", grepl("Outcome switched", jd$support[jd$domain == "D5"]))
  stopifnot(grepl("large language model", html(output$airob_methods)), grepl("RoB 2", html(output$airob_methods)))
  ok("exports and methods text")

  session$setInputs(rob_tool = "robins2")
  stopifnot(!grepl("Smith 2019", html(output$airob_cards)), length(rev$airob$assessments()) == 2)
  session$setInputs(rob_tool = "rob2")
  stopifnot(grepl("Smith 2019", html(output$airob_cards)))
  ok("tool switch keeps but hides")
})

tr <- seq_transport("rob2_alternation.json", "rob2_alternation.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  ids <- start(session, ai_lib, n_pdf = 1)
  link(session, `Smith 2019` = ids[1], `Jones 2020` = ids[1]); session$setInputs(airob_run = 1)
  b <- environment(tr)$sent()
  stopifnot(n_calls(tr) == 2, grepl("Smith 2019", b[[1]]$messages[[1]]$content[[2]]$text), grepl("Jones 2020", b[[2]]$messages[[1]]$content[[2]]$text))
  ok("one PDF for two studies")
})

pending <- list()
options(ponderis.ai_runner = function(job, done) pending[[length(pending) + 1]] <<- list(job = job, done = done))
testServer(app, {
  ids <- start(session, ai_lib)
  link(session, `Smith 2019` = ids[1], `Jones 2020` = ids[2]); session$setInputs(airob_run = 1)
  stopifnot(length(pending) == 1)
  session$setInputs(airob_discard = 1)
  pending[[1]]$done(list(ok = FALSE, error = "network", message = "late", called = TRUE))
  session$flushReact()
  stopifnot(length(rev$airob$assessments()) == 0, length(pending) == 1)
  session$setInputs(airob_run = 2)
  stopifnot(length(pending) == 2)
  ok("discard during a run")
})
options(ponderis.ai_runner = "sync")

testServer(app, {
  session$setInputs(rob_tool = "rob2", rob_studies = "Smith 2019", airob_run = 1)
  stopifnot(grepl("API key", html(output$airob_reason)), length(rev$airob$assessments()) == 0)
  session$setInputs(rob_tool = "nos")
  stopifnot(grepl("RoB 2 and ROBINS-I V2", html(output$airob_reason)))
  ok("disabled when not ready or tool not covered")
})

# ---- review fixes ----
# a question with no NI option that has no valid answer shows "answer needed", never a wrong selected value
tr <- seq_transport("robins2_b3_critical.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  ready_ai(session)
  session$setInputs(rob_tool = "robins2", rob_studies = "Lee 2020", airob_variant = "A", airob_outcome = "stroke", airob_pdfs = pdfs(1))
  link(session, `Lee 2020` = ai_lib$entries()[[1]]$id); session$setInputs(airob_run = 1)
  cards <- html(output$airob_cards)
  sel14 <- regmatches(cards, regexpr('id: \\&quot;1.4\\&quot;[^<]*>(<option[^>]*>[^<]*</option>)+', cards))
  if (!length(sel14)) sel14 <- regmatches(cards, regexpr("1\\.4[^<]*</td>.*?</select>", cards))
  stopifnot(grepl("answer needed", cards), length(sel14) == 1, grepl('selected[^>]*>[^<]*answer needed', sel14))
  csv <- read.csv(output$dl_airob_csv, stringsAsFactors = FALSE)
  stopifnot(csv$answer[csv$question_id == "1.4"] == "", isTRUE(csv$needs_answer[csv$question_id == "1.4"]))
  ok("question without NI shows answer needed")

  # Critical from B3 reaches the table, the figure and GRADE
  stopifnot(rev$airob$final("robins2|Lee 2020")[["D5"]] == "Critical")
  session$setInputs(airob_apply = 1)
  d <- rev$rob_data()
  stopifnot(d$D5[1] == "Critical", rob_overall(d, "robins2") == "Critical")
  ok("preliminary Critical carried to the table")
})

# the PDF choice follows the study name, not its line in the list
tr <- seq_transport("rob2_alternation.json")
options(ponderis.ai_transport = tr)
testServer(app, {
  ids <- start(session, ai_lib, studies = "Alpha 2019\nBeta 2020", n_pdf = 2)
  link(session, `Alpha 2019` = ids[1], `Beta 2020` = ids[2])
  session$setInputs(rob_studies = "Beta 2020", airob_run = 1)
  a <- rev$airob$assessments()
  stopifnot(names(a) == "rob2|Beta 2020", a[[1]]$pdf_name == "trial2.pdf", n_calls(tr) == 1)
  ok("PDF choice keyed by study name")
})

cat("AI ROB APP TESTS PASSED\n")
