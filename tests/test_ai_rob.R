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
  for (q in bank) stopifnot(setequal(unlist(sc$properties$answers$properties[[q$id]]$properties$answer$enum), union(q$answers, "NA")))
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
