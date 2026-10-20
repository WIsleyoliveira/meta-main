# AI features without network: the Claude API client, the extraction schema and parsing,
# and the local check of quotes and numbers against the PDF text. A fake transport stands
# in for the API, so these tests cost nothing and need no key.
source("R/helpers.R")
source("R/ai_client.R")
source("R/ai_extract.R")

ok <- function(name) cat("ok:", name, "\n")
fx <- function(f) paste(readLines(file.path("tests/fixtures/ai", f), warn = FALSE), collapse = "\n")
fake <- function(status, body_text = "") { n <- 0; f <- function(body, key) { n <<- n + 1; list(status = as.integer(status), body_text = body_text) }; environment(f)$calls <- function() n; f }
calls <- function(tr) environment(tr)$calls()
one_page_pdf <- function(txt = "x") { f <- tempfile(fileext = ".pdf"); grDevices::pdf(f); plot.new(); text(0.5, 0.5, txt); grDevices::dev.off(); f }

# ---- request body ----
b <- ai_body("claude-opus-5-5", "sys", list(ai_text_block("hi")), list(type = "object"))
stopifnot(b$model == "claude-opus-5-5", identical(b$fallbacks, "default"),
          b$output_config$format$type == "json_schema", b$output_config$effort == "high",
          is.null(b$thinking), b$messages[[1]]$role == "user", b$system == "sys", b$max_tokens == 16000)
ok("body: model, fallbacks, format")

f <- one_page_pdf()
blk <- ai_pdf_block(f)
stopifnot(blk$type == "document", blk$source$media_type == "application/pdf", !grepl("\n", blk$source$data),
          identical(jsonlite::base64_dec(blk$source$data), readBin(f, "raw", file.size(f))))
ok("pdf block")

# ---- parsing ----
key <- "sk-ant-test123"
r <- ai_call(key, "claude-opus-5-5", "sys", list(ai_text_block("hi")), list(type = "object"), transport = fake(200, fx("ok_dich.json")))
stopifnot(isTRUE(r$ok), r$data$studies[[1]]$events_tx$value == 12, r$usage$input_tokens == 15000, r$model == "claude-opus-5-5")
ok("parse ok")

err_of <- function(status, body = "") ai_call(key, "claude-opus-5-5", "sys", list(), list(), transport = fake(status, body))
cases <- list(list(401, "", "bad_key"), list(403, "", "bad_key"), list(429, "", "rate_limited"), list(529, "", "overloaded"),
              list(500, "", "overloaded"), list(413, "", "too_large"), list(0, "", "network"),
              list(200, fx("refusal.json"), "refusal"), list(200, fx("max_tokens.json"), "truncated"), list(200, fx("bad_json.json"), "bad_json"))
for (cs in cases) {
  r <- err_of(cs[[1]], cs[[2]])
  if (!(identical(r$ok, FALSE) && identical(r$error, cs[[3]]) && is.character(r$message) && nchar(r$message) > 10))
    stop(sprintf("status %s: expected %s, got %s", cs[[1]], cs[[3]], r$error %||% "ok"))
}
ok("parse errors")

tr <- fake(200, fx("ok_dich.json"))
r <- ai_call("", "claude-opus-5-5", "sys", list(), list(), transport = tr)
stopifnot(identical(r$ok, FALSE), r$error == "no_key", calls(tr) == 0)
r <- ai_call(NULL, "claude-opus-5-5", "sys", list(), list(), transport = tr)
stopifnot(r$error == "no_key", calls(tr) == 0)
ok("no key")

r <- err_of(401, '{"type":"error","error":{"type":"authentication_error","message":"invalid x-api-key sk-ant-test123"}}')
stopifnot(!grepl("sk-ant-test123", r$message, fixed = TRUE))
stopifnot(!grepl("sk-ant-other99", ai_redact("bad key sk-ant-other99 here", key), fixed = TRUE),
          !grepl(key, ai_redact(paste("x", key), key), fixed = TRUE))
ok("redaction")

stopifnot(isTRUE(all.equal(ai_cost(list(input_tokens = 1e6, output_tokens = 1e6), "claude-opus-5-5"), 24)),
          isTRUE(all.equal(ai_cost(list(input_tokens = 1e6, output_tokens = 1e6), "claude-sonnet-5-5"), 12)))
ok("cost")

stopifnot(is.null(ai_pdf_check(f)))
big <- tempfile(fileext = ".pdf"); writeBin(raw(33 * 1024^2), big)
r <- ai_pdf_check(big)
stopifnot(identical(r$ok, FALSE), r$error == "too_large")
ok("pdf check")

seen <- NULL
httr2::local_mocked_responses(function(req) {
  seen <<- req
  httr2::response(200, headers = list(`content-type` = "application/json"), body = charToRaw(fx("ok_dich.json")))
})
r <- ai_call(key, "claude-sonnet-5-5", "sys", list(ai_text_block("hi")), list(type = "object"))
h <- seen$headers
sent <- jsonlite::fromJSON(as.character(seen$body$data), simplifyVector = FALSE)
stopifnot(isTRUE(r$ok), seen$url == "https://api.anthropic.com/v1/messages",
          unclass(h$`anthropic-version`) == "2023-06-01", unclass(h$`anthropic-beta`) == "server-side-fallback-2026-07-01",
          sent$model == "claude-sonnet-5-5", sent$fallbacks == "default", sent$messages[[1]]$content[[1]]$text == "hi",
          sent$output_config$format$type == "json_schema")
ok("http transport: url, headers, json body")
pol <- seen$policies
stopifnot(isTRUE(pol$retry_on_failure), pol$retry_max_tries == 3,
          all(vapply(c(429, 500, 502, 503, 504, 529), function(s) pol$retry_is_transient(httr2::response(s)), logical(1))),
          !pol$retry_is_transient(httr2::response(400)))
ok("http transport: retries connection failures and 429/5xx")

httr2::local_mocked_responses(function(req) httr2::response(401))
r <- ai_call(key, "claude-opus-5-5", "sys", list(), list())
stopifnot(identical(r$error, "bad_key"))
ok("http transport: error with empty body")

stopifnot(is.character(ai_available()), length(ai_available()) == 0)
ok("packages available")

cat("AI CLIENT TESTS PASSED\n")

# ==================== extraction: schema, prompt, parsing ====================
walk <- function(x, f) { f(x); if (is.list(x)) for (el in x) walk(el, f) }
for (type in names(SPEC)) {
  sc <- ai_extract_schema(type)
  walk(sc, function(x) if (is.list(x) && identical(x$type, "object")) {
    stopifnot(identical(x$additionalProperties, FALSE), setequal(unlist(x$required), names(x$properties)))
  })
  bad <- character(0)
  walk(sc, function(x) if (is.list(x)) bad <<- c(bad, intersect(names(x), c("minimum", "maximum", "minLength", "maxLength", "multipleOf"))))
  stopifnot(!length(bad))
  stopifnot(identical(names(sc$properties$studies$items$properties), c("study_id", "year", "subgroup", names(SPEC[[type]]$cols))))
}
ok("schema strict")

js <- as.character(jsonlite::toJSON(ai_extract_schema("cont"), auto_unbox = TRUE))
stopifnot(grepl('"additionalProperties":false', js, fixed = TRUE), grepl('"$defs"', js, fixed = TRUE),
          grepl('"required":["studies","ambiguities"]', js, fixed = TRUE))
ok("schema serializes")

stopifnot(grepl("Treatment: SD", ai_extract_system("cont"), fixed = TRUE), grepl("verbatim", ai_extract_system("dich")))
ok("system names columns")

pdf1 <- one_page_pdf("Trial Z")
r <- ai_extract_one(pdf1, "trialz.pdf", "dich", "30-day mortality", "", "", key, "claude-opus-5-5", transport = fake(200, fx("ok_dich.json")))
cl <- r$cells
stopifnot(isTRUE(r$ok), r$name == "trialz.pdf", nrow(cl) == 2 * length(template_cols("dich")),
          setequal(unique(cl$col), template_cols("dich")),
          cl$value[cl$row == 1 & cl$col == "n_tx"] == "150", cl$page[cl$row == 1 & cl$col == "n_tx"] == 3L,
          is.na(cl$value[cl$row == 1 & cl$col == "subgroup"]), cl$value[cl$row == 1 & cl$col == "year"] == "2021",
          all(is.na(cl$status)), length(r$ambiguities) == 1, r$usage$input_tokens == 15000)
ok("extract one ok")

r <- ai_extract_one(pdf1, "review.pdf", "dich", "", "", "", key, "claude-opus-5-5", transport = fake(200, fx("zero_rows.json")))
stopifnot(isTRUE(r$ok), nrow(r$cells) == 0, grepl("Review article", r$ambiguities[1]))
ok("extract one zero rows")

tr <- fake(200, fx("ok_dich.json"))
r <- ai_extract_one(big, "big.pdf", "dich", "", "", "", key, "claude-opus-5-5", transport = tr)
stopifnot(identical(r$ok, FALSE), r$error == "too_large", calls(tr) == 0, r$name == "big.pdf")
r <- ai_extract_one(pdf1, "x.pdf", "dich", "", "", "", key, "claude-opus-5-5", transport = fake(529))
stopifnot(identical(r$ok, FALSE), r$error == "overloaded", nzchar(r$message))
ok("extract one errors")

got <- NULL
cap <- function(body, key) { got <<- body; list(status = 200L, body_text = fx("ok_dich.json")) }
invisible(ai_extract_one(pdf1, "x.pdf", "dich", "30-day mortality", "PCI vs CABG", "use ITT", key, "claude-opus-5-5", transport = cap))
ct <- got$messages[[1]]$content
stopifnot(ct[[1]]$type == "document", ct[[2]]$type == "text", grepl("30-day mortality", ct[[2]]$text, fixed = TRUE),
          grepl("PCI vs CABG", ct[[2]]$text, fixed = TRUE), grepl("use ITT", ct[[2]]$text, fixed = TRUE),
          identical(got$output_config$format$schema, ai_extract_schema("dich")))
ok("request carries PDF and outcome")

cat("AI EXTRACTION PARSING TESTS PASSED\n")

# ==================== extraction: local verification ====================
cells1 <- function(value, quote, page = 1L, col = "events_tx") data.frame(row = 1L, col = col, value = value, page = page,
  quote = quote, note = "", status = NA_character_, stringsAsFactors = FALSE)
st_of <- function(value, quote, pages, page = 1L, col = "events_tx") ai_verify(cells1(value, quote, page, col), pages)$status

stopifnot(st_of("12", "mortality was 12/150", "Overall mortal-\nity was 12/150 at day 30") == "verified")
ok("verify hyphenation")
stopifnot(st_of("0.85", "significant 0.85", "a signiﬁcant 0,85 reduction") == "verified")
ok("verify ligature")
stopifnot(st_of("-1.2", "MD -1.2", "pooled MD −1.2 points") == "verified")
ok("verify minus")
stopifnot(st_of("12", "12 of 150", c("intro", "methods", "12 of 150 died"), page = 2L) == "verified")
ok("verify adjacent page")
stopifnot(st_of("12", "12 of 150", c("12 of 150 died", "x", "y", "z"), page = 4L) == "verified")
ok("verify quote on another page")
stopifnot(st_of("13", "12 of 150", "12 of 150 died") == "quote_only")
ok("verify value missing")
stopifnot(st_of("12", "15 of 150", "12 of 150 died") == "not_found", st_of("12", "", "12 of 150 died") == "not_found")
ok("verify not found")
stopifnot(st_of("12", "12 of 150", c("", "  \n ")) == "unverifiable", st_of(NA_character_, "", c("", "")) == "empty")
ok("verify scanned")
stopifnot(st_of(NA_character_, "", "12 of 150") == "empty")
ok("verify empty")
stopifnot(st_of("150", "Deaths 12 150", "Table 2\nDeaths     12\n     150\n") == "verified")
ok("verify: table cells across lines")
stopifnot(st_of("Smith 2019", "Smith et al. randomized", "Smith et al. randomized 300 patients", col = "study_id") == "verified")
ok("verify text cell needs only the quote")

stopifnot(ai_num_in("1234", "n = 1,234"), ai_num_in("12", "12.0%"), ai_num_in("0.5", "0,5"), ai_num_in("-1.2", "md -1.2"),
          !ai_num_in("12", "112"), !ai_num_in("12", "1.2"), !ai_num_in("-15", "12-15 days"), ai_num_in("15", "12-15 days"))
ok("num formats")

pdf2 <- tempfile(fileext = ".pdf"); grDevices::pdf(pdf2); plot.new(); text(0.5, 0.5, "Events 12 of 150 in the treatment arm"); grDevices::dev.off()
stopifnot(ai_verify(cells1("12", "Events 12 of 150", 1L), pdftools::pdf_text(pdf2))$status == "verified")
ok("real pdf")

w1 <- ai_cells_wide(r1 <- ai_extract_one(pdf1, "trialz.pdf", "dich", "", "", "", key, "claude-opus-5-5", transport = fake(200, fx("ok_dich.json")))$cells, "dich")
stopifnot(identical(names(w1), template_cols("dich")), nrow(w1) == 2, w1$events_tx[1] == "12", w1$study_id[2] == "Trial Z 2021 (elderly)")
ok("cells wide")

wd <- data.frame(study_id = "Bad 2020", events_tx = "200", n_tx = "150", events_ctrl = "5", n_ctrl = "100", year = NA, subgroup = NA)
wn <- ai_check_rows(wd, "dich")
stopifnot(length(wn) == 1, grepl("Bad 2020", wn))
wg <- data.frame(study_id = "Odd 2018", effect = "2", ci_lower = "0.5", ci_upper = "1.5", se = NA, year = NA, subgroup = NA)
stopifnot(length(ai_check_rows(wg, "gen")) == 1)
wc <- data.frame(study_id = "Sd 2017", mean_tx = "1", sd_tx = "0", n_tx = "10", mean_ctrl = "2", sd_ctrl = "1", n_ctrl = "10", year = NA, subgroup = NA)
stopifnot(length(ai_check_rows(wc, "cont")) == 1, length(ai_check_rows(w1, "dich")) == 0)
ok("consistency")

# review fixes
stopifnot(st_of("298", "a double-blind, placebo-controlled trial of 298 adults", "a double-\nblind, placebo-\ncontrolled trial of 298 adults") == "verified")
ok("verify: compound words hyphenated at a line break")
v <- ai_verify(cells1("12", "12 of 150", 1L), c("intro", "x", "y", "z", "w", "12 of 150 died"))
stopifnot(v$status == "verified", v$page == 6L, grepl("page 1", v$note), grepl("page 6", v$note))
ok("verify: page corrected when the quote is on another page")
stopifnot(ai_num_in("0.35", "HR 0\u00b735"), !ai_num_in("3", "HR 3\u00b75"), ai_num_in("0.05", "p < .05"),
          st_of("0.35", "HR 0\u00b735", "the hazard ratio was HR 0\u00b735") == "verified")
ok("num formats: mid-dot decimal and leading point")

# minor review fixes
wt <- data.frame(study_id = "Tot 2019", events_tx = "10", n_tx = "150", events_ctrl = "12", n_ctrl = "100", year = NA, subgroup = NA)
q_far <- data.frame(study_id = "", events_tx = "", n_tx = "A total of 400 patients were randomized: 150 to PCI and 100 to CABG",
                    events_ctrl = "", n_ctrl = "A total of 400 patients were randomized: 150 to PCI and 100 to CABG", year = "", subgroup = "")
q_near <- q_far; q_near$n_tx <- q_near$n_ctrl <- "A total of 252 patients were randomized: 150 to PCI and 100 to CABG"
q_arm <- q_far; q_arm$n_tx <- "a total of 150 patients in the PCI group"; q_arm$n_ctrl <- "100 patients received CABG"
wn <- ai_check_rows(wt, "dich", q_far)
stopifnot(length(wn) == 1, grepl("Tot 2019", wn), grepl("400", wn), grepl("250", wn),
          length(ai_check_rows(wt, "dich", q_near)) == 0, length(ai_check_rows(wt, "dich", q_arm)) == 0,
          length(ai_check_rows(wt, "dich")) == 0)
wtc <- data.frame(study_id = "Tc 2020", mean_tx = "1", sd_tx = "1", n_tx = "150", mean_ctrl = "2", sd_ctrl = "1", n_ctrl = "100", year = NA, subgroup = NA)
qc <- data.frame(study_id = "", mean_tx = "", sd_tx = "", n_tx = "We randomised 400 participants", mean_ctrl = "", sd_ctrl = "",
                 n_ctrl = "", year = "", subgroup = "")
qc2 <- qc; qc2$n_tx <- "150 to the intervention"; qc2$n_ctrl <- "A total of 400 participants"
stopifnot(length(ai_check_rows(wtc, "cont", qc)) == 1, length(ai_check_rows(wtc, "cont", qc2)) == 1)
q_odd <- q_far; q_odd$n_tx <- "150 patients were randomized 1:1"; q_odd$n_ctrl <- "100 patients at a total of 12 centres"
stopifnot(length(ai_check_rows(wt, "dich", q_odd)) == 0)
ok("consistency: arms far from a total in the quote")

cat("AI VERIFICATION TESTS PASSED\n")
