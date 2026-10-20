# AI features without network: the Claude API client, the extraction schema and parsing,
# and the local check of quotes and numbers against the PDF text. A fake transport stands
# in for the API, so these tests cost nothing and need no key.
source("R/helpers.R")
source("R/ai_client.R")

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

httr2::local_mocked_responses(function(req) httr2::response(401))
r <- ai_call(key, "claude-opus-5-5", "sys", list(), list())
stopifnot(identical(r$error, "bad_key"))
ok("http transport: error with empty body")

stopifnot(is.character(ai_available()), length(ai_available()) == 0)
ok("packages available")

cat("AI CLIENT TESTS PASSED\n")
