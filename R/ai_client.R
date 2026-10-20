# Claude API client for the optional AI features. Plain functions, no Shiny.
#
# Every request asks for JSON that follows a schema (structured outputs), so the app never
# has to interpret free text, and nothing the model returns is ever run as code.
# The transport is a plain function (body, key) -> list(status, body_text); the tests pass a
# fake one, so they need no network and no key.

AI_URL <- "https://api.anthropic.com/v1/messages"
AI_MODELS <- c("Claude Opus 5.5 (most accurate)" = "claude-opus-5-5",
               "Claude Sonnet 5.5 (cheaper)" = "claude-sonnet-5-5")
# US dollars per million tokens (input, output); shown as an estimate
AI_PRICES <- list("claude-opus-5-5" = c(4, 20), "claude-sonnet-5-5" = c(2, 10))
AI_PKGS <- c("httr2", "jsonlite", "mirai", "promises", "pdftools")
AI_MAX_PDF_BYTES <- 32 * 1024^2
AI_MAX_PDF_PAGES <- 600

ai_available <- function() AI_PKGS[!vapply(AI_PKGS, requireNamespace, logical(1), quietly = TRUE)]

ai_text_block <- function(txt) list(type = "text", text = as.character(txt))

ai_pdf_block <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  list(type = "document", source = list(type = "base64", media_type = "application/pdf",
                                        data = gsub("[\r\n]", "", jsonlite::base64_enc(bytes))))
}

ai_body <- function(model, system, content, schema, max_tokens = 16000, effort = "high") {
  list(model = model, max_tokens = max_tokens, system = system,
       messages = list(list(role = "user", content = content)),
       output_config = list(effort = effort, format = list(type = "json_schema", schema = schema)),
       # on a policy decline the API re-runs the request on a fallback model inside the same call
       fallbacks = "default")
}

ai_http_transport <- function(body, key) {
  json <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA)
  req <- httr2::request(AI_URL) |>
    httr2::req_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01",
                       `anthropic-beta` = "server-side-fallback-2026-07-01", .redact = "x-api-key") |>
    httr2::req_body_raw(json, type = "application/json") |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE,
                     is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504, 529)) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  # no answer at all: status 0, and whether the wait ran out (httr2 wraps the curl error as `parent`)
  if (inherits(resp, "error")) {
    timeout <- inherits(resp, "curl_error_operation_timedout") || inherits(resp$parent, "curl_error_operation_timedout")
    return(list(status = 0L, body_text = "", error = if (timeout) "timeout" else "network"))
  }
  list(status = httr2::resp_status(resp),
       body_text = if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else "")
}

ai_redact <- function(txt, key = "") {
  txt <- as.character(txt)
  if (length(key) == 1 && !is.na(key) && nzchar(key)) txt <- gsub(key, "[key hidden]", txt, fixed = TRUE)
  gsub("sk-ant-[A-Za-z0-9_-]+", "[key hidden]", txt, perl = TRUE)
}

AI_ERRORS <- c(
  no_key = "No API key: add your Anthropic key in the AI panel.",
  bad_key = "The API key was refused. Check it in the AI panel.",
  rate_limited = "Too many requests to the AI service. Wait a minute and try again.",
  overloaded = "The AI service is busy or unavailable. Try again in a few minutes.",
  too_large = "This PDF is too large for the AI (more than 32 MB or 600 pages). Split it or send only the relevant pages.",
  bad_pdf = "This file could not be read as a PDF.",
  bad_request = "The AI service rejected the request.",
  refusal = "The AI declined to process this document.",
  truncated = "The AI answer was cut off before the end, so it was not used. Try again, or send fewer pages.",
  bad_json = "The AI answer could not be read, so it was not used. Try again.",
  network = "Could not reach the AI service. Check the internet connection.",
  timeout = "The AI service took too long to answer (more than 5 minutes). Try again, or send fewer pages.",
  limit_reached = "The AI limit for this session was reached.")

ai_fail <- function(code, detail = "", key = "") {
  msg <- AI_ERRORS[[code]]
  if (nzchar(detail)) msg <- paste(msg, ai_redact(detail, key))
  list(ok = FALSE, error = code, message = msg)
}

ai_parse <- function(resp, key = "") {
  st <- as.integer(resp$status)
  if (st != 200L) {
    api_msg <- tryCatch(jsonlite::fromJSON(resp$body_text)$error$message, error = function(e) NULL)
    code <- if (st == 0L) (if (identical(resp$error, "timeout")) "timeout" else "network") else if (st %in% c(401L, 403L)) "bad_key" else if (st == 429L) "rate_limited"
            else if (st == 413L) "too_large" else if (st >= 500L) "overloaded" else "bad_request"
    return(ai_fail(code, if (code == "bad_request" && is.character(api_msg)) api_msg[1] else "", key))
  }
  m <- tryCatch(jsonlite::fromJSON(resp$body_text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(m)) return(ai_fail("bad_json"))
  # stop_reason first: a refused or truncated answer may not follow the schema
  if (identical(m$stop_reason, "refusal")) return(ai_fail("refusal"))
  if (identical(m$stop_reason, "max_tokens")) return(ai_fail("truncated"))
  txt <- NULL
  for (b in m$content) if (identical(b$type, "text")) { txt <- b$text; break }
  data <- if (is.null(txt)) NULL else tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(data)) return(ai_fail("bad_json"))
  list(ok = TRUE, data = data,
       usage = list(input_tokens = m$usage$input_tokens %||% 0, output_tokens = m$usage$output_tokens %||% 0),
       model = m$model %||% "")
}

ai_call <- function(key, model, system, content, schema, max_tokens = 16000, effort = "high", transport = ai_http_transport) {
  if (length(key) != 1 || is.na(key) || !nzchar(key)) return(ai_fail("no_key"))
  resp <- tryCatch(transport(ai_body(model, system, content, schema, max_tokens, effort), key),
                   error = function(e) list(status = 0L, body_text = ""))
  ai_parse(resp, key)
}

# NULL when the PDF can be sent, otherwise the error to show
ai_pdf_check <- function(path) {
  if (file.size(path) > AI_MAX_PDF_BYTES) return(ai_fail("too_large"))
  pages <- tryCatch(pdftools::pdf_info(path)$pages, error = function(e) NA)
  if (is.na(pages)) return(ai_fail("bad_pdf"))
  if (pages > AI_MAX_PDF_PAGES) return(ai_fail("too_large"))
  NULL
}

ai_cost <- function(usage, model) {
  p <- AI_PRICES[[model]] %||% AI_PRICES[["claude-opus-5-5"]]
  (usage$input_tokens * p[1] + usage$output_tokens * p[2]) / 1e6
}
