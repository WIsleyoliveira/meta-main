# Reading a Kaplan-Meier figure with AI, without network: schema, prompt, parsing, checks, position scaling;
# then the digitizer's AI block in the Shiny server with a fake transport.
source("R/helpers.R"); source("R/ai_client.R"); source("R/ai_figure.R")
ok <- function(name) cat("ok:", name, "\n")
fx <- function(f) paste(readLines(file.path("tests/fixtures/ai", f), warn = FALSE), collapse = "\n")
fake <- function(status, body_text = "") { f <- function(body, key) { environment(f)$last <- body; list(status = as.integer(status), body_text = body_text) }; f }
key <- "sk-ant-test"

walk <- function(x, f) { f(x); if (is.list(x)) for (el in x) walk(el, f) }
sc <- ai_figure_schema()
walk(sc, function(x) if (is.list(x) && identical(x$type, "object")) stopifnot(identical(x$additionalProperties, FALSE), setequal(unlist(x$required), names(x$properties))))
bad <- character(0); walk(sc, function(x) if (is.list(x)) bad <<- c(bad, intersect(names(x), c("minimum", "maximum", "minLength", "maxLength", "pattern"))))
stopifnot(!length(bad), setequal(names(sc$properties), c("axes", "arms", "risk_table", "annotations", "notes")))
ok("schema strict")

stopifnot(grepl("never estimate", ai_figure_system(), ignore.case = TRUE), grepl("top-left", ai_figure_system()))
ut <- ai_figure_user_text(1000, 700, "Trial S")
stopifnot(grepl("1000", ut), grepl("700", ut), grepl("Trial S", ut))
b <- ai_image_block("QUJD")
stopifnot(b$type == "image", b$source$type == "base64", b$source$media_type == "image/png", b$source$data == "QUJD")
ok("prompt and image block")

tr <- fake(200, fx("figure_two_arms.json"))
r <- ai_figure_read("QUJD", 1000, 700, "Trial S", key, "claude-opus-5-5", transport = tr)
body <- environment(tr)$last
stopifnot(isTRUE(r$ok), isTRUE(r$called), length(r$reading$arms) == 2, r$reading$arms[[1]]$label == "Intervention",
          isTRUE(all.equal(as.numeric(unlist(r$reading$risk_table$times)), c(0, 12, 24, 36))), r$reading$annotations$hr == 0.75,
          body$messages[[1]]$content[[1]]$type == "image", identical(body$output_config$format$schema, ai_figure_schema()))
stopifnot(any(grepl("Control", r$warnings)), any(grepl("36", r$warnings)))   # the dash at 36 months is reported
ok("parse a reading")

r2 <- ai_figure_read("QUJD", 1000, 700, "", key, "claude-opus-5-5", transport = fake(529))
stopifnot(identical(r2$ok, FALSE), r2$error == "overloaded", isTRUE(r2$called))
stopifnot(ai_figure_read("QUJD", 1000, 700, "", "", "claude-opus-5-5", transport = fake(200))$error == "no_key")
ok("errors")

base <- jsonlite::fromJSON(jsonlite::fromJSON(fx("figure_two_arms.json"), simplifyVector = FALSE)$content[[1]]$text, simplifyVector = FALSE)
w <- function(m) ai_figure_check(m)
stopifnot(!length(setdiff(w(base), w(base))))
m <- base; m$risk_table$rows[[1]]$n <- list(250, 260, 120, 60); stopifnot(any(grepl("increase", w(m))))
m <- base; m$risk_table$rows[[1]]$n <- list(250, 180, 120); stopifnot(any(grepl("numbers", w(m))))
m <- base; m$risk_table$times <- list(0, 24, 12, 36); stopifnot(any(grepl("increasing", w(m))))
m <- base; m$axes$x$ticks[[2]]$px <- 50; stopifnot(any(grepl("X axis", w(m))))
m <- base; m$axes$y$ticks[[2]]$py <- 700; stopifnot(any(grepl("Y axis", w(m))))
m <- base; m$axes$x$ticks <- m$axes$x$ticks[1]; stopifnot(any(grepl("two ticks", w(m))))
m <- base; m$axes$x$scale <- "log"; stopifnot(any(grepl("log", w(m))))
m <- base; m$annotations$hr <- 1.2; stopifnot(any(grepl("outside", w(m))))
m <- base; m$risk_table$rows[[2]]$label <- "Placebo"; stopifnot(any(grepl("Placebo", w(m))))
m <- base; m$risk_table$times <- list(6, 12, 24, 36); stopifnot(any(grepl("time 0", w(m))))
ok("checks")

s2 <- ai_figure_scale(base, 2)
stopifnot(s2$axes$x$ticks[[2]]$px == 2 * base$axes$x$ticks[[2]]$px, s2$axes$y$ticks[[3]]$py == 2 * base$axes$y$ticks[[3]]$py)
ok("scale positions to the original image")

cat("AI FIGURE TESTS PASSED\n")
