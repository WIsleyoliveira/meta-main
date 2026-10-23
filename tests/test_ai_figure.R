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

# review fixes (plain functions)
ax <- ai_figure_axes(base)
stopifnot(ax$start == "top", length(ax$x_all) == 6, length(ax$y_all) == 6)
m <- base; m$axes$curve_type <- "cumulative"; stopifnot(ai_figure_axes(m)$start == "bottom")
m <- base; m$axes$y_percent <- TRUE; stopifnot(max(vapply(ai_figure_axes(m)$y_all, function(t) t$value, 0)) == 100)
a1 <- ai_figure_arm(base, 1)
stopifnot(a1$n == "250", identical(a1$others, list("#B23A48")))
m <- base; m$arms[[1]]$colour_hex <- "red"; stopifnot(ai_figure_arm(m, 1)$hex == "")
m <- base; m$arms[[1]]$colour_hex <- "#3366A0FF"; stopifnot(ai_figure_arm(m, 1)$hex == "")
m <- base; m$arms[[2]]$label <- "Control (n=248)"; m$arms[[2]]$risk_label <- "Control"
stopifnot(ai_figure_arm(m, 2)$hr == "", ai_figure_arm(m, 1)$hr == "0.75")
m <- base; m$annotations$reference_arm <- "control group"; stopifnot(ai_figure_arm(m, 2)$hr == "", ai_figure_arm(m, 1)$hr == "0.75")
ok("review fixes: start, all ticks, colours, n at time 0, reference arm")

cat("AI FIGURE TESTS PASSED\n")

# ==================== the digitizer's AI block in the Shiny server ====================
suppressMessages({ library(shiny); library(meta) })
app <- shinyAppDir(".")
html <- function(x) paste(as.character(x$html %||% x), collapse = "")
Sys.unsetenv(c("ANTHROPIC_API_KEY", "PONDERIS_AI_MAX_CALLS"))
ready_ai <- function(session) session$setInputs(ai_key = "sk-ant-x", ai_save_key = 1, ai_consent = TRUE)
options(ponderis.ai_runner = "sync")
seq_transport <- function(...) { answers <- list(...); n <- 0
  f <- function(body, key) { n <<- n + 1; a <- answers[[min(n, length(answers))]]
    if (is.numeric(a)) list(status = as.integer(a), body_text = "") else list(status = 200L, body_text = fx(a)) }
  environment(f)$count <- function() n; f }
img_event <- function(nonce = 1, width = 1000) list(b64 = "QUJD", width = width, height = 700 * width / 1000, orig_width = 1000, orig_height = 700, study = "Trial S", nonce = nonce)

tr <- seq_transport("figure_two_arms.json"); options(ponderis.ai_transport = tr)
testServer(app, {
  session$setInputs(aifig_image = img_event())
  stopifnot(environment(tr)$count() == 0, grepl("API key", html(output$aifig_reason)), is.null(aifig$reading()))
  ok("not ready: nothing sent")

  ready_ai(session)
  session$setInputs(aifig_image = img_event(2))
  rd <- aifig$reading()
  stopifnot(environment(tr)$count() == 1, length(rd$reading$arms) == 2, grepl("Intervention", html(output$aifig_panel)),
            grepl("0.75", html(output$aifig_panel)), grepl("dash or blank", html(output$aifig_panel)), grepl("1 AI call", output$ai_usage))
  ok("reading shown with warnings and usage counted")

  session$setInputs(aifig_use_axes = 1)
  ax <- aifig$sent()$aifig_axes
  stopifnot(ax$xscale == "linear", ax$curve_type == "survival", length(ax$x) == 2, length(ax$y) == 2,
            ax$x[[1]]$value == 0, ax$x[[2]]$value == 60, ax$y[[1]]$value == 0, ax$y[[2]]$value == 1,
            abs(ax$x[[2]]$px - (960 + 6)) < 1e-9)
  ok("use these axes: the two farthest ticks per axis")

  session$setInputs(aifig_use_arm = list(i = 1, nonce = 1))
  arm <- aifig$sent()$aifig_arm
  stopifnot(arm$label == "Intervention", arm$times == "0, 12, 24, 36", arm$nrisk == "250, 180, 120, 60", arm$hex == "#3366A0",
            arm$hr == "0.75", arm$hrlo == "0.6", arm$hrhi == "0.94")
  session$setInputs(aifig_use_arm = list(i = 2, nonce = 2))
  arm <- aifig$sent()$aifig_arm
  stopifnot(arm$label == "Control", arm$times == "0, 12, 24", arm$nrisk == "248, 150, 90", arm$hr == "", grepl("36", arm$note))
  ok("use this arm: details, colour, HR only for the experimental arm")

  session$setInputs(aifig_new_image = 3)
  stopifnot(is.null(aifig$reading()), !grepl("Use these axes", html(output$aifig_panel)))
  ok("a new image clears the reading")
})

# percent axis: tick values read as 0-1 on a 0-100 axis are put back on 0-100
tr <- seq_transport("figure_two_arms.json"); options(ponderis.ai_transport = tr)
testServer(app, {
  ready_ai(session)
  session$setInputs(aifig_image = img_event(1, width = 500))
  m <- aifig$reading()$reading; m$axes$y_percent <- TRUE
  stopifnot(identical(ai_figure_axes(m)$y[[2]]$value, 100), ai_figure_axes(aifig$reading()$reading)$y[[2]]$value == 1)
  session$setInputs(aifig_use_axes = 1)
  stopifnot(abs(aifig$sent()$aifig_axes$x[[2]]$px - 2 * (960 + 6)) < 1e-9)   # read on a half-size image, applied on the original
  ok("percent axis and scaling to the original image")
})

cat("AI FIGURE APP TESTS PASSED\n")
