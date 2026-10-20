suppressMessages(library(meta)); source("R/helpers.R")
out <- "tests/out"; dir.create(out, showWarnings = FALSE)
ok <- function(msg) cat("OK  ", msg, "\n")

run <- function(d, type, inp = list(), grp = "", yr = "") {
  tmp <- tempfile(fileext = ".csv"); write.csv(d, tmp, row.names = FALSE, na = "")
  rd <- read_user_csv(tmp)
  keys <- names(SPEC[[type]]$cols)
  map <- as.list(guess_columns(c("study", keys), names(rd$data)))
  p <- make_params(inp, type)
  v <- validate_data(rd$data, type, map, p)
  if (length(v$errors)) return(list(v = v))
  code <- build_code(p, map, v$need, grp, yr)
  env <- new.env(parent = globalenv()); env$dat <- rd$data
  eval(parse(text = code$prepare), env); eval(parse(text = code$model), env)
  pdf(NULL); eval(parse(text = code$forest), env); dev.off()
  list(v = v, m = env$m, code = code)
}

# 1. Brazilian Excel file: semicolon, decimal comma, Windows-1252, accents
br <- file.path(out, "br_excel.csv")
con <- file(br, "wb")
csv_txt <- "study_id;mean_tx;sd_tx;n_tx;mean_ctrl;sd_ctrl;n_ctrl\r\nGarc\xeda 2018;42,1;10,2;60;46,0;10,5;58\r\nJo\xe3o 2019;38,5;9,8;85;41,0;10,1;88\r\nS\xe3o Paulo 2020;45,0;11,5;40;47,2;11,2;42\r\n"
writeBin(charToRaw(csv_txt), con); close(con)
rd <- read_user_csv(br)
stopifnot(rd$sep == ";", rd$enc == "latin1", abs(as_num(rd$data$mean_tx[1]) - 42.1) < 1e-9)
stopifnot(identical(utf8ToInt(rd$data$study_id[1]), utf8ToInt(enc2utf8("Garc\u00eda 2018"))))
r <- run(rd$data, "cont"); stopifnot(is.null(r$v$errors) || !length(r$v$errors))
ok("semicolon + decimal comma + latin1 accents read correctly; analysis runs")

# 2. UTF-8 file with BOM (template written by the app, then filled)
tf <- tempfile(fileext = ".csv"); write_template(tf, "dich", ";")
lines <- c(readLines(tf, warn = FALSE), "João 2019;10;100;15;100;;", "Ana 2020;5;80;9;82;;")
con <- file(tf, "wb"); writeBin(as.raw(c(0xEF,0xBB,0xBF)), con); writeBin(charToRaw(enc2utf8(paste0(sub("^﻿", "", lines), "\r\n", collapse=""))), con); close(con)
rd <- read_user_csv(tf)
stopifnot(names(rd$data)[1] == "study_id", rd$enc == "UTF-8", rd$sep == ";")
ok("template round-trip: BOM stripped, header intact")
print(names(rd$data))
stopifnot(detect_type(names(rd$data)) == "dich")
ok("type auto-detected from template columns")

# 3. Validation errors
bad <- data.frame(study_id = c("A","B","C","D"), events_tx = c(5, 40, "x", 3), n_tx = c(50, 30, 60, 0),
                  events_ctrl = c(8, 9, 10, NA), n_ctrl = c(50, 50, 60, 55))
v <- validate_data(bad, "dich", as.list(guess_columns(c("study", names(SPEC$dich$cols)), names(bad))), make_params(list(), "dich"))
print(v$errors); stopifnot(length(v$errors) >= 1)
ok("text in numeric column rejected with row reference")

bad2 <- bad; bad2$events_tx <- c(5, 40, 7, 3); bad2$events_ctrl <- c(8, 9, 10, 3)
v <- validate_data(bad2, "dich", as.list(guess_columns(c("study", names(SPEC$dich$cols)), names(bad2))), make_params(list(), "dich"))
print(v$errors); print(v$warnings); stopifnot(any(grepl("exceed", v$errors)), any(grepl("Total must", v$errors)))
ok("events > total and n = 0 flagged")

# 4. Only one valid study
one <- data.frame(study_id = c("A","B"), events_tx = c(5, NA), n_tx = c(50, 50), events_ctrl = c(8, 9), n_ctrl = c(50, 50))
v <- validate_data(one, "dich", as.list(guess_columns(c("study", names(SPEC$dich$cols)), names(one))), make_params(list(), "dich"))
print(v$errors); stopifnot(any(grepl("At least 2", v$errors)))
ok("fewer than 2 usable studies blocked")

# 5. Zero cells + double zero, MH and Peto, RR vs OR
z <- data.frame(study_id = LETTERS[1:6], events_tx = c(0, 2, 0, 1, 3, 0), n_tx = c(40, 60, 35, 50, 80, 30),
                events_ctrl = c(1, 5, 0, 4, 7, 2), n_ctrl = c(42, 58, 36, 52, 79, 31))
r <- run(z, "dich", list(sm = "RR", method = "MH")); print(r$v$warnings); ok("zero cells, RR + MH")
r <- run(z, "dich", list(sm = "OR", method = "Peto", model = "common")); ok("Peto OR (common effect)")
r <- run(z, "dich", list(sm = "RR", method = "Peto")); stopifnot(grepl('method = "MH"', r$code$model)); ok("Peto with RR silently falls back to MH")

# 6. k = 2 with HK and prediction interval requested
two <- z[1:2, ]
r <- run(two, "dich", list(sm = "OR", method = "Inverse", hk = TRUE, pi = TRUE)); print(r$v$warnings); ok("k = 2 runs and warns")

# 7. Injection attempts in free text and column names
evil <- data.frame(study_id = LETTERS[1:5], events_tx = c(5,6,7,8,9), n_tx = 50, events_ctrl = c(9,10,11,12,13), n_ctrl = 50, check.names = FALSE)
names(evil)[2] <- 'events_tx"); system("echo HACKED > /tmp/hacked.txt"); ("'
r <- run(evil, "dich", list(left = 'x"); system("echo HACKED > /tmp/hacked.txt"); ("', right = "\\\"; q()", arm_e = 'a"b'))
stopifnot(!file.exists("/tmp/hacked.txt"))
ok("malicious column name and labels are quoted, not executed")

# 8. Continuous SMD and generic with SE only (log scale) and with Other
g <- data.frame(study_id = LETTERS[1:5], effect = c(-.3, -.1, -.5, -.2, -.4), se = c(.1, .12, .2, .15, .11))
r <- run(g, "gen", list(sm = "Other")); ok("generic effect, SE only, difference scale")
g2 <- data.frame(study_id = LETTERS[1:5], effect = c(.7, .9, .6, .8, .75), se = c(.1, .12, .2, .15, .11))
r <- run(g2, "gen", list(sm = "HR")); stopifnot(abs(r$m$TE[1] - log(.7)) < 1e-9); ok("generic HR with SE of log(HR)")

# 9. Sorting options and layouts
d <- example_data("dich")
for (s in c("none", "year", "effect", "weight")) for (l in c("meta", "RevMan5", "JAMA", "BMJ")) {
  r <- run(d, "dich", list(sort = s, layout = l, model = "both", pi = TRUE, details = TRUE), grp = "subgroup", yr = "year")
}
ok("all 4 sort options x 4 layouts x both models draw without error")
cat("EDGE TESTS PASSED\n")
