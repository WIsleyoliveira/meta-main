# Headless test of the analysis engine: for each outcome type, build code from
# the example data, run it, draw the forest plot, and run the exported script.
suppressMessages(library(meta))
source("R/helpers.R")
out <- Sys.getenv("OUT_DIR", "tests/out"); dir.create(out, showWarnings = FALSE, recursive = TRUE)

run_case <- function(type, inp = list(), group = "subgroup", year = "year") {
  d <- example_data(type)
  tmp <- file.path(out, paste0("example_", type, ".csv")); write.csv(d, tmp, row.names = FALSE, na = "")
  rd <- read_user_csv(tmp)
  stopifnot(identical(rd$sep, ","))
  keys <- names(SPEC[[type]]$cols)
  map <- as.list(guess_columns(c("study", keys), names(rd$data)))
  if (type == "gen" && !nzchar(map$se)) map$se <- ""
  p <- make_params(inp, type)
  if (type == "gen") { p$sm <- pick(inp$sm, unname(SM_CHOICES$gen)) }
  v <- validate_data(rd$data, type, map, p)
  cat(sprintf("[%s] k=%s errors=%d warnings=%d\n", type, v$k, length(v$errors), length(v$warnings)))
  if (length(v$errors)) { print(v$errors); stop("validation failed") }
  code <- build_code(p, map, v$need, group_col = group, year_col = year)
  env <- new.env(parent = globalenv()); env$dat <- rd$data
  eval(parse(text = code$prepare), env); eval(parse(text = code$model), env)
  dm <- eval(parse(text = code$dims), env); w <- round(dm$width + 0.2, 2); h <- round(dm$height + 0.2, 2)
  png(file.path(out, paste0("forest_", type, ".png")), width = w, height = h, units = "in", res = 110)
  eval(parse(text = code$forest), env); dev.off()
  png(file.path(out, paste0("loo_", type, ".png")), width = 9, height = 5, units = "in", res = 100)
  eval(parse(text = code$loo), env); dev.off()
  png(file.path(out, paste0("funnel_", type, ".png")), width = 6, height = 5, units = "in", res = 100)
  eval(parse(text = code$funnel), env); dev.off()
  b <- eval(parse(text = code$bias), env)
  cat("  bias test ok:", class(b)[1], "\n")
  # Exported script must run on its own
  sc <- build_script(code, p, basename(tmp), rd$sep, rd$enc, w, h)
  sf <- file.path(out, paste0("script_", type, ".R")); writeLines(sc, sf)
  old <- setwd(out); on.exit(setwd(old), add = TRUE)
  res <- system2("Rscript", c(basename(sf)), stdout = TRUE, stderr = TRUE)
  st <- attr(res, "status"); if (!is.null(st) && st != 0) { print(tail(res, 15)); stop("exported script failed") }
  stopifnot(file.exists("forest_plot.png"), file.exists("forest_plot.pdf"), file.exists("forest_plot.svg"))
  file.rename("forest_plot.png", paste0("script_forest_", type, ".png"))
  invisible(list(m = env$m, code = code))
}

r1 <- run_case("dich", list(sm = "RR", method = "MH", model = "random", hk = TRUE, pi = TRUE,
                            label_left = "Favours treatment", label_right = "Favours control",
                            arm_e = "PCI", arm_c = "CABG", sort = "year"))
r2 <- run_case("cont", list(sm = "SMD", model = "both", pi = FALSE))
r3 <- run_case("gen", list(sm = "HR", model = "random", hk = TRUE))
r4 <- run_case("prop", list(sm = "PLOGIT", model = "random", pi = TRUE))
cat("ALL ENGINE TESTS PASSED\n")
