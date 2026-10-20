# Readings and code tab text: run with every data type and check that nothing breaks and the sentences agree with the numbers.
suppressMessages({library(meta); library(shiny); library(bslib)})
for (f in c("R/helpers.R","R/km_report.R","R/extra_helpers.R","R/howto.R")) source(f, echo = FALSE)
`%+%` <- function(a, b) paste0(a, b)
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "OK   " else "FAIL ", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }
for (type in c("dich","cont","gen","prop")) {
  d <- example_data(type)
  input <- list(type = type)
  p <- make_params(input, type)
  map <- setNames(as.list(names(d)[match(c("study_id", names(SPEC[[type]]$cols)), names(d))]), c("study", names(SPEC[[type]]$cols)))
  v <- validate_data(d, type, map, p)
  code <- build_code(p, map, v$need)
  env <- new.env(); env$dat <- d
  eval(parse(text = code$prepare), env); eval(parse(text = code$model), env)
  m <- env$m
  for (mod in c("random", "common")) {
    p$model <- mod; p$pi <- TRUE
    r <- reading_forest(m, p); ok(length(r) >= 2 && all(nzchar(r)), paste(type, mod, "forest reading:", substr(r[1], 1, 90)))
    l <- reading_loo(m, p); ok(length(l) >= 1, paste(type, mod, "loo reading:", substr(l[length(l)], 1, 70)))
  }
  p$trimfill <- TRUE
  ok(length(reading_funnel(m, p)) >= 1, paste(type, "funnel reading"))
  ir <- infl_run(m); ok(isTRUE(ir$ok) && length(reading_influence(ir)) >= 1, paste(type, "influence reading"))
  ok(length(guide_forest(m$sm)) >= 1, paste(type, "guide"))
}
sv <- software_versions(); ok(grepl("^All analyses were performed in R \\(version [0-9.]+\\) using the meta package", software_sentence(sv)), "software sentence")
ok(grepl("R \\(version", methods_text(make_params(list(type="dich"), "dich"), 8)), "methods text names the R version")
# direction wording follows the numbers
m <- metabin(c(5,6,4,7), c(100,100,100,100), c(20,22,18,25), c(100,100,100,100), sm = "RR", random = TRUE, common = FALSE)
r <- reading_forest(m, make_params(list(type = "dich", model = "random"), "dich"))
ok(grepl("left of the line at 1", r[1]) && grepl("lower risk", r[1]), "protective effect reads as lower risk, left of 1")
m2 <- metabin(c(20,22,18,25), c(100,100,100,100), c(5,6,4,7), c(100,100,100,100), sm = "RR", random = TRUE, common = FALSE)
r <- reading_forest(m2, make_params(list(type = "dich", model = "random"), "dich"))
ok(grepl("right of the line at 1", r[1]) && grepl("higher risk", r[1]), "harmful effect reads as higher risk, right of 1")
cat("HOWTO TESTS PASSED\n")
