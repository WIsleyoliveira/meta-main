# Launcher: checks the packages, installs what is missing, and starts Ponderis.
# Run from R:  source("run_app.R")   (or double-click run_app.bat on Windows / run_app.command on Mac)
here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NA_character_)
if (is.na(here)) {
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  here <- if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
}
setwd(here)
need <- c("shiny", "bslib", "DT", "meta", "metafor", "metadat", "survival", "jsonlite", "ragg", "IPDfromKM", "estmeansd")
missing_pkgs <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages(missing_pkgs)
  still <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still)) stop("Could not install: ", paste(still, collapse = ", "), ". Install them manually and run again.", call. = FALSE)
}
shiny::runApp(here, launch.browser = TRUE)
