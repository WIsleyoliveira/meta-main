# Server-level test with shiny::testServer: load example data for each type,
# set options, and check that the fit succeeds and outputs render.
suppressMessages({library(shiny); library(meta)})
source("R/helpers.R")
app <- shinyAppDir(".")
for (type in c("dich", "cont", "gen", "prop")) {
  testServer(app, {
    session$setInputs(type = type, tpl_sep = ",", model = "random", tau = "REML", hk = TRUE, pi = FALSE,
                      layout = "meta", sort = "none", weights = TRUE, het = TRUE, overall_p = TRUE, details = FALSE,
                      arm_e = "PCI", arm_c = "CABG", label_left = "Favours PCI", label_right = "Favours CABG", smlab = "",
                      bias = "Egger", trimfill = FALSE)
    session$setInputs(load_example = 1)
    session$flushReact()
    # mapping inputs are created by renderUI in a browser; set them as the guesses would be
    d <- raw()$data
    keys <- names(SPEC[[type]]$cols)
    g <- guess_columns(c("study", "year", "subgroup", keys), names(d))
    inp <- c(list(map_study = g[["study"]], map_year = g[["year"]], map_group = g[["subgroup"]]),
             setNames(as.list(g[keys]), paste0("map_", keys)))
    do.call(session$setInputs, inp)
    session$setInputs(sm = unname(SM_CHOICES[[type]][1]))
    Sys.sleep(0.6); session$elapse(1000); session$flushReact()
    f <- fit()
    stopifnot(isTRUE(f$ok))
    cat(sprintf("[%s] fit ok, k=%d, warnings=%d\n", type, f$k, length(f$warnings)))
    out <- output$headline; stopifnot(!is.null(out$html))
    stopifnot(nzchar(output$methods_txt))
    stopifnot(grepl("pooled", as.character(output$results_txt$html)) && grepl("Heterogeneity", as.character(output$results_txt$html)))
    p <- output$forest_plot; stopifnot(!is.null(p$src))
    cat("   headline + methods + forest plot rendered\n")
  })
}
cat("APP TESTS PASSED\n")
