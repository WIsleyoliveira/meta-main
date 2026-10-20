# Extra tabs of the forest-plot mode: meta-regression, and influence (Baujat, Galbraith, influence diagnostics).
# extra_tabs_ui() returns the nav panels; extra_server() wires them to the analysis `fit` and the raw data `raw`.

code_box <- function(id) {
  tags$details(class = "mapping", tags$summary("R code for this analysis"), verbatimTextOutput(id))
}

extra_tabs_ui <- function() {
  list(
    nav_panel("Meta-regression", value = "metareg",
      p(class = "aside", "Does the effect change with a study characteristic (year, mean age, percentage of diabetics, follow-up, or a group such as the design)? ",
        "Pick a column from your file. Numbers give a slope; text with 2 to 6 different values gives a comparison between groups."),
      uiOutput("mr_pick"),
      uiOutput("mr_notice"),
      conditionalPanel("output.mr_ready",
        div(class = "sheet narrow", plotOutput("mr_plot", width = "620px", height = "460px")),
        howto_box("metareg", guide_metareg(), uiOutput("hw_mr")),
        div(class = "downloads", span("Download"),
          downloadButton("dl_mr_png", "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
          downloadButton("dl_mr_pdf", "PDF", class = "btn-outline-primary btn-sm")),
        h3("Coefficients"),
        tableOutput("mr_tbl"),
        uiOutput("mr_text"),
        code_box("mr_code")
      )
    ),
    nav_panel("Influence", value = "influence",
      p(class = "aside", "Which studies pull the result, or cause the heterogeneity? The Baujat plot puts each study by how much it adds to heterogeneity (horizontal) and to the pooled result (vertical): ",
        "studies in the top right matter most. The Galbraith plot shows studies that fall outside the band around the line."),
      uiOutput("infl_note"),
      h3("Baujat plot"),
      div(class = "sheet narrow", plotOutput("baujat_plot", width = "560px", height = "480px")),
      h3("Galbraith (radial) plot"),
      div(class = "sheet narrow", plotOutput("radial_plot", width = "560px", height = "480px")),
      howto_box("influence", guide_influence(), uiOutput("hw_infl")),
      div(class = "downloads", span("Download"),
        downloadButton("dl_baujat", "Baujat PNG", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_baujat_pdf", "Baujat PDF", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_radial", "Galbraith PNG", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_radial_pdf", "Galbraith PDF", class = "btn-outline-primary btn-sm")),
      h3("Influence diagnostics"),
      p(class = "aside", "Studentized residual, DFFITS, Cook's distance and hat value, and what the I² and τ² would be without each study."),
      DTOutput("infl_tbl"),
      div(class = "sheet narrow", plotOutput("infl_plot", width = "720px", height = "720px")),
      div(class = "downloads", span("Download"),
        downloadButton("dl_infl_png", "Diagnostics PNG", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_infl_pdf", "Diagnostics PDF", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_infl_csv", "Diagnostics CSV", class = "btn-outline-primary btn-sm")),
      code_box("infl_code")
    )
  )
}

extra_server <- function(input, output, session, fit, raw) {

  # ---- candidates for meta-regression ----
  cand <- reactive({
    f <- fit(); req(isTRUE(f$ok))
    d <- f$env$dat
    used <- f$used_cols %||% character(0)
    mr_candidates(d, exclude = used)
  })

  output$mr_pick <- renderUI({
    f <- fit(); req(isTRUE(f$ok)); cd <- cand()
    if (!nrow(cd)) return(div(class = "notice warn",
      "No column of your file can be used as a covariate. Add a numeric column (for example mean age or follow-up) to the CSV, or a text column with 2 to 6 groups, and load it again."))
    nm <- setNames(cd$col, paste0(cd$col, if (TRUE) ifelse(cd$kind == "number", " (number)", " (groups)") else ""))
    selectInput("mr_cov", "Covariate", nm, width = "320px")
  })

  mr_res <- reactive({
    f <- fit(); req(isTRUE(f$ok)); cd <- cand(); req(nrow(cd) > 0, input$mr_cov %in% cd$col)
    kind <- cd$kind[cd$col == input$mr_cov]
    x <- mr_covariate(f$env$dat[[input$mr_cov]], kind)
    mr_run(f$env$m, x, kind, input$mr_cov, f$p$tau, f$p$hk)
  })

  output$mr_ready <- reactive({ r <- tryCatch(mr_res(), error = function(e) NULL); !is.null(r) && isTRUE(r$ok) })
  outputOptions(output, "mr_ready", suspendWhenHidden = FALSE)

  output$mr_notice <- renderUI({
    r <- tryCatch(mr_res(), error = function(e) NULL); req(!is.null(r))
    if (!isTRUE(r$ok)) div(class = "notice error", role = "alert", r$error)
    else if (r$k < 10) div(class = "notice warn", sprintf("Only %d studies. A meta-regression needs about 10 studies per covariate; treat this as exploratory.", r$k))
  })
  output$mr_plot <- renderPlot({ r <- mr_res(); req(isTRUE(r$ok)); mr_plot(r) }, width = 620, height = 460, res = 96)
  output$mr_tbl <- renderTable({
    r <- mr_res(); req(isTRUE(r$ok)); t <- r$table
    out <- data.frame(Term = t$term, Estimate = sprintf("%.3f", t$estimate), `95% CI` = sprintf("%.3f to %.3f", t$ci_lower, t$ci_upper),
                      p = ifelse(t$p < 0.001, "< 0.001", sprintf("%.3f", t$p)), check.names = FALSE)
    if (isTRUE(r$ratio)) {
      out[["Ratio (95% CI)"]] <- sprintf("%.3f (%.3f to %.3f)", t$ratio, t$ratio_lower, t$ratio_upper)
      if (r$kind == "number") out[["Ratio (95% CI)"]][t$term == "Intercept"] <- "not meaningful (covariate = 0)"
    }
    out
  }, striped = TRUE, spacing = "s", width = "auto")
  output$mr_text <- renderUI({
    r <- mr_res(); req(isTRUE(r$ok))
    div(class = "report-block",
      div(class = "report-head", tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = copy_js, "Copy")),
      div(class = "report-text", tags$p(enc2utf8(paste0(r$text, sprintf(" Test of moderators: QM = %.2f, p %s.", r$qm, if (r$qm_p < 0.001) "< 0.001" else sprintf("= %.3f", r$qm_p)))))))
  })
  output$hw_mr <- renderUI({ r <- mr_res(); req(isTRUE(r$ok)); howto_text(reading_metareg(r)) })
  output$mr_code <- renderText({ r <- mr_res(); req(isTRUE(r$ok)); paste(r$code, collapse = "\n") })

  save_base <- function(file, ext, w, h, draw) {
    if (ext == "png") ragg::agg_png(file, width = w, height = h, units = "in", res = 300) else grDevices::cairo_pdf(file, width = w, height = h)
    on.exit(grDevices::dev.off()); draw()
  }
  output$dl_mr_png <- downloadHandler("meta_regression.png", function(file) { r <- mr_res(); req(isTRUE(r$ok)); save_base(file, "png", 6.5, 4.8, function() mr_plot(r)) })
  output$dl_mr_pdf <- downloadHandler("meta_regression.pdf", function(file) { r <- mr_res(); req(isTRUE(r$ok)); save_base(file, "pdf", 6.5, 4.8, function() mr_plot(r)) })

  # ---- influence ----
  infl <- reactive({ f <- fit(); req(isTRUE(f$ok)); infl_run(f$env$m, f$p$tau, f$p$hk) })
  output$infl_note <- renderUI({
    f <- fit(); req(isTRUE(f$ok)); r <- infl()
    if (f$k < 5) tagList(div(class = "notice warn", sprintf("Only %d studies. These plots are only a rough guide with so few studies.", f$k)), div(class = "aside", infl_note(r)))
    else div(class = "aside", infl_note(r))
  })
  output$baujat_plot <- renderPlot({ f <- fit(); req(isTRUE(f$ok)); baujat_plot(f$env$m) }, width = 560, height = 480, res = 96)
  output$radial_plot <- renderPlot({ f <- fit(); req(isTRUE(f$ok)); radial_plot(f$env$m) }, width = 560, height = 480, res = 96)
  output$infl_plot <- renderPlot({ r <- infl(); req(isTRUE(r$ok)); infl_plot(r) }, width = 720, height = 720, res = 96)
  output$hw_infl <- renderUI({ r <- infl(); req(isTRUE(r$ok)); howto_text(reading_influence(r)) })
  output$infl_tbl <- renderDT({
    r <- infl(); req(isTRUE(r$ok)); t <- r$table
    names(t) <- c("Study", "Studentized residual", "DFFITS", "Cook's distance", "Hat value", "Weight (%)", "τ² without", "I² without (%)", "Influential")
    datatable(t, rownames = FALSE, options = list(pageLength = 20, dom = "t", scrollX = TRUE)) |>
      formatRound(c("Studentized residual", "DFFITS", "Cook's distance", "Hat value", "Weight (%)", "τ² without", "I² without (%)"), 3)
  })
  output$infl_code <- renderText({ r <- infl(); req(isTRUE(r$ok)); paste(r$code, collapse = "\n") })
  output$dl_baujat <- downloadHandler("baujat_plot.png", function(file) { f <- fit(); req(isTRUE(f$ok)); save_base(file, "png", 6, 5.2, function() baujat_plot(f$env$m)) })
  output$dl_radial <- downloadHandler("galbraith_plot.png", function(file) { f <- fit(); req(isTRUE(f$ok)); save_base(file, "png", 6, 5.2, function() radial_plot(f$env$m)) })
  output$dl_baujat_pdf <- downloadHandler("baujat_plot.pdf", function(file) { f <- fit(); req(isTRUE(f$ok)); save_base(file, "pdf", 6, 5.2, function() baujat_plot(f$env$m)) })
  output$dl_radial_pdf <- downloadHandler("galbraith_plot.pdf", function(file) { f <- fit(); req(isTRUE(f$ok)); save_base(file, "pdf", 6, 5.2, function() radial_plot(f$env$m)) })
  output$dl_infl_png <- downloadHandler("influence_diagnostics.png", function(file) { r <- infl(); req(isTRUE(r$ok)); save_base(file, "png", 7.5, 7.5, function() infl_plot(r)) })
  output$dl_infl_pdf <- downloadHandler("influence_diagnostics.pdf", function(file) { r <- infl(); req(isTRUE(r$ok)); save_base(file, "pdf", 7.5, 7.5, function() infl_plot(r)) })
  output$dl_infl_csv <- downloadHandler("influence_diagnostics.csv", function(file) { r <- infl(); req(isTRUE(r$ok)); utils::write.csv(r$table, file, row.names = FALSE) })
}
