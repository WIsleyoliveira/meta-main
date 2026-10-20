# "Review tools" mode: PRISMA 2020 flow diagram, risk of bias (RoB 2, ROBINS-I, Newcastle-Ottawa) and GRADE certainty of evidence.

review_ui <- function() {
  div(class = "review",
    navset_underline(id = "rev_tabs",
      nav_panel("PRISMA flow diagram", value = "prisma",
        div(class = "workspace",
          div(class = "controls",
            div(class = "aside", "Type the numbers your search and screening gave. The app works out the rest of the flow and warns you when they do not add up."),
            step("Identification", help = "List each database you searched, one per line, followed by a comma and how many records it gave. Register or other-source records are optional: leave them empty and they disappear from the diagram. If records were removed before screening for another reason, write the reason and the count, one per line.",
              textAreaInput("pr_dbs", "Databases searched, with the number of records from each", rows = 4, width = "100%",
                            placeholder = "PubMed, 612\nEmbase, 540\nCochrane CENTRAL, 98"),
              fluidRow(column(6, numericInput("pr_other", "Records from registers or other sources", NA, min = 0)),
                       column(6, textInput("pr_other_name", "Name of that source (optional)", "", placeholder = "ClinicalTrials.gov"))),
              fluidRow(column(6, numericInput("pr_dup", "Duplicates removed", NA, min = 0)),
                       column(6, numericInput("pr_auto", "Removed by automation tools", NA, min = 0))),
              textAreaInput("pr_oth_rem", "Removed for other reasons, with the reason (optional)", rows = 2, width = "100%",
                            placeholder = "Retracted records, 4")),
            step("Screening", help = "Type how many records were excluded by title and abstract. If your review did not track reports sought or not retrieved, untick the box and that step leaves the diagram.",
              numericInput("pr_excl", "Records excluded at title and abstract", NA, min = 0),
              checkboxInput("pr_use_sought", "Show the step \"reports sought / not retrieved\"", TRUE),
              conditionalPanel("input.pr_use_sought", numericInput("pr_nr", "Reports not retrieved", NA, min = 0))),
            step("Eligibility and inclusion", help = "List why reports were excluded after reading them, one reason per line with the count. The flow diagram updates as you type.",
              textAreaInput("pr_reasons", "Reports excluded after reading, with the reason", rows = 5, width = "100%",
                            placeholder = "Wrong population, 12\nNo outcome data, 8\nConference abstract, 5"),
              div(class = "aside", "One reason per line, followed by a comma and the number."),
              numericInput("pr_ma", "Studies in the meta-analysis (optional)", NA, min = 0),
              actionLink("pr_from_fit", "Use the number of studies from my forest plot")),
            step("Appearance", help = "Choose what to show. The side labels (Identification, Screening, Included) and the yellow title follow the official PRISMA 2020 template, and you can hide either one.",
              checkboxInput("pr_banner", "Show the title banner", TRUE),
              checkboxInput("pr_bands", "Show the side labels (Identification, Screening, Included)", TRUE))
          ),
          div(class = "results",
            uiOutput("pr_notice"),
            div(class = "sheet narrow", plotOutput("pr_plot", width = "720px", height = "860px")),
            div(class = "downloads", span("Download"),
              downloadButton("dl_pr_png", "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_pr_pdf", "PDF", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_pr_svg", "SVG", class = "btn-outline-primary btn-sm")),
            div(class = "aside", "Format follows the PRISMA 2020 statement (Page et al., BMJ 2021).")
          )
        )),
      nav_panel("Risk of bias", value = "rob",
        div(class = "workspace",
          div(class = "controls",
            selectInput("rob_tool", "Tool", setNames(names(ROB_TOOLS), vapply(ROB_TOOLS, `[[`, "", "name")), width = "100%"),
            textAreaInput("rob_studies", "Studies (one per line)", rows = 6, width = "100%", placeholder = "Kemna 2016\nSmith 2012"),
            actionLink("rob_from_fit", "Take the study names from my forest plot"),
            conditionalPanel("input.rob_tool != 'nos'", textInput("rob_letter", "Panel letter on the figure (optional)", "", placeholder = "A", width = "120px")),
            div(class = "aside", "Rows are kept when you add studies at the end. The overall judgement is filled in by the app: the worst domain for RoB 2 and ROBINS-I, and the usual star cut-offs for Newcastle-Ottawa. Adjust it in your text if your guideline asks for more judgement.")
          ),
          div(class = "results",
            uiOutput("rob_note"),
            uiOutput("rob_grid"),
            conditionalPanel("output.rob_has",
              conditionalPanel("output.rob_fig_on",
                div(class = "sheet", plotOutput("rob_fig", width = "auto", height = "auto"))),
              conditionalPanel("!output.rob_fig_on",
                div(class = "sheet", plotOutput("rob_traffic", width = "auto", height = "auto")),
                conditionalPanel("input.rob_tool != 'nos'", div(class = "sheet", plotOutput("rob_summary", width = "760px", height = "380px")))),
              h3("Table"),
              tableOutput("rob_tbl"),
              div(class = "downloads", span("Download"),
                downloadButton("dl_rob_png", "Figure PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
                downloadButton("dl_rob_tiff", "Figure TIFF", class = "btn-outline-primary btn-sm"),
                downloadButton("dl_rob_pdf", "Figure PDF", class = "btn-outline-primary btn-sm"),
                downloadButton("dl_rob_csv", "Table CSV", class = "btn-outline-primary btn-sm"))
            )
          )
        )),
      grade_panel_ui()
    )
  )
}

review_server <- function(input, output, session, fit) {

  save_plot <- function(file, ext, w, h, draw) {
    switch(ext, png = ragg::agg_png(file, width = w, height = h, units = "in", res = 300),
                pdf = grDevices::cairo_pdf(file, width = w, height = h), svg = svglite::svglite(file, width = w, height = h))
    on.exit(grDevices::dev.off()); draw()
  }

  # ---------------- PRISMA ----------------
  observeEvent(input$pr_from_fit, {
    f <- fit(); if (isTRUE(f$ok)) updateNumericInput(session, "pr_ma", value = f$k)
    else showNotification("Load your data in the forest-plot mode first.", type = "warning")
  })
  pr <- reactive({
    dbs <- prisma_parse_reasons(input$pr_dbs); rem <- prisma_parse_reasons(input$pr_oth_rem)
    prisma_flow(0, input$pr_other, input$pr_dup, input$pr_auto, 0, input$pr_excl, input$pr_nr, prisma_parse_reasons(input$pr_reasons),
                if (is.na(input$pr_ma %||% NA)) NA else input$pr_ma,
                db_list = if (nrow(dbs)) dbs else NULL, removed_list = if (nrow(rem)) rem else NULL,
                other_name = input$pr_other_name, use_sought = isTRUE(input$pr_use_sought %||% TRUE))
  })
  pr_draw <- function() prisma_plot(pr(), bands = isTRUE(input$pr_bands %||% TRUE), banner = isTRUE(input$pr_banner %||% TRUE))
  output$pr_notice <- renderUI({
    fl <- pr(); tagList(lapply(fl$errors, function(e) div(class = if (grepl("^Enter the databases", e)) "notice info" else "notice error", role = if (grepl("^Enter the databases", e)) NULL else "alert", e)), lapply(fl$warnings, function(w) div(class = "notice warn", w)),
                        if (anyNA(c(prisma_parse_reasons(input$pr_reasons)$n, prisma_parse_reasons(input$pr_dbs)$n, prisma_parse_reasons(input$pr_oth_rem)$n))) div(class = "notice warn", "A line has no number after it and is counted as 0. Write it as: name, 12"))
  })
  output$pr_plot <- renderPlot(pr_draw(), width = 720, height = 860, res = 96)
  output$dl_pr_png <- downloadHandler("prisma_flow.png", function(file) save_plot(file, "png", 7.5, 9, function() pr_draw()))
  output$dl_pr_pdf <- downloadHandler("prisma_flow.pdf", function(file) save_plot(file, "pdf", 7.5, 9, function() pr_draw()))
  output$dl_pr_svg <- downloadHandler("prisma_flow.svg", function(file) save_plot(file, "svg", 7.5, 9, function() pr_draw()))

  # ---------------- Risk of bias ----------------
  observeEvent(input$rob_from_fit, {
    f <- fit()
    if (!isTRUE(f$ok)) { showNotification("Load your data in the forest-plot mode first.", type = "warning"); return() }
    updateTextAreaInput(session, "rob_studies", value = paste(as.character(f$env$m$studlab), collapse = "\n"))
  })
  rob_names <- reactive({
    s <- trimws(unlist(strsplit(input$rob_studies %||% "", "\n", fixed = TRUE))); s <- s[nzchar(s)]; s <- make.unique(s)
    s
  })
  rob_key <- function(tool, i, s, j) paste0("rob_", tool, "_", gsub("[^A-Za-z0-9]", "_", s), "_", j)

  output$rob_grid <- renderUI({
    tool <- input$rob_tool %||% "rob2"; tl <- ROB_TOOLS[[tool]]; st <- rob_names()
    if (!length(st)) return(div(class = "empty", h2("Add the studies on the left"), p("One name per line, then judge each domain here.")))
    head <- tags$tr(tags$th("Study"), lapply(seq_along(tl$short), function(j) tags$th(title = tl$domains[j], tl$short[j])))
    rows <- lapply(seq_along(st), function(i) {
      tags$tr(tags$td(st[i]), lapply(seq_along(tl$short), function(j) {
        id <- rob_key(tool, i, st[i], j); cur <- isolate(input[[id]])
        tags$td(if (tool == "nos")
          numericInput(id, NULL, value = if (is.null(cur)) NA else cur, min = 0, max = tl$max[j], step = 1, width = "70px")
        else selectInput(id, NULL, c("", tl$levels), selected = cur %||% "", width = "130px", selectize = FALSE))
      }))
    })
    tagList(div(class = "aside", paste(paste0(tl$short, " = ", tl$domains), collapse = ";  ")),
            div(class = "rob-grid", tags$table(tags$thead(head), tags$tbody(rows))))
  })

  rob_data <- reactive({
    tool <- input$rob_tool %||% "rob2"; tl <- ROB_TOOLS[[tool]]; st <- rob_names(); req(length(st) > 0)
    d <- rob_template(tool, st)
    for (j in seq_along(tl$short)) d[[tl$short[j]]] <- vapply(seq_along(st), function(i) {
      v <- input[[rob_key(tool, i, st[i], j)]]
      if (is.null(v) || (length(v) == 1 && (is.na(v) || identical(v, "")))) NA_character_ else as.character(v)
    }, character(1))
    d
  })
  complete_rows <- reactive({ d <- rob_data(); tool <- input$rob_tool %||% "rob2"; d[stats::complete.cases(d), , drop = FALSE] })
  output$rob_has <- reactive({ r <- tryCatch(complete_rows(), error = function(e) NULL); !is.null(r) && nrow(r) > 0 })
  outputOptions(output, "rob_has", suspendWhenHidden = FALSE)
  rob_tool <- reactive(input$rob_tool %||% "rob2")
  rob_rows <- debounce(complete_rows, 500)   # the figure takes a moment to draw, so wait until the person stops changing cells
  output$rob_note <- renderUI(if (!rob_has_robvis() && rob_tool() != "nos")
    div(class = "notice warn", "For the figure in the style of the robvis package (traffic light with the domain key, plus the summary bars), install it once in R: install.packages(\"robvis\"). Until then a simpler figure is drawn."))
  rob_fig_ok <- reactive(rob_has_robvis() && rob_tool() != "nos")
  output$rob_fig_on <- reactive(rob_fig_ok()); outputOptions(output, "rob_fig_on", suspendWhenHidden = FALSE)
  rob_psize <- ROB_PSIZE
  rob_dim <- reactive(rob_fig_size(rob_rows(), rob_tool(), rob_psize))
  rob_h <- reactive({ n <- nrow(rob_rows()); round(max(3.2, 0.62 * n + 2.8) * 96) })
  rob_w <- reactive({ round(max(7, 5 + length(ROB_TOOLS[[rob_tool()]]$short) * 1.1) * 96) })
  output$rob_fig <- renderPlot({ req(rob_fig_ok()); tryCatch(rob_fig_draw(rob_rows(), rob_tool(), input$rob_letter %||% "", rob_psize), error = function(e) { grid::grid.newpage(); grid::grid.text(paste(strwrap(paste("Could not draw the figure with robvis:", conditionMessage(e)), 70), collapse = "\n")) }) },
                               width = function() if (rob_fig_ok()) round(rob_dim()[["w"]] * 96) else 100, height = function() if (rob_fig_ok()) round(rob_dim()[["h"]] * 96) else 100, res = 96)
  output$rob_traffic <- renderPlot(rob_traffic_plot(rob_rows(), rob_tool()), width = function() rob_w(), height = function() rob_h(), res = 96)
  output$rob_summary <- renderPlot(rob_summary_plot(rob_rows(), rob_tool()), width = 760, height = 380, res = 96)
  output$rob_tbl <- renderTable(rob_table(rob_rows(), rob_tool()), striped = TRUE, spacing = "s", width = "auto")
  # one figure file: the robvis-style figure when available, else the plain traffic light
  rob_save <- function(file, ext) {
    d <- complete_rows(); tool <- rob_tool()
    if (rob_fig_ok()) {
      sz <- rob_fig_size(d, tool, rob_psize); w <- sz[["w"]]; h <- sz[["h"]]
      draw <- function() rob_fig_draw(d, tool, input$rob_letter %||% "", rob_psize)
    } else { w <- rob_w() / 96; h <- rob_h() / 96; draw <- function() rob_traffic_plot(d, tool) }
    switch(ext, png = ragg::agg_png(file, width = w, height = h, units = "in", res = 300),
                tiff = ragg::agg_tiff(file, width = w, height = h, units = "in", res = 300, compression = "lzw"),
                pdf = grDevices::cairo_pdf(file, width = w, height = h))
    on.exit(grDevices::dev.off()); draw()
  }
  output$dl_rob_png <- downloadHandler("risk_of_bias_figure.png", function(file) rob_save(file, "png"))
  output$dl_rob_tiff <- downloadHandler("risk_of_bias_figure.tiff", function(file) rob_save(file, "tiff"))
  output$dl_rob_pdf <- downloadHandler("risk_of_bias_figure.pdf", function(file) rob_save(file, "pdf"))
  output$dl_rob_csv <- downloadHandler("risk_of_bias.csv", function(file) utils::write.csv(rob_table(complete_rows(), input$rob_tool %||% "rob2"), file, row.names = FALSE))

  # ---------------- GRADE ----------------
  grade_server(input, output, session, fit, rob_rows = complete_rows, rob_tool = rob_tool)
}
