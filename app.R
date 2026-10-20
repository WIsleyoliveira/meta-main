# Ponderis: upload a CSV, pick an analysis, get a publication-ready forest plot.
# Run locally with:  shiny::runApp()
# Needs: shiny, bslib, DT, meta (>= 8.0), svglite, ragg

library(shiny)
library(bslib)
library(DT)
library(meta)

options(shiny.maxRequestSize = 5 * 1024^2)  # 5 MB upload limit

app_theme <- bs_theme(
  version = 5, bg = "#ECEFF2", fg = "#0F1B27", primary = "#1E3A5F",
  base_font = font_google("Atkinson Hyperlegible", local = FALSE),
  heading_font = font_google("Literata", local = FALSE),
  "border-radius" = "3px", "font-size-base" = "0.94rem"
)

ui <- fluidPage(
  theme = app_theme,
  title = "Ponderis",
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(src = "digitizer.js"),
    tags$script(src = "hints.js"),
    tags$script(src = "ui.js"),
    tags$script(HTML("$(document).on('click', '.subnav-btn', function(){ $('input[name=mode][value=' + $(this).data('mode') + ']').click(); });")),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),

  tags$header(class = "topbar",
    tags$a(class = "brand", href = "#", `data-go` = "home", `aria-label` = "Ponderis, home", ponderis_mark(), span("Ponderis")),
    div(class = "mode-bar",
      radioButtons("mode", NULL, inline = TRUE, selected = "home",
        choiceValues = c("home", "forest", "dig", "review", "conv", "km"),
        choiceNames = list(
          HTML('<i class="tt" data-tip="Overview: what you can do here and how the parts fit together.">Home</i>'),
          HTML('<i class="tt" data-tip="Pool studies from a CSV: forest plot, leave-one-out, influence, funnel plot and meta-regression.">Forest plot</i>'),
          HTML('<i class="tt" data-tip="Digitize curves from published figures, then pool them into one Kaplan-Meier analysis.">Survival curves</i>'),
          HTML('<i class="tt" data-tip="PRISMA flow diagram, risk of bias (RoB 2, ROBINS-I, NOS) and GRADE certainty of evidence.">Quality &amp; reporting</i>'),
          HTML('<i class="tt" data-tip="Convert median and IQR to mean and SD, SE or CI to SD, and combine groups.">Calculator</i>'),
          HTML('km'))))
  ),

  # steps inside "Survival curves"
  conditionalPanel("input.mode == 'dig' || input.mode == 'km'", div(class = "subnav",
    tags$button(type = "button", class = "subnav-btn", `data-mode` = "dig", span("1"), "Digitize curves"),
    tags$button(type = "button", class = "subnav-btn", `data-mode` = "km", span("2"), "Pooled analysis and report"))),

  conditionalPanel("input.mode == 'dig' || input.mode == 'km'", div(class = "project-bar",
    span(class = "project-label", "Project"),
    downloadButton("proj_save", "Save project", class = "btn-outline-primary btn-sm"),
    fileInput("proj_open", NULL, accept = c(".json", "application/json"), buttonLabel = "Open project", placeholder = "No project chosen", width = "260px"),
    span(class = "project-note", "Saves the digitized points, the Kaplan-Meier tables and the settings. The figure image is not included.")
  )),

  conditionalPanel("input.mode == 'home'", home_ui()),

  conditionalPanel("input.mode == 'forest'", div(class = "workspace",

    # ------------------------------ Controls ------------------------------
    div(class = "controls",

      step("Choose your data type", help = "1. Pick what your studies report: events out of totals, means and SDs, or a ready-made effect such as a hazard ratio.\n2. Click Download blank template.\n3. Fill one row per study in Excel and save it as CSV.",
        selectInput("type", NULL, choices = TYPE_CHOICES, width = "100%"),
        radioButtons("tpl_sep", "CSV separator in the template",
          choices = c("Comma (,)" = ",", "Semicolon (;), for Excel in Brazil and Europe" = ";"),
          selected = ","),
        downloadButton("dl_template", "Download blank template", class = "btn-outline-primary btn-sm"),
        div(class = "aside", "Fill in one row per study. Columns marked year and subgroup are optional.")
      ),

      step("Upload your CSV", help = "Click Choose file and pick the CSV you filled in. Just exploring? Click Load example data. Then check the note under the file: it should list the right number of studies.",
        fileInput("csv", NULL, accept = c(".csv", ".txt", "text/csv"),
                  buttonLabel = "Choose file", placeholder = "No file yet", width = "100%"),
        actionLink("load_example", "Load example data instead"),
        uiOutput("detect_note"),
        uiOutput("mapping")
      ),

      step("Set up the analysis", help = "Choose the effect measure that fits your outcome (risk ratio for yes/no outcomes). Keep Random effects unless you have a reason not to, and leave Hartung-Knapp on when you have few studies.",
        uiOutput("measure_ui"),
        selectInput("model", "Model",
          c("Random effects (recommended)" = "random", "Common effect" = "common", "Both" = "both"),
          width = "100%"),
        conditionalPanel("input.model != 'common'", checkboxInput("pi", "Show prediction interval", FALSE)),
        adv_box("Advanced options",
          uiOutput("method_ui"),
          conditionalPanel("input.model != 'common'",
            selectInput("tau", "Between-study variance estimator",
              c("REML" = "REML", "DerSimonian-Laird" = "DL", "Paule-Mandel" = "PM"), width = "100%"),
            checkboxInput("hk", "Hartung-Knapp adjustment (recommended with few studies)", TRUE)),
          div(class = "aside", "The defaults suit most reviews. Change them only for a reason you can state in the methods."))
      ),

      step("Shape the plot", open = FALSE, help = "Optional. Choose a journal style, the order of the studies and the text on each side of the line. If higher values are better for your outcome, swap the two labels. Download the figure under the plot.",
        selectInput("layout", "Style",
          c("Default" = "meta", "RevMan 5" = "RevMan5", "JAMA" = "JAMA", "BMJ" = "BMJ"), width = "100%"),
        selectInput("sort", "Order studies by",
          c("As in the file" = "none", "Year" = "year", "Effect size" = "effect", "Weight" = "weight"),
          width = "100%"),
        conditionalPanel("input.type == 'dich' || input.type == 'cont'",
          fluidRow(
            column(6, textInput("arm_e", "Treatment arm name", "Treatment")),
            column(6, textInput("arm_c", "Control arm name", "Control")))
        ),
        fluidRow(
          column(6, textInput("label_left", "Left of the line", "Favours treatment")),
          column(6, textInput("label_right", "Right of the line", "Favours control"))
        ),
        div(class = "aside", "Edit these two labels if higher values are better for your outcome."),
        checkboxInput("weights", "Show study weights", TRUE),
        checkboxInput("het", "Show heterogeneity (I\u00b2, \u03c4\u00b2)", TRUE),
        checkboxInput("overall_p", "Show test for overall effect", TRUE),
        adv_box("More plot options",
          textInput("smlab", "Heading above the plot (optional)", "", width = "100%"),
          checkboxInput("details", "Describe the methods under the plot", FALSE))
      )
    ),

    # ------------------------------ Results -------------------------------
    div(class = "results",
      conditionalPanel("!output.has_data",
        guided_empty("Your forest plot will appear here",
          "Pool the results of several studies in three steps. Everything is checked as you go, and the figure is ready for a manuscript.",
          list(list("Choose the data type", "Events and totals, means and SDs, a ready-made effect such as a hazard ratio, or a single proportion."),
               list("Upload your CSV", "One row per study. Download the blank template on the left if you want the right columns."),
               list("Read the result", "A forest plot with a plain-language reading of your numbers, plus leave-one-out, funnel plot and more.")),
          preview = forest_preview_svg(), button_id = "load_example2", note = "Made-up numbers, just to see how it works.")
      ),
      conditionalPanel("output.has_data",
        uiOutput("notices"),
        results_bar("dl_all", "Figures, results text, script and data in one file"),
        navset_underline(id = "tabs",
          nav_panel("Forest plot", value = "forest",
            div(class = "sheet", plotOutput("forest_plot", width = "auto", height = "auto")),
            howto_box("forest", uiOutput("hw_forest_guide"), uiOutput("hw_forest")),
            div(class = "downloads",
              span("Download"),
              downloadButton("dl_png", "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_tiff", "TIFF (300 dpi)", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_pdf", "PDF", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_svg", "SVG", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_script", "R script", class = "btn-primary btn-sm")
            )
          ),
          nav_panel("Leave-one-out", value = "loo",
            p(class = "aside", "Each row shows the pooled result after removing that study. If no single study moves the estimate much, the result is robust."),
            div(class = "sheet", plotOutput("loo_plot", width = "auto", height = "auto")),
            howto_box("loo", guide_loo(), uiOutput("hw_loo")),
            div(class = "downloads", span("Download"),
              downloadButton("dl_loo_png", "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_loo_pdf", "PDF", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_loo_svg", "SVG", class = "btn-outline-primary btn-sm"))
          ),
          extra_tabs_ui()[[2]],
          nav_panel("Funnel and bias", value = "funnel",
            div(class = "inline-opts",
              selectInput("bias", "Bias test", c("Egger" = "Egger", "Begg" = "Begg", "Thompson-Sharp" = "Thompson"), width = "200px"),
              checkboxInput("trimfill", "Add trim-and-fill", FALSE)
            ),
            uiOutput("funnel_note"),
            div(class = "sheet narrow", plotOutput("funnel_plot", width = "560px", height = "480px")),
            howto_box("funnel", guide_funnel(), uiOutput("hw_funnel")),
            div(class = "downloads", span("Download"),
              downloadButton("dl_funnel_png", "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_funnel_pdf", "PDF", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_funnel_svg", "SVG", class = "btn-outline-primary btn-sm")),
            verbatimTextOutput("bias_txt")
          ),
          extra_tabs_ui()[[1]],
          nav_panel("Results", value = "results",
            uiOutput("headline"),
            h3("Methods paragraph"),
            uiOutput("methods_txt"),
            h3("Results paragraph"),
            uiOutput("results_txt"),
            h3("Full output"),
            verbatimTextOutput("summary_txt")
          ),
          code_tab_ui(),
          nav_panel("Data", value = "data",
            p(class = "aside", "The file as read. Rows with missing values are skipped in the analysis."),
            DTOutput("data_tbl")
          )
        )
      )
    )
  )),

  conditionalPanel("input.mode == 'km'", div(class = "workspace",
    km_controls_ui(),
    km_results_ui()
  )),

  conditionalPanel("input.mode == 'dig'", div(class = "workspace dig-ws",
    dig_controls_ui(),
    dig_results_ui()
  )),

  conditionalPanel("input.mode == 'review'", review_ui()),

  conditionalPanel("input.mode == 'conv'", div(class = "workspace",
    conv_controls_ui(),
    conv_results_ui()
  )),

  tags$footer(class = "colophon",
    "Forest plots run with the R package meta; Kaplan-Meier curves are rebuilt with IPDfromKM (Guyot et al. 2012). Uploaded files are processed in your session and are not stored.")
)

# --------------------------------------------------------------------------

server <- function(input, output, session) {

  raw <- reactiveVal(NULL)
  detect_msg <- reactiveVal("")

  km_server(input, output, session, forest_raw = raw, forest_note = detect_msg)
  conv_server(input, output, session, forest_raw = raw, forest_note = detect_msg)

  output$has_data <- reactive(!is.null(raw()))
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  # once the data are in, the two first steps fold away and leave their choices as a one-line summary
  observeEvent(!is.null(raw()), {
    ids <- c("choose-your-data-type", "upload-your-csv")
    session$sendCustomMessage(if (!is.null(raw())) "steps_done" else "steps_open", ids)
  }, ignoreInit = TRUE)

  # ---- Template download ----
  output$dl_template <- downloadHandler(
    filename = function() paste0("template_", input$type, ".csv"),
    content = function(file) write_template(file, pick(input$type, names(SPEC)), pick(input$tpl_sep, c(",", ";")))
  )

  # ---- Loading data ----
  observeEvent(input$csv, {
    f <- input$csv
    res <- tryCatch(read_user_csv(f$datapath), error = function(e) e)
    if (inherits(res, "error")) {
      raw(NULL); detect_msg("")
      showNotification(paste("Could not read the file.", conditionMessage(res)), type = "error", duration = 10)
      return()
    }
    res$name <- f$name
    raw(res)
    dt <- detect_type(names(res$data))
    if (is.na(dt)) {
      detect_msg("Could not tell the data type from the column names. Choose it in step 1 and match the columns below.")
    } else {
      updateSelectInput(session, "type", selected = dt)
      detect_msg(paste0("Detected: ", SPEC[[dt]]$title, "."))
    }
  })

  load_example <- function() {
    type <- pick(input$type, names(SPEC))
    d <- example_data(type)
    d[] <- lapply(d, as.character)
    raw(list(data = d, sep = ",", enc = "UTF-8", name = paste0("example_", type, ".csv")))
    detect_msg("Example data (made-up numbers for illustration, not real studies).")
  }
  observeEvent(input$load_example, load_example())
  observeEvent(input$load_example2, load_example())
  # Home: the example buttons open the Forest plot area with the example loaded
  observeEvent(c(input$home_example, input$home_example2), {
    updateRadioButtons(session, "mode", selected = "forest")
    load_example()
  }, ignoreInit = TRUE)

  output$detect_note <- renderUI({
    if (nzchar(detect_msg())) div(class = "aside", detect_msg())
  })

  # ---- Column matching ----
  output$mapping <- renderUI({
    d <- raw(); req(d)
    type <- pick(input$type, names(SPEC))
    nms <- names(d$data)
    keys <- names(SPEC[[type]]$cols)
    g <- guess_columns(c("study", "year", "subgroup", keys), nms)
    required <- if (type == "gen") "effect" else keys
    all_matched <- all(nzchar(g[required])) && (type != "gen" || nzchar(g[["se"]]) || (nzchar(g[["ci_lower"]]) && nzchar(g[["ci_upper"]])))
    sel <- function(id, label, choice0, selected)
      selectInput(paste0("map_", id), label, choices = c(setNames("", choice0), nms), selected = selected, width = "100%")
    req_inputs <- lapply(keys, function(k) {
      lab <- SPEC[[type]]$cols[[k]]
      sel(k, lab, if (type == "gen" && k %in% c("ci_lower", "ci_upper", "se")) "Not used" else "Choose a column", g[[k]])
    })
    tags$details(class = "mapping", open = if (!all_matched) NA else NULL,
      tags$summary(if (all_matched) "Columns matched automatically (review)" else "Match your columns"),
      sel("study", "Study name", "Use row numbers", g[["study"]]),
      req_inputs,
      if (type == "gen") div(class = "aside",
        "Give either the CI limits or the standard error. For HR, OR and RR, enter the estimate and CI as reported (ratio scale); a standard error must be the SE of the log estimate."),
      sel("year", "Year (optional, for ordering)", "None", g[["year"]]),
      sel("group", "Group studies by (optional)", "No subgroups", g[["subgroup"]])
    )
  })

  # ---- Measure options depend on the type ----
  output$measure_ui <- renderUI({
    type <- pick(input$type, names(SPEC))
    tagList(
      selectInput("sm", "Effect measure", SM_CHOICES[[type]], width = "100%")
    )
  })
  # pooling method and zero-cell correction (dichotomous data only) live under "Advanced options"
  output$method_ui <- renderUI({
    type <- pick(input$type, names(SPEC))
    if (type == "dich") tagList(
      selectInput("method", "Pooling method",
        c("Mantel-Haenszel (good with sparse data)" = "MH", "Inverse variance" = "Inverse",
          "Peto (odds ratio only)" = "Peto"), width = "100%"),
      selectInput("incr", "Studies with zero cells",
        c("Add 0.5 only to those studies" = "only0", "Add 0.5 to all studies" = "all"), width = "100%"))
  })
  outputOptions(output, "method_ui", suspendWhenHidden = FALSE)

  current_map <- reactive({
    type <- pick(input$type, names(SPEC))
    keys <- names(SPEC[[type]]$cols)
    cols <- names(raw()$data)
    clean <- function(x) if (length(x) == 1 && !is.na(x) && x %in% cols) x else ""
    c(list(study = clean(input$map_study)),
      setNames(lapply(keys, function(k) clean(input[[paste0("map_", k)]])), keys))
  })

  # ---- The analysis (debounced so typing in a label does not re-run on every key) ----
  fit <- debounce(reactive({
    d <- raw(); req(d)
    type <- pick(input$type, names(SPEC))
    map <- current_map()
    p <- make_params(input, type)
    cols <- names(d$data)
    gcol <- if (!is.null(input$map_group) && input$map_group %in% cols) input$map_group else ""
    ycol <- if (!is.null(input$map_year) && input$map_year %in% cols) input$map_year else ""

    v <- validate_data(d$data, type, map, p)
    if (length(v$errors)) return(list(ok = FALSE, errors = v$errors, warnings = v$warnings))
    if (p$sort == "year" && !nzchar(ycol))
      v$warnings <- c(v$warnings, "Ordering by year needs a year column. Choose one under 'Match your columns'; the file order is used for now.")

    code <- build_code(p, map, v$need, group_col = gcol, year_col = ycol)
    env <- new.env(parent = globalenv())
    env$dat <- d$data
    err <- tryCatch({
      eval(parse(text = code$prepare), env)
      eval(parse(text = code$model), env)
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err))
      return(list(ok = FALSE, errors = paste("The analysis could not run:", err), warnings = v$warnings))

    k <- env$m$k
    n_sub <- if (nzchar(gcol)) length(unique(env$grp)) else 0
    # Exact figure size from meta, with a small margin; fall back to an estimate.
    dm <- tryCatch(eval(parse(text = code$dims), env), error = function(e) NULL)
    # Fonts differ a little between devices (screen, PNG, PDF, SVG), so leave a proportional margin.
    w <- if (is.null(dm)) 11 else round(dm$width * 1.12 + 0.3, 2)
    h <- if (is.null(dm)) plot_height_in(k, n_sub, pi = p$pi && p$model != "common", het = p$het, details = p$details)
         else round(dm$height * 1.05 + 0.3, 2)
    dl <- tryCatch(eval(parse(text = code$loo_dims), env), error = function(e) NULL)
    list(ok = TRUE, env = env, code = code, p = p, k = k, warnings = v$warnings, w = w, h = h,
         loo_w = if (is.null(dl)) 9 else round(dl$width * 1.12 + 0.3, 2),
         loo_h = if (is.null(dl)) max(3, 0.27 * k + 2.5) else round(dl$height * 1.05 + 0.3, 2),
         used_cols = unique(c(unlist(map[v$need]), map$study)),
         csv = d$name, sep = d$sep, enc = d$enc)
  }), 400)

  extra_server(input, output, session, fit, raw)
  review_server(input, output, session, fit)
  code_server(input, output, session, fit)
  output$hw_forest_guide <- renderUI({ f <- fit(); req(isTRUE(f$ok)); guide_forest(f$env$m$sm) })

  output$notices <- renderUI({
    f <- fit()
    tagList(
      lapply(f$errors, function(e) div(class = "notice error", role = "alert", e)),
      lapply(f$warnings, function(w) div(class = "notice warn", w))
    )
  })

  # ---- Drawing ----
  draw <- function(code, f) {
    tryCatch(eval(parse(text = code), f$env), error = function(e) {
      plot.new(); text(0.5, 0.5, paste("This plot could not be drawn.\n", conditionMessage(e)), cex = 0.9)
    })
  }

  output$forest_plot <- renderPlot({
    f <- fit(); req(isTRUE(f$ok))
    draw(f$code$forest, f)
  }, width = function() { f <- fit(); if (isTRUE(f$ok)) round(f$w * 96) else 400 },
   height = function() { f <- fit(); if (isTRUE(f$ok)) round(f$h * 96) else 200 }, res = 96)

  output$loo_plot <- renderPlot({
    f <- fit(); req(isTRUE(f$ok))
    draw(f$code$loo, f)
  }, width = function() { f <- fit(); if (isTRUE(f$ok)) round(f$loo_w * 96) else 400 },
   height = function() { f <- fit(); if (isTRUE(f$ok)) round(f$loo_h * 96) else 200 }, res = 96)

  output$funnel_plot <- renderPlot({
    f <- fit(); req(isTRUE(f$ok))
    draw(f$code$funnel, f)
  }, width = 560, height = 480, res = 96)

  output$funnel_note <- renderUI({
    f <- fit(); req(isTRUE(f$ok))
    if (f$k < 10) div(class = "notice warn",
      sprintf("Only %d studies. Funnel plots and bias tests are unreliable with fewer than 10.", f$k))
  })

  output$bias_txt <- renderPrint({
    f <- fit(); req(isTRUE(f$ok))
    tryCatch(print(eval(parse(text = f$code$bias), f$env)),
             error = function(e) cat("The bias test could not be run:", conditionMessage(e)))
  })

  # ---- Results tab ----
  output$headline <- renderUI({
    f <- fit(); req(isTRUE(f$ok))
    m <- f$env$m
    bt <- function(x) tryCatch(meta::backtransf(x, sm = m$sm, n = m$n.harmonic.mean), error = function(e) x)
    num <- function(x) formatC(x, digits = 2, format = "f")
    pv <- function(x) if (is.na(x)) "n/a" else if (x < 0.0001) "< 0.0001" else formatC(x, digits = 4, format = "f")
    line <- function(lab, te, lo, hi, p) {
      div(class = "stat-line",
        span(class = "stat-label", lab),
        span(class = "stat-value", num(bt(te))),
        span(class = "stat-ci", sprintf("95%% CI %s to %s", num(bt(lo)), num(bt(hi)))),
        span(class = "stat-p", paste("p", if (grepl("^<", pv(p))) pv(p) else paste("=", pv(p)))))
    }
    lab <- if (nzchar(m$sm)) m$sm else "Effect"
    mdl <- f$p$model
    tagList(
      if (mdl %in% c("random", "both")) line(paste(lab, "(random)"), m$TE.random, m$lower.random, m$upper.random, m$pval.random),
      if (mdl %in% c("common", "both")) line(paste(lab, "(common)"), m$TE.common, m$lower.common, m$upper.common, m$pval.common),
      div(class = "stat-line sub",
        span(sprintf("%d studies", m$k)),
        span(sprintf("I\u00b2 = %.0f%%", 100 * m$I2)),
        span(sprintf("\u03c4\u00b2 = %.3f", m$tau2)),
        span(paste("Q test p", if (is.na(m$pval.Q)) "= n/a" else if (m$pval.Q < 0.0001) "< 0.0001" else sprintf("= %.4f", m$pval.Q))))
    )
  })

  output$methods_txt <- renderUI({
    f <- fit(); req(isTRUE(f$ok))
    div(class = "methods-text", enc2utf8(methods_text(f$p, f$k)))
  })
  output$results_txt <- renderUI({
    f <- fit(); req(isTRUE(f$ok))
    txt_ <- tryCatch(results_text(f$env$m, f$p, f$k), error = function(e) "A results paragraph could not be written for this analysis.")
    div(class = "report-block",
      div(class = "report-head", tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = copy_js, "Copy")),
      div(class = "report-text", tags$p(enc2utf8(txt_))))
  })
  output$summary_txt <- renderPrint({
    f <- fit(); req(isTRUE(f$ok)); print(f$env$m)
  })

  output$data_tbl <- renderDT({
    d <- raw(); req(d)
    datatable(d$data, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tip"))
  })

  # ---- Downloads ----
  save_plot <- function(file, ext, f) {
    w <- f$w; h <- f$h
    switch(ext,
      png = ragg::agg_png(file, width = w, height = h, units = "in", res = 300),
      tiff = ragg::agg_tiff(file, width = w, height = h, units = "in", res = 300, compression = "lzw"),
      pdf = grDevices::cairo_pdf(file, width = w, height = h),
      svg = svglite::svglite(file, width = w, height = h))
    on.exit(grDevices::dev.off())
    eval(parse(text = f$code$forest), f$env)
  }
  output$dl_png <- downloadHandler("forest_plot.png", function(file) { f <- fit(); req(isTRUE(f$ok)); save_plot(file, "png", f) })
  output$dl_tiff <- downloadHandler("forest_plot.tiff", function(file) { f <- fit(); req(isTRUE(f$ok)); save_plot(file, "tiff", f) })
  output$dl_pdf <- downloadHandler("forest_plot.pdf", function(file) { f <- fit(); req(isTRUE(f$ok)); save_plot(file, "pdf", f) })
  output$dl_svg <- downloadHandler("forest_plot.svg", function(file) { f <- fit(); req(isTRUE(f$ok)); save_plot(file, "svg", f) })
  # leave-one-out and funnel plots, drawn from the same code as on screen
  save_sens <- function(file, ext, w, h, code, f) {
    switch(ext,
      png = ragg::agg_png(file, width = w, height = h, units = "in", res = 300),
      pdf = grDevices::cairo_pdf(file, width = w, height = h),
      svg = svglite::svglite(file, width = w, height = h))
    on.exit(grDevices::dev.off())
    eval(parse(text = code), f$env)
  }
  for (ext in c("png", "pdf", "svg")) local({
    e2 <- ext
    output[[paste0("dl_loo_", e2)]] <- downloadHandler(paste0("leave_one_out.", e2), function(file) {
      f <- fit(); req(isTRUE(f$ok)); save_sens(file, e2, f$loo_w, f$loo_h, f$code$loo, f) })
    output[[paste0("dl_funnel_", e2)]] <- downloadHandler(paste0("funnel_plot.", e2), function(file) {
      f <- fit(); req(isTRUE(f$ok)); save_sens(file, e2, 5.8, 5, f$code$funnel, f) })
  })
  # everything in one zip
  output$dl_all <- downloadHandler(function() paste0("meta_analysis_", format(Sys.Date(), "%Y%m%d"), ".zip"), function(file) {
    f <- fit(); req(isTRUE(f$ok))
    d <- tempfile("ma_all_"); dir.create(file.path(d, "figures"), recursive = TRUE); dir.create(file.path(d, "tables"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    withProgress(message = "Preparing the zip file", value = 0.1, {
      one <- function(nm, ext, w, h, draw) tryCatch({
        p <- file.path(d, "figures", paste0(nm, ".", ext))
        switch(ext, png = ragg::agg_png(p, width = w, height = h, units = "in", res = 300), pdf = grDevices::cairo_pdf(p, width = w, height = h), svg = svglite::svglite(p, width = w, height = h))
        draw(); grDevices::dev.off()
      }, error = function(err) { while (grDevices::dev.cur() > 1) grDevices::dev.off() })
      for (ext in c("png", "pdf", "svg")) one("forest_plot", ext, f$w, f$h, function() eval(parse(text = f$code$forest), f$env))
      incProgress(0.2)
      for (ext in c("png", "pdf")) {
        one("leave_one_out", ext, f$loo_w, f$loo_h, function() eval(parse(text = f$code$loo), f$env))
        one("funnel_plot", ext, 5.8, 5, function() eval(parse(text = f$code$funnel), f$env))
        one("baujat_plot", ext, 6, 5.2, function() baujat_plot(f$env$m))
        one("galbraith_plot", ext, 6, 5.2, function() radial_plot(f$env$m))
      }
      incProgress(0.3)
      ir <- tryCatch(infl_run(f$env$m, f$p$tau, f$p$hk), error = function(err) NULL)
      if (!is.null(ir) && isTRUE(ir$ok)) utils::write.csv(ir$table, file.path(d, "tables", "influence_diagnostics.csv"), row.names = FALSE)
      if (!is.null(raw())) utils::write.csv(raw()$data, file.path(d, "tables", "data.csv"), row.names = FALSE, na = "")
      txt <- c("METHODS", methods_text(f$p, f$k), "", "RESULTS", tryCatch(results_text(f$env$m, f$p, f$k), error = function(err) ""), "", "FULL OUTPUT", utils::capture.output(print(f$env$m)))
      writeLines(enc2utf8(txt), file.path(d, "methods_results_and_output.txt"), useBytes = TRUE)
      nm <- if (is.null(f$csv) || !grepl("\\.(csv|txt)$", f$csv, ignore.case = TRUE)) "your_data.csv" else f$csv
      writeLines(build_script(f$code, f$p, nm, f$sep, f$enc, f$w, f$h), file.path(d, "forest_plot_analysis.R"), useBytes = TRUE)
      writeLines(c("Contents", "figures/  forest plot (PNG, PDF, SVG), leave-one-out, funnel, Baujat and Galbraith plots", "tables/   the data and the influence diagnostics",
                   "methods_results_and_output.txt  written Methods and Results and the full model output", "forest_plot_analysis.R  script that repeats the analysis"), file.path(d, "README.txt"))
      setProgress(0.95); zip_dir(d, file)
    })
  })
  output$dl_script <- downloadHandler("forest_plot_analysis.R", function(file) {
    f <- fit(); req(isTRUE(f$ok))
    nm <- if (is.null(f$csv) || !grepl("\\.(csv|txt)$", f$csv, ignore.case = TRUE)) "your_data.csv" else f$csv
    writeLines(build_script(f$code, f$p, nm, f$sep, f$enc, f$w, f$h), file, useBytes = TRUE)
  })
}

shinyApp(ui, server)
