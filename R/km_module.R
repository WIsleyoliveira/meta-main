# Kaplan-Meier mode: interface pieces and server logic.
# All input ids start with "km_" so they never clash with the forest-plot inputs.

# A numbered step in the left column (shared by both modes; defined here so R/ code can see it).
step <- function(title, ..., help = NULL, open = FALSE) {
  # the text appears when the mouse rests on the button for a moment (see www/hints.js), no click needed
  info <- if (!is.null(help)) tags$button(type = "button", class = "info-btn", tabindex = "0",
    `aria-label` = paste("What to do:", title), `data-help` = help, tags$span("i"))
  # A step can be folded: the header keeps its number and, when folded, a one-line summary of the choices made (see www/ui.js)
  div(class = paste("step", if (!open) "collapsed"), `data-step` = gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", tolower(title))),
    div(class = "step-head", role = "button", tabindex = "0", `aria-expanded` = if (open) "true" else "false", h3(title), info, tags$span(class = "step-chev", `aria-hidden` = "true")),
    div(class = "step-sum"),
    div(class = "step-body", ...))
}

KM_PARAM_NAMES <- c("scale", "curve_type", "display", "time_unit", "name_tx", "name_ctrl", "conf",
                    "marks", "risk_table", "ypct", "xmax", "ymin", "landmarks", "tau",
                    "landmark_time", "cuts", "truncs", "tv_df", "outcome")

# Pull the km_* inputs into the plain list that km_params() expects.
km_pin <- function(input, which = KM_PARAM_NAMES) {
  out <- lapply(which, function(n) input[[paste0("km_", n)]])
  names(out) <- which
  out
}

km_file_input <- function(id, label)
  fileInput(paste0("km_file_", id), label, accept = c(".csv", ".txt", "text/csv"),
            buttonLabel = "Choose file", placeholder = "No file yet", width = "100%")

# A plot with PNG and PDF download buttons
km_plot_block <- function(id, howto = NULL) {
  tagList(
    div(class = "sheet", plotOutput(paste0("km_plot_", id), width = "auto", height = "auto")),
    howto,
    div(class = "downloads",
      span("Download"),
      downloadButton(paste0("km_dl_", id, "_png"), "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
      downloadButton(paste0("km_dl_", id, "_pdf"), "PDF", class = "btn-outline-primary btn-sm"))
  )
}

# Copy the text of a report block to the clipboard
copy_js <- paste0("var b=this; var t=b.closest('.report-block').querySelector('.report-text').innerText;",
                  "navigator.clipboard.writeText(t).then(function(){b.textContent='Copied'; setTimeout(function(){b.textContent='Copy'},1500);});")

# Small HTML table from a data frame of strings
km_table <- function(d) {
  tags$table(class = "table table-sm km-table",
    tags$thead(tags$tr(lapply(names(d), tags$th))),
    tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(lapply(d[i, , drop = FALSE], function(x) tags$td(as.character(x)))))))
}
f2 <- function(x) ifelse(is.na(x), "n/a", sprintf("%.2f", x))
fp <- function(x) ifelse(is.na(x), "n/a", ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x)))
fci <- function(h, l, u) ifelse(is.na(h), "n/a", sprintf("%.2f (%.2f to %.2f)", h, l, u))

# ---- Interface ---------------------------------------------------------------

km_controls_ui <- function() {
  div(class = "controls",
    step("Download the three templates", help = "Download the three CSV templates and fill them from the papers: curve points, numbers at risk and arms. The Survival curves step 1 mode can fill the curve points for you.",
      radioButtons("km_tpl_sep", "CSV separator in the templates",
        choices = c("Comma (,)" = ",", "Semicolon (;), for Excel in Brazil and Europe" = ";"), selected = ","),
      div(class = "tpl-buttons",
        downloadButton("km_dl_curve", "1. Curve points", class = "btn-outline-primary btn-sm"),
        downloadButton("km_dl_risk", "2. Numbers at risk", class = "btn-outline-primary btn-sm"),
        downloadButton("km_dl_arms", "3. Arms", class = "btn-outline-primary btn-sm")),
      div(class = "aside",
        tags$b("Curve points: "), "points read from each published curve, one row per point (study_id, arm, time, survival). ",
        "A digitizer such as WebPlotDigitizer exports exactly this. ", tags$br(),
        tags$b("Numbers at risk: "), "the table printed under each curve (study_id, arm, time, n_risk). ", tags$br(),
        tags$b("Arms: "), "one row per arm, with group = treatment or control, the number of patients, and total events if reported. ",
        "Optionally, type the hazard ratio the paper reports (hr_published, hr_ci_lower, hr_ci_upper) on the treatment row, to compare it with the reconstructed one.")
    ),
    step("Upload your three files", help = "Upload each filled file in its own box, in order. Or click Load example data. The note below tells you if something is missing.",
      km_file_input("curve", "Curve points"),
      km_file_input("risk", "Numbers at risk"),
      km_file_input("arms", "Arms"),
      actionLink("km_load_example", "Load example data instead"),
      uiOutput("km_file_note")
    ),
    step("Set up the analysis", help = "Say whether your curve values are proportions or percent, and whether the figures show survival or cumulative incidence. Everything else is optional.",
      selectInput("km_scale", "Survival values in the curve file",
        c("Proportions (0 to 1)" = "proportion", "Percent (0 to 100)" = "percent"), width = "100%"),
      selectInput("km_curve_type", "The published curve shows",
        c("Survival (falling curve)" = "survival", "Cumulative incidence (rising curve)" = "cumulative"), width = "100%"),
      textInput("km_outcome", "Outcome (used in the written report)", "", placeholder = "for example all-cause mortality", width = "100%"),
      textInput("km_time_unit", "Time unit", "months", width = "100%"),
      numericInput("km_tau", "RMST horizon (optional, in time units)", value = NA, min = 0, width = "100%"),
      div(class = "aside", "Restricted mean survival time is computed up to this time. Left empty, it uses the shortest follow-up among the studies."),
      textInput("km_landmarks", "Survival at these times (optional)", "12, 24, 36", width = "100%")
    ),
    step("Hazards that change over time, and sensitivity", open = FALSE, help = "Optional. If you leave all of these empty, the app chooses sensible values from the length of follow-up.",
      numericInput("km_landmark_time", "Landmark time (optional)", value = NA, min = 0, width = "100%"),
      textInput("km_cuts", "Hazard ratio by interval: cut points (optional)", "", placeholder = "for example 6, 24", width = "100%"),
      textInput("km_truncs", "Shorter follow-up: cut at (optional)", "", placeholder = "for example 12, 36", width = "100%"),
      selectInput("km_tv_df", "Flexibility of the hazard-ratio-over-time curve",
        c("Smooth (2)" = "2", "Moderate (3)" = "3", "Flexible (4)" = "4", "Very flexible (5)" = "5"), selected = "3", width = "100%"),
      div(class = "aside", "Anything left empty is chosen from the length of follow-up. The values used are shown with the results.")
    ),
    step("Shape the plot", open = FALSE, help = "Optional. Name the two arms and choose what to show on the curve. Download the figure under the plot.",
      fluidRow(
        column(6, textInput("km_name_tx", "Treatment arm name", "Treatment")),
        column(6, textInput("km_name_ctrl", "Control arm name", "Control"))),
      selectInput("km_display", "Plot",
        c("Survival" = "survival", "Cumulative incidence" = "cumulative"), width = "100%"),
      checkboxInput("km_conf", "Show 95% confidence bands", TRUE),
      checkboxInput("km_marks", "Mark censored patients", FALSE),
      checkboxInput("km_risk_table", "Show reconstructed numbers at risk", TRUE),
      checkboxInput("km_ypct", "Show the vertical axis as percent", FALSE),
      fluidRow(
        column(6, numericInput("km_xmax", "Time axis ends at", value = NA, min = 0)),
        column(6, numericInput("km_ymin", "Vertical axis starts at", value = 0, min = 0, max = 0.95, step = 0.1)))
    )
  )
}

km_results_ui <- function() {
  div(class = "results",
    conditionalPanel("!output.km_has_data",
      guided_empty("Your pooled Kaplan-Meier curve will appear here",
        "Rebuild patient-level data from published survival curves, then pool them into one analysis with a hazard ratio and its checks.",
        list(list("Digitize the curves", "In step 1 (above), load each published figure and click along the curves. It exports the three tables for you."),
             list("Upload the three files", "Curve points, numbers at risk and arms. Download the templates on the left if you prefer to fill them by hand."),
             list("Read the analysis", "Pooled curve, hazard ratios, proportional-hazards checks, sensitivity analyses and a written Methods and Results.")),
        preview = km_preview_svg(), button_id = "km_load_example2", note = "Three simulated trials, just to see how it works.")
    ),
    conditionalPanel("output.km_has_data",
      uiOutput("km_notices"),
      results_bar("km_dl_all", "Figures, tables, text, script and data in one file"),
      navset_underline(id = "km_tabs",
        nav_panel("Pooled curve", value = "pooled",
          km_plot_block("pooled", howto_box("km_pooled", guide_km_pooled(), uiOutput("hw_km_pooled"))),
          div(class = "downloads",
            span("More"),
            downloadButton("km_dl_svg", "SVG", class = "btn-outline-primary btn-sm"),
            downloadButton("km_dl_script", "R script (everything)", class = "btn-primary btn-sm"),
            downloadButton("km_dl_ipd", "Reconstructed patients (CSV)", class = "btn-outline-primary btn-sm"))
        ),
        nav_panel("By study", value = "bystudy",
          p(class = "aside", "Each panel is the reconstructed curve of one study. Dots are the points you digitized; the curve should pass through them."),
          km_plot_block("study", howto_box("km_study", guide_km_study(), uiOutput("hw_km_study")))
        ),
        nav_panel("Summary", value = "summary",
          howto_box("km_summary", guide_km_summary(), uiOutput("hw_km_summary")),
          h3("By arm"),
          uiOutput("km_arm_summary"),
          h3("Median, restricted mean and landmark survival"),
          uiOutput("km_survival_tables")
        ),
        nav_panel("Hazard ratios", value = "hr",
          howto_box("km_hr", guide_km_hr(), uiOutput("hw_km_hr")),
          uiOutput("km_headline"),
          h3("Each study"),
          uiOutput("km_hr_table"),
          div(class = "aside", "Hazard ratios rebuilt from curves are approximations. When a paper reports its own hazard ratio, enter it in the arms file and compare it here."),
          h3("Pool them in a forest plot"),
          radioButtons("km_send_src", NULL,
            choices = c("Reconstructed hazard ratios" = "reconstructed",
                        "Published where available, reconstructed for the others" = "published_first",
                        "Published hazard ratios only" = "published_only"), selected = "reconstructed"),
          div(class = "downloads",
            actionButton("km_send", "Send to the forest plot", class = "btn-primary btn-sm"),
            downloadButton("km_dl_hr", "Study hazard ratios (CSV)", class = "btn-outline-primary btn-sm"))
        ),
        nav_panel("Non-proportional hazards", value = "nph",
          uiOutput("km_nph_verdict"),
          h3("Landmark analysis"),
          p(class = "aside", "Hazard ratio before the landmark, and among patients still alive and under follow-up at the landmark. The curves restart at the landmark."),
          km_plot_block("landmark", howto_box("km_nph", guide_km_nph(), uiOutput("hw_km_nph"))),
          uiOutput("km_landmark_table"),
          h3("Hazard ratio by time interval"),
          uiOutput("km_piecewise_table"),
          h3("Hazard ratio over time"),
          p(class = "aside", "A Cox model whose treatment effect is allowed to change smoothly with time (natural splines). The band is the 95% confidence interval; the ends of the curve are the least reliable."),
          km_plot_block("tv"),
          h3("Log-log plot"),
          km_plot_block("loglog")
        ),
        nav_panel("Sensitivity", value = "sens",
          uiOutput("km_sens_verdict"),
          div(class = "downloads",
            actionButton("km_run_variants", "Add reconstruction checks (a few seconds)", class = "btn-outline-primary btn-sm"),
            numericInput("km_digit_B", "Digitizing-error repeats", value = 30, min = 0, max = 200, step = 10, width = "170px"),
            numericInput("km_digit_noise", "Reading error (survival units)", value = 0.005, min = 0.001, max = 0.05, step = 0.001, width = "190px"),
            span(class = "aside", "Rebuilds the patients without the reported events, and without the numbers at risk. It also repeats the reconstruction with small random reading errors added to the digitized points, to show how much digitizing error widens the pooled hazard ratio (0 repeats skips it).")),
          km_plot_block("sens", howto_box("km_sens", guide_km_sens(), uiOutput("hw_km_sens"))),
          uiOutput("km_sens_table"),
          uiOutput("km_digit_note"),
          uiOutput("km_heterogeneity"),
          h3("Leave one study out"),
          uiOutput("km_loo_note"),
          km_plot_block("loo"),
          h3("Reconstructed against published"),
          km_plot_block("pub")
        ),
        nav_panel("Report text", value = "report",
          p(class = "aside", "Methods and Results paragraphs written from the analysis on screen, ready to paste into a manuscript. Read them, adapt them to your protocol, and check the references before submitting."),
          uiOutput("km_report"),
          div(class = "downloads", downloadButton("km_dl_report", "Download as text (.txt)", class = "btn-outline-primary btn-sm"))
        ),
        nav_panel("Check", value = "check",
          howto_box("km_check", guide_km_check(), uiOutput("hw_km_check")),
          p(class = "aside", "How closely each reconstruction matches what was published. Reconstructed events should be close to the reported events, and the reconstructed numbers at risk close to your table."),
          uiOutput("km_check_notes"),
          DTOutput("km_check_tbl"),
          h3("Rebuilt curve against the digitized points"),
          p(class = "aside", "The rebuilt curve is read at the same times as your digitized points. Small differences are expected; large ones mean the points, the numbers at risk or the total events disagree. The median is the first time the curve reaches 50%."),
          DTOutput("km_fidelity_tbl")
        ),
        nav_panel("Data", value = "data",
          h3("Curve points"), DTOutput("km_data_curve"),
          h3("Numbers at risk"), DTOutput("km_data_risk"),
          h3("Arms"), DTOutput("km_data_arms")
        )
      )
    )
  )
}

# ---- Server ------------------------------------------------------------------

km_server <- function(input, output, session, forest_raw, forest_note) {

  files <- reactiveValues(curve = NULL, risk = NULL, arms = NULL)
  file_msg <- reactiveVal(character(0))
  dig_server(input, output, session, files, file_msg)

  output$km_has_data <- reactive(!is.null(files$curve) && !is.null(files$risk) && !is.null(files$arms))
  outputOptions(output, "km_has_data", suspendWhenHidden = FALSE)
  observeEvent(!is.null(files$curve) && !is.null(files$risk) && !is.null(files$arms), {
    ok <- !is.null(files$curve) && !is.null(files$risk) && !is.null(files$arms)
    session$sendCustomMessage(if (ok) "steps_done" else "steps_open", c("download-the-three-templates", "upload-your-three-files"))
  }, ignoreInit = TRUE)

  # ---- Templates
  for (w in c("curve", "risk", "arms")) local({
    which <- w
    output[[paste0("km_dl_", which)]] <- downloadHandler(
      filename = function() paste0("km_template_", which, ".csv"),
      content = function(file) km_write_template(file, which, pick(input$km_tpl_sep, c(",", ";"))))
  })

  # ---- Loading the files
  read_one <- function(which, f) {
    res <- tryCatch(read_user_csv(f$datapath), error = function(e) e)
    if (inherits(res, "error")) {
      files[[which]] <- NULL
      showNotification(paste0("Could not read the ", tolower(KM_TITLES[[which]]), " file. ", conditionMessage(res)), type = "error", duration = 10)
      return(invisible())
    }
    st <- km_standardize(res$data, which)
    if (!is.null(st$error)) {
      files[[which]] <- NULL
      showNotification(st$error, type = "error", duration = 12)
      return(invisible())
    }
    files[[which]] <- list(data = st$data, orig = st$orig, name = f$name, sep = res$sep, enc = res$enc)
    file_msg(character(0))
  }
  observeEvent(input$km_file_curve, read_one("curve", input$km_file_curve))
  observeEvent(input$km_file_risk, read_one("risk", input$km_file_risk))
  observeEvent(input$km_file_arms, read_one("arms", input$km_file_arms))

  load_example <- function() {
    ex <- km_example()
    for (w in c("curve", "risk", "arms")) {
      d <- ex[[w]]; d[] <- lapply(d, as.character)
      files[[w]] <- list(data = d, orig = setNames(KM_COLS[[w]], KM_COLS[[w]]),
                         name = paste0("example_km_", w, ".csv"), sep = ",", enc = "UTF-8")
    }
    updateSelectInput(session, "km_scale", selected = "proportion")
    updateSelectInput(session, "km_curve_type", selected = "survival")
    file_msg("Example data: three simulated trials (made-up numbers for illustration, not real studies). Trial B has no published hazard ratio.")
  }
  observeEvent(input$km_load_example, load_example())
  observeEvent(input$km_load_example2, load_example())

  output$km_file_note <- renderUI({
    got <- vapply(c("curve", "risk", "arms"), function(w) !is.null(files[[w]]), logical(1))
    tagList(
      if (any(got) && !all(got))
        div(class = "aside", paste0("Still needed: ", paste(tolower(KM_TITLES[names(got)[!got]]), collapse = ", "), ".")),
      if (length(file_msg())) div(class = "aside", file_msg()))
  })

  # ---- Core analysis (reconstruction is slow, so it only reruns when its own inputs change)
  core <- debounce(reactive({
    cu <- files$curve; rk <- files$risk; ar <- files$arms
    req(cu, rk, ar)
    p <- km_params(km_pin(input, c("scale", "curve_type", "tau", "landmarks")))
    if (!km_available())
      return(list(ok = FALSE, errors = paste0("This mode needs the R packages survival and IPDfromKM. ",
        "Install them once with install.packages(c(\"survival\", \"IPDfromKM\")) and restart the app."), warnings = character(0)))
    v <- km_validate(cu$data, rk$data, ar$data, p)
    if (length(v$errors)) return(list(ok = FALSE, errors = v$errors, warnings = v$warnings))
    km_attach()
    code <- list(prep = paste(km_prep_code(p), collapse = "\n"), analysis = km_analysis_code(p))
    # parent = the app's own environment, so the km_plot_* functions are found
    env <- new.env(parent = environment(km_server))
    env$curve <- cu$data; env$risk <- rk$data; env$arms <- ar$data
    err <- tryCatch({
      withProgress(message = "Reconstructing patients", detail = "This takes a few seconds", value = 0.3, {
        eval(parse(text = code$prep), env)
        eval(parse(text = code$analysis), env)
      })
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err))
      return(list(ok = FALSE, errors = paste("The reconstruction could not run:", err), warnings = v$warnings))
    list(ok = TRUE, env = env, code = code, warnings = v$warnings)
  }), 500)

  # ---- Extra analyses (non-proportional hazards and sensitivity); rerun when their own inputs change
  extra <- debounce(reactive({
    f <- core(); req(isTRUE(f$ok))
    p <- km_params(km_pin(input, c("landmark_time", "cuts", "truncs", "tv_df")))
    env <- new.env(parent = f$env)
    err <- tryCatch({
      withProgress(message = "Running the additional analyses", value = 0.5, {
        eval(parse(text = km_extra_code(p)), env)
        eval(parse(text = km_sens_code(p)), env)
      })
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) return(list(ok = FALSE, error = err))
    list(ok = TRUE, env = env, p = p)
  }), 600)

  # The slow reconstruction variants only run when asked for, and are dropped when the inputs change.
  variants <- eventReactive(input$km_run_variants, {
    e <- extra(); req(isTRUE(e$ok))
    env <- new.env(parent = e$env)
    err <- tryCatch({
      withProgress(message = "Rebuilding the patients with less information", value = 0.4,
                   eval(parse(text = km_variants_code(
                     digit_B = max(0, min(200, as.integer(input$km_digit_B %||% 30))),
                     digit_noise = max(0.001, min(0.05, as.numeric(input$km_digit_noise %||% 0.005))))), env))
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) { showNotification(paste("The reconstruction checks could not run:", err), type = "error"); req(FALSE) }
    list(env = env, base = e)
  })
  sens_env <- reactive({
    e <- extra(); req(isTRUE(e$ok))
    v <- tryCatch(variants(), error = function(err) NULL)
    if (!is.null(v) && identical(v$base, e)) v$env else e$env
  })

  plot_p <- reactive(km_params(km_pin(input)))
  plot_w <- 9.5
  plot_h <- reactive(if (isTRUE(plot_p()$risk_table)) 7.4 else 5.6)
  study_dims <- reactive({
    f <- core(); req(isTRUE(f$ok))
    k <- nlevels(f$env$ipd$study_id); nc <- min(3, k); nr <- ceiling(k / nc)
    c(w = max(5, 3.7 * nc + 0.3), h = 3.5 * nr + 0.3)
  })

  output$km_notices <- renderUI({
    f <- core()
    e <- if (isTRUE(f$ok)) extra() else NULL
    tagList(
      lapply(f$errors, function(e) div(class = "notice error", role = "alert", e)),
      lapply(f$warnings, function(w) div(class = "notice warn", w)),
      if (!is.null(e) && !isTRUE(e$ok)) div(class = "notice error", role = "alert", paste("The additional analyses could not run:", e$error)))
  })

  draw <- function(code, env) {
    tryCatch(eval(parse(text = code), env), error = function(e) {
      plot.new(); text(0.5, 0.5, paste("This plot could not be drawn.\n", conditionMessage(e)), cex = 0.9)
    })
  }
  save_dev <- function(file, ext, w, h) {
    switch(ext,
      png = ragg::agg_png(file, width = w, height = h, units = "in", res = 300),
      pdf = grDevices::cairo_pdf(file, width = w, height = h),
      svg = svglite::svglite(file, width = w, height = h))
  }

  # ---- Every plot: how to draw it, in which environment, and how big
  calls <- reactive(km_extra_calls(plot_p()))
  n_groups <- function(x) length(unique(x))
  spec <- function(id) {
    switch(id,
      pooled = list(code = km_plot_call(plot_p()), env = core()$env, w = plot_w, h = plot_h()),
      study = { d <- study_dims(); list(code = km_by_study_call(plot_p()), env = core()$env, w = d[["w"]], h = d[["h"]]) },
      landmark = list(code = calls()$landmark, env = extra()$env, w = 9.5, h = 5.8),
      tv = list(code = calls()$tv, env = extra()$env, w = 8.5, h = 5.4),
      loglog = list(code = calls()$loglog, env = extra()$env, w = 7.5, h = 5.2),
      sens = { e <- sens_env(); a <- e$sens_all
               list(code = calls()$sens, env = e, w = 10, h = km_rows_height(nrow(a), n_groups(a$group))) },
      loo = { e <- extra()$env; n <- if (is.null(e$loo_tab)) 1 else nrow(e$loo_tab)
              list(code = if (is.null(e$loo_tab)) "plot.new(); text(0.5, 0.5, 'Leave-one-out needs at least 3 studies.', cex = 0.9)" else calls()$loo,
                   env = e, w = 9, h = km_rows_height(n)) },
      pub = { e <- extra()$env; n <- sum(!is.na(e$pub_tab$hr_published))
              list(code = calls()$pub, env = e, w = 9, h = km_rows_height(max(2, 2 * n), max(1, n))) })
  }
  for (id in c("pooled", "study", "landmark", "tv", "loglog", "sens", "loo", "pub")) local({
    pid <- id
    ready <- function() { f <- core(); req(isTRUE(f$ok)); if (!pid %in% c("pooled", "study")) { e <- extra(); req(isTRUE(e$ok)) } }
    output[[paste0("km_plot_", pid)]] <- renderPlot({
      ready(); s <- spec(pid); draw(s$code, s$env)
    }, width = function() { ready(); round(spec(pid)$w * 96) }, height = function() { ready(); round(spec(pid)$h * 96) }, res = 96)
    for (ext in c("png", "pdf")) local({
      e2 <- ext
      output[[paste0("km_dl_", pid, "_", e2)]] <- downloadHandler(
        filename = function() paste0("km_", pid, ".", e2),
        content = function(file) {
          ready(); s <- spec(pid)
          save_dev(file, e2, s$w, s$h); on.exit(grDevices::dev.off())
          eval(parse(text = s$code), s$env)
        })
    })
  })
  output$km_dl_svg <- downloadHandler("km_pooled.svg", function(file) {
    s <- spec("pooled"); save_dev(file, "svg", s$w, s$h); on.exit(grDevices::dev.off()); eval(parse(text = s$code), s$env)
  })

  # ---- Summary tab
  output$km_arm_summary <- renderUI({
    e <- extra(); req(isTRUE(e$ok)); x <- e$env
    p <- plot_p(); u <- if (nzchar(p$time_unit)) p$time_unit else "time units"
    nm <- c(control = if (nzchar(p$name_ctrl)) p$name_ctrl else "Control", treatment = if (nzchar(p$name_tx)) p$name_tx else "Treatment")
    a <- x$arm_summary
    d <- data.frame(Arm = unname(nm[a$group]), Patients = format(a$patients, big.mark = ","), Events = format(a$events, big.mark = ","),
                    `Person-time` = format(round(a$person_time), big.mark = ","),
                    `Events per 100 person-time units` = sprintf("%.2f", a$rate_per_100),
                    `Median follow-up (reverse Kaplan-Meier)` = ifelse(is.na(a$median_followup), "n/a", sprintf("%.1f %s", a$median_followup, u)),
                    check.names = FALSE)
    tagList(km_table(d),
      div(class = "stat-line sub", span(sprintf("Crude rate ratio (treatment vs control): %.2f (95%% CI %.2f to %.2f). It ignores differences between studies; use the hazard ratio for the main comparison.",
                                                x$rate_ratio$rr, x$rate_ratio$lo, x$rate_ratio$hi))))
  })

  output$km_survival_tables <- renderUI({
    f <- core(); req(isTRUE(f$ok)); e <- f$env
    ex <- extra(); xe <- if (isTRUE(ex$ok)) ex$env else NULL
    p <- plot_p(); u <- if (nzchar(p$time_unit)) p$time_unit else "time units"
    nm <- c(control = if (nzchar(p$name_ctrl)) p$name_ctrl else "Control", treatment = if (nzchar(p$name_tx)) p$name_tx else "Treatment")
    r1 <- function(x) ifelse(is.na(x), "not reached", sprintf("%.1f", x))
    med <- e$medians; rm <- e$rmst
    tagList(
      km_table(data.frame(
        Arm = unname(nm[med$group]),
        `Median survival` = sprintf("%s %s (95%% CI %s to %s)", r1(med$median), u, r1(med$lower), r1(med$upper)),
        `RMST up to the horizon` = sprintf("%.2f %s (SE %.2f)", rm$rmst, u, rm$se), check.names = FALSE)),
      div(class = "stat-line sub",
        span(sprintf("RMST horizon: %s %s. RMST difference (treatment minus control): %.2f %s (95%% CI %.2f to %.2f)", signif(rm$tau[1], 4), u,
                     e$rmst_diff$diff, u, e$rmst_diff$diff - 1.96 * e$rmst_diff$se, e$rmst_diff$diff + 1.96 * e$rmst_diff$se))),
      if (!is.null(e$landmark_tab) && nrow(e$landmark_tab)) {
        lt <- e$landmark_tab; rd <- if (!is.null(xe)) xe$rd_tab else NULL
        rows <- lapply(sort(unique(lt$time)), function(tm) {
          cell <- function(g) { z <- lt[lt$time == tm & lt$group == g, ]
            if (!nrow(z) || is.na(z$survival[1])) "n/a" else sprintf("%.1f%% (%.1f to %.1f)", 100 * z$survival[1], 100 * z$lower[1], 100 * z$upper[1]) }
          r <- rd[rd$time == tm, ]
          data.frame(Time = paste(tm, u), a = cell("control"), b = cell("treatment"),
                     Difference = if (is.null(r) || !nrow(r) || is.na(r$difference[1])) "n/a" else sprintf("%+.1f points (%.1f to %.1f)", 100 * r$difference[1], 100 * r$lower[1], 100 * r$upper[1]),
                     check.names = FALSE)
        })
        d <- do.call(rbind, rows); names(d)[2:3] <- unname(nm[c("control", "treatment")])
        tagList(h3("Survival at fixed times"), km_table(d))
      })
  })

  # ---- Hazard ratios tab
  output$km_headline <- renderUI({
    f <- core(); req(isTRUE(f$ok)); e <- f$env
    p <- plot_p(); tx <- if (nzchar(p$name_tx)) p$name_tx else "Treatment"; ct <- if (nzchar(p$name_ctrl)) p$name_ctrl else "Control"
    h <- e$hr_pooled
    pc <- function(x) if (is.na(x)) "n/a" else if (x < 0.001) "< 0.001" else sprintf("= %.3f", x)
    tagList(
      div(class = "stat-line",
        span(class = "stat-label", "Hazard ratio"),
        span(class = "stat-value", sprintf("%.2f", h$hr)),
        span(class = "stat-ci", sprintf("95%% CI %.2f to %.2f", h$lo, h$hi)),
        span(class = "stat-p", paste("p", pc(h$p)))),
      div(class = "stat-line sub",
        span(sprintf("%s vs %s, Cox model stratified by study", tx, ct)),
        span(paste("Stratified log-rank p", pc(e$logrank_p))),
        span(sprintf("%d %s, %s patients", nlevels(e$ipd$study_id), if (nlevels(e$ipd$study_id) == 1) "study" else "studies", format(nrow(e$ipd), big.mark = ",")))),
      if (!is.na(e$ph_p) && e$ph_p < 0.05)
        div(class = "notice warn", sprintf(
          "The proportional hazards test is significant (p %s). The hazard ratio changes over time, so a single hazard ratio is only a rough summary. See the Non-proportional hazards tab, and the restricted mean survival time in Summary.", pc(e$ph_p)))
      else if (!is.na(e$ph_p))
        div(class = "aside", sprintf("Proportional hazards test: p %s.", pc(e$ph_p)))
    )
  })

  output$km_hr_table <- renderUI({
    e <- extra(); req(isTRUE(e$ok)); d <- e$env$pub_tab
    out <- data.frame(Study = d$study_id,
      `Reconstructed HR (95% CI)` = fci(d$hr_reconstructed, d$rec_lo, d$rec_hi),
      `Published HR (95% CI)` = ifelse(is.na(d$hr_published), "not entered", fci(d$hr_published, d$pub_lo, d$pub_hi)),
      `Reconstructed / published` = ifelse(is.na(d$ratio_rec_to_pub), "", sprintf("%.2f", d$ratio_rec_to_pub)),
      check.names = FALSE)
    km_table(out)
  })

  # ---- Non-proportional hazards tab
  output$km_nph_verdict <- renderUI({
    e <- extra(); req(isTRUE(e$ok)); x <- e$env
    php <- if (is.null(x$ph_tab)) NA_real_ else x$ph_tab$p
    p_tv <- x$tv_p
    flag_ <- (!is.na(php) && php < 0.05) || (!is.na(p_tv) && p_tv < 0.05)
    tagList(
      div(class = "stat-line sub",
        span(paste("Proportional hazards test (Schoenfeld): p", if (is.na(php)) "n/a" else fp(php))),
        span(paste("Test that the hazard ratio is constant over time (spline model): p", if (is.na(p_tv)) "n/a" else fp(p_tv))),
        span(sprintf("Landmark used: %s. Intervals: %s.", format(x$lm_time), paste(x$piecewise$interval, collapse = "; ")))),
      if (flag_) div(class = "notice warn", "There is evidence that the effect of treatment changes over time. Report the landmark or interval hazard ratios (or the restricted mean survival time) alongside, or instead of, the single hazard ratio. Check whether the curves cross.")
      else div(class = "aside", "No clear evidence that the hazard ratio changes over time. With few events, a test can miss a real change, so also look at the hazard-ratio-over-time plot."),
      if (!is.null(x$ph_study) && nrow(x$ph_study) > 1)
        div(class = "aside", paste0("Proportional hazards test by study: ", paste(sprintf("%s p %s", x$ph_study$study_id, fp(x$ph_study$ph_p)), collapse = "; "), ".")))
  })
  output$km_landmark_table <- renderUI({
    e <- extra(); req(isTRUE(e$ok)); d <- e$env$landmark_hr
    km_table(data.frame(Period = d$period, `Events, control` = d$events_control, `Events, treatment` = d$events_treatment,
                        `Hazard ratio (95% CI)` = fci(d$hr, d$lo, d$hi), p = fp(d$p), check.names = FALSE))
  })
  output$km_piecewise_table <- renderUI({
    e <- extra(); req(isTRUE(e$ok)); d <- e$env$piecewise
    km_table(data.frame(Interval = d$interval, `Events, control` = d$events_control, `Events, treatment` = d$events_treatment,
                        `Hazard ratio (95% CI)` = fci(d$hr, d$lo, d$hi), p = fp(d$p), check.names = FALSE))
  })

  # ---- Sensitivity tab
  output$km_sens_verdict <- renderUI({
    e <- sens_env(); a <- e$sens_all
    ok <- is.finite(a$hr)
    if (!any(ok)) return(NULL)
    below <- sum(a$hr[ok] < 1); sig <- sum(a$lo[ok] > 1 | a$hi[ok] < 1)
    tagList(
      div(class = "stat-line sub",
        span(sprintf("%d analyses: hazard ratios from %.2f to %.2f", sum(ok), min(a$hr[ok]), max(a$hr[ok]))),
        span(sprintf("%d point below 1, %d above 1", below, sum(ok) - below)),
        span(sprintf("%d have a confidence interval that excludes 1", sig))),
      div(class = "aside", "These analyses use the same patients, so agreement between them does not make the result more certain; it shows that the result does not depend on one modelling choice. Follow-up cuts answer a different question (the effect early on), so they can differ."))
  })
  output$km_sens_table <- renderUI({
    e <- sens_env(); a <- e$sens_all
    km_table(data.frame(Analysis = a$analysis, `Hazard ratio (95% CI)` = fci(a$hr, a$lo, a$hi), p = fp(a$p), check.names = FALSE))
  })
  output$km_digit_note <- renderUI({
    e <- sens_env(); d <- e$digit_tab
    if (is.null(d)) return(NULL)
    div(class = "stat-line sub",
      span(sprintf("Digitizing error (%d repeats, SD %.3f): pooled HR %.2f, SD of log HR %.3f; HR range %.2f to %.2f",
                   d$runs, d$noise, d$hr_base, d$sd_loghr, d$hr_lo_2.5, d$hr_hi_97.5)),
      span(sprintf("95%% CI widened from %.2f-%.2f to %.2f-%.2f", exp(log(d$hr_base) - 1.96 * d$se_loghr_base), exp(log(d$hr_base) + 1.96 * d$se_loghr_base), d$ci_total_lo, d$ci_total_hi)))
  })
  output$km_heterogeneity <- renderUI({
    e <- extra(); req(isTRUE(e$ok)); h <- e$env$heterogeneity
    if (is.null(h)) return(NULL)
    div(class = "stat-line sub",
      span(sprintf("Heterogeneity between studies (two-stage): I\u00b2 = %.0f%%, \u03c4\u00b2 = %.3f, Q test p %s", 100 * h$I2, h$tau2, fp(h$Q_p))),
      if (!is.na(h$pred_lo)) span(sprintf("95%% prediction interval for a new study: %.2f to %.2f", h$pred_lo, h$pred_hi)))
  })
  output$km_loo_note <- renderUI({
    e <- extra(); req(isTRUE(e$ok))
    if (is.null(e$env$loo_tab)) div(class = "aside", "Leave-one-out needs at least three studies.")
    else div(class = "aside", "Hazard ratio after removing each study in turn. The dotted line is the pooled estimate with every study.")
  })

  # ---- "How to read this" readings (guides are in R/km_howto.R)
  km_rd <- function(fun) renderUI({
    f <- core(); req(isTRUE(f$ok))
    howto_text(tryCatch(fun(f), error = function(err) { message("km howto reading failed: ", conditionMessage(err)); character(0) }))
  })
  output$hw_km_pooled <- km_rd(function(f) reading_km_pooled(f$env, plot_p()))
  output$hw_km_study <- km_rd(function(f) reading_km_study(f$env, plot_p()))
  output$hw_km_check <- km_rd(function(f) reading_km_check(f$env))
  km_rd2 <- function(fun) renderUI({
    f <- core(); req(isTRUE(f$ok)); e <- extra(); req(isTRUE(e$ok))
    howto_text(tryCatch(fun(f, e), error = function(err) { message("km howto reading failed: ", conditionMessage(err)); character(0) }))
  })
  output$hw_km_summary <- km_rd2(function(f, e) reading_km_summary(f$env, e$env, plot_p()))
  output$hw_km_hr <- km_rd2(function(f, e) reading_km_hr(f$env, e$env, plot_p()))
  output$hw_km_nph <- km_rd2(function(f, e) reading_km_nph(e$env, plot_p()))
  output$hw_km_sens <- km_rd2(function(f, e) reading_km_sens(f$env, e$env, sens_env(), plot_p()))

  # ---- Written report
  report_parts <- reactive({
    f <- core(); req(isTRUE(f$ok)); e <- extra(); req(isTRUE(e$ok))
    p <- plot_p(); p$tv_df <- e$p$tv_df
    v <- sens_env()
    list(methods = km_methods_text(p, f$env, e$env, v), results = km_results_text(p, f$env, e$env, v))
  })
  output$km_report <- renderUI({
    r <- report_parts()
    block <- function(title, paras, refs = FALSE) div(class = "report-block",
      div(class = "report-head", h3(title), if (!refs) tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = copy_js, "Copy")),
      div(class = "report-text", lapply(paras, tags$p)))
    tagList(block("Methods", r$methods), block("Results", r$results),
            block("References", paste0(seq_along(km_ref_list), ". ", km_ref_list), refs = TRUE))
  })
  output$km_dl_report <- downloadHandler("km_methods_and_results.txt", function(file) {
    r <- report_parts(); writeLines(enc2utf8(km_report_plain(r$methods, r$results)), file, useBytes = TRUE)
  })

  # ---- Check tab
  output$km_check_notes <- renderUI({
    f <- core(); req(isTRUE(f$ok)); ck <- f$env$checks
    msgs <- character(0)
    for (i in seq_len(nrow(ck))) {
      er <- ck$events_reported[i]; ec <- ck$events_reconstructed[i]
      if (!is.na(er) && er > 0 && abs(ec - er) / er > 0.05)
        msgs <- c(msgs, sprintf("%s / %s: %d events reconstructed against %d reported. Check the digitized points and the numbers at risk.", ck$study_id[i], ck$arm[i], ec, er))
      if (!is.na(ck$risk_table_diff[i]) && ck$risk_table_diff[i] > 0.1 * ck$n[i])
        msgs <- c(msgs, sprintf("%s / %s: reconstructed numbers at risk differ from the table by up to %d patients.", ck$study_id[i], ck$arm[i], round(ck$risk_table_diff[i])))
    }
    fd <- f$env$fidelity
    for (i in seq_len(nrow(fd))) {
      if (fd$mean_abs_diff[i] > 0.02)
        msgs <- c(msgs, sprintf("%s / %s: the rebuilt curve differs from the digitized points by %.3f on average (%.3f at most). Check the points and the numbers at risk.", fd$study_id[i], fd$arm[i], fd$mean_abs_diff[i], fd$max_abs_diff[i]))
      md <- fd$median_digitized[i]; mr <- fd$median_reconstructed[i]
      if (is.finite(md) && is.finite(mr) && md > 0 && abs(mr - md) / md > 0.1)
        msgs <- c(msgs, sprintf("%s / %s: median survival is %.1f on the digitized curve and %.1f on the rebuilt one.", fd$study_id[i], fd$arm[i], md, mr))
      if (fd$points[i] < 30)
        msgs <- c(msgs, sprintf("%s / %s: only %d digitized points. With fewer than 30 the hazard ratio is less accurate.", fd$study_id[i], fd$arm[i], fd$points[i]))
    }
    if (length(msgs)) lapply(msgs, function(m) div(class = "notice warn", m))
    else div(class = "aside", "No reconstruction differs from the published numbers by more than 5% of events or 10% of the starting sample, and each rebuilt curve stays within 0.02 of its digitized points.")
  })
  output$km_check_tbl <- renderDT({
    f <- core(); req(isTRUE(f$ok)); ck <- f$env$checks
    out <- data.frame(Study = ck$study_id, Arm = ck$arm, Group = ck$group, Patients = ck$n,
      `Events reported` = ck$events_reported, `Events reconstructed` = ck$events_reconstructed,
      RMSE = round(ck$rmse, 4), `Max error` = round(ck$max_abs_error, 4), `K-S p` = round(ck$ks_p, 3),
      `Risk table max diff` = round(ck$risk_table_diff), check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE))
  })

  output$km_fidelity_tbl <- renderDT({
    f <- core(); req(isTRUE(f$ok)); fd <- f$env$fidelity
    out <- data.frame(Study = fd$study_id, Arm = fd$arm, `Digitized points` = fd$points,
      `Mean difference` = round(fd$mean_abs_diff, 4), `Largest difference` = round(fd$max_abs_diff, 4),
      `Median, digitized` = round(fd$median_digitized, 2), `Median, rebuilt` = round(fd$median_reconstructed, 2), check.names = FALSE)
    datatable(out, rownames = FALSE, options = list(dom = "t", pageLength = 50, scrollX = TRUE))
  })

  # ---- Data tab
  dt_of <- function(which) renderDT({
    d <- files[[which]]; req(d)
    datatable(d$data, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
  })
  output$km_data_curve <- dt_of("curve")
  output$km_data_risk <- dt_of("risk")
  output$km_data_arms <- dt_of("arms")

  # ---- Send hazard ratios to the forest plot
  observeEvent(input$km_send, {
    e <- extra(); req(isTRUE(e$ok))
    d <- e$env$pub_tab
    src <- pick(input$km_send_src, c("reconstructed", "published_first", "published_only"))
    has_pub <- !is.na(d$hr_published)
    out <- switch(src,
      reconstructed = data.frame(study_id = d$study_id, effect = d$hr_reconstructed, ci_lower = d$rec_lo, ci_upper = d$rec_hi),
      published_first = data.frame(study_id = d$study_id,
        effect = ifelse(has_pub, d$hr_published, d$hr_reconstructed),
        ci_lower = ifelse(has_pub, d$pub_lo, d$rec_lo), ci_upper = ifelse(has_pub, d$pub_hi, d$rec_hi)),
      published_only = data.frame(study_id = d$study_id[has_pub], effect = d$hr_published[has_pub],
                                  ci_lower = d$pub_lo[has_pub], ci_upper = d$pub_hi[has_pub]))
    out <- out[!is.na(out$effect) & !is.na(out$ci_lower) & !is.na(out$ci_upper), ]
    if (nrow(out) < 2) {
      showNotification("Fewer than two studies have a usable hazard ratio for this choice, so there is nothing to pool.", type = "warning", duration = 8)
      return()
    }
    out[2:4] <- lapply(out[2:4], function(x) format(round(x, 4), trim = TRUE))
    out$study_id <- as.character(out$study_id)
    forest_raw(list(data = out, sep = ",", enc = "UTF-8", name = "km_study_hazard_ratios.csv"))
    forest_note(switch(src,
      reconstructed = "Hazard ratios reconstructed from Kaplan-Meier curves (treatment vs control).",
      published_first = "Published hazard ratios where entered, reconstructed ones for the other studies (treatment vs control).",
      published_only = "Published hazard ratios from the arms file (treatment vs control)."))
    updateRadioButtons(session, "mode", selected = "forest")
    updateSelectInput(session, "type", selected = "gen")
    showNotification("Hazard ratios loaded in the forest plot. Choose the effect measure HR and review the labels.", type = "message", duration = 8)
  })

  # ---- Other downloads
  output$km_dl_ipd <- downloadHandler("reconstructed_ipd.csv", function(file) {
    f <- core(); req(isTRUE(f$ok))
    utils::write.csv(f$env$ipd[, c("study_id", "arm", "group", "time", "status")], file, row.names = FALSE)
  })
  output$km_dl_hr <- downloadHandler("study_hazard_ratios.csv", function(file) {
    e <- extra(); req(isTRUE(e$ok))
    utils::write.csv(e$env$pub_tab, file, row.names = FALSE)
  })
  km_script_to <- function(file) {
    f <- core(); req(isTRUE(f$ok)); e <- extra(); req(isTRUE(e$ok))
    p <- plot_p()
    code <- km_build_code(p, digit_B = max(0, min(200, as.integer(input$km_digit_B %||% 30))), digit_noise = max(0.001, min(0.05, as.numeric(input$km_digit_noise %||% 0.005))))
    fl <- lapply(c(curve = "curve", risk = "risk", arms = "arms"), function(w) {
      x <- files[[w]]
      nm <- if (grepl("\\.(csv|txt)$", x$name, ignore.case = TRUE)) x$name else paste0(w, ".csv")
      list(name = nm, sep = x$sep, enc = x$enc, orig = x$orig)
    })
    sa <- e$env$sens_all; lo <- e$env$loo_tab; pb <- sum(!is.na(e$env$pub_tab$hr_published))
    sizes <- list(sens = round(km_rows_height(nrow(sa) + 2, n_groups(sa$group)), 1),
                  loo = round(km_rows_height(if (is.null(lo)) 1 else nrow(lo)), 1),
                  pub = round(km_rows_height(max(2, 2 * pb), max(1, pb)), 1))
    writeLines(km_build_script(code, fl, plot_w, plot_h(), sizes), file, useBytes = TRUE)
  }
  output$km_dl_script <- downloadHandler("km_reconstruction.R", function(file) km_script_to(file))

  # ---- Everything in one zip: figures, tables, text, script and the data
  output$km_dl_all <- downloadHandler(function() paste0("survival_analysis_", format(Sys.Date(), "%Y%m%d"), ".zip"), function(file) {
    f <- core(); req(isTRUE(f$ok)); e <- extra(); req(isTRUE(e$ok)); x <- e$env; v <- sens_env()
    d <- tempfile("km_all_"); dir.create(file.path(d, "figures"), recursive = TRUE); dir.create(file.path(d, "tables")); dir.create(file.path(d, "data"))
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    withProgress(message = "Preparing the zip file", value = 0.1, {
      for (id in c("pooled", "study", "landmark", "tv", "loglog", "sens", "loo", "pub")) {
        s <- tryCatch(spec(id), error = function(err) NULL); if (is.null(s)) next
        for (ext in c("png", "pdf")) tryCatch({
          save_dev(file.path(d, "figures", paste0("km_", id, ".", ext)), ext, s$w, s$h)
          eval(parse(text = s$code), s$env); grDevices::dev.off()
        }, error = function(err) { while (grDevices::dev.cur() > 1) grDevices::dev.off() })
        incProgress(0.08)
      }
      wr <- function(obj, nm) if (is.data.frame(obj) && nrow(obj)) utils::write.csv(obj, file.path(d, "tables", nm), row.names = FALSE, na = "")
      wr(f$env$ipd[, c("study_id", "arm", "group", "time", "status")], "reconstructed_patients.csv")
      wr(x$arm_summary, "arms_summary.csv"); wr(f$env$rmst, "rmst.csv"); wr(f$env$medians, "median_survival.csv"); wr(x$rd_tab, "survival_difference_at_times.csv")
      wr(x$pub_tab, "study_hazard_ratios.csv"); wr(x$landmark_hr, "landmark_hazard_ratios.csv"); wr(x$piecewise, "hazard_ratio_by_interval.csv")
      wr(v$sens_all, "sensitivity_analyses.csv"); wr(x$loo_tab, "leave_one_out.csv"); wr(f$env$checks, "reconstruction_checks.csv"); wr(f$env$fidelity, "fidelity_to_digitized_points.csv")
      for (w in c("curve", "risk", "arms")) if (!is.null(files[[w]])) utils::write.csv(files[[w]]$data, file.path(d, "data", paste0(w, ".csv")), row.names = FALSE, na = "")
      r <- report_parts()
      writeLines(enc2utf8(km_report_plain(r$methods, r$results)), file.path(d, "methods_and_results.txt"), useBytes = TRUE)
      tryCatch(km_script_to(file.path(d, "km_reconstruction.R")), error = function(err) NULL)
      writeLines(c("Contents", "figures/  every plot of the analysis as PNG (300 dpi) and PDF", "tables/   the numbers behind the plots (CSV)",
                   "data/     the three tables the analysis was run from", "methods_and_results.txt  written Methods and Results", "km_reconstruction.R  script that repeats the analysis"),
                 file.path(d, "README.txt"))
      setProgress(0.95); zip_dir(d, file)
    })
  })
}

# Folded block for the options most people never change
adv_box <- function(title, ...) tags$details(class = "adv", tags$summary(title), div(class = "adv-body", ...))
