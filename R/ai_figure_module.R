# "Read this figure with AI" in step 1 of the digitizer. The browser sends the image only when the button is pressed
# (www/dig_ai.js); the reading is shown here, and "Use these axes" / "Use this arm" send what to apply back to the
# browser, where the tick positions are snapped onto the image and the existing colour trace runs.

ai_figure_ui <- function() {
  div(class = "ai-extract aifig",
    h4("Read this figure with AI"),
    uiOutput("aifig_reason"),
    div(class = "ai-run-row",
      tags$button(id = "aifig_read_btn", type = "button", class = "btn btn-primary btn-sm", disabled = NA, "Read this figure with AI"),
      textOutput("aifig_progress", inline = TRUE)),
    div(class = "aside", "The AI reads the axes, the legend, the numbers-at-risk table and a hazard ratio printed on the figure. It does not trace the curves: the digitizer does, as before."),
    uiOutput("aifig_panel"))
}

ai_figure_server <- function(input, output, session, ai) {
  reading <- reactiveVal(NULL)   # list(reading, warnings, factor, nonce)
  progress <- reactiveVal("")
  sent <- reactiveVal(list())    # the last message of each type sent to the browser (read by the tests)
  send <- function(type, msg) { s <- isolate(sent()); s[[type]] <- msg; sent(s); session$sendCustomMessage(type, msg) }
  run_id <- 0L; runner <- NULL
  get_runner <- function() {
    r <- getOption("ponderis.ai_runner")
    if (identical(r, "sync")) return(function(job, done) done(ai_figure_job(job, transport = getOption("ponderis.ai_transport", ai_http_transport))))
    if (is.function(r)) return(r)
    if (is.null(runner)) runner <<- ai_runner_mirai("ai_figure_job", c("helpers.R", "ai_client.R", "ai_figure.R"))
    runner
  }

  # the browser enables the button only when the AI is ready
  observe(session$sendCustomMessage("aifig_ready", list(ready = isTRUE(ai$ready()))))
  output$aifig_reason <- renderUI(if (!ai$ready()) div(class = "notice info", ai$reason()))
  output$aifig_progress <- renderText(progress())

  observeEvent(input$aifig_image, {
    ev <- input$aifig_image
    if (!isolate(ai$ready()) || is.null(ev$b64) || !nzchar(ev$b64)) return()
    job <- list(b64 = ev$b64, width = ev$width, height = ev$height, study = ev$study %||% "", key = ai$key(), model = isolate(ai$model()))
    factor <- as.numeric(ev$orig_width) / as.numeric(ev$width)
    run_id <<- run_id + 1L; my_run <- run_id
    reading(NULL); progress("Reading the figure…")
    get_runner()(job, function(r) {
      if (my_run != run_id) return(invisible())
      if (is.null(r)) r <- list(ok = FALSE, message = "The background worker stopped unexpectedly. Try again.", called = TRUE)
      if (!isFALSE(r$called)) ai$record_usage(r$usage, job$model)
      if (!isTRUE(r$ok)) { progress(""); reading(list(error = r$message)); return(invisible()) }
      progress("")
      reading(list(reading = ai_figure_scale(r$reading, factor), warnings = r$warnings))
    })
  })
  # another image loaded in the browser: the reading belongs to the old one
  observeEvent(input$aifig_new_image, { run_id <<- run_id + 1L; reading(NULL); progress("") })

  observeEvent(input$aifig_use_axes, {
    rd <- reading(); if (is.null(rd$reading)) return()
    ax <- ai_figure_axes(rd$reading)
    updateSelectInput(session, "dig_ctype", selected = ax$curve_type)
    send("aifig_axes", ax)
  })
  observeEvent(input$aifig_use_arm, {
    rd <- reading(); if (is.null(rd$reading)) return()
    i <- suppressWarnings(as.integer(input$aifig_use_arm$i))
    if (is.na(i) || i < 1 || i > length(rd$reading$arms)) return()
    send("aifig_arm", ai_figure_arm(rd$reading, i))
  })

  output$aifig_panel <- renderUI({
    rd <- reading(); if (is.null(rd)) return(NULL)
    if (!is.null(rd$error)) return(div(class = "notice error", rd$error))
    m <- rd$reading
    tick_txt <- function(a) { v <- vapply(a$ticks %||% list(), function(t) as.numeric(t$value), 0)
      if (length(v)) paste0(format(min(v), drop0trailing = TRUE), " to ", format(max(v), drop0trailing = TRUE)) else "no ticks" }
    rows <- m$risk_table$rows %||% list()
    risk_of <- function(a) { r <- Filter(function(x) tolower(x$label) %in% tolower(c(a$label, a$risk_label %||% "")), rows)
      if (length(r)) paste(vapply(r[[1]]$n, function(v) if (is.null(v)) "–" else as.character(v), ""), collapse = ", ") else "" }
    an <- m$annotations
    div(class = "aifig-panel",
      lapply(rd$warnings, function(w) div(class = "notice warn", w)),
      div(class = "aifig-axes",
        div(tags$strong("X: "), m$axes$x$label, sprintf(" (%s; ticks %s)", m$axes$x$scale, tick_txt(m$axes$x))),
        div(tags$strong("Y: "), m$axes$y$label, sprintf(" (%s%s; ticks %s)", m$axes$y$scale, if (isTRUE(m$axes$y_percent)) ", percent" else "", tick_txt(m$axes$y))),
        div(if (identical(m$axes$curve_type, "cumulative")) "Cumulative incidence (rising curves)" else "Survival (falling curves)"),
        actionButton("aifig_use_axes", "Use these axes", class = "btn-outline-primary btn-sm")),
      if (length(m$arms)) tags$table(class = "aifig-arms",
        tags$thead(tags$tr(tags$th(""), tags$th("Arm"), tags$th("Line"), tags$th(if (length(rows)) paste("At risk:", paste(vapply(m$risk_table$times, as.character, ""), collapse = ", ")) else "No risk table"), tags$th(""))),
        tags$tbody(lapply(seq_along(m$arms), function(i) { a <- m$arms[[i]]
          tags$tr(tags$td(span(class = "aifig-swatch", style = if (!is.null(a$colour_hex) && grepl("^#[0-9A-Fa-f]{6}$", a$colour_hex)) paste0("background:", a$colour_hex) else NULL)),
                  tags$td(a$label), tags$td(a$line_style), tags$td(risk_of(a)),
                  tags$td(tags$button(type = "button", class = "btn btn-primary btn-sm",
                                      onclick = sprintf("Shiny.setInputValue('aifig_use_arm', {i: %d, nonce: Math.random()}, {priority: 'event'})", i), "Use this arm"))) }))),
      if (!is.null(an$hr)) div(class = "aifig-hr", tags$strong("Hazard ratio on the figure: "), an$hr_text %||% "",
                               if (is.null(an$reference_arm)) span(class = "aside", " (reference arm not stated)")),
      if (length(m$notes)) div(class = "notice info", paste(unlist(m$notes), collapse = " ")),
      div(class = "aside", "After \"Use this arm\", check the traced points, erase strays, and press Save this curve as usual."))
  })

  list(reading = reading, sent = sent)
}
