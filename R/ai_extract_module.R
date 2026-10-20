# "Extract from PDFs with AI" in the Forest plot mode: the PDFs go to the AI one at a time in the
# background, the answers come back as a review table where every cell shows how well its quote
# matched the PDF, and "Use these rows" hands the table to the usual CSV flow.

AI_MAX_PDFS <- 20
AI_STATUS_LABEL <- c(verified = "Verified: the sentence and the number were found in the PDF.",
                     quote_only = "Check: the sentence was found in the PDF, but this number is not in it.",
                     not_found = "Check: the sentence was not found in the PDF.",
                     unverifiable = "Not checked: the PDF has no text layer (scanned).",
                     empty = "Not reported in the article.",
                     edited = "Edited by you.")
AI_STATUS_BG <- c(verified = "#E3F3EA", quote_only = "#FBF1CF", not_found = "#F8DADF",
                  unverifiable = "#EEF1F4", empty = "#FFFFFF", edited = "#DCE8F6")

ai_col_label <- function(col, type) {
  labs <- c(study_id = "Study", SPEC[[type]]$cols, year = "Year", subgroup = "Subgroup")
  if (col %in% names(labs)) labs[[col]] else col
}

ai_extract_ui <- function() {
  div(class = "ai-extract",
    h4("Or extract the data from PDFs with AI"),
    uiOutput("ai_extract_reason"),
    fileInput("ai_pdfs", NULL, multiple = TRUE, accept = c(".pdf", "application/pdf"),
              buttonLabel = "Choose PDFs", placeholder = "One PDF per study (up to 20)", width = "100%"),
    textInput("ai_outcome", "Outcome", placeholder = "e.g. all-cause mortality at 30 days", width = "100%"),
    textInput("ai_arms", "Treatment vs control (optional)", placeholder = "e.g. PCI vs CABG", width = "100%"),
    textAreaInput("ai_notes", "Instructions for the AI (optional)", rows = 2, width = "100%",
                  placeholder = "e.g. use intention-to-treat; ignore the per-protocol analysis"),
    div(class = "ai-run-row",
      actionButton("ai_run", "Extract with AI", class = "btn-primary btn-sm", disabled = TRUE),
      actionButton("ai_cancel", "Stop", class = "btn-outline-secondary btn-sm"),
      textOutput("ai_progress", inline = TRUE)),
    div(class = "aside", "The AI reads each article and fills the table with the page and sentence behind every number. You review it before it is used.")
  )
}

# Background runner: one ExtendedTask; its mirai worker loads the plain AI functions and runs ai_run_job().
ai_runner_mirai <- function() {
  dir <- normalizePath(".")
  waiting <- list()   # callbacks in the order the jobs were handed to the task
  task <- ExtendedTask$new(function(job) mirai::mirai({
    source(file.path(dir, "R", "helpers.R")); source(file.path(dir, "R", "ai_client.R")); source(file.path(dir, "R", "ai_extract.R"))
    ai_run_job(job)
  }, job = job, dir = dir))
  observeEvent(task$status(), {
    st <- task$status()
    if (!st %in% c("success", "error") || !length(waiting)) return()
    cb <- waiting[[1]]; waiting <<- waiting[-1]
    res <- tryCatch(task$result(), error = function(e) NULL)
    cb(res)
  })
  function(job, done) { waiting[[length(waiting) + 1]] <<- done; task$invoke(job) }
}

ai_runner_sync <- function(job, done) done(ai_run_job(job, transport = getOption("ponderis.ai_transport", ai_http_transport)))

ai_extract_server <- function(input, output, session, ai, raw, detect_msg) {
  results <- reactiveVal(list())   # one entry per PDF of the current run, in upload order
  # bumped when answers arrive or are cleared, not on a cell edit: the review and its table are drawn
  # again only then, and an edit replaces the table's data in place (scroll and selection stay)
  table_rev <- reactiveVal(0L)
  new_results <- function(rs) { results(rs); table_rev(isolate(table_rev()) + 1L) }
  jobs <- reactiveVal(list())
  run_type <- reactiveVal("dich")
  running <- reactiveVal(FALSE)
  cancel <- reactiveVal(FALSE)
  progress <- reactiveVal("")
  runner <- NULL
  run_id <- 0L   # a new run, or Discard, makes answers still on their way from an older run be ignored
  get_runner <- function() {
    r <- getOption("ponderis.ai_runner")
    if (identical(r, "sync")) return(ai_runner_sync)
    if (is.function(r)) return(r)
    if (is.null(runner)) runner <<- ai_runner_mirai()
    runner
  }

  set_result <- function(i, r) { rs <- results(); rs[[i]] <- r; new_results(rs) }

  run_queue <- function(queue) {
    if (!length(queue)) {
      running(FALSE)
      n_ok <- sum(vapply(results(), function(r) isTRUE(r$ok), logical(1)))
      progress(sprintf("Done: %d of %d PDFs read.", n_ok, length(results())))
      return(invisible())
    }
    i <- queue[1]; rest <- queue[-1]
    if (isTRUE(isolate(cancel())) || !isolate(ai$ready())) {
      why <- if (isTRUE(isolate(cancel()))) "stopped by you." else isolate(ai$reason())
      for (k in queue) set_result(k, list(ok = FALSE, name = jobs()[[k]]$name, error = "stopped", message = paste("Not run:", why),
                                          cells = ai_cells_long(list(), run_type()), ambiguities = character(0)))
      return(run_queue(integer(0)))
    }
    job <- jobs()[[i]]
    job$key <- ai$key(); job$model <- isolate(ai$model())
    progress(sprintf("Reading article %d of %d (%s)\u2026", i, length(jobs()), job$name))
    my_run <- run_id
    get_runner()(job, function(r) {
      if (my_run != run_id) return(invisible())
      if (is.null(r)) r <- list(ok = FALSE, error = "network", message = "The background worker stopped unexpectedly. Try again.")
      r$name <- job$name
      if (isTRUE(r$called) || is.null(r$called)) ai$record_usage(r$usage, job$model)
      set_result(i, r)
      run_queue(rest)
    })
  }

  observeEvent(input$ai_run, {
    if (!isolate(ai$ready()) || isTRUE(running())) return()
    f <- input$ai_pdfs
    if (is.null(f) || !nrow(f)) { showNotification("Choose one or more PDFs first.", type = "warning"); return() }
    if (nrow(f) > AI_MAX_PDFS) {
      showNotification(sprintf("Only the first %d PDFs are read in one run.", AI_MAX_PDFS), type = "warning")
      f <- f[seq_len(AI_MAX_PDFS), , drop = FALSE]
    }
    type <- pick(input$type, names(SPEC)); run_type(type)
    jobs(lapply(seq_len(nrow(f)), function(i) list(path = f$datapath[i], name = f$name[i], type = type,
      outcome = input$ai_outcome %||% "", arms = input$ai_arms %||% "", notes = input$ai_notes %||% "")))
    new_results(vector("list", nrow(f)))
    run_id <<- run_id + 1L
    cancel(FALSE); running(TRUE)
    run_queue(seq_len(nrow(f)))
  })
  observeEvent(input$ai_cancel, if (isTRUE(running())) cancel(TRUE))
  observeEvent(input$ai_retry, {
    i <- suppressWarnings(as.integer(input$ai_retry))
    if (is.na(i) || i < 1 || i > length(jobs()) || isTRUE(running()) || !isolate(ai$ready())) return()
    run_id <<- run_id + 1L
    cancel(FALSE); running(TRUE)
    run_queue(i)
  })
  observeEvent(input$ai_discard, {
    run_id <<- run_id + 1L   # the PDF being read, if any, finishes in the background and its answer is dropped
    new_results(list()); jobs(list()); progress(""); running(FALSE); cancel(FALSE)
  })

  # all cells of the PDFs that answered, with the PDF they came from and a row number across PDFs
  cells <- reactive({
    rs <- results()
    parts <- lapply(seq_along(rs), function(i) {
      r <- rs[[i]]
      if (!isTRUE(r$ok) || !nrow(r$cells)) return(NULL)
      cbind(pdf_i = i, pdf = r$name, r$cells, stringsAsFactors = FALSE)
    })
    parts <- Filter(Negate(is.null), parts)
    if (!length(parts)) return(NULL)
    d <- do.call(rbind, parts)
    d$gi <- match(paste(d$pdf_i, d$row), unique(paste(d$pdf_i, d$row)))
    d
  })
  table <- reactive({
    d <- cells(); cols <- template_cols(run_type())
    if (is.null(d)) return(as.data.frame(setNames(rep(list(character(0)), length(cols) + 1), c("pdf", cols)), stringsAsFactors = FALSE))
    w <- ai_cells_wide(transform(d, row = gi), run_type())
    cbind(pdf = vapply(seq_len(nrow(w)), function(g) d$pdf[d$gi == g][1], ""), w, stringsAsFactors = FALSE)
  })

  output$ai_extract_reason <- renderUI(if (!ai$ready()) div(class = "notice info", ai$reason()))
  observe(updateActionButton(session, "ai_run", disabled = !ai$ready()))
  output$ai_progress <- renderText(progress())

  output$ai_errors <- renderUI({
    rs <- results()
    items <- lapply(seq_along(rs), function(i) {
      r <- rs[[i]]
      if (is.null(r)) return(NULL)
      retry <- tags$button(type = "button", class = "btn btn-link btn-sm",
                           onclick = sprintf("Shiny.setInputValue('ai_retry', %d, {priority: 'event'})", i), "Retry")
      if (!isTRUE(r$ok)) return(div(class = "notice error", div(tags$strong(paste0(r$name, ":")), " ", r$message, retry)))
      if (!nrow(r$cells)) return(div(class = "notice warn", div(tags$strong(paste0(r$name, ":")), " No study rows found in this PDF.",
                                     if (length(r$ambiguities)) tags$span(" ", paste(r$ambiguities, collapse = " ")))))
      NULL
    })
    tagList(Filter(Negate(is.null), items))
  })
  output$ai_ambig <- renderUI({
    rs <- results()
    items <- lapply(rs, function(r) if (isTRUE(r$ok) && nrow(r$cells) && length(r$ambiguities))
      tags$li(tags$strong(paste0(r$name, ":")), " ", paste(r$ambiguities, collapse = " ")))
    items <- Filter(Negate(is.null), items)
    if (length(items)) div(class = "notice info", div("Choices the AI made (check them):", tags$ul(items)))
  })
  output$ai_warnings <- renderUI({
    w <- table(); if (!nrow(w)) return(NULL)
    d <- cells(); quotes <- ai_cells_wide(transform(d, row = gi, value = quote), run_type())
    msgs <- ai_check_rows(w[, -1, drop = FALSE], run_type(), quotes)
    ids <- w$study_id[!is.na(w$study_id)]
    for (s in unique(ids)) { k <- length(unique(w$pdf[w$study_id %in% s])); if (k > 1) msgs <- c(msgs, sprintf("Study '%s' appears in %d PDFs: keep only one source per study.", s, k)) }
    tagList(lapply(msgs, function(m) div(class = "notice warn", m)))
  })

  # the table as shown: the values, then one hidden status column per value (it sets the cell colour)
  shown <- reactive({
    w <- table(); d <- cells(); cols <- template_cols(run_type())
    st <- as.data.frame(lapply(cols, function(k) vapply(seq_len(nrow(w)), function(g) d$status[d$gi == g & d$col == k][1], "")),
                        col.names = paste0(cols, "__st"), stringsAsFactors = FALSE)
    cbind(w, st)
  })
  output$ai_table <- DT::renderDT({
    table_rev()
    w <- isolate(table()); req(nrow(w) > 0)
    cols <- template_cols(isolate(run_type()))
    show <- isolate(shown())
    labels <- c("PDF", "Study", unname(SPEC[[isolate(run_type())]]$cols), "Year", "Subgroup", paste0(cols, "__st"))
    dt <- DT::datatable(show, rownames = FALSE, colnames = labels, escape = TRUE,
                        editable = list(target = "cell", disable = list(columns = 0)),
                        selection = list(mode = "single", target = "cell"),
                        options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE,
                                       columnDefs = list(list(visible = FALSE, targets = seq(ncol(w), ncol(show) - 1)))))
    for (k in cols) dt <- DT::formatStyle(dt, k, valueColumns = paste0(k, "__st"),
                                          backgroundColor = DT::styleEqual(names(AI_STATUS_BG), unname(AI_STATUS_BG)))
    dt
  })

  table_proxy <- DT::dataTableProxy("ai_table", session)

  # the cell under the cursor: (global row, column name), or NULL
  cell_at <- function(r, j0) {
    w <- isolate(table()); nm <- names(w)[j0 + 1]
    if (is.na(r) || r < 1 || r > nrow(w) || is.na(nm) || nm == "pdf") return(NULL)
    list(g = r, col = nm)
  }
  observeEvent(input$ai_table_cell_edit, {
    e <- input$ai_table_cell_edit
    at <- cell_at(as.integer(e$row[1]), as.integer(e$col[1])); if (is.null(at)) return()
    d <- isolate(cells()); hit <- d[d$gi == at$g & d$col == at$col, , drop = FALSE][1, ]
    rs <- results(); cl <- rs[[hit$pdf_i]]$cells
    k <- which(cl$row == hit$row & cl$col == at$col)
    v <- trimws(as.character(e$value[1]))
    cl$value[k] <- if (nzchar(v)) v else NA_character_
    cl$status[k] <- "edited"
    rs[[hit$pdf_i]]$cells <- cl; results(rs)
    DT::replaceData(table_proxy, isolate(shown()), resetPaging = FALSE, clearSelection = "none", rownames = FALSE)
  })
  output$ai_evidence <- renderUI({
    s <- input$ai_table_cells_selected
    if (is.null(s) || !length(s)) return(div(class = "aside", "Click a cell to see the page and the sentence it came from."))
    at <- cell_at(as.integer(s[1, 1]), as.integer(s[1, 2])); if (is.null(at)) return(NULL)
    d <- cells(); hit <- d[d$gi == at$g & d$col == at$col, , drop = FALSE]
    if (!nrow(hit)) return(NULL)
    hit <- hit[1, ]
    div(class = "ai-evidence",
      div(tags$strong(hit$pdf), if (!is.na(hit$page)) paste0(", page ", hit$page), " \u2014 ", ai_col_label(at$col, run_type())),
      if (nzchar(hit$quote)) tags$blockquote(hit$quote),
      if (nzchar(hit$note)) div(hit$note),
      div(class = "aside", AI_STATUS_LABEL[[hit$status %||% "empty"]] %||% ""))
  })

  rows_for_forest <- function() { w <- table(); w[, -1, drop = FALSE] }
  observeEvent(input$ai_use, {
    d <- rows_for_forest()
    # an optional column empty in every row is left out: an empty subgroup column would make one subgroup "Not specified"
    for (k in c("year", "subgroup")) if (k %in% names(d) && all(is.na(d[[k]]) | !nzchar(trimws(d[[k]])))) d[[k]] <- NULL
    if (!nrow(d)) { showNotification("There are no rows to use yet.", type = "warning"); return() }
    updateSelectInput(session, "type", selected = run_type())
    raw(list(data = d, sep = ",", enc = "UTF-8", name = "ai_extracted.csv"))
    n_pdf <- length(unique(table()$pdf))
    detect_msg(sprintf("Rows extracted with AI from %d PDF%s: check them against the articles before publishing.", n_pdf, if (n_pdf == 1) "" else "s"))
  })
  output$ai_dl_csv <- downloadHandler("ai_extracted.csv", function(file) utils::write.csv(rows_for_forest(), file, row.names = FALSE, na = ""))

  output$ai_review <- renderUI({
    table_rev()
    if (!length(isolate(results()))) return(NULL)
    has_rows <- nrow(isolate(table())) > 0
    div(class = "ai-review",
      h3("Data extracted with AI: review before use"),
      div(class = "ai-legend", lapply(c("verified", "quote_only", "not_found", "unverifiable", "edited"), function(s)
        span(class = s, c(verified = "verified", quote_only = "number not in the sentence", not_found = "sentence not found",
                          unverifiable = "scanned PDF", edited = "edited by you")[[s]]))),
      uiOutput("ai_errors"), uiOutput("ai_ambig"), uiOutput("ai_warnings"),
      if (has_rows) tagList(DT::DTOutput("ai_table"), uiOutput("ai_evidence")),
      div(class = "ai-actions",
        if (has_rows) actionButton("ai_use", "Use these rows", class = "btn-primary btn-sm"),
        if (has_rows) downloadButton("ai_dl_csv", "Download as CSV", class = "btn-outline-primary btn-sm"),
        actionButton("ai_discard", "Discard", class = "btn-outline-secondary btn-sm")))
  })

  list(results = results, cells = cells, table = table,
       from_ai = reactive(identical(raw()$name, "ai_extracted.csv")))
}
