# "Assess with AI" in Quality & reporting -> Risk of bias. For each study in the list, the AI answers the signalling
# questions of RoB 2 or ROBINS-I V2 from the study's PDF; the tool's algorithms (R/rob_algorithms.R) give the domain
# judgements; the reviewer checks or changes answers, overrides a domain with a reason, and applies the result to the table.

AI_ROB_TOOLS <- c("rob2", "robins2")
AI_ROB_LABEL <- c(rob2 = "Suggested by the RoB 2 algorithm",
                  robins2 = "Proposed by the ROBINS-I V2 algorithm (draft, November 2025): confirm")
AI_ROB_STATUS <- c(verified = "quote found in the PDF", not_found = "quote not found in the PDF",
                   unverifiable = "scanned PDF, quote not checked", empty = "no quote")

ai_rob_ui <- function() {
  div(class = "ai-extract",
    h4("Assess with AI (signalling questions)"),
    uiOutput("airob_reason"),
    conditionalPanel("input.rob_tool == 'rob2' || input.rob_tool == 'robins2'",
      textInput("airob_outcome", "Outcome assessed", placeholder = "e.g. all-cause mortality at 30 days", width = "100%"),
      textInput("airob_result", "Result assessed (optional)", placeholder = "e.g. RR 0.80 (0.65 to 0.98), Table 2", width = "100%"),
      conditionalPanel("input.rob_tool == 'rob2'",
        radioButtons("airob_effect", "Effect of interest", width = "100%",
          c("Effect of assignment to intervention (intention-to-treat)" = "assignment",
            "Effect of adhering to intervention (per-protocol)" = "adherence")),
        conditionalPanel("input.airob_effect == 'adherence'",
          checkboxGroupInput("airob_devs", "Deviations to address", setNames(names(ROB2_DEVIATIONS), ROB2_DEVIATIONS), width = "100%"))),
      conditionalPanel("input.rob_tool == 'robins2'",
        radioButtons("airob_variant", "The analysis estimates", width = "100%",
          c("The intention-to-treat effect (Domain 1, Variant A)" = "A", "The per-protocol effect (Domain 1, Variant B)" = "B")),
        textAreaInput("airob_confounders", "Important confounding factors (from your protocol)", rows = 2, width = "100%", placeholder = "e.g. age; sex; disease severity"),
        textInput("airob_cointerventions", "Important co-interventions (optional)", width = "100%")),
      fileInput("airob_pdfs", "Add study PDFs", multiple = TRUE, accept = c(".pdf", "application/pdf"), buttonLabel = "Choose PDFs",
                placeholder = "PDFs from the forest-plot extraction are already here", width = "100%"),
      uiOutput("airob_links"),
      div(class = "ai-run-row",
        actionButton("airob_run", "Assess with AI", class = "btn-primary btn-sm"),
        actionButton("airob_cancel", "Stop", class = "btn-outline-secondary btn-sm"),
        textOutput("airob_progress", inline = TRUE)),
      div(class = "aside", "The AI answers each signalling question with the page and sentence it relied on; the tool's algorithm, not the AI, gives the judgements. You check every answer before applying them."))
  )
}

ai_rob_key <- function(tool, study) paste0(tool, "|", study)
ai_rob_slug <- function(x) gsub("[^A-Za-z0-9]", "_", x)

ai_rob_server <- function(input, output, session, ai, library, rob_names, rob_tool, apply_levels) {
  assessments <- reactiveVal(list())   # by "tool|study"
  running <- reactiveVal(FALSE); cancel <- reactiveVal(FALSE); progress <- reactiveVal("")
  apply_note <- reactiveVal(NULL)
  run_id <- 0L; queue_jobs <- list()
  runner <- NULL
  get_runner <- function() {
    r <- getOption("ponderis.ai_runner")
    if (identical(r, "sync")) return(function(job, done) done(ai_rob_one(job, transport = getOption("ponderis.ai_transport", ai_http_transport))))
    if (is.function(r)) return(r)
    if (is.null(runner)) runner <<- ai_runner_mirai("ai_rob_one", c("helpers.R", "review_helpers.R", "ai_client.R", "ai_extract.R",
                                                                     "rob_questions.R", "rob_algorithms.R", "ai_rob.R"))
    runner
  }
  tool_ok <- reactive(rob_tool() %in% AI_ROB_TOOLS)
  variant <- reactive(if (rob_tool() == "rob2") pick(input$airob_effect, c("assignment", "adherence")) else pick(input$airob_variant, c("A", "B")))
  settings <- reactive(list(outcome = input$airob_outcome %||% "", result = input$airob_result %||% "",
                            effect = variant(), deviations = if (rob_tool() == "rob2" && variant() == "adherence") input$airob_devs %||% character(0) else character(0),
                            confounders = input$airob_confounders %||% "", cointerventions = input$airob_cointerventions %||% ""))

  observeEvent(input$airob_pdfs, { f <- input$airob_pdfs; if (!is.null(f) && nrow(f)) library$add(f) })

  # which PDF each study uses: the reviewer's choice, else the PDF the extraction linked to that study
  pdf_for <- function(i, study) {
    v <- input[[paste0("airob_pdf_", i)]]
    if (!is.null(v) && nzchar(v)) return(v)
    hit <- Filter(function(e) identical(tolower(e$study), tolower(study)), library$entries())
    if (length(hit)) hit[[1]]$id else ""
  }
  output$airob_links <- renderUI({
    st <- rob_names(); e <- library$entries()
    if (!length(st)) return(div(class = "aside", "Add the studies above, then choose each one's PDF here."))
    if (!length(e)) return(div(class = "aside", "No PDFs yet: add them here or in the forest-plot extraction."))
    ch <- c("No PDF" = "", setNames(vapply(e, `[[`, "", "id"), vapply(e, `[[`, "", "name")))
    tagList(lapply(seq_along(st), function(i) selectInput(paste0("airob_pdf_", i), paste("PDF for", st[i]), ch,
                                                           selected = isolate(pdf_for(i, st[i])), width = "100%", selectize = FALSE)))
  })

  set_as <- function(k, x) { a <- assessments(); a[[k]] <- x; assessments(a) }
  run_queue <- function(queue) {
    if (!length(queue)) { running(FALSE); progress(sprintf("Done: %d stud%s assessed.", sum(vapply(assessments(), function(a) isTRUE(a$ok), TRUE)), if (length(assessments()) == 1) "y" else "ies")); return(invisible()) }
    k <- queue[1]; rest <- queue[-1]
    job <- queue_jobs[[k]]
    if (isTRUE(isolate(cancel())) || !isolate(ai$ready())) {
      why <- if (isTRUE(isolate(cancel()))) "stopped by you." else isolate(ai$reason())
      for (kk in queue) set_as(kk, list(ok = FALSE, study = queue_jobs[[kk]]$study, tool = queue_jobs[[kk]]$tool, error = "stopped", message = paste("Not run:", why)))
      return(run_queue(character(0)))
    }
    job$key <- ai$key(); job$model <- isolate(ai$model())
    progress(sprintf("Assessing %s\u2026", job$study))
    my_run <- run_id
    get_runner()(job, function(r) {
      if (my_run != run_id) return(invisible())
      if (is.null(r)) r <- list(ok = FALSE, error = "network", message = "The background worker stopped unexpectedly. Try again.", called = TRUE)
      r$study <- job$study; r$tool <- job$tool; r$variant <- job$variant; r$settings <- job$settings; r$pdf_name <- job$pdf_name
      if (!isFALSE(r$called)) ai$record_usage(r$usage, job$model)
      r$overrides <- list()
      set_as(k, r)
      run_queue(rest)
    })
  }
  start <- function(keys_only = NULL) {
    if (!isolate(ai$ready()) || !isolate(tool_ok()) || isTRUE(running())) return()
    st <- rob_names(); tool <- rob_tool()
    jobs <- list(); skipped <- character(0)
    for (i in seq_along(st)) {
      k <- ai_rob_key(tool, st[i])
      if (!is.null(keys_only) && !k %in% keys_only) next
      e <- library$get(pdf_for(i, st[i]))
      if (is.null(e)) { skipped <- c(skipped, st[i]); next }
      jobs[[k]] <- list(path = e$path, pdf_name = e$name, pages = e$pages, study = st[i], tool = tool, variant = variant(), settings = settings())
    }
    if (length(skipped)) showNotification(paste("No PDF linked, not assessed:", paste(skipped, collapse = ", ")), type = "warning", duration = 8)
    if (!length(jobs)) return()
    queue_jobs <<- jobs; run_id <<- run_id + 1L
    cancel(FALSE); running(TRUE)
    run_queue(names(jobs))
  }
  observeEvent(input$airob_run, start())
  observeEvent(input$airob_retry, start(keys_only = as.character(input$airob_retry)))
  observeEvent(input$airob_cancel, if (isTRUE(running())) cancel(TRUE))
  observeEvent(input$airob_discard, { run_id <<- run_id + 1L; assessments(list()); running(FALSE); progress(""); apply_note(NULL) })

  # ---- answers, judgements, overrides ----
  judgement <- function(k) {
    a <- isolate(assessments())[[k]]
    if (!isTRUE(a$ok)) return(NULL)
    rob_judge(a$tool, a$variant, setNames(a$answers$answer, a$answers$id), a$settings$deviations %||% character(0))
  }
  final <- function(k) {
    j <- judgement(k); if (is.null(j)) return(NULL)
    ov <- isolate(assessments())[[k]]$overrides
    d <- j$domains; for (dn in names(ov)) d[[dn]] <- ov[[dn]]$level
    c(d, Overall = if (isTRUE(j$critical_prelim)) "Critical" else if (isolate(assessments())[[k]]$tool == "rob2") rob2_overall(d) else robins2_overall(d))
  }
  observeEvent(input$airob_answer, {
    ev <- input$airob_answer; k <- as.character(ev$study); a <- assessments()[[k]]
    if (!isTRUE(a$ok)) return()
    i <- match(as.character(ev$id), a$answers$id); if (is.na(i)) return()
    q <- rob_bank(a$tool, a$variant)[[i]]
    v <- as.character(ev$answer); if (!v %in% union(q$answers, "NA")) return()
    a$answers$answer[i] <- v; a$answers$edited[i] <- TRUE
    set_as(k, a)
  })
  observeEvent(input$airob_override, {
    ev <- input$airob_override; k <- as.character(ev$study); a <- assessments()[[k]]
    if (!isTRUE(a$ok)) return()
    dn <- as.character(ev$domain); lv <- as.character(ev$level %||% ""); reason <- trimws(as.character(ev$reason %||% ""))
    if (!nzchar(lv)) { a$overrides[[dn]] <- NULL; set_as(k, a); return() }
    if (!lv %in% ROB_TOOLS[[a$tool]]$levels) return()
    if (!nzchar(reason)) { showNotification("Give the reason for overriding the algorithm's judgement.", type = "warning"); return() }
    a$overrides[[dn]] <- list(level = lv, reason = reason)
    set_as(k, a)
  })

  # ---- apply to the risk-of-bias table ----
  observeEvent(input$airob_apply, {
    tool <- rob_tool(); st <- rob_names(); as <- assessments()
    mine <- Filter(function(a) isTRUE(a$ok) && identical(a$tool, tool), as)
    unmatched <- character(0); applied <- character(0)
    for (k in names(mine)) {
      a <- mine[[k]]; i <- match(a$study, st)
      if (is.na(i)) { unmatched <- c(unmatched, a$study); next }
      f <- final(k); apply_levels(tool, i, st[i], unname(f[ROB_TOOLS[[tool]]$short])); applied <- c(applied, a$study)
    }
    apply_note(list(applied = applied, unmatched = unmatched))
  })
  output$airob_apply_note <- renderUI({
    n <- apply_note(); if (is.null(n)) return(NULL)
    tagList(if (length(n$applied)) div(class = "notice info", sprintf("Applied to the table: %s.", paste(n$applied, collapse = ", "))),
            if (length(n$unmatched)) div(class = "notice warn", sprintf("Assessed but not in the study list (names changed?), not applied: %s.", paste(n$unmatched, collapse = ", "))))
  })

  # ---- rendering ----
  output$airob_reason <- renderUI({
    if (!tool_ok()) return(div(class = "notice info", "The AI assessment covers RoB 2 and ROBINS-I V2. Choose one of them as the tool above."))
    if (!ai$ready()) div(class = "notice info", ai$reason())
  })
  output$airob_progress <- renderText(progress())
  output$airob_errors <- renderUI({
    as <- Filter(function(a) identical(a$tool, rob_tool()) && !isTRUE(a$ok), assessments())
    tagList(lapply(names(as), function(k) div(class = "notice error", div(tags$strong(paste0(as[[k]]$study, ":")), " ", as[[k]]$message,
      tags$button(type = "button", class = "btn btn-link btn-sm", onclick = sprintf("Shiny.setInputValue('airob_retry', %s, {priority: 'event'})", jsonlite::toJSON(k, auto_unbox = TRUE)), "Retry")))))
  })

  answer_select <- function(k, q, current) {
    opts <- union(q$answers, if (!is.null(q$when)) "NA")
    tags$select(class = "form-select form-select-sm airob-answer",
      onchange = sprintf("Shiny.setInputValue('airob_answer', {study: %s, id: %s, answer: this.value}, {priority: 'event'})",
                         jsonlite::toJSON(k, auto_unbox = TRUE), jsonlite::toJSON(q$id, auto_unbox = TRUE)),
      lapply(opts, function(o) tags$option(value = o, selected = if (identical(o, current)) NA else NULL, paste0(o, " \u2013 ", ROB_ANS_LABEL[[o]]))))
  }
  override_form <- function(k, a, dn) {
    id <- paste0("airob_ov_", ai_rob_slug(k), "_", dn)
    js <- sprintf("Shiny.setInputValue('airob_override', {study: %s, domain: '%s', level: document.getElementById('%s_l').value, reason: document.getElementById('%s_r').value}, {priority: 'event'})",
                  jsonlite::toJSON(k, auto_unbox = TRUE), dn, id, id)
    ov <- a$overrides[[dn]]
    tags$details(class = "airob-override", tags$summary("Override"),
      tags$select(id = paste0(id, "_l"), class = "form-select form-select-sm", tags$option(value = "", "Keep the algorithm's judgement"),
                  lapply(ROB_TOOLS[[a$tool]]$levels, function(l) tags$option(value = l, selected = if (identical(ov$level, l)) NA else NULL, l))),
      tags$input(id = paste0(id, "_r"), type = "text", class = "form-control form-control-sm", placeholder = "Reason (required)", value = ov$reason %||% ""),
      tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = js, "Save"))
  }
  card <- function(k) {
    a <- assessments()[[k]]; j <- rob_judge(a$tool, a$variant, setNames(a$answers$answer, a$answers$id), a$settings$deviations %||% character(0))
    fin <- final(k); bank <- rob_bank(a$tool, a$variant); tl <- ROB_TOOLS[[a$tool]]
    vw <- ai_rob_variant_warning(a$tool, a$variant, j$answers)
    doms <- sort(unique(vapply(bank, function(q) q$domain, 0L)))
    tags$details(class = "airob-card", open = NA,
      tags$summary(tags$strong(a$study), span(class = "airob-pdf", a$pdf_name), span(class = paste("airob-level", ai_rob_slug(fin[["Overall"]])), paste("Overall:", fin[["Overall"]]))),
      if (nzchar(vw)) div(class = "notice warn", vw),
      if (isTRUE(j$critical_prelim)) div(class = "notice warn", "B2 or B3 is Yes or Probably yes: the result is at Critical risk of bias and needs no further assessment."),
      if (length(a$notes)) div(class = "notice info", paste(a$notes, collapse = " ")),
      lapply(doms, function(dd) {
        qs <- Filter(function(q) q$domain == dd, bank)
        dn <- if (dd == 0) NULL else paste0("D", dd)
        div(class = "airob-domain",
          h5(if (dd == 0) "Preliminary questions" else paste0(dn, ". ", tl$domains[dd]),
             if (!is.null(dn)) span(class = paste("airob-level", ai_rob_slug(fin[[dn]])), fin[[dn]]),
             if (!is.null(dn)) span(class = "aside", if (!is.null(a$overrides[[dn]])) paste("Overridden:", a$overrides[[dn]]$reason) else AI_ROB_LABEL[[a$tool]])),
          tags$table(class = "airob-q", tags$tbody(lapply(qs, function(q) {
            r <- a$answers[a$answers$id == q$id, ]; eff <- j$answers[[q$id]]
            tags$tr(class = if (eff == "NA") "airob-na" else NULL,
              tags$td(class = "airob-id", q$id),
              tags$td(if (nzchar(q$cond)) span(class = "airob-cond", q$cond, ": "), q$text,
                      if (nzchar(r$quote)) tags$blockquote(r$quote, if (!is.na(r$page)) span(class = "aside", paste0(" (page ", r$page, ")"))),
                      if (nzchar(r$rationale)) div(class = "aside", r$rationale)),
              tags$td(answer_select(k, q, eff), div(class = paste("ai-cell", if (isTRUE(r$edited)) "edited" else c(verified = "verified", not_found = "not_found", unverifiable = "unverifiable", empty = "empty")[[r$status]]),
                      if (isTRUE(r$edited)) "edited by you" else AI_ROB_STATUS[[r$status]])))
          }))),
          if (!is.null(dn)) override_form(k, a, dn))
      }))
  }
  output$airob_cards <- renderUI({
    as <- Filter(function(a) identical(a$tool, rob_tool()) && isTRUE(a$ok), assessments())
    tagList(lapply(names(as), card))
  })
  output$airob_review <- renderUI({
    if (!tool_ok() || !length(Filter(function(a) identical(a$tool, rob_tool()), assessments()))) return(NULL)
    div(class = "ai-review",
      h3("AI risk-of-bias assessments: check each answer"),
      div(class = "aside", ROB_CITATION[[if (rob_tool() == "rob2") "rob2" else "robins"]]),
      uiOutput("airob_errors"), uiOutput("airob_cards"), uiOutput("airob_apply_note"),
      div(class = "ai-actions",
        actionButton("airob_apply", "Apply to the risk-of-bias table", class = "btn-primary btn-sm"),
        downloadButton("dl_airob_csv", "Answers (CSV)", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_airob_judgements", "Judgements (CSV)", class = "btn-outline-primary btn-sm"),
        downloadButton("dl_airob_doc", "Word (.doc)", class = "btn-outline-primary btn-sm"),
        actionButton("airob_discard", "Discard", class = "btn-outline-secondary btn-sm")),
      h4("Methods"), uiOutput("airob_methods"))
  })

  # ---- exports ----
  mine_ok <- function() Filter(function(a) identical(a$tool, rob_tool()) && isTRUE(a$ok), assessments())
  answers_table <- function() {
    as <- mine_ok(); if (!length(as)) return(data.frame())
    do.call(rbind, lapply(names(as), function(k) {
      a <- as[[k]]; j <- judgement(k); bank <- rob_bank(a$tool, a$variant)
      data.frame(study = a$study, tool = a$tool, variant = a$variant, domain = a$answers$domain, question_id = a$answers$id,
                 question = vapply(bank, `[[`, "", "text"), answer = unname(j$answers[a$answers$id]), ai_answer = a$answers$ai_answer,
                 edited = a$answers$edited, page = a$answers$page, quote = a$answers$quote, rationale = a$answers$rationale,
                 status = a$answers$status, stringsAsFactors = FALSE, row.names = NULL)
    }))
  }
  support_text <- function(a, j, dd) {
    r <- a$answers[a$answers$domain == dd & j$answers[a$answers$id] != "NA", , drop = FALSE]
    paste(sprintf("%s %s%s%s", r$id, j$answers[r$id], ifelse(nzchar(r$rationale), paste0(": ", r$rationale), ""),
                  ifelse(nzchar(r$quote), paste0(" (\u201c", r$quote, "\u201d", ifelse(is.na(r$page), "", paste0(", p. ", r$page)), ")"), "")), collapse = " ")
  }
  judgements_table <- function() {
    as <- mine_ok(); if (!length(as)) return(data.frame())
    do.call(rbind, lapply(names(as), function(k) {
      a <- as[[k]]; j <- judgement(k); f <- final(k); tl <- ROB_TOOLS[[a$tool]]
      rows <- lapply(seq_along(tl$short), function(dd) { dn <- tl$short[dd]; ov <- a$overrides[[dn]]
        data.frame(study = a$study, domain = dn, domain_name = tl$domains[dd], judgement = f[[dn]], algorithm = j$domains[[dn]],
                   source = if (is.null(ov)) "algorithm" else "override",
                   support = paste0(support_text(a, j, dd), if (!is.null(ov)) paste0(" Override: ", ov$reason) else ""), stringsAsFactors = FALSE) })
      rbind(do.call(rbind, rows), data.frame(study = a$study, domain = "Overall", domain_name = "Overall", judgement = f[["Overall"]],
                                             algorithm = j$overall, source = "rule", support = "", stringsAsFactors = FALSE))
    }))
  }
  output$dl_airob_csv <- downloadHandler("risk_of_bias_answers.csv", function(file) utils::write.csv(answers_table(), file, row.names = FALSE, na = ""))
  output$dl_airob_judgements <- downloadHandler("risk_of_bias_judgements.csv", function(file) utils::write.csv(judgements_table(), file, row.names = FALSE, na = ""))
  output$dl_airob_doc <- downloadHandler("risk_of_bias_assessments.doc", function(file) {
    jt <- judgements_table(); esc <- function(x) htmltools::htmlEscape(as.character(x))
    body <- paste(vapply(unique(jt$study), function(s) {
      r <- jt[jt$study == s, ]
      paste0("<h2>", esc(s), "</h2><table border='1' cellpadding='4' style='border-collapse:collapse'><tr><th>Domain</th><th>Judgement</th><th>Support for judgement</th></tr>",
             paste0("<tr><td>", esc(r$domain_name), "</td><td>", esc(r$judgement), "</td><td>", esc(r$support), "</td></tr>", collapse = ""), "</table>")
    }, ""), collapse = "")
    writeLines(grade_word_doc(paste0(body, "<p>", esc(ROB_CITATION[[if (rob_tool() == "rob2") "rob2" else "robins"]]), "</p>"), "Risk of bias assessments"), file, useBytes = TRUE)
  })
  output$airob_methods <- renderUI({
    tool_txt <- if (rob_tool() == "rob2") "RoB 2" else "ROBINS-I V2"
    out <- trimws(input$airob_outcome %||% "")
    p(enc2utf8(paste0("Risk of bias was assessed with ", tool_txt, if (nzchar(out)) paste0(" for the outcome ", out), ". ",
      "A large language model (Claude, Anthropic) proposed answers to the signalling questions with supporting quotes; ",
      "the authors checked every answer, and domain judgements were derived with the tool's algorithms.")))
  })

  list(assessments = assessments, judgement = judgement, final = final)
}
