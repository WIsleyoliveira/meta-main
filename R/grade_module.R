# GRADE tab: question, outcomes with the five domains, and the Summary of findings table, evidence profile and text for the paper.
# The pure functions (certainty, absolute effects, tables) live in grade_helpers.R.

GRADE_DOMAIN_CHOICES <- list(
  rob = c("Not serious" = "0", "Serious (-1)" = "1", "Very serious (-2)" = "2", "Extremely serious (-3, ROBINS-I critical)" = "3"),
  other = c("Not serious" = "0", "Serious (-1)" = "1", "Very serious (-2)" = "2"),
  pub = c("Undetected" = "0", "Suspected (-1)" = "1", "Strongly suspected (-2)" = "2"))
GRADE_DOMAIN_LABELS <- c(rob = "Risk of bias", inc = "Inconsistency", ind = "Indirectness", imp = "Imprecision", pub = "Publication bias")
GRADE_DOMAIN_HINTS <- c(
  rob = "Why? For example: most of the weight comes from trials with unclear allocation concealment.",
  inc = "Why? For example: substantial heterogeneity (I² = 62%) that is not explained.",
  ind = "Why? For example: the population differs from the review question, or a surrogate outcome was used.",
  imp = "Why? For example: the 95% CI includes appreciable benefit and harm.",
  pub = "Why? For example: funnel plot asymmetry, or many small studies from the industry.")

grade_panel_ui <- function() {
  nav_panel("GRADE", value = "grade",
    div(class = "workspace",
      div(class = "controls",
        step("Review question", help = "These four lines become the title and the header of the Summary of findings table, as in Cochrane reviews. For example, population: adults with chronic coronary occlusion; intervention: PCI; comparison: CABG.",
          textInput("gr_population", "Patient or population", "", width = "100%", placeholder = "Adults with a chronic total occlusion"),
          textInput("gr_setting", "Setting", "", width = "100%", placeholder = "Hospitals in several countries"),
          textInput("gr_intervention", "Intervention", "", width = "100%", placeholder = "PCI"),
          textInput("gr_comparison", "Comparison", "", width = "100%", placeholder = "CABG")),
        step("Outcomes", help = "One block per outcome. The Cochrane Handbook asks for at most seven, the ones that matter most to patients and decision makers (for example mortality, major morbidity, adverse effects, quality of life). Use \"Add my forest-plot analysis\" to fill a block from the analysis you have open; its suggestions are only a start.",
          div(class = "btn-row",
            actionButton("gr_add", "Add an outcome", class = "btn-outline-primary btn-sm"),
            actionButton("gr_del", "Remove the last", class = "btn-outline-primary btn-sm")),
          actionLink("gr_from_fit", "Add my forest-plot analysis as an outcome"),
          uiOutput("gr_suggest_note"),
          div(class = "aside", sprintf("At most %d outcomes, in order of importance. Include the important ones even when no study reported them.", GRADE_MAX_OUTCOMES)))),
      div(class = "results",
        uiOutput("gr_blocks"),
        uiOutput("gr_issues"),
        navset_underline(id = "gr_views",
          nav_panel("Summary of findings", value = "sof",
            div(class = "sof-wrap", uiOutput("gr_sof")),
            div(class = "downloads", span("Copy or download"),
              tags$button(type = "button", class = "btn btn-primary btn-sm", onclick = grade_copy_js("gr_sof"), "Copy for Word"),
              downloadButton("dl_gr_sof_doc", "Word (.doc)", class = "btn-outline-primary btn-sm"),
              downloadButton("dl_gr_csv", "Table CSV", class = "btn-outline-primary btn-sm")),
            div(class = "aside", "Layout and wording follow Chapter 14 of the Cochrane Handbook. \"Copy for Word\" keeps the formatting when you paste into Word.")),
          nav_panel("Evidence profile", value = "ep",
            div(class = "sof-wrap", uiOutput("gr_profile")),
            div(class = "downloads", span("Copy or download"),
              tags$button(type = "button", class = "btn btn-primary btn-sm", onclick = grade_copy_js("gr_profile"), "Copy for Word"),
              downloadButton("dl_gr_ep_doc", "Word (.doc)", class = "btn-outline-primary btn-sm")),
            div(class = "aside", "The same judgements, one column per domain. It is usually placed in the supplement.")),
          nav_panel("Text for your paper", value = "txt",
            h3("Methods"),
            div(class = "report-block", div(class = "report-head", tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = copy_js, "Copy")),
                div(class = "report-text", uiOutput("gr_methods"))),
            h3("Results"),
            div(class = "report-block", div(class = "report-head", tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = copy_js, "Copy")),
                div(class = "report-text", uiOutput("gr_results"))),
            h3("References to cite"),
            tags$ul(lapply(grade_refs, tags$li)))
        ))))
}

grade_copy_js <- function(id) sprintf(paste0("var t=document.querySelector('#%s table'); if(!t) return; var r=document.createRange(); r.selectNode(t); var s=window.getSelection(); s.removeAllRanges(); s.addRange(r);",
  "var ok=false; try{ ok=document.execCommand('copy'); }catch(e){} s.removeAllRanges(); var b=this; b.textContent= ok ? 'Copied' : 'Press Ctrl+C'; setTimeout(function(){b.textContent='Copy for Word'},1800);"), id)

grade_server <- function(input, output, session, fit, rob_rows, rob_tool) {
  n_out <- reactiveVal(1L)
  gr_seed <- reactiveVal(list())     # values to put in blocks that are about to be (re)drawn
  gr_ver <- reactiveVal(0L)
  observeEvent(input$gr_add, { if (n_out() < GRADE_MAX_OUTCOMES) n_out(n_out() + 1L) else showNotification(sprintf("A Summary of findings table holds at most %d outcomes.", GRADE_MAX_OUTCOMES), type = "warning") })
  observeEvent(input$gr_del, n_out(max(n_out() - 1L, 1L)))

  # ---------------- outcome blocks ----------------
  output$gr_blocks <- renderUI({
    n <- n_out(); gr_ver()
    seed <- isolate(gr_seed())
    blocks <- lapply(seq_len(n), function(i) {
      g <- function(f, default = "") {
        s <- seed[[as.character(i)]][[f]]
        if (!is.null(s)) s else { v <- isolate(input[[paste0("gr_", f, "_", i)]]); if (is.null(v)) default else v }
      }
      id <- function(f) paste0("gr_", f, "_", i)
      dom <- function(d) div(class = "gr-dom",
        selectInput(id(d), GRADE_DOMAIN_LABELS[[d]], GRADE_DOMAIN_CHOICES[[if (d == "rob") "rob" else if (d == "pub") "pub" else "other"]], selected = g(d, "0"), width = "100%"),
        textInput(id(paste0(d, "_t")), NULL, g(paste0(d, "_t")), width = "100%", placeholder = GRADE_DOMAIN_HINTS[[d]]))
      up <- function(d, label, choices) div(class = "gr-dom",
        selectInput(id(d), label, choices, selected = g(d, "0"), width = "100%"),
        textInput(id(paste0(d, "_t")), NULL, g(paste0(d, "_t")), width = "100%", placeholder = "Why?"))
      tags$details(class = "gr-block", open = NA,
        tags$summary(paste("Outcome", i)),
        div(class = "gr-body",
          fluidRow(column(5, textInput(id("name"), "Outcome", g("name"), width = "100%", placeholder = "All-cause mortality")),
                   column(4, textInput(id("time"), "Time frame and scale (optional)", g("time"), width = "100%", placeholder = "30 days")),
                   column(3, selectInput(id("importance"), "Importance", c("Critical", "Important"), selected = g("importance", "Critical"), width = "100%"))),
          selectInput(id("design"), "Study design (decides where the certainty starts)",
            c("Randomized trials (start high)" = GRADE_DESIGN_RCT,
              "Observational studies (start low)" = GRADE_DESIGN_NRS,
              "Observational studies assessed with ROBINS-I (start high)" = GRADE_DESIGN_ROBINS),
            selected = g("design", GRADE_DESIGN_RCT), width = "100%"),
          div(class = "gr-grid",
            selectInput(id("measure"), "Effect measure", GRADE_MEASURES, selected = g("measure", "RR"), width = "100%"),
            numericInput(id("est"), "Estimate", suppressWarnings(as.numeric(g("est", NA))), width = "100%"),
            numericInput(id("lo"), "95% CI lower", suppressWarnings(as.numeric(g("lo", NA))), width = "100%"),
            numericInput(id("hi"), "95% CI upper", suppressWarnings(as.numeric(g("hi", NA))), width = "100%"),
            numericInput(id("k"), "Studies", suppressWarnings(as.numeric(g("k", NA))), min = 0, width = "100%"),
            numericInput(id("n"), "Participants", suppressWarnings(as.numeric(g("n", NA))), min = 0, width = "100%")),
          conditionalPanel(sprintf("['RR','OR','HR'].indexOf(input.gr_measure_%d) >= 0", i),
            div(class = "gr-grid",
              numericInput(id("ctrl"), "Assumed risk with the comparator, per 1,000", suppressWarnings(as.numeric(g("ctrl", NA))), min = 0, max = 1000, width = "100%"),
              textInput(id("src"), "Where that risk comes from (footnote)", g("src"), width = "100%", placeholder = "Median control group risk across the studies"),
              numericInput(id("rrr"), "Smallest relative reduction worth detecting, % (for the optimal information size)", suppressWarnings(as.numeric(g("rrr", 25))), min = 1, max = 99, width = "100%")),
            uiOutput(id("ois"))),
          conditionalPanel(sprintf("['MD','SMD'].indexOf(input.gr_measure_%d) >= 0", i),
            div(class = "gr-grid",
              numericInput(id("cmean"), "Mean in the control group (optional)", suppressWarnings(as.numeric(g("cmean", NA))), width = "100%"),
              textInput(id("unit"), "Unit (optional)", g("unit"), width = "100%", placeholder = "points"))),
          div(class = "gr-label", "Rate down (explain each one: it becomes a footnote)"),
          dom("rob"), dom("inc"), dom("ind"), dom("imp"), dom("pub"),
          conditionalPanel(sprintf("input.gr_design_%d == '%s'", i, GRADE_DESIGN_NRS),
            div(class = "gr-label", "Rate up (only when nothing is rated down)"),
            up("large", "Large effect", c("None" = "0", "Large (+1)" = "1", "Very large (+2)" = "2")),
            up("dose", "Dose-response gradient", c("None" = "0", "Present (+1)" = "1")),
            up("conf", "Confounding would reduce the effect", c("No" = "0", "Yes (+1)" = "1"))),
          textInput(id("comment"), "Comment for the table (optional)", g("comment"), width = "100%", placeholder = "Probably reduces mortality")))
    })
    gr_seed(list())
    tagList(blocks)
  })

  # ---------------- the outcomes, read from the blocks ----------------
  outcomes <- reactive({
    lapply(seq_len(n_out()), function(i) {
      v <- function(f) input[[paste0("gr_", f, "_", i)]]
      o <- list(name = .gt(v("name")), time = .gt(v("time")), importance = v("importance") %||% "Critical", design = v("design") %||% GRADE_DESIGN_RCT,
                measure = v("measure") %||% "RR", k = v("k"), n = v("n"), est = v("est"), lo = v("lo"), hi = v("hi"), control = v("ctrl"),
                source = .gt(v("src")), cmean = v("cmean"), unit = .gt(v("unit")), comment = .gt(v("comment")), rrr = v("rrr"))
      for (d in names(GRADE_DOMAINS)) { o[[d]] <- .gn(v(d)); o[[paste0(d, "_t")]] <- .gt(v(paste0(d, "_t"))) }
      for (d in names(GRADE_UPS)) { o[[d]] <- .gn(v(d)); o[[paste0(d, "_t")]] <- .gt(v(paste0(d, "_t"))) }
      o
    })
  })
  question <- reactive(list(population = input$gr_population, setting = input$gr_setting, intervention = input$gr_intervention, comparison = input$gr_comparison))

  lapply(seq_len(GRADE_MAX_OUTCOMES), function(i) output[[paste0("gr_ois_", i)]] <- renderUI({
    o <- outcomes(); req(i <= length(o)); o <- o[[i]]
    ois <- grade_ois(.gv(o$control) / 1000, .gv(o$rrr) / 100)
    if (!is.finite(ois)) return(NULL)
    n <- .gv(o$n)
    div(class = "aside", sprintf("Optimal information size for a %.0f%% relative reduction (alpha 0.05, power 80%%): %s participants%s.", .gv(o$rrr), .gint(ois),
                                 if (is.finite(n)) sprintf(". Yours: %s", .gint(n)) else ""))
  }))

  # ---------------- fill an outcome from the open forest-plot analysis ----------------
  note <- reactiveVal(NULL)
  observeEvent(input$gr_from_fit, {
    f <- fit()
    if (!isTRUE(f$ok)) { showNotification("Load your data in the forest-plot mode first.", type = "warning"); return() }
    m <- f$env$m; rnd <- f$p$model != "common"
    te <- if (rnd) m$TE.random else m$TE.common; lo <- if (rnd) m$lower.random else m$lower.common; hi <- if (rnd) m$upper.random else m$upper.common
    bt <- function(x) tryCatch(meta::backtransf(x, sm = m$sm, n = m$n.harmonic.mean), error = function(e) x)
    measure <- if (m$sm %in% GRADE_MEASURES) m$sm else "Other"
    N <- if (!is.null(m$n.e) && !is.null(m$n.c)) sum(m$n.e + m$n.c, na.rm = TRUE) else if (!is.null(m$n)) sum(m$n, na.rm = TRUE) else NA_real_
    ctrl <- if (!is.null(m$event.c) && !is.null(m$n.c)) stats::median(m$event.c / m$n.c, na.rm = TRUE) * 1000 else NA_real_
    cmean <- if (!is.null(m$mean.c)) stats::median(m$mean.c, na.rm = TRUE) else NA_real_
    mb <- tryCatch(suppressWarnings(meta::metabias(m, method.bias = "Egger", k.min = 3)), error = function(e) NULL)
    bp <- if (is.null(mb)) NA_real_ else suppressWarnings(as.numeric((mb$pval %||% mb$p.value %||% NA)[1]))
    s <- grade_suggest(m, bp, if (measure %in% c("RR", "OR", "HR")) ctrl else NA_real_, 25, N)

    rb <- tryCatch({
      r <- rob_rows(); tool <- rob_tool()
      if (is.null(r) || !nrow(r) || tool == "nos") NULL
      else grade_rob_suggest(m, data.frame(study = r$study, overall = rob_overall(r, tool), stringsAsFactors = FALSE), tool, GRADE_DESIGN_RCT, rnd)
    }, error = function(e) NULL)

    # which block: the first empty one, else a new one
    n <- n_out(); isolate_in <- function(i, f) input[[paste0("gr_", f, "_", i)]]
    empty <- which(vapply(seq_len(n), function(i) !nzchar(.gt(isolate_in(i, "name"))) && !is.finite(.gv(isolate_in(i, "est"))), TRUE))
    target <- if (length(empty)) empty[1] else if (n < GRADE_MAX_OUTCOMES) n + 1L else { showNotification(sprintf("All %d outcomes are in use.", GRADE_MAX_OUTCOMES), type = "warning"); return() }

    sd <- list(name = if (nzchar(.gt(input$smlab))) .gt(input$smlab) else "Main outcome", k = m$k, n = if (is.finite(N)) N else NA, measure = measure,
               est = round(bt(te), 3), lo = round(bt(lo), 3), hi = round(bt(hi), 3),
               ctrl = if (is.finite(ctrl) && measure %in% c("RR", "OR", "HR")) round(ctrl, 1) else NA,
               src = if (is.finite(ctrl) && measure %in% c("RR", "OR", "HR")) "Assumed risk is the median control group risk across the studies." else "",
               cmean = if (is.finite(cmean) && measure == "MD") round(cmean, 2) else NA,
               inc = as.character(s$inconsistency), inc_t = if (s$inconsistency > 0) s$inc_text else "",
               imp = as.character(s$imprecision), imp_t = if (s$imprecision > 0) s$imp_text else "",
               pub = as.character(s$pubbias), pub_t = if (s$pubbias > 0) s$pub_text else "")
    if (!is.null(rb) && isTRUE(rb$matched)) { sd$rob <- as.character(rb$level); sd$rob_t <- if (rb$level > 0) rb$text else "" }
    seed <- gr_seed(); seed[[as.character(target)]] <- sd; gr_seed(seed)
    note(paste(c(s$note, if (!is.null(rb)) rb$text else "Risk of bias is read from the Risk of bias tab when its study names match the forest plot."), collapse = " "))
    if (target > n) n_out(target)
    gr_ver(gr_ver() + 1L)
  })
  output$gr_suggest_note <- renderUI({ s <- note(); if (!is.null(s)) div(class = "aside", paste("Suggested from your analysis:", s, "Indirectness is for you to judge. Check every suggestion before you use it.")) })

  # ---------------- results ----------------
  output$gr_issues <- renderUI({
    w <- grade_issues(outcomes())
    if (length(w)) div(class = "notice warn", tags$div(tags$b("Check before you use the table")), tags$ul(class = "gr-issues", lapply(w, tags$li)))
  })
  output$gr_sof <- renderUI(grade_sof_html(question(), outcomes()))
  output$gr_profile <- renderUI(grade_profile_html(question(), outcomes()))
  output$gr_methods <- renderUI(tags$p(enc2utf8(grade_methods_text(outcomes()))))
  output$gr_results <- renderUI(tagList(lapply(grade_result_sentences(outcomes()), function(s) tags$p(enc2utf8(s)))))
  output$dl_gr_csv <- downloadHandler("grade_summary_of_findings.csv", function(file) utils::write.csv(grade_csv(outcomes()), file, row.names = FALSE, fileEncoding = "UTF-8"))
  output$dl_gr_sof_doc <- downloadHandler("summary_of_findings.doc", function(file) writeLines(grade_word_doc(grade_sof_html(question(), outcomes()), "Summary of findings"), file, useBytes = TRUE))
  output$dl_gr_ep_doc <- downloadHandler("evidence_profile.doc", function(file) writeLines(grade_word_doc(grade_profile_html(question(), outcomes()), "Evidence profile"), file, useBytes = TRUE))
}
