# Calculator: median and IQR (or range) <-> mean and SD, other conversions, and a table of studies that can go to the forest plot.

conv_controls_ui <- function() {
  div(class = "controls",
    step("What to convert", help = "Choose the kind of conversion you need. The form below changes to match.",
      radioButtons("cv_dir", NULL, c("Median and quartiles (or range) to mean and SD" = "tomean",
                                     "Mean and SD to median and quartiles" = "tomedian",
                                     "Other: SE or 95% CI to SD, combine two groups" = "other"), width = "100%")),
    conditionalPanel("input.cv_dir == 'tomean'",
      step("One study", help = "Type the sample size and whatever the paper reported: the median with the quartiles, or the minimum and maximum. The result appears on the right.",
        numericInput("cv_n", "Sample size (n)", value = NA, min = 3),
        fluidRow(column(4, numericInput("cv_q1", "Q1", NA)), column(4, numericInput("cv_med", "Median", NA)), column(4, numericInput("cv_q3", "Q3", NA))),
        fluidRow(column(6, numericInput("cv_min", "Minimum", NA)), column(6, numericInput("cv_max", "Maximum", NA))),
        div(class = "aside", "Fill in the median plus the quartiles (Q1 and Q3), or the minimum and maximum, or all five. The more that was reported, the better the estimate."),
        actionButton("cv_example", "Fill an example", class = "btn-link btn-sm")),
      step("Many studies at once", help = "Paste a table from Excel with one row per study arm. Fill what was reported and leave the rest empty. The result table appears on the right.",
        textAreaInput("cv_batch", NULL, rows = 8, width = "100%",
          placeholder = "study\tgroup\tn\tmean\tsd\tmin\tq1\tmedian\tq3\tmax\nSmith 2019\ttreatment\t60\t\t\t\t45\t52\t60\t\nSmith 2019\tcontrol\t58\t55.1\t12.3\t\t\t\t\t\nLee 2021\ttreatment\t80\t\t\t30\t\t55\t\t90"),
        div(class = "aside", "Paste rows from Excel (or CSV). One row per study arm. Columns: study, group (treatment or control), n, mean, sd, min, q1, median, q3, max. Fill the mean and SD when the paper reports them; they are kept as they are. Leave empty what was not reported."),
        selectInput("cv_batch_method", "Method for the table", c("Choose for each row (recommended)" = "auto", "Wan 2014 + Luo 2018" = "wan", "Box-Cox" = "bc",
                                                                "Quantile estimation" = "qe", "Lognormal" = "mln"), width = "100%"),
        actionButton("cv_batch_example", "Fill an example table", class = "btn-link btn-sm"))),
    conditionalPanel("input.cv_dir == 'tomedian'",
      step("Values", help = "Type the mean and SD. The median and quartiles appear on the right.",
        fluidRow(column(6, numericInput("cv_mean", "Mean", NA)), column(6, numericInput("cv_sd", "SD", NA))))),
    conditionalPanel("input.cv_dir == 'other'",
      step("Which conversion", help = "Pick the conversion, then fill in the numbers that appear.",
        selectInput("cv_other", NULL, c("Standard error to SD" = "se", "95% confidence interval to SD" = "ci", "Combine two groups into one" = "comb"), width = "100%"),
        conditionalPanel("input.cv_other == 'se'", fluidRow(column(6, numericInput("cv_se", "Standard error", NA)), column(6, numericInput("cv_se_n", "n", NA, min = 2)))),
        conditionalPanel("input.cv_other == 'ci'",
          fluidRow(column(4, numericInput("cv_ci_lo", "Lower limit", NA)), column(4, numericInput("cv_ci_hi", "Upper limit", NA)), column(4, numericInput("cv_ci_n", "n", NA, min = 2))),
          div(class = "aside", "For the CI of a mean. The t distribution is used, as the Cochrane Handbook advises for small samples.")),
        conditionalPanel("input.cv_other == 'comb'",
          fluidRow(column(4, numericInput("cv_c_n1", "n, group 1", NA)), column(4, numericInput("cv_c_m1", "Mean 1", NA)), column(4, numericInput("cv_c_s1", "SD 1", NA))),
          fluidRow(column(4, numericInput("cv_c_n2", "n, group 2", NA)), column(4, numericInput("cv_c_m2", "Mean 2", NA)), column(4, numericInput("cv_c_s2", "SD 2", NA))))))
  )
}

conv_results_ui <- function() {
  div(class = "results",
    uiOutput("cv_msg"),
    uiOutput("cv_out"),
    uiOutput("cv_batch_out"),
    uiOutput("cv_methods_note"))
}

conv_fmt <- function(x) ifelse(is.finite(x), sub("\\.?0+$", "", sprintf("%.3f", x)), "")
conv_pfmt <- function(p) if (!is.finite(p)) "n/a" else if (p <= 0.01) "< 0.01" else if (p >= 0.5) "> 0.5" else sprintf("%.2f", p)

conv_table <- function(d, cols) {
  tags$table(class = "km-table conv-table",
    tags$thead(tags$tr(lapply(names(cols), tags$th))),
    tags$tbody(lapply(seq_len(nrow(d)), function(i)
      tags$tr(class = if (isTRUE(d$.best[i])) "conv-best", lapply(cols, function(cn) {
        v <- d[[cn]][i]; tags$td(if (is.numeric(v)) conv_fmt(v) else if (is.na(v)) "" else v)})))))
}

# Read a pasted table (tab, semicolon or comma separated) into the standard columns
conv_read_batch <- function(tx) {
  if (!nzchar(trimws(tx %||% ""))) return(NULL)
  first <- strsplit(tx, "\n")[[1]][1]
  sep <- if (grepl("\t", first)) "\t" else if (grepl(";", first)) ";" else ","
  d <- tryCatch(utils::read.table(text = tx, header = TRUE, sep = sep, quote = "\"", fill = TRUE, stringsAsFactors = FALSE,
                                  check.names = FALSE, na.strings = c("", "NA", "-"), colClasses = "character", comment.char = ""), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(list(error = "The pasted table could not be read. Use a header row with: study, group, n, mean, sd, min, q1, median, q3, max."))
  if (nrow(d) > 500) return(list(error = "Please paste at most 500 rows."))
  names(d) <- tolower(trimws(names(d)))
  al <- list(study = c("study", "study_id", "trial", "author"), group = c("group", "arm", "treatment", "grp"), n = c("n", "size", "sample", "sample size"),
             mean = c("mean", "avg"), sd = c("sd", "stdev"), min = c("min", "minimum"), q1 = c("q1", "p25", "25%"),
             median = c("median", "med"), q3 = c("q3", "p75", "75%"), max = c("max", "maximum"))
  pickc <- function(k) { h <- intersect(al[[k]], names(d)); if (length(h)) d[[h[1]]] else rep(NA_character_, nrow(d)) }
  num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
  st <- as.character(pickc("study")); st[is.na(st)] <- paste("Row", seq_along(st))[is.na(st)]
  list(data = data.frame(study = st, group = as.character(pickc("group")), n = num(pickc("n")), mean = num(pickc("mean")), sd = num(pickc("sd")),
                         min = num(pickc("min")), q1 = num(pickc("q1")), median = num(pickc("median")), q3 = num(pickc("q3")), max = num(pickc("max")),
                         stringsAsFactors = FALSE))
}

# Convert every row; returns the table with mean, sd, source and note
conv_batch_run <- function(d, method) {
  res <- lapply(seq_len(nrow(d)), function(i) { r <- d[i, ]
    cbind(data.frame(study = r$study, group = r$group, n = r$n, stringsAsFactors = FALSE),
          conv_row(r$n, r$mean, r$sd, r$min, r$q1, r$median, r$q3, r$max, method)) })
  do.call(rbind, res)
}

# Pair treatment and control rows of the same study into the continuous forest-plot layout
conv_pairs <- function(tab) {
  g <- km_group_of(tab$group); tab$g <- g
  msgs <- character(0); rows <- list()
  for (s in unique(tab$study)) {
    d <- tab[tab$study == s, , drop = FALSE]
    tx <- d[which(d$g == "treatment"), , drop = FALSE]; ct <- d[which(d$g == "control"), , drop = FALSE]
    if (nrow(tx) != 1 || nrow(ct) != 1) { msgs <- c(msgs, paste0(s, ": needs exactly one treatment row and one control row")); next }
    if (!all(is.finite(c(tx$mean, tx$sd, tx$n, ct$mean, ct$sd, ct$n)))) { msgs <- c(msgs, paste0(s, ": a mean, SD or n is missing")); next }
    rows[[length(rows) + 1]] <- data.frame(study_id = s, mean_tx = tx$mean, sd_tx = tx$sd, n_tx = tx$n, mean_ctrl = ct$mean, sd_ctrl = ct$sd, n_ctrl = ct$n, stringsAsFactors = FALSE)
  }
  list(data = if (length(rows)) do.call(rbind, rows) else NULL, skipped = msgs)
}

conv_server <- function(input, output, session, forest_raw, forest_note) {
  observeEvent(input$cv_example, {
    updateNumericInput(session, "cv_n", value = 120); updateNumericInput(session, "cv_q1", value = 45)
    updateNumericInput(session, "cv_med", value = 52); updateNumericInput(session, "cv_q3", value = 60)
    updateNumericInput(session, "cv_min", value = NA); updateNumericInput(session, "cv_max", value = NA)
  })
  observeEvent(input$cv_batch_example, {
    updateTextAreaInput(session, "cv_batch", value = paste(c(
      "study\tgroup\tn\tmean\tsd\tmin\tq1\tmedian\tq3\tmax",
      "Smith 2019\ttreatment\t60\t\t\t\t45\t52\t60\t",
      "Smith 2019\tcontrol\t58\t55.1\t12.3\t\t\t\t\t",
      "Lee 2021\ttreatment\t80\t\t\t30\t\t55\t\t90",
      "Lee 2021\tcontrol\t82\t\t\t28\t\t61\t\t97",
      "Costa 2022\ttreatment\t45\t50.2\t9.8\t\t\t\t\t",
      "Costa 2022\tcontrol\t47\t53.4\t11.1\t\t\t\t\t"), collapse = "\n"))
  })
  num1 <- function(x) if (is.null(x) || length(x) != 1 || is.na(x)) NA_real_ else as.numeric(x)

  output$cv_msg <- renderUI({
    if (!identical(input$cv_dir, "tomean")) return(NULL)
    if (is.na(num1(input$cv_n)) && is.na(num1(input$cv_med)) && !nzchar(input$cv_batch %||% ""))
      div(class = "aside", "Type the numbers on the left, or paste a table of studies. The estimates appear here.")
  })

  output$cv_out <- renderUI({
    if (identical(input$cv_dir, "other")) {
      w <- input$cv_other %||% "se"
      if (w == "se") {
        se <- num1(input$cv_se); n <- num1(input$cv_se_n)
        if (is.na(se) || is.na(n)) return(div(class = "aside", "Type the standard error and n."))
        if (se <= 0 || n < 2) return(div(class = "notice error", "The standard error must be above zero and n at least 2."))
        return(tagList(h3("Standard deviation"), div(class = "report-text", sprintf("SD = SE \u00d7 \u221an = %s", conv_fmt(conv_se_to_sd(se, n))))))
      }
      if (w == "ci") {
        lo <- num1(input$cv_ci_lo); hi <- num1(input$cv_ci_hi); n <- num1(input$cv_ci_n)
        if (anyNA(c(lo, hi, n))) return(div(class = "aside", "Type the two limits and n."))
        if (hi <= lo || n < 2) return(div(class = "notice error", "The upper limit must be above the lower limit, and n at least 2."))
        return(tagList(h3("Standard deviation"), div(class = "report-text", sprintf("SD = \u221an \u00d7 (upper \u2212 lower) / (2 \u00d7 t) = %s, with t = %.3f (%d degrees of freedom)", conv_fmt(conv_ci_to_sd(lo, hi, n)), qt(0.975, n - 1), round(n - 1)))))
      }
      v <- vapply(c("cv_c_n1", "cv_c_m1", "cv_c_s1", "cv_c_n2", "cv_c_m2", "cv_c_s2"), function(k) num1(input[[k]]), 0)
      if (anyNA(v)) return(div(class = "aside", "Type n, mean and SD for both groups."))
      if (v[1] < 2 || v[4] < 2 || v[3] <= 0 || v[6] <= 0) return(div(class = "notice error", "Each group needs n of at least 2 and an SD above zero."))
      r <- conv_combine(v[1], v[2], v[3], v[4], v[5], v[6])
      return(tagList(h3("Combined group"), conv_table(data.frame(n = r$n, mean = r$mean, sd = r$sd), c(n = "n", Mean = "mean", SD = "sd")),
                     div(class = "aside", "Formula from the Cochrane Handbook (section 6.5.2.10). It gives the mean and SD of the two groups taken together as one.")))
    }
    if (identical(input$cv_dir, "tomedian")) {
      m <- num1(input$cv_mean); s <- num1(input$cv_sd)
      if (is.na(m) || is.na(s)) return(div(class = "aside", "Type the mean and the SD."))
      if (s <= 0) return(div(class = "notice error", "The SD must be larger than zero."))
      r <- conv_to_median(m, s)
      r$.best <- seq_len(nrow(r)) == if (m - 2 * s < 0 && m > 0 && nrow(r) > 1) 2 else 1
      note <- if (m > 0 && m - 2 * s < 0) "The mean is less than 2 SDs above zero, so the data are probably right-skewed. The lognormal row is then the better choice, if the variable cannot be negative." else
        "If the data look roughly symmetric, the normal row is the one to use."
      return(tagList(h3("Median and quartiles"), conv_table(r, c(Assumption = "method", Median = "median", Q1 = "q1", Q3 = "q3", IQR = "iqr")),
                     div(class = "aside", note),
                     div(class = "aside", "These are estimates built on an assumed distribution. Say so in the methods.")))
    }
    n <- num1(input$cv_n); v <- lapply(list(min = input$cv_min, q1 = input$cv_q1, med = input$cv_med, q3 = input$cv_q3, max = input$cv_max), num1)
    if (is.na(n) && is.na(v$med)) return(NULL)
    err <- do.call(conv_check, c(list(n = n), v)); if (!is.null(err)) return(div(class = "notice warn", err))
    r <- do.call(conv_to_meansd, c(list(n = n), v)); sk <- do.call(conv_skew, c(v, list(n = n)))
    rec <- if (sk$skewed && nrow(r) > 1) 2 else 1; r$.best <- seq_len(nrow(r)) == rec
    tagList(h3("Mean and SD"),
      conv_table(r, c(Method = "method", Mean = "mean", SD = "sd")),
      div(class = "aside", if (sk$skewed)
        sprintf("The reported values look asymmetric (skewness index %.2f, approximate p = %s), so a skewed-data method (highlighted) is the safer choice.", sk$index, conv_pfmt(sk$p))
        else sprintf("No clear asymmetry in the reported values (skewness index %.2f, approximate p = %s), so Wan and Luo (highlighted) is appropriate. This check has little power with small samples or mild skew: if the variable cannot be negative and is usually skewed (length of stay, operative time), prefer a skewed-data method.", sk$index, conv_pfmt(sk$p))),
      if (!conv_have_estmeansd()) div(class = "notice warn", "The package estmeansd is not installed, so only Wan and Luo are shown. Install it with install.packages(\"estmeansd\") to add the skewed-data methods."),
      div(class = "aside", "Several methods agree when the data are symmetric. If they disagree, the data are skewed: report the skewed-data one and say which method you used."))
  })

  batch <- reactive(conv_read_batch(input$cv_batch))
  batch_res <- reactive({
    b <- batch(); if (is.null(b) || !is.null(b$error)) return(b)
    list(table = conv_batch_run(b$data, input$cv_batch_method %||% "auto"))
  })

  output$cv_batch_out <- renderUI({
    if (!identical(input$cv_dir, "tomean")) return(NULL)
    b <- batch_res(); if (is.null(b)) return(NULL)
    if (!is.null(b$error)) return(div(class = "notice warn", b$error))
    d <- b$table
    has_group <- any(!is.na(km_group_of(d$group)))
    tagList(h3("Table of studies"),
      conv_table(d, c(Study = "study", Group = "group", n = "n", Mean = "mean", SD = "sd", `Where it came from` = "source", Note = "note")),
      div(class = "downloads",
        downloadButton("cv_dl", "Download as CSV", class = "btn-outline-primary btn-sm"),
        if (has_group) actionButton("cv_send", "Send to the forest plot (continuous)", class = "btn-primary btn-sm")),
      if (!has_group) div(class = "aside", "Add a group column (treatment or control) to send the table to the forest plot."))
  })
  output$cv_dl <- downloadHandler(filename = function() "mean_sd_converted.csv",
    content = function(file) { b <- batch_res(); utils::write.csv(b$table, file, row.names = FALSE, na = "") })

  observeEvent(input$cv_send, {
    b <- batch_res(); req(b, is.null(b$error)); pr <- conv_pairs(b$table)
    if (is.null(pr$data) || nrow(pr$data) < 2) {
      showNotification(paste0("At least two complete studies are needed. ", paste(pr$skipped, collapse = "; ")), type = "warning", duration = 10); return()
    }
    out <- pr$data; out[2:7] <- lapply(out[2:7], function(x) format(round(x, 4), trim = TRUE))
    forest_raw(list(data = out, sep = ",", enc = "UTF-8", name = "converted_mean_sd.csv"))
    forest_note("Means and SDs from the calculator: reported values kept, the others estimated from medians and quartiles or ranges. Check the sources in the calculator table.")
    updateRadioButtons(session, "mode", selected = "forest")
    updateSelectInput(session, "type", selected = "cont")
    showNotification(paste0(nrow(out), " studies loaded in the forest plot.", if (length(pr$skipped)) paste0(" Left out: ", paste(pr$skipped, collapse = "; "), ".") else ""),
                     type = if (length(pr$skipped)) "warning" else "message", duration = 10)
  })

  output$cv_methods_note <- renderUI({
    div(class = "report-block",
      h3("Which method to use"),
      tags$ul(
        tags$li(tags$b("Wan 2014 (SD) with Luo 2018 (mean): "), "the standard choice when the data are roughly symmetric. Quick and widely cited."),
        tags$li(tags$b("Box-Cox and quantile estimation (McGrath 2020): "), "for skewed data. They need the median with quartiles or range and the sample size."),
        tags$li(tags$b("Lognormal (Cai 2021): "), "for skewed data that cannot be negative, such as hospital stay or bypass time."),
        tags$li(tags$b("Not used: "), "Hozo 2005. It is older and is known to overestimate the SD. The shortcut SD = IQR / 1.35 is the Cochrane rule of thumb and is only valid for symmetric data.")),
      div(class = "aside", "References: Wan X et al. BMC Med Res Methodol 2014;14:135. Luo D et al. Stat Methods Med Res 2018;27:1785. McGrath S et al. Stat Methods Med Res 2020;29:2520. Cai S et al. Stat Methods Med Res 2021;30:2131. Higgins JPT et al. Cochrane Handbook, section 6.5.2."))
  })
}
