# Digitizer mode: read the points off a Kaplan-Meier figure inside the browser (www/digitizer.js),
# type the numbers at risk and arm sizes, and send everything to the Kaplan-Meier analysis.

# The saved library keeps one extra column for arms: a free label for the subgroup (never used by the analysis itself).
# (Written out in full because this file is loaded before km_helpers.R; the tests check that it agrees with KM_COLS.)
DIG_COLS <- list(curve = c("study_id", "arm", "time", "survival"), risk = c("study_id", "arm", "time", "n_risk"),
                 arms = c("study_id", "arm", "group", "n_total", "total_events", "hr_published", "hr_ci_lower", "hr_ci_upper", "subgroup"))
dig_empty <- function(which) {
  as.data.frame(setNames(replicate(length(DIG_COLS[[which]]), character(0), simplify = FALSE), DIG_COLS[[which]]),
                stringsAsFactors = FALSE)
}
# A comparison is a list of "units" (one row each of the meta-analysis); each unit has curves on the treatment side and on the control side.
dig_cmp_empty <- function() data.frame(comparison = character(0), unit = character(0), group = character(0),
                                       study_id = character(0), arm = character(0), stringsAsFactors = FALSE)

# Turn the rows of one comparison into the three tables the analysis reads. Every unit becomes a "study";
# several curves on one side are pooled by the analysis itself (their patients are rebuilt and put together).
# Returns list(error) or list(curve, risk, arms, warnings).
# Survival cannot be above 1 or below 0. Small excursions (up to 10%) come from tracing the thick line a few pixels past the axis value
# and are clipped; larger ones are left alone so a wrong calibration or scale still raises the error.
dig_clamp01 <- function(x) { x <- as.numeric(x); i <- !is.na(x) & ((x > 1 & x <= 1.1) | (x < 0 & x >= -0.1)); x[i] <- pmin(pmax(x[i], 0), 1); x }

dig_build_comparison <- function(cm, curve, risk, arms) {
  key <- function(d) paste(d$study_id, d$arm, sep = "\r")
  show <- function(k) gsub("\r", " / ", k)
  if (!nrow(cm)) return(list(error = "This comparison has no units yet."))
  ck <- paste(cm$study_id, cm$arm, sep = "\r")
  miss <- setdiff(ck, key(arms))
  if (length(miss)) return(list(error = paste0("Still missing the patients in the arm for: ", paste(show(utils::head(miss, 4)), collapse = "; "),
                                                 ". Fill them in step 4 (Edit a saved curve).")))
  for (u in unique(cm$unit)) {
    g <- cm$group[cm$unit == u]
    if (!all(c("treatment", "control") %in% g))
      return(list(error = sprintf("The unit \"%s\" needs at least one curve on the treatment side and one on the control side.", u)))
    k <- ck[cm$unit == u]
    if (anyDuplicated(k)) return(list(error = sprintf("The unit \"%s\" uses the same curve twice (%s).", u, show(k[duplicated(k)][1]))))
  }
  wrn <- character(0)
  rep_k <- unique(ck[duplicated(ck)])
  if (length(rep_k)) wrn <- c(wrn, paste0("The same curve is used in more than one unit (", paste(show(utils::head(rep_k, 3)), collapse = "; "),
                                           "). Its patients are counted in each of them, so the units are not independent: the pooled confidence interval can be too narrow."))
  oc <- orr <- oa <- list()
  for (i in seq_len(nrow(cm))) {
    u <- cm$unit[i]; k <- ck[i]
    same <- cm$unit == u
    lab <- if (sum(cm$arm[same] == cm$arm[i]) == 1) cm$arm[i] else paste0(cm$arm[i], " [", cm$study_id[i], "]")
    cv <- curve[key(curve) == k, KM_COLS$curve, drop = FALSE]; cv$study_id <- u; cv$arm <- lab
    rk <- risk[key(risk) == k, KM_COLS$risk, drop = FALSE]
    if (nrow(rk)) { rk$study_id <- u; rk$arm <- lab }
    ar <- arms[key(arms) == k, KM_COLS$arms, drop = FALSE][1, , drop = FALSE]
    ar$study_id <- u; ar$arm <- lab; ar$group <- cm$group[i]
    oc[[i]] <- cv; orr[[i]] <- rk; oa[[i]] <- ar
  }
  list(curve = do.call(rbind, oc), risk = do.call(rbind, orr), arms = do.call(rbind, oa), warnings = wrn)
}

dig_btn <- function(label, ..., class = "btn-outline-primary btn-sm") tags$button(type = "button", class = paste("btn", class), ..., label)
dig_num <- function(id, label, value = "") div(class = "dig-num",
  tags$label(`for` = id, label),
  tags$input(id = id, type = "text", inputmode = "decimal", class = "form-control", value = value, autocomplete = "off"))

dig_controls_ui <- function() {
  div(class = "controls",
    step("Load the figure", help = "Choose the image of the figure, or paste a screenshot (Ctrl+V, or Cmd+V on Mac). Then type the study and arm names.",
      tags$input(id = "dig_file", type = "file", accept = "image/*", class = "form-control"),
      div(class = "aside", "Or paste a screenshot with Ctrl+V (Cmd+V on Mac), or drop an image on the figure. The image stays in your browser and is not uploaded."),
      fluidRow(
        column(6, div(class = "dig-num", tags$label(`for` = "dig_study", "Study name"),
                      tags$input(id = "dig_study", type = "text", class = "form-control", placeholder = "Trial A 2012", autocomplete = "off"))),
        column(6, div(class = "dig-num", tags$label(`for` = "dig_arm", "Arm name"),
                      tags$input(id = "dig_arm", type = "text", class = "form-control", placeholder = "Intervention", autocomplete = "off"))))
    ),
    step("Calibrate the axes", help = "Teach the app the axes. Type the value of a tick, press its Set button, then click that tick on the figure. Do it for two ticks on each axis, far apart.",
      tip("Type the value of one tick on each axis, press its button, then click that tick on the figure. Use two ticks that are far apart, such as 0 and the last tick."),
      div(class = "dig-cal-grid",
        dig_num("dig_x1v", "X1 value", "0"), dig_btn("Set X1", `data-dig-mode` = "cal-x1"),
        dig_num("dig_x2v", "X2 value", ""), dig_btn("Set X2", `data-dig-mode` = "cal-x2"),
        dig_num("dig_y1v", "Y1 value", "0"), dig_btn("Set Y1", `data-dig-mode` = "cal-y1"),
        dig_num("dig_y2v", "Y2 value", "1"), dig_btn("Set Y2", `data-dig-mode` = "cal-y2")),
      div(class = "dig-cal-grid two",
        div(class = "dig-num", tags$label(`for` = "dig_xscale", "X axis"),
            tags$select(id = "dig_xscale", class = "form-select", tags$option(value = "linear", "Linear"), tags$option(value = "log", "Logarithmic"))),
        div(class = "dig-num", tags$label(`for` = "dig_yscale", "Y axis"),
            tags$select(id = "dig_yscale", class = "form-select", tags$option(value = "linear", "Linear"), tags$option(value = "log", "Logarithmic")))),
      tip("Y values are read as they appear on the figure: 0 to 1, or 0 to 100 if the axis shows percent. Kaplan-Meier curves almost always have linear axes. Choose Logarithmic only if the ticks are spaced that way (values must be above zero), and calibrate after choosing."),
      div(class = "dig-calstate", id = "dig_calstate", "\u2022 X1   \u2022 X2   \u2022 Y1   \u2022 Y2"),
      dig_btn("Reset calibration", `data-dig-act` = "reset-cal", class = "btn-link btn-sm"),
      selectInput("dig_ctype", "This figure shows",
        c("Survival (falling curve)" = "survival", "Cumulative incidence (rising curve)" = "cumulative"), width = "100%")
    ),
    step("Trace the curve", help = "Press Add points and click along the curve, or pick its colour and press Trace by colour. Fill the details you know, then press Save this curve.",
      div(class = "dig-tools",
        dig_btn("Add points", `data-dig-mode` = "add"),
        dig_btn("Erase", `data-dig-mode` = "erase"),
        dig_btn("Undo", `data-dig-act` = "undo"),
        dig_btn("Clear", `data-dig-act` = "clear")),
      div(class = "aside", "Click along the curve, about every 2 to 5% of the time axis and at each drop. Or trace automatically by colour:"),
      div(class = "dig-tools",
        dig_btn("Pick colour", `data-dig-mode` = "pick"), tags$span(id = "dig_swatch", class = "dig-swatch", title = "Picked colour"),
        dig_btn("Trace by colour", `data-dig-act` = "trace", class = "btn-primary btn-sm")),
      div(class = "dig-cal-grid two",
        dig_num("dig_tol", "Colour tolerance", "60"), dig_num("dig_step", "Pixels between points", "3")),
      div(class = "dig-num", tags$label(`for` = "dig_start", "The curve begins at"),
          tags$select(id = "dig_start", class = "form-select", tags$option(value = "top", "The top of the scale (survival curves)"),
                      tags$option(value = "bottom", "The bottom of the scale (cumulative incidence)"), tags$option(value = "click", "Where I clicked"))),
      div(class = "dig-cal-grid two",
        tags$label(class = "dig-check", tags$input(type = "checkbox", id = "dig_mask"), " Ignore curves already saved"),
        dig_num("dig_maskw", "Ignored band (pixels, each side)", "4")),
      tip("For a dashed or overlapping curve in the same colour as another: trace and save the continuous curve first, then tick this box and trace the dashed one. ",
          "Where the dashed curve hides behind the saved one, or between dashes, its position is estimated from the saved curve and those points are shown in orange: check them."),
      div(class = "dig-tools",
        dig_btn("Limit to a box", `data-dig-mode` = "box"), dig_btn("Remove box", `data-dig-act` = "clear-box", class = "btn-link btn-sm")),
      tip("Automatic tracing works best for a curve with a distinct colour. Check the result and erase stray points (legend, censoring marks, other curves). ",
          "Black and grey curves: trace the continuous curve first and save it; then tick \"Ignore curves already saved\" for the dashed one. A dashed line is often fainter (grey, blurred): raise the tolerance to about 100\u2013130 for it. Where two curves overlap closely, check the result by eye and correct by clicking."),
      tags$h4(class = "dig-sub", "Details of this curve"),
      tip("Fill what you have now and everything is saved together with the curve. Anything left empty can be added later in step 4. ",
          "You decide which curves are treatment and which are control later, in \"Compare groups\"."),
      div(class = "dig-cal-grid two",
        dig_num("dig_sub", "Subgroup (optional label)"), dig_num("dig_n", "Patients in the arm")),
      div(class = "dig-cal-grid two",
        dig_num("dig_ev", "Total events (optional)"), dig_num("dig_tmult", "Multiply the times by", "1")),
      tip("The multiplier puts this figure in the same time unit as your other studies, for example 12 to turn years into months. It applies to the curve and to the times of the risk table typed here."),
      div(class = "dig-num", tags$label(`for` = "dig_tt", "Times in the table under the figure"),
          tags$input(id = "dig_tt", type = "text", class = "form-control", placeholder = "0, 12, 24, 36", autocomplete = "off")),
      div(class = "dig-num", tags$label(`for` = "dig_nr", "Numbers at risk"),
          tags$input(id = "dig_nr", type = "text", class = "form-control", placeholder = "250, 180, 120, 60", autocomplete = "off")),
      tags$details(class = "mapping",
        tags$summary("Hazard ratio published for this arm (optional)"),
        div(class = "aside", "Use it on the arm that will be the treatment. It is only used to compare with the reconstructed one."),
        div(class = "dig-cal-grid two", dig_num("dig_hr", "HR"), dig_num("dig_hrlo", "CI lower")),
        dig_num("dig_hrhi", "CI upper")),
      div(class = "dig-save",
        tags$span(class = "dig-count", tags$b(id = "dig_npts", "0"), " points"),
        dig_btn("Save this curve and its details", `data-dig-act` = "save", class = "btn-primary"))
    ),
    step("Edit a saved curve", help = "Only needed to fix or complete a curve you already saved. Choose it here and change its details.",
      uiOutput("dig_pair_ui"),
      div(class = "aside", "Add or correct the details of a curve that is already saved, and check it against the figure. Times and numbers are separated by commas.")
    )
  )
}

# A long explanation that stays out of the way until asked for
tip <- function(...) tags$details(class = "tip", tags$summary("Tip"), div(class = "aside", ...))

dig_results_ui <- function() {
  div(class = "results",
    navset_underline(id = "dig_tabs",
      nav_panel("Figure", value = "fig",
    div(class = "dig-card",
      div(class = "dig-toolbar",
        div(class = "dig-tools",
          dig_btn("\u2212", `data-dig-act` = "zoom-out", `aria-label` = "Zoom out"),
          dig_btn("+", `data-dig-act` = "zoom-in", `aria-label` = "Zoom in"),
          dig_btn("Fit", `data-dig-act` = "fit")),
        div(class = "dig-readout", id = "dig_readout"),
        tags$canvas(id = "dig_loupe", width = 110, height = 110, class = "dig-loupe", title = "Magnifier")),
      div(id = "dig_status", class = "dig-status", role = "status", "Load a figure to start."),
      div(id = "dig_wrap", class = "dig-wrap",
        div(id = "dig_empty", class = "dig-empty",
          h2("Your figure appears here"),
          p("Choose an image on the left, paste a screenshot, or drop a file on this area.")),
        tags$canvas(id = "dig_canvas"))
    )
      ),
      nav_panel("Saved curves and comparisons", value = "saved",
    h3("Saved curves"),
    uiOutput("dig_saved_note"),
    DTOutput("dig_tbl_curves"),
    div(class = "aside", "Each row is one saved curve. Select rows here to remove them or to build a comparison in the next tab."),
    tags$details(class = "mapping",
      tags$summary("Numbers at risk and arm sizes (details)"),
      h4("Numbers at risk"),
      DTOutput("dig_tbl_risk"),
      h4("Arms"),
      DTOutput("dig_tbl_arms")),
    div(class = "downloads",
      actionButton("dig_rm", "Remove selected rows", class = "btn-outline-primary btn-sm"),
      downloadButton("dig_dl_curve", "Curve points (CSV)", class = "btn-outline-primary btn-sm"),
      downloadButton("dig_dl_risk", "Numbers at risk (CSV)", class = "btn-outline-primary btn-sm"),
      downloadButton("dig_dl_arms", "Arms (CSV)", class = "btn-outline-primary btn-sm")),
    div(class = "aside", "Removing a row of the curve table removes that whole curve, with its numbers at risk and arm size."),
      h3("Compare groups"),
    div(class = "aside", "Choose which saved curves are compared. A comparison is made of units; each unit is one row of the meta-analysis and has curves on the treatment side and on the control side. ",
        "Curves of the same side in one unit are pooled (their patients are rebuilt and put together). The same saved curves can be used again in other comparisons."),
    fluidRow(
      column(6, textInput("dig_cmp_name", "Comparison name", "Comparison 1", width = "100%")),
      column(6, selectInput("dig_cmp_pick", "Or open one you made", c("(none yet)" = ""), width = "100%"))),
    textInput("dig_unit", "Unit name (one row of the meta-analysis)", "", placeholder = "For example the study name", width = "100%"),
    uiOutput("dig_stage_ui"),
    div(class = "downloads",
      actionButton("dig_add_tx", "Selected rows \u2192 treatment side", class = "btn-outline-primary btn-sm"),
      actionButton("dig_add_ct", "Selected rows \u2192 control side", class = "btn-outline-primary btn-sm"),
      actionButton("dig_clear_stage", "Clear both sides", class = "btn-link btn-sm"),
      actionButton("dig_add_unit", "Add this unit to the comparison", class = "btn-primary btn-sm")),
    uiOutput("dig_cmp_note"),
    DTOutput("dig_tbl_cmp"),
    div(class = "downloads",
      actionButton("dig_cmp_rm", "Remove selected rows", class = "btn-outline-primary btn-sm"),
      actionButton("dig_cmp_del", "Delete this comparison", class = "btn-outline-primary btn-sm")),
    fluidRow(
      column(6, textInput("dig_lab_tx", "Name of the treatment side in the plots", "Treatment", width = "100%")),
      column(6, textInput("dig_lab_ct", "Name of the control side in the plots", "Control", width = "100%"))),
    div(class = "downloads",
      actionButton("dig_use", "Use this comparison in the Kaplan-Meier analysis", class = "btn-primary")),
    div(class = "aside", "If you made no comparison, the curves that have a role (set in step 4) are used as before.")
      )
    )
  )
}

dig_server <- function(input, output, session, files, file_msg) {
  dig <- reactiveValues(curve = dig_empty("curve"), risk = dig_empty("risk"), arms = dig_empty("arms"), cmp = dig_cmp_empty(), scale = "proportion", tmult = 1)

  pair_key <- function(d) paste(d$study_id, d$arm, sep = "\r")

  # Numbers at risk and the arm row of one saved curve. Returns NULL, or a message when something is wrong.
  save_details <- function(study, arm, nt, ev, times_txt, nr_txt, hr, subgroup = "", group = "", tmult = 1) {
    key <- paste(study, arm, sep = "\r")
    if (is.null(nt) || length(nt) != 1 || is.na(nt) || nt <= 0) return("Type the number of patients in the arm.")
    nums <- function(x) suppressWarnings(as.numeric(strsplit(gsub(",", " ", gsub(";", " ", x %||% "")), "[[:space:]]+")[[1]]))
    tt <- nums(times_txt); nr <- nums(nr_txt); tt <- tt[!is.na(tt)]; nr <- nr[!is.na(nr)]
    if (length(tt) != length(nr)) return(sprintf("There are %d times and %d numbers at risk. They must have the same length.", length(tt), length(nr)))
    tt <- tt * tmult
    r <- dig$risk; r <- r[pair_key(r) != key, , drop = FALSE]
    if (length(tt)) r <- rbind(r, data.frame(study_id = study, arm = arm, time = as.character(tt), n_risk = as.character(nr), stringsAsFactors = FALSE))
    dig$risk <- r
    has_hr <- length(hr) == 3 && !anyNA(hr)
    grp <- if (length(group) == 1 && !is.na(group) && group %in% c("treatment", "control")) group else ""
    a <- dig$arms; a <- a[pair_key(a) != key, , drop = FALSE]
    row <- data.frame(study_id = study, arm = arm, group = grp,
                      n_total = as.character(nt), total_events = if (is.null(ev) || length(ev) != 1 || is.na(ev)) NA_character_ else as.character(ev),
                      hr_published = if (has_hr) as.character(hr[1]) else NA_character_,
                      hr_ci_lower = if (has_hr) as.character(hr[2]) else NA_character_,
                      hr_ci_upper = if (has_hr) as.character(hr[3]) else NA_character_,
                      subgroup = txt(subgroup, 60), stringsAsFactors = FALSE)
    dig$arms <- rbind(a, row)
    NULL
  }

  # A curve arrives from the browser, together with the details typed beside it.
  # Whatever the figure shows (percent, cumulative incidence, other time unit) is stored as survival proportion in the analysis time unit,
  # so that curves from different figures can be used together.
  observeEvent(input$dig_save, {
    s <- input$dig_save
    tm <- as.numeric(unlist(s$time)); sv <- as.numeric(unlist(s$surv))
    if (length(tm) < 5 || length(tm) != length(sv) || anyNA(c(tm, sv))) {
      showNotification("The points could not be read. Try saving the curve again.", type = "error"); return()
    }
    study <- txt(s$study, 80); arm <- txt(s$arm, 80)
    if (!nzchar(study) || !nzchar(arm)) return()
    d <- s$det
    num1 <- function(x) { v <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x %||% "")))); if (length(v) != 1) NA_real_ else v }
    tmult <- num1(d$tmult); if (is.na(tmult) || tmult <= 0) tmult <- 1
    pct <- isTRUE(as.numeric(s$ymax) > 1.5)
    if (pct) sv <- sv / 100
    if (identical(input$dig_ctype, "cumulative")) sv <- 1 - sv
    sv <- dig_clamp01(sv)   # tracing slightly past the 100% / 0% line is a few pixels of drawing error, not a real value
    tm <- tm * tmult
    # A curve is identified by its study and arm name. A new curve with the same two names but another subgroup is a DIFFERENT curve:
    # keep both (the new one is named "arm (subgroup)"). Same names with the same or no subgroup is a re-trace of the same curve and replaces it.
    renamed <- FALSE; sub_new <- txt(d$sub %||% "", 60); k0 <- paste(study, arm, sep = "\r")
    if (k0 %in% pair_key(dig$curve) && nzchar(sub_new)) {
      sub_old <- dig$arms$subgroup[match(k0, pair_key(dig$arms))]
      if (length(sub_old) == 1 && !is.na(sub_old) && nzchar(sub_old) && !identical(sub_old, sub_new)) { arm <- paste0(arm, " (", sub_new, ")"); renamed <- TRUE }
    }
    replaced <- paste(study, arm, sep = "\r") %in% pair_key(dig$curve)
    new <- data.frame(study_id = study, arm = arm, time = as.character(round(tm, 4)), survival = as.character(round(sv, 6)), stringsAsFactors = FALSE)
    old <- dig$curve; old <- old[pair_key(old) != paste(study, arm, sep = "\r"), , drop = FALSE]
    dig$curve <- rbind(old, new)
    dig$scale <- if (pct) "percent" else "proportion"   # how THIS figure is drawn (used to draw the check overlay on it)
    dig$tmult <- tmult
    msg <- NULL; has_d <- !is.null(d) && !is.na(num1(d$n))
    if (has_d) msg <- save_details(study, arm, num1(d$n), num1(d$ev), d$tt, d$nr, c(num1(d$hr), num1(d$hrlo), num1(d$hrhi)),
                                   subgroup = d$sub, group = "", tmult = tmult)
    showNotification(paste0(sprintf("Saved %d points for %s / %s", length(tm), study, arm),
                            if (renamed) " (the arm name already existed with another subgroup, so this curve was kept as a separate one)" else if (replaced) " (it replaced the curve saved before with the same study and arm name)" else "",
                            if (has_d && is.null(msg)) " and its details." else ".",
                            if (!is.null(msg)) paste0(" The details were not saved: ", msg, " Add them in step 4.") else
                              if (!has_d) " Add its patients and numbers at risk in step 4." else ""),
                     type = if (is.null(msg)) "message" else "warning", duration = 8)
  })

  pairs <- reactive({
    d <- dig$curve
    if (!nrow(d)) return(data.frame(study_id = character(0), arm = character(0)))
    unique(d[, c("study_id", "arm")])
  })

  output$dig_pair_ui <- renderUI({
    pr <- pairs()
    if (!nrow(pr)) return(div(class = "aside", "Save a curve first (step 3). Then its arm appears here."))
    ch <- setNames(seq_len(nrow(pr)), paste(pr$study_id, pr$arm, sep = " / "))
    tagList(
      selectInput("dig_pair", "Arm", choices = ch, width = "100%"),
      textInput("dig_subgroup", "Subgroup (optional label)", "", width = "100%"),
      selectInput("dig_group", "Role (optional; you can decide later in Compare groups)",
                  c("Decide later" = "", "Treatment" = "treatment", "Control" = "control"), width = "100%"),
      fluidRow(
        column(6, numericInput("dig_ntotal", "Patients in the arm", value = NA, min = 1)),
        column(6, numericInput("dig_events", "Total events (optional)", value = NA, min = 0))),
      textInput("dig_times", "Times in the table", "", placeholder = "0, 12, 24, 36", width = "100%"),
      textInput("dig_nrisk", "Numbers at risk", "", placeholder = "250, 180, 120, 60", width = "100%"),
      tags$details(class = "mapping",
        tags$summary("Hazard ratio published in the paper (optional, on the treatment arm)"),
        fluidRow(
          column(4, numericInput("dig_hr", "HR", value = NA)),
          column(4, numericInput("dig_hr_lo", "CI lower", value = NA)),
          column(4, numericInput("dig_hr_hi", "CI upper", value = NA)))),
      actionButton("dig_add_arm", "Save these details", class = "btn-primary btn-sm"),
      div(class = "aside", "When the arm is saved, check the rebuilt curve against the figure:"),
      div(class = "dig-tools",
        actionButton("dig_check", "Check against the figure", class = "btn-outline-primary btn-sm"),
        dig_btn("Remove overlay", `data-dig-act` = "clear-overlay", class = "btn-link btn-sm"))
    )
  })

  # Details of a saved curve, from the edit panel
  observeEvent(input$dig_add_arm, {
    pr <- pairs(); i <- suppressWarnings(as.integer(input$dig_pair))
    if (is.na(i) || i < 1 || i > nrow(pr)) return()
    study <- pr$study_id[i]; arm <- pr$arm[i]
    msg <- save_details(study, arm, input$dig_ntotal, input$dig_events, input$dig_times, input$dig_nrisk,
                        c(input$dig_hr, input$dig_hr_lo, input$dig_hr_hi), subgroup = input$dig_subgroup, group = input$dig_group)
    if (!is.null(msg)) { showNotification(msg, type = "warning"); return() }
    showNotification(sprintf("Saved the details of %s / %s: %d patients.", study, arm, round(input$dig_ntotal)), type = "message")
  })

  # Show what is already saved for the chosen curve
  observeEvent(input$dig_pair, {
    pr <- pairs(); i <- suppressWarnings(as.integer(input$dig_pair))
    if (is.na(i) || i < 1 || i > nrow(pr)) return()
    k <- paste(pr$study_id[i], pr$arm[i], sep = "\r"); a <- dig$arms; j <- match(k, pair_key(a))
    n1 <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) != 1) NA_real_ else v }
    if (is.na(j)) {
      updateNumericInput(session, "dig_ntotal", value = NA); updateNumericInput(session, "dig_events", value = NA)
      updateTextInput(session, "dig_times", value = ""); updateTextInput(session, "dig_nrisk", value = "")
      updateTextInput(session, "dig_subgroup", value = ""); updateSelectInput(session, "dig_group", selected = "")
      for (z in c("dig_hr", "dig_hr_lo", "dig_hr_hi")) updateNumericInput(session, z, value = NA)
      return()
    }
    r <- dig$risk[pair_key(dig$risk) == k, , drop = FALSE]; r <- r[order(suppressWarnings(as.numeric(r$time))), , drop = FALSE]
    updateNumericInput(session, "dig_ntotal", value = n1(a$n_total[j])); updateNumericInput(session, "dig_events", value = n1(a$total_events[j]))
    updateTextInput(session, "dig_times", value = paste(r$time, collapse = ", ")); updateTextInput(session, "dig_nrisk", value = paste(r$n_risk, collapse = ", "))
    updateTextInput(session, "dig_subgroup", value = if (is.na(a$subgroup[j])) "" else a$subgroup[j])
    updateSelectInput(session, "dig_group", selected = if (a$group[j] %in% c("treatment", "control")) a$group[j] else "")
    updateNumericInput(session, "dig_hr", value = n1(a$hr_published[j])); updateNumericInput(session, "dig_hr_lo", value = n1(a$hr_ci_lower[j]))
    updateNumericInput(session, "dig_hr_hi", value = n1(a$hr_ci_upper[j]))
  }, ignoreInit = TRUE)

  # ---- Tables
  curve_summary <- reactive({
    d <- dig$curve
    if (!nrow(d)) return(data.frame(Study = character(0), Arm = character(0), Subgroup = character(0), Patients = character(0),
                                    Events = character(0), Points = integer(0), check.names = FALSE))
    k <- pair_key(d); u <- unique(k); a <- dig$arms; ia <- match(u, pair_key(a))
    na0 <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
    data.frame(Study = d$study_id[match(u, k)], Arm = d$arm[match(u, k)], Subgroup = na0(a$subgroup[ia]),
               Patients = na0(a$n_total[ia]), Events = na0(a$total_events[ia]),
               Points = as.integer(table(factor(k, levels = u))), check.names = FALSE)
  })
  tbl <- function(d, sel = TRUE) datatable(d, rownames = FALSE, selection = if (sel) "multiple" else "none",
                                           options = list(dom = "tip", pageLength = 6, scrollX = TRUE))
  output$dig_tbl_curves <- renderDT(tbl(curve_summary()))
  output$dig_tbl_risk <- renderDT(tbl(dig$risk))
  output$dig_tbl_arms <- renderDT(tbl(dig$arms[, setdiff(names(dig$arms), "group"), drop = FALSE]))
  output$dig_saved_note <- renderUI({
    if (!nrow(dig$curve)) div(class = "aside", "Nothing saved yet. Each saved curve appears here.")
    else {
      miss <- setdiff(pair_key(unique(dig$curve[, c("study_id", "arm")])), pair_key(dig$arms))
      if (length(miss)) div(class = "notice warn", paste0("Still missing the number of patients for: ", paste(gsub("\r", " / ", miss), collapse = "; "), ". Add it in step 4 (Edit a saved curve)."))
    }
  })

  observeEvent(input$dig_rm, {
    ic <- input$dig_tbl_curves_rows_selected; ir <- input$dig_tbl_risk_rows_selected; ia <- input$dig_tbl_arms_rows_selected
    if (length(ir)) dig$risk <- dig$risk[-ir, , drop = FALSE]
    if (length(ia)) dig$arms <- dig$arms[-ia, , drop = FALSE]
    if (length(ic)) {   # removing a curve removes everything saved with it
      cs <- curve_summary()[ic, ]; k <- paste(cs$Study, cs$Arm, sep = "\r")
      dig$curve <- dig$curve[!pair_key(dig$curve) %in% k, , drop = FALSE]
      dig$risk <- dig$risk[!pair_key(dig$risk) %in% k, , drop = FALSE]
      dig$arms <- dig$arms[!pair_key(dig$arms) %in% k, , drop = FALSE]
      dig$cmp <- dig$cmp[!pair_key(dig$cmp) %in% k, , drop = FALSE]
    }
  })

  # ---- Compare groups: build a comparison from the saved curves, without digitizing anything again
  stg <- reactiveValues(tx = character(0), ct = character(0))
  cmp_name <- reactive({ n <- trimws(input$dig_cmp_name %||% ""); if (nzchar(n)) txt(n, 60) else "Comparison 1" })
  cmp_rows <- reactive({ d <- dig$cmp; d[d$comparison == cmp_name(), , drop = FALSE] })
  observe({
    nm <- unique(dig$cmp$comparison)
    updateSelectInput(session, "dig_cmp_pick", choices = c(setNames("", if (length(nm)) "Choose one" else "(none yet)"), setNames(nm, nm)), selected = "")
  })
  observeEvent(input$dig_cmp_pick, { if (nzchar(input$dig_cmp_pick %||% "")) updateTextInput(session, "dig_cmp_name", value = input$dig_cmp_pick) }, ignoreInit = TRUE)
  lib_label <- function(k) {
    a <- dig$arms; j <- match(k, pair_key(a)); pr <- strsplit(k, "\r", fixed = TRUE)[[1]]
    sg <- if (!is.na(j) && !is.na(a$subgroup[j]) && nzchar(a$subgroup[j])) paste0(" - ", a$subgroup[j]) else ""
    sprintf("%s%s (%s)", pr[2], sg, pr[1])
  }
  add_side <- function(side) {
    ic <- input$dig_tbl_curves_rows_selected
    if (!length(ic)) { showNotification("Select one or more rows in the table of saved curves first.", type = "warning"); return() }
    cs <- curve_summary()[ic, ]; k <- paste(cs$Study, cs$Arm, sep = "\r"); other <- if (side == "tx") "ct" else "tx"
    stg[[side]] <- union(stg[[side]], k); stg[[other]] <- setdiff(stg[[other]], k)
    DT::selectRows(DT::dataTableProxy("dig_tbl_curves"), NULL)   # the rows are on a side now: clear the selection
  }
  observeEvent(input$dig_add_tx, add_side("tx"))
  observeEvent(input$dig_add_ct, add_side("ct"))
  observeEvent(input$dig_clear_stage, { stg$tx <- character(0); stg$ct <- character(0) })
  output$dig_stage_ui <- renderUI({
    f <- function(v) if (length(v)) paste(vapply(v, lib_label, ""), collapse = "; ") else "none yet"
    div(class = "aside", tags$b("Treatment side: "), f(stg$tx), tags$br(), tags$b("Control side: "), f(stg$ct))
  })
  observeEvent(input$dig_add_unit, {
    u <- txt(input$dig_unit, 80)
    if (!nzchar(u)) { showNotification("Type a name for the unit.", type = "warning"); return() }
    if (!length(stg$tx) || !length(stg$ct)) { showNotification("A unit needs at least one curve on the treatment side and one on the control side.", type = "warning"); return() }
    k <- c(stg$tx, stg$ct); nm <- cmp_name()
    rows <- data.frame(comparison = nm, unit = u, group = c(rep("treatment", length(stg$tx)), rep("control", length(stg$ct))),
                       study_id = sub("\r.*$", "", k), arm = sub("^[^\r]*\r", "", k), stringsAsFactors = FALSE)
    old <- dig$cmp; old <- old[!(old$comparison == nm & old$unit == u), , drop = FALSE]
    dig$cmp <- rbind(old, rows)
    stg$tx <- character(0); stg$ct <- character(0); updateTextInput(session, "dig_unit", value = "")
    showNotification(sprintf("Unit \"%s\" added to \"%s\".", u, nm), type = "message", duration = 4)
  })
  cmp_view <- reactive({
    d <- cmp_rows()
    if (!nrow(d)) return(data.frame(Unit = character(0), Side = character(0), Study = character(0), Arm = character(0), Subgroup = character(0), Patients = character(0)))
    a <- dig$arms; i <- match(paste(d$study_id, d$arm, sep = "\r"), pair_key(a)); na0 <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
    data.frame(Unit = d$unit, Side = ifelse(d$group == "treatment", "Treatment", "Control"), Study = d$study_id, Arm = d$arm,
               Subgroup = na0(a$subgroup[i]), Patients = na0(a$n_total[i]))
  })
  output$dig_tbl_cmp <- renderDT(tbl(cmp_view()))
  output$dig_cmp_note <- renderUI({
    d <- cmp_rows()
    if (!nrow(d)) return(div(class = "aside", "No units in this comparison yet: select rows in the table of saved curves, send them to a side, name the unit and press Add this unit."))
    k <- paste(d$study_id, d$arm, sep = "\r"); nn <- suppressWarnings(as.numeric(dig$arms$n_total[match(k, pair_key(dig$arms))]))
    lines <- vapply(unique(d$unit), function(u) {
      t_ <- sum(nn[d$unit == u & d$group == "treatment"]); c_ <- sum(nn[d$unit == u & d$group == "control"])
      sprintf("%s: %s patients treatment, %s control", u, format(t_, big.mark = ","), format(c_, big.mark = ","))
    }, "")
    tagList(div(class = "aside", paste(lines, collapse = " | ")),
            if (anyDuplicated(k)) div(class = "notice warn", "The same curve is used in more than one unit. Its patients are counted in each, so the units are not independent and the pooled confidence interval can be too narrow."))
  })
  observeEvent(input$dig_cmp_rm, {
    ir <- input$dig_tbl_cmp_rows_selected; if (!length(ir)) return()
    d <- cmp_rows()[ir, ]; hit <- paste(dig$cmp$comparison, dig$cmp$unit, dig$cmp$group, dig$cmp$study_id, dig$cmp$arm) %in% paste(d$comparison, d$unit, d$group, d$study_id, d$arm)
    dig$cmp <- dig$cmp[!hit, , drop = FALSE]
  })
  observeEvent(input$dig_cmp_del, { dig$cmp <- dig$cmp[dig$cmp$comparison != cmp_name(), , drop = FALSE] })

  # ---- Send to the analysis
  observeEvent(input$dig_use, {
    if (!nrow(dig$curve) || !nrow(dig$arms)) {
      showNotification("Save at least one curve and its number of patients first.", type = "warning"); return()
    }
    cm <- cmp_rows()
    if (nrow(cm)) {
      b <- dig_build_comparison(cm, dig$curve, dig$risk, dig$arms)
      if (!is.null(b$error)) { showNotification(b$error, type = "error", duration = 10); return() }
      for (w in b$warnings) showNotification(w, type = "warning", duration = 15)
      tabs <- b[c("curve", "risk", "arms")]
    } else {
      if (!all(dig$arms$group %in% c("treatment", "control"))) {
        showNotification("Make a comparison first (Compare groups), or give each curve a role in step 4.", type = "warning", duration = 10); return()
      }
      tabs <- lapply(setNames(c("curve", "risk", "arms"), c("curve", "risk", "arms")), function(w) dig[[w]][, KM_COLS[[w]], drop = FALSE])
    }
    tabs$curve$survival <- as.character(dig_clamp01(as.numeric(tabs$curve$survival)))
    for (w in c("curve", "risk", "arms"))
      files[[w]] <- list(data = tabs[[w]], orig = setNames(KM_COLS[[w]], KM_COLS[[w]]),
                         name = paste0("digitized_", w, ".csv"), sep = ",", enc = "UTF-8")
    updateSelectInput(session, "km_scale", selected = "proportion")
    updateSelectInput(session, "km_curve_type", selected = "survival")
    if (nzchar(trimws(input$dig_lab_tx %||% ""))) updateTextInput(session, "km_name_tx", value = txt(input$dig_lab_tx, 40))
    if (nzchar(trimws(input$dig_lab_ct %||% ""))) updateTextInput(session, "km_name_ctrl", value = txt(input$dig_lab_ct, 40))
    updateRadioButtons(session, "mode", selected = "km")
    file_msg(paste0("Points digitized in the app", if (nrow(cm)) paste0(" (comparison \"", cmp_name(), "\")") else "",
                    ". Download them from the digitizer to keep a copy; the exported R script expects those CSV files."))
  })

  # ---- Check one study against its figure: rebuild the patients and draw the rebuilt curve on the image
  observeEvent(input$dig_check, {
    pr <- pairs(); i <- suppressWarnings(as.integer(input$dig_pair))
    if (is.na(i) || i < 1 || i > nrow(pr)) return()
    study <- pr$study_id[i]
    arms <- dig$arms[dig$arms$study_id == study, , drop = FALSE]
    if (!nrow(arms)) { showNotification("Save the arm size (press Save these for the arm) before checking.", type = "warning"); return() }
    if (!km_available()) { showNotification("The packages IPDfromKM and survival are needed for this check.", type = "error"); return() }
    km_attach()
    ctype <- pick(input$dig_ctype, c("survival", "cumulative"))
    p <- km_params(list(scale = "proportion", curve_type = "survival"))   # the saved curves are always survival proportions
    env <- new.env(parent = environment(dig_server))
    env$curve <- dig$curve[dig$curve$study_id == study, , drop = FALSE]
    env$risk <- dig$risk[dig$risk$study_id == study, , drop = FALSE]
    env$arms <- arms
    err <- tryCatch({
      withProgress(message = "Rebuilding the patients", value = 0.5, {
        eval(parse(text = paste(km_prep_code(p), collapse = "\n")), env)
        eval(parse(text = km_reconstruct_code(p)), env)
      }); NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(err)) { showNotification(paste("The check could not run:", err), type = "error"); return() }
    curves <- lapply(seq_len(nrow(env$arms)), function(k) {
      d <- env$ipd[as.character(env$ipd$study_id) == env$arms$study_id[k] & env$ipd$arm == env$arms$arm[k], ]
      f <- survfit(Surv(time, status) ~ 1, data = d)
      sv <- f$surv; if (ctype == "cumulative") sv <- 1 - sv; if (dig$scale == "percent") sv <- 100 * sv
      list(arm = env$arms$arm[k], time = c(0, f$time) / dig$tmult, surv = c(if (dig$scale == "percent") 100 * (ctype == "survival") else as.numeric(ctype == "survival"), sv))
    })
    session$sendCustomMessage("dig_overlay", list(curves = curves))
    fd <- env$fidelity
    showNotification(HTML(paste0("Rebuilt curves are drawn on the figure (dashed).<br>", paste(sprintf("%s: mean difference from your points %.3f, largest %.3f.", fd$arm, fd$mean_abs_diff, fd$max_abs_diff), collapse = "<br>"))),
                     type = if (any(fd$mean_abs_diff > 0.02)) "warning" else "message", duration = 12)
  })

  # ---- Save and open a project
  output$proj_save <- downloadHandler(
    filename = function() paste0("ponderis-project-", format(Sys.Date(), "%Y%m%d"), ".json"),
    content = function(file) {
      st <- list(); for (n in PROJECT_SETTINGS) { v <- input[[paste0("km_", n)]]; if (!is.null(v) && length(v) == 1) st[[n]] <- v }
      kf <- list(curve = files$curve, risk = files$risk, arms = files$arms)
      proj_write(file, dig, kf, st)
    })
  observeEvent(input$proj_open, {
    f <- input$proj_open; req(f)
    r <- proj_read(f$datapath)
    if (!isTRUE(r$ok)) { showNotification(r$error, type = "error", duration = 8); return() }
    pj <- r$project
    for (w in c("curve", "risk", "arms")) dig[[w]] <- pj$digitizer[[w]]
    dig$scale <- pj$digitizer$scale
    dig$cmp <- pj$digitizer$cmp; dig$tmult <- 1
    for (w in names(pj$km_files)) files[[w]] <- pj$km_files[[w]]
    for (n in names(pj$settings)) session$sendInputMessage(paste0("km_", n), list(value = pj$settings[[n]]))
    if (length(pj$km_files)) file_msg(paste0("Project opened (saved ", r$saved %||% "earlier", "). The figure image is not part of the project: load it again to keep digitizing."))
    showNotification(paste0("Project opened (saved ", r$saved %||% "earlier", "). Load the figure image again to keep digitizing."), type = "message", duration = 8)
  })

  # ---- Downloads
  for (w in c("curve", "risk", "arms")) local({
    which <- w
    output[[paste0("dig_dl_", which)]] <- downloadHandler(
      filename = function() paste0("digitized_", which, ".csv"),
      content = function(file) utils::write.csv(dig[[which]], file, row.names = FALSE, na = ""))
  })

}
