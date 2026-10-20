# Teaching panels ("How to read this") and the Code tab of the forest-plot area.
#
# howto_box() is a collapsible panel placed above a figure: the reading of the user's own numbers on top, then a short guide to the
# elements of the figure. The reading_* functions turn the fitted model into plain sentences and never print a conclusion the numbers
# do not support.

# ---- Panel ----------------------------------------------------------------

howto_box <- function(key, guide, reading = NULL) {
  tags$details(class = "howto", `data-key` = key,
    tags$summary(tags$span(class = "howto-mark", "?"), "How to read this"),
    div(class = "howto-body",
      if (!is.null(reading)) div(class = "howto-reading", div(class = "howto-label", "Your result"), reading),
      div(class = "howto-guide", div(class = "howto-label", "What you are looking at"), guide)))
}

# term/definition list used by the guides
howto_list <- function(...) {
  items <- list(...)
  tags$ul(class = "howto-list", lapply(items, function(x) tags$li(tags$b(x[[1]]), " ", x[[2]])))
}

howto_text <- function(x) tagList(lapply(x[nzchar(x)], function(s) tags$p(enc2utf8(s))))

# ---- Guides (static text) ----------------------------------------------------

guide_forest <- function(sm = "RR") {
  prop <- sm %in% c("PLOGIT", "PFT", "PAS", "PRAW")
  ratio <- sm %in% RATIO_SM
  null <- if (ratio) "1" else "0"
  howto_list(
    list("Each row is one study.", "The square is its estimate. The bigger the square, the more weight the study has in the pooled result. The horizontal line is its 95% confidence interval."),
    if (!prop) list("The vertical line is \"no effect\".", sprintf("It sits at %s for this measure. A confidence interval that touches or crosses the line means that the study alone is compatible with no effect.", null))
    else list("There is no \"no effect\" line.", "A single proportion has no null value. Read the diamond as the pooled percentage and compare the studies with it."),
    list("The diamond is the pooled result.", "Its centre is the estimate and its width is the 95% confidence interval. A narrow diamond means a more precise result."),
    if (!prop) list("Left and right.", "Which side is better depends on the outcome. The labels under the axis say what each side favours (for a harmful event such as death, the left side favours the treatment)."),
    list("I², τ² and the Q test.", "They measure how much the studies disagree. I² is the share of the variation that comes from real differences and not chance (about 25% is low, 50% moderate and 75% high). τ² is the variance of the true effects. A Q test with p below 0.10 suggests heterogeneity, but it has little power with few studies."),
    list("Prediction interval (if shown).", "The range where the effect of a new, similar study is expected to fall. It is wider than the confidence interval when the studies differ."))
}

guide_loo <- function() howto_list(
  list("Each row is the pooled result after removing that one study.", "The row name says which study was left out."),
  list("What you want to see.", "All rows close to each other and on the same side of the line of no effect. Then no single study decides the conclusion, and the result is robust."),
  list("What to worry about.", "A row that moves far from the others, or whose interval crosses the line of no effect. The conclusion then depends on that study. Report it and discuss why that study is different (population, design, risk of bias)."))

guide_influence <- function() howto_list(
  list("Baujat plot.", "Each point is a study. Horizontal: how much it adds to the heterogeneity. Vertical: how much it pulls the pooled result. Studies in the top right matter most for both."),
  list("Galbraith (radial) plot.", "Studies are placed by precision (horizontal) and standardized effect (vertical). The line is the pooled estimate and the band is ±2. Points outside the band differ from the others by more than chance would explain."),
  list("Influence diagnostics.", "The studentized residual flags outliers (beyond about ±2 to 3). Cook's distance and the hat value show how strongly a study moves the model. The last columns show the heterogeneity left when that study is removed."),
  list("What to do.", "A flagged study is not an error to delete. Look for a reason, and report the result with and without it."))

guide_funnel <- function() howto_list(
  list("Each point is a study.", "Horizontal: its effect. Vertical: its standard error, so large and precise studies are at the top."),
  list("The funnel.", "The central line is the pooled effect and the dashed lines show where 95% of studies are expected to fall by chance. Without bias the points spread evenly on both sides, like an upside-down funnel."),
  list("Asymmetry.", "A gap in one lower corner (small studies that are missing on one side) suggests small-study effects. Publication bias is one cause, but heterogeneity, differences in population and chance can also cause it."),
  list("The bias test.", "A p value below 0.05 suggests asymmetry. The tests need about 10 studies or more, and a non-significant result with few studies does not rule out bias."),
  list("Trim-and-fill.", "Estimates which studies are missing and what the pooled effect would be with them. Use it as a sensitivity check, not as the main result."))

guide_metareg <- function() howto_list(
  list("Each circle is a study.", "Its size is its weight. The line is the effect predicted for each value of the covariate and the band is its 95% confidence interval."),
  list("The slope.", "How much the effect changes for each 1-unit increase of the covariate. For ratio measures the table also gives it as a ratio. For groups, it is the difference between a group and the reference group."),
  list("The test of moderators.", "A p value below 0.05 means that the covariate is associated with the effect. R² is the share of the between-study variance that it explains."),
  list("Cautions.", "Needs about 10 studies per covariate. The relation is between studies, not between patients, so it does not prove cause. Testing many covariates produces false positives."))

# ---- Readings of the user's own numbers ----------------------------------------

.hw_n <- function(x, d = 2) formatC(x, digits = d, format = "f")
.hw_p <- function(p) if (is.na(p)) "n/a" else if (p < 0.001) "< 0.001" else paste("=", formatC(p, digits = 3, format = "f"))
.hw_bt <- function(m, v) tryCatch(meta::backtransf(v, sm = m$sm, n = m$n.harmonic.mean), error = function(e) v)

reading_forest <- function(m, p) {
  rnd <- p$model %in% c("random", "both")
  te <- if (rnd) m$TE.random else m$TE.common
  lo <- if (rnd) m$lower.random else m$lower.common
  hi <- if (rnd) m$upper.random else m$upper.common
  est <- .hw_bt(m, te); l <- .hw_bt(m, lo); h <- .hw_bt(m, hi)
  sm <- m$sm; k <- m$k
  model <- if (rnd) "random-effects" else "common-effect"
  out <- character(0)

  if (sm %in% c("PLOGIT", "PFT", "PAS", "PRAW")) {
    out <- c(out, sprintf("The diamond says that about %s%% of patients had the outcome (95%% CI %s%% to %s%%), which is roughly %.0f in 100. A wide interval means that the studies are too few or too different to give a precise percentage.",
                          .hw_n(100 * est, 1), .hw_n(100 * l, 1), .hw_n(100 * h, 1), 100 * est))
  } else {
    ratio <- sm %in% RATIO_SM
    null <- if (ratio) 1 else 0
    sig <- l > null || h < null
    shown <- if (nzchar(sm)) sm else "effect"
    what <- if (ratio) c(RR = "risk", OR = "odds", HR = "hazard")[[sm]] else NULL
    if (sig) {
      below <- h < null
      size <- if (ratio) sprintf("a %s%% %s %s", .hw_n(100 * abs(1 - est), 0), if (below) "lower" else "higher", what)
              else if (sm == "RD") sprintf("%s percentage points %s events per 100 patients", .hw_n(100 * abs(est), 1), if (below) "fewer" else "more")
              else if (sm == "SMD") sprintf("a mean that was %s standard deviations %s", .hw_n(abs(est)), if (below) "lower" else "higher")
              else if (sm == "MD") sprintf("a mean that was %s units %s", .hw_n(abs(est)), if (below) "lower" else "higher")
              else sprintf("an estimate that was %s %s", .hw_n(abs(est)), if (below) "lower" else "higher")
      out <- c(out, sprintf("In the %s model the diamond is to the %s of the line at %s and its 95%% CI (%s to %s) does not touch the line. The first group (treatment) had %s than the comparison group, and the result is statistically significant. Whether this is good or bad depends on the outcome.",
                            model, if (below) "left" else "right", null, .hw_n(l), .hw_n(h), size))
    } else {
      out <- c(out, sprintf("In the %s model the diamond crosses the line at %s: the estimate is %s (95%% CI %s to %s). The data are compatible with no difference, and also with a difference in either direction within that interval. This does not prove that the groups are equal, because a few small studies can lack the power to show a real effect.",
                            model, null, .hw_n(est), .hw_n(l), .hw_n(h)))
    }
  }

  if (!is.na(m$I2)) {
    lvl <- if (m$I2 < 0.25) "low" else if (m$I2 < 0.5) "moderate" else if (m$I2 < 0.75) "substantial" else "considerable"
    out <- c(out, sprintf("Heterogeneity is %s (I² = %.0f%%, Q test p %s). %s", lvl, 100 * m$I2, .hw_p(m$pval.Q),
      if (m$I2 >= 0.5) "The studies differ by more than chance, so read the diamond as an average of different effects. Look at the prediction interval, the leave-one-out and influence tabs, and consider a meta-regression."
      else "The studies agree reasonably well."))
  }
  if (rnd && isTRUE(p$pi) && !is.null(m$lower.predict) && is.finite(m$lower.predict) && !(sm %in% c("PLOGIT", "PFT", "PAS", "PRAW"))) {
    pl <- .hw_bt(m, m$lower.predict); ph <- .hw_bt(m, m$upper.predict); null <- if (sm %in% RATIO_SM) 1 else 0
    out <- c(out, sprintf("The prediction interval is %s to %s. %s", .hw_n(pl), .hw_n(ph),
      if (pl > null || ph < null) "A new study is expected to find an effect in the same direction."
      else "It crosses the line of no effect, so a new study could find no effect or an effect in the opposite direction."))
  }
  if (k < 5) out <- c(out, sprintf("Only %d studies: treat all of this with caution.", k))
  out
}

# Per-study results with each study left out. Returns NULL when meta cannot compute it.
loo_table <- function(m, rnd) {
  inf <- tryCatch(suppressWarnings(meta::metainf(m, pooled = if (rnd) "random" else "common")), error = function(e) NULL)
  if (is.null(inf)) return(NULL)
  i <- grepl("^Omitting ", inf$studlab)
  if (!any(i)) return(NULL)
  data.frame(study = sub("^Omitting ", "", inf$studlab[i]), te = inf$TE[i], lo = inf$lower[i], hi = inf$upper[i], stringsAsFactors = FALSE)
}

reading_loo <- function(m, p) {
  rnd <- p$model %in% c("random", "both")
  tb <- loo_table(m, rnd); if (is.null(tb) || !nrow(tb)) return(character(0))
  te0 <- if (rnd) m$TE.random else m$TE.common
  lo0 <- if (rnd) m$lower.random else m$lower.common
  hi0 <- if (rnd) m$upper.random else m$upper.common
  prop <- m$sm %in% c("PLOGIT", "PFT", "PAS", "PRAW")
  f <- function(v) .hw_n(.hw_bt(m, v), if (prop) 3 else 2)
  # on the analysis scale the line of no effect is 0 for every measure (ratios are on the log scale)
  sig <- function(lo, hi) lo > 0 | hi < 0
  rng <- range(tb$te)
  out <- character(0)
  if (!prop) {
    flip <- which(sig(tb$lo, tb$hi) != sig(lo0, hi0))
    if (length(flip)) {
      out <- c(out, sprintf("The conclusion changes when you remove %s: the pooled result %s. The finding depends on %s, so say it in the text and discuss why %s is different.",
                            paste(tb$study[flip], collapse = ", "), if (sig(lo0, hi0)) "stops being significant" else "becomes significant",
                            if (length(flip) > 1) "these studies" else "that study", if (length(flip) > 1) "they are" else "it is"))
    } else {
      out <- c(out, sprintf("No single removal changes the conclusion: the result stays %s in every row.", if (sig(lo0, hi0)) "statistically significant" else "not significant"))
    }
  }
  j <- which.max(abs(tb$te - te0))
  out <- c(out, sprintf("The estimate stays between %s and %s (all studies: %s). The biggest change comes from removing %s, which gives %s (95%% CI %s to %s).",
                        f(rng[1]), f(rng[2]), f(te0), tb$study[j], f(tb$te[j]), f(tb$lo[j]), f(tb$hi[j])))
  out
}

reading_influence <- function(r) {
  if (!isTRUE(r$ok)) return(character(0))
  tb <- r$table; out <- infl_note(r)
  d <- tb$I2_without - r$fit$I2
  if (any(is.finite(d)) && min(d, na.rm = TRUE) < -10) {
    j <- which.min(d)
    out <- c(out, sprintf("Without %s, I² falls from %.0f%% to %.0f%%, so this study explains much of the heterogeneity.", tb$study[j], r$fit$I2, tb$I2_without[j]))
  }
  out
}

reading_funnel <- function(m, p) {
  k <- m$k
  out <- character(0)
  if (k < 10) out <- c(out, sprintf("With only %d studies, the funnel plot is hard to judge and the test has little power. A non-significant result does not rule out bias.", k))
  mb <- tryCatch(meta::metabias(m, method.bias = p$bias, k.min = 3), error = function(e) NULL)
  pv <- if (is.null(mb)) NA_real_ else suppressWarnings(as.numeric((mb$pval %||% mb$p.value %||% NA)[1]))
  if (length(pv) == 1 && is.finite(pv)) {
    nm <- c(Egger = "Egger", Begg = "Begg", Thompson = "Thompson-Sharp")[[p$bias]]
    out <- c(out, if (pv < 0.05)
      sprintf("The %s test is significant (p %s): the plot is asymmetric, so small studies show different results from large ones. Publication bias is one explanation, but heterogeneity or chance can also cause it.", nm, .hw_p(pv))
      else sprintf("The %s test does not show asymmetry (p %s). This is not proof that there is no bias.", nm, .hw_p(pv)))
  }
  if (isTRUE(p$trimfill)) {
    tf <- tryCatch(suppressWarnings(meta::trimfill(m)), error = function(e) NULL)
    if (!is.null(tf)) {
      rnd <- p$model %in% c("random", "both")
      te <- if (rnd) tf$TE.random else tf$TE.common
      te0 <- if (rnd) m$TE.random else m$TE.common
      out <- c(out, if (tf$k0 == 0) "Trim-and-fill did not find missing studies."
        else sprintf("Trim-and-fill added %d missing studies and moves the estimate from %s to %s.", tf$k0, .hw_n(.hw_bt(m, te0)), .hw_n(.hw_bt(m, te))))
    }
  }
  out
}

reading_metareg <- function(r) {
  if (!isTRUE(r$ok)) return(character(0))
  t <- r$table
  out <- sprintf("The test of moderators gives p %s, so the covariate %s associated with the effect.", .hw_p(r$qm_p), if (isTRUE(r$qm_p < 0.05)) "is" else "is not clearly")
  if (r$kind == "number" && nrow(t) >= 2) {
    s <- if (isTRUE(r$ratio)) sprintf("For each 1-unit increase in %s the effect is multiplied by %s (95%% CI %s to %s), which is about %s%% %s per unit.",
                                      r$label, .hw_n(t$ratio[2], 3), .hw_n(t$ratio_lower[2], 3), .hw_n(t$ratio_upper[2], 3), .hw_n(100 * abs(t$ratio[2] - 1), 1), if (t$ratio[2] < 1) "lower" else "higher")
         else sprintf("For each 1-unit increase in %s the effect changes by %s (95%% CI %s to %s).", r$label, .hw_n(t$estimate[2], 3), .hw_n(t$ci_lower[2], 3), .hw_n(t$ci_upper[2], 3))
    out <- c(out, s)
  }
  if (is.finite(r$r2)) out <- c(out, sprintf("It explains about %.0f%% of the variance between studies.", r$r2))
  if (r$k < 10) out <- c(out, sprintf("Only %d studies: exploratory.", r$k))
  out
}

# ---- Code tab ------------------------------------------------------------------

code_copy_js <- paste0("var b=this; var t=b.closest('.code-block').querySelector('pre').innerText;",
  "navigator.clipboard.writeText(t).then(function(){b.textContent='Copied'; setTimeout(function(){b.textContent='Copy'},1500);});")

code_block <- function(title, id, note = NULL, extra = NULL) {
  div(class = "code-block",
    div(class = "code-head", span(class = "code-title", title),
      div(class = "code-actions", extra, tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = code_copy_js, "Copy"))),
    if (!is.null(note)) div(class = "aside", note),
    verbatimTextOutput(id))
}

code_tab_ui <- function() {
  nav_panel("Code", value = "code",
    p(class = "aside", "Some journals ask for the code or the command behind the analysis. Everything here is exactly what produced your results, so you can paste it into the methods or the supplement."),
    code_block("1. Analysis command", "code_analysis",
      "Reads your columns and runs the meta-analysis. This is the part journals usually ask for."),
    code_block("2. Forest plot command", "code_forest_cmd"),
    code_block("3. Sensitivity analyses and publication bias", "code_sens",
      "Leave-one-out, funnel plot and bias test, with the settings chosen in this app. Meta-regression and influence have their own code under their tabs."),
    h3("Software"),
    uiOutput("code_software"),
    code_block("4. Complete script", "code_full",
      "The whole workflow in one file: reading the CSV, analysis, plots saved at publication size, and the versions used.",
      extra = downloadButton("dl_script2", "Download .R", class = "btn-primary btn-sm"))
  )
}

software_versions <- function() {
  v <- function(pkg) if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else NA_character_
  list(r = paste(R.version$major, R.version$minor, sep = "."), r_year = R.version$year, meta = v("meta"), metafor = v("metafor"))
}

software_sentence <- function(sv = software_versions(), metafor = FALSE) {
  paste0("All analyses were performed in R (version ", sv$r, ") using the meta package (version ", sv$meta, ")",
         if (metafor && !is.na(sv$metafor)) paste0(" and the metafor package (version ", sv$metafor, ") for meta-regression and influence diagnostics") else "", ".")
}

code_server <- function(input, output, session, fit) {
  lines <- function(x) paste(x, collapse = "\n")
  output$code_analysis <- renderText({ f <- fit(); req(isTRUE(f$ok)); lines(c("library(meta)", "", f$code$prepare, "", f$code$model, "", "print(m)")) })
  output$code_forest_cmd <- renderText({ f <- fit(); req(isTRUE(f$ok)); f$code$forest })
  output$code_sens <- renderText({ f <- fit(); req(isTRUE(f$ok)); lines(c("# Leave-one-out", f$code$loo, "", "# Funnel plot", f$code$funnel, "", "# Test for funnel plot asymmetry (needs about 10 or more studies)", f$code$bias)) })
  output$code_full <- renderText({
    f <- fit(); req(isTRUE(f$ok))
    nm <- if (is.null(f$csv) || !grepl("\\.(csv|txt)$", f$csv, ignore.case = TRUE)) "your_data.csv" else f$csv
    lines(build_script(f$code, f$p, nm, f$sep, f$enc, f$w, f$h))
  })
  output$code_software <- renderUI({
    f <- fit(); req(isTRUE(f$ok)); sv <- software_versions()
    sen <- software_sentence(sv, metafor = TRUE)
    tagList(
      div(class = "aside", "These are the versions of R and of the packages running this app. If you run the script on another computer, check them with the last lines of the script."),
      tags$table(class = "table table-sm km-table",
        tags$thead(tags$tr(tags$th("Software"), tags$th("Version"), tags$th("Used for"))),
        tags$tbody(
          tags$tr(tags$td("R"), tags$td(sv$r), tags$td("Everything")),
          tags$tr(tags$td("meta"), tags$td(sv$meta), tags$td("Pooling, forest plot, leave-one-out, funnel plot, bias tests")),
          tags$tr(tags$td("metafor"), tags$td(if (is.na(sv$metafor)) "not installed" else sv$metafor), tags$td("Meta-regression, influence diagnostics")))),
      div(class = "report-block",
        div(class = "report-head", tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = copy_js, "Copy sentence")),
        div(class = "report-text", tags$p(sen))),
      tags$details(class = "mapping", tags$summary("References to cite"),
        tags$ul(
          tags$li(sprintf("R Core Team (%s). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/", sv$r_year)),
          tags$li("Balduzzi S, Rücker G, Schwarzer G (2019). How to perform a meta-analysis with R: a practical tutorial. Evidence-Based Mental Health 22:153-160."),
          tags$li("Viechtbauer W (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software 36(3):1-48."))))
  })
  output$dl_script2 <- downloadHandler("forest_plot_analysis.R", function(file) {
    f <- fit(); req(isTRUE(f$ok))
    nm <- if (is.null(f$csv) || !grepl("\\.(csv|txt)$", f$csv, ignore.case = TRUE)) "your_data.csv" else f$csv
    writeLines(build_script(f$code, f$p, nm, f$sep, f$enc, f$w, f$h), file, useBytes = TRUE)
  })

  # readings
  rd <- function(fun) function() { f <- fit(); req(isTRUE(f$ok)); howto_text(tryCatch(fun(f), error = function(e) { message("howto reading failed: ", conditionMessage(e), " in ", paste(deparse(conditionCall(e)), collapse = " ")); character(0) })) }
  output$hw_forest <- renderUI(rd(function(f) reading_forest(f$env$m, f$p))())
  output$hw_loo <- renderUI(rd(function(f) reading_loo(f$env$m, f$p))())
  output$hw_funnel <- renderUI(rd(function(f) reading_funnel(f$env$m, f$p))())
}
