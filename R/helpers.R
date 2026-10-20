# Helpers for the forest plot app: column specs, CSV reading, validation,
# and a generator that turns the user's choices into plain R code.
#
# The app RUNS the generated code and also EXPORTS it, so the downloaded
# script is exactly what produced the plot on screen.

# ---- Column specification -------------------------------------------------

SPEC <- list(
  dich = list(
    title = "Dichotomous (events and totals)",
    cols = c(events_tx = "Treatment: events", n_tx = "Treatment: total",
             events_ctrl = "Control: events", n_ctrl = "Control: total")
  ),
  cont = list(
    title = "Continuous (mean, SD, n)",
    cols = c(mean_tx = "Treatment: mean", sd_tx = "Treatment: SD", n_tx = "Treatment: n",
             mean_ctrl = "Control: mean", sd_ctrl = "Control: SD", n_ctrl = "Control: n")
  ),
  gen = list(
    title = "Pre-calculated effect (HR, OR, RR, MD...)",
    cols = c(effect = "Effect estimate", ci_lower = "CI lower limit",
             ci_upper = "CI upper limit", se = "Standard error")
  ),
  prop = list(
    title = "Single proportion (prevalence, incidence)",
    cols = c(events = "Events", n = "Total")
  )
)

TYPE_CHOICES <- setNames(names(SPEC), vapply(SPEC, function(s) s$title, ""))

SM_CHOICES <- list(
  dich = c("Risk ratio (RR)" = "RR", "Odds ratio (OR)" = "OR", "Risk difference (RD)" = "RD"),
  cont = c("Mean difference (MD)" = "MD", "Standardized mean difference (SMD, Hedges' g)" = "SMD"),
  gen  = c("Hazard ratio (HR)" = "HR", "Odds ratio (OR)" = "OR", "Risk ratio (RR)" = "RR",
           "Mean difference (MD)" = "MD", "Standardized mean difference (SMD)" = "SMD",
           "Other (difference scale)" = "Other"),
  prop = c("Logit" = "PLOGIT", "Freeman-Tukey double arcsine" = "PFT",
           "Arcsine" = "PAS", "Raw proportion" = "PRAW")
)

SM_NAMES <- c(RR = "risk ratio", OR = "odds ratio", RD = "risk difference",
              MD = "mean difference", SMD = "standardized mean difference (Hedges' g)",
              HR = "hazard ratio", Other = "effect estimate",
              PLOGIT = "proportion (logit transformation)",
              PFT = "proportion (Freeman-Tukey double arcsine transformation)",
              PAS = "proportion (arcsine transformation)", PRAW = "proportion (untransformed)")

RATIO_SM <- c("HR", "OR", "RR")

# ---- Names, aliases, auto-detection ---------------------------------------

norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

ALIASES <- list(
  study       = c("study", "studyid", "studyname", "studylabel", "author", "authors", "firstauthor", "trial", "id"),
  year        = c("year", "pubyear", "publicationyear", "ano"),
  subgroup    = c("subgroup", "group", "category", "stratum", "subgrupo", "grupo"),
  events_tx   = c("eventstx", "eventst", "eventstreatment", "eventsexp", "eventsexperimental", "eventsintervention", "evente", "events1"),
  n_tx        = c("ntx", "nt", "ntreatment", "nexp", "nexperimental", "nintervention", "ne", "n1", "totaltx", "totaltreatment"),
  events_ctrl = c("eventsctrl", "eventsc", "eventscontrol", "eventsctl", "events2"),
  n_ctrl      = c("nctrl", "nc", "ncontrol", "nctl", "n2", "totalctrl", "totalcontrol"),
  mean_tx     = c("meantx", "meant", "meantreatment", "meanexp", "meane", "mean1"),
  sd_tx       = c("sdtx", "sdt", "sdtreatment", "sdexp", "sde", "sd1"),
  mean_ctrl   = c("meanctrl", "meanc", "meancontrol", "meanctl", "mean2"),
  sd_ctrl     = c("sdctrl", "sdc", "sdcontrol", "sdctl", "sd2"),
  effect      = c("effect", "effectsize", "es", "estimate", "te", "hr", "or", "rr", "md", "smd"),
  ci_lower    = c("cilower", "lower", "lcl", "cil", "lowerci", "cilow", "ll"),
  ci_upper    = c("ciupper", "upper", "ucl", "ciu", "upperci", "ciup", "ul"),
  se          = c("se", "sem", "stderr", "standarderror", "seloghr", "selogor", "selogrr"),
  events      = c("events", "event", "cases", "x"),
  n           = c("n", "total", "samplesize", "nn")
)

has_col <- function(key, nms) {
  any(c(norm(key), ALIASES[[key]]) %in% norm(nms))
}

detect_type <- function(nms) {
  all_has <- function(keys) all(vapply(keys, has_col, TRUE, nms = nms))
  if (all_has(names(SPEC$cont$cols))) return("cont")
  if (all_has(names(SPEC$dich$cols))) return("dich")
  if (has_col("effect", nms) && (has_col("se", nms) ||
      (has_col("ci_lower", nms) && has_col("ci_upper", nms)))) return("gen")
  if (all_has(names(SPEC$prop$cols))) return("prop")
  NA_character_
}

# Best guess of the user's column for each key; never reuses a column.
guess_columns <- function(keys, nms, aliases = ALIASES) {
  used <- character(0)
  out <- setNames(rep("", length(keys)), keys)
  for (k in keys) {
    cand <- c(norm(k), aliases[[k]])
    hit <- nms[norm(nms) %in% cand & !(nms %in% used)]
    # exact standard name wins over aliases
    exact <- hit[norm(hit) == norm(k)]
    pick1 <- if (length(exact)) exact[1] else if (length(hit)) hit[1] else ""
    out[k] <- pick1
    if (nzchar(pick1)) used <- c(used, pick1)
  }
  out
}

# ---- Templates and example data -------------------------------------------

template_cols <- function(type) c("study_id", names(SPEC[[type]]$cols), "year", "subgroup")

# Header-only CSV (UTF-8 with BOM so Excel shows accents correctly).
write_template <- function(file, type, sep = ",") {
  con <- file(file, open = "wb")
  on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeBin(charToRaw(paste0(paste(template_cols(type), collapse = sep), "\r\n")), con)
}

example_data <- function(type) {
  study <- c("Trial A 2009", "Trial B 2011", "Trial C 2012", "Trial D 2014",
             "Trial E 2015", "Trial F 2017", "Trial G 2018", "Trial H 2020")
  year <- c(2009, 2011, 2012, 2014, 2015, 2017, 2018, 2020)
  grp <- c("Low risk", "Low risk", "High risk", "Low risk", "High risk", "High risk", "Low risk", "High risk")
  switch(type,
    dich = data.frame(study_id = study,
      events_tx = c(12, 25, 9, 31, 18, 40, 15, 22), n_tx = c(150, 310, 120, 400, 210, 520, 180, 260),
      events_ctrl = c(18, 33, 14, 38, 27, 52, 19, 35), n_ctrl = c(148, 305, 118, 405, 215, 515, 182, 255),
      year = year, subgroup = grp),
    cont = data.frame(study_id = study,
      mean_tx = c(42.1, 38.5, 45.0, 40.2, 36.8, 41.5, 39.9, 43.3), sd_tx = c(10.2, 9.8, 11.5, 10.0, 9.1, 10.7, 9.6, 11.0),
      n_tx = c(60, 85, 40, 120, 75, 150, 90, 110),
      mean_ctrl = c(46.0, 41.0, 47.2, 44.5, 39.0, 45.1, 42.0, 47.0), sd_ctrl = c(10.5, 10.1, 11.2, 10.4, 9.5, 10.9, 9.9, 11.3),
      n_ctrl = c(58, 88, 42, 118, 72, 148, 92, 108), year = year, subgroup = grp),
    gen = data.frame(study_id = study,
      effect = c(0.78, 0.85, 0.65, 0.92, 0.71, 0.88, 0.80, 0.74),
      ci_lower = c(0.52, 0.66, 0.38, 0.74, 0.50, 0.70, 0.58, 0.55),
      ci_upper = c(1.17, 1.10, 1.11, 1.14, 1.01, 1.11, 1.10, 0.99),
      se = NA_real_, year = year, subgroup = grp),
    prop = data.frame(study_id = study,
      events = c(14, 30, 9, 45, 22, 60, 18, 35), n = c(120, 260, 95, 310, 190, 420, 140, 250),
      year = year, subgroup = grp)
  )
}

# ---- Reading a CSV ---------------------------------------------------------

NA_STRINGS <- c("", "NA", "na", "N/A", "n/a", "NaN", ".")
MAX_ROWS <- 2000

# Returns list(data, sep, enc). Everything is read as text; numbers are converted
# later so that decimal commas (Brazilian/European Excel) work. Files saved by
# Excel as "CSV" are often Windows-1252 rather than UTF-8, so the encoding is checked.
BOM_PATTERN <- "^(\ufeff|\u00ef\\.\\.)"

read_user_csv <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) stop("The file is empty.")
  enc <- if (all(validUTF8(lines))) "UTF-8" else "latin1"
  first <- lines[1]
  counts <- c("," = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
              ";" = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))),
              "\t" = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE))))
  sep <- if (max(counts) == 0) "," else names(counts)[which.max(counts)]
  args <- list(path, sep = sep, colClasses = "character", na.strings = NA_STRINGS,
               check.names = FALSE, strip.white = TRUE, nrows = MAX_ROWS + 1,
               stringsAsFactors = FALSE)
  args$encoding <- enc  # declares the encoding without converting, so it works in any server locale
  d <- do.call(utils::read.csv, args)
  names(d) <- enc2utf8(names(d))
  d[] <- lapply(d, enc2utf8)
  names(d)[1] <- sub(BOM_PATTERN, "", names(d)[1])
  d <- d[rowSums(!is.na(d)) > 0, , drop = FALSE]  # drop fully empty rows
  if (nrow(d) > MAX_ROWS) stop(sprintf("The file has more than %d rows. Split it or remove unused rows.", MAX_ROWS))
  if (ncol(d) < 2) stop("The file has fewer than 2 columns. Check that it is a CSV with a header row.")
  list(data = d, sep = sep, enc = enc)
}

as_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))

# ---- Code-generation helpers (every interpolated value is whitelisted or deparsed) ----

q <- function(x) paste(deparse(as.character(x)[1], width.cutoff = 500L), collapse = "")
pick <- function(x, choices, default = choices[1]) {
  if (length(x) == 1 && !is.na(x) && x %in% choices) x else default
}
flag <- function(x, default = FALSE) if (length(x) == 1 && !is.na(x)) isTRUE(as.logical(x)) else default
txt <- function(x, max = 80) {
  if (length(x) != 1 || is.na(x)) return("")
  substr(gsub("[\r\n]+", " ", trimws(x)), 1, max)
}
colref <- function(name) sprintf("dat[[%s]]", q(name))
rbool <- function(x) if (isTRUE(x)) "TRUE" else "FALSE"

# Turn raw input values into a clean, whitelisted parameter list.
make_params <- function(input, type) {
  sm <- pick(input$sm, unname(SM_CHOICES[[type]]))
  method <- pick(input$method, c("MH", "Inverse", "Peto"))
  if (type == "dich" && method == "Peto" && sm != "OR") method <- "MH"
  list(
    type = type, sm = sm, method = method,
    incr = pick(input$incr, c("only0", "all")),
    model = pick(input$model, c("random", "common", "both")),
    tau = pick(input$tau, c("REML", "DL", "PM")),
    hk = flag(input$hk, TRUE), pi = flag(input$pi, FALSE),
    arm_e = txt(input$arm_e, 40), arm_c = txt(input$arm_c, 40),
    left = txt(input$label_left), right = txt(input$label_right),
    smlab = txt(input$smlab),
    sort = pick(input$sort, c("none", "year", "effect", "weight")),
    layout = pick(input$layout, c("meta", "RevMan5", "JAMA", "BMJ")),
    weights = flag(input$weights, TRUE), het = flag(input$het, TRUE),
    overall_p = flag(input$overall_p, TRUE), details = flag(input$details, FALSE),
    trimfill = flag(input$trimfill, FALSE),
    bias = pick(input$bias, c("Egger", "Begg", "Thompson"))
  )
}

# ---- Validation ------------------------------------------------------------

fmt_rows <- function(idx, labels, what, max_show = 4) {
  if (!length(idx)) return(character(0))
  shown <- idx[seq_len(min(length(idx), max_show))]
  s <- paste0("row ", shown + 1, " (", labels[shown], ")", collapse = ", ")  # +1: header row
  more <- if (length(idx) > max_show) sprintf(" and %d more", length(idx) - max_show) else ""
  paste0(what, ": ", s, more, ".")
}

# Returns list(errors, warnings, keep) where keep marks rows used in the analysis.
validate_data <- function(d, type, map, p) {
  errors <- character(0); warns <- character(0)
  need <- switch(type,
    dich = names(SPEC$dich$cols), cont = names(SPEC$cont$cols),
    prop = names(SPEC$prop$cols),
    gen = c("effect", if (nzchar(map$ci_lower %||% "") && nzchar(map$ci_upper %||% "")) c("ci_lower", "ci_upper") else "se"))
  missing_map <- need[!nzchar(unlist(map[need]) %||% "")]
  if (length(missing_map)) {
    errors <- c(errors, paste0("Match these columns first: ",
                               paste(SPEC[[type]]$cols[missing_map], collapse = ", "), "."))
    return(list(errors = errors, warnings = warns, keep = NULL, need = need))
  }
  cols <- unlist(map[need])
  if (anyDuplicated(cols)) {
    errors <- c(errors, "The same column is matched to two different fields. Choose a different column for each field.")
    return(list(errors = errors, warnings = warns, keep = NULL, need = need))
  }
  labels <- if (nzchar(map$study %||% "")) as.character(d[[map$study]]) else paste("Study", seq_len(nrow(d)))
  labels[is.na(labels) | labels == ""] <- paste("Study", which(is.na(labels) | labels == ""))
  v <- lapply(cols, function(cn) as_num(d[[cn]])); names(v) <- need

  # Text that is not a number
  for (k in need) {
    raw_na <- is.na(d[[cols[[k]]]])
    bad <- which(is.na(v[[k]]) & !raw_na)
    if (length(bad)) errors <- c(errors, fmt_rows(bad, labels, paste0(SPEC[[type]]$cols[[k]], " is not a number")))
  }
  if (length(errors)) return(list(errors = errors, warnings = warns, keep = NULL, need = need))

  miss <- Reduce(`|`, lapply(v, is.na))
  if (any(miss)) warns <- c(warns, fmt_rows(which(miss), labels, "Skipped because of missing values"))
  ok <- !miss
  chk <- function(cond, msg) { i <- which(cond & ok); if (length(i)) errors <<- c(errors, fmt_rows(i, labels, msg)); invisible() }
  wrn <- function(cond, msg) { i <- which(cond & ok); if (length(i)) warns <<- c(warns, fmt_rows(i, labels, msg)); invisible() }

  if (type == "dich") {
    chk(v$n_tx <= 0 | v$n_ctrl <= 0, "Total must be greater than 0")
    chk(v$events_tx < 0 | v$events_ctrl < 0, "Events cannot be negative")
    chk(v$events_tx > v$n_tx, "Treatment events exceed treatment total")
    chk(v$events_ctrl > v$n_ctrl, "Control events exceed control total")
    wrn(v$events_tx %% 1 != 0 | v$events_ctrl %% 1 != 0 | v$n_tx %% 1 != 0 | v$n_ctrl %% 1 != 0, "Counts are not whole numbers")
    wrn(v$events_tx == 0 & v$events_ctrl == 0, "No events in either arm (adds no information for RR/OR)")
  } else if (type == "cont") {
    chk(v$n_tx < 1 | v$n_ctrl < 1, "n must be at least 1")
    chk(v$sd_tx < 0 | v$sd_ctrl < 0, "SD cannot be negative")
    wrn(v$sd_tx == 0 | v$sd_ctrl == 0, "SD is 0")
  } else if (type == "prop") {
    chk(v$n <= 0, "Total must be greater than 0")
    chk(v$events < 0, "Events cannot be negative")
    chk(v$events > v$n, "Events exceed total")
  } else {
    ratio <- p$sm %in% RATIO_SM
    use_ci <- "ci_lower" %in% need
    if (ratio) chk(v$effect <= 0, "Ratio estimates must be greater than 0")
    if (use_ci) {
      chk(v$ci_lower > v$ci_upper, "CI lower limit is above the upper limit")
      if (ratio) chk(v$ci_lower <= 0 | v$ci_upper <= 0, "Ratio CI limits must be greater than 0")
      wrn(v$effect < v$ci_lower | v$effect > v$ci_upper, "Estimate lies outside its CI")
    } else {
      chk(v$se <= 0, "Standard error must be greater than 0")
    }
  }
  keep <- ok
  k <- sum(keep)
  if (!length(errors)) {
    if (k < 2) errors <- c(errors, "At least 2 studies with complete data are needed for a meta-analysis.")
    else if (k < 3) warns <- c(warns, "Only 2 studies: random-effects and prediction intervals are unreliable.")
    dup <- unique(labels[keep][duplicated(labels[keep])])
    if (length(dup)) warns <- c(warns, paste0("Duplicated study names: ", paste(utils::head(dup, 4), collapse = ", "), "."))
  }
  list(errors = errors, warnings = warns, keep = keep, need = need, k = k)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Code generation -------------------------------------------------------

# map: named list of the user's column names (study, year, group + required keys)
build_code <- function(p, map, need, group_col = "", year_col = "") {
  type <- p$type
  cols <- unlist(map[need])
  grp <- nzchar(group_col)
  use_year <- p$sort == "year" && nzchar(year_col)

  prepare <- c(
    "num <- function(x) suppressWarnings(as.numeric(gsub(\",\", \".\", as.character(x), fixed = TRUE)))",
    sprintf("num_cols <- c(%s)", paste(vapply(unname(cols), q, ""), collapse = ", ")),
    "dat[num_cols] <- lapply(dat[num_cols], num)",
    "dat <- dat[complete.cases(dat[num_cols]), , drop = FALSE]",
    if (nzchar(map$study %||% ""))
      sprintf("study <- trimws(as.character(%s))", colref(map$study))
    else "study <- paste(\"Study\", seq_len(nrow(dat)))",
    if (grp) c(sprintf("grp <- trimws(as.character(%s))", colref(group_col)),
               "grp[is.na(grp) | grp == \"\"] <- \"Not specified\""),
    if (use_year) sprintf("yr <- num(%s)", colref(year_col))
  )

  rnd <- p$model %in% c("random", "both")
  cmn <- p$model %in% c("common", "both")
  common_args <- c(
    sprintf("common = %s", rbool(cmn)), sprintf("random = %s", rbool(rnd)),
    sprintf("method.tau = %s", q(p$tau)),
    if (rnd && p$hk) "method.random.ci = \"HK\"",
    sprintf("prediction = %s", rbool(rnd && p$pi)),
    if (grp) c("subgroup = grp", "subgroup.name = \"\"")
  )
  arm <- c(if (nzchar(p$arm_e)) sprintf("label.e = %s", q(p$arm_e)),
           if (nzchar(p$arm_c)) sprintf("label.c = %s", q(p$arm_c)))

  model <- switch(type,
    dich = c("m <- metabin(",
      paste0("  ", c(sprintf("event.e = %s", colref(map$events_tx)), sprintf("n.e = %s", colref(map$n_tx)),
        sprintf("event.c = %s", colref(map$events_ctrl)), sprintf("n.c = %s", colref(map$n_ctrl)),
        "studlab = study", sprintf("sm = %s", q(p$sm)), sprintf("method = %s", q(p$method)),
        sprintf("method.incr = %s", q(p$incr)), "incr = 0.5", common_args, arm), collapse = ",\n"), ")"),
    cont = c("m <- metacont(",
      paste0("  ", c(sprintf("n.e = %s", colref(map$n_tx)), sprintf("mean.e = %s", colref(map$mean_tx)),
        sprintf("sd.e = %s", colref(map$sd_tx)), sprintf("n.c = %s", colref(map$n_ctrl)),
        sprintf("mean.c = %s", colref(map$mean_ctrl)), sprintf("sd.c = %s", colref(map$sd_ctrl)),
        "studlab = study", sprintf("sm = %s", q(p$sm)),
        if (p$sm == "SMD") "method.smd = \"Hedges\"", common_args, arm), collapse = ",\n"), ")"),
    prop = c("m <- metaprop(",
      paste0("  ", c(sprintf("event = %s", colref(map$events)), sprintf("n = %s", colref(map$n)),
        "studlab = study", sprintf("sm = %s", q(p$sm)), "method = \"Inverse\"", common_args), collapse = ",\n"), ")"),
    gen = {
      ratio <- p$sm %in% RATIO_SM
      use_ci <- "ci_lower" %in% need
      te <- if (ratio) sprintf("log(%s)", colref(map$effect)) else colref(map$effect)
      se <- if (use_ci) {
        if (ratio) sprintf("(log(%s) - log(%s)) / (2 * qnorm(0.975))", colref(map$ci_upper), colref(map$ci_lower))
        else sprintf("(%s - %s) / (2 * qnorm(0.975))", colref(map$ci_upper), colref(map$ci_lower))
      } else colref(map$se)
      c(sprintf("te <- %s", te), sprintf("se_te <- %s", se),
        "m <- metagen(",
        paste0("  ", c("TE = te", "seTE = se_te", "studlab = study",
          sprintf("sm = %s", q(if (p$sm == "Other") "" else p$sm)), common_args), collapse = ",\n"), ")")
    }
  )

  # Forest plot call
  wcols <- c(if (cmn) "w.common", if (rnd) "w.random")
  rightcols <- if (p$weights) c("effect", "ci", wcols) else c("effect", "ci")
  sortarg <- switch(p$sort,
    year = if (use_year) "sortvar = yr",
    effect = "sortvar = TE",
    weight = sprintf("sortvar = -%s", if (rnd) "w.random" else "w.common"),
    NULL)
  fargs <- c(
    sprintf("layout = %s", q(p$layout)),
    sortarg,
    sprintf("rightcols = c(%s)", paste(vapply(rightcols, q, ""), collapse = ", ")),
    sprintf("hetstat = %s", rbool(p$het)),
    sprintf("test.overall = %s", rbool(p$overall_p)),
    sprintf("details = %s", rbool(p$details)),
    if (grp) "sep.subgroup = \"\"",
    if (nzchar(p$left)) sprintf("label.left = %s", q(p$left)),
    if (nzchar(p$right)) sprintf("label.right = %s", q(p$right)),
    if (nzchar(p$smlab)) sprintf("smlab = %s", q(p$smlab))
  )
  forest <- c("forest(m,", paste0("  ", fargs, collapse = ",\n"), ")")
  dims <- sub("^forest\\(", "forest_dims(", forest)

  loo_pooled <- if (rnd) "random" else "common"
  loo_args <- c("details = FALSE",
                if (nzchar(p$left)) sprintf("label.left = %s", q(p$left)),
                if (nzchar(p$right)) sprintf("label.right = %s", q(p$right)))
  loo <- c(sprintf("inf <- metainf(m, pooled = %s)", q(loo_pooled)),
           sprintf("forest(inf, %s)", paste(loo_args, collapse = ", ")))
  loo_dims <- sub("forest\\(inf", "forest_dims(inf", loo[2])

  funnel <- if (p$trimfill) c("tf <- trimfill(m)", "funnel(tf)") else "funnel(m)"
  bias <- sprintf("metabias(m, method.bias = %s, k.min = 3)", q(p$bias))

  list(prepare = paste(unlist(prepare), collapse = "\n"),
       model = paste(model, collapse = "\n"),
       forest = paste(forest, collapse = "\n"),
       dims = paste(dims, collapse = "\n"),
       loo = paste(loo, collapse = "\n"),
       loo_dims = paste(c(loo[1], loo_dims), collapse = "\n"),
       funnel = paste(funnel, collapse = "\n"),
       bias = bias)
}

# Reproducible script the user can download.
build_script <- function(code, p, csv_name, sep, enc, width_in, height_in) {
  stamp <- format(Sys.Date(), "%Y-%m-%d")
  c(
    sprintf("# Meta-analysis forest plot, generated by the app on %s", stamp),
    "# Requires R and the 'meta' package (version 8.0 or later): install.packages(\"meta\")",
    "# Put this script and your CSV in the same folder, then run it.",
    "",
    "library(meta)",
    "",
    "# 1. Read the data (everything is read as text; numbers are converted below,",
    "#    so decimal commas from Excel work).",
    sprintf("dat <- read.csv(%s, sep = %s, colClasses = \"character\",", q(csv_name), q(sep)),
    sprintf("                na.strings = c(%s),", paste(vapply(NA_STRINGS, q, ""), collapse = ", ")),
    sprintf("                check.names = FALSE, strip.white = TRUE, %s)",
            sprintf("encoding = %s", q(enc))),
    "names(dat)[1] <- sub(\"^(\\ufeff|\\u00ef\\\\.\\\\.)\", \"\", names(dat)[1])  # remove Excel's hidden marker",
    "",
    "# 2. Prepare the data",
    code$prepare,
    "",
    "# 3. Meta-analysis",
    code$model,
    "",
    "# 4. Print the results",
    "print(m)",
    "",
    "# 5. Forest plot (shown in the R graphics window)",
    code$forest,
    "",
    "# 6. Save the forest plot as PNG, PDF and SVG (publication size)",
    sprintf("png(\"forest_plot.png\", width = %s, height = %s, units = \"in\", res = 300)", width_in, height_in),
    code$forest, "dev.off()",
    sprintf("pdf(\"forest_plot.pdf\", width = %s, height = %s)", width_in, height_in),
    code$forest, "dev.off()",
    "# SVG: svglite if installed (base svg() needs Cairo, which macOS R gets from XQuartz)",
    svg_device_code("forest_plot.svg", width_in, height_in),
    code$forest, "dev.off()",
    "",
    "# 7. Optional: sensitivity and publication bias",
    "# Leave-one-out analysis",
    paste0("# ", strsplit(code$loo, "\n")[[1]]),
    "# Funnel plot and bias test (needs about 10 or more studies)",
    paste0("# ", strsplit(code$funnel, "\n")[[1]]),
    paste0("# ", code$bias),
    "",
    script_versions_code("meta")
  )
}

# Line that opens an SVG device in an exported script: svglite when installed,
# otherwise base svg() (which silently writes nothing when Cairo cannot load)
svg_device_code <- function(file, width, height) sprintf(
  "if (requireNamespace(\"svglite\", quietly = TRUE)) svglite::svglite(\"%1$s\", width = %2$s, height = %3$s) else svg(\"%1$s\", width = %2$s, height = %3$s)",
  file, width, height)

# Lines that record the R and package versions used, at the end of an exported script
script_versions_code <- function(pkgs) c(
  "# Versions used (report these in the methods; save them with your results)",
  sprintf("cat(\"R\", paste(R.version$major, R.version$minor, sep = \".\"), \"\\n\")"),
  sprintf("for (pkg in c(%s)) if (requireNamespace(pkg, quietly = TRUE)) cat(pkg, as.character(packageVersion(pkg)), \"\\n\")", paste0("\"", pkgs, "\"", collapse = ", ")),
  "sessionInfo()")

# Plot height in inches, from the number of studies and subgroups.
plot_height_in <- function(k, n_sub = 0, pi = FALSE, het = TRUE, details = FALSE, hdr = 1.1) {
  rows <- k + 7 + 3 * n_sub + (if (pi) 1 else 0) + (if (het) 1 else 0) + (if (details) 9 else 0)
  max(3.4, round(hdr + 0.27 * rows, 1))
}

methods_text <- function(p, k, ai = FALSE) {
  rnd <- p$model %in% c("random", "both"); cmn <- p$model %in% c("common", "both")
  what <- SM_NAMES[[p$sm]]
  mod <- if (rnd && cmn) "common-effect and random-effects models"
         else if (rnd) "a random-effects model" else "a common-effect model"
  est <- if (rnd) sprintf(" The between-study variance was estimated with the %s estimator%s.",
                          c(REML = "restricted maximum likelihood (REML)", DL = "DerSimonian-Laird",
                            PM = "Paule-Mandel")[[p$tau]],
                          if (p$hk) " and confidence intervals used the Hartung-Knapp adjustment" else "") else ""
  meth <- if (p$type == "dich" && cmn) sprintf(" The common-effect estimate used the %s method.",
                                              c(MH = "Mantel-Haenszel", Inverse = "inverse-variance", Peto = "Peto")[[p$method]]) else ""
  paste0("We pooled ", k, " studies using ", mod, ", with the ", what, " as the effect measure.", est, meth,
         " Heterogeneity was assessed with the I\u00b2 statistic and \u03c4\u00b2",
         if (rnd && p$pi) ", and a prediction interval is reported" else "",
         ". Analyses were performed in R (version ",
         paste(R.version$major, R.version$minor, sep = "."), ") with the meta package (version ",
         as.character(utils::packageVersion("meta")), ").",
         if (isTRUE(ai)) " Data were extracted with the help of a large language model (Claude, Anthropic) and checked by the authors against the source articles." else "",
         " Adapt this paragraph to your protocol before use.")
}
