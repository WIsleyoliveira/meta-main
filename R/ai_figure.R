# Reading a Kaplan-Meier figure with the AI: what is printed on it (axes, legend, numbers-at-risk table, hazard ratio)
# as schema-validated JSON, and plain checks of that reading. The AI never traces curves; the digitizer does.
# Plain functions, no Shiny.

AI_MAX_IMAGE_BYTES <- 5 * 1024^2   # API limit per image

ai_image_block <- function(b64, media_type = "image/png")
  list(type = "image", source = list(type = "base64", media_type = media_type, data = gsub("[\r\n]", "", b64)))

ai_figure_schema <- function() {
  obj <- function(props) list(type = "object", additionalProperties = FALSE, required = as.list(names(props)), properties = props)
  nullable <- function(type) list(anyOf = list(list(type = type), list(type = "null")))
  str <- list(type = "string")
  axis <- obj(list(label = str, scale = list(type = "string", enum = list("linear", "log")), unit = str,
                   ticks = list(type = "array", items = obj(list(value = list(type = "number"), px = list(type = "number"), py = list(type = "number"))))))
  obj(list(
    axes = obj(list(x = axis, y = axis, y_percent = list(type = "boolean"),
                    curve_type = list(type = "string", enum = list("survival", "cumulative")))),
    arms = list(type = "array", items = obj(list(label = str, colour_hex = nullable("string"),
                                                 line_style = list(type = "string", enum = list("solid", "dashed", "dotted", "other")),
                                                 risk_label = nullable("string")))),
    risk_table = list(anyOf = list(obj(list(times = list(type = "array", items = list(type = "number")),
                                            rows = list(type = "array", items = obj(list(label = str, n = list(type = "array", items = nullable("integer"))))))),
                                   list(type = "null"))),
    annotations = obj(list(hr = nullable("number"), ci_lower = nullable("number"), ci_upper = nullable("number"),
                           p = nullable("string"), hr_text = str, reference_arm = nullable("string"))),
    notes = list(type = "array", items = str)))
}

ai_figure_system <- function() paste0(
  "You read a Kaplan-Meier figure from a published study for a systematic reviewer who will digitize its curves.\n",
  "Report only what is printed on the figure. Never estimate, compute or guess a number that is not printed: use null instead.\n\n",
  "Axes: for each axis give its label, scale (linear or log), unit, and at least two printed tick values with the position of each tick ",
  "in pixels of the image you were sent (origin top-left, x to the right, y down). Use the centre of the tick mark where it meets the axis line; ",
  "if there are no tick marks, the centre of the tick label projected onto the axis line. Prefer the first and last ticks. ",
  "y_percent is true when the Y axis runs 0 to 100 (percent) rather than 0 to 1. curve_type is survival for falling curves, cumulative for rising ones.\n",
  "Arms: one entry per curve in the legend, in legend order: label as printed, the curve's colour as #RRGGBB (null if you cannot tell), line style, ",
  "and the label of its row in the numbers-at-risk table (null if none).\n",
  "Numbers at risk: the times printed above the table and, for each row, the numbers exactly as printed (null for a dash or an empty cell). ",
  "risk_table is null when the figure has no table.\n",
  "Annotations: a hazard ratio printed on the figure with its confidence interval and p value, the text verbatim, and the arm used as reference ",
  "(the comparator) when the text says so; nulls when absent.\n",
  "Notes: anything the reviewer should check (overlapping curves, cropped axes, several panels, unreadable numbers).")

ai_figure_user_text <- function(width, height, study = "")
  paste0("Read this figure. The image is ", width, " x ", height, " pixels.",
         if (nzchar(trimws(study %||% ""))) paste0(" It is from the study: ", trimws(study), ".") else "")

ai_figure_read <- function(b64, width, height, study, key, model, transport = ai_http_transport) {
  if (length(key) != 1 || is.na(key) || !nzchar(key)) return(c(ai_fail("no_key"), list(called = FALSE)))
  if (nchar(b64) * 3 / 4 > AI_MAX_IMAGE_BYTES) return(c(ai_fail("too_large"), list(called = FALSE)))
  content <- list(ai_image_block(b64), ai_text_block(ai_figure_user_text(width, height, study)))
  r <- ai_call(key, model, ai_figure_system(), content, ai_figure_schema(), transport = transport)
  if (!isTRUE(r$ok)) return(c(r, list(called = TRUE)))
  list(ok = TRUE, reading = r$data, warnings = ai_figure_check(r$data), usage = r$usage, called = TRUE, error = NULL, message = NULL)
}

# Plain checks of a reading; one warning per problem.
ai_figure_check <- function(m) {
  out <- character(0); add <- function(...) out <<- c(out, sprintf(...))
  num <- function(x) vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)
  for (ax in c("x", "y")) {
    a <- m$axes[[ax]]; nm <- toupper(ax)
    tk <- a$ticks %||% list()
    v <- num(lapply(tk, `[[`, "value")); p <- num(lapply(tk, `[[`, if (ax == "x") "px" else "py"))
    if (length(unique(v[!is.na(v)])) < 2) { add("The %s axis has fewer than two ticks with different values: set its calibration by hand.", nm); next }
    if (identical(a$scale, "log") && any(v <= 0, na.rm = TRUE)) add("The %s axis is read as log but has a tick at or below 0.", nm)
    o <- order(v); d <- diff(p[o])
    if (any(if (ax == "x") d <= 0 else d >= 0, na.rm = TRUE)) add("The %s axis tick positions do not follow their values: check the calibration.", nm)
  }
  rt <- m$risk_table
  if (!is.null(rt)) {
    tm <- num(rt$times %||% list())
    if (length(tm) && any(diff(tm) <= 0, na.rm = TRUE)) add("The times of the risk table are not increasing.")
    if (length(tm) && !isTRUE(tm[1] == 0)) add("The risk table does not start at time 0.")
    labels <- tolower(trimws(c(vapply(m$arms %||% list(), function(a) a$label %||% "", ""),
                               vapply(m$arms %||% list(), function(a) a$risk_label %||% "", ""))))
    for (r in rt$rows %||% list()) {
      n <- num(r$n %||% list())
      if (length(n) != length(tm)) add("Row '%s' has %d numbers for %d times.", r$label, length(n), length(tm))
      if (anyNA(n)) add("Row '%s': no number at time %s (printed as a dash or blank).", r$label, paste(tm[which(is.na(n))], collapse = ", "))
      nn <- n[!is.na(n)]
      if (length(nn) > 1 && any(diff(nn) > 0)) add("Row '%s': the numbers at risk increase over time.", r$label)
      if (!tolower(trimws(r$label)) %in% labels) add("Risk table row '%s' matches no arm in the legend.", r$label)
    }
  }
  for (a in m$arms %||% list()) if (is.null(a$colour_hex)) add("Arm '%s' has no colour: pick it on the figure.", a$label)
  an <- m$annotations
  if (!is.null(an$hr) && !is.null(an$ci_lower) && !is.null(an$ci_upper) && (an$hr < an$ci_lower || an$hr > an$ci_upper))
    add("The HR %s is outside its confidence interval (%s to %s).", an$hr, an$ci_lower, an$ci_upper)
  out
}

# Tick positions read on the image sent, scaled back to the original image
ai_figure_scale <- function(m, factor) {
  for (ax in c("x", "y")) m$axes[[ax]]$ticks <- lapply(m$axes[[ax]]$ticks %||% list(), function(t) { t$px <- t$px * factor; t$py <- t$py * factor; t })
  m
}

# The calibration to apply: scales, curve type, and the two ticks farthest apart on each axis (most precise).
# A percent axis whose ticks were read as 0-1 is put back on 0-100, as the digitizer reads Y as printed.
ai_figure_axes <- function(m) {
  far2 <- function(tk) {
    v <- vapply(tk, function(t) as.numeric(t$value), 0)
    if (length(v) < 2) return(tk)
    tk[c(which.min(v), which.max(v))]
  }
  y_all <- m$axes$y$ticks %||% list()
  if (isTRUE(m$axes$y_percent) && length(y_all) && max(vapply(y_all, function(t) as.numeric(t$value), 0)) <= 1)
    y_all <- lapply(y_all, function(t) { t$value <- t$value * 100; t })
  ct <- m$axes$curve_type %||% "survival"
  # all ticks go to the browser, which snaps them and checks they line up (x, y: the two farthest, kept for the record)
  list(xscale = m$axes$x$scale %||% "linear", yscale = m$axes$y$scale %||% "linear", y_percent = isTRUE(m$axes$y_percent),
       curve_type = ct, start = if (identical(ct, "cumulative")) "bottom" else "top",
       x = far2(m$axes$x$ticks %||% list()), y = far2(y_all), x_all = m$axes$x$ticks %||% list(), y_all = y_all)
}

# The details of one arm for the digitizer fields. The HR goes to the experimental arm only: the arm that is not the reference.
ai_figure_arm <- function(m, i) {
  a <- m$arms[[i]]; note <- character(0)
  key <- tolower(trimws(c(a$risk_label %||% "", a$label %||% "")))
  row <- Filter(function(r) tolower(trimws(r$label)) %in% key[nzchar(key)], m$risk_table$rows %||% list())
  times <- vapply(m$risk_table$times %||% list(), as.numeric, 0)
  tt <- character(0); nr <- character(0)
  if (length(row)) {
    n <- vapply(row[[1]]$n, function(v) if (is.null(v)) NA_real_ else as.numeric(v), 0)
    keep <- !is.na(n) & seq_along(n) <= length(times)
    if (any(!keep[seq_along(n)])) note <- c(note, sprintf("No number at risk at time %s (left out).", paste(times[seq_along(n)][!keep], collapse = ", ")))
    tt <- format(times[seq_along(n)][keep], trim = TRUE, drop0trailing = TRUE); nr <- format(n[keep], trim = TRUE)
  } else if (!is.null(m$risk_table)) note <- c(note, "No row of the risk table matches this arm.")
  an <- m$annotations; ref <- tolower(trimws(an$reference_arm %||% ""))
  # the reference arm as printed may be shorter or longer than the legend label ("Control" / "Control (n=248)" / "control group")
  labs <- tolower(trimws(c(a$label %||% "", a$risk_label %||% ""))); labs <- labs[nzchar(labs)]
  is_ref <- nzchar(ref) && any(vapply(labs, function(l) grepl(ref, l, fixed = TRUE) || grepl(l, ref, fixed = TRUE), TRUE))
  give_hr <- !is.null(an$hr) && nzchar(ref) && !is_ref
  if (!is.null(an$hr) && !nzchar(ref)) note <- c(note, "The figure does not say which arm is the reference, so its HR was not filled in.")
  s <- function(x) if (is.null(x)) "" else as.character(x)
  hex_ok <- function(h) is.character(h) && length(h) == 1 && grepl("^#[0-9A-Fa-f]{6}$", h)
  others <- Filter(hex_ok, lapply(m$arms[-i], function(o) o$colour_hex))
  # patients in the arm: the number at risk at time 0, so the details read here are saved with the curve
  n0 <- if (length(tt) && tt[1] == "0") nr[1] else ""
  list(label = a$label %||% paste("Curve", i), times = paste(tt, collapse = ", "), nrisk = paste(nr, collapse = ", "), n = n0,
       hr = if (give_hr) s(an$hr) else "", hrlo = if (give_hr) s(an$ci_lower) else "", hrhi = if (give_hr) s(an$ci_upper) else "",
       hex = if (hex_ok(a$colour_hex)) a$colour_hex else "", others = others, note = paste(note, collapse = " "))
}

# The background job: plain values in, plain values out
ai_figure_job <- function(job, transport = ai_http_transport)
  ai_figure_read(job$b64, job$width, job$height, job$study, job$key, job$model, transport = transport)
