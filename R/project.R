# Save and open a project: the digitized tables, the Kaplan-Meier tables and the main settings, as one JSON file.
# The figure image is not saved (it stays in the browser); load it again from its file.

PROJECT_FORMAT <- 1L
PROJECT_SETTINGS <- c("scale", "curve_type", "display", "time_unit", "name_tx", "name_ctrl", "outcome", "conf", "marks",
                      "risk_table", "ypct", "xmax", "ymin", "landmarks", "tau", "landmark_time", "cuts", "truncs", "tv_df")

proj_df <- function(d) if (is.null(d) || !nrow(d)) NULL else { d[] <- lapply(d, function(x) enc2utf8(as.character(x))); d }

proj_write <- function(path, dig, km_files, settings) {
  files <- lapply(km_files, function(f) if (is.null(f)) NULL else
    list(data = proj_df(f$data), orig = as.list(f$orig), name = f$name, sep = f$sep, enc = f$enc))
  obj <- list(app = "Ponderis", format = PROJECT_FORMAT, saved = format(Sys.time(), "%Y-%m-%d %H:%M"),
              digitizer = list(curve = proj_df(dig$curve), risk = proj_df(dig$risk), arms = proj_df(dig$arms), cmp = proj_df(dig$cmp),
                               scale = dig$scale, stored = "survival proportion"),
              km_files = files, settings = settings)
  writeLines(jsonlite::toJSON(obj, dataframe = "rows", na = "null", null = "null", auto_unbox = TRUE, pretty = TRUE), path, useBytes = TRUE)
}

# Returns list(ok, error, project). Checks everything it reads: shape, columns, sizes.
proj_read <- function(path) {
  fail <- function(m) list(ok = FALSE, error = m)
  if (!file.exists(path) || file.size(path) > 20e6) return(fail("The project file is missing or too large."))
  obj <- tryCatch(jsonlite::fromJSON(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), simplifyVector = TRUE, simplifyDataFrame = TRUE),
                  error = function(e) NULL)
  if (!is.list(obj) || !obj$app %in% c("Ponderis", "Meta-Forest")) return(fail("This is not a Ponderis project file."))
  if (!is.numeric(obj$format) || obj$format > PROJECT_FORMAT) return(fail("This project was saved by a newer version of the app."))
  as_tab <- function(d, which, keep = KM_COLS[[which]]) {
    if (is.null(d) || (is.data.frame(d) && !nrow(d)) || length(d) == 0) return(NULL)
    if (!is.data.frame(d)) return(NA)
    need <- KM_COLS[[which]]
    if (!all(need %in% names(d))) return(NA)
    for (k in setdiff(keep, names(d))) d[[k]] <- NA_character_      # a column added by a later version (the subgroup label)
    d <- d[, keep, drop = FALSE]; d[] <- lapply(d, function(x) { x <- as.character(x); x[x %in% c("NA", "")] <- NA_character_; x })
    if (nrow(d) > 200000) return(NA)
    d
  }
  dg <- obj$digitizer %||% list(); out <- list(digitizer = list(), km_files = list(), settings = list())
  for (w in c("curve", "risk", "arms")) {
    t <- as_tab(dg[[w]], w, keep = DIG_COLS[[w]]); if (identical(t, NA)) return(fail(sprintf("The digitizer %s table in the project is not valid.", w)))
    out$digitizer[[w]] <- if (is.null(t)) dig_empty(w) else t
  }
  out$digitizer$scale <- if (identical(dg$scale, "percent")) "percent" else "proportion"
  # comparisons (added in a later version): the units with their curves
  cm <- dg$cmp
  if (is.data.frame(cm) && nrow(cm) && all(names(dig_cmp_empty()) %in% names(cm))) {
    cm <- cm[, names(dig_cmp_empty()), drop = FALSE]; cm[] <- lapply(cm, function(x) trimws(as.character(x)))
    cm <- cm[!is.na(cm$comparison) & nzchar(cm$comparison) & cm$group %in% c("treatment", "control"), , drop = FALSE]
    if (nrow(cm) > 5000) return(fail("The comparisons in the project are too large."))
    out$digitizer$cmp <- cm
  } else out$digitizer$cmp <- dig_cmp_empty()
  # Projects saved before the curves were stored as survival proportions keep the figure's own scale: convert them now
  if (is.null(dg$stored) && nrow(out$digitizer$curve)) {
    sv <- suppressWarnings(as.numeric(out$digitizer$curve$survival))
    if (identical(dg$scale, "percent")) sv <- sv / 100
    if (identical(obj$settings$curve_type, "cumulative")) sv <- 1 - sv
    out$digitizer$curve$survival <- as.character(round(sv, 6))
  }
  kf <- obj$km_files %||% list()
  for (w in c("curve", "risk", "arms")) {
    f <- kf[[w]]; if (is.null(f) || is.null(f$data)) next
    t <- as_tab(f$data, w); if (is.null(t) || identical(t, NA)) return(fail(sprintf("The Kaplan-Meier %s table in the project is not valid.", w)))
    o <- unlist(f$orig); if (!is.character(o) || is.null(names(o))) o <- setNames(KM_COLS[[w]], KM_COLS[[w]])
    out$km_files[[w]] <- list(data = t, orig = o, name = txt(f$name %||% paste0(w, ".csv"), 120), sep = if (identical(f$sep, ";")) ";" else ",", enc = txt(f$enc %||% "UTF-8", 20))
  }
  st <- obj$settings; if (is.list(st)) for (n in intersect(names(st), PROJECT_SETTINGS)) {
    v <- st[[n]]; if ((is.character(v) || is.numeric(v) || is.logical(v)) && length(v) == 1) out$settings[[n]] <- v
  }
  list(ok = TRUE, project = out, saved = obj$saved)
}
