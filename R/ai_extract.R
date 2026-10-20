# Data extraction from study PDFs with the AI: the schema the answer must follow, the prompt,
# parsing into one row per cell, and the local check of every quote and number against the
# text of the PDF. Plain functions, no Shiny.

ai_cell_def <- function(value_type) list(
  type = "object", additionalProperties = FALSE, required = list("value", "page", "quote", "note"),
  properties = list(value = list(anyOf = list(list(type = value_type), list(type = "null"))),
                    page = list(anyOf = list(list(type = "integer"), list(type = "null"))),
                    quote = list(type = "string"), note = list(type = "string")))

ai_extract_schema <- function(type) {
  cols <- names(SPEC[[type]]$cols)
  props <- c(list(study_id = list(`$ref` = "#/$defs/text_cell"), year = list(`$ref` = "#/$defs/num_cell"),
                  subgroup = list(`$ref` = "#/$defs/text_cell")),
             setNames(rep(list(list(`$ref` = "#/$defs/num_cell")), length(cols)), cols))
  list(type = "object", additionalProperties = FALSE, required = list("studies", "ambiguities"),
       properties = list(
         studies = list(type = "array", items = list(type = "object", additionalProperties = FALSE,
                                                     required = as.list(names(props)), properties = props)),
         ambiguities = list(type = "array", items = list(type = "string"))),
       `$defs` = list(num_cell = ai_cell_def("number"), text_cell = ai_cell_def("string")))
}

ai_extract_system <- function(type) {
  cols <- SPEC[[type]]$cols
  col_lines <- paste0("- ", names(cols), ": ", unname(cols), collapse = "\n")
  paste0(
    "You extract data for a meta-analysis from the full text of one study article (PDF).\n",
    "Data type: ", SPEC[[type]]$title, ". For each comparison you report one row with these fields:\n",
    "- study_id: a short study label, first author and year (for example \"Smith 2019\")\n",
    "- year: publication year\n",
    "- subgroup: only when the article reports this outcome separately for subgroups, otherwise null\n",
    col_lines, "\n\n",
    "Rules:\n",
    "1. Every field has value, page, quote and note. The quote is copied verbatim from the article, ",
    "character for character, and contains the number you report. Keep quotes short (one sentence or one table row).\n",
    "2. page is the page of the PDF where the quote is, counting the first page of the file as 1.\n",
    "3. Report only numbers the article states. Never compute, convert, estimate or impute a value: ",
    "when a value is not reported, set value to null, quote to \"\" and say in note what the article reports instead.\n",
    "4. When the article offers alternatives (intention-to-treat and per-protocol, several time points, several doses or arms), ",
    "follow the reviewer's instructions if they decide it; otherwise choose the primary analysis and describe the choice in ambiguities.\n",
    "5. Report one row per subgroup or comparison the reviewer asked for. If the article has no data for the outcome, ",
    "return no rows and explain why in ambiguities.\n",
    "6. Treatment means the experimental intervention, control the comparator, unless the reviewer names the arms.")
}

ai_extract_user_text <- function(type, outcome = "", arms = "", notes = "") {
  line <- function(lab, x) if (nzchar(trimws(x %||% ""))) paste0(lab, ": ", trimws(x), "\n") else ""
  paste0("Extract the data from the attached article.\n",
         line("Outcome", outcome), line("Arms (treatment vs control)", arms), line("Reviewer's instructions", notes))
}

ai_num_str <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (is.numeric(v)) return(format(v, scientific = FALSE, trim = TRUE, drop0trailing = TRUE, digits = 15))
  as.character(v)
}

ai_cells_long <- function(studies, type) {
  cols <- template_cols(type)
  rows <- lapply(seq_along(studies), function(i) {
    st <- studies[[i]]
    data.frame(row = i, col = cols,
               value = vapply(cols, function(k) ai_num_str(st[[k]]$value), ""),
               page = vapply(cols, function(k) { p <- st[[k]]$page; if (is.null(p)) NA_integer_ else as.integer(p) }, 0L),
               quote = vapply(cols, function(k) as.character(st[[k]]$quote %||% ""), ""),
               note = vapply(cols, function(k) as.character(st[[k]]$note %||% ""), ""),
               status = NA_character_, stringsAsFactors = FALSE, row.names = NULL)
  })
  if (!length(rows)) return(data.frame(row = integer(0), col = character(0), value = character(0), page = integer(0),
                                       quote = character(0), note = character(0), status = character(0), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

ai_extract_one <- function(path, name, type, outcome, arms, notes, key, model, transport = ai_http_transport) {
  fail <- function(e) c(e, list(name = name, cells = ai_cells_long(list(), type), ambiguities = character(0), usage = NULL))
  chk <- ai_pdf_check(path)
  if (!is.null(chk)) return(fail(chk))
  content <- list(ai_pdf_block(path), ai_text_block(ai_extract_user_text(type, outcome, arms, notes)))
  r <- ai_call(key, model, ai_extract_system(type), content, ai_extract_schema(type), transport = transport)
  if (!isTRUE(r$ok)) return(fail(r))
  list(ok = TRUE, name = name, cells = ai_cells_long(r$data$studies, type),
       ambiguities = vapply(r$data$ambiguities, as.character, ""), usage = r$usage, error = NULL, message = NULL)
}

# ---- Local check of quotes and numbers against the PDF text ----

AI_LIGATURES <- c("ﬀ" = "ff", "ﬁ" = "fi", "ﬂ" = "fl", "ﬃ" = "ffi", "ﬄ" = "ffl", "ﬅ" = "st", "ﬆ" = "st")

# Text as it is compared: line-break hyphenation joined, ligatures spelled out, every dash and minus
# written "-", whitespace collapsed, lower case; with commas = TRUE a comma between digits becomes a point.
ai_norm_text <- function(x, commas = TRUE) {
  x <- enc2utf8(as.character(x))
  x <- gsub("­", "", x, fixed = TRUE)                                  # soft hyphen
  x <- gsub("([[:alpha:]])-[ \t]*\r?\n[ \t]*([[:lower:]])", "\\1\\2", x)    # mortal-\nity
  for (l in names(AI_LIGATURES)) x <- gsub(l, AI_LIGATURES[[l]], x, fixed = TRUE)
  x <- gsub("[‐-―−﹣－]", "-", x)
  x <- gsub("[   ]", " ", x)
  if (commas) x <- gsub("([0-9]),([0-9])", "\\1.\\2", x)
  tolower(trimws(gsub("[[:space:]]+", " ", x)))
}

# Is the number `value` written in `quote`? Accepts 12 = 12.0, 1234 = 1,234 = 1 234, 0.5 = 0,5, a trailing %.
ai_num_in <- function(value, quote) {
  v <- suppressWarnings(as.numeric(value))
  if (is.na(v)) return(FALSE)
  q <- ai_norm_text(quote, commas = FALSE)
  variants <- unique(c(gsub("([0-9])[, ]([0-9]{3})(?![0-9])", "\\1\\2", q, perl = TRUE),  # thousands separators
                       gsub("([0-9]),([0-9])", "\\1.\\2", q)))                             # decimal comma
  for (s in variants) {
    m <- regmatches(s, gregexpr("(-?)([0-9]+(?:\\.[0-9]+)?)", s, perl = TRUE))[[1]]
    if (!length(m)) next
    neg <- startsWith(m, "-")
    num <- as.numeric(sub("^-", "", m))
    # a minus right after a digit is a range ("12-15"), not a sign
    if (any(abs(num - abs(v)) < 1e-9 & (v >= 0 | neg))) {
      if (v >= 0) return(TRUE)
      pos <- gregexpr("(-?)([0-9]+(?:\\.[0-9]+)?)", s, perl = TRUE)[[1]]
      for (i in which(abs(num - abs(v)) < 1e-9 & neg))
        if (pos[i] == 1 || !grepl("[0-9]", substr(s, pos[i] - 1, pos[i] - 1))) return(TRUE)
    }
  }
  FALSE
}

AI_TEXT_COLS <- c("study_id", "subgroup")

# Sets cells$status: empty, unverifiable (no text in the PDF), verified, quote_only, not_found.
ai_verify <- function(cells, pages) {
  if (!nrow(cells)) return(cells)
  scanned <- !any(nzchar(trimws(pages)))
  norm_pages <- ai_norm_text(pages)
  squash <- function(x) gsub(" ", "", x, fixed = TRUE)
  all_text <- paste(norm_pages, collapse = " ")
  found <- function(q, page) {
    q <- ai_norm_text(q)
    if (!nzchar(q)) return(FALSE)
    near <- if (!is.na(page)) norm_pages[intersect(page + (-1:1), seq_along(norm_pages))] else character(0)
    any(grepl(q, c(near, all_text), fixed = TRUE)) || grepl(squash(q), squash(all_text), fixed = TRUE)
  }
  cells$status <- vapply(seq_len(nrow(cells)), function(i) {
    v <- cells$value[i]
    if (is.na(v) || !nzchar(trimws(v))) return("empty")
    if (scanned) return("unverifiable")
    if (!found(cells$quote[i], cells$page[i])) return("not_found")
    if (cells$col[i] %in% AI_TEXT_COLS || ai_num_in(v, cells$quote[i])) "verified" else "quote_only"
  }, "")
  cells
}

ai_cells_wide <- function(cells, type) {
  cols <- template_cols(type)
  rows <- sort(unique(cells$row))
  d <- as.data.frame(setNames(rep(list(rep(NA_character_, length(rows))), length(cols)), cols), stringsAsFactors = FALSE)
  for (j in seq_along(rows)) {
    cj <- cells[cells$row == rows[j], , drop = FALSE]
    d[j, cj$col] <- cj$value
  }
  d
}

# Plain arithmetic checks on the extracted rows; one warning per problem, naming the study.
ai_check_rows <- function(wide, type) {
  n <- function(x) suppressWarnings(as.numeric(x))
  out <- character(0)
  for (i in seq_len(nrow(wide))) {
    r <- wide[i, , drop = FALSE]; s <- as.character(r$study_id %||% paste("Row", i)); if (is.na(s)) s <- paste("Row", i)
    add <- function(msg) out <<- c(out, paste0(s, ": ", msg))
    over <- function(e, t, arm) if (!is.na(n(r[[e]])) && !is.na(n(r[[t]])) && n(r[[e]]) > n(r[[t]]))
      add(sprintf("events (%s) are more than the total (%s)%s.", r[[e]], r[[t]], arm))
    if (type == "dich") { over("events_tx", "n_tx", " in the treatment arm"); over("events_ctrl", "n_ctrl", " in the control arm") }
    if (type == "prop") over("events", "n", "")
    if (type == "cont") for (k in c("sd_tx", "sd_ctrl")) if (!is.na(n(r[[k]])) && n(r[[k]]) <= 0) add(sprintf("%s is %s; a standard deviation must be above 0.", k, r[[k]]))
    if (type == "gen") {
      e <- n(r$effect); lo <- n(r$ci_lower); hi <- n(r$ci_upper)
      if (!any(is.na(c(e, lo, hi))) && (e < min(lo, hi) || e > max(lo, hi))) add(sprintf("the estimate %s is outside its confidence interval (%s to %s).", r$effect, r$ci_lower, r$ci_upper))
    }
  }
  out
}
