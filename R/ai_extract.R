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

AI_LIGATURES <- c("\ufb00" = "ff", "\ufb01" = "fi", "\ufb02" = "fl", "\ufb03" = "ffi", "\ufb04" = "ffl", "\ufb05" = "st", "\ufb06" = "st")

# Text as it is compared: line-break hyphenation joined, ligatures spelled out, every dash and minus
# written "-", whitespace collapsed, lower case; with commas = TRUE a comma between digits becomes a point.
ai_norm_text <- function(x, commas = TRUE) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\u00ad", "", x, fixed = TRUE)                                  # soft hyphen
  x <- gsub("([[:alpha:]])-[ \t]*\r?\n[ \t]*([[:lower:]])", "\\1\\2", x)    # mortal-\nity
  for (l in names(AI_LIGATURES)) x <- gsub(l, AI_LIGATURES[[l]], x, fixed = TRUE)
  x <- gsub("[\u2010-\u2015\u2212\ufe63\uff0d]", "-", x)
  x <- gsub("[\u00a0\u2009\u202f]", " ", x)
  x <- gsub("([0-9])\u00b7([0-9])", "\\1.\\2", x)                         # mid-dot decimal (Lancet style)
  if (commas) x <- gsub("([0-9]),([0-9])", "\\1.\\2", x)
  tolower(trimws(gsub("[[:space:]]+", " ", x)))
}

AI_NUM_RE <- "(-?)((?<![0-9])\\.[0-9]+|[0-9]+(?:\\.[0-9]+)?)"

# Is the number `value` written in `quote`? Accepts 12 = 12.0, 1234 = 1,234 = 1 234, 0.5 = 0,5, a trailing %.
ai_num_in <- function(value, quote) {
  v <- suppressWarnings(as.numeric(value))
  if (is.na(v)) return(FALSE)
  q <- ai_norm_text(quote, commas = FALSE)
  variants <- unique(c(gsub("([0-9])[, ]([0-9]{3})(?![0-9])", "\\1\\2", q, perl = TRUE),  # thousands separators
                       gsub("([0-9]),([0-9])", "\\1.\\2", q)))                             # decimal comma
  for (s in variants) {
    m <- regmatches(s, gregexpr(AI_NUM_RE, s, perl = TRUE))[[1]]
    if (!length(m)) next
    neg <- startsWith(m, "-")
    num <- as.numeric(sub("^-", "", m))
    # a minus right after a digit is a range ("12-15"), not a sign
    if (any(abs(num - abs(v)) < 1e-9 & (v >= 0 | neg))) {
      if (v >= 0) return(TRUE)
      pos <- gregexpr(AI_NUM_RE, s, perl = TRUE)[[1]]
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
  # forms of a text compared: as normalized, without spaces, without spaces and hyphens
  # (pdf_text splits and joins words in tables; "placebo-\ncontrolled" loses its hyphen when joined)
  forms <- function(x) c(x, squash(x), gsub("-", "", squash(x), fixed = TRUE))
  page_forms <- lapply(norm_pages, forms)
  all_forms <- forms(all_text)
  in_forms <- function(qf, tf) any(vapply(seq_along(qf), function(k) grepl(qf[k], tf[k], fixed = TRUE), logical(1)))
  # where the quote is: the stated page (or next to it), another page, across a page break, or nowhere
  locate <- function(q, page) {
    q <- ai_norm_text(q)
    if (!nzchar(q)) return(list(found = FALSE))
    qf <- forms(q)
    hits <- which(vapply(page_forms, function(tf) in_forms(qf, tf), logical(1)))
    near <- if (is.na(page)) integer(0) else intersect(page + (-1:1), seq_along(norm_pages))
    if (length(intersect(hits, near))) return(list(found = TRUE, page = page))
    if (length(hits)) return(list(found = TRUE, page = hits[1], moved = TRUE))
    list(found = in_forms(qf, all_forms), page = page)
  }
  for (i in seq_len(nrow(cells))) {
    v <- cells$value[i]
    cells$status[i] <- if (is.na(v) || !nzchar(trimws(v))) "empty" else if (scanned) "unverifiable" else {
      at <- locate(cells$quote[i], cells$page[i])
      if (isTRUE(at$moved)) {
        said <- if (is.na(cells$page[i])) "no page" else paste("page", cells$page[i])
        cells$note[i] <- trimws(paste(cells$note[i], sprintf("The AI gave %s; the sentence is on page %d.", said, at$page)))
        cells$page[i] <- as.integer(at$page)
      }
      if (!at$found) "not_found"
      else if (cells$col[i] %in% AI_TEXT_COLS || ai_num_in(v, cells$quote[i])) "verified" else "quote_only"
    }
  }
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

# Totals of participants a quote states: "a total of 400", "in total 400", "400 patients were randomized",
# "we enrolled 400".
ai_quote_totals <- function(quote) {
  q <- gsub("([0-9])[, ]([0-9]{3})(?![0-9])", "\\1\\2", ai_norm_text(quote, commas = FALSE), perl = TRUE)
  res <- c("total(?: of)?[: ]+(?:n ?= ?)?([0-9]+)", "in total,? ([0-9]+)",
           "([0-9]+) (?:[a-z-]+ ){0,2}(?:were |was )?(?:randomi[sz]ed|enrolled)", "(?:randomi[sz]ed|enrolled) ([0-9]+)")
  unlist(lapply(res, function(re) {
    m <- unlist(regmatches(q, gregexpr(re, q, perl = TRUE)))
    as.numeric(sub(re, "\\1", m, perl = TRUE))
  }))
}

# Plain arithmetic checks on the extracted rows; one warning per problem, naming the study.
# `quotes`, when given, has the same shape as `wide` and holds the quote behind each value.
ai_check_rows <- function(wide, type, quotes = NULL) {
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
    if (type %in% c("dich", "cont") && !is.null(quotes) && !any(is.na(c(n(r$n_tx), n(r$n_ctrl))))) {
      nt <- n(r$n_tx); nc <- n(r$n_ctrl)
      # a total up to the larger arm is that arm's size or counts something else ("1:1", "12 centres");
      # within 10% the gap is usually dropouts
      tot <- ai_quote_totals(unique(c(quotes$n_tx[i], quotes$n_ctrl[i])))
      tot <- tot[tot > max(nt, nc)]
      if (length(tot) && all(abs(nt + nc - tot) > 0.1 * tot))
        add(sprintf("the arms add up to %s (%s + %s), but the quote states a total of %s.", nt + nc, r$n_tx, r$n_ctrl, tot[1]))
    }
  }
  out
}

# One PDF from start to end, as the background worker runs it: extraction, then the local check.
# `job` is a plain list (no reactives): path, name, type, outcome, arms, notes, key, model.
ai_run_job <- function(job, transport = ai_http_transport) {
  r <- ai_extract_one(job$path, job$name, job$type, job$outcome, job$arms, job$notes, job$key, job$model, transport = transport)
  if (isTRUE(r$ok) && nrow(r$cells)) {
    pages <- tryCatch(pdftools::pdf_text(job$path), error = function(e) character(0))
    r$cells <- ai_verify(r$cells, pages)
  }
  # the call was made (and is counted) unless the file was refused before sending
  r$called <- !(identical(r$error, "too_large") || identical(r$error, "bad_pdf") || identical(r$error, "no_key"))
  r
}
