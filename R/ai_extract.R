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
