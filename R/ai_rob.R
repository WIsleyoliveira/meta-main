# Risk-of-bias assistant: the AI answers the signalling questions of RoB 2 or ROBINS-I V2 from the PDF,
# each answer with page, verbatim quote and rationale. Judgements are never asked from the AI:
# rob_judge() derives them with the tool's algorithms. Plain functions, no Shiny.

ROB_TOOL_TITLE <- c(rob2 = "RoB 2 (randomized trials)", robins2 = "ROBINS-I V2 (non-randomized studies of interventions)")
ROB2_DEVIATIONS <- c(nonprotocol = "occurrence of non-protocol interventions",
                     implementation = "failures in implementing the intervention that could have affected the outcome",
                     adherence = "non-adherence to their assigned intervention by trial participants")

ai_rob_bank <- function(tool, variant) rob_bank(tool, variant)

# A safe, collision-free id part for a study name (hex of its UTF-8 bytes)
ai_rob_hex <- function(x) paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(as.character(x))))), collapse = "")

# An answer the reviewer still has to give: no valid code (only possible for questions without NI, such as 1.4 or 4.11)
ai_rob_needs_answer <- function(q, eff) !(eff %in% union(q$answers, if (!is.null(q$when)) "NA"))

ai_rob_schema <- function(tool, variant) {
  bank <- ai_rob_bank(tool, variant)
  cell <- function(q) list(type = "object", additionalProperties = FALSE, required = list("answer", "page", "quote", "rationale"),
    properties = list(answer = list(type = "string", enum = as.list(if (is.null(q$when)) q$answers else union(q$answers, "NA"))),
                      page = list(anyOf = list(list(type = "integer"), list(type = "null"))),
                      quote = list(type = "string"), rationale = list(type = "string")))
  props <- setNames(lapply(bank, cell), vapply(bank, `[[`, "", "id"))
  list(type = "object", additionalProperties = FALSE, required = list("answers", "notes"),
       properties = list(answers = list(type = "object", additionalProperties = FALSE, required = as.list(names(props)), properties = props),
                         notes = list(type = "array", items = list(type = "string"))))
}

ai_rob_system <- function(tool, variant) {
  bank <- ai_rob_bank(tool, variant)
  lab <- function(codes) paste(paste0(codes, " (", ROB_ANS_LABEL[codes], ")"), collapse = ", ")
  qs <- vapply(bank, function(q) paste0(q$id, if (nzchar(q$cond)) paste0(" [", q$cond, "]"), ": ", q$text, "\n   Answers: ", lab(q$answers)), "")
  paste0(
    "You help a systematic reviewer assess risk of bias with ", ROB_TOOL_TITLE[[tool]],
    if (tool == "robins2") " (ROBINS-I V2, draft of 20 November 2025)" else " (version of 22 August 2019)", ".\n",
    "Read the attached study report and answer every signalling question below, following the tool's own guidance for each answer.\n\n",
    "Rules:\n",
    "1. For each question give: answer (one of the allowed codes), page (PDF page, first page of the file = 1), a verbatim quote from the report ",
    "that supports the answer (copied character for character, one or two sentences), and a one-sentence rationale.\n",
    "2. When the report gives no information, answer NI with an empty quote and say in the rationale what is missing. Use the 'probably' answers only when the guidance allows a judgement.\n",
    "3. Answer NA when the question's condition in brackets is not met by your earlier answers.\n",
    "4. Never decide a domain-level or overall risk-of-bias judgement: the reviewer's software derives them from your answers with the tool's algorithms.\n",
    "5. Assess only the study, outcome and result named by the reviewer. Put anything the reviewer should check in notes.\n\n",
    "Signalling questions:\n", paste(qs, collapse = "\n"))
}

ai_rob_user_text <- function(settings, study, tool = "rob2") {
  line <- function(lab, x) if (nzchar(trimws(x %||% ""))) paste0(lab, ": ", trimws(x), "\n") else ""
  s <- settings
  paste0("Assess the study: ", study, "\n",
    line("Outcome assessed", s$outcome), line("Numerical result assessed", s$result),
    if (tool == "rob2") paste0(
      "Effect of interest: ", if (identical(s$effect, "adherence")) "the effect of adhering to intervention (the 'per-protocol' effect)"
                              else "the effect of assignment to intervention (the 'intention-to-treat' effect)", "\n",
      if (identical(s$effect, "adherence") && length(s$deviations))
        paste0("Deviations to address: ", paste(ROB2_DEVIATIONS[intersect(s$deviations, names(ROB2_DEVIATIONS))], collapse = "; "), "\n") else "")
    else paste0("The analysis estimates: ", if (identical(s$effect, "B")) "the per-protocol effect (Variant B of Domain 1)"
                else "the intention-to-treat effect (Variant A of Domain 1)", "\n",
                line("Important confounding factors (from the review protocol)", s$confounders),
                line("Important co-interventions", s$cointerventions)),
    line("Reviewer's notes", s$notes))
}

# One study: the AI call, the answers checked against the allowed codes and the tool's conditions, the quotes against the PDF.
ai_rob_one <- function(job, transport = ai_http_transport) {
  bank <- ai_rob_bank(job$tool, job$variant)
  ids <- vapply(bank, `[[`, "", "id")
  base <- list(study = job$study, pdf_name = job$pdf_name, tool = job$tool, variant = job$variant, notes = character(0), answers = NULL)
  chk <- ai_pdf_check(job$path)
  if (!is.null(chk)) return(c(chk, base, list(called = FALSE)))
  content <- list(ai_pdf_block(job$path), ai_text_block(ai_rob_user_text(job$settings, job$study, job$tool)))
  r <- ai_call(job$key, job$model, ai_rob_system(job$tool, job$variant), content, ai_rob_schema(job$tool, job$variant), transport = transport)
  if (!isTRUE(r$ok)) return(c(r, base, list(called = TRUE)))
  got <- r$data$answers
  raw <- vapply(bank, function(q) { x <- got[[q$id]]$answer %||% "NI"; if (x %in% union(q$answers, "NA")) x else "NI" }, "")
  names(raw) <- ids
  eff <- rob_force_na(bank, raw, job$settings$deviations %||% character(0))
  cell <- function(id, field, default) { x <- got[[id]][[field]]; if (is.null(x)) default else x }
  a <- data.frame(id = ids, domain = vapply(bank, function(q) q$domain, 0L), answer = unname(eff), ai_answer = unname(raw),
                  page = vapply(ids, function(id) { p <- cell(id, "page", NA); if (is.null(p) || is.na(p)) NA_integer_ else as.integer(p) }, 0L),
                  quote = vapply(ids, function(id) as.character(cell(id, "quote", "")), ""),
                  rationale = vapply(ids, function(id) as.character(cell(id, "rationale", "")), ""),
                  status = NA_character_, forced = unname(eff != raw), edited = FALSE, stringsAsFactors = FALSE, row.names = NULL)
  pages <- job$pages %||% tryCatch(pdftools::pdf_text(job$path), error = function(e) character(0))
  locate <- ai_quote_locator(pages); scanned <- ai_scanned(pages)
  for (i in seq_len(nrow(a))) {
    a$status[i] <- if (!nzchar(trimws(a$quote[i]))) "empty" else if (scanned) "unverifiable" else {
      at <- locate(a$quote[i], a$page[i])
      if (isTRUE(at$moved)) {
        a$rationale[i] <- trimws(paste(a$rationale[i], sprintf("(The AI gave page %s; the sentence is on page %d.)", a$page[i], at$page)))
        a$page[i] <- as.integer(at$page)
      }
      if (at$found) "verified" else "not_found"
    }
  }
  c(base[setdiff(names(base), c("answers", "notes"))],
    list(ok = TRUE, answers = a, notes = vapply(r$data$notes %||% list(), as.character, ""), usage = r$usage, called = TRUE, error = NULL, message = NULL))
}

# ROBINS-I V2: the variant of Domain 1 is the reviewer's choice; warn when the AI's answer to C4 points to the other one.
ai_rob_variant_warning <- function(tool, variant, answers) {
  if (tool != "robins2" || !("C4" %in% names(answers))) return("")
  c4 <- answers[["C4"]]
  if (variant == "A" && c4 == "Y") "The AI found that the analysis accounted for switches or other protocol deviations (the per-protocol effect): consider Variant B of Domain 1."
  else if (variant == "B" && c4 == "N") "The AI found that the analysis did not account for switches or protocol deviations (the intention-to-treat effect): consider Variant A of Domain 1."
  else ""
}
