# Ponderis AI, part 4: writing assistant

Date: 2026-10-23. Status: approved in conversation, waiting for review of this written spec.
Builds on parts 0-3 (AI core, five principles, `ai_call`, AI panel, background runner, quote verification).

## Goal

A fourth step in Quality & reporting, **Write**, where the AI drafts manuscript text and answers questions about the analysis on screen,
using only numbers the app computed. The app checks every number in what the AI writes against those numbers and marks the ones it
cannot match. Four tabs: Draft sections, Ask about my analysis, Reviewer responses, PRISMA checklist.

## Decisions taken with the user

- Scope: all four tabs.
- Output language chosen by the user: English (default), Português, Español; the chat answers in the language of the question.
  The app interface stays in English.
- Approach A: a facts sheet built by the app + structured outputs + number check. No raw study data, R output or script is sent
  (except the manuscript in the PRISMA tab, which the person uploads for that purpose). The AI runs no analysis; when one is needed it says which and where in the app.

## Facts sheet (`R/ai_facts.R`)

`ai_facts(forest = NULL, km = NULL, rob = NULL, grade = NULL, prisma = NULL) -> data.frame(id, group, label, value, text)`:
`value` numeric (or NA for text facts), `text` the value as the app shows it. Ids are short and stable (`pooled_est`, `pooled_lo`, `i2`, `study_3_est`…).

- Forest plot (from `fit()`): data type, effect measure, model and τ² estimator, Hartung-Knapp, k, total participants (and events for dichotomous data),
  pooled estimate with CI and p, prediction interval, I², τ², Q and its p, each study's label, estimate, CI and weight, subgroup estimates and the test for
  subgroup differences, leave-one-out range (smallest and largest pooled estimate and the study omitted), small-study test (name, p), trim-and-fill
  (studies added, adjusted estimate), meta-regression (covariate, slope, p, R²) when run.
- Kaplan-Meier (from the pooled analysis when run): pooled HR with CI and p, medians per group, RMST difference at the horizon, proportional-hazards test p.
- Risk of bias (from the table when complete): tool, number of studies per overall judgement, per-study overall judgement.
- GRADE (from the outcomes entered): certainty per outcome and the reasons for rating down.
- PRISMA (when the flow is valid): records identified, duplicates removed, screened, reports assessed, studies included, studies in the meta-analysis.
- The facts sheet is shown to the person in a collapsible table ("What the AI receives"), so they see exactly what is sent.

## Number check (`ai_check_numbers(text, facts)`)

- Extracts every number from the AI text (decimal point or comma, minus signs, percentages, ranges "0.60 to 0.94", "p < 0.001").
- A number is **matched** when it equals a fact's value rounded to 0-3 decimals, or as a percentage of a proportion fact (I² 0.73 → 73%),
  or is in the always-allowed set: 95 (CI level), 2020 (PRISMA 2020), integers 1-10 when used as counts in prose ("two studies"), years that appear in study labels.
- Unmatched numbers are highlighted in red in the shown text with the tooltip "Not found in your analysis: check it"; a count of unmatched numbers is shown above each text.
- p values written with "<" match when the fact's p is below the stated bound.

## Tabs (`R/ai_write_module.R`)

Common: language select (`aiw_lang`), the facts table, AI readiness reason, costs in the AI panel, background runs as before.

1. **Draft sections**: checkboxes Abstract (structured, PRISMA 2020 for Abstracts headings), Results, Discussion (strengths; limitations from risk of bias,
   heterogeneity, small-study effects, imprecision; GRADE certainty; implications), Plain-language summary; optional notes ("target journal: JAMA, 250 words").
   One call per section. Schema: `paragraphs: [{heading, text, facts_used: [ids]}]`, `cautions: [string]`. Shown with the number check, a copy button per section,
   "Download all (.doc)" and the disclosure sentence: "Parts of this manuscript were drafted with the help of a large language model (Claude, Anthropic) from the
   results computed in Ponderis; the authors checked and edited all text."
2. **Ask about my analysis**: a chat. History kept in the session, append-only (earlier turns are never edited). Each answer:
   `{answer (markdown), facts_used: [ids], suggested_analysis: string or null}`. The answer is rendered as plain text with paragraphs and lists (no raw HTML),
   with the number check. "Clear chat" starts a new conversation.
3. **Reviewer responses**: a text area for the peer reviewers' comments. Schema: `items: [{comment (verbatim excerpt), response, needs_new_analysis (bool), analysis (string)}]`.
   Shown as a two-column table (comment / response) with the number check; download .doc.
4. **PRISMA checklist**: upload the manuscript as PDF or .docx (text extracted locally: `pdftools::pdf_text`, or the XML of the .docx).
   The 27 items and their sub-items (42 rows) are stored verbatim in `R/prisma_items.R` from the official PRISMA 2020 checklist (prisma-statement.org, CC BY 4.0;
   Page MJ et al. BMJ 2021;372:n71). Schema: `items: [{item, status ("reported"|"partial"|"missing"), location (string), quote (string), comment}]`, one entry per row.
   Quotes are checked against the manuscript text with the part 1 locator; the table can be downloaded as CSV or .doc (filled checklist, "Location where item is reported").

## Error handling

As before (AI errors with Retry; partial or refused answers never used). Write tab without an analysis: the facts table says what is missing and Draft sections is disabled
with the reason; the chat and reviewer tabs still work with whatever facts exist. Manuscript over the PDF limits → part 1 message.

## Testing

- `tests/test_ai_facts.R`: facts from the example data of each forest-plot type (ids present, values equal to `fit()`), with RoB/GRADE/PRISMA inputs;
  `ai_check_numbers()` cases: 0.75 / 0,75 / 75% / "0.60 to 0.94" / "p < 0.001" / rounding (0.746 → 0.75) / unmatched 0.81 / allowed 95% CI / years in labels.
- `tests/test_ai_write.R`: schemas strict for the four tasks; prompts contain the language and the facts; fake-transport server flow for each tab
  (draft shown with an unmatched number flagged; chat keeps history append-only across two turns; reviewer items table; PRISMA checklist 42 rows with quote statuses);
  disabled reasons.
- `tests/test_ai_live.R`: an optional draft of a Results section for the example data, with no unmatched numbers allowed.
- Every existing test keeps passing.

## Out of scope

Running analyses from the chat (tool use), editing the manuscript file, citation search, other checklists (MOOSE, PRISMA-NMA).
