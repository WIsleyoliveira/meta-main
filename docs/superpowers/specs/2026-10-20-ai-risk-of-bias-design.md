# Ponderis AI, part 2: risk-of-bias assistant (RoB 2 and ROBINS-I V2)

Date: 2026-10-20. Status: approved in conversation, waiting for review of this written spec.
Builds on part 0+1 (`docs/superpowers/specs/2026-10-20-ai-core-pdf-extraction-design.md`): same AI core, same five principles.

## Goal

In Quality & reporting → Risk of bias, the AI reads each study's PDF and answers the official signalling questions of the
tool, each answer with page, verbatim quote and a one-sentence rationale. The domain judgements are computed by the official
algorithms written as plain R code, never by the AI. The reviewer checks or changes any answer (the judgement follows), may
override a domain judgement with a reason, and applies the result to the existing risk-of-bias table, figure and GRADE.

## Decisions taken with the user

- Depth: signalling questions + official algorithms (not AI-proposed domain judgements).
- Tools in this part: RoB 2 (individually randomized parallel-group trials) and ROBINS-I V2. Newcastle-Ottawa stays manual.
- PDFs: one session PDF library shared by the forest-plot extraction, the risk-of-bias assistant and later parts.
- One AI call per study per tool, answering all questions of the tool in one schema-validated JSON (approach A).

## Sources (binding)

| Tool | Document | Use |
|---|---|---|
| RoB 2 | Higgins, Savović, Page, Sterne (eds). RoB 2 full guidance, version of 22 August 2019, and cribsheet of 22 August 2019 (riskofbias.info) | Question wording (verbatim), response options, conditional questions, Tables 4, 6, 8, 10, 12 and 14 (mapping of answers to judgements), overall judgement rule |
| ROBINS-I V2 | ROBINS-I V2 cribsheet, 20 November 2025 (riskofbias.info; marked draft by its authors) | Question wording (verbatim), response options including WY/SY/WN/SN, preliminary questions B2/B3 (critical risk without further assessment), C4 (Variant A or B of Domain 1), domain algorithms (flowcharts), overall algorithm |

Both are CC BY-NC-ND 4.0: question texts are reproduced verbatim, never edited, with the citation shown in the UI and in exports.
The README states the licence and that commercial use of these texts needs the authors' permission.

ROBINS-I V2 algorithms are published only as flowcharts. Each is transcribed into a decision function with one test per
flowchart path, and the UI labels ROBINS-I V2 judgements "proposed by the ROBINS-I V2 algorithm (draft, November 2025): confirm".
RoB 2 algorithms come from text tables and are labelled "suggested by the RoB 2 algorithm", as the tool itself says.

## Components

| File | Purpose |
|---|---|
| `R/ai_library.R` | Session PDF library: plain functions for entries; Shiny server part that both AI modules use |
| `R/rob_questions.R` | Question banks: `ROB2_Q` (Domain 2 in its two variants, assignment and adherence) and `ROBINS2_Q`, with id, domain, verbatim text, allowed answers, applicability condition |
| `R/rob_algorithms.R` | `rob2_domain(domain, answers, variant)`, `robins2_domain(domain, answers, variant)`, `rob2_overall()`, `robins2_overall()`, applicability `rob_applicable(bank, answers)` |
| `R/ai_rob.R` | Schema from the bank, system prompt, user text, parsing, quote verification |
| `R/ai_rob_module.R` | UI block in the Risk of bias tab, background runs, review panel, apply to table, exports |
| `tests/test_rob_algorithms.R` | Algorithms against the published tables / flowchart paths |
| `tests/test_ai_rob.R` | Schema, parsing, verification, server flow with the fake transport |

### Session PDF library (`ai_library.R`)

- Entry: `id` (stable within the session), `name`, `path`, `pages` (text from `pdftools::pdf_text`, read once, `character(0)` when unreadable), `study` (label or `NA`).
- `ai_library_server(input, output, session)` returns `list(entries = reactive(list), add = function(files_df), set_study = function(id, study), get = function(id))`.
- The forest-plot extraction (part 1) adds its PDFs to the library and, after extraction, sets `study` from the extracted `study_id` when a PDF gave exactly one study.
  Its own behaviour and tests stay as they are.
- The Risk of bias block can add PDFs to the same library.

### Question banks and algorithms

- A question: `list(id = "1.1", domain = 1, text = "<verbatim>", answers = c("Y","PY","PN","N","NI"), when = <condition or NULL>)`.
  `when` encodes the bracketed conditions of the tool ("If Y/PY/NI to 2.1 or 2.2"), as R code in the bank, not text to parse.
- `rob_applicable(bank, answers)` gives the questions that apply given the earlier answers; non-applicable questions take `"NA"`.
  The app enforces this after the AI answers, whatever the AI said.
- RoB 2: Domain 1 (Table 4), Domain 2 assignment (Table 6, two parts combined) and adherence (Table 8), Domain 3 (Table 10),
  Domain 4 (Table 12), Domain 5 (Table 14). Overall: Low if all domains Low; High if any domain High; otherwise Some concerns
  (the "multiple Some concerns may be High" judgement is left to the reviewer, with a note in the UI).
- ROBINS-I V2: B2/B3 Yes/PY → Critical, no further questions. C4 selects Domain 1 Variant A or B. Domains 1-7 from the flowcharts.
  Overall from the overall algorithm table (Low except uncontrolled confounding / Moderate / Serious / Critical).
- Judgement levels map onto the existing `ROB_TOOLS` levels so the existing figure, table and GRADE work unchanged
  (RoB 2: Low / Some concerns / High; ROBINS-I: Low / Moderate / Serious / Critical / No information).

### AI request (`ai_rob.R`)

- One call per (study, tool). Content: the PDF block, then a text block with the tool settings:
  RoB 2: outcome assessed, numerical result assessed (optional), effect of interest (assignment or adherence; for adherence the deviations to address).
  ROBINS-I V2: outcome, result, the reviewer's list of important confounding factors and co-interventions (from the protocol), and whether the analysis estimates the ITT or per-protocol effect if known.
- System prompt: the role, the rule that answers must follow the tool's own guidance, the question list with verbatim texts and allowed answers,
  "quote verbatim, give the PDF page, answer NI when the article does not say", and "never decide a domain judgement".
- Schema: `answers` object with one property per question id of the selected variant, each `{answer (enum of that question's allowed answers plus "NA"), page (integer or null), quote (string), rationale (string)}`;
  plus `notes` (array of strings). `additionalProperties: false` everywhere; enums are the only constraint used.
- Verification: the quote check of part 1 (same normalisation, page correction note), without the number check:
  `verified` (quote found), `not_found`, `unverifiable` (scanned), `empty` (no quote; allowed for NI answers).

### Review panel (`ai_rob_module.R`)

- In the Risk of bias tab, below the study list: block "Assess with AI" with the tool settings above, a PDF selector per study
  (from the library, plus "Add PDFs"), and "Assess with AI". Disabled with the reason when `ai$ready()` is FALSE.
- Runs: same queue as part 1 (one PDF at a time in the background, Stop, Retry, run token so Discard and new runs drop stale answers, session call cap).
- Per study, a collapsible card: domains in order; for each domain the applicable questions with the AI answer (a select the reviewer can change),
  page, quote, rationale, verification colour; the domain judgement from the algorithm, labelled as above; "Override" with a required reason.
  Changing an answer recomputes applicability and the judgement immediately. Changed answers are marked "edited by you".
- "Apply to the risk-of-bias table" sets the domain selects of that study in the existing grid (the same input ids `rob_key()` uses),
  so the robvis figure, table, CSV and GRADE use them unchanged. Studies are matched by name; unmatched studies are listed.
- Exports: CSV with one row per (study, question): domain, question id, question text, answer, page, quote, rationale, status, edited;
  and one row per (study, domain) with judgement, source (algorithm / override) and the support for judgement text
  (built from the answers and the override reason). A Word (.doc) "Risk of bias assessments" with one table per study, using the existing `.doc` writer of the GRADE module.
- Methods sentence (shown under the table, copy button): "Risk of bias was assessed with RoB 2 [ROBINS-I V2] for the outcome <outcome>.
  A large language model (Claude, Anthropic) proposed answers to the signalling questions with supporting quotes; the authors checked every answer, and domain judgements were derived with the tool's algorithms."

## Error handling

As part 1 (per-study failures listed with Retry; partial or refused answers never used). A study with no PDF linked is listed as "No PDF linked" and skipped.
An AI answer outside the allowed set for a question (cannot happen with the enum, checked anyway) is replaced by NI and flagged.

## Testing

- `tests/test_rob_algorithms.R`: every row of RoB 2 Tables 4, 6, 8, 10, 12, 14 as a test case; every path of each ROBINS-I V2 flowchart; applicability rules; overall rules; mapping to `ROB_TOOLS` levels.
- `tests/test_ai_rob.R` (fake transport): schema strict for every tool/variant; parsing; NA enforcement on non-applicable questions; verification statuses;
  server flow: assess two studies (one fails), change an answer and see the judgement change, override with reason, apply to the grid and check `rob_data()`; Discard during a run; exports.
- `tests/test_ai_live.R` gains an optional RoB 2 case: a made-up trial report with alternation for allocation → Domain 1 High.
- Every existing test keeps passing.

## Out of scope

Newcastle-Ottawa with AI, RoB 2 for cluster and crossover trials, two-reviewer reconciliation, saving assessments in the project file (follow-ups).
