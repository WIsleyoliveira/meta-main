# Ponderis AI risk-of-bias assistant Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The AI answers the RoB 2 / ROBINS-I V2 signalling questions from each study's PDF with quotes; official algorithms in R derive the domain judgements; the reviewer checks, edits, overrides and applies them to the existing risk-of-bias table.

**Architecture:** Question banks and algorithms are plain data and functions (`R/rob_questions.R`, `R/rob_algorithms.R`), tested against the published tables and flowcharts. The AI request/parse/verify layer (`R/ai_rob.R`) reuses the part-1 client and quote check. A session PDF library (`R/ai_library.R`) is shared with the part-1 extraction. The Shiny block (`R/ai_rob_module.R`) lives inside `review_server`.

**Tech Stack:** R 4.6, shiny 1.14, httr2, jsonlite, mirai, pdftools, DT (as part 1).

**Spec:** `docs/superpowers/specs/2026-10-20-ai-risk-of-bias-design.md`

## Global Constraints

- All part-1 Global Constraints hold (model ids, fallback header, structured outputs, key never shown, optional packages, English UI copy).
- Sources: RoB 2 guidance and cribsheet of 22 August 2019; ROBINS-I V2 cribsheet of 20 November 2025. Local copies (read only, outside the repo):
  `/private/tmp/claude-501/-Users-wisley-Downloads-meta-main/2d081b75-2343-48fd-8aa0-c6c79270f604/scratchpad/rob_docs/` (`rob2_guidance.pdf/.txt`, `rob2_cribsheet.pdf/.txt`, `robins2_cribsheet.pdf/.txt`).
- Question texts verbatim from the cribsheets (whitespace normalised only). Citation strings, exactly:
  RoB 2: "Sterne JAC, Savović J, Page MJ, et al. RoB 2: a revised tool for assessing risk of bias in randomised trials. BMJ 2019;366:l4898. Questions reproduced from the RoB 2 cribsheet (22 August 2019), CC BY-NC-ND 4.0."
  ROBINS-I V2: "ROBINS-I V2 (draft of 20 November 2025), riskofbias.info. Questions reproduced from the ROBINS-I V2 cribsheet, CC BY-NC-ND 4.0."
- Judgement labels: RoB 2 "Suggested by the RoB 2 algorithm"; ROBINS-I "Proposed by the ROBINS-I V2 algorithm (draft, November 2025): confirm".
- Answer codes: `Y PY PN N NI NA`, ROBINS-I also `WY SY WN SN` where the cribsheet offers them. Domain levels as `ROB_TOOLS` (`R/review_helpers.R:134`).
- Methods sentence exactly as in the spec ("Risk of bias was assessed with ... derived with the tool's algorithms.").
- Every existing test file keeps passing; full suite = every `tests/*.R` run one by one with `Rscript` from the repo root.

## Review Focus

1. The reviewer changes an early answer (1.1 → N) after the AI ran — expected: questions that stop applying become NA, questions that start applying show "Not answered" (NI) for review, and the judgement updates. Test in Task 6 (`edit answer recomputes`).
2. The study list in the RoB tab is renamed after the assessment ("Smith 2019" → "Smith 2019a") — expected: Apply lists the assessment as unmatched instead of writing into another study's row. Test in Task 6.
3. The AI answers a question that does not apply (2.4 although 2.3 was N) — expected: forced to NA, with no effect on the judgement. Test in Task 4.
4. A PDF linked to two studies (one paper reporting two trials) — expected: allowed; two calls, each told which study to assess. Test in Task 6.
5. Switching tool (RoB 2 → ROBINS-I) with assessments on screen — expected: assessments of the other tool are kept but hidden, never applied to the wrong tool's grid. Test in Task 6.

---

### Task 1: RoB 2 question bank and applicability

**Files:** Create `R/rob_questions.R`; Test `tests/test_rob_algorithms.R` (new).

**Interfaces — Produces:**
- `ROB2_Q`: list keyed by variant `"assignment"`, `"adherence"`; each a list of questions `list(id = "1.1", domain = 1L, text = <chr>, answers = <chr>, when = function(a) <lgl> or NULL)`; `a` is a named character vector of answers by id.
- `ROB_DOMAIN_NAMES$rob2`: the five domain titles as in `ROB_TOOLS$rob2$domains`.
- `rob_applicable(bank, answers) -> chr vector of ids that apply`; `rob_force_na(bank, answers) -> answers` with non-applicable ids set to `"NA"` and missing applicable ids set to `"NI"`.

- [ ] Step 1: tests — `rob2 bank sizes`: assignment has 1.1-1.3, 2.1-2.7, 3.1-3.4, 4.1-4.5, 5.1-5.3 (22 ids); adherence has 2.1-2.6 in Domain 2 (21 ids total); `rob2 texts verbatim`: `ROB2_Q$assignment` 1.1 text == "Was the allocation sequence random?", 3.1 starts with "Were data for this outcome available for all, or nearly all, participants randomized?"; `applicability`: with 2.1 = N and 2.2 = N, 2.3/2.4/2.5 are not applicable; with 2.3 = Y, 2.4 applies; 3.2 applies only if 3.1 in N/PN/NI; `force na`: an answer to 2.4 when 2.3 = N becomes "NA".
- [ ] Step 2: run `Rscript tests/test_rob_algorithms.R` — FAIL (`ROB2_Q` not found).
- [ ] Step 3: transcribe from `rob2_cribsheet.txt` (pages 4-22) every question text and its "If …" condition into `when` closures.
- [ ] Step 4: run — PASS. Step 5: commit `feat(rob): RoB 2 question bank and applicability`.

### Task 2: RoB 2 algorithms

**Files:** Create `R/rob_algorithms.R`; Test `tests/test_rob_algorithms.R` (append).

**Interfaces — Produces:** `rob2_domain(domain, answers, variant) -> "Low" | "Some concerns" | "High"`; `rob2_overall(judgements) -> same levels` (High if any High; Low if all Low; else Some concerns).

- [ ] Step 1: tests — one `stopifnot` per row of Tables 4, 6 (both parts and their combination), 8, 10, 12, 14 in `rob2_guidance.txt` (lines ~925, 1432, 1621, 2091, 2469, 2838), each row expanded to concrete answers (e.g. Table 4 row 1: 1.1 ∈ {Y, PY, NI} × 1.2 ∈ {Y, PY} × 1.3 ∈ {N, PN, NI} → Low, every combination); overall rule cases.
- [ ] Step 2: run — FAIL. Step 3: implement each domain as the table's rule order. Step 4: run — PASS. Step 5: commit `feat(rob): RoB 2 algorithms from the published tables`.

### Task 3: ROBINS-I V2 bank and algorithms

**Files:** Modify `R/rob_questions.R`, `R/rob_algorithms.R`; Test `tests/test_rob_algorithms.R` (append).

**Interfaces — Produces:** `ROBINS2_Q` keyed by variant `"A"`, `"B"` (Domain 1 variant), including preliminary `B2`, `B3`, `C4`; `robins2_domain(domain, answers, variant) -> "Low" | "Moderate" | "Serious" | "Critical"` (Domain 1 "Low" means "Low except for uncontrolled confounding"); `robins2_overall(judgements, critical_prelim = FALSE)`; `robins2_variant(answers)` from C4.

- [ ] Step 1: tests — bank ids per domain as in `robins2_cribsheet.txt`; WY/SY/WN/SN only where offered; one test per flowchart path (render pages with the algorithms via the Read tool: Domain 1 A p.20, B p.28, and each later "Algorithm for reaching default risk of bias judgement" page); B2 or B3 Y/PY → Critical overall without domains; overall table (cribsheet line ~1354).
- [ ] Step 2: run — FAIL. Step 3: implement; transcribe each flowchart twice (second pass re-reading the image against the code) and ledger the check. Step 4: run — PASS. Step 5: commit `feat(rob): ROBINS-I V2 bank and algorithms (draft Nov 2025)`.

### Task 4: AI request, parsing and verification (`R/ai_rob.R`)

**Files:** Create `R/ai_rob.R`; Modify `R/ai_extract.R` (extract the quote locator used by `ai_verify` into `ai_locate_quote(quote, page, pages) -> list(found, page, moved)` without changing `ai_verify` behaviour); Test `tests/test_ai_rob.R` (new).

**Interfaces — Consumes:** `ai_call`, `ai_pdf_block`, `ai_text_block`, `ai_pdf_check` (part 1); banks (Tasks 1, 3).
**Produces:** `ai_rob_schema(tool, variant)`; `ai_rob_system(tool, variant)`; `ai_rob_user_text(settings, study)`; `ai_rob_one(job, transport) -> list(ok, study, tool, variant, answers = data.frame(id, domain, answer, page, quote, rationale, status, edited), notes, usage, called, error, message)`; job fields: `path, pdf_name, study, tool, variant, settings, key, model`.

- [ ] Step 1: tests — schema strict (same walker as `tests/test_ai.R`) for rob2/assignment, rob2/adherence, robins A, robins B; each answer property's `answer` enum equals the bank's answers plus "NA"; parse a fixture `tests/fixtures/ai/rob2_alternation.json` (1.1 = N with quote "allocated alternately") → Domain 1 High via `rob2_domain`; non-applicable answer forced to NA (Review Focus 3); quote statuses verified / not_found / unverifiable / empty; `ai_verify` part-1 tests still pass.
- [ ] Step 2-5: FAIL → implement → PASS → commit `feat(ai): risk-of-bias request, parsing and quote check`.

### Task 5: Session PDF library

**Files:** Create `R/ai_library.R`; Modify `R/ai_extract_module.R` (add uploads to the library; set `study` when a PDF yields exactly one study), `app.R` (create the library before both modules and pass it); Test `tests/test_ai_app.R` (append).

**Interfaces — Produces:** `ai_library_server(input, output, session) -> list(entries = reactive(list of entries), add = function(files_df) -> ids, set_study = function(id, study), get = function(id))`; entry `list(id, name, path, pages, study)`.

- [ ] Step 1: tests — after an extraction run with `ok_dich.json`, the library has 2 entries, `study` NA for both (that fixture gives 2 rows per PDF); with a one-row fixture, `study` is set; adding the same file twice keeps one entry (same name and size); all existing part-1 tests unchanged.
- [ ] Step 2-5: FAIL → implement → PASS (plus `tests/test_ai_app.R` whole file) → commit `feat(ai): session PDF library shared by AI features`.

### Task 6: Risk-of-bias block, review, apply, exports

**Files:** Create `R/ai_rob_module.R`; Modify `R/review_module.R` (`review_server(input, output, session, fit, ai = NULL, library = NULL)`; call `ai_rob_server` when both given; place `ai_rob_ui()` below the study list), `app.R` (pass `ai`, `library`), `www/style.css`; Test `tests/test_ai_rob.R` (append, `testServer`).

**Interfaces — Consumes:** `ai`, library (Task 5), `rob_names`, `rob_key` inside `review_server`, `ai_rob_one` (Task 4), algorithms (Tasks 2-3), runner pattern of `R/ai_extract_module.R` (sync / function / mirai, run token).
**Produces:** inputs `airob_outcome`, `airob_result`, `airob_effect` (assignment/adherence), `airob_confounders`, `airob_pdf_<i>` (library id per study), `airob_run`, `airob_cancel`, `airob_discard`, `airob_retry`, `airob_answer` (event: `list(study, id, answer)`), `airob_override` (event: `list(study, domain, level, reason)`), `airob_apply`; outputs `airob_reason`, `airob_cards`, `airob_errors`, `airob_methods`, downloads `dl_airob_csv`, `dl_airob_doc`; returned `list(assessments = reactive)`.

- [ ] Step 1: tests (sync runner, fake transport): assess two studies, second fails → one card, one error with Retry; `edit answer recomputes` (Review Focus 1); override needs a reason (empty reason ignored, notice shown); Apply sets the grid inputs and `rob_data()` has the domain levels; renamed study → unmatched notice (Review Focus 2); one PDF for two studies → two calls with the study names in the user text (Review Focus 4); tool switch keeps but hides (Review Focus 5); Discard during a run (function runner, late answer dropped); CSV rows = questions × studies; methods text contains "large language model"; disabled reason when not ready.
- [ ] Step 2-5: FAIL → implement → PASS + full suite → browser check (fake-transport dev launch: cards, colours, edit, override, apply → robvis figure) → commit `feat(ai): risk-of-bias assistant in the Risk of bias tab`.

### Task 7: README and live test

**Files:** Modify `README.md` (section "AI risk-of-bias assistant", licence note, files and tests lists), `tests/test_ai_live.R` (optional RoB 2 case: made-up report with alternation → Domain 1 High).

- [ ] Steps: write → run live test without key (SKIPPED) → full suite → commit `docs(ai): risk-of-bias assistant`.
