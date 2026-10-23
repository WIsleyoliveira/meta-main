# Ponderis AI writing assistant Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A "Write" step in Quality & reporting with four tabs (Draft sections, Ask about my analysis, Reviewer responses, PRISMA checklist) where the AI writes only from a facts sheet the app builds, and every number it writes is checked against those facts.

**Architecture:** `R/ai_facts.R` (facts sheet + number check, plain R), `R/prisma_items.R` (verbatim PRISMA 2020 items), `R/ai_write.R` (schemas, prompts, calls for the four tasks, plain R), `R/ai_write_module.R` (Shiny tab). KM, GRADE and the review module return their reactives so the facts sheet can read them.

**Tech Stack:** R 4.6, shiny, httr2, jsonlite, pdftools (part 1), base `utils::unzip` for .docx text.

**Spec:** `docs/superpowers/specs/2026-10-23-ai-writing-assistant-design.md`

## Global Constraints

- Part 0-3 constraints hold. Languages: `c(English = "en", "Português" = "pt", "Español" = "es")`, default "en".
- Only the facts sheet is sent for Draft / Ask / Reviewer; the PRISMA tab sends the manuscript text (not the file) the person uploaded.
- Disclosure sentence exactly as in the spec.
- PRISMA items verbatim from `PRISMA_2020_checklist.pdf` (prisma-statement.org), local copy: `/private/tmp/claude-501/-Users-wisley-Downloads-meta-main/2d081b75-2343-48fd-8aa0-c6c79270f604/scratchpad/prisma/prisma2020_checklist.pdf/.txt`; citation "Page MJ, McKenzie JE, Bossuyt PM, et al. The PRISMA 2020 statement: an updated guideline for reporting systematic reviews. BMJ 2021;372:n71. Checklist reproduced under CC BY 4.0."
- Chat history append-only (earlier turns never edited; required by preserved thinking on Opus 5.5).
- AI text rendered as text (paragraphs, lists via a minimal safe markdown subset), never as raw HTML.
- Every existing test keeps passing.

## Review Focus

1. The analysis changes after a draft was written (new data, other model) — expected: the draft is marked "Written from an earlier analysis" and its number check uses the facts it was written from. Test in Task 4.
2. A number written as a range with a hyphen ("0.60-0.94") or with thousands separators ("1,234 participants") — expected: matched like "0.60 to 0.94" and "1234". Test in Task 1.
3. Markdown or HTML in the AI answer (`<script>`, `**bold**`, lists) — expected: shown safely as text with basic formatting, never executed. Test in Task 4.
4. A .docx manuscript with tables and footnotes — expected: text extracted (tables included), quotes verifiable. Test in Task 3.
5. Long chat (many turns) — expected: works; the request stays under the context (history capped at the last 20 turns with a note). Test in Task 4.

---

### Task 1: Facts sheet and number check (`R/ai_facts.R`)

**Files:** Create `R/ai_facts.R`; Modify `R/km_module.R` (return `list(core = core, extra = extra, params = <reactive p>)` from `km_server`), `R/grade_module.R` (return `list(outcomes = outcomes, question = question)`), `R/review_module.R` (return also `pr`, `grade`, `rob_tool`), `app.R` (keep the returned objects: `km <- km_server(...)`); Test `tests/test_ai_facts.R`.
**Produces:** `ai_facts(forest = NULL, km = NULL, rob = NULL, rob_tool = NULL, grade = NULL, prisma = NULL) -> data.frame(id, group, label, value, text)`; `ai_check_numbers(text, facts) -> data.frame(number, start, end, matched)`; `ai_mark_numbers(text, facts) -> htmltools tags` (unmatched numbers wrapped in `span.aiw-unmatched` with title).
- [ ] Tests: facts for each forest example type (`pooled_est` equals `exp(m$TE.random)` for ratio measures, `i2`, `k`, every study's estimate and weight), with a RoB table, GRADE outcomes and a PRISMA flow; KM facts from the KM example (pooled HR present); number check cases from the spec and Review Focus 2.
- [ ] FAIL → implement (KM facts read the same objects `km_results_text()` reads) → PASS + existing tests → commit `feat(ai): analysis facts sheet and number check`.

### Task 2: Draft, chat and reviewer requests (`R/ai_write.R`)

**Files:** Create `R/ai_write.R`, fixtures `tests/fixtures/ai/write_draft.json`, `write_chat.json`, `write_reviewer.json`; Test `tests/test_ai_write.R`.
**Produces:** `ai_write_schema(task)` for `task` in `"draft"`, `"chat"`, `"reviewer"`, `"prisma"`; `ai_write_system(task, lang)`; `ai_facts_text(facts)` (the sheet as compact text with ids);
`ai_write_draft(section, facts, lang, notes, key, model, transport)`; `ai_write_chat(history, question, facts, lang, key, model, transport)` (history = list of `list(role, text)`; capped at 20 turns);
`ai_write_reviewer(comments, facts, lang, key, model, transport)`. Each returns `list(ok, data, usage, called, error, message)`.
- [ ] Tests: schemas strict; prompts contain the language name, the section's instructions (Abstract headings of PRISMA 2020 for Abstracts) and the facts with ids; chat request messages alternate user/assistant and end with the new question, earlier turns byte-identical across calls; history cap; fixtures parse.
- [ ] FAIL → implement → PASS → commit `feat(ai): drafting, chat and reviewer-response requests`.

### Task 3: PRISMA items and manuscript checklist

**Files:** Create `R/prisma_items.R` (`PRISMA_ITEMS`: data.frame(item, section, topic, text), 42 rows, verbatim), add to `R/ai_write.R`: `ai_manuscript_text(path, name) -> list(pages = chr)` (PDF via pdftools; .docx via unzip of `word/document.xml`, paragraphs and table cells as lines, tags stripped, entities decoded), `ai_write_prisma(pages, lang, key, model, transport)` with quotes verified by `ai_quote_locator()`; fixture `write_prisma.json`; Test `tests/test_ai_write.R` (append).
- [ ] Tests: 42 items, ids 1, 2, 3, 4, 5, 6, 7, 8, 9, 10a, 10b, 11, 12, 13a-13f, 14, 15, 16a, 16b, 17, 18, 19, 20a-20d, 21, 22, 23a-23d, 24a-24c, 25, 26, 27; item 1 text "Identify the report as a systematic review."; a generated .docx (zip with a document.xml containing a paragraph and a table) gives its text including the table cell (Review Focus 4); checklist parse with quote statuses; manuscript too long → part 1 limits message.
- [ ] FAIL → implement → PASS → commit `feat(ai): PRISMA 2020 checklist from the manuscript`.

### Task 4: The Write tab

**Files:** Create `R/ai_write_module.R`; Modify `R/review_module.R` (nav_panel "Write" after GRADE; call `ai_write_server`), `app.R` (pass forest `fit`, `km`, `ai`), `www/style.css`; Test `tests/test_ai_write.R` (append, testServer).
**Produces:** inputs `aiw_lang`, `aiw_sections` (checkbox group), `aiw_notes`, `aiw_draft`, `aiw_question`, `aiw_ask`, `aiw_clear`, `aiw_comments`, `aiw_reviewer`, `aiw_ms` (file), `aiw_prisma`; outputs `aiw_reason`, `aiw_facts_tbl`, `aiw_drafts`, `aiw_chat`, `aiw_reviewer_out`, `aiw_prisma_out`; downloads `dl_aiw_doc`, `dl_aiw_reviewer_doc`, `dl_aiw_prisma_csv`, `dl_aiw_prisma_doc`; returned `list(drafts, chat)`.
- [ ] Tests (fake transport, sync runner, forest example loaded): draft of Results shown with one unmatched number flagged; analysis changed → draft marked earlier analysis (Review Focus 1); `<script>` in an answer is shown as text (Review Focus 3); two chat turns, second request contains the first verbatim; 25 turns → capped with note (Review Focus 5); reviewer table; PRISMA 42 rows; reasons when not ready or no analysis.
- [ ] FAIL → implement → PASS + full suite → browser check with the fake transport → commit `feat(ai): writing assistant (Write step in Quality & reporting)`.

### Task 5: README and live test

**Files:** Modify `README.md`, `tests/test_ai_live.R` (Results draft for the dichotomous example: no unmatched numbers).
- [ ] Write → SKIPPED without key → full suite → commit `docs(ai): writing assistant`.
