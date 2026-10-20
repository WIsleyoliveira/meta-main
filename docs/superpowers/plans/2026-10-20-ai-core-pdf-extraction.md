# Ponderis AI core and PDF extraction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let a reviewer upload study PDFs in Forest plot mode and get the data table filled by Claude, every value with page, quote and a local verification status, confirmed by the user before it enters the analysis.

**Architecture:** Plain R functions (no Shiny) for the HTTP client (`R/ai_client.R`) and for schema, parsing and verification (`R/ai_extract.R`); a Shiny module for the AI panel (`R/ai_module.R`) and one for the extraction block (`R/ai_extract_module.R`). Calls go through a swappable `transport` (tests use a fake) and run in the background through a swappable `runner` (default `ExtendedTask` + `mirai`, tests use a synchronous one).

**Tech Stack:** R 4.6, shiny 1.14 (`ExtendedTask`), httr2, jsonlite, mirai, promises, pdftools, DT.

**Spec:** `docs/superpowers/specs/2026-10-20-ai-core-pdf-extraction-design.md`

## Global Constraints

- Model ids exactly `claude-opus-5-5` (default) and `claude-sonnet-5-5`. Endpoint `https://api.anthropic.com/v1/messages`, header `anthropic-version: 2023-06-01`.
- Refusal fallback on every request: header `anthropic-beta: server-side-fallback-2026-07-01`, body `fallbacks = "default"`.
- Structured output via `output_config = list(effort = ..., format = list(type = "json_schema", schema = ...))`. No `thinking` field, no prefill, no `minimum`/`maximum`/`minLength` keywords in schemas, `additionalProperties: false` on every object.
- The API key never appears in any notification, log line, error message, project file, exported script or zip.
- Server key from `Sys.getenv("ANTHROPIC_API_KEY")`; session call cap `PONDERIS_AI_MAX_CALLS` (default 30) only when the server key is used.
- Prices (estimate): Opus 5.5 $4 in / $20 out, Sonnet 5.5 $2 / $10 per million tokens.
- Upload cap 32 MB (`options(shiny.maxRequestSize = 32 * 1024^2)`); PDFs over 32 MB or 600 pages are rejected before sending (`too_large`). At most 20 PDFs per run.
- AI packages (`httr2`, `jsonlite`, `mirai`, `promises`, `pdftools`) are optional: the app starts and every existing feature works without them.
- Interface copy in English, like the rest of the app. Consent text exactly: "Files you send to the AI (PDFs, images) are sent to Anthropic for processing. Do not send documents you are not allowed to share."
- Footer gains exactly: " If you use the AI features, the files you send are processed by Anthropic."
- Methods sentence when data came from AI extraction, exactly: " Data were extracted with the help of a large language model (Claude, Anthropic) and checked by the authors against the source articles."
- Every existing test file in `tests/` keeps its current result. Baseline on 2026-10-20 (R 4.6.1, meta 8.5.0): all pass except `test_engine.R` (`file.exists("forest_plot.svg")` fails when the exported script runs on its own) and `test_km_engine.R` (same, `km_pooled.png`/`.pdf` not written). Those two failures predate this work and are out of scope; they must not get worse.
- The folder is not a git repository: "Checkpoint" steps commit only if `git rev-parse` succeeds; otherwise skip them.

## Review Focus

1. A PDF whose text layer exists but whose numbers sit in a table split across lines ("12 / 150" printed as "12" newline "150") — expected: quote found → `quote_only` or `verified`, never `not_found` because of whitespace. Test in Task 3 (`verify: table cells across lines`).
2. The model returns zero studies for a PDF (wrong outcome, review article) — expected: the PDF is listed under its name with "No study rows found in this PDF" and its ambiguities, not a silent nothing. Test in Task 5.
3. User edits a cell to a non-number ("12a") in a numeric column — expected: the cell keeps the text, is marked edited, and the existing `validate` flow reports it after "Use these rows"; nothing crashes. Test in Task 5.
4. The same study appears in two PDFs (main paper + follow-up) — expected: both rows shown, a warning "Study 'X' appears in 2 PDFs" above the table. Test in Task 5.
5. The AI stops being available in the middle of a run (session call cap reached, key forgotten, consent unticked) — expected: the running call finishes, no new PDF is started, the remaining PDFs are listed as "Not run: " followed by `ai$reason()`. Test in Task 5 (cap reached after one PDF).

---

### Task 1: HTTP client (`R/ai_client.R`)

**Files:**
- Create: `R/ai_client.R`
- Create: `tests/fixtures/ai/ok_dich.json`, `refusal.json`, `max_tokens.json`, `bad_json.json`
- Test: `tests/test_ai.R` (new; sources `R/helpers.R`, `R/ai_client.R`)

**Interfaces:**
- Produces:
  - `AI_MODELS <- c("Claude Opus 5.5 (most accurate)" = "claude-opus-5-5", "Claude Sonnet 5.5 (cheaper)" = "claude-sonnet-5-5")`
  - `ai_body(model, system, content, schema, max_tokens = 16000, effort = "high") -> list` (the JSON body)
  - `ai_http_transport(body, key) -> list(status = <int>, body_text = <chr>)`; network failure returns `status = 0`.
  - `ai_parse(resp, key) -> list(ok = TRUE, data, usage = list(input_tokens, output_tokens), model)` or `list(ok = FALSE, error = <code>, message = <chr>)`
  - `ai_call(key, model, system, content, schema, max_tokens = 16000, effort = "high", transport = ai_http_transport)` = `ai_parse(transport(ai_body(...), key), key)`, returns `no_key` when `key` is empty without calling transport.
  - `ai_pdf_block(path) -> list(type = "document", source = list(type = "base64", media_type = "application/pdf", data = <chr>))`; `ai_text_block(txt) -> list(type = "text", text = txt)`
  - `ai_pdf_check(path) -> NULL` (fine) or the `too_large` error list (size > 32 MB or `pdftools::pdf_info(path)$pages > 600`).
  - `ai_cost(usage, model) -> numeric` dollars.
  - `ai_redact(txt, key) -> chr` replaces the key (and any `sk-ant-[A-Za-z0-9_-]+`) with `[key hidden]`.
  - `ai_available() -> chr` vector of missing AI packages (empty when all present).

- [ ] **Step 1: Write the failing tests** in `tests/test_ai.R`, plain `stopifnot` style like `tests/test_review.R`, each block printing `ok: <name>`:
  - `body: model, fallbacks, format`: `b <- ai_body("claude-opus-5-5", "sys", list(ai_text_block("hi")), list(type="object"))`; `b$model == "claude-opus-5-5"`, `b$fallbacks == "default"`, `b$output_config$format$type == "json_schema"`, `b$output_config$effort == "high"`, `is.null(b$thinking)`, `b$messages[[1]]$role == "user"`.
  - `pdf block`: write a 1-page PDF with `grDevices::pdf(); plot.new(); text(.5,.5,"x"); dev.off()`; `ai_pdf_block(f)$source$data` has no `"\n"` and decodes (`jsonlite::base64_dec`) to the file bytes.
  - `parse ok`: fake transport returning `list(status = 200L, body_text = readChar(fixture "ok_dich.json"))` → `ok`, `data$studies[[1]]$events_tx$value == 12`, `usage$input_tokens == 15000`.
  - `parse errors`: status 401 → `bad_key`; 429 → `rate_limited`; 529 → `overloaded`; 413 → `too_large`; 0 → `network`; fixture `refusal.json` → `refusal`; `max_tokens.json` → `truncated`; `bad_json.json` → `bad_json`. Every `message` is a non-empty sentence.
  - `no key`: `ai_call("", ...)` returns `no_key` and the fake transport is never called (counter stays 0).
  - `redaction`: a 401 whose body text contains the key `"sk-ant-test123"` → `!grepl("sk-ant-test123", r$message)`.
  - `cost`: `ai_cost(list(input_tokens = 1e6, output_tokens = 1e6), "claude-opus-5-5") == 24`; Sonnet → `12`.
  - `pdf check`: a file padded past 32 MB with `writeBin(raw(33 * 1024^2), f)` → `error == "too_large"`.
  Fixtures follow the Messages API response shape: `{"id","type":"message","role":"assistant","model":"claude-opus-5-5","content":[{"type":"text","text":"<JSON string>"}],"stop_reason":"end_turn","usage":{"input_tokens":15000,"output_tokens":900}}`; `refusal.json` has `stop_reason: "refusal"` and `stop_details: {"type":"refusal","category":null}`; `max_tokens.json` has truncated text; `bad_json.json` has `stop_reason: "end_turn"` and text `"{not json"`.

- [ ] **Step 2: Run** `Rscript tests/test_ai.R` — Expected: FAIL, `could not find function "ai_body"`.

- [ ] **Step 3: Implement `R/ai_client.R`.** `ai_http_transport` uses `httr2::request() |> req_headers(...) |> req_body_json(body, auto_unbox = TRUE, null = "null") |> req_timeout(300) |> req_retry(max_tries = 3, is_transient = function(r) resp_status(r) %in% c(429, 500, 502, 503, 529)) |> req_error(is_error = function(r) FALSE) |> req_perform()`, wrapped in `tryCatch` → `status = 0` on connection error. Only `ai_http_transport` and `ai_pdf_check` touch `httr2`/`pdftools`, guarded by `requireNamespace`, so sourcing the file never fails without them. Mapping of `stop_reason` happens before reading `content`.

- [ ] **Step 4: Run** `Rscript tests/test_ai.R` — Expected: every block prints `ok:` and the last line is `AI CLIENT TESTS PASSED`.

- [ ] **Step 5: Checkpoint** — `git add R/ai_client.R tests/test_ai.R tests/fixtures/ai && git commit -m "feat(ai): Claude API client with fake-transport tests"`.

---

### Task 2: Extraction schema, prompt and parsing (`R/ai_extract.R`, part 1)

**Files:**
- Create: `R/ai_extract.R`
- Test: `tests/test_ai.R` (append)

**Interfaces:**
- Consumes: `SPEC`, `template_cols()` from `R/helpers.R`; `ai_call`, `ai_pdf_block`, `ai_text_block`, `ai_pdf_check` from Task 1.
- Produces:
  - `ai_extract_schema(type) -> list` (exact shape in the spec, columns from `names(SPEC[[type]]$cols)`).
  - `ai_extract_system(type) -> chr` (fixed text naming each column with its `SPEC` label; rules: quote verbatim, `null` when not reported, never compute, choices go to `ambiguities`, several rows allowed for subgroups/comparisons, `page` is the PDF page number starting at 1).
  - `ai_extract_user_text(type, outcome = "", arms = "", notes = "") -> chr`.
  - `ai_extract_one(path, name, type, outcome, arms, notes, key, model, transport = ai_http_transport) -> list(ok, name, cells, ambiguities, usage, error, message)`.
  - `cells` is a long `data.frame(row = int, col = chr, value = chr, page = int, quote = chr, note = chr, status = chr)` with `col` in `template_cols(type)` (`study_id`, the SPEC columns, `year`, `subgroup`); `status` is filled by Task 3 (`NA` here).

- [ ] **Step 1: Write the failing tests:**
  - `schema strict`: for each of `names(SPEC)`, walk `ai_extract_schema(type)` recursively; every list with `type == "object"` has `additionalProperties == FALSE` and `required` equal to `names(properties)`; no element named `minimum`, `maximum`, `minLength`, `maxLength`, `multipleOf`; the study item's properties are exactly `c("study_id", "year", "subgroup", names(SPEC[[type]]$cols))`.
  - `schema serializes`: `jsonlite::toJSON(ai_extract_schema("cont"), auto_unbox = TRUE)` contains `"additionalProperties":false` and `"$defs"`.
  - `system names columns`: `grepl("Treatment: SD", ai_extract_system("cont"))`.
  - `extract one ok`: fake transport returning `ok_dich.json` (2 studies) → `ok`, `nrow(cells) == 2 * length(template_cols("dich"))`, `cells$value[cells$row == 1 & cells$col == "n_tx"] == "150"`, `length(ambiguities) == 1`.
  - `extract one zero rows`: fixture text `{"studies":[],"ambiguities":["Review article"]}` → `ok`, `nrow(cells) == 0`, ambiguity kept.
  - `extract one too large`: a 33 MB file → `ok == FALSE`, `error == "too_large"`, transport not called.
  - `request carries PDF and outcome`: fake transport capturing `body`; `body$messages[[1]]$content[[1]]$type == "document"`, text block contains the outcome `"30-day mortality"`.

- [ ] **Step 2: Run** `Rscript tests/test_ai.R` — Expected: FAIL at `could not find function "ai_extract_schema"`.

- [ ] **Step 3: Implement** the functions above in `R/ai_extract.R`. Numbers become character with `format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)`; `NULL` value → `NA_character_`.

- [ ] **Step 4: Run** `Rscript tests/test_ai.R` — Expected: PASS.

- [ ] **Step 5: Checkpoint** — commit `R/ai_extract.R tests/test_ai.R tests/fixtures/ai` with `feat(ai): extraction schema and parsing`.

---

### Task 3: Local verification (`R/ai_extract.R`, part 2)

**Files:**
- Modify: `R/ai_extract.R`
- Test: `tests/test_ai.R` (append)

**Interfaces:**
- Consumes: `cells` from Task 2.
- Produces:
  - `ai_norm_text(x) -> chr`: NFKC (`utf8::utf8_normalize` if available, else `iconv`-free manual map), ligatures ﬁ ﬂ ﬀ ﬃ ﬄ → letters, soft hyphen removed, `-\s*\n\s*` joined, all dashes/minus (`‐-―`, `−`) → `-`, whitespace collapsed to one space, lower case.
  - `ai_num_in(value, quote) -> logical`: TRUE when the number appears in the normalized quote, accepting decimal comma, thousands separators (`1,234`, `1 234`), trailing zeros (`12.0` = `12`), and `%`.
  - `ai_verify(cells, pages) -> cells` with `status` set; `pages` is `pdftools::pdf_text(path)` (character, one per page). Rules exactly as the spec: `empty`, `unverifiable` (all pages blank after trimming), `verified` (quote on `page`, `page ± 1`, and value in quote), `quote_only`, `not_found`. A quote not on the stated page but elsewhere in the PDF counts as found.
  - `ai_check_rows(wide, type) -> chr` warnings, `wide` being the `data.frame` of Task 5's `ai_cells_wide()`: events > n (dich/prop), SD <= 0 (cont), CI not containing the estimate (gen), each message naming the study.
  - `ai_cells_wide(cells, type) -> data.frame` (character, columns `template_cols(type)`, one row per `row`).

- [ ] **Step 1: Write the failing tests** using text vectors as `pages` (no PDF needed), plus one real PDF for `pdf_text`:
  - `verify hyphenation`: page `"mortal-\nity was 12/150"`, quote `"mortality was 12/150"`, value `"12"` → `verified`.
  - `verify ligature`: page `"signiﬁcant 0,85"`, quote `"significant 0.85"`, value `"0.85"` → `verified`.
  - `verify minus`: page `"MD −1.2"`, quote `"MD -1.2"`, value `"-1.2"` → `verified`.
  - `verify adjacent page`: quote on page 3, `page = 2` → `verified`.
  - `verify value missing`: quote found, value `"13"` not in it → `quote_only`.
  - `verify not found`: quote absent → `not_found`.
  - `verify scanned`: `pages = c("", "  ")` → every non-empty cell `unverifiable`.
  - `verify empty`: value `NA` → `empty`.
  - `verify: table cells across lines`: page `"Deaths  12\n  150"`, quote `"Deaths 12 150"`, value `"150"` → `verified` (Review Focus 1).
  - `num formats`: `ai_num_in("1234", "n = 1,234")`, `ai_num_in("12", "12.0%")`, `ai_num_in("0.5", "0,5")` all TRUE; `ai_num_in("12", "112")` FALSE.
  - `real pdf`: `grDevices::cairo_pdf` a page with `"Events 12 of 150"`; `ai_verify` with `pdftools::pdf_text` → `verified`.
  - `consistency`: dich row with `events_tx = 200, n_tx = 150` → one warning containing the study name; gen row `effect 2, ci 0.5–1.5` → warning.

- [ ] **Step 2: Run** `Rscript tests/test_ai.R` — Expected: FAIL at `could not find function "ai_verify"`.

- [ ] **Step 3: Implement.** Number matching: build the regex `(?<![0-9.])<int part>([.,]<decimals>0*)?(?![0-9])` from the value after normalizing both, plus a pass with thousands separators stripped from the quote.

- [ ] **Step 4: Run** `Rscript tests/test_ai.R` — Expected: PASS.

- [ ] **Step 5: Checkpoint** — commit with `feat(ai): local verification of quotes and numbers`.

---

### Task 4: AI panel (`R/ai_module.R`) and app wiring

**Files:**
- Create: `R/ai_module.R`
- Modify: `app.R:10` (upload cap 32 MB), `app.R:32-46` (AI button in topbar), `app.R:209-210` (footer), `app.R` server start (call `ai_server`)
- Modify: `install_packages.R` (add `httr2`, `jsonlite`, `mirai`, `promises`, `pdftools`)
- Modify: `www/style.css` (styles for `.ai-btn`, `.ai-status`, status dots `.ai-dot.verified|quote_only|not_found|unverifiable|empty|edited`)
- Test: `tests/test_ai_app.R` (new, `testServer` on `shinyAppDir(".")` like `tests/test_app.R`)

**Interfaces:**
- Consumes: `AI_MODELS`, `ai_cost`, `ai_available` (Task 1).
- Produces:
  - `ai_button_ui()` — the topbar button `actionButton("ai_open", ...)` with a status dot.
  - `ai_server(input, output, session) -> list(ready = reactive(lgl), key = function() chr, model = reactive(chr), record_usage = function(usage, model), calls_left = reactive(int or Inf), reason = reactive(chr))`. `reason()` is the one-line explanation when not ready: missing packages, no key, consent not given, limit reached.
  - Inputs created in the modal: `ai_key` (passwordInput, only without server key), `ai_model` (selectInput from `AI_MODELS`), `ai_consent` (checkbox, consent copy from Global Constraints).
  - The key is held in a `reactiveVal` inside `ai_server`; `ai_key` input is cleared with `updateTextInput(session, "ai_key", value = "")` after it is stored, and a "Forget my key" link empties it.

- [ ] **Step 1: Write the failing tests** in `tests/test_ai_app.R`:
  - `not ready by default`: with `Sys.unsetenv("ANTHROPIC_API_KEY")`, `ai$ready()` is FALSE and `ai$reason()` mentions "key".
  - `user key + consent`: `session$setInputs(ai_key = "sk-ant-x", ai_save_key = 1, ai_consent = TRUE)` → `ready()` TRUE, `calls_left()` is `Inf`.
  - `consent required`: key without consent → FALSE, reason mentions "consent".
  - `server key and cap`: `Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-server", PONDERIS_AI_MAX_CALLS = "2")`; consent TRUE; two `record_usage()` calls → `calls_left() == 0`, `ready()` FALSE, reason mentions "limit".
  - `usage`: `record_usage(list(input_tokens = 1e6, output_tokens = 0), "claude-opus-5-5")` → `output$ai_usage` text contains `"$4.00"`.
  - `key never rendered`: after storing `"sk-ant-x"`, no rendered output (`output$ai_usage`, `output$ai_status`) contains `"sk-ant-x"`.
  - `existing app unaffected`: `Rscript tests/test_app.R` still prints `APP TESTS PASSED`.

- [ ] **Step 2: Run** `Rscript tests/test_ai_app.R` — Expected: FAIL (`ai` not found).

- [ ] **Step 3: Implement** `R/ai_module.R` and the wiring. In `server`, `ai <- ai_server(input, output, session)` before `km_server(...)`. Expose `ai` to `testServer` by assigning it in the server function environment (same pattern as `raw`, `fit`).

- [ ] **Step 4: Run** `Rscript tests/test_ai_app.R` then `Rscript tests/test_app.R` — Expected: `AI PANEL TESTS PASSED` and `APP TESTS PASSED`.

- [ ] **Step 5: Checkpoint** — commit with `feat(ai): AI panel with key, consent, model and usage`.

---

### Task 5: Extraction block, background runner and hand-off

**Files:**
- Create: `R/ai_extract_module.R`
- Modify: `app.R:73-79` (step "Upload your CSV": add `ai_extract_ui()` under the file input), `app.R` server (call `ai_extract_server`), `app.R:448-451` and `app.R:522` (Methods sentence)
- Modify: `R/helpers.R:472` — `methods_text(p, k, ai = FALSE)` appends the Methods sentence from Global Constraints when `ai` is TRUE.
- Modify: `www/style.css` (review table, evidence box)
- Test: `tests/test_ai_app.R` (append)

**Interfaces:**
- Consumes: `ai` object (Task 4); `ai_extract_one`, `ai_verify`, `ai_cells_wide`, `ai_check_rows` (Tasks 2-3); `raw`, `detect_msg` reactiveVals in `app.R`.
- Produces:
  - `ai_extract_ui()`; inputs `ai_pdfs` (fileInput, `multiple = TRUE`, `accept = ".pdf"`), `ai_outcome`, `ai_arms`, `ai_notes`, buttons `ai_run`, `ai_cancel`, `ai_use`, `ai_discard`, download `ai_dl_csv`, `DT` output `ai_table` with `editable = "cell"`.
  - A runner runs ONE job: `runner(job, done)`, `job` a plain list of `ai_extract_one` arguments (no reactives), `done(result)` called with its return value. The module queues PDFs and starts the next one from `done`, checking `ai$ready()` before each.
  - `ai_runner_mirai(job, done)` — default: an `ExtendedTask` whose `mirai` worker sources `R/helpers.R`, `R/ai_client.R`, `R/ai_extract.R` from the app directory, calls `ai_extract_one`, then `ai_verify` with `pdftools::pdf_text`; `done` runs when the task result arrives.
  - `ai_runner_sync(job, done)` — same work in-process with `transport = getOption("ponderis.ai_transport", ai_http_transport)`; selected by `options(ponderis.ai_runner = ai_runner_sync)` in tests.
  - `ai_extract_server(input, output, session, ai, type, raw, detect_msg)` returning `list(results = reactive(list), from_ai = reactive(lgl))`; `from_ai()` is TRUE while `raw()$name == "ai_extracted.csv"`.
  - Before each PDF the module re-checks `ai$ready()`; when FALSE the remaining PDFs get `error = "stopped"`, message `paste("Not run:", ai$reason())`.

- [ ] **Step 1: Write the failing tests** (fake transport via `options(ponderis.ai_transport = ...)` returning fixtures by file name; user key + consent set as in Task 4):
  - `two pdfs one fails`: `ai_pdfs` = data.frame of two generated PDFs (`name`, `datapath`); transport returns `ok_dich.json` for the first, status 529 for the second → table has 2 rows from PDF 1, `output$ai_errors` lists PDF 2 with "busy" in the message and a retry button id `ai_retry_2`.
  - `zero rows`: transport returns zero studies → `output$ai_errors` contains "No study rows found in this PDF" and the ambiguity text (Review Focus 2).
  - `use rows feeds forest`: after a successful run, `session$setInputs(ai_use = 1)` → `raw()$name == "ai_extracted.csv"`, `names(raw()$data) == template_cols("dich")`; then set mapping inputs as in `tests/test_app.R` and `fit()$ok` is TRUE; `output$methods_txt` contains "large language model".
  - `edited non-number`: `session$setInputs(ai_table_cell_edit = data.frame(row = 1, col = <index of events_tx>, value = "12a"))` → cell status `edited`, `ai_use` still fills `raw()`; `fit()` returns `ok = FALSE` with a message (no error thrown) (Review Focus 3).
  - `duplicate study`: both PDFs return the same `study_id` → `output$ai_warnings` contains "appears in 2 PDFs" (Review Focus 4).
  - `stopped mid-run`: server key with `PONDERIS_AI_MAX_CALLS = "1"`, two PDFs, both fixtures ok → PDF 1 rows in the table, PDF 2 listed with "Not run:" and "limit", transport called exactly once (Review Focus 5).
  - `disabled when not ready`: no key → `output$ai_extract_reason` contains the `ai$reason()` text and `ai_run` does nothing (transport counter 0).
  - `csv download`: `output$ai_dl_csv` file reads back with `read_user_csv()` to the same rows.

- [ ] **Step 2: Run** `Rscript tests/test_ai_app.R` — Expected: FAIL (`ai_extract_ui` not found).

- [ ] **Step 3: Implement** `R/ai_extract_module.R`, the `methods_text(..., ai = )` argument (default FALSE keeps every existing caller and test unchanged) and the `app.R` wiring. Clicking a cell (`input$ai_table_cells_selected`) renders the evidence box: PDF name, page, quote, note, status label in words.

- [ ] **Step 4: Run** `Rscript tests/test_ai_app.R`, `Rscript tests/test_app.R`, `Rscript tests/test_engine.R` — Expected: `AI EXTRACTION TESTS PASSED`, `APP TESTS PASSED`, and test_engine's usual success line.

- [ ] **Step 5: Manual check in the browser** — start the app (`Rscript -e 'shiny::runApp(port = 4760)'`), open the AI panel, confirm the block is disabled without a key, then with `options(ponderis.ai_runner = ai_runner_sync, ponderis.ai_transport = <fixture transport>)` set in a dev launch confirm the table, dots, evidence box and "Use these rows" work end to end.

- [ ] **Step 6: Checkpoint** — commit with `feat(ai): extract forest-plot data from PDFs`.

---

### Task 6: Live test and documentation

**Files:**
- Create: `tests/test_ai_live.R`
- Modify: `README.md` (new section "AI features (optional)" after "Navigation"; "Files" table gains the four new R files; "Tests" gains `test_ai.R`, `test_ai_app.R`, `test_ai_live.R`; "Putting it online" gains the `ANTHROPIC_API_KEY` / `PONDERIS_AI_MAX_CALLS` lines and the privacy note)

**Interfaces:**
- Consumes: `ai_extract_one`, `ai_verify` (Tasks 2-3).

- [ ] **Step 1: Write `tests/test_ai_live.R`**: exits with `cat("SKIPPED (set PONDERIS_AI_LIVE=1 and ANTHROPIC_API_KEY)\n")` unless both are set; otherwise generates a one-page PDF describing a made-up trial ("Trial Z 2021 ... 12 of 150 patients died in the treatment group and 20 of 148 in the control group"), runs `ai_extract_one(type = "dich")` with `claude-sonnet-5-5`, and checks `events_tx == 12`, `n_tx == 150`, `events_ctrl == 20`, `n_ctrl == 148`, all four `verified`.

- [ ] **Step 2: Run** `Rscript tests/test_ai_live.R` — Expected without key: `SKIPPED ...`. With a key (only if the user provides one and agrees to the cost, about $0.01): all checks pass.

- [ ] **Step 3: Write the README section**: what the AI does, the five principles in one line each, how to give a key (server env var or panel), the session cap, cost estimate, the privacy note, and that results must be checked against the articles.

- [ ] **Step 4: Run the full suite** — every file listed in README "Tests" plus `test_ai.R` and `test_ai_app.R`; Expected: same results as the baseline recorded before Task 1, plus the new PASSED lines.

- [ ] **Step 5: Checkpoint** — commit with `docs(ai): README and live test`.
