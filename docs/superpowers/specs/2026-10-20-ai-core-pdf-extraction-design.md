# Ponderis AI, part 0+1: AI core and data extraction from PDFs

Date: 2026-10-20. Status: approved in conversation, waiting for review of this written spec.

## Goal

Add generative AI (Claude, Anthropic API) to Ponderis so a reviewer can upload study PDFs and get the
forest-plot table filled in, with every number traceable to a page and a literal quote, checked
locally before it can be used. This part also builds the shared AI core that parts 2-4 will reuse:

| Part | Content | Spec |
|---|---|---|
| 0 | AI core: client, key, consent, background calls, cost | this file |
| 1 | Data extraction from PDFs (Forest plot mode) | this file |
| 2 | Risk-of-bias assistant (RoB 2 / ROBINS-I suggestions with quotes) | later |
| 3 | Vision digitizer (auto axis calibration, numbers-at-risk table from the image) | later |
| 4 | Writing assistant and chat about the analysis on screen | later |

## Decisions taken with the user

- Provider: Anthropic Claude API, called from R over HTTP with `httr2` (R has no official SDK). No Python sidecar, no calls from the browser.
- Key: server key (`ANTHROPIC_API_KEY`) when set, otherwise the user pastes their own key, held only in the session.
- Default model `claude-opus-5-5`; the user can switch to `claude-sonnet-5-5` (cheaper) in the AI panel.
- Principles, all five binding on every part:
  1. The AI proposes, the person confirms. Nothing enters an analysis without an explicit click.
  2. Every extracted value carries a page and a literal quote, and the app checks the quote against the PDF text locally.
  3. The AI never writes code the app runs. It returns schema-validated JSON that goes through the existing validators.
  4. Privacy is explicit: AI buttons stay disabled until the user accepts that files go to Anthropic. The footer says so.
  5. Without a key or internet the app behaves exactly as today.

## Part 0: AI core

### Files

| File | Purpose |
|---|---|
| `R/ai_client.R` | Plain functions, no Shiny: request building, sending, response parsing, error mapping, cost |
| `R/ai_module.R` | AI panel UI and server: key, consent, model, status, usage; exposes an `ai` object to other modules |
| `tests/test_ai.R` | Unit and `testServer` tests with a fake transport |
| `tests/fixtures/ai/*.json` | Recorded-shape API responses used by the tests |

### `ai_call()` contract

```r
ai_call(key, model, system, content, schema, max_tokens = 16000, effort = "high",
        transport = ai_http_transport)
# returns list(ok = TRUE, data = <parsed JSON as list>, usage = list(input_tokens, output_tokens), model = <served model>)
#      or list(ok = FALSE, error = <code>, message = <plain-language text>)
```

- Endpoint `POST https://api.anthropic.com/v1/messages`, headers `x-api-key`, `anthropic-version: 2023-06-01`, `content-type: application/json`.
- Body: `model`, `max_tokens`, `system`, `messages = list(list(role = "user", content = content))`,
  `output_config = list(effort = effort, format = list(type = "json_schema", schema = schema))`.
  Thinking is left out (adaptive by default on Opus 5.5 and Sonnet 5.5).
- Refusal fallback is on by default: header `anthropic-beta: server-side-fallback-2026-07-01` and `fallbacks = "default"`.
- `content` is a list of content blocks; helper `ai_pdf_block(path)` returns
  `list(type = "document", source = list(type = "base64", media_type = "application/pdf", data = <base64 without newlines>))`,
  placed before the text block.
- `transport` is a function `(body_list, key) -> list(status, body_text)`. `ai_http_transport` uses `httr2` with
  `req_retry(max_tries = 3)` on 429, 5xx and connection errors, and a 300 s timeout. Tests pass a fake.
- Parsing: check `stop_reason` first. `refusal` -> error `refusal`; `max_tokens` -> error `truncated`;
  otherwise the first `text` block is parsed with `jsonlite::fromJSON(simplifyVector = FALSE)`.
- Error codes and messages (shown to the user as notices, never raw JSON):
  `no_key`, `bad_key` (401/403), `rate_limited` (429 after retries), `overloaded` (529/5xx after retries),
  `too_large` (413, or PDF over 32 MB / 600 pages before sending), `refusal`, `truncated`, `bad_json`, `network`, `limit_reached`.
- The key never appears in messages, logs, notifications, project files or exported scripts. `ai_call` redacts it from any error text.

### Cost and limits

- `ai_cost(usage, model)` uses a fixed price table in `ai_client.R` (Opus 5.5 $4 / $20, Sonnet 5.5 $2 / $10 per million tokens),
  labelled "estimate" in the UI.
- With the server key, each session may make at most `PONDERIS_AI_MAX_CALLS` calls (default 30); beyond that `limit_reached`.
  With the user's own key there is no app limit.

### Background execution

- Calls run in `shiny::ExtendedTask` backed by `mirai`, so the session stays responsive and other users are not blocked.
  The task receives only plain values (key, model, file paths, schema), never reactive objects.
- Progress text in the UI: "Reading article 2 of 5…". A Cancel button stops queuing the remaining PDFs (the running one finishes).

### AI panel (`ai_module.R`)

- Opened from an "AI" button in the top bar, as a modal.
- Shows: status (on via server key / on via your key / off), password field for the key (hidden when a server key exists),
  model choice, consent checkbox with the text "Files you send to the AI (PDFs, images) are sent to Anthropic for processing.
  Do not send documents you are not allowed to share.", and the session usage (calls, tokens, estimated cost).
- Exposes to other modules: `ai$ready()` (reactive TRUE when key + consent), `ai$key()`, `ai$model()`, `ai$record_usage(usage)`, `ai$calls_left()`.
- Footer text gains: "If you use the AI features, the files you send are processed by Anthropic."

### New dependencies

`httr2`, `jsonlite`, `mirai`, `promises`, `pdftools` added to `install_packages.R`. All optional at runtime:
when one is missing, the AI button shows "AI needs the packages …" and everything else works.

## Part 1: data extraction from PDFs

### Where

Forest plot mode, step 2 "Upload your CSV", a new block below the file input: "Extract from PDFs with AI".
Disabled with a short reason until `ai$ready()`.

### Input

- 1 to 20 PDFs (`fileInput(multiple = TRUE)`); upload limit raised from 5 MB to 32 MB (`shiny.maxRequestSize`).
  The 2000-row limit of CSVs is unchanged.
- Data type from step 1 (`input$type`), optional text fields "Outcome" and "Treatment arm / control arm",
  optional "Notes for the AI" (for example "use intention-to-treat; 30-day timepoint").

### Request (one per PDF)

- `system`: fixed text, kept in `R/ai_extract.R`, explaining the task, the column meaning from `SPEC[[type]]$cols`,
  that quotes must be copied verbatim from the article, that values not reported must be `null` (never computed or guessed),
  and that choices between alternatives (ITT vs per-protocol, several timepoints, several arms) go in `ambiguities`.
- `content`: the PDF block, then a text block with type, outcome, arms and notes.
- `schema` built by `ai_extract_schema(type)` from `SPEC`, so all four data types work:

```json
{
  "type": "object", "additionalProperties": false,
  "required": ["studies", "ambiguities"],
  "properties": {
    "studies": { "type": "array", "items": {
      "type": "object", "additionalProperties": false,
      "required": ["study_id", "year", "subgroup", "<each SPEC column>"],
      "properties": {
        "study_id": { "$ref": "#/$defs/text_cell" },
        "year":     { "$ref": "#/$defs/num_cell" },
        "subgroup": { "$ref": "#/$defs/text_cell" },
        "<col>":    { "$ref": "#/$defs/num_cell" }
      }}},
    "ambiguities": { "type": "array", "items": { "type": "string" } }
  },
  "$defs": {
    "num_cell":  { "type": "object", "additionalProperties": false, "required": ["value", "page", "quote", "note"],
                   "properties": { "value": {"anyOf": [{"type": "number"}, {"type": "null"}]},
                                   "page":  {"anyOf": [{"type": "integer"}, {"type": "null"}]},
                                   "quote": {"type": "string"}, "note": {"type": "string"} } },
    "text_cell": { "type": "object", "additionalProperties": false, "required": ["value", "page", "quote", "note"],
                   "properties": { "value": {"anyOf": [{"type": "string"}, {"type": "null"}]},
                                   "page":  {"anyOf": [{"type": "integer"}, {"type": "null"}]},
                                   "quote": {"type": "string"}, "note": {"type": "string"} } }
  }
}
```

  Several rows per PDF are allowed (subgroups, several comparisons in one trial).

### Local verification (`ai_verify()` in `R/ai_extract.R`)

- `pdftools::pdf_text(path)` gives the text per page; both page text and quote are normalized
  (Unicode NFKC, ligatures, soft hyphens and line-break hyphenation removed, all whitespace collapsed, case folded,
  typographic minus and dashes mapped to `-`, decimal comma accepted as point).
- Status per cell:
  - `verified`: the quote is found on the given page (or on an adjacent page) and the value appears in the quote
    (number match tolerant to formatting: `12.0` vs `12`, `1,234` vs `1234`, percentages written with `%`);
  - `quote_only`: quote found, value not in it (for example a value derived from a table cell the quote does not contain);
  - `not_found`: quote not found anywhere in the PDF;
  - `unverifiable`: the PDF has no extractable text (scanned);
  - `empty`: value is `null`.
- Simple arithmetic consistency checks become warnings: events > n, SD <= 0, CI not containing the estimate,
  n_tx + n_ctrl far from a total stated in the same quote.

### Review table

- One row per extracted study row with the PDF name, the template columns, and a status dot per cell
  (green verified, yellow quote_only, red not_found, grey unverifiable/empty).
- Editable (`DT` with cell editing). Clicking a cell shows page, quote, note and status in a side box.
  An edited cell becomes `edited by you` (blue).
- Above the table: the ambiguities of each PDF, and the failed PDFs with their error message and a Retry button.
- Buttons: "Use these rows" (enabled when at least one row has the required columns), "Download as CSV", "Discard".

### Hand-off to the existing flow

"Use these rows" builds a character `data.frame` with `template_cols(type)` and calls
`raw(list(data = d, sep = ",", enc = "UTF-8", name = "ai_extracted.csv"))`, the same shape `read_user_csv()` returns.
From there, column matching, `validate`, the generated R script, the zip download and every test of the current flow apply unchanged.
`detect_msg()` says "Rows extracted with AI from N PDFs: check them against the articles before publishing."
The Methods paragraph gains one optional sentence when the data came from AI extraction:
"Data were extracted with the help of a large language model (Claude, Anthropic) and checked by the authors against the source articles."

## Error handling summary

- Per-PDF failures never stop the others; each failed PDF is listed with its message and a Retry button.
- Missing packages, missing key, missing consent and the server-key call limit each show one plain sentence where the button would be.
- A truncated or refused response is reported as such; the partial JSON is never used.

## Testing

- `tests/test_ai.R` (no network):
  - request body shape for each data type (model, headers, schema, document block, fallback header);
  - schema from `ai_extract_schema()` for the four types has `additionalProperties: false` on every object and no unsupported keywords;
  - response parsing: success, refusal, max_tokens, 401, 429 after retries, 529, invalid JSON;
  - key redaction in error text;
  - `ai_verify()`: hyphenation across lines, ligatures, decimal comma, typographic minus, quote on adjacent page,
    value missing from quote, scanned PDF (no text), using small PDFs generated in the test with `grDevices::pdf`;
  - consistency warnings;
  - `testServer`: with a fake transport, uploading two PDFs, one failing, gives one row and one listed error;
    "Use these rows" fills `raw()` and `fit()` succeeds; with no key and no consent the block is disabled.
- `tests/test_ai_live.R`: one real call, run only when `PONDERIS_AI_LIVE=1` and a key is set.
- The existing test files keep passing unchanged.

## Out of scope for this part

Risk of bias, vision digitizer, writing assistant (parts 2-4); extraction into the Kaplan-Meier tables; batch API; storing PDFs.
