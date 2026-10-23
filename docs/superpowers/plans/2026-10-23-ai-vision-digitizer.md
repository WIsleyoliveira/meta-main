# Ponderis AI figure reading (digitizer) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** One AI call reads a Kaplan-Meier figure (axes, arms, risk table, HR); "Use these axes" calibrates with ticks snapped in the image; "Use this arm" fills the curve details and runs the existing colour trace.

**Architecture:** Plain R for request/schema/parse/checks (`R/ai_figure.R`); a Shiny block in the digitizer step 1 (`R/ai_figure_module.R`); browser logic in `www/dig_ai.js` (sending the scaled image, pure `snapTick()` / `nearestColour()`, applying axes and arms through a small API exposed by `www/digitizer.js`).

**Tech Stack:** R 4.6, shiny, httr2, jsonlite; browser JS (no libraries); node for pure-function tests.

**Spec:** `docs/superpowers/specs/2026-10-23-ai-vision-digitizer-design.md`

## Global Constraints

- Part 0-2 constraints hold (models, fallback header, structured outputs, key never shown, consent, optional packages, English UI copy).
- The image leaves the browser only on a click of "Read this figure with AI" while `ai$ready()`; long side ≤ 1568 px; PNG base64.
- New aside text exactly: "The image stays in your browser and is not uploaded, unless you press Read this figure with AI."
- Snapping: axis line search within ±6% of the image size; tick search window ±2.5% along the axis; snapped result within 1 px of the true tick in tests.
- The AI never traces curves; "Use this arm" calls the existing trace.
- Every existing R test keeps passing; `node tests/test_dig_snap.js` passes.

## Review Focus

1. Figures whose axis has gridlines at every tick — expected: the snap uses the tick mark on the axis line, not a gridline far from it. Test in Task 2 (`gridlines do not capture`).
2. A legend colour that differs from the drawn curve (anti-aliasing, JPEG) — expected: `nearestColour()` returns the real curve colour. Test in Task 2.
3. Loading another image after a reading — expected: the reading and its buttons disappear (no applying stale axes). Test in Task 3.
4. A risk table with "—" or blank cells — expected: those numbers are null, the row is kept, a warning names it. Test in Task 1.
5. The reading says Y is in percent — expected: Y calibration values become 0 and 100 (or the read tick values), never 0-1 on a percent axis. Test in Task 3.

---

### Task 1: Request, schema, parsing and checks (`R/ai_figure.R`)

**Files:** Create `R/ai_figure.R`, `tests/fixtures/ai/figure_two_arms.json`; Test `tests/test_ai_figure.R`.
**Interfaces — Produces:** `ai_image_block(b64, media_type = "image/png")`; `ai_figure_schema()`; `ai_figure_system()`; `ai_figure_user_text(width, height, study = "")`;
`ai_figure_read(b64, width, height, study, key, model, transport) -> list(ok, reading, warnings, usage, called, error, message)`;
`ai_figure_check(reading) -> character`; `ai_figure_scale(reading, factor) -> reading` (tick px/py multiplied).
- [ ] Tests: schema strict (walker as in `tests/test_ai.R`); prompt contains width/height and "never estimate"; fixture parse (2 arms, colours, risk table 4 times, HR 0.75 0.60-0.94); checks: increasing n, length mismatch, non-monotone ticks, HR outside CI, unmatched risk row, null cells ("—") warned; scaling by 2 doubles positions; image block shape; errors map as part 0 (529 → overloaded).
- [ ] FAIL → implement → PASS → commit `feat(ai): figure reading request, schema and checks`.

### Task 2: Snapping and colour (`www/dig_ai.js`, pure part)

**Files:** Create `www/dig_ai.js` (pure functions first, exported with `if (typeof module !== 'undefined') module.exports = {...}`); Test `tests/test_dig_snap.js` (node).
**Interfaces — Produces:** `snapTick(img, axis, px, py, opts) -> {px, py, snapped, dist}`; `nearestColour(img, hex, box) -> [r, g, b]`; `hexToRgb(hex)`.
- [ ] Tests build RGBA images in JS (white 1000×700, black axis lines at x=100 and y=600, tick marks every 172 px, light grey gridlines, black text blobs, a coloured curve):
  `snap x from +10 px`, `snap y from -9 px`, `no tick marks → snapped false`, `gridlines do not capture` (Review Focus 1), `curve does not capture`,
  `nearestColour from a lighter legend hex` (Review Focus 2).
- [ ] FAIL (`node tests/test_dig_snap.js`) → implement → PASS → commit `feat(dig): snap AI tick positions onto tick marks; nearest curve colour`.

### Task 3: AI reading in the digitizer (UI, browser wiring, apply)

**Files:** Create `R/ai_figure_module.R`; Modify `www/dig_ai.js` (browser part), `www/digitizer.js` (expose `window.KMDigitizerAPI = {draw, trace, status, setCal}` and fire a `dig:image` event on load), `R/dig_module.R` (aside text, `ai_figure_ui()` in step 1), `app.R` (script tag for `dig_ai.js`, `ai_figure_server(...)`), `www/style.css`; Test `tests/test_ai_figure.R` (append, testServer).
**Interfaces — Consumes:** `ai` (part 0), runner pattern of part 1/2 (`"sync"` / function / mirai), Task 1 functions.
**Produces:** input `aifig_image` (event from JS: `{b64, width, height, orig_width, orig_height, nonce}`), `aifig_read` (button), `aifig_use_axes`, `aifig_use_arm` (event `{i}`);
custom messages to JS `aifig_axes` (`{xscale, yscale, y_percent, curve_type, ticks: {x: [...], y: [...]}}`) and `aifig_arm` (`{label, times, nrisk, hr, hrlo, hrhi, hex}`); outputs `aifig_reason`, `aifig_panel`, `aifig_progress`.
- [ ] Tests (fake transport, sync runner): reason shown without key or without image; image event + read → reading kept, panel lists 2 arms and the HR; "Use these axes" sends `aifig_axes` with the two farthest ticks per axis and percent handled (Review Focus 5); "Use this arm" sends `aifig_arm` with the arm's times/numbers and HR only for the experimental arm; a new image event clears the reading (Review Focus 3); usage counted.
- [ ] FAIL → implement → PASS + full suite → browser check (synthetic figure from `tests/make_fig.R`, fake transport answering the fixture with the figure's true tick positions plus noise): Read → Use these axes (crosses on ticks) → Use this arm (fields filled, curve traced) → Save → commit `feat(ai): read Kaplan-Meier figures with AI in the digitizer`.

### Task 4: README and live test

**Files:** Modify `README.md` (section, files, tests), `tests/test_ai_live.R` (a synthetic figure with a risk table drawn in R; the table read exactly).
- [ ] Write → live test SKIPPED without key → full suite + node test → commit `docs(ai): figure reading`.
