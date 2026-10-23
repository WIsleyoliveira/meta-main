# Ponderis AI, part 3: reading Kaplan-Meier figures with AI (digitizer)

Date: 2026-10-23. Status: approved in conversation, waiting for review of this written spec.
Builds on parts 0-2 (AI core, five principles, `ai_call`, AI panel, background runner).

## Goal

In Survival curves → Digitize curves, one button, **Read this figure with AI**, sends the loaded figure to Claude once and gets back
what can be read as text from it: the axes (tick values, approximate tick positions, scale, Y unit, curve type), the arms in the legend
(label, colour, line style), the numbers-at-risk table (times and numbers per arm) and a hazard ratio printed on the figure.
The reviewer then uses that reading in two clicks: **Use these axes** (calibration snapped onto the tick marks in the image) and
**Use this arm** (fills the curve details and runs the existing trace by colour). The AI never traces the curve.

## Decisions taken with the user

- Scope: read the risk table and legend (with HR), calibrate the axes with snapping, choose the colour and start the existing tracing.
  No AI tracing of the curve itself (vision coordinates are less precise than the validated colour trace).
- One AI call per figure (approach A).

## Privacy

The digitizer today promises that the image never leaves the browser. That stays true unless the person presses
**Read this figure with AI** with the AI ready (key and consent, as in part 0). Only then the canvas image, scaled down so its long
side is at most 1568 px, is sent (PNG, base64) to the server and to Anthropic. The aside under the file input becomes:
"The image stays in your browser and is not uploaded, unless you press Read this figure with AI."

## Components

| File | Purpose |
|---|---|
| `R/ai_figure.R` | Plain functions: schema, system prompt, image block, parsing, checks of the risk table, scaling of positions |
| `R/ai_figure_module.R` | Shiny: the "Read this figure with AI" block and the "AI reading" panel in step 1 of the digitizer |
| `www/dig_ai.js` | Browser: sends the scaled image; `snapTick()` (pure function); applying axes; applying an arm (fill fields, pick colour, trace) |
| `tests/test_ai_figure.R` | R: schema, parsing, checks, server flow with the fake transport |
| `tests/test_dig_snap.js` | Node (no browser): `snapTick()` and `nearestColour()` on synthetic RGBA images |

### Request (`ai_figure.R`)

- `ai_image_block(b64, media_type = "image/png")` → `list(type = "image", source = list(type = "base64", media_type, data))`, placed before the text block.
- Text block: image width and height in pixels (of the image sent), and the reviewer's study name if typed.
- Schema (strict, `additionalProperties: false` everywhere, nullable via `anyOf` with null):
  - `axes`: `x` and `y`, each `{label (string), scale ("linear"|"log"), unit (string), ticks: array of {value (number), px (number), py (number)}}`;
    `y_percent` (boolean: the Y axis is in % rather than 0-1); `curve_type` ("survival"|"cumulative").
  - `arms`: array of `{label, colour_hex (string "#RRGGBB" or null), line_style ("solid"|"dashed"|"dotted"|"other"), risk_label (the row name in the risk table or null)}`.
  - `risk_table`: `{times: array of number, rows: array of {label, n: array of (integer or null)}}` or null when there is no table.
  - `annotations`: `{hr (number or null), ci_lower, ci_upper, p (string or null), hr_text (verbatim string), reference_arm (string or null)}`.
  - `notes`: array of strings (things the reviewer should check).
- System prompt: read only what is printed; never estimate a number that is not printed (null instead); tick positions are the centre of the tick mark
  (or of the tick label when there is no mark) in pixels of the image sent, origin top-left; risk table numbers verbatim; one arm per legend entry.
- Positions returned in the sent image's pixels are converted to the original image's pixels in the browser (factor = original width / sent width).

### Checks (`ai_figure_check(reading)` → character vector of warnings)

- Risk table: times strictly increasing; each row has as many numbers as times; numbers never increase over time; the first time is 0 (warn if not).
- Axes: at least two ticks per axis with distinct values; log scale only with positive values; tick positions monotone with values
  (X increasing to the right; Y increasing upwards).
- Arms: risk table rows that match no arm label, and arms without a colour, are listed.
- HR: lower ≤ HR ≤ upper when all are present.

### Snapping (`www/dig_ai.js`)

- `snapTick(img, axis, px, py, opts)`: `img` = `{width, height, data}` (RGBA), `axis` = "x" or "y".
  Finds the axis line near the proposed position (the longest dark run, horizontal for x and vertical for y, within ±6% of the image size),
  then the tick mark: a short dark segment perpendicular to the axis line, touching it, within a window of ±2.5% of the image size
  around the proposed position along the axis. Returns `{px, py, snapped: true, dist}` at the tick's centre on the axis line,
  or `{px, py, snapped: false}` with the proposed position when no tick mark is found (for example axes without tick marks).
- Calibration uses the two ticks per axis that are farthest apart (more precise). Snapped points are drawn as the usual calibration crosses;
  unsnapped ones in orange with the status message "Check the orange calibration points". The existing Set X1…Y2 buttons still re-set any point.
- `nearestColour(img, hex, box)`: the colour actually present in the plot area closest to the legend colour (so tracing starts from the
  true pixel colour); used by "Use this arm" before calling the existing trace.

### AI reading panel (`ai_figure_module.R`)

- Under "Load the figure": a button **Read this figure with AI** (disabled with the reason when the AI is not ready or no image is loaded),
  the progress text, and the panel when a reading exists:
  axes summary with **Use these axes**; one row per arm with a colour swatch, label, its risk numbers and **Use this arm**;
  the HR line; the warnings from `ai_figure_check()`; the notes.
- **Use these axes**: sets the X/Y scale selects, the Y values (0-100 when `y_percent`), the curve type select, and the four calibration points
  (snapped) in the canvas state.
- **Use this arm**: fills Arm name (the legend label), Times in the table, Numbers at risk, and the HR fields when the HR text names this arm
  as the experimental one (otherwise the HR stays empty and a note says so); picks the colour with `nearestColour()`; runs the existing trace.
  The reviewer then checks the points and presses Save as today.
- The reading is kept per image (cleared when another image is loaded). Runs in the background like parts 1-2; call counted in the session usage.

## Error handling

AI errors as in part 0 (shown under the button). A figure with no legend gives arms without labels ("Curve 1", "Curve 2"). No risk table → the panel says so.
A reading whose checks fail is still shown, with the warnings above it; nothing is applied without a click.

## Testing

- `tests/test_ai_figure.R`: schema strict; prompt includes image size and rules; parsing of a fixture reading (two arms, table, HR);
  `ai_figure_check()` cases (increasing n, unequal lengths, non-monotone ticks, HR outside CI, unmatched rows); position scaling;
  server flow with the fake transport: no image → disabled; reading shown; "Use this arm" sends the right custom message (fields, colour, trace).
- `tests/test_dig_snap.js` (`node tests/test_dig_snap.js`, no browser): synthetic RGBA images with axis lines and tick marks;
  proposals off by up to ±10 px snap within 1 px; image without tick marks → `snapped: false`; distractors (gridlines, text, the curve) do not capture the snap;
  `nearestColour()` returns the drawn curve colour from a slightly different legend hex.
- Manual check in the built-in browser with a synthetic figure (tests/make_fig.R) and a fake transport: Read → Use these axes → Use this arm → trace → Save.
- `tests/test_ai_live.R` gains an optional case: a synthetic figure with a risk table → the table is read exactly.
- Every existing test keeps passing (the existing Playwright scripts need a local Playwright install and are not part of this check).

## Out of scope

AI tracing of curves, multi-panel figures (one panel at a time: crop with the existing box), reading curves from PDFs directly.
