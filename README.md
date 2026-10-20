# Ponderis

A small web app (R + Shiny) that turns a CSV of study results into a forest plot.
The user downloads a CSV template, fills in one row per study, uploads it, picks the
analysis, and gets the plot, leave-one-out analysis, funnel plot with bias test, a
methods paragraph, and the exact R script that reproduces everything.

The app is named Ponderis (from Latin pondus, weight). The name is set in `app.R`.

## Run it on your computer

1. Install R (4.1 or later) from https://cran.r-project.org
2. Easiest: double-click `run_app.bat` (Windows) or `run_app.command` (Mac). Or in R: `source("run_app.R")`.
   It checks the packages, installs the missing ones, and opens the app in your browser.
3. Manual way, in R from this folder:

   ```r
   source("install_packages.R")   # once
   shiny::runApp()
   ```

## What v1 does

| Data type | Columns in the template | Measures |
|---|---|---|
| Dichotomous | `events_tx`, `n_tx`, `events_ctrl`, `n_ctrl` | RR, OR, RD (Mantel-Haenszel, inverse variance, Peto) |
| Continuous | `mean_tx`, `sd_tx`, `n_tx`, `mean_ctrl`, `sd_ctrl`, `n_ctrl` | MD, SMD (Hedges' g) |
| Pre-calculated effect | `effect`, `ci_lower`, `ci_upper` (or `se`) | HR, OR, RR, MD, SMD, other |
| Single proportion | `events`, `n` | logit, Freeman-Tukey, arcsine, raw |

Every template also has `study_id`, and optional `year` and `subgroup`.

Analysis options: common or random effects, REML / DerSimonian-Laird / Paule-Mandel,
Hartung-Knapp adjustment, prediction interval, subgroups, ordering by year, effect or
weight, four plot styles (default, RevMan 5, JAMA, BMJ), leave-one-out, funnel plot with
Egger, Begg or Thompson-Sharp test, and trim-and-fill.

Also in the forest-plot mode: **meta-regression** (any numeric column of your file, or a column with 2 to 6 groups, with a bubble plot, coefficients, a ready paragraph and the R code), **influence** (Baujat plot, Galbraith plot and influence diagnostics from metafor), and TIFF export (300 dpi) next to PNG, PDF and SVG.

**Review tools** (own mode): PRISMA 2020 flow diagram (type the counts; the rest is derived and checked), risk of bias (RoB 2, ROBINS-I, Newcastle-Ottawa; traffic-light plot, summary bars, table) and GRADE (certainty per outcome, summary-of-findings table, suggestions taken from the forest-plot analysis).

Not included: network meta-analysis, diagnostic accuracy, trial sequential analysis.

## Kaplan-Meier mode (pooled curve)

Switch to "Pooled Kaplan-Meier curve from published curves" at the top of the page. The mode
rebuilds individual patient data from published curves (Guyot et al. 2012, through the
IPDfromKM package), pools all studies, and draws one curve per group.

Three separate files, each with a downloadable template:

| File | Columns | What goes in it |
|---|---|---|
| Curve points | `study_id`, `arm`, `time`, `survival` | points read from each published curve (WebPlotDigitizer exports this layout) |
| Numbers at risk | `study_id`, `arm`, `time`, `n_risk` | the table under each curve |
| Arms | `study_id`, `arm`, `group`, `n_total`, `total_events` | `group` is treatment or control; `total_events` is optional but improves accuracy |

Optional columns in the arms file, on the treatment row: `hr_published`, `hr_ci_lower`, `hr_ci_upper`
(the hazard ratio the paper reports), to compare with the reconstructed one.

Tabs and what they contain:

| Tab | Content |
|---|---|
| Pooled curve | pooled KM curve with confidence bands and reconstructed numbers at risk |
| By study | one panel per study with the digitized points over the reconstructed curve |
| Summary | patients, events, person-time, event rate, median follow-up (reverse KM); median survival, restricted mean survival time (RMST) and difference; survival and absolute difference at fixed times |
| Hazard ratios | pooled HR (Cox stratified by study), stratified log-rank, proportional hazards test, HR per study against the published HR, and a button that sends the HRs (reconstructed, published, or published where available) to the forest plot |
| Non-proportional hazards | landmark analysis (HR before and after a time point, curves restart), HR by time interval, HR over time (Cox with natural-spline time-varying effect) with a test of constant effect, log-log plot |
| Sensitivity | stratified vs unstratified Cox, gamma frailty, two-stage meta-analysis (common and random effects, Hartung-Knapp), published-HR variants, shorter follow-up, optional reconstruction variants (without total events, without numbers at risk), heterogeneity and prediction interval, leave-one-study-out, reconstructed vs published |
| Report text | Methods and Results paragraphs and a reference list, written from the analysis on screen (copy button and .txt download). Fill in the *Outcome* field in step 3 so the text names it |
| Check | reconstructed vs reported events and numbers at risk, error measures per arm |

Every plot has PNG and PDF downloads; the R script reproduces every table and figure and writes them to files.

Things to keep in mind: reconstruction is approximate and only as good as the digitized points; the
hazard ratio assumes proportional hazards (the app warns when the test is significant, and restricted
mean survival does not need that assumption); when a paper reports its own hazard ratio, prefer it.
The two-stage analyses use `meta`. Install `IPDfromKM` and `survival` from CRAN (`install_packages.R` does this). `IPDfromKM` must be
attached with `library()`, which the app and the exported script both do.


Same study and arm name with another subgroup: the curve is kept as a separate one, named `<arm> (<subgroup>)`. Saving again with the same (or an empty) subgroup replaces the earlier curve, and the app says so.

## Digitizer mode (read points off a figure)

Switch to "Digitize a curve from an image". It works like WebPlotDigitizer, inside the app:

1. Load the figure (file, Ctrl+V paste, or drag and drop). The image stays in your browser.
2. Calibrate: type the value of a tick on each axis (X1, X2, Y1, Y2), press its Set button and click that tick.
3. Trace: click along the curve, or pick the curve colour and press "Trace by colour". Erase stray points (censoring marks, legend).
4. Under "Details of this curve" type what you have: a subgroup label (free text), the patients in the arm, total events (optional), the times and the numbers at risk printed under the figure, and the published hazard ratio (optional). Press "Save this curve and its details": the curve, the risk table and the arm size are saved together. Anything left empty can be added later in step 4 "Edit a saved curve". Whether a curve is treatment or control is NOT decided here.
5. Repeat for every curve of every study. Everything stays in the library under "Saved so far", with one row per curve (study, arm, subgroup, patients, events, points).
6. "Compare groups": give the comparison a name, select curves in the library table, send them to the treatment or the control side, name the unit and press "Add this unit to the comparison". A unit is one row of the meta-analysis. Add more units for more studies or strata. Press "Use this comparison in the Kaplan-Meier analysis". The same saved curves can be used in as many comparisons as you like (they are kept with the project), so a new question needs no new digitizing.

Several curves on one side of a unit are pooled: their patients are rebuilt and put together, which is exact (unlike averaging curves). Example, sex mismatch: units "Male recipients" (F-M against M-M) and "Female recipients" (M-F against F-F) give the pooled effect adjusted for recipient sex; one unit with F-M and M-F on the treatment side and M-M and F-F on the control side gives the crude pooled comparison, which mixes recipient sex with the effect (the proportion of male recipients differs between the sides). The app warns when the same curve is used in more than one unit (those units are not independent). "Name of the treatment/control side in the plots" sets the names used in the plots and the report.

Figures with different conventions can be saved in the same library: whatever the figure shows (percent or proportion, survival or cumulative incidence) is stored as survival proportion, and "Multiply the times by" (for example 12 for years to months) puts each figure in the unit of your analysis (also applied to the times of the risk table typed with the curve). Tests: `tests/test_compare.R` (comparisons against the true patients) and `tests/test_dig_compare.js` (the whole flow in the browser).

Axes can be linear or logarithmic (choose the type for each axis before calibrating). Move the cursor over the figure to read the calibrated X and Y.
"Check the rebuilt curve" draws the curve rebuilt from your points over the figure, so you can see where it departs. "Save project" / "Open project" store the digitized tables and settings in one file (the image itself is not saved).

Black-and-white figures (a solid and a dashed curve): trace and save the continuous curve first, then tick "Ignore curves already saved" and trace the dashed one.
The app skips the saved curve and the legend, and fills the stretches where the dashed curve hides behind the saved one (or between dashes) by keeping its distance from the saved curve.
On a synthetic test figure this gave a mean error of 0.004 on the dashed curve (0.001 on the continuous one). Between two dashes nothing of the curve is visible, so the app interpolates its distance from the saved curve; where the dashed curve is almost vertical (the first months) a gap can hide most of the drop, and the first few points may be off by several percent. Check the orange points by eye and correct them by clicking.

Automatic tracing goes up to 20% further right than the last x tick you calibrated (curves usually run past it); with a box drawn, it stays in the box.
Where the curve is steep (the first months of a survival curve) the trace takes a point at every pixel column instead of every few, so the early drop is not reduced to two or three points; the first point sits at the very top of the curve (survival 100%).
Tracing always begins at the start of the curve (choose "The curve begins at": top of the scale for survival curves, bottom for cumulative incidence, or where you clicked) and follows it to the right.
Points shown in orange were not seen in the figure: the curve is hidden there behind a saved curve (or between dashes), so its position is estimated from the saved curve. A line partly hidden by the saved curve is corrected for the hidden part of its width.
Automatic tracing leaves out the straight lines of the plot frame (x axis, y axis, box lines) of the same colour as the curve, even when they are thin or interrupted, and refuses any line far from where the curve is expected to be (frame, legend, text). Run `FIG=F node tests/test_dig_black.js` for the black-and-white test (FIG=E is the easier one).

Limits: accuracy depends on image resolution, and legends or censoring marks of the same colour must be erased by hand. The axes are not detected automatically.
Checked on a synthetic figure: traced survival within about 0.01 of the truth.

## Median and IQR to mean and SD (calculator)

The fourth mode converts summary statistics for a meta-analysis. Type one study, or paste a table (study, n, min, q1, median, q3, max) and download the result as CSV.
It also goes the other way, from mean and SD to median and quartiles.

- Wan 2014 (SD) with Luo 2018 (mean): roughly symmetric data. Works with quartiles, with the range, or with both.
- Box-Cox and quantile estimation (McGrath 2020) and the lognormal method (Cai 2021): skewed data. These need the R package `estmeansd`.
- The app shows all methods side by side and highlights the one that fits the skewness of what was reported. Skewness is flagged by an approximate p-value against normal data of the same size (about 10% false alarms on normal data).
- Other conversions: SE or a confidence interval to SD, and combining two groups into one mean and SD (Cochrane Handbook 6.5.2).
- "Send to forest plot" sends the converted mean and SD table to the forest-plot mode.

## Checks in the Kaplan-Meier mode

- The Check tab compares the rebuilt curve with the digitized points (mean and maximum difference, and the two medians) and warns when a curve has fewer than 30 points or does not start at time 0.
- The Sensitivity tab can repeat the reconstruction with small random reading errors added to the digitized points (digitizing-error repeats and size are set next to the button). The result shows how much digitizing error widens the confidence interval of the pooled hazard ratio, and it is added to the Report text.

## How the exported script stays faithful

The app does not draw the plot with hidden code. It builds a plain R script from the
user's choices, runs that script, and offers the same text for download. Column names
and labels typed by the user are always quoted with `deparse()`, and every menu value is
checked against a fixed list, so nothing a user types is ever run as code.

## Files

```
app.R                 interface and server
R/helpers.R           column rules, CSV reading, validation, code generator (forest plots)
R/km_helpers.R        Kaplan-Meier: file rules, validation, analysis code, plot functions
R/km_extra.R          Kaplan-Meier: arm summaries, non-proportional hazards, sensitivity analyses
R/km_report.R         written Methods and Results (Kaplan-Meier mode) and the Results paragraph (forest plot mode)
R/km_module.R         Kaplan-Meier interface and server logic
www/style.css         look and feel
install_packages.R    one-time package install
tests/                headless tests (see below)
```

## Validation

`tests/validation/` checks the numbers against independent software and simulated truth, and writes `VALIDATION_REPORT.md`:
the forest-plot engine against metafor and the published BCG-vaccine values, the median-to-mean conversions by simulation,
Kaplan-Meier reconstruction on simulated trials with known patient data, the digitizer on synthetic figures (including a log axis), and
reconstruction from real patient data in the R `survival` package (veteran, lung, ovarian, colon). It does not include your own meta-analysis:
check that one against its published result.
Exported scripts end with the R and package versions (`sessionInfo()`), so you can report them.
Run `Rscript tests/validation/run_validation.R` from the app folder (about one minute). For the digitizer part, start the app on port 4760
and run `node tests/validation/val_digitizer.js` first (needs Playwright; optional).

Further tests: `tests/test_extra.R` (meta-regression and influence), `tests/test_review.R` (PRISMA, risk of bias, GRADE) and `tests/test_review_browser.js` (the new screens in the browser).

## Navigation

Five entries in the top bar, grouped by what you are doing: Meta-analysis (forest plot, bias, regression), Survival curves step 1 (digitize and compare curves) and step 2 (pooled Kaplan-Meier analysis), Review tools, and the median/IQR calculator. In the digitizer the right side has two tabs: Figure (stays in view while you use the controls on the left) and Saved curves and comparisons.

## Tests

```
Rscript tests/test_engine.R   # all 4 data types, exported script runs on its own
Rscript tests/test_edge.R     # Excel (;) CSV in Windows-1252, bad data, injection attempts
Rscript tests/test_app.R      # Shiny server logic with shiny::testServer
Rscript tests/test_km_engine.R # Kaplan-Meier engine: accuracy against simulated trials, exported script parity
Rscript tests/test_km_extra.R  # non-proportional hazards and sensitivity code against a simulation with known truth
Rscript tests/test_km_app.R   # Kaplan-Meier mode in the Shiny server, including send-to-forest
Rscript tests/test_compare.R  # comparisons built from the saved digitizer library, pooled sides, warnings
```

Run them from this folder.

## Putting it online

Pick one:

- **shinyapps.io** (easiest): `install.packages("rsconnect")`, create a free account, then
  `rsconnect::deployApp()`. Free tier has monthly hour limits.
- **Your own server**: install Shiny Server (or ShinyProxy with Docker) and copy this folder
  into its app directory.
- **Posit Connect**: publish from RStudio.

Before launching publicly, decide on:

- **Privacy text.** The app does not store uploads (they live in the user's session and
  are deleted when it ends), and the footer says so. Keep that true on your host: do not
  enable request logging of uploaded files.
- **Limits.** Uploads are capped at 5 MB and 2000 rows (`options(shiny.maxRequestSize)` in
  `app.R`, `MAX_ROWS` in `R/helpers.R`).
- **Fonts.** The page loads Newsreader and Public Sans from Google Fonts. Without internet
  access it falls back to Georgia and a system sans-serif.

## Version note

Developed and tested against R 4.3.3 and **meta 9.0-0** (installed from the package's
GitHub repository, because CRAN was not reachable where this was built). The code uses
`forest_dims()` for exact figure sizing and falls back to an estimate if it is missing,
but other arguments (`common`, `random`, `method.random.ci`, `details`, `hetstat`,
`subgroup.name`) should be checked against the CRAN release of `meta` you install.
If a plot shows "This plot could not be drawn", run `tests/test_engine.R` to see which
argument your version does not accept.

## Design

The interface uses the "Bisturi" palette (ink blue `#1E3A5F`, steel greys, crimson only for the pooled result and errors), Atkinson Hyperlegible for text and Literata for the brand and big numbers (both loaded from Google Fonts; the app falls back to system fonts offline). Each numbered step in the left column has an **i** icon beside its title: click it to read exactly what to do in that step.

The top bar has four areas: Forest plot, Survival curves (with two steps inside: 1 Digitize curves, 2 Pooled analysis), Quality & reporting (PRISMA, risk of bias, GRADE) and Calculator. Hover over an area to read what it contains.

PRISMA flow diagram: databases are typed by name with their counts (one per line), registers and other sources and the reasons for removal before screening can be named or left out, the "reports sought / not retrieved" step and the side labels can be hidden, and the drawing follows the official PRISMA 2020 template (yellow title banner, blue stage bands, Arial-style text).

Risk of bias: for RoB 2 and ROBINS-I the figure is drawn with the robvis package (McGuinness & Higgins, Res Synth Methods 2021): traffic light with the domain key and judgement legend, and the summary bars below, in one file (PNG 300 dpi, TIFF or PDF), with an optional panel letter (A, B). Install it once with `install.packages("robvis")`; without it the app draws a simpler figure and says so. Newcastle-Ottawa keeps the simple figure.

## Interface
- **Steps** in the left rail fold: the header shows the number (a green check once the step is done) and, when folded, a one-line summary of the choices. After the data are loaded the data and upload steps fold by themselves. Optional steps start folded. Code in `R/km_module.R` (`step()`), `www/ui.js` and `www/style.css`.
- **Advanced options** (pooling method, zero cells, τ² estimator, Hartung-Knapp, plot heading, methods under the plot) are folded under each step; defaults suit most reviews.
- **Guided start**: each area starts with three numbered steps and a small preview of the result (`R/ui_extras.R`). The PRISMA screen starts with a neutral note instead of an error.
- **Survival curves** has a workflow bar: 1 Digitize curves, 2 Pooled analysis, 3 Report and export.
- **Download everything (.zip)** above the result tabs, in Forest plot (figures in PNG/PDF/SVG, text, script, data, influence table) and Survival curves (all figures, all tables, reconstructed patients, written report, script, input tables). Needs the R package `zip` (or a system zip program): `install.packages("zip")`.
- Print stylesheet (figures and text only) and a small-screen layout. Tests: `node tests/test_downloads_browser.js`, `node tests/test_km_howto_browser.js`.

## Method hints
Hovering an option in the Forest plot panel for about 0.7 s (data type, effect measure, pooling method, model, τ² estimator, Hartung-Knapp, prediction interval, style, order, bias test, trim-and-fill) shows where it is used and the best practice. The texts live in `www/hints.js`.

## Survival curves: hints and reading guides
- **Hints**: hovering for about 0.7 s over an option or the name of a field in Digitize curves and Pooled analysis (value scale, curve type, RMST horizon, landmark, interval cuts, shorter follow-up, spline flexibility, plot options, source of the hazard ratios sent to the forest plot, digitizer scales, colour tolerance, numbers at risk, and so on) shows what it does and the best practice. Texts in `www/hints.js`.
- **How to read this**: a panel at the top of Pooled curve, By study, Summary, Hazard ratios, Non-proportional hazards, Sensitivity and Check. It reads the user's own numbers (curves above/below/crossing, HR and CI, RMST, landmark and interval HRs, sensitivity range, reconstruction quality) and explains the figure. Code in `R/km_howto.R`. Tests: `Rscript tests/test_km_howto.R` and `node tests/test_km_howto_browser.js`.

## Code tab and reading guides
- **Code** (Forest plot area): the analysis command, the forest plot command, leave-one-out/funnel/bias commands, the R, meta and metafor versions with a ready-to-paste sentence and references, and the complete script (copy or download). Code is in `R/howto.R`.
- **How to read this**: a collapsible panel above the forest plot, leave-one-out, influence, funnel and meta-regression. It gives a reading of the user's own numbers and a short guide to the figure. It opens the first time and then remembers the choice. Tested by `tests/test_howto.R`.

## GRADE (Quality & reporting)
Follows Chapter 14 of the Cochrane Handbook. One block per outcome (at most 7) with the study design (randomized: start high; observational: start low, or start high when assessed with ROBINS-I), the effect with its 95% CI, an assumed comparator risk, and the five domains with a written explanation for each rating (it becomes the footnote). "Add my forest-plot analysis as an outcome" fills the effect, numbers, assumed risk and suggests inconsistency (I² and direction), imprecision (CI and optimal information size), publication bias (funnel test) and risk of bias (weight of studies at each risk, read from the Risk of bias tab when the study names match). Output: Summary of findings table (title, absolute effects per 1,000 or in words for continuous outcomes, certainty with symbols, lettered footnotes, definitions of the four grades), evidence profile, text for Methods and Results, Word (.doc) and CSV. Code in `R/grade_helpers.R` and `R/grade_module.R`, tests in `tests/test_grade.R`.
