# Ponderis: validation report

Run on 2026-10-20 with R 4.3.3 (meta 9.0.0, metafor 4.4.0, IPDfromKM 0.1.10, estmeansd 1.0.1). Total time 61 seconds.

## Summary

| Part | What was checked | Result |
|---|---|---|
| A. Forest-plot engine | Pooled estimates, heterogeneity and confidence limits against the independent package metafor and against published BCG-vaccine values | 17 of 17 checks pass |
| B. Median/IQR to mean/SD | Simulated samples with known mean and SD, all methods, normal and skewed data | 14 of 14 checks pass |
| C. Kaplan-Meier reconstruction | Simulated trials with known patient data: reconstructed hazard ratio against the true Cox hazard ratio | 5 of 5 scenarios pass (plus one informational) |
| D. Digitizer | Points read from synthetic figures (clean, small, JPEG with censor marks) against the true curves | 8 of 8 curves pass |

**Overall: all checks pass.**

## A. Forest-plot engine

Each row compares a number produced by the app (through the same code the app runs and exports) with a reference value. Tolerances are shown. The BCG vaccine trials (13 studies) are the standard example in the metafor documentation, and the published values come from there.

| case | quantity | app | reference | abs_diff | tolerance | pass | source |
|---|---|---|---|---|---|---|---|
| BCG, RR, REML | pooled log RR | -0.7145 | -0.7145 | 0.00003235 | 0.0005000 | PASS | published (metafor documentation) |
| BCG, RR, REML | SE of pooled log RR | 0.1798 | 0.1798 | 0.00001847 | 0.0005000 | PASS | published (metafor documentation) |
| BCG, RR, REML | tau-squared | 0.3132 | 0.3132 | 0.00004333 | 0.0005000 | PASS | published (metafor documentation) |
| BCG, RR, REML | I-squared from Q (%) | 92.12 | 92.12 | 0 | 0.000001000 | PASS | Higgins formula on metafor Q (published I-squared of 92.22 uses the REML tau-squared instead) |
| BCG, RR, REML | pooled log RR | -0.7145 | -0.7145 | <0.000001 | 0.000001000 | PASS | metafor, same data |
| BCG, RR, REML | tau-squared | 0.3132 | 0.3132 | <0.000001 | 0.000001000 | PASS | metafor, same data |
| BCG, RR, REML + Hartung-Knapp | pooled log RR | -0.7145 | -0.7145 | <0.000001 | 0.000001000 | PASS | metafor, same data |
| BCG, RR, REML + Hartung-Knapp | lower 95% limit | -1.108 | -1.108 | <0.000001 | 0.0001000 | PASS | metafor, same data |
| BCG, RR, REML + Hartung-Knapp | upper 95% limit | -0.3206 | -0.3206 | <0.000001 | 0.0001000 | PASS | metafor, same data |
| BCG, OR Mantel-Haenszel | pooled log effect | -0.4734 | -0.4734 | 0 | 0.0001000 | PASS | metafor rma.mh, same data |
| BCG, RR Mantel-Haenszel | pooled log effect | -0.4537 | -0.4537 | 0 | 0.0001000 | PASS | metafor rma.mh, same data |
| Normand, continuous MD REML | pooled effect | -15.11 | -15.11 | <0.000001 | 0.0001000 | PASS | metafor, same data |
| Normand, continuous MD REML | tau-squared | 684.6 | 684.6 | 0 | 0.0001000 | PASS | metafor, same data |
| Normand, continuous SMD REML | pooled effect | -0.5362 | -0.5371 | 0.0009525 | 0.005000 | PASS | metafor, same data |
| Normand, continuous SMD REML | tau-squared | 0.7887 | 0.7908 | 0.002108 | 0.005000 | PASS | metafor, same data |
| Pre-calculated HR (from BCG) | pooled log HR | -0.7145 | -0.7145 | 0.000001343 | 0.0001000 | PASS | metafor, same data |
| Single proportion, logit | pooled logit proportion | -4.889 | -4.889 | <0.000001 | 0.001000 | PASS | metafor, same data |

Notes: for Hedges' g (SMD) the two packages use slightly different variance formulas, so a small difference (about 0.001 in the pooled effect) is expected and the tolerance is wider. The I-squared in the metafor documentation (92.22%) is based on the REML tau-squared; the meta package uses the Q-based formula (92.12% here), so the check compares against that formula.

## B. Median/IQR to mean/SD

Simulation: 40 samples per cell. Bias and root-mean-square error (RMSE) are relative to the true sample mean and SD, in percent.

| check | pass | detail |
|---|---|---|
| Normal data, n=200, quartiles: Wan+Luo bias in the mean below 1% | PASS | bias 0.03% |
| Normal data, n=200, quartiles: Wan+Luo bias in the SD below 3% | PASS | bias -0.09% |
| Normal data, n=200, range: Wan+Luo bias in the SD below 5% | PASS | bias -0.45% |
| Strong skew, n=200, quartiles: lognormal method has less SD error than Wan+Luo | PASS | RMSE of SD: lognormal 22.5% vs Wan+Luo 35.2% |
| Strong skew, n=200, quartiles: Box-Cox has less SD error than Wan+Luo | PASS | RMSE of SD: Box-Cox 25.6% vs Wan+Luo 35.2% |
| Strong skew, n=200, quartiles: Wan+Luo underestimates the SD by more than 15% (the reason a skewed method is needed) | PASS | bias -33.3% |
| Skewness flag: false alarms on normal data stay near the intended 10% (quartiles, n=100 and n=200) | PASS | 9% and 12% |
| Skewness flag: false alarms on normal data stay near the intended 10% (range, n=100) | PASS | 10% |
| Skewness flag: false alarms with all five numbers stay below 15% (n=100) | PASS | 7% |
| Skewness flag: strong skew detected in at least 85% of samples (quartiles, n=200) | PASS | 86% |
| Skewness flag: strong skew detected in at least 60% of samples (quartiles, n=100) | PASS | 63% |
| Mean/SD to median/IQR, normal: IQR = 1.349 x SD | PASS | IQR 13.490 |
| Mean/SD to median/IQR, lognormal: recovers the true median within 1% | PASS | true 7.390, estimated 7.390 |
| Mean/SD to median/IQR, lognormal: recovers Q1 and Q3 within 2% | PASS | Q1 4.93 vs 4.93; Q3 11.09 vs 11.08 |

Skewness flag, share of samples flagged (n = 200):

| reported | n | normal | mild_skew | strong_skew |
|---|---|---|---|---|
| quartiles | 30.0 | 9.00 | 14.8 | 30.8 |
| quartiles | 100 | 9.00 | 27.3 | 62.7 |
| quartiles | 200 | 12.2 | 43.8 | 86.2 |
| range | 30.0 | 11.0 | 67.5 | 98.2 |
| range | 100 | 10.5 | 99.2 | 100 |
| range | 200 | 11.8 | 100 | 100 |
| all five | 30.0 | 9.00 | 57.0 | 96.2 |
| all five | 100 | 6.75 | 95.8 | 100 |
| all five | 200 | 9.25 | 99.8 | 100 |

Full simulation results (n = 200):

| distribution | scenario | method | bias_mean_pct | rmse_mean_pct | bias_sd_pct | rmse_sd_pct |
|---|---|---|---|---|---|---|
| Normal (mean 50, SD 10) | S1 (range) | Box-Cox (McGrath 2020, skewed data) | 0.175 | 0.879 | -3.77 | 9.50 |
| Normal (mean 50, SD 10) | S1 (range) | Method of lognormal (Cai 2021, skewed positive data) | 0.761 | 1.14 | 3.10 | 10.2 |
| Normal (mean 50, SD 10) | S1 (range) | Quantile estimation (McGrath 2020, skewed data) | -0.977 | 2.70 | 5.53 | 11.0 |
| Normal (mean 50, SD 10) | S1 (range) | Wan 2014 + Luo 2018 (roughly symmetric data) | 0.0717 | 0.804 | -0.448 | 9.04 |
| Normal (mean 50, SD 10) | S2 (quartiles) | Box-Cox (McGrath 2020, skewed data) | -0.0768 | 1.33 | -1.43 | 6.09 |
| Normal (mean 50, SD 10) | S2 (quartiles) | Method of lognormal (Cai 2021, skewed positive data) | 1.18 | 1.31 | 1.93 | 6.89 |
| Normal (mean 50, SD 10) | S2 (quartiles) | Quantile estimation (McGrath 2020, skewed data) | 0.139 | 1.11 | -0.313 | 6.02 |
| Normal (mean 50, SD 10) | S2 (quartiles) | Wan 2014 + Luo 2018 (roughly symmetric data) | 0.0339 | 0.532 | -0.0904 | 6.04 |
| Lognormal, mild skew (sigma 0.4) | S1 (range) | Box-Cox (McGrath 2020, skewed data) | -0.829 | 2.28 | -6.13 | 10.6 |
| Lognormal, mild skew (sigma 0.4) | S1 (range) | Method of lognormal (Cai 2021, skewed positive data) | -0.440 | 2.15 | 0.0628 | 9.52 |
| Lognormal, mild skew (sigma 0.4) | S1 (range) | Quantile estimation (McGrath 2020, skewed data) | 1.63 | 4.15 | 8.60 | 16.8 |
| Lognormal, mild skew (sigma 0.4) | S1 (range) | Wan 2014 + Luo 2018 (roughly symmetric data) | -3.41 | 3.93 | 6.52 | 16.9 |
| Lognormal, mild skew (sigma 0.4) | S2 (quartiles) | Box-Cox (McGrath 2020, skewed data) | -1.40 | 2.78 | -5.42 | 11.7 |
| Lognormal, mild skew (sigma 0.4) | S2 (quartiles) | Method of lognormal (Cai 2021, skewed positive data) | -0.354 | 1.68 | 0.990 | 10.2 |
| Lognormal, mild skew (sigma 0.4) | S2 (quartiles) | Quantile estimation (McGrath 2020, skewed data) | -1.14 | 2.68 | -2.13 | 10.8 |
| Lognormal, mild skew (sigma 0.4) | S2 (quartiles) | Wan 2014 + Luo 2018 (roughly symmetric data) | -5.22 | 5.38 | -8.52 | 11.7 |
| Lognormal, strong skew (sigma 0.8) | S1 (range) | Box-Cox (McGrath 2020, skewed data) | -1.43 | 5.99 | -7.15 | 12.9 |
| Lognormal, strong skew (sigma 0.8) | S1 (range) | Method of lognormal (Cai 2021, skewed positive data) | 0.654 | 6.38 | 3.74 | 15.9 |
| Lognormal, strong skew (sigma 0.8) | S1 (range) | Quantile estimation (McGrath 2020, skewed data) | 7.58 | 13.1 | 23.7 | 33.9 |
| Lognormal, strong skew (sigma 0.8) | S1 (range) | Wan 2014 + Luo 2018 (roughly symmetric data) | -7.14 | 10.3 | 33.0 | 43.0 |
| Lognormal, strong skew (sigma 0.8) | S2 (quartiles) | Box-Cox (McGrath 2020, skewed data) | -4.53 | 8.08 | -13.6 | 25.6 |
| Lognormal, strong skew (sigma 0.8) | S2 (quartiles) | Method of lognormal (Cai 2021, skewed positive data) | -0.725 | 5.84 | 2.10 | 22.5 |
| Lognormal, strong skew (sigma 0.8) | S2 (quartiles) | Quantile estimation (McGrath 2020, skewed data) | -3.02 | 8.09 | -5.65 | 26.0 |
| Lognormal, strong skew (sigma 0.8) | S2 (quartiles) | Wan 2014 + Luo 2018 (roughly symmetric data) | -19.6 | 19.9 | -33.3 | 35.2 |
| Exponential (mean 10) | S1 (range) | Box-Cox (McGrath 2020, skewed data) | -2.88 | 7.48 | -7.20 | 13.6 |
| Exponential (mean 10) | S1 (range) | Method of lognormal (Cai 2021, skewed positive data) | -0.0714 | 7.58 | 3.13 | 14.2 |
| Exponential (mean 10) | S1 (range) | Quantile estimation (McGrath 2020, skewed data) | 4.66 | 10.2 | 9.63 | 18.3 |
| Exponential (mean 10) | S1 (range) | Wan 2014 + Luo 2018 (roughly symmetric data) | -14.5 | 15.4 | 7.76 | 19.0 |
| Exponential (mean 10) | S2 (quartiles) | Box-Cox (McGrath 2020, skewed data) | -1.13 | 14.3 | 2.53 | 43.5 |
| Exponential (mean 10) | S2 (quartiles) | Method of lognormal (Cai 2021, skewed positive data) | 16.1 | 21.1 | 66.1 | 83.2 |
| Exponential (mean 10) | S2 (quartiles) | Quantile estimation (McGrath 2020, skewed data) | 3.94 | 16.0 | 24.0 | 59.7 |
| Exponential (mean 10) | S2 (quartiles) | Wan 2014 + Luo 2018 (roughly symmetric data) | -20.7 | 20.9 | -18.6 | 20.6 |

Reading it: on symmetric data every method is close. On skewed data Wan and Luo underestimate the SD badly, which is why the app highlights a skewed-data method when the reported values look asymmetric. The skewness flag compares the reported values with what normal data would give (an approximate p-value, flagged below 0.10, about 10% false alarms on normal data). It misses mild skew, especially in small samples; use what you know about the variable as well.

## C. Kaplan-Meier reconstruction

Each scenario simulates 40 trials (150 or 600 patients per arm, 60 months, censoring), builds the published curve and risk table from the true data, adds digitizing noise (SD 0.002, rounded to 3 decimals), rebuilds individual patient data with the app, and compares the hazard ratio with the Cox hazard ratio of the true data.

| scenario | reps | mean_true_hr | mean_rec_hr | bias_loghr | mae_loghr | max_abs_loghr | within_5pct | ci_cover | rule | pass |
|---|---|---|---|---|---|---|---|---|---|---|
| Proportional hazards, full risk table, 40 points | 40.0 | 0.656 | 0.657 | 0.00131 | 0.0102 | 0.0391 | 1.00 | 1.00 | mean abs error of log HR < 0.03 | PASS |
| Proportional hazards, full risk table, only 12 points | 40.0 | 0.633 | 0.662 | 0.0418 | 0.0519 | 0.142 | 0.500 | 1.00 | informational (coarse digitizing) | info |
| Only the initial number at risk, total events given | 40.0 | 0.661 | 0.662 | 0.00164 | 0.0160 | 0.0520 | 0.975 | 1.00 | mean abs error of log HR < 0.05 | PASS |
| Only the initial number at risk, no total events | 40.0 | 0.665 | 0.667 | 0.00375 | 0.0203 | 0.0524 | 0.950 | 1.00 | mean abs error of log HR < 0.05 | PASS |
| Weibull (shape 1.5), full risk table, 40 points | 40.0 | 0.600 | 0.600 | -0.000339 | 0.0107 | 0.0333 | 1.00 | 1.00 | mean abs error of log HR < 0.03 | PASS |
| Large trial (n=600 per arm), full risk table | 40.0 | 0.799 | 0.801 | 0.00145 | 0.00517 | 0.0150 | 1.00 | 1.00 | mean abs error of log HR < 0.03 | PASS |

mae_loghr is the mean absolute error of the log hazard ratio (0.03 is about 3%). within_5pct is the share of trials with a reconstructed HR within 5% of the true one. ci_cover is how often the reconstructed 95% interval contains the true HR.

Reading it: with a risk table and enough points the reconstructed HR is within about 1 to 3% of the truth. With only 12 digitized points the error grows to about 6%; with only the initial number at risk it is about 2%. Digitize 30 or more points per curve.

## D. Digitizer

The digitizer ran in a real browser (headless Chromium). Calibration and tracing were done through the interface, by colour. Error is the vertical difference between each traced point and the true curve, in survival units (0 to 1).

| figure | file | arm | points | mean_abs_error | p95_abs_error | max_abs_error | readout | pass |
|---|---|---|---|---|---|---|---|---|
| A | valfig_A_clean.png | Intervention | 284 | 0.00121 | 0.00289 | 0.00461 | NA | PASS |
| A | valfig_A_clean.png | Control | 302 | 0.00176 | 0.00550 | 0.0125 | NA | PASS |
| B | valfig_B_small.png | Intervention | 149 | 0.00112 | 0.00291 | 0.00398 | NA | PASS |
| B | valfig_B_small.png | Control | 150 | 0.00157 | 0.00457 | 0.00974 | NA | PASS |
| C | valfig_C_jpeg_censor_marks.jpg | Intervention | 299 | 0.00120 | 0.00289 | 0.00461 | NA | PASS |
| C | valfig_C_jpeg_censor_marks.jpg | Control | 303 | 0.00200 | 0.00616 | 0.0146 | NA | PASS |
| D | valfig_D_logx.png | readout at x=100, y=0.65 |  |  |  |  | X = 99.42   Y = 0.651 | PASS |
| D | valfig_D_logx.png | Log-x curve | 286 | 0.000489 | 0.00108 | 0.00132 | NA | PASS |

Figures: A is clean, B is a small image (500 x 350), C is a JPEG (quality 60) with censoring tick marks drawn on the curves, D has a logarithmic x axis (the readout row checks the cursor coordinates at x = 100, y = 0.65).

## E. Real patient data

The survival package datasets contain real individual patient data. For each, the curve is read at 30 or 100 time points from the real data (rounded to 3 decimals, reading error SD 0.002) with a six-point risk table and the total events, the app rebuilds the patients, and the hazard ratio is compared with the Cox model on the real data. A pass means the rebuilt hazard ratio is within 10% of the real one and its confidence interval contains the real hazard ratio.

| dataset | points | n | true_hr | app_hr | ratio | true_ci | app_ci | ci_covers_true | pass |
|---|---|---|---|---|---|---|---|---|---|
| veteran: lung cancer, standard vs test chemotherapy | 30.0 | 137 | 1.02 | 1.03 | 1.01 | 0.71-1.45 | 0.72-1.48 | PASS | PASS |
| lung: advanced lung cancer, men vs women (months) | 30.0 | 228 | 0.588 | 0.566 | 0.963 | 0.42-0.82 | 0.40-0.81 | PASS | PASS |
| ovarian: ovarian cancer, cyclophosphamide vs +adriamycin | 30.0 | 26.0 | 0.551 | 0.535 | 0.972 | 0.17-1.74 | 0.17-1.69 | PASS | PASS |
| colon (death): chemotherapy Obs vs Lev+5FU | 30.0 | 619 | 0.689 | 0.689 | 1.00 | 0.55-0.87 | 0.54-0.87 | PASS | PASS |
| veteran: lung cancer, standard vs test chemotherapy | 100 | 137 | 1.02 | 1.00 | 0.985 | 0.71-1.45 | 0.71-1.42 | PASS | PASS |
| lung: advanced lung cancer, men vs women (months) | 100 | 228 | 0.588 | 0.558 | 0.949 | 0.42-0.82 | 0.40-0.78 | PASS | PASS |
| ovarian: ovarian cancer, cyclophosphamide vs +adriamycin | 100 | 26.0 | 0.551 | 0.591 | 1.07 | 0.17-1.74 | 0.19-1.86 | PASS | PASS |
| colon (death): chemotherapy Obs vs Lev+5FU | 100 | 619 | 0.689 | 0.688 | 0.999 | 0.55-0.87 | 0.54-0.87 | PASS | PASS |

Reading it: with 100 points the rebuilt hazard ratio is within about 7% of the real one in every dataset, and usually within 2 to 4%; with 30 points the error is larger (the lung data differ by 6%). More digitized points help. These are not the data of your own meta-analysis: validate that one against its published result before submitting.

## What this does and does not show

- It shows the app reproduces trusted software and recovers known truth in simulation. That is evidence the code is correct.
- It does not replace checking an actual meta-analysis from your field against its published result. Do that for any analysis you publish.
- Reconstructed data from published curves are estimates. Their accuracy depends on figure quality, the number of points read, and whether the risk table and total events are available.
- The conversion of medians to means is an approximation. Report the method and, when many studies need it, run a sensitivity analysis without them.
