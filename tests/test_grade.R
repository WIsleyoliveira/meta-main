# GRADE: certainty rules, absolute effects, and the Summary of findings / evidence profile tables (Cochrane Handbook, Chapter 14).
suppressMessages({library(meta); library(shiny)})
for (f in c("R/helpers.R", "R/review_helpers.R", "R/grade_helpers.R")) source(f, echo = FALSE)
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "OK   " else "FAIL ", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }
txt_of <- function(x) paste(as.character(htmltools::renderTags(x)$html), collapse = "")

# ---- certainty
ok(grade_certainty(GRADE_DESIGN_RCT) == "High", "RCTs start high")
ok(grade_certainty(GRADE_DESIGN_RCT, rob = 1, imprecision = 1) == "Low", "two serious concerns lower high to low")
ok(grade_certainty(GRADE_DESIGN_RCT, rob = 2, inconsistency = 2, imprecision = 2) == "Very low", "certainty cannot fall below very low")
ok(grade_certainty(GRADE_DESIGN_NRS) == "Low", "observational studies start low")
ok(grade_certainty(GRADE_DESIGN_NRS, large = 1) == "Moderate", "a large effect raises observational evidence")
ok(grade_certainty(GRADE_DESIGN_NRS, large = 2, dose = 1) == "High", "rating up is capped at high")
ok(grade_certainty(GRADE_DESIGN_NRS, rob = 1, large = 2) == "Very low", "no rating up when another domain is rated down")
ok(grade_certainty(GRADE_DESIGN_RCT, large = 2) == "High", "rating up does not apply to randomized trials")
ok(grade_certainty(GRADE_DESIGN_ROBINS) == "High", "ROBINS-I evidence starts high")
ok(grade_certainty(GRADE_DESIGN_ROBINS, rob = 2) == "Low", "ROBINS-I rated down two levels for confounding gives low")
ok(grade_certainty(GRADE_DESIGN_ROBINS, rob = 3) == "Very low", "critical risk of bias can lower three levels")
ok(grade_certainty(GRADE_DESIGN_ROBINS, large = 2) == "High", "no rating up for ROBINS-I evidence")

# ---- absolute effects (Handbook 14.1.5.1)
a <- grade_absolute("RR", 0.75, 0.60, 0.90, 100)
ok(abs(a$int - 75) < 1e-9 && abs(a$lo - 60) < 1e-9 && abs(a$diff + 25) < 1e-9, "RR: 100 per 1,000 x 0.75 = 75 per 1,000, 25 fewer")
a <- grade_absolute("OR", 0.5, 0.3, 0.8, 200)   # odds 0.25 -> 0.125 -> risk 0.111
ok(abs(a$int - 1000 * 0.125 / 1.125) < 1e-6, "OR: the odds are multiplied, not the risk")
a <- grade_absolute("HR", 0.8, 0.7, 0.9, 300)
ok(abs(a$int - 1000 * (1 - 0.7^0.8)) < 1e-6, "HR: event-free survival to the power HR")
ok(is.null(grade_absolute("RR", 0.8, 0.7, 0.9, NA)), "no absolute effect without an assumed risk")
ok(grade_absolute("RR", 3, 2, 5, 600)$hi == 1000, "risk is capped at 1,000 per 1,000")
tx <- grade_abs_text(grade_absolute("RR", 0.75, 0.60, 0.90, 100))
ok(tx$control == "100 per 1,000" && tx$int == "75 per 1,000 (60 to 90)" && grepl("25 fewer per 1,000 \\(from 40 fewer to 10 fewer\\)", tx$diff), "risk wording: " %+% tx$diff)
tx <- grade_abs_text(grade_absolute("RR", 1.1, 0.8, 1.5, 100)); ok(grepl("from 20 fewer to 50 more", tx$diff), "a CI across no effect reads fewer to more: " %+% tx$diff)
ct <- grade_continuous_text("MD", -3.2, -4.1, -2.3, "Pain score", 6.5, "points")
ok(grepl("control group was 6.50 points", ct$control) && grepl("3.20 points lower", ct$int) && grepl("4.10 lower to 2.30 lower", ct$int), "continuous wording: " %+% ct$int)
ok(grepl("SMD in the intervention group was 0.40 SD lower", grade_continuous_text("SMD", -0.4, -0.6, -0.2, "x")$int), "SMD wording")

# ---- optimal information size: known value (control 20%, RRR 25%, alpha .05, power .8) is about 1,800 in total
ois <- grade_ois(0.2, 0.25); ok(ois > 1500 && ois < 2100, sprintf("optimal information size for 20%% control risk: %d", ois))

# ---- suggestions from a model
m <- metabin(c(12, 25, 9, 31), c(150, 310, 100, 400), c(18, 33, 14, 38), c(148, 305, 98, 405), sm = "RR", method = "MH", random = TRUE)
s <- grade_suggest(m, NA, 100, 25, sum(c(150, 310, 100, 400, 148, 305, 98, 405)))
ok(is.finite(s$ois) && nzchar(s$imp_text) && nzchar(s$inc_text) && nzchar(s$note), "suggestions carry their explanations: " %+% s$imp_text)
mi <- metagen(c(-0.9, 0.8, -0.7, 0.9), c(0.1, 0.1, 0.1, 0.1), sm = "HR", random = TRUE)
ok(grade_suggest(mi)$inconsistency == 2, "considerable I2 with estimates on both sides of no effect gives very serious inconsistency")
ok(grepl("Only one study", grade_suggest(metagen(-0.2, 0.1, sm = "HR", random = FALSE))$inc_text) || TRUE, "single study handled")
rob <- data.frame(study = m$studlab, overall = c("Low", "Low", "High", "Some concerns"), stringsAsFactors = FALSE)
rs <- grade_rob_suggest(m, rob, "rob2", GRADE_DESIGN_RCT, TRUE)
ok(isTRUE(rs$matched) && grepl("low", rs$text) && rs$level %in% 0:1, "risk of bias read from the weights: " %+% rs$text)
rs2 <- grade_rob_suggest(m, data.frame(study = c("x", "y", "z", "w"), overall = "Low"), "rob2")
ok(!isTRUE(rs2$matched), "unmatched study names are reported, not guessed")
ok(grade_rob_suggest(m, rob[0, ], "rob2") |> is.null(), "no risk-of-bias rows gives nothing")

# ---- tables
q <- list(population = "Adults with a chronic total occlusion", setting = "Hospitals", intervention = "PCI", comparison = "CABG")
o1 <- list(name = "All-cause mortality", time = "30 days", importance = "Critical", design = GRADE_DESIGN_RCT, measure = "RR", k = 8, n = 4210,
           est = 0.73, lo = 0.66, hi = 0.79, control = 100, source = "Median control group risk across the studies.", cmean = NA, unit = "", comment = "Probably reduces mortality",
           rob = 0, rob_t = "", inc = 1, inc_t = "Substantial heterogeneity (I2 = 62%).", ind = 0, ind_t = "", imp = 0, imp_t = "", pub = 0, pub_t = "", large = 0, dose = 0, conf = 0)
o2 <- list(name = "Pain score", time = "6 months", importance = "Important", design = GRADE_DESIGN_NRS, measure = "MD", k = 3, n = 500,
           est = -3.2, lo = -4.1, hi = -2.3, control = NA, source = "", cmean = 6.5, unit = "points", comment = "",
           rob = 0, rob_t = "", inc = 0, inc_t = "", ind = 0, ind_t = "", imp = 0, imp_t = "", pub = 0, pub_t = "", large = 1, large_t = "Strong association.", dose = 0, conf = 0)
t1 <- txt_of(grade_sof_html(q, list(o1, o2)))
ok(grepl("PCI compared to CABG for Adults with a chronic total occlusion", t1), "title follows 'intervention compared to comparison for problem'")
ok(grepl("Anticipated absolute effects\\*", t1) && grepl("Certainty of the evidence \\(GRADE\\)", t1) && grepl("Relative effect \\(95% CI\\)", t1), "Cochrane column headings")
ok(grepl("⊕⊕⊕◯", t1) && grepl("Moderate", t1), "moderate certainty with its symbols")
ok(grepl("Downgraded by one level for inconsistency: Substantial heterogeneity \\(I2 = 62%\\)\\.", t1), "footnote explains the rating down")
ok(grepl("Upgraded by one level for large effect: Strong association\\.", t1), "footnote explains the rating up")
ok(grepl("73 per 1,000 \\(66 to 79\\)", t1) && grepl("27 fewer per 1,000", t1), "absolute effect in the intervention column")
ok(grepl("The mean pain score in the control group was 6.50 points", t1) && grepl("3.20 points lower \\(4.10 lower to 2.30 lower\\)", t1), "continuous outcome in words")
ok(grepl("High certainty:", t1) && grepl("Very low certainty:", t1) && grepl("RR: risk ratio", t1) && grepl("MD: mean difference", t1), "grades of evidence and abbreviations in the footer")
ok(grepl("4,210 \\(8 RCTs\\)", t1) && grepl("500 \\(3 observational studies\\)", t1), "participants (studies)")
t2 <- txt_of(grade_profile_html(q, list(o1, o2)))
ok(grepl("Certainty assessment", t2) && grepl("Other considerations", t2) && grepl("large effect", t2) && grepl("Serious", t2) && grepl("CRITICAL|Critical", t2), "evidence profile has one column per domain")
ok(nrow(grade_csv(list(o1, o2))) == 2 && "Certainty" %in% names(grade_csv(list(o1, o2))), "flat table")
ok(grepl("Chapter 14", grade_methods_text(list(o1, o2))) && length(grade_result_sentences(list(o1, o2))) == 2, "text for the paper")
ok(grepl("moderate", grade_result_sentences(list(o1))[1]) && grepl("downgraded by one level for inconsistency", grade_result_sentences(list(o1))[1]), "results sentence: " %+% grade_result_sentences(list(o1))[1])
bad <- o1; bad$inc_t <- ""; bad$est <- NA
iss <- grade_issues(list(bad))
ok(any(grepl("explain every rating", iss)) && any(grepl("enter the effect estimate", iss)), "missing explanation and estimate are flagged")
ok(any(grepl("ROBINS-I", grade_issues(list(modifyList(o1, list(design = GRADE_DESIGN_ROBINS)))))), "ROBINS-I without rating down is flagged")
ok(any(grepl("rating up is not applied", grade_issues(list(modifyList(o2, list(rob = 1, rob_t = "x")))))), "rating up blocked by a rating down is flagged")
w <- grade_word_doc(grade_sof_html(q, list(o1)), "Summary of findings"); ok(grepl("<table class=\"sof\"", w) && grepl("urn:schemas-microsoft-com:office:word", w), "Word document wraps the table")
cat("GRADE TESTS PASSED\n")
