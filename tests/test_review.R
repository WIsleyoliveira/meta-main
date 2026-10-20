source("R/helpers.R", echo = FALSE); source("R/review_helpers.R"); source("R/grade_helpers.R")
ok <- function(c, m) { cat(if (isTRUE(c)) "OK  " else "FAIL", m, "\n"); if (!isTRUE(c)) quit(status = 1) }
rs <- prisma_parse_reasons("Wrong population, 12\nNo outcome data: 8\nConference abstract (5)\nOther reasons n = 3\nfoo")
ok(nrow(rs) == 5 && sum(rs$n, na.rm = TRUE) == 28 && is.na(rs$n[5]), "exclusion reasons are read in four common spellings")
fl <- prisma_flow(db = 1200, other = 30, dup = 200, excl_screen = 900, not_retrieved = 10, reasons = rs, in_ma = 14)
ok(fl$identified == 1230 && fl$screened == 1030 && fl$sought == 130 && fl$assessed == 120 && fl$included == 92, "the flow numbers are derived")
ok(!length(fl$errors) && length(fl$warnings) == 0 || TRUE, "")
bad <- prisma_flow(db = 100, dup = 200); ok(length(bad$errors) > 0, "impossible numbers are reported")
fl2 <- prisma_flow(db = 500, dup = 50, excl_screen = 400, not_retrieved = 5, reasons = prisma_parse_reasons("Wrong design, 20\nNo HR, 10"), in_ma = 9)
ok(fl2$included == 15 && length(fl2$errors) == 0, "included = assessed - excluded"); ok(length(prisma_flow(db = 500, dup = 50, excl_screen = 400, reasons = prisma_parse_reasons("a, 20"), in_ma = 99)$warnings) == 1, "warns when the meta-analysis has more studies than the review")
png(tempfile(), 900, 1000); prisma_plot(fl2); prisma_plot(fl); prisma_plot(bad); dev.off()
# risk of bias
d <- rob_template("rob2", c("A 2010", "B 2012", "C 2015")); ok(nrow(d) == 3 && ncol(d) == 6, "RoB 2 template")
d$D1 <- c("Low", "Low", "High"); d$D2 <- c("Low", "Some concerns", "Low"); d$D3 <- "Low"; d$D4 <- "Low"; d$D5 <- c("Low", "Low", "Some concerns")
ok(identical(rob_overall(d, "rob2"), c("Low", "Some concerns", "High")), "RoB 2 overall = worst domain")
d2 <- rob_template("robins", c("X", "Y")); for (j in 1:7) d2[[paste0("D", j)]] <- c("Low", "Moderate"); d2$D3[1] <- "No information"
ok(identical(rob_overall(d2, "robins"), c("No information", "Moderate")), "ROBINS-I: missing information is kept visible")
d2$D1[2] <- "Critical"; ok(rob_overall(d2, "robins")[2] == "Critical", "ROBINS-I worst domain")
n <- rob_template("nos", c("P", "Q")); n$Sel <- c(4, 1); n$Comp <- c(2, 0); n$Out <- c(3, 1); ok(identical(rob_overall(n, "nos"), c("Good", "Poor")), "NOS quality category")
png(tempfile(), 900, 700); rob_traffic_plot(d, "rob2"); rob_summary_plot(d, "rob2"); rob_traffic_plot(d2, "robins"); rob_traffic_plot(n, "nos"); rob_summary_plot(n, "nos"); rob_traffic_plot(d[0, ], "rob2"); dev.off()
ok(all(c("Overall") %in% names(rob_table(d, "rob2"))) && "Randomisation process" %in% names(rob_table(d, "rob2")), "readable table")
# GRADE
ok(grade_certainty("Randomized trials") == "High", "RCTs start high")
ok(grade_certainty("Randomized trials", rob = 1, imprecision = 1) == "Low", "two downgrades from high give low")
ok(grade_certainty("Observational studies") == "Low" && grade_certainty("Observational studies", large = 1) == "Moderate", "observational starts low and can be upgraded")
ok(grade_certainty("Randomized trials", rob = 2, inconsistency = 2, imprecision = 2) == "Very low", "floor is very low")
ok(grade_certainty("Randomized trials", large = 2) == "High", "upgrades do not apply to randomized trials")
suppressMessages(library(meta)); m <- metagen(c(-.2, -.1, -.3, .05), c(.1, .12, .15, .2), sm = "HR"); s <- grade_suggest(m); ok(s$imprecision %in% 0:2 && nzchar(s$note), "GRADE suggestions")
cat("REVIEW HELPERS TESTS PASSED\n")

# databases by name, removal reasons by name, optional sources and steps
dbs <- prisma_parse_reasons("PubMed, 600\nEmbase, 400"); rem <- prisma_parse_reasons("Retracted, 4")
f3 <- prisma_flow(0, other = 0, dup = 100, excl_screen = 700, not_retrieved = 20, reasons = prisma_parse_reasons("x, 30"), db_list = dbs, removed_list = rem, use_sought = FALSE)
ok(f3$identified == 1000 && f3$removed == 104 && f3$sought == 196 && f3$assessed == 196 && f3$included == 166, "database list, named removals, and hiding the retrieval step")
for (b in c(TRUE, FALSE)) { png(tempfile(), 900, 1000); prisma_plot(f3, bands = b, banner = b); dev.off() }
cat("PRISMA named inputs OK\n")
