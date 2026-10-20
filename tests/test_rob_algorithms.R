# Risk-of-bias question banks and algorithms against the published tools:
# RoB 2 (guidance and cribsheet of 22 August 2019) and ROBINS-I V2 (cribsheet of 20 November 2025).
source("R/review_helpers.R")
source("R/rob_questions.R")
source("R/rob_algorithms.R")
ok <- function(name) cat("ok:", name, "\n")
ans <- function(...) { a <- c(...); setNames(as.character(a), names(a)) }

# ==================== RoB 2: bank and applicability ====================
ids_of <- function(bank) vapply(bank, `[[`, "", "id")
asg <- ROB2_Q$assignment; adh <- ROB2_Q$adherence
stopifnot(identical(ids_of(asg), c("1.1", "1.2", "1.3", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6", "2.7",
                                    "3.1", "3.2", "3.3", "3.4", "4.1", "4.2", "4.3", "4.4", "4.5", "5.1", "5.2", "5.3")),
          identical(ids_of(adh), c("1.1", "1.2", "1.3", "2.1", "2.2", "2.3", "2.4", "2.5", "2.6",
                                    "3.1", "3.2", "3.3", "3.4", "4.1", "4.2", "4.3", "4.4", "4.5", "5.1", "5.2", "5.3")),
          all(vapply(asg, function(q) q$domain, 0L) == as.integer(substr(ids_of(asg), 1, 1))))
ok("rob2 bank sizes")

txt <- function(bank, id) bank[[match(id, ids_of(bank))]]$text
stopifnot(txt(asg, "1.1") == "Was the allocation sequence random?",
          startsWith(txt(asg, "3.1"), "Were data for this outcome available for all, or nearly all, participants randomized?"),
          txt(asg, "2.3") == "Were there deviations from the intended intervention that arose because of the trial context?",
          txt(adh, "2.6") == "Was an appropriate analysis used to estimate the effect of adhering to the intervention?",
          grepl("multiple eligible analyses of the data\\?$", txt(asg, "5.3")),
          asg[[match("2.3", ids_of(asg))]]$cond == "If Y/PY/NI to 2.1 or 2.2",
          length(ROB_DOMAIN_NAMES$rob2) == 5, identical(ROB_DOMAIN_NAMES$rob2, ROB_TOOLS$rob2$domains))
ok("rob2 texts verbatim")

stopifnot(all(vapply(asg, function(q) all(c("Y", "PY", "PN", "N", "NI") %in% q$answers), logical(1))),
          !("NA" %in% asg[[1]]$answers), "NA" %in% asg[[match("2.3", ids_of(asg))]]$answers)
ok("rob2 answer options")

ap <- rob_applicable(asg, ans(`2.1` = "N", `2.2` = "N"))
stopifnot(!any(c("2.3", "2.4", "2.5") %in% ap), all(c("2.1", "2.2", "2.6", "1.1") %in% ap))
ap <- rob_applicable(asg, ans(`2.1` = "Y", `2.2` = "N", `2.3` = "Y", `2.4` = "N"))
stopifnot("2.3" %in% ap, "2.4" %in% ap, !("2.5" %in% ap))
stopifnot(!("3.2" %in% rob_applicable(asg, ans(`3.1` = "Y"))), "3.2" %in% rob_applicable(asg, ans(`3.1` = "NI")),
          "3.3" %in% rob_applicable(asg, ans(`3.1` = "N", `3.2` = "PN")), !("3.3" %in% rob_applicable(asg, ans(`3.1` = "N", `3.2` = "NI"))),
          !("2.7" %in% rob_applicable(asg, ans(`2.6` = "Y"))), "2.7" %in% rob_applicable(asg, ans(`2.6` = "PN")),
          "4.3" %in% rob_applicable(asg, ans(`4.1` = "N", `4.2` = "NI")), !("4.3" %in% rob_applicable(asg, ans(`4.1` = "Y", `4.2` = "N"))))
# adherence: 2.3-2.5 apply only for the deviations the reviewer chose to address
dev <- c("nonprotocol", "implementation", "adherence")
stopifnot("2.3" %in% rob_applicable(adh, ans(`2.1` = "Y"), deviations = dev),
          !("2.3" %in% rob_applicable(adh, ans(`2.1` = "Y"), deviations = "adherence")),
          !("2.3" %in% rob_applicable(adh, ans(`2.1` = "N", `2.2` = "N"), deviations = dev)),
          "2.5" %in% rob_applicable(adh, ans(), deviations = "adherence"), !("2.4" %in% rob_applicable(adh, ans(), deviations = "adherence")),
          "2.6" %in% rob_applicable(adh, ans(`2.5` = "Y"), deviations = "adherence"),
          !("2.6" %in% rob_applicable(adh, ans(`2.3` = "Y", `2.4` = "N", `2.5` = "N"), deviations = dev)))
ok("rob2 applicability")

f <- rob_force_na(asg, ans(`2.1` = "N", `2.2` = "N", `2.3` = "Y", `2.4` = "Y", `1.1` = "Y"))
stopifnot(f[["2.3"]] == "NA", f[["2.4"]] == "NA", f[["1.1"]] == "Y", f[["1.2"]] == "NI", length(f) == length(asg))
ok("rob2 force NA and NI")

cat("ROB2 BANK TESTS PASSED\n")

# ==================== RoB 2: algorithms, row by row from the guidance tables ====================
YP <- c("Y", "PY"); NP <- c("N", "PN"); NI <- "NI"; NA_ <- "NA"; ANY <- c("Y", "PY", "PN", "N", "NI")
# every combination of the answer sets of one table row must give `want`
row_is <- function(domain, variant, want, ...) {
  g <- expand.grid(list(...), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(g))) {
    a <- setNames(unlist(g[i, ]), names(g))
    got <- rob2_domain(domain, a, variant)
    if (!identical(got, want)) stop(sprintf("D%d %s %s: got %s, want %s", domain, variant, paste(names(a), a, sep = "=", collapse = " "), got, want))
  }
}
# Table 4 (randomization)
row_is(1, "assignment", "Low", `1.1` = c(YP, NI), `1.2` = YP, `1.3` = c(NI, NP))
row_is(1, "assignment", "Some concerns", `1.1` = YP, `1.2` = YP, `1.3` = YP)
row_is(1, "assignment", "Some concerns", `1.1` = c(NP, NI), `1.2` = YP, `1.3` = YP)
row_is(1, "assignment", "Some concerns", `1.1` = ANY, `1.2` = NI, `1.3` = c(NP, NI))
row_is(1, "assignment", "High", `1.1` = ANY, `1.2` = NI, `1.3` = YP)
row_is(1, "assignment", "High", `1.1` = ANY, `1.2` = NP, `1.3` = ANY)
# not a row of Table 4; Figure 1 leads a non-random sequence with concealment and no imbalance to Some concerns
row_is(1, "assignment", "Some concerns", `1.1` = NP, `1.2` = YP, `1.3` = c(NP, NI))
ok("rob2 table 4")

# Table 6 (assignment): part 1 x part 2, combined as the worst of the two
p1 <- list(list("Low", list(`2.1` = NP, `2.2` = NP, `2.3` = NA_, `2.4` = NA_, `2.5` = NA_)),
           list("Low", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = NP, `2.4` = NA_, `2.5` = NA_)),
           list("Some concerns", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = NI, `2.4` = NA_, `2.5` = NA_)),
           list("Some concerns", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = YP, `2.4` = NP, `2.5` = NA_)),
           list("Some concerns", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = YP, `2.4` = c(YP, NI), `2.5` = YP)),
           list("High", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = YP, `2.4` = c(YP, NI), `2.5` = c(NP, NI))),
           list("Some concerns", list(`2.1` = NP, `2.2` = c(YP, NI), `2.3` = NI, `2.4` = NA_, `2.5` = NA_)))
p2 <- list(list("Low", list(`2.6` = YP, `2.7` = NA_)), list("Some concerns", list(`2.6` = c(NP, NI), `2.7` = NP)),
           list("High", list(`2.6` = c(NP, NI), `2.7` = c(YP, NI))))
worst <- function(x, y) c("Low", "Some concerns", "High")[max(match(c(x, y), c("Low", "Some concerns", "High")))]
for (r1 in p1) for (r2 in p2) do.call(row_is, c(list(2, "assignment", worst(r1[[1]], r2[[1]])), r1[[2]], r2[[2]]))
ok("rob2 table 6")

# Table 8 (adherence); NA where the reviewer did not ask for the question or it does not apply
NPA <- c(NP, NA_)
t8 <- list(
  list("Low", list(`2.1` = NP, `2.2` = NP, `2.3` = NA_, `2.4` = NPA, `2.5` = NPA, `2.6` = NA_)),
  list("Low", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(YP, NA_), `2.4` = NPA, `2.5` = NPA, `2.6` = NA_)),
  list("Some concerns", list(`2.1` = NP, `2.2` = NP, `2.3` = NA_, `2.4` = c(YP, NI), `2.5` = NPA, `2.6` = YP)),
  list("Some concerns", list(`2.1` = NP, `2.2` = NP, `2.3` = NA_, `2.4` = NPA, `2.5` = c(YP, NI), `2.6` = YP)),
  list("Some concerns", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(YP, NA_), `2.4` = c(YP, NI), `2.5` = NPA, `2.6` = YP)),
  list("Some concerns", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(YP, NA_), `2.4` = NPA, `2.5` = c(YP, NI), `2.6` = YP)),
  list("Some concerns", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(NP, NI), `2.4` = c(ANY, NA_), `2.5` = c(ANY, NA_), `2.6` = YP)),
  list("High", list(`2.1` = NP, `2.2` = NP, `2.3` = NA_, `2.4` = c(YP, NI), `2.5` = NPA, `2.6` = c(NP, NI))),
  list("High", list(`2.1` = NP, `2.2` = NP, `2.3` = NA_, `2.4` = NPA, `2.5` = c(YP, NI), `2.6` = c(NP, NI))),
  list("High", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(YP, NA_), `2.4` = c(YP, NI), `2.5` = NPA, `2.6` = c(NP, NI))),
  list("High", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(YP, NA_), `2.4` = NPA, `2.5` = c(YP, NI), `2.6` = c(NP, NI))),
  list("High", list(`2.1` = c(YP, NI), `2.2` = ANY, `2.3` = c(NP, NI), `2.4` = c(ANY, NA_), `2.5` = c(ANY, NA_), `2.6` = c(NP, NI))))
for (r in t8) do.call(row_is, c(list(2, "adherence", r[[1]]), r[[2]]))
ok("rob2 table 8")

# Table 10 (missing outcome data)
row_is(3, "assignment", "Low", `3.1` = YP, `3.2` = NA_, `3.3` = NA_, `3.4` = NA_)
row_is(3, "assignment", "Low", `3.1` = c(NP, NI), `3.2` = YP, `3.3` = NA_, `3.4` = NA_)
row_is(3, "assignment", "Low", `3.1` = c(NP, NI), `3.2` = NP, `3.3` = NP, `3.4` = NA_)
row_is(3, "assignment", "Some concerns", `3.1` = c(NP, NI), `3.2` = NP, `3.3` = c(YP, NI), `3.4` = NP)
row_is(3, "assignment", "High", `3.1` = c(NP, NI), `3.2` = NP, `3.3` = c(YP, NI), `3.4` = c(YP, NI))
ok("rob2 table 10")

# Table 12 (measurement of the outcome)
NPNI <- c(NP, NI)
row_is(4, "assignment", "Low", `4.1` = NPNI, `4.2` = NP, `4.3` = NP, `4.4` = NA_, `4.5` = NA_)
row_is(4, "assignment", "Low", `4.1` = NPNI, `4.2` = NP, `4.3` = c(YP, NI), `4.4` = NP, `4.5` = NA_)
row_is(4, "assignment", "Some concerns", `4.1` = NPNI, `4.2` = NP, `4.3` = c(YP, NI), `4.4` = c(YP, NI), `4.5` = NP)
row_is(4, "assignment", "High", `4.1` = NPNI, `4.2` = NP, `4.3` = c(YP, NI), `4.4` = c(YP, NI), `4.5` = c(YP, NI))
row_is(4, "assignment", "Some concerns", `4.1` = NPNI, `4.2` = NI, `4.3` = NP, `4.4` = NA_, `4.5` = NA_)
row_is(4, "assignment", "Some concerns", `4.1` = NPNI, `4.2` = NI, `4.3` = c(YP, NI), `4.4` = NP, `4.5` = NA_)
row_is(4, "assignment", "Some concerns", `4.1` = NPNI, `4.2` = NI, `4.3` = c(YP, NI), `4.4` = c(YP, NI), `4.5` = NP)
row_is(4, "assignment", "High", `4.1` = NPNI, `4.2` = NI, `4.3` = c(YP, NI), `4.4` = c(YP, NI), `4.5` = c(YP, NI))
row_is(4, "assignment", "High", `4.1` = YP, `4.2` = ANY, `4.3` = c(ANY, NA_), `4.4` = c(ANY, NA_), `4.5` = c(ANY, NA_))
row_is(4, "assignment", "High", `4.1` = ANY, `4.2` = YP, `4.3` = c(ANY, NA_), `4.4` = c(ANY, NA_), `4.5` = c(ANY, NA_))
ok("rob2 table 12")

# Table 14 (selection of the reported result)
row_is(5, "assignment", "Low", `5.1` = YP, `5.2` = NP, `5.3` = NP)
row_is(5, "assignment", "Some concerns", `5.1` = NPNI, `5.2` = NP, `5.3` = NP)
row_is(5, "assignment", "Some concerns", `5.1` = ANY, `5.2` = NP, `5.3` = NI)
row_is(5, "assignment", "Some concerns", `5.1` = ANY, `5.2` = NI, `5.3` = NP)
row_is(5, "assignment", "Some concerns", `5.1` = ANY, `5.2` = NI, `5.3` = NI)
row_is(5, "assignment", "High", `5.1` = ANY, `5.2` = YP, `5.3` = ANY)
row_is(5, "assignment", "High", `5.1` = ANY, `5.2` = ANY, `5.3` = YP)
ok("rob2 table 14")

stopifnot(rob2_overall(rep("Low", 5)) == "Low", rob2_overall(c("Low", "Some concerns", "Low", "Low", "Low")) == "Some concerns",
          rob2_overall(c("Some concerns", "High", "Low", "Low", "Low")) == "High",
          identical(rob2_domains(rob_force_na(ROB2_Q$assignment, c(`1.1` = "N", `1.2` = "N")), "assignment")[1], c(D1 = "High")))
ok("rob2 overall")

cat("ROB2 ALGORITHM TESTS PASSED\n")
