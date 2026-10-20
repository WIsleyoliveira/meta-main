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
  if (!nrow(g)) stop("a table row has no answer combinations")
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

# ==================== ROBINS-I V2: one test per flowchart path (cribsheet of 20 November 2025) ====================
# path(domain, variant, want, answers...): each answer may be a set; every combination must give `want`
path <- function(domain, variant, want, ...) {
  g <- expand.grid(list(...), stringsAsFactors = FALSE)
  if (!nrow(g)) stop("a test path has no answer combinations")
  for (i in seq_len(nrow(g))) {
    a <- setNames(unlist(g[i, ]), names(g))
    got <- robins2_domain(domain, a, variant)
    if (!identical(got, want)) stop(sprintf("ROBINS D%d%s %s: got %s, want %s", domain, variant, paste(names(a), a, sep = "=", collapse = " "), got, want))
  }
}
L <- "Low"; M <- "Moderate"; S <- "Serious"; C <- "Critical"; YPY <- c("Y", "PY"); NPN <- c("N", "PN"); NPNNI <- c("N", "PN", "NI")

# Domain 1, Variant A (page 20)
path(1, "A", L, `1.1` = YPY, `1.3` = NPNNI, `1.2` = YPY, `1.4` = NPN)
path(1, "A", S, `1.1` = YPY, `1.3` = NPNNI, `1.2` = YPY, `1.4` = YPY)
path(1, "A", M, `1.1` = YPY, `1.3` = NPNNI, `1.2` = "WN", `1.4` = NPN)
path(1, "A", S, `1.1` = YPY, `1.3` = NPNNI, `1.2` = "WN", `1.4` = YPY)
path(1, "A", S, `1.1` = YPY, `1.3` = NPNNI, `1.2` = c("SN", "NI"), `1.4` = c(YPY, NPN))
path(1, "A", M, `1.1` = "WN", `1.3` = NPNNI, `1.2` = c(YPY, "WN"), `1.4` = NPN)
path(1, "A", S, `1.1` = "WN", `1.3` = NPNNI, `1.2` = c(YPY, "WN"), `1.4` = YPY)
path(1, "A", S, `1.1` = "WN", `1.3` = NPNNI, `1.2` = c("SN", "NI"), `1.4` = c(YPY, NPN))
path(1, "A", C, `1.1` = c(YPY, "WN"), `1.3` = YPY, `1.2` = c(YPY, "WN", "SN", "NI"), `1.4` = YPY)
path(1, "A", S, `1.1` = c(YPY, "WN"), `1.3` = YPY, `1.2` = YPY, `1.4` = NPN)
path(1, "A", C, `1.1` = c(YPY, "WN"), `1.3` = YPY, `1.2` = c("SN", "WN", "NI"), `1.4` = NPN)
path(1, "A", S, `1.1` = c("SN", "NI"), `1.2` = "NA", `1.3` = "NA", `1.4` = NPN)
path(1, "A", C, `1.1` = c("SN", "NI"), `1.2` = "NA", `1.3` = "NA", `1.4` = YPY)
ok("robins2 domain 1 variant A")

# Domain 1, Variant B (page 24)
path(1, "B", L, `1.1` = YPY, `1.2` = YPY, `1.3` = YPY, `1.4` = "NA", `1.5` = NPN)
path(1, "B", S, `1.1` = YPY, `1.2` = YPY, `1.3` = YPY, `1.4` = "NA", `1.5` = YPY)
path(1, "B", M, `1.1` = YPY, `1.2` = YPY, `1.3` = "WN", `1.4` = "NA", `1.5` = NPN)
path(1, "B", S, `1.1` = YPY, `1.2` = YPY, `1.3` = "WN", `1.4` = "NA", `1.5` = YPY)
path(1, "B", S, `1.1` = YPY, `1.2` = YPY, `1.3` = c("SN", "NI"), `1.4` = "NA", `1.5` = c(YPY, NPN))
path(1, "B", M, `1.1` = YPY, `1.2` = "WN", `1.3` = c(YPY, "WN"), `1.4` = "NA", `1.5` = NPN)
path(1, "B", S, `1.1` = YPY, `1.2` = "WN", `1.3` = c(YPY, "WN"), `1.4` = "NA", `1.5` = YPY)
path(1, "B", S, `1.1` = YPY, `1.2` = "WN", `1.3` = c("SN", "NI"), `1.4` = "NA", `1.5` = c(YPY, NPN))
path(1, "B", S, `1.1` = YPY, `1.2` = c("SN", "NI"), `1.3` = "NA", `1.4` = "NA", `1.5` = NPN)
path(1, "B", C, `1.1` = YPY, `1.2` = c("SN", "NI"), `1.3` = "NA", `1.4` = "NA", `1.5` = YPY)
path(1, "B", C, `1.1` = NPNNI, `1.2` = "NA", `1.3` = "NA", `1.4` = YPY, `1.5` = c(YPY, NPN))
path(1, "B", S, `1.1` = NPNNI, `1.2` = "NA", `1.3` = "NA", `1.4` = NPNNI, `1.5` = NPN)
path(1, "B", C, `1.1` = NPNNI, `1.2` = "NA", `1.3` = "NA", `1.4` = NPNNI, `1.5` = YPY)
ok("robins2 domain 1 variant B")

# Domain 2 (page 28): the way in (2.1, 2.2, 2.3) sets the row; 2.4 and 2.5 move down from there
top <- list(list(`2.1` = YPY, `2.2` = "NA", `2.3` = "NA"), list(`2.1` = NPNNI, `2.2` = YPY, `2.3` = "NA"), list(`2.1` = NPNNI, `2.2` = NPNNI, `2.3` = "SY"))
for (w in top) {
  do.call(path, c(list(2, "A", L), w, list(`2.4` = NPN, `2.5` = NPN)))
  do.call(path, c(list(2, "A", M), w, list(`2.4` = NPN, `2.5` = c(YPY, "NI"))))
  do.call(path, c(list(2, "A", M), w, list(`2.4` = c("WY", "NI"), `2.5` = NPN)))
  do.call(path, c(list(2, "A", S), w, list(`2.4` = c("WY", "NI"), `2.5` = c(YPY, "NI"))))
  do.call(path, c(list(2, "A", S), w, list(`2.4` = "SY", `2.5` = NPN)))
  do.call(path, c(list(2, "A", C), w, list(`2.4` = "SY", `2.5` = c(YPY, "NI"))))
}
mid <- list(`2.1` = NPNNI, `2.2` = NPNNI, `2.3` = c("WY", "NI"))
do.call(path, c(list(2, "A", M), mid, list(`2.4` = NPN, `2.5` = NPN)))
do.call(path, c(list(2, "A", S), mid, list(`2.4` = NPN, `2.5` = c(YPY, "NI"))))
do.call(path, c(list(2, "A", S), mid, list(`2.4` = c("WY", "NI"), `2.5` = NPN)))
do.call(path, c(list(2, "A", C), mid, list(`2.4` = c("WY", "NI"), `2.5` = c(YPY, "NI"))))
do.call(path, c(list(2, "A", C), mid, list(`2.4` = "SY", `2.5` = c(YPY, NPN, "NI"))))
bot <- list(`2.1` = NPNNI, `2.2` = NPNNI, `2.3` = NPN)
do.call(path, c(list(2, "A", S), bot, list(`2.4` = NPN, `2.5` = NPN)))
do.call(path, c(list(2, "A", C), bot, list(`2.4` = NPN, `2.5` = c(YPY, "NI"))))
do.call(path, c(list(2, "A", C), bot, list(`2.4` = c("SY", "WY", "NI"), `2.5` = c(YPY, NPN, "NI"))))
ok("robins2 domain 2")

# Domain 3 (page 32): part A and part B, combined; at least one Serious goes through 3.6-3.8
partA <- list(L = list(`3.1` = YPY, `3.2` = NPNNI), M1 = list(`3.1` = YPY, `3.2` = YPY), M2 = list(`3.1` = c("WN", "NI"), `3.2` = "NA"),
              S = list(`3.1` = "SN", `3.2` = "NA"))
partB <- list(L1 = list(`3.3` = NPN, `3.4` = "NA", `3.5` = "NA"), M1 = list(`3.3` = "NI", `3.4` = "NA", `3.5` = "NA"),
              L2 = list(`3.3` = YPY, `3.4` = NPN, `3.5` = "NA"), M2 = list(`3.3` = YPY, `3.4` = "NI", `3.5` = "NA"),
              M3 = list(`3.3` = YPY, `3.4` = YPY, `3.5` = NPNNI), S = list(`3.3` = YPY, `3.4` = YPY, `3.5` = YPY))
lev <- function(k) substr(k, 1, 1)
for (ka in names(partA)) for (kb in names(partB)) {
  la <- lev(ka); lb <- lev(kb)
  if (la != "S" && lb != "S") {
    want <- if (la == "L" && lb == "L") L else M
    do.call(path, c(list(3, "A", want), partA[[ka]], partB[[kb]], list(`3.6` = "NA", `3.7` = "NA", `3.8` = "NA")))
  } else {
    do.call(path, c(list(3, "A", M), partA[[ka]], partB[[kb]], list(`3.6` = YPY, `3.7` = "NA", `3.8` = "NA")))
    do.call(path, c(list(3, "A", M), partA[[ka]], partB[[kb]], list(`3.6` = NPNNI, `3.7` = YPY, `3.8` = "NA")))
    do.call(path, c(list(3, "A", S), partA[[ka]], partB[[kb]], list(`3.6` = NPNNI, `3.7` = NPNNI, `3.8` = NPNNI)))
    do.call(path, c(list(3, "A", C), partA[[ka]], partB[[kb]], list(`3.6` = NPNNI, `3.7` = NPNNI, `3.8` = YPY)))
  }
}
ok("robins2 domain 3")

# Domain 4 (page 38)
# the questions of Domain 4 not on a path are "NA"; NULL removes one (it is given by the caller)
na4 <- function(...) {
  x <- list(`4.4` = "NA", `4.5` = "NA", `4.6` = "NA", `4.7` = "NA", `4.8` = "NA", `4.9` = "NA", `4.10` = "NA", `4.11` = "NA")
  y <- list(...); drop <- names(y)[vapply(y, is.null, logical(1))]; y <- y[!names(y) %in% drop]
  x[names(y)] <- y; x[!names(x) %in% drop]
}
miss <- list(list(`4.1` = NPNNI, `4.2` = c(YPY, NPNNI), `4.3` = YPY), list(`4.1` = YPY, `4.2` = YPY, `4.3` = NPNNI))
do.call(path, c(list(4, "A", L), list(`4.1` = YPY, `4.2` = YPY, `4.3` = YPY), na4()))
for (m in miss) {
  cc <- c(m, list(`4.4` = c(YPY, "NI")))
  do.call(path, c(list(4, "A", L), cc, na4(`4.4` = NULL, `4.5` = NPN)))
  do.call(path, c(list(4, "A", L), cc, na4(`4.4` = NULL, `4.5` = c(YPY, "NI"), `4.6` = YPY, `4.11` = YPY)))
  do.call(path, c(list(4, "A", M), cc, na4(`4.4` = NULL, `4.5` = c(YPY, "NI"), `4.6` = YPY, `4.11` = NPN)))
  do.call(path, c(list(4, "A", M), cc, na4(`4.4` = NULL, `4.5` = c(YPY, "NI"), `4.6` = c("WN", "NI"), `4.11` = YPY)))
  do.call(path, c(list(4, "A", S), cc, na4(`4.4` = NULL, `4.5` = c(YPY, "NI"), `4.6` = c("WN", "NI"), `4.11` = NPN)))
  do.call(path, c(list(4, "A", S), cc, na4(`4.4` = NULL, `4.5` = c(YPY, "NI"), `4.6` = "SN", `4.11` = YPY)))
  do.call(path, c(list(4, "A", C), cc, na4(`4.4` = NULL, `4.5` = c(YPY, "NI"), `4.6` = "SN", `4.11` = NPN)))
  imp <- c(m, list(`4.4` = NPN, `4.7` = YPY))
  do.call(path, c(list(4, "A", S), imp, na4(`4.4` = NULL, `4.7` = NULL, `4.8` = NPNNI)))
  do.call(path, c(list(4, "A", L), imp, na4(`4.4` = NULL, `4.7` = NULL, `4.8` = YPY, `4.9` = YPY)))
  do.call(path, c(list(4, "A", M), imp, na4(`4.4` = NULL, `4.7` = NULL, `4.8` = YPY, `4.9` = c("WN", "NI"), `4.11` = YPY)))
  do.call(path, c(list(4, "A", S), imp, na4(`4.4` = NULL, `4.7` = NULL, `4.8` = YPY, `4.9` = c("WN", "NI"), `4.11` = NPN)))
  do.call(path, c(list(4, "A", S), imp, na4(`4.4` = NULL, `4.7` = NULL, `4.8` = YPY, `4.9` = "SN", `4.11` = YPY)))
  do.call(path, c(list(4, "A", C), imp, na4(`4.4` = NULL, `4.7` = NULL, `4.8` = YPY, `4.9` = "SN", `4.11` = NPN)))
  alt <- c(m, list(`4.4` = NPN, `4.7` = c("PN", "NI")))
  do.call(path, c(list(4, "A", L), alt, na4(`4.4` = NULL, `4.7` = NULL, `4.10` = YPY)))
  do.call(path, c(list(4, "A", M), alt, na4(`4.4` = NULL, `4.7` = NULL, `4.10` = c("WN", "NI"), `4.11` = YPY)))
  do.call(path, c(list(4, "A", S), alt, na4(`4.4` = NULL, `4.7` = NULL, `4.10` = c("WN", "NI"), `4.11` = NPN)))
  do.call(path, c(list(4, "A", S), alt, na4(`4.4` = NULL, `4.7` = NULL, `4.10` = "SN", `4.11` = YPY)))
  do.call(path, c(list(4, "A", C), alt, na4(`4.4` = NULL, `4.7` = NULL, `4.10` = "SN", `4.11` = NPN)))
}
ok("robins2 domain 4")

# Domain 5 (page 41)
path(5, "A", S, `5.1` = YPY, `5.2` = c(YPY, NPN, "NI"), `5.3` = c("NA", "SY", "WY", "PN", "N", "NI"))
path(5, "A", L, `5.1` = NPN, `5.2` = NPN, `5.3` = "NA")
path(5, "A", L, `5.1` = NPN, `5.2` = c(YPY, "NI"), `5.3` = NPN)
path(5, "A", M, `5.1` = NPN, `5.2` = c(YPY, "NI"), `5.3` = c("WY", "NI"))
path(5, "A", S, `5.1` = NPN, `5.2` = c(YPY, "NI"), `5.3` = "SY")
path(5, "A", M, `5.1` = "NI", `5.2` = NPN, `5.3` = "NA")
path(5, "A", M, `5.1` = "NI", `5.2` = c(YPY, "NI"), `5.3` = c("WY", "N", "PN", "NI"))
path(5, "A", S, `5.1` = "NI", `5.2` = c(YPY, "NI"), `5.3` = "SY")
ok("robins2 domain 5")

# Domain 6 (page 47)
ALL <- c(YPY, NPN, "NI")
path(6, "A", L, `6.1` = YPY, `6.2` = ALL, `6.3` = ALL, `6.4` = ALL)
path(6, "A", L, `6.1` = NPNNI, `6.2` = NPN, `6.3` = NPN, `6.4` = NPN)
path(6, "A", M, `6.1` = NPNNI, `6.2` = "NI", `6.3` = NPN, `6.4` = c(NPN, "NI"))
path(6, "A", M, `6.1` = NPNNI, `6.2` = NPN, `6.3` = NPN, `6.4` = "NI")
path(6, "A", S, `6.1` = NPNNI, `6.2` = "NI", `6.3` = "NI", `6.4` = "NI")
path(6, "A", S, `6.1` = NPNNI, `6.2` = YPY, `6.3` = c(NPN, "NI"), `6.4` = c(NPN, "NI"))
path(6, "A", S, `6.1` = NPNNI, `6.2` = c(NPN, "NI"), `6.3` = c(NPN, "NI"), `6.4` = YPY)
path(6, "A", C, `6.1` = NPNNI, `6.2` = YPY, `6.3` = YPY, `6.4` = ALL)
path(6, "A", C, `6.1` = NPNNI, `6.2` = ALL, `6.3` = YPY, `6.4` = YPY)
ok("robins2 domain 6")

# Overall (page 49) and the preliminary questions (page 9)
stopifnot(robins2_overall(rep(L, 6)) == L, robins2_overall(c(L, M, L, L, L, L)) == M, robins2_overall(c(M, S, L, M, L, L)) == S,
          robins2_overall(c(L, L, C, L, L, L)) == C, robins2_overall(rep(L, 6), critical_prelim = TRUE) == C,
          robins2_prelim_critical(c(B1 = "N", B2 = "PY", B3 = "N")), robins2_prelim_critical(c(B1 = "Y", B2 = "NA", B3 = "Y")),
          !robins2_prelim_critical(c(B1 = "Y", B2 = "NA", B3 = "PN")),
          robins2_variant(c(C4 = "N")) == "A", robins2_variant(c(C4 = "Y")) == "B", robins2_variant(c()) == "A")
ok("robins2 overall and preliminary")

stopifnot(length(ROB_DOMAIN_NAMES$robins2) == 6,
          all(vapply(ROBINS2_Q$A, function(q) q$domain %in% 0:6, logical(1))),
          identical(ROBINS2_Q$B[[match("1.5", vapply(ROBINS2_Q$B, `[[`, "", "id"))]]$answers, c("Y", "PY", "PN", "N")))
ok("robins2 bank")

cat("ROBINS2 ALGORITHM TESTS PASSED\n")
