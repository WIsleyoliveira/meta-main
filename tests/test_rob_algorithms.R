# Risk-of-bias question banks and algorithms against the published tools:
# RoB 2 (guidance and cribsheet of 22 August 2019) and ROBINS-I V2 (cribsheet of 20 November 2025).
source("R/review_helpers.R")
source("R/rob_questions.R")
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
