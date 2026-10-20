# Signalling questions of the risk-of-bias tools, as data. Texts are verbatim from the published cribsheets
# (CC BY-NC-ND 4.0: reproduced without changes, with the citation below). Plain R, no Shiny.
#
# A question: id, domain, text, cond (the tool's own "If ..." wording, shown before the text), answers (allowed),
# when (function(a, dev) -> TRUE when the question applies, given the answers so far `a` and, for RoB 2 adherence,
# the deviations the reviewer chose to address `dev`).

ROB_CITATION <- c(
  rob2 = "Sterne JAC, Savović J, Page MJ, et al. RoB 2: a revised tool for assessing risk of bias in randomised trials. BMJ 2019;366:l4898. Questions reproduced from the RoB 2 cribsheet (22 August 2019), CC BY-NC-ND 4.0.",
  robins = "ROBINS-I V2 (draft of 20 November 2025), riskofbias.info. Questions reproduced from the ROBINS-I V2 cribsheet, CC BY-NC-ND 4.0.")

ROB_ANS <- c("Y", "PY", "PN", "N", "NI")
ROB_ANS_NA <- c("NA", ROB_ANS)
ROB_ANS_LABEL <- c(Y = "Yes", PY = "Probably yes", PN = "Probably no", N = "No", NI = "No information", "NA" = "Not applicable",
                   WY = "Weak yes", SY = "Strong yes", WN = "Weak no", SN = "Strong no")

rob_yes <- function(x) !is.na(x) & x %in% c("Y", "PY", "WY", "SY")
rob_no <- function(x) !is.na(x) & x %in% c("N", "PN", "WN", "SN")
rob_ni <- function(x) is.na(x) | x == "NI"
rob_a <- function(a, id) if (id %in% names(a)) a[[id]] else NA_character_

rq <- function(id, text, cond = "", when = NULL, answers = if (is.null(when)) ROB_ANS else ROB_ANS_NA)
  list(id = id, domain = as.integer(sub("\\..*", "", id)), text = text, cond = cond, answers = answers, when = when)

ROB2_COMMON_1 <- list(
  rq("1.1", "Was the allocation sequence random?"),
  rq("1.2", "Was the allocation sequence concealed until participants were enrolled and assigned to interventions?"),
  rq("1.3", "Did baseline differences between intervention groups suggest a problem with the randomization process?"))

ROB2_COMMON_345 <- list(
  rq("3.1", "Were data for this outcome available for all, or nearly all, participants randomized?"),
  rq("3.2", "Is there evidence that the result was not biased by missing outcome data?", "If N/PN/NI to 3.1",
     function(a, dev) { x <- rob_a(a, "3.1"); rob_no(x) || rob_ni(x) }),
  rq("3.3", "Could missingness in the outcome depend on its true value?", "If N/PN to 3.2",
     function(a, dev) rob_no(rob_a(a, "3.2"))),
  rq("3.4", "Is it likely that missingness in the outcome depended on its true value?", "If Y/PY/NI to 3.3",
     function(a, dev) { x <- rob_a(a, "3.3"); x != "NA" && (rob_yes(x) || rob_ni(x)) }),
  rq("4.1", "Was the method of measuring the outcome inappropriate?"),
  rq("4.2", "Could measurement or ascertainment of the outcome have differed between intervention groups?"),
  rq("4.3", "Were outcome assessors aware of the intervention received by study participants?", "If N/PN/NI to 4.1 and 4.2",
     function(a, dev) { x <- rob_a(a, "4.1"); y <- rob_a(a, "4.2"); (rob_no(x) || rob_ni(x)) && (rob_no(y) || rob_ni(y)) }),
  rq("4.4", "Could assessment of the outcome have been influenced by knowledge of intervention received?", "If Y/PY/NI to 4.3",
     function(a, dev) { x <- rob_a(a, "4.3"); x != "NA" && (rob_yes(x) || rob_ni(x)) }),
  rq("4.5", "Is it likely that assessment of the outcome was influenced by knowledge of intervention received?", "If Y/PY/NI to 4.4",
     function(a, dev) { x <- rob_a(a, "4.4"); x != "NA" && (rob_yes(x) || rob_ni(x)) }),
  rq("5.1", "Were the data that produced this result analysed in accordance with a pre-specified analysis plan that was finalized before unblinded outcome data were available for analysis?"),
  rq("5.2", "Is the numerical result being assessed likely to have been selected, on the basis of the results, from multiple eligible outcome measurements (e.g. scales, definitions, time points) within the outcome domain?"),
  rq("5.3", "Is the numerical result being assessed likely to have been selected, on the basis of the results, from multiple eligible analyses of the data?"))

aware <- function(a) { x <- rob_a(a, "2.1"); y <- rob_a(a, "2.2"); rob_yes(x) || rob_ni(x) || rob_yes(y) || rob_ni(y) }

ROB2_Q <- list(
  assignment = c(ROB2_COMMON_1, list(
    rq("2.1", "Were participants aware of their assigned intervention during the trial?"),
    rq("2.2", "Were carers and people delivering the interventions aware of participants' assigned intervention during the trial?"),
    rq("2.3", "Were there deviations from the intended intervention that arose because of the trial context?", "If Y/PY/NI to 2.1 or 2.2",
       function(a, dev) aware(a)),
    rq("2.4", "Were these deviations likely to have affected the outcome?", "If Y/PY to 2.3",
       function(a, dev) rob_yes(rob_a(a, "2.3"))),
    rq("2.5", "Were these deviations from intended intervention balanced between groups?", "If Y/PY/NI to 2.4",
       function(a, dev) { x <- rob_a(a, "2.4"); x != "NA" && (rob_yes(x) || rob_ni(x)) }),
    rq("2.6", "Was an appropriate analysis used to estimate the effect of assignment to intervention?"),
    rq("2.7", "Was there potential for a substantial impact (on the result) of the failure to analyse participants in the group to which they were randomized?",
       "If N/PN/NI to 2.6", function(a, dev) { x <- rob_a(a, "2.6"); rob_no(x) || rob_ni(x) })), ROB2_COMMON_345),
  adherence = c(ROB2_COMMON_1, list(
    rq("2.1", "Were participants aware of their assigned intervention during the trial?"),
    rq("2.2", "Were carers and people delivering the interventions aware of participants' assigned intervention during the trial?"),
    rq("2.3", "Were important non-protocol interventions balanced across intervention groups?", "[If applicable:] If Y/PY/NI to 2.1 or 2.2",
       function(a, dev) "nonprotocol" %in% dev && aware(a)),
    rq("2.4", "Were there failures in implementing the intervention that could have affected the outcome?", "[If applicable:]",
       function(a, dev) "implementation" %in% dev),
    rq("2.5", "Was there non-adherence to the assigned intervention regimen that could have affected participants’ outcomes?", "[If applicable:]",
       function(a, dev) "adherence" %in% dev),
    rq("2.6", "Was an appropriate analysis used to estimate the effect of adhering to the intervention?", "If N/PN/NI to 2.3, or Y/PY/NI to 2.4 or 2.5",
       function(a, dev) {
         x3 <- rob_a(a, "2.3"); x4 <- rob_a(a, "2.4"); x5 <- rob_a(a, "2.5")
         (x3 != "NA" && (rob_no(x3) || rob_ni(x3))) || (x4 != "NA" && (rob_yes(x4) || rob_ni(x4))) || (x5 != "NA" && (rob_yes(x5) || rob_ni(x5)))
       })), ROB2_COMMON_345))

ROB_DOMAIN_NAMES <- list(rob2 = c("Randomisation process", "Deviations from intended interventions", "Missing outcome data",
                                  "Measurement of the outcome", "Selection of the reported result"))

# Effective answers in question order: a question that does not apply is "NA"; one that applies but has no answer is "NI".
rob_force_na <- function(bank, answers, deviations = character(0)) {
  eff <- setNames(character(0), character(0))
  for (q in bank) {
    applies <- is.null(q$when) || isTRUE(q$when(eff, deviations))
    given <- rob_a(answers, q$id)
    eff[[q$id]] <- if (!applies) "NA" else if (is.na(given) || given == "NA" || !(given %in% q$answers)) "NI" else given
  }
  eff
}

rob_applicable <- function(bank, answers, deviations = character(0)) {
  eff <- rob_force_na(bank, answers, deviations)
  names(eff)[eff != "NA"]
}
