# Signalling questions of the risk-of-bias tools, as data. Texts are verbatim from the published cribsheets
# (CC BY-NC-ND 4.0: reproduced without changes, with the citation below). Plain R, no Shiny.
#
# A question: id, domain, text, cond (the tool's own "If ..." wording, shown before the text), answers (allowed),
# when (function(a, dev) -> TRUE when the question applies, given the answers so far `a` and, for RoB 2 adherence,
# the deviations the reviewer chose to address `dev`).

ROB_CITATION <- c(
  rob2 = "Sterne JAC, Savovi\u0107 J, Page MJ, et al. RoB 2: a revised tool for assessing risk of bias in randomised trials. BMJ 2019;366:l4898. Questions reproduced from the RoB 2 cribsheet (22 August 2019), CC BY-NC-ND 4.0.",
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
  list(id = id, domain = { d <- suppressWarnings(as.integer(sub("\\..*", "", id))); if (is.na(d)) 0L else d }, text = text, cond = cond, answers = answers, when = when)

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
    rq("2.5", "Was there non-adherence to the assigned intervention regimen that could have affected participants\u2019 outcomes?", "[If applicable:]",
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

# ---------------------------- ROBINS-I V2 (cribsheet of 20 November 2025) ----------------------------
# Variant A of Domain 1 when the analysis estimates the intention-to-treat effect (No to C4), Variant B for the per-protocol effect.

# TRUE when the answer to `id` is one of `codes`; a question not yet answered counts as NI, a skipped one ("NA") as nothing
rob_is <- function(a, id, codes) { x <- rob_a(a, id); if (is.na(x)) x <- "NI"; x %in% codes }
YPY <- c("Y", "PY"); NPN <- c("N", "PN"); YPYNI <- c("Y", "PY", "NI"); NPNNI <- c("N", "PN", "NI")
R_YN <- c("Y", "PY", "PN", "N"); R_W <- c("Y", "PY", "WN", "SN", "NI"); R_S <- c("SY", "WY", "PN", "N", "NI")

ROBINS2_PRE <- list(
  rq("B1", "Did the authors make any attempt to control for confounding in the result being assessed?", answers = R_YN),
  rq("B2", "Is there sufficient potential for confounding that this result should not be considered further?", "If N/PN to B1",
     function(a, dev) rob_is(a, "B1", NPN), answers = c("NA", R_YN)),
  rq("B3", "Was the method of measuring the outcome inappropriate?", answers = R_YN),
  rq("C4", "Did the analysis account for switches during follow-up between the intervention strategies being compared, or for other protocol deviations during follow-up?",
     answers = c("Y", "N")))

ROBINS2_D1 <- list(
  A = list(
    rq("1.1", "Did the authors control for all the important confounding factors for which this was necessary?", answers = R_W),
    rq("1.2", "Were confounding factors that were controlled for (and for which control was necessary) measured validly and reliably by the variables available in this study?",
       "If Y/PY/WN to 1.1", function(a, dev) rob_is(a, "1.1", c(YPY, "WN")), answers = c("NA", R_W)),
    rq("1.3", "Did the authors control for any post-intervention variables that could have been affected by the intervention?",
       "If Y/PY/WN to 1.1", function(a, dev) rob_is(a, "1.1", c(YPY, "WN"))),
    rq("1.4", "Did the use of negative controls, quantitative bias analysis, or other considerations, suggest serious uncontrolled confounding?", answers = R_YN)),
  B = list(
    rq("1.1", "Did the authors use an analysis method that was appropriate to control for time-varying as well as baseline confounding?"),
    rq("1.2", "Did the authors control for all the important baseline and time-varying confounding factors for which this was necessary?",
       "If Y/PY to 1.1", function(a, dev) rob_is(a, "1.1", YPY), answers = c("NA", R_W)),
    rq("1.3", "Were confounding factors that were controlled for (and for which control was necessary) measured validly and reliably by the variables available in this study?",
       "If Y/PY/WN to 1.2", function(a, dev) rob_is(a, "1.2", c(YPY, "WN")), answers = c("NA", R_W)),
    rq("1.4", "Did the authors control for time-varying factors or other variables measured after the start of intervention?",
       "If N/PN/NI to 1.1", function(a, dev) rob_is(a, "1.1", NPNNI)),
    rq("1.5", "Did the use of negative controls, or other considerations, suggest serious uncontrolled confounding?", answers = R_YN)))

ROBINS2_D26 <- list(
  rq("2.1", "Were the intervention strategies distinguishable at the time when follow-up would have started in the target trial?"),
  rq("2.2", "Did all or nearly all outcome events occur after the intervention and comparator strategies could be distinguished?",
     "If N/PN/NI to 2.1", function(a, dev) rob_is(a, "2.1", NPNNI)),
  rq("2.3", "Did the analysis avoid problems arising from intervention strategies that are not distinguishable at the start of follow-up?",
     "If N/PN/NI to 2.2", function(a, dev) rob_is(a, "2.2", NPNNI), answers = c("NA", R_S)),
  rq("2.4", "Was classification of intervention status influenced by knowledge of the outcome or risk of the outcome?", answers = R_S),
  rq("2.5", "Were further classification errors (not influenced by knowledge of the outcome or risk of the outcome) likely?"),
  rq("3.1", "Did follow up in the analysis begin at the start of the intervention strategies being compared?", answers = R_W),
  rq("3.2", "Were outcome events during a period of follow-up after the start of the interventions excluded from the analysis?",
     "If Y/PY to 3.1", function(a, dev) rob_is(a, "3.1", YPY)),
  rq("3.3", "Was selection of participants into the study (or into the analysis) based on participant characteristics observed after the start of intervention (additional to the situations addressed in 3.1 and 3.2)?"),
  rq("3.4", "Were the post-intervention variables that influenced selection likely to be associated with intervention?",
     "If Y/PY to 3.3", function(a, dev) rob_is(a, "3.3", YPY)),
  rq("3.5", "Were the post-intervention variables that influenced selection likely to be influenced by the outcome or a cause of the outcome?",
     "If Y/PY to 3.4", function(a, dev) rob_is(a, "3.4", YPY)),
  rq("3.6", "Is it likely that the analysis corrected for all of the potential selection biases identified above?",
     "If SN to 3.1 or Y/PY to 3.5", function(a, dev) rob_is(a, "3.1", "SN") || rob_is(a, "3.5", YPY)),
  rq("3.7", "Did sensitivity analyses demonstrate that the likely impact of the potential selection biases identified above was minimal?",
     "If N/PN/NI to 3.6", function(a, dev) rob_is(a, "3.6", NPNNI)),
  rq("3.8", "Were potential selection biases identified above sufficiently severe that the result should not be included in a quantitative synthesis?",
     "If N/PN/NI to 3.7", function(a, dev) rob_is(a, "3.7", NPNNI)),
  rq("4.1", "Were complete data on intervention status available for all, or nearly all, participants?"),
  rq("4.2", "Were complete data on the outcome available for all, or nearly all, participants?"),
  rq("4.3", "Were complete data on important confounding variables available for all, or nearly all, participants?"),
  # printed as "4.1, 4.4 or 4.4" in the cribsheet; 4.1, 4.2 or 4.3 is meant
  rq("4.4", "Is the result based on a complete case analysis?", "If N/PN/NI to 4.1, 4.4 or 4.4",
     function(a, dev) rob_is(a, "4.1", NPNNI) || rob_is(a, "4.2", NPNNI) || rob_is(a, "4.3", NPNNI)),
  rq("4.5", "Was exclusion from the analysis because of missing data (in intervention, confounders or the outcome) likely to be related to the true value of the outcome?",
     "If Y/PY/NI to 4.4", function(a, dev) rob_is(a, "4.4", YPYNI)),
  rq("4.6", "Is the relationship between the outcome and missingness likely to be explained by the variables in the analysis model?",
     "If Y/PY/NI to 4.5", function(a, dev) rob_is(a, "4.5", YPYNI), answers = c("NA", R_W)),
  rq("4.7", "Was the analysis based on imputing missing values?", "If N/PN to 4.4",
     function(a, dev) rob_is(a, "4.4", NPN), answers = c("NA", "Y", "PY", "PN", "NI")),
  rq("4.8", "Is it reasonable to assume that data were \u2018missing at random\u2019 (MAR) or \u2018missing completely at random\u2019 (MCAR)?",
     "If Y/PY to 4.7", function(a, dev) rob_is(a, "4.7", YPY)),
  rq("4.9", "Was imputation performed appropriately?", "If Y/PY to 4.8",
     function(a, dev) rob_is(a, "4.8", YPY), answers = c("NA", R_W)),
  rq("4.10", "Was an appropriate alternative method used to correct for bias due to missing data?", "If N/PN/NI to 4.7",
     function(a, dev) rob_is(a, "4.7", NPNNI), answers = c("NA", R_W)),
  rq("4.11", "Is there evidence that the result was not biased by missing data?",
     "If PN/N/NI to 4.1, 4.2 or 4.3 AND (Y/PY/NI to 4.5 OR WN/SN/NI to 4.9 OR WN/SN/NI to 4.10)",
     function(a, dev) (rob_is(a, "4.1", NPNNI) || rob_is(a, "4.2", NPNNI) || rob_is(a, "4.3", NPNNI)) &&
       (rob_is(a, "4.5", YPYNI) || rob_is(a, "4.9", c("WN", "SN", "NI")) || rob_is(a, "4.10", c("WN", "SN", "NI"))),
     answers = c("NA", R_YN)),
  rq("5.1", "Could measurement or ascertainment of the outcome have differed between intervention groups?"),
  rq("5.2", "Were outcome assessors aware of the intervention received by study participants?"),
  rq("5.3", "Could assessment of the outcome have been influenced by knowledge of the intervention received?", "If Y/PY/NI to 5.2",
     function(a, dev) rob_is(a, "5.2", YPYNI), answers = c("NA", R_S)),
  rq("6.1", "Was the result reported in accordance with an available, pre-determined analysis plan?"),
  rq("6.2", "Is the numerical result being assessed likely to have been selected, on the basis of the results, from multiple outcome measurements (e.g. scales, definitions, time points) within the outcome domain?"),
  rq("6.3", "Is the numerical result being assessed likely to have been selected, on the basis of the results, from multiple analyses of the data?"),
  rq("6.4", "Is the numerical result being assessed likely to have been selected, on the basis of the results, from multiple subgroups?"))

ROBINS2_Q <- list(A = c(ROBINS2_PRE, ROBINS2_D1$A, ROBINS2_D26), B = c(ROBINS2_PRE, ROBINS2_D1$B, ROBINS2_D26))
ROB_DOMAIN_NAMES$robins2 <- c("Confounding", "Classification of interventions", "Selection of participants", "Missing data",
                              "Measurement of the outcome", "Selection of the reported result")
