# Risk-of-bias algorithms: map the answers to the signalling questions onto the tool's suggested judgement.
# Plain R, no AI: the AI only proposes answers. Expects answers already passed through rob_force_na().
#
# RoB 2: Tables 4, 6, 8, 10, 12 and 14 of the RoB 2 guidance (22 August 2019). Where a table leaves a combination
# out, the matching figure of the guidance decides (noted at the place).

ROB2_LEVELS <- c("Low", "Some concerns", "High")
rob_worst <- function(x, levels) levels[max(match(x, levels))]

rob2_domain <- function(domain, answers, variant = "assignment") {
  a <- function(id) rob_a(answers, id)
  y <- function(id) rob_yes(a(id)); n <- function(id) rob_no(a(id)); ni <- function(id) a(id) == "NI"
  yni <- function(id) y(id) || ni(id)
  na <- function(id) is.na(a(id)) || a(id) == "NA"
  switch(as.character(domain),
    "1" = {
      if (n("1.2")) "High"
      else if (ni("1.2")) { if (y("1.3")) "High" else "Some concerns" }
      else if (y("1.3")) "Some concerns"
      else if (n("1.1")) "Some concerns"   # absent from Table 4; Figure 1: sequence not random -> Some concerns
      else "Low"
    },
    "2" = if (variant == "assignment") {
      aware <- yni("2.1") || yni("2.2")
      part1 <- if (!aware || n("2.3")) "Low" else if (ni("2.3")) "Some concerns" else if (n("2.4")) "Some concerns"
               else if (y("2.5")) "Some concerns" else "High"
      part2 <- if (y("2.6")) "Low" else if (n("2.7")) "Some concerns" else "High"
      rob_worst(c(part1, part2), ROB2_LEVELS)
    } else {
      aware <- yni("2.1") || yni("2.2")
      problem <- (aware && !na("2.3") && !y("2.3")) || (!na("2.4") && yni("2.4")) || (!na("2.5") && yni("2.5"))
      if (!problem) "Low" else if (y("2.6")) "Some concerns" else "High"
    },
    "3" = {
      if (y("3.1") || y("3.2") || n("3.3")) "Low" else if (n("3.4")) "Some concerns" else "High"
    },
    "4" = {
      if (y("4.1") || y("4.2")) "High"
      else if (n("4.3") || n("4.4")) { if (n("4.2")) "Low" else "Some concerns" }
      else if (n("4.5")) "Some concerns"
      else "High"
    },
    "5" = {
      if (y("5.2") || y("5.3")) "High"
      else if (n("5.2") && n("5.3") && y("5.1")) "Low"
      else "Some concerns"
    },
    stop("unknown RoB 2 domain ", domain))
}

rob2_domains <- function(answers, variant = "assignment")
  setNames(vapply(1:5, function(d) rob2_domain(d, answers, variant), ""), paste0("D", 1:5))

# Overall: Low only if every domain is Low; High if any domain is High; otherwise Some concerns.
# (The guidance also allows High for several domains with Some concerns: a reviewer's judgement, shown as a note.)
rob2_overall <- function(judgements) rob_worst(judgements, ROB2_LEVELS)

# ---------------------------- ROBINS-I V2 (cribsheet of 20 November 2025, flowcharts) ----------------------------
# Domain 1 "Low" means "Low risk of bias except for concerns about uncontrolled confounding".
ROBINS2_LEVELS <- c("Low", "Moderate", "Serious", "Critical")

robins2_variant <- function(answers) if (rob_is(answers, "C4", "Y")) "B" else "A"
robins2_prelim_critical <- function(answers) rob_is(answers, "B2", c("Y", "PY")) || rob_is(answers, "B3", c("Y", "PY"))

robins2_domain <- function(domain, answers, variant = "A") {
  is <- function(id, codes) rob_is(answers, id, codes)
  YP <- c("Y", "PY"); NP <- c("N", "PN"); NPI <- c("N", "PN", "NI")
  neg <- function(id) is(id, YP)   # negative controls etc. suggest serious uncontrolled confounding (no NI option: no answer = no)
  switch(as.character(domain),
    "1" = if (variant == "A") {
      if (is("1.1", c(YP, "WN"))) {
        if (is("1.3", YP)) { if (neg("1.4")) "Critical" else if (is("1.2", YP)) "Serious" else "Critical" }
        else if (is("1.1", YP)) {
          if (is("1.2", YP)) { if (neg("1.4")) "Serious" else "Low" }
          else if (is("1.2", "WN")) { if (neg("1.4")) "Serious" else "Moderate" }
          else "Serious"
        } else {                                  # 1.1 WN
          if (is("1.2", c(YP, "WN"))) { if (neg("1.4")) "Serious" else "Moderate" } else "Serious"
        }
      } else if (neg("1.4")) "Critical" else "Serious"
    } else {
      if (is("1.1", YP)) {
        if (is("1.2", YP)) {
          if (is("1.3", YP)) { if (neg("1.5")) "Serious" else "Low" }
          else if (is("1.3", "WN")) { if (neg("1.5")) "Serious" else "Moderate" }
          else "Serious"
        } else if (is("1.2", "WN")) {
          if (is("1.3", c(YP, "WN"))) { if (neg("1.5")) "Serious" else "Moderate" } else "Serious"
        } else if (neg("1.5")) "Critical" else "Serious"
      } else if (is("1.4", YP)) "Critical"
      else if (neg("1.5")) "Critical" else "Serious"
    },
    "2" = {
      # the way in sets the starting row (0 = top, 1 = middle, 2 = bottom), 2.4 moves it down, 2.5 one step more
      row <- if (is("2.1", YP) || is("2.2", YP) || is("2.3", "SY")) 0 else if (is("2.3", c("WY", "NI"))) 1 else 2
      step <- row + (if (is("2.4", NP)) 0 else if (is("2.4", c("WY", "NI"))) 1 else 2)
      if (step >= 3) "Critical" else ROBINS2_LEVELS[step + 1 + (if (is("2.5", NP)) 0 else 1)]
    },
    "3" = {
      a <- if (is("3.1", YP)) { if (is("3.2", YP)) "Moderate" else "Low" } else if (is("3.1", "SN")) "Serious" else "Moderate"
      b <- if (is("3.3", NP)) "Low" else if (is("3.3", YP)) {
             if (is("3.4", NP)) "Low" else if (is("3.4", YP)) { if (is("3.5", YP)) "Serious" else "Moderate" } else "Moderate"
           } else "Moderate"
      ab <- rob_worst(c(a, b), ROBINS2_LEVELS)
      if (ab != "Serious") ab
      else if (is("3.6", YP) || is("3.7", YP)) "Moderate"
      else if (is("3.8", YP)) "Critical" else "Serious"
    },
    "4" = {
      by_411 <- function(level) {   # which 4.11 box: 0 = top, 1 = middle, 2 = bottom
        ok <- is("4.11", YP)
        c(if (ok) "Low" else "Moderate", if (ok) "Moderate" else "Serious", if (ok) "Serious" else "Critical")[level + 1]
      }
      if (is("4.1", YP) && is("4.2", YP) && is("4.3", YP)) "Low"
      else if (is("4.4", c(YP, "NI"))) {
        if (is("4.5", NP)) "Low"
        else if (is("4.6", YP)) by_411(0) else if (is("4.6", c("WN", "NI"))) by_411(1) else by_411(2)
      } else if (is("4.7", YP)) {
        if (!is("4.8", YP)) "Serious"
        else if (is("4.9", YP)) "Low" else if (is("4.9", c("WN", "NI"))) by_411(1) else by_411(2)
      } else {
        if (is("4.10", YP)) "Low" else if (is("4.10", c("WN", "NI"))) by_411(1) else by_411(2)
      }
    },
    "5" = {
      if (is("5.1", YP)) "Serious"
      else if (is("5.1", NP)) {
        if (is("5.2", NP)) "Low" else if (is("5.3", NP)) "Low" else if (is("5.3", "SY")) "Serious" else "Moderate"
      } else {
        if (is("5.2", NP)) "Moderate" else if (is("5.3", "SY")) "Serious" else "Moderate"
      }
    },
    "6" = {
      if (is("6.1", YP)) "Low" else {
        x <- c(rob_a(answers, "6.2"), rob_a(answers, "6.3"), rob_a(answers, "6.4")); x[is.na(x)] <- "NI"
        n_y <- sum(x %in% YP); n_ni <- sum(x == "NI")
        if (n_y >= 2) "Critical" else if (n_y == 1 || n_ni == 3) "Serious" else if (n_ni >= 1) "Moderate" else "Low"
      }
    },
    stop("unknown ROBINS-I V2 domain ", domain))
}

robins2_domains <- function(answers) {
  v <- robins2_variant(answers)
  setNames(vapply(1:6, function(d) robins2_domain(d, answers, v), ""), paste0("D", 1:6))
}

# Overall: the worst domain; Critical without assessing the domains when B2 or B3 is Y/PY.
# (Several Moderate -> Serious, several Serious -> Critical are the reviewer's additive judgement, shown as a note.)
robins2_overall <- function(judgements, critical_prelim = FALSE)
  if (isTRUE(critical_prelim)) "Critical" else rob_worst(judgements, ROBINS2_LEVELS)
