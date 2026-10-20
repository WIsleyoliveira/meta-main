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
