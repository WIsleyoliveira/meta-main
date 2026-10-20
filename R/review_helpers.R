# Helpers for the "Review tools" mode: PRISMA 2020 flow diagram, risk-of-bias tables and plots, GRADE certainty of evidence.
# Plain functions (no Shiny) so they can be tested on their own.

# =============================== PRISMA 2020 ===============================

# Reasons for exclusion at full-text stage: one per line, "reason, n" or "reason: n" or "reason (n)". Returns data.frame(reason, n).
prisma_parse_reasons <- function(txt) {
  ln <- trimws(unlist(strsplit(as.character(txt %||% ""), "\n", fixed = TRUE))); ln <- ln[nzchar(ln)]
  if (!length(ln)) return(data.frame(reason = character(0), n = numeric(0), stringsAsFactors = FALSE))
  one <- function(s) {
    m <- regmatches(s, regexec("^(.*?)[,:;\\(]\\s*(?:n\\s*=\\s*)?([0-9]+)\\s*\\)?$", s, perl = TRUE))[[1]]
    if (length(m) != 3) m <- regmatches(s, regexec("^(.*?)\\s+n\\s*=\\s*([0-9]+)$", s, perl = TRUE))[[1]]
    if (length(m) == 3) c(trimws(m[2]), m[3]) else c(s, NA)
  }
  r <- t(vapply(ln, one, character(2)))
  data.frame(reason = r[, 1], n = suppressWarnings(as.numeric(r[, 2])), stringsAsFactors = FALSE, row.names = NULL)
}

# From the counts a person knows, work out the rest of the flow and check it is consistent.
# db_list / removed_list: data.frame(reason, n) of databases searched and of "other" removal reasons (names shown in the diagram).
prisma_flow <- function(db, other = 0, dup = 0, auto = 0, other_removed = 0, excl_screen = 0, not_retrieved = 0, reasons = NULL, in_ma = NA,
                        db_list = NULL, removed_list = NULL, other_name = "", use_sought = TRUE) {
  num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) != 1 || is.na(x)) 0 else x }
  clean <- function(d) { if (is.null(d) || !nrow(d)) return(NULL); d$n[is.na(d$n)] <- 0; d }
  db_list <- clean(db_list); removed_list <- clean(removed_list)
  db <- if (!is.null(db_list)) sum(db_list$n) else num(db)
  other_removed <- if (!is.null(removed_list)) sum(removed_list$n) else num(other_removed)
  other <- num(other); dup <- num(dup); auto <- num(auto)
  excl_screen <- num(excl_screen); not_retrieved <- if (isTRUE(use_sought)) num(not_retrieved) else 0
  if (is.null(reasons)) reasons <- data.frame(reason = character(0), n = numeric(0))
  reasons$n[is.na(reasons$n)] <- 0
  identified <- db + other
  removed <- dup + auto + other_removed
  screened <- identified - removed
  sought <- screened - excl_screen
  assessed <- sought - not_retrieved
  excl_full <- sum(reasons$n)
  included <- assessed - excl_full
  err <- character(0)
  chk <- c(screened = screened, sought = sought, assessed = assessed, included = included)
  for (nm in names(chk)) if (chk[[nm]] < 0) err <- c(err, sprintf("The numbers do not add up: %s would be negative (%d). Check what was removed or excluded at the step before.", nm, chk[[nm]]))
  if (identified <= 0) err <- c(err, "Enter the databases searched and how many records each gave.")
  ma <- num(in_ma)
  wrn <- character(0)
  if (!is.na(in_ma) && nzchar(as.character(in_ma)) && length(err) == 0 && ma > included)
    wrn <- c(wrn, "More studies in the meta-analysis than studies included in the review.")
  list(db = db, other = other, dup = dup, auto = auto, other_removed = other_removed, identified = identified, removed = removed,
       screened = screened, excl_screen = excl_screen, sought = sought, not_retrieved = not_retrieved, assessed = assessed,
       reasons = reasons, excl_full = excl_full, included = included, in_ma = if (is.na(in_ma) || !nzchar(as.character(in_ma))) NA else ma,
       db_list = db_list, removed_list = removed_list, other_name = trimws(as.character(other_name %||% "")), use_sought = isTRUE(use_sought),
       errors = err, warnings = wrn)
}

# PRISMA 2020 flow diagram, drawn after the official template (yellow title banner, light-blue stage bands, black-outlined boxes, Arial-like text).
# bands = FALSE hides the Identification / Screening / Included side labels; banner = FALSE hides the title banner.
prisma_plot <- function(fl, bands = TRUE, banner = TRUE) {
  if (length(fl$errors)) { plot.new(); text(0.5, 0.5, paste(strwrap(fl$errors[1], 50), collapse = "\n")); return(invisible()) }
  INK <- "#000000"; BAND <- "#9DC3E6"; YEL <- "#FFC000"
  op <- par(mar = c(0.2, 0.2, 0.2, 0.2), xpd = NA, family = "sans"); on.exit(par(op))
  plot.new(); plot.window(xlim = c(0, 100), ylim = c(0, 100))
  fm <- function(x) format(x, big.mark = ",", trim = TRUE)
  wrap <- function(s, w) strwrap(s, w)
  nline <- function(v) length(v)
  # ---- text of every box, as a vector of lines (a leading "  " marks an indented line) ----
  l_id <- c("Records identified from:",
            if (!is.null(fl$db_list)) paste0("   ", unlist(lapply(seq_len(nrow(fl$db_list)), function(i) {
              w <- wrap(paste0(fl$db_list$reason[i], " (n = ", fm(fl$db_list$n[i]), ")"), 28); c(w[1], if (length(w) > 1) paste0(" ", w[-1])) })))
            else paste0("   Databases (n = ", fm(fl$db), ")"),
            if (fl$other > 0) paste0("   ", unlist(lapply(paste0(if (nzchar(fl$other_name)) fl$other_name else "Registers and other sources", " (n = ", fm(fl$other), ")"), wrap, 28))))
  l_rem <- c("Records removed before screening:",
             paste0("   Duplicate records removed (n = ", fm(fl$dup), ")"),
             if (fl$auto > 0) paste0("   ", wrap(paste0("Marked as ineligible by automation tools (n = ", fm(fl$auto), ")"), 34)),
             if (!is.null(fl$removed_list)) paste0("   ", unlist(lapply(seq_len(nrow(fl$removed_list)), function(i) wrap(paste0(fl$removed_list$reason[i], " (n = ", fm(fl$removed_list$n[i]), ")"), 34))))
             else if (fl$other_removed > 0) paste0("   Removed for other reasons (n = ", fm(fl$other_removed), ")"))
  rs <- fl$reasons
  l_exf <- c(paste0("Reports excluded (n = ", fm(fl$excl_full), "):"),
             if (nrow(rs)) paste0("   ", unlist(lapply(seq_len(nrow(rs)), function(i) wrap(paste0(rs$reason[i], " (n = ", fm(rs$n[i]), ")"), 34)))))
  l_inc <- c("Studies included in review", paste0("(n = ", fm(fl$included), ")"),
             if (!is.na(fl$in_ma)) c("", "Included in meta-analysis", paste0("(n = ", fm(fl$in_ma), ")")))
  rows <- list(
    list(id = "ident", L = l_id, R = l_rem),
    list(id = "screen", L = c("Records screened", paste0("(n = ", fm(fl$screened), ")")), R = c("Records excluded", paste0("(n = ", fm(fl$excl_screen), ")"))),
    if (fl$use_sought) list(id = "sought", L = c("Reports sought for retrieval", paste0("(n = ", fm(fl$sought), ")")), R = c("Reports not retrieved", paste0("(n = ", fm(fl$not_retrieved), ")"))),
    list(id = "assess", L = c("Reports assessed for eligibility", paste0("(n = ", fm(fl$assessed), ")")), R = l_exf),
    list(id = "incl", L = l_inc, R = NULL))
  rows <- Filter(Negate(is.null), rows)
  # ---- layout: line height and gaps in plot units, scaled down if the diagram is tall ----
  lh0 <- 2.9; pad0 <- 2.2; gap0 <- 5
  hgt <- function(v, lh) if (is.null(v)) 0 else nline(v) * lh + 2 * pad0 * lh / lh0
  need <- sum(vapply(rows, function(r) max(hgt(r$L, lh0), hgt(r$R, lh0)), 0)) + gap0 * (length(rows) - 1)
  avail <- 100 - if (banner) 11 else 5
  s <- min(1, avail / need); lh <- lh0 * s; gap <- gap0 * s; pad <- pad0 * s; cx <- max(0.5, 0.72 * s)
  XL <- c(11, 46); XR <- c(55, 97); y <- 99 - if (banner) 6.2 else 0
  # (simple rounded rectangle: a rectangle with the four corners cut by arcs)
  rbox <- function(x1, y1, x2, y2, col, r = 1.8, border = NA, lwd = 1) {
    t <- seq(0, pi / 2, length.out = 9)
    xs <- c(x2 - r + r * cos(t), x1 + r + r * cos(t + pi / 2), x1 + r + r * cos(t + pi), x2 - r + r * cos(t + 3 * pi / 2))
    ys <- c(y2 - r + r * sin(t), y2 - r + r * sin(t + pi / 2), y1 + r + r * sin(t + pi), y1 + r + r * sin(t + 3 * pi / 2))
    polygon(xs, ys, col = col, border = border, lwd = lwd)
  }
  tbox <- function(x1, y1, x2, y2, lines, fill = "white", center = FALSE, bold = FALSE) {
    rect(x1, y1, x2, y2, col = fill, border = INK, lwd = 1.3)
    if (center) text((x1 + x2) / 2, (y1 + y2) / 2, paste(lines, collapse = "\n"), cex = cx, font = 2, col = INK)
    else for (i in seq_along(lines)) text(x1 + 1.4, y2 - pad - (i - 0.5) * lh, lines[i], adj = c(0, 0.5), cex = cx, font = 2, col = INK)
  }
  arr <- function(x1, y1, x2, y2) arrows(x1, y1, x2, y2, length = 0.08, lwd = 1.2, col = INK)
  if (banner) { rbox(XL[1], 94.2, XR[2], 99, YEL, r = 1.2, border = "#BF9000", lwd = 1.5); text((XL[1] + XR[2]) / 2, 96.6, "Identification of studies via databases and registers", font = 2, cex = max(0.6, 0.8 * s), col = INK) }
  tops <- numeric(length(rows)); bots <- numeric(length(rows))
  for (i in seq_along(rows)) {
    r <- rows[[i]]; hL <- hgt(r$L, lh0) * s; hR <- if (is.null(r$R)) 0 else max(hgt(r$R, lh0) * s, hL)
    tops[i] <- y; bots[i] <- y - max(hL, hR)
    isl <- r$id == "incl"
    tbox(XL[1], y - hL, XL[2], y, r$L, fill = if (isl) "#DDEBF7" else "white", center = !(r$id == "ident") && !is.null(r$R) || isl, bold = isl)
    if (!is.null(r$R)) {
      tbox(XR[1], y - hR, XR[2], y, r$R, center = !(r$id %in% c("ident", "assess")))
      arr(XL[2], y - hL / 2, XR[1], y - hL / 2)
    }
    hrow <- max(hL, hR)
    if (i < length(rows)) { arr((XL[1] + XL[2]) / 2, y - hL, (XL[1] + XL[2]) / 2, y - hrow - gap); y <- y - hrow - gap }
  }
  # ---- stage bands on the left ----
  if (bands) {
    ids <- vapply(rows, function(r) r$id, "")
    band <- function(i1, i2, lab) { y1 <- bots[i2] - 0.5; y2 <- tops[i1] + 0.5
      rbox(2, y1, 8.2, y2, BAND, r = 2, border = INK, lwd = 1.5); text(5.1, (y1 + y2) / 2, lab, srt = 90, font = 2, cex = max(0.65, 0.8 * s), col = INK) }
    iS <- which(ids == "screen"); iA <- which(ids == "assess"); iI <- which(ids == "incl")
    band(1, 1, "Identification"); band(iS, iA, "Screening"); band(iI, iI, "Included")
  }
  text(98, 1.2, "PRISMA 2020 flow diagram. Page MJ, et al. BMJ 2021;372:n71", adj = c(1, 0), cex = 0.55, col = "grey35")
}

# =============================== Risk of bias ===============================

ROB_TOOLS <- list(
  rob2 = list(name = "RoB 2 (randomized trials)",
              domains = c("Randomisation process", "Deviations from intended interventions", "Missing outcome data",
                          "Measurement of the outcome", "Selection of the reported result"),
              short = c("D1", "D2", "D3", "D4", "D5"),
              levels = c("Low", "Some concerns", "High")),
  robins = list(name = "ROBINS-I (non-randomized studies)",
                domains = c("Confounding", "Selection of participants", "Classification of interventions", "Deviations from intended interventions",
                            "Missing data", "Measurement of outcomes", "Selection of the reported result"),
                short = c("D1", "D2", "D3", "D4", "D5", "D6", "D7"),
                levels = c("Low", "Moderate", "Serious", "Critical", "No information")),
  nos = list(name = "Newcastle-Ottawa Scale (cohort or case-control)",
             domains = c("Selection (0-4 stars)", "Comparability (0-2 stars)", "Outcome or exposure (0-3 stars)"),
             short = c("Sel", "Comp", "Out"), levels = NULL, max = c(4, 2, 3))
)

# Colours for the judgements (colour-blind friendly: traffic-light order, with symbols also drawn in the plot)
ROB_COL <- c("Low" = "#3E9B5B", "Some concerns" = "#E8B53A", "High" = "#C0392B", "Moderate" = "#E8B53A", "Serious" = "#E07B39",
             "Critical" = "#C0392B", "No information" = "#9AA5AE", "Good" = "#3E9B5B", "Fair" = "#E8B53A", "Poor" = "#C0392B")
ROB_SYM <- c("Low" = "+", "Some concerns" = "?", "High" = "-", "Moderate" = "?", "Serious" = "-", "Critical" = "x", "No information" = "")

rob_template <- function(tool, studies) {
  studies <- trimws(as.character(studies)); studies <- studies[nzchar(studies)]
  d <- data.frame(study = studies, stringsAsFactors = FALSE)
  for (j in seq_along(ROB_TOOLS[[tool]]$domains)) d[[ROB_TOOLS[[tool]]$short[j]]] <- NA_character_
  d
}

# Overall judgement from the domains. RoB 2 and ROBINS-I: the worst domain (the official algorithms add judgement for several
# "some concerns" domains, so the app says clearly that it is the default rule and can be changed by hand).
rob_overall <- function(d, tool) {
  tl <- ROB_TOOLS[[tool]]; cols <- tl$short
  if (tool == "nos") {
    s <- vapply(seq_len(nrow(d)), function(i) {
      v <- suppressWarnings(as.numeric(unlist(d[i, cols]))); if (anyNA(v)) return(NA_character_)
      if (v[1] >= 3 && v[2] >= 1 && v[3] >= 2) "Good" else if (v[1] >= 2 && v[2] >= 1 && v[3] >= 2) "Fair" else "Poor"
    }, character(1))
    return(s)
  }
  rank <- setNames(seq_along(tl$levels), tl$levels)
  vapply(seq_len(nrow(d)), function(i) {
    v <- unlist(d[i, cols]); if (anyNA(v) || any(!nzchar(v))) return(NA_character_)
    if (tool == "robins") {
      real <- v[v != "No information"]
      if (!length(real)) return("No information")
      worst <- names(rank)[max(rank[real])]
      if (any(v == "No information") && rank[worst] <= rank["Moderate"]) return("No information")
      return(worst)
    }
    names(rank)[max(rank[v])]
  }, character(1))
}

# Traffic-light plot: one row per study, one column per domain plus overall
rob_traffic_plot <- function(d, tool) {
  tl <- ROB_TOOLS[[tool]]; cols <- tl$short
  if (!nrow(d)) { plot.new(); text(0.5, 0.5, "Add studies to draw the plot."); return(invisible()) }
  ov <- rob_overall(d, tool)
  cell <- function(v) if (tool == "nos") NA_character_ else v
  n <- nrow(d); nc <- length(cols) + 1
  op <- par(mar = c(6, max(8, 0.55 * max(nchar(d$study))) * 0.6 + 1, 4, 1), xpd = NA); on.exit(par(op))
  plot.new(); plot.window(xlim = c(0.4, nc + 0.6), ylim = c(n + 0.6, 0.4))
  heads <- c(cols, "Overall")
  text(seq_len(nc), 0.1, heads, font = 2, cex = 0.85, adj = c(0.5, 0))
  text(rep(0.3, n), seq_len(n), d$study, adj = c(1, 0.5), cex = 0.85)
  for (i in seq_len(n)) for (j in seq_len(nc)) {
    v <- if (j <= length(cols)) as.character(d[[cols[j]]][i]) else ov[i]
    if (tool == "nos" && j <= length(cols)) {
      if (is.na(v) || !nzchar(v)) next
      text(j, i, v, cex = 0.9); next
    }
    if (is.na(v) || !nzchar(v) || is.na(ROB_COL[v])) next
    symbols(j, i, circles = 0.22, inches = FALSE, add = TRUE, bg = ROB_COL[[v]], fg = NA)
    if (!is.na(ROB_SYM[v])) text(j, i, ROB_SYM[[v]], col = "white", font = 2, cex = 0.9)
  }
  lv <- if (tool == "nos") c("Good", "Fair", "Poor") else tl$levels
  legend("bottom", legend = lv, pch = 21, pt.bg = ROB_COL[lv], col = NA, horiz = TRUE, inset = c(0, -0.12), bty = "n", cex = 0.85, pt.cex = 1.6)
  legend_title <- tl$name
  mtext(legend_title, side = 3, line = 2.2, adj = 0, font = 2, cex = 0.95)
  key <- strwrap(paste(paste0(cols, " = ", tl$domains), collapse = ";  "), 95)
  mtext(key, side = 1, line = 3.6 + 0.9 * (seq_along(key) - 1), adj = 0, cex = 0.62, col = "grey30")
}

# Summary bars: share of studies per judgement in each domain
rob_summary_plot <- function(d, tool) {
  tl <- ROB_TOOLS[[tool]]
  if (!nrow(d) || tool == "nos") { plot.new(); text(0.5, 0.5, if (tool == "nos") "The summary bars are for RoB 2 and ROBINS-I." else "Add studies to draw the plot."); return(invisible()) }
  cols <- tl$short; ov <- rob_overall(d, tool)
  tabs <- lapply(c(cols, "Overall"), function(cn) {
    v <- if (cn == "Overall") ov else as.character(d[[cn]])
    v <- v[!is.na(v) & nzchar(v)]
    tb <- table(factor(v, levels = tl$levels)); if (sum(tb)) 100 * tb / sum(tb) else tb * NA_real_
  })
  mat <- do.call(cbind, tabs); colnames(mat) <- c(paste0(cols, " ", tl$domains), "Overall")
  mat[is.na(mat)] <- 0
  op <- par(mar = c(4, 22, 3, 1), xpd = NA); on.exit(par(op))
  barplot(mat[, ncol(mat):1, drop = FALSE], horiz = TRUE, col = ROB_COL[tl$levels], border = "white", las = 1, xlim = c(0, 100), xlab = "Studies (%)", cex.names = 0.8)
  legend("top", legend = tl$levels, fill = ROB_COL[tl$levels], horiz = TRUE, inset = c(0, -0.12), bty = "n", cex = 0.85)
}

rob_table <- function(d, tool) {
  tl <- ROB_TOOLS[[tool]]; out <- d
  i <- match(tl$short, names(out)); good <- !is.na(i); names(out)[i[good]] <- tl$domains[good]   # a tool switch can leave the old columns for a moment
  out[[if (tool == "nos") "Quality" else "Overall"]] <- rob_overall(d, tool)
  out
}
