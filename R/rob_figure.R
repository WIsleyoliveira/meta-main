# Risk-of-bias figure in the style of the robvis package (McGuinness & Higgins, Res Synth Methods 2021):
# traffic-light plot with the domain key and the judgement legend, and the weighted-summary bars below it.
# Uses robvis and ggplot2 when they are installed; otherwise the app falls back to the simpler base-graphics plots.

rob_has_robvis <- function() requireNamespace("robvis", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)

# Colours read from the reference figure (Low, Some concerns / Moderate, High / Serious, Critical, No information)
ROBVIS_COLOURS <- list(rob2 = c("#56BA37", "#E0E04A", "#AE2418", "#4EA1F7"),
                       robins = c("#56BA37", "#E0E04A", "#AE2418", "#74140C", "#4EA1F7"))
ROBVIS_TOOL <- c(rob2 = "ROB2", robins = "ROBINS-I")

# The data frame robvis expects: Study, D1.., Overall, Weight
rob_robvis_data <- function(d, tool, weight = FALSE) {
  out <- data.frame(Study = as.character(d$study), stringsAsFactors = FALSE)
  for (cn in ROB_TOOLS[[tool]]$short) out[[cn]] <- as.character(d[[cn]])
  out$Overall <- rob_overall(d, tool)
  if (weight) out$Weight <- 1   # some robvis versions require a weight column; 1 for every study gives the plain count
  out
}

# The summary bars with their legend taken out, so the legend can be centred under the whole figure and never cut off
rob_summary_parts <- function(d, tool) {
  g <- ggplot2::ggplotGrob(rob_fig_summary(d, tool))
  i <- which(grepl("^guide-box", g$layout$name) & !vapply(g$grobs, function(x) inherits(x, "zeroGrob"), TRUE))
  if (!length(i)) return(list(g = g, leg = NULL, w = 0, h = 0))
  i <- i[1]; leg <- g$grobs[[i]]
  prev <- grDevices::dev.cur(); tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, 200, 200)
  on.exit({ grDevices::dev.off(); if (prev > 1) try(grDevices::dev.set(prev), silent = TRUE); unlink(tmp) })
  lw <- grid::convertWidth(sum(leg$widths), "in", valueOnly = TRUE); lh <- grid::convertHeight(sum(leg$heights), "in", valueOnly = TRUE)
  g$heights[g$layout$t[i]] <- grid::unit(0, "pt"); g$grobs[[i]] <- ggplot2::zeroGrob()
  list(g = g, leg = leg, w = lw, h = lh)
}

# Size (inches) of the figure: traffic light on top, summary bars and a centred legend below. Proportions follow the reference figure
# (text large relative to the page, circles about 4% of the width).
ROB_PSIZE <- 10.5
rob_fig_size <- function(d, tool, psize = ROB_PSIZE, bars = TRUE) {
  nd <- length(ROB_TOOLS[[tool]]$short)
  tf_h <- 1.22 * (0.37 * nrow(d) + 0.9 + 0.135 * nd)
  w <- 1.22 * max(5.6, 2.0 + 0.055 * max(0, nchar(d$study)) + 0.5 * (nd + 1))
  if (!bars) return(c(w = w, h = tf_h, tf_h = tf_h, sm_h = 0, leg_h = 0))
  sp <- rob_summary_parts(d, tool)
  sm_h <- 1.22 * (0.62 + 0.14 * (nd + 1)); leg_h <- sp$h + 0.25
  c(w = max(w, sp$w + 0.5), h = tf_h + sm_h + leg_h, tf_h = tf_h, sm_h = sm_h, leg_h = leg_h)
}

# Different robvis versions expect different columns, so try the plain table first and add a weight column if the package asks for one
rob_try <- function(fun, d, tool, ...) {
  go <- function(w) do.call(fun, c(list(rob_robvis_data(d, tool, w), tool = ROBVIS_TOOL[[tool]], colour = ROBVIS_COLOURS[[tool]]), list(...)))
  tryCatch(go(FALSE), error = function(e) go(TRUE))
}
rob_fig_traffic <- function(d, tool, psize = ROB_PSIZE) rob_try(robvis::rob_traffic_light, d, tool, psize = psize)
rob_fig_summary <- function(d, tool) {
  args <- if ("weighted" %in% names(formals(robvis::rob_summary))) list(weighted = FALSE) else list()
  do.call(rob_try, c(list(robvis::rob_summary, d, tool), args))
}

# Draw the whole figure on the current device. letter: optional panel label ("A", "B"); bars = FALSE draws the traffic light alone.
rob_fig_draw <- function(d, tool, letter = "", psize = ROB_PSIZE, bars = TRUE) {
  sz <- rob_fig_size(d, tool, psize, bars)
  tf <- ggplot2::ggplotGrob(rob_fig_traffic(d, tool, psize))
  grid::grid.newpage()
  if (bars) {
    sp <- rob_summary_parts(d, tool)
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 1, heights = grid::unit(c(sz[["tf_h"]], sz[["sm_h"]], sz[["leg_h"]]), "in"))))
    grid::pushViewport(grid::viewport(layout.pos.row = 1)); grid::grid.draw(tf); grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 2)); grid::grid.draw(sp$g); grid::popViewport()
    if (!is.null(sp$leg)) {
      grid::pushViewport(grid::viewport(layout.pos.row = 3))
      grid::pushViewport(grid::viewport(x = 0.5, y = 0.5, width = grid::unit(sp$w, "in"), height = grid::unit(sp$h, "in")))
      grid::grid.draw(sp$leg); grid::popViewport(2)
    }
    grid::popViewport()
  } else grid::grid.draw(tf)
  if (nzchar(trimws(letter %||% "")))
    grid::grid.text(trimws(letter), x = grid::unit(0.012, "npc"), y = grid::unit(1, "npc") - grid::unit(0.05, "in"), just = c("left", "top"),
                    gp = grid::gpar(fontsize = 34, fontface = "bold"))
  invisible(sz)
}
