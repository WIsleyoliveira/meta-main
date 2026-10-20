# Black-and-white KM figure (solid and dashed curve that cross, light grid lines) for testing the digitizer on curves without colour.
suppressMessages({library(ragg); library(jsonlite)})
out <- "../out"; f <- file.path(out, "valfig_E_black.png")
agg_png(f, width = 1000, height = 700, res = 100); par(mar = c(5, 5, 2, 2))
tt <- seq(0, 22, by = 0.05)
# steep early drop, then a long almost-linear decline; the two curves stay within about 0.02 of each other and cross twice
sA <- 0.88 * exp(-0.045 * tt) + 0.12 * exp(-6 * tt)
# the dashed curve falls faster than the continuous one in the first months, lies under it from about 2 to 4 years, then moves around it and crosses it several times
sB <- sA - 0.09 * (1 - exp(-tt / 0.15)) * exp(-tt / 1.5) + ifelse(tt < 4, 0, 0.025 * sin((tt - 4) / 4))   # falls faster than the continuous curve at first, joins it, then crosses it
plot(NA, xlim = c(0, 22), ylim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "Years", ylab = "Survival", axes = FALSE)
abline(h = seq(0, 1, .1), col = "grey88", lwd = 1.5)
axis(1, at = c(0, 5, 10, 15, 20)); axis(2, at = seq(0, 1, .2), las = 1); box()
lines(tt, sA, type = "s", col = "black", lwd = 2.5); lines(tt, sB, type = "s", col = "black", lwd = 2.5, lty = 2)
segments(4, 0.18, 8, 0.18, lwd = 2.5); segments(4, 0.10, 8, 0.10, lwd = 2.5, lty = 2); text(8.5, 0.18, "M to M", adj = 0); text(8.5, 0.10, "F to M", adj = 0)  # legend, black like the curves
px <- grconvertX(c(0, 20), "user", "device"); py <- grconvertY(c(0, 1), "user", "device"); dev.off()
write_json(list(file = "valfig_E_black.png", x0 = px[1], x20 = px[2], y0 = py[1], y1 = py[2]), file.path(out, "valfig_E_cal.json"), auto_unbox = TRUE)
# truth, to compare against the traced solid curve
write.csv(data.frame(t = tt, solid = sA, dashed = sB), file.path(out, "valfig_E_truth.csv"), row.names = FALSE)

# Figure F: like a published figure with a thin grey frame, a curve that begins a little to the right of the y axis,
# and curves that run past the last labelled x tick (20) to 22.5
f <- file.path(out, "valfig_F_offset.png")
agg_png(f, width = 1000, height = 700, res = 100); par(mar = c(5, 5, 2, 2))
tt <- seq(0, 22.5, by = 0.05)
sA <- 0.88 * exp(-0.045 * tt) + 0.12 * exp(-6 * tt)
sB <- sA - 0.09 * (1 - exp(-tt / 0.15)) * exp(-tt / 1.5) + ifelse(tt < 4, 0, 0.025 * sin((tt - 4) / 4))   # falls faster than the continuous curve at first, joins it, then crosses it
plot(NA, xlim = c(-1, 23.5), ylim = c(0.07, 1.0), xaxs = "i", yaxs = "i", xlab = "Years", ylab = "Survival", axes = FALSE)
abline(h = seq(0.1, 1, .1), col = "grey88", lwd = 1.5)
axis(1, at = c(0, 5, 10, 15, 20), col = "grey35", lwd = 0.8); axis(2, at = seq(0.2, 1, .2), las = 1, col = "grey35", lwd = 0.8)
# the frame is thin and dark, and the x axis has a gap (about a quarter of its length), as in figures with an overlaid table
segments(-1, 0.07, 8, 0.07, col = "grey20", lwd = 1); segments(14.5, 0.07, 23.5, 0.07, col = "grey20", lwd = 1); segments(-1, 0.07, -1, 1, col = "grey20", lwd = 1)
lines(tt, sA, type = "s", col = "black", lwd = 2.5); lines(tt, sB, type = "s", col = "black", lwd = 2.5, lty = 2)
segments(4, 0.20, 8, 0.20, lwd = 2.5); segments(4, 0.13, 8, 0.13, lwd = 2.5, lty = 2); text(8.5, 0.20, "M to M", adj = 0); text(8.5, 0.13, "F to M", adj = 0)
# calibration: x = 0 is the origin of the curve (one unit right of the frame), x = 20 the last tick; y = 0 is 7% below the frame
px <- grconvertX(c(0, 20), "user", "device"); py <- grconvertY(c(0, 1), "user", "device"); dev.off()
write_json(list(file = "valfig_F_offset.png", x0 = px[1], x20 = px[2], y0 = py[1], y1 = py[2]), file.path(out, "valfig_F_cal.json"), auto_unbox = TRUE)
write.csv(data.frame(t = tt, solid = sA, dashed = sB), file.path(out, "valfig_F_truth.csv"), row.names = FALSE)
