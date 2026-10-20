# Synthetic Kaplan-Meier figures with known curves, for validating the digitizer.
suppressMessages({library(ragg); library(jsonlite)})
set.seed(9); out <- "../out"; dir.create(out, showWarnings = FALSE)
spec <- list(
  A = list(file = "valfig_A_clean.png", w = 1000, h = 700, res = 100, dev = "png", ticks = FALSE),
  B = list(file = "valfig_B_small.png", w = 500, h = 350, res = 50, dev = "png", ticks = FALSE),
  C = list(file = "valfig_C_jpeg_censor_marks.jpg", w = 1000, h = 700, res = 100, dev = "jpeg", ticks = TRUE))
cal <- list()
for (k in names(spec)) {
  s <- spec[[k]]; f <- file.path(out, s$file)
  if (s$dev == "png") agg_png(f, width = s$w, height = s$h, res = s$res) else agg_jpeg(f, width = s$w, height = s$h, res = s$res, quality = 60)
  par(mar = c(5, 5, 2, 2), cex = if (k == "B") 0.7 else 1)
  tt <- seq(0, 60, by = .25); s1 <- exp(-0.025 * tt); s0 <- exp(-0.045 * tt)
  plot(NA, xlim = c(0, 60), ylim = c(0, 1), xaxs = "i", yaxs = "i", xlab = "Months", ylab = "Overall survival", axes = FALSE)
  axis(1, at = seq(0, 60, 12)); axis(2, at = seq(0, 1, .2), las = 1); box()
  lines(tt, s1, type = "s", col = "#2F5D8A", lwd = 3); lines(tt, s0, type = "s", col = "#B23A48", lwd = 3)
  if (s$ticks) for (r in c(0.025, 0.045)) { cc <- if (r == 0.025) "#2F5D8A" else "#B23A48"; tm <- sort(runif(40, 1, 59)); segments(tm, exp(-r * tm) - .02, tm, exp(-r * tm) + .02, col = cc, lwd = 2) }
  px <- grconvertX(c(0, 60), "user", "device"); py <- grconvertY(c(0, 1), "user", "device"); dev.off()
  cal[[k]] <- list(file = s$file, x0 = px[1], x60 = px[2], y0 = py[1], y1 = py[2])
}
# D: logarithmic x axis (1 to 1000), one curve y = 0.9 - 0.25 * log10(x)
agg_png(file.path(out, "valfig_D_logx.png"), width = 1000, height = 700, res = 100); par(mar = c(5, 5, 2, 2))
plot(NA, xlim = c(1, 1000), ylim = c(0, 1), log = "x", xaxs = "i", yaxs = "i", xlab = "Dose", ylab = "Response", axes = FALSE)
axis(1, at = c(1, 10, 100, 1000)); axis(2, at = seq(0, 1, .2), las = 1); box()
xx <- 10^seq(0, 3, length.out = 400); lines(xx, 0.9 - 0.25 * log10(xx), col = "#2F5D8A", lwd = 3)
px <- grconvertX(c(1, 1000), "user", "device"); py <- grconvertY(c(0, 1), "user", "device"); dev.off()
cal[["D"]] <- list(file = "valfig_D_logx.png", x0 = px[1], x60 = px[2], y0 = py[1], y1 = py[2])
write_json(cal, file.path(out, "valfig_cal.json"), auto_unbox = TRUE)
print(cal)
