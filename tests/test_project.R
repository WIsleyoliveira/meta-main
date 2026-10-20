# Project save/open: round trip, odd values, and bad files
suppressMessages({library(shiny)}); source("R/helpers.R"); source("R/km_helpers.R"); source("R/dig_module.R"); source("R/project.R")
ok <- function(c, m) if (!isTRUE(c)) stop(m, call. = FALSE) else cat("OK  ", m, "\n")
chr <- function(l) lapply(l, function(d) { d[] <- lapply(d, as.character); d })
ex <- chr(km_example())
std <- lapply(setNames(names(KM_COLS), names(KM_COLS)), function(w) km_standardize(ex[[w]], w)$data)
kf <- lapply(names(KM_COLS), function(w) list(data = std[[w]], orig = setNames(KM_COLS[[w]], KM_COLS[[w]]), name = paste0(w, ".csv"), sep = ";", enc = "UTF-8")); names(kf) <- names(KM_COLS)
dig <- list(curve = data.frame(study_id = "Trial \"Q\" \u00e9", arm = "Arm, 1", time = c("0", "12.5", "24"), survival = c("1", "0.8", "0.55")),
            risk = dig_empty("risk"), arms = data.frame(study_id = "Trial \"Q\" \u00e9", arm = "Arm, 1", group = "treatment", n_total = "100", total_events = NA, hr_published = NA, hr_ci_lower = NA, hr_ci_upper = NA), scale = "percent")
f <- tempfile(fileext = ".json"); proj_write(f, dig, kf, list(scale = "percent", time_unit = "months", conf = FALSE, xmax = 60))
r <- proj_read(f); ok(isTRUE(r$ok), "project reads back")
d2 <- r$project$digitizer
ok(identical(d2$curve$study_id, dig$curve$study_id) && identical(d2$curve$survival, dig$curve$survival), "digitized curve survives, including quotes, commas and accents")
ok(nrow(d2$risk) == 0 && identical(names(d2$risk), KM_COLS$risk), "empty table stays empty with the right columns")
ok(is.na(d2$arms$total_events) && d2$arms$n_total == "100" && d2$scale == "percent", "missing values and scale survive")
ok(identical(r$project$km_files$curve$data$survival, std$curve$survival) && r$project$km_files$curve$sep == ";", "Kaplan-Meier tables and separator survive")
ok(isTRUE(r$project$settings$conf == FALSE) && r$project$settings$xmax == 60 && r$project$settings$time_unit == "months", "settings survive")
bad <- function(txt) { g <- tempfile(); writeLines(txt, g); proj_read(g) }
ok(!bad("not json")$ok && !bad('{"app":"other"}')$ok && !bad('{"app":"Ponderis","format":99}')$ok, "wrong files are refused")
ok(!bad('{"app":"Ponderis","format":1,"digitizer":{"curve":[{"a":1}]}}')$ok, "a table with missing columns is refused")
ok(!proj_read("/no/such/file")$ok, "missing file is refused")
b2 <- bad('{"app":"Ponderis","format":1,"settings":{"scale":"percent","evil":"x","xmax":[1,2]}}'); ok(b2$ok && identical(names(b2$project$settings), "scale"), "unknown or malformed settings are dropped")
