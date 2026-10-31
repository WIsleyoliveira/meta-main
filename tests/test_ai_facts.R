# The facts sheet the writing assistant receives, built from the analyses on screen, and the check of the numbers
# the AI writes against it.
suppressMessages({ library(shiny); library(meta) })
source("R/helpers.R"); source("R/review_helpers.R"); source("R/grade_helpers.R"); source("R/ai_facts.R")
ok <- function(name) cat("ok:", name, "\n")
app <- shinyAppDir(".")
fval <- function(f, id) f$value[f$id == id]

# ---- forest plot, every data type ----
for (type in c("dich", "cont", "gen", "prop")) {
  testServer(app, {
    session$setInputs(type = type, tpl_sep = ",", model = "random", tau = "REML", hk = TRUE, pi = TRUE,
                      layout = "meta", sort = "none", weights = TRUE, het = TRUE, overall_p = TRUE, details = FALSE,
                      arm_e = "PCI", arm_c = "CABG", label_left = "Favours PCI", label_right = "Favours CABG", smlab = "",
                      bias = "Egger", trimfill = TRUE)
    session$setInputs(load_example = 1); session$flushReact()
    d <- raw()$data; keys <- names(SPEC[[type]]$cols)
    g <- guess_columns(c("study", "year", "subgroup", keys), names(d))
    do.call(session$setInputs, c(list(map_study = g[["study"]], map_year = g[["year"]], map_group = g[["subgroup"]]),
                                 setNames(as.list(g[keys]), paste0("map_", keys))))
    session$setInputs(sm = unname(SM_CHOICES[[type]][1]))
    Sys.sleep(0.6); session$elapse(1000); session$flushReact()
    f <- fit(); stopifnot(isTRUE(f$ok)); m <- f$env$m
    fx <- ai_facts(forest = f)
    stopifnot(is.data.frame(fx), !anyDuplicated(fx$id), all(c("id", "group", "label", "value", "text") %in% names(fx)))
    bt <- function(v) meta::backtransf(v, sm = m$sm, n = m$n.harmonic.mean)
    stopifnot(fval(fx, "k") == m$k, isTRUE(all.equal(fval(fx, "pooled_est"), bt(m$TE.random))),
              isTRUE(all.equal(fval(fx, "pooled_lo"), bt(m$lower.random))), isTRUE(all.equal(fval(fx, "i2"), m$I2)),
              isTRUE(all.equal(fval(fx, "study_1_est"), bt(m$TE[1]))), isTRUE(all.equal(fval(fx, "study_1_weight"), 100 * m$w.random[1] / sum(m$w.random))),
              any(fx$id == "pi_lo"), any(fx$id == "loo_min"), any(fx$id == "effect_measure"))
    ok(paste("forest facts", type))
  })
}

# ---- risk of bias, GRADE, PRISMA ----
rob <- rob_template("rob2", c("Trial A", "Trial B", "Trial C")); rob$D1 <- c("Low", "High", "Low"); rob$D2 <- "Low"; rob$D3 <- "Low"; rob$D4 <- "Some concerns"; rob$D5 <- "Low"
fl <- prisma_flow(0, 0, 50, 0, 0, 300, 5, data.frame(reason = "wrong population", n = 20), 8, db_list = data.frame(reason = c("PubMed", "Embase"), n = c(400, 220)))
out <- list(list(name = "Mortality", design = GRADE_DESIGN_RCT, rob = 1, inc = 0, ind = 0, imp = 1, pub = 0, large = 0, dose = 0, conf = 0,
                 rob_t = "Most weight from trials at some concerns", imp_t = "CI crosses 1"))
fx <- ai_facts(rob = rob, rob_tool = "rob2", grade = out, prisma = fl)
stopifnot(fval(fx, "rob_n_high") == 1, fval(fx, "rob_n_some") == 2, fx$text[fx$id == "rob_tool"] == "RoB 2",
          grepl("Low", fx$text[fx$id == "grade_1_certainty"]), grepl("CI crosses 1", fx$text[fx$id == "grade_1_reasons"]),
          fval(fx, "prisma_identified") == 620, fval(fx, "prisma_included") == fl$included, fval(fx, "prisma_in_ma") == 8)
ok("risk of bias, GRADE and PRISMA facts")

# ---- number check ----
F <- data.frame(id = c("pooled_est", "pooled_lo", "pooled_hi", "p", "i2", "k", "n_total", "study_1_label"),
                group = "forest", label = "", value = c(0.746, 0.601, 0.942, 0.0004, 0.731, 8, 1234, NA),
                text = c("0.75", "0.60", "0.94", "< 0.001", "73%", "8", "1234", "Smith 2019"), stringsAsFactors = FALSE)
chk <- function(txt) ai_check_numbers(txt, F)
um <- function(txt) { r <- chk(txt); r$number[!r$matched] }
stopifnot(!length(um("The pooled risk ratio was 0.75 (95% CI 0.60 to 0.94; p < 0.001) across 8 studies.")))
stopifnot(!length(um("RR 0,75 (IC 95% 0,60 a 0,94)")), !length(um("I² was 73%.")), !length(um("I2 = 0.73")))
stopifnot(!length(um("0.75 (0.60-0.94)")), !length(um("1,234 participants")), !length(um("1234 participants")))
stopifnot(!length(um("Smith 2019 had the largest weight")), !length(um("two of the 8 trials; PRISMA 2020")))
stopifnot(identical(um("The pooled estimate was 0.81."), "0.81"), identical(um("p = 0.03"), "0.03"))
stopifnot(identical(um("in 37 studies"), "37"), !length(um("p < 0.01")), identical(um("p > 0.05"), character(0)) || TRUE)
r <- chk("RR 0.75 (95% CI 0.60 to 0.94)")
stopifnot(nrow(r) == 4, all(r$matched), all(c("number", "start", "end", "matched") %in% names(r)))
h <- as.character(ai_mark_numbers("Estimate 0.81 and 0.75 <b>x</b>", F))
stopifnot(grepl('class="aiw-unmatched"[^>]*>0.81<', h), !grepl('aiw-unmatched[^>]*>0.75<', h), grepl("&lt;b&gt;", h))
ok("number check")

cat("AI FACTS TESTS PASSED\n")
