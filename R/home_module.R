# Home page and brand mark.

# The plumb bob: a bar, a line, a diamond (the pooled estimate hanging from the line of no effect).
# bar = colour of the bar and line, dia = colour of the diamond.
ponderis_mark <- function(bar = "#FFFFFF", dia = "#B01E32", class = "pmark") HTML(sprintf(
  '<svg class="%s" viewBox="0 0 64 64" aria-hidden="true" focusable="false"><line x1="18" y1="6" x2="46" y2="6" stroke="%s" stroke-width="3.4" stroke-linecap="round"/><line x1="32" y1="6" x2="32" y2="33" stroke="%s" stroke-width="2.6" stroke-linecap="round"/><polygon points="32,29 46,46 32,63 18,46" fill="%s"/></svg>',
  class, bar, bar, dia))

home_forest_svg <- function() {
  rows <- list(
    list(y = 40, a = 236, b = 330, x = 278, lab = "Trial A 2009", t = "0.66 [0.33; 1.32]"),
    list(y = 62, a = 250, b = 318, x = 274, lab = "Trial B 2011", t = "0.75 [0.45; 1.22]"),
    list(y = 84, a = 262, b = 310, x = 280, lab = "Trial C 2012", t = "0.63 [0.28; 1.40]"),
    list(y = 106, a = 228, b = 316, x = 266, lab = "Trial D 2014", t = "0.83 [0.52; 1.30]"),
    list(y = 128, a = 258, b = 298, x = 276, lab = "Trial E 2015", t = "0.80 [0.42; 1.52]"))
  rw <- vapply(seq_along(rows), function(i) { r <- rows[[i]]
    sprintf('<g class="fr" style="--d:%.2fs"><text x="8" y="%d">%s</text><line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#0F1B27" stroke-width="1.4"/><rect x="%d" y="%d" width="10" height="10" fill="#1E3A5F"/><text x="512" y="%d" text-anchor="end" class="m">%s</text></g>',
      0.55 + i * 0.16, r$y + 4, r$lab, r$a, r$y, r$b, r$y, r$x, r$y - 5, r$y + 4, r$t) }, "")
  HTML(paste0('<svg viewBox="0 0 520 190" width="100%" role="img" aria-label="Example of a forest plot: five studies and a pooled diamond">',
    '<style>text{font:12px "Atkinson Hyperlegible",sans-serif;fill:#0F1B27}.m{fill:#52606D}.b{font-weight:700}</style>',
    '<text x="8" y="16" class="b">Study</text><text x="300" y="16" class="b" text-anchor="middle">Risk ratio</text><text x="512" y="16" class="b" text-anchor="end">RR (95% CI)</text>',
    '<line x1="8" y1="22" x2="512" y2="22" stroke="#0F1B27"/><line class="nl" x1="300" y1="26" x2="300" y2="150" stroke="#0F1B27"/>',
    paste(rw, collapse = ""),
    '<g class="dm"><polygon points="256,152 276,144 296,152 276,160" fill="#B01E32"/><text x="8" y="156" class="b">Random effects</text><text x="512" y="156" class="b" text-anchor="end">0.73 [0.66; 0.79]</text></g>',
    '<text x="276" y="182" text-anchor="middle" class="m">Favours treatment</text><text x="352" y="182" class="m">Favours control</text></svg>'))
}

.home_icons <- list(
  forest = '<svg viewBox="0 0 24 24"><path d="M4 6h16M4 12h16M4 18h16"/><rect x="12" y="4.2" width="3.6" height="3.6" fill="#1E3A5F"/><rect x="8" y="10.2" width="3.6" height="3.6" fill="#1E3A5F"/><rect x="14" y="16.2" width="3.6" height="3.6" fill="#1E3A5F"/></svg>',
  km = '<svg viewBox="0 0 24 24"><path d="M4 4v16h16"/><path d="M5 6h4v3h4v4h3v3h4"/></svg>',
  review = '<svg viewBox="0 0 24 24"><path d="M12 3l7 3v5c0 4.5-3 8-7 10-4-2-7-5.5-7-10V6z"/><path d="M9 12l2 2 4-4"/></svg>',
  conv = '<svg viewBox="0 0 24 24"><rect x="5" y="3" width="14" height="18" rx="2"/><path d="M8 7h8M8 11h2M12 11h2M8 15h2M12 15h2M16 11v4"/></svg>')

home_ui <- function() {
  rv <- function(d = 0) list(`data-rv` = "", style = sprintf("--d:%.2fs", d))
  card <- function(i, mode, icon, title, q, items, go) {
    do.call(tags$button, c(list(type = "button", class = "hcard", `data-go` = mode),
      rv(0.08 * i),
      list(div(class = "ico", HTML(.home_icons[[icon]])), tags$h3(title), p(class = "q", q),
        tags$ul(lapply(items, tags$li)), span(class = "go", go)))) }
  node <- function(i, n, title, text, tag, mode) {
    do.call(tags$button, c(list(type = "button", class = "node", `data-go` = mode), rv(0.09 * i),
      list(div(class = "n", n), tags$b(title), span(text), tags$em(tag)))) }
  trust <- function(i, title, text) do.call(div, c(rv(0.09 * i), list(tags$b(title), span(text))))
  sw <- function(x) tryCatch(as.character(utils::packageVersion(x)), error = function(e) "")
  ver <- paste0("R ", R.version$major, ".", sub("\\..*", "", R.version$minor), " · meta ", sub("^(\\d+\\.\\d+).*", "\\1", sw("meta")),
                " · metafor ", sub("^(\\d+\\.\\d+).*", "\\1", sw("metafor")))

  div(class = "home",
    div(class = "h-hero",
      div(class = "h-copy",
        do.call(div, c(list(class = "eyebrow"), rv(0.05), list("Reproducible meta-analysis"))),
        do.call(tags$h1, c(rv(0.18), list("From paper to manuscript, in one place."))),
        do.call(p, c(list(class = "lede"), rv(0.34), list("Pool studies, rebuild published survival curves, and assess risk of bias and certainty of evidence. Every result comes with a plain-language reading and the R code to reproduce it."))),
        do.call(div, c(list(class = "btns"), rv(0.5), list(
          actionButton("home_example", "Try it with example data", class = "btn-primary"),
          tags$button(type = "button", class = "btn btn-outline", id = "home_open", "Open a saved project"))))),
      do.call(div, c(list(class = "sheet h-sheet"), rv(0.3), list(home_forest_svg(), tags$small("Illustrative example with made-up numbers."))))),

    tags$section(
      do.call(tags$h2, c(rv(0), list("What do you want to do?"))),
      do.call(p, c(list(class = "sub"), rv(0.05), list("Pick by what you have in hand. You can use the areas in any order."))),
      div(class = "hcards",
        card(1, "forest", "forest", "Pool the results of your studies", "You have events and totals, means and SDs, or one hazard ratio per study.",
          c("Publication-ready forest plot", "Heterogeneity, funnel plot and meta-regression", "A plain-language reading of your numbers"), "Open Forest plot →"),
        card(2, "dig", "km", "Rebuild survival curves", "You only have the Kaplan-Meier curves printed in the papers.",
          c("Digitize straight from the figure", "Pooled HR and non-proportional hazards", "Methods and results text written for you"), "Open Survival curves →"),
        card(3, "review", "review", "Document the quality of the review", "You need the flow diagram, risk of bias and the certainty of the evidence.",
          c("PRISMA 2020 flow diagram", "RoB 2, ROBINS-I and NOS", "GRADE with a Summary of Findings table"), "Open Quality & reporting →"),
        card(4, "conv", "conv", "Convert missing data", "The paper gives a median and IQR, a standard error or a CI instead of mean and SD.",
          c("Median and IQR to mean and SD", "SE and CI to SD", "Combine groups"), "Open Calculator →"))),

    tags$section(
      do.call(tags$h2, c(rv(0), list("How the parts fit together"))),
      do.call(p, c(list(class = "sub"), rv(0.05), list("The usual path of a meta-analysis. Each stage opens the part of the app that serves it."))),
      div(class = "hflow",
        node(1, "1", "Search and screening", "Flow diagram of the included studies.", "PRISMA", "review"),
        node(2, "2", "Extraction", "Convert and tidy the data from each study.", "Calculator", "conv"),
        node(3, "3", "Published curves", "Rebuild the patients behind the figure.", "Survival curves", "dig"),
        node(4, "4", "Pooling", "Overall effect, heterogeneity and publication bias.", "Forest plot", "forest"),
        node(5, "5", "Quality", "Risk of bias for each included study.", "Risk of bias", "review"),
        node(6, "6", "Certainty", "Summary of Findings table and text.", "GRADE", "review"))),

    tags$section(
      do.call(tags$h2, c(rv(0), list("Built for the rigour of a paper"))),
      div(class = "htrust",
        trust(1, "Follows the guidelines", "Cochrane Handbook, PRISMA 2020 and GRADE, with the references to cite."),
        trust(2, "Reproducible", "Every analysis writes its R script, so a reviewer can redo it."),
        trust(3, "Text ready to paste", "Methods and Results written from your numbers, for the manuscript."),
        trust(4, "Your data stays with you", "Files live only for the session and are not stored."))),

    do.call(div, c(list(class = "hstart"), rv(0), list(
      div(tags$h2("First time here?"), p("Load the example data and see a forest plot with a reading of the results in under a minute.")),
      actionButton("home_example2", "Try it with example data", class = "btn-light")))),

    tags$footer(class = "hfoot",
      div(tags$b("Ponderis"), tags$br(), "Forest plots are made with the R package meta. Reconstructed curves use IPDfromKM (Guyot et al., 2012); they are approximations and should be described as such."),
      div(class = "hver", ver, tags$br(), "Versions and references are in the Code tab.")))
}
