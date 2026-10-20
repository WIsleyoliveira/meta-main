# Guided empty states and small illustrations (static SVG, drawn here so the page needs no image files).

forest_preview_svg <- function() HTML('<svg viewBox="0 0 520 190" role="img" aria-label="Example of a forest plot: five studies and a pooled diamond">
<style>text{font:12px "Atkinson Hyperlegible",sans-serif;fill:#0F1B27}.m{fill:#52606D}.b{font-weight:700}</style>
<text x="8" y="16" class="b">Study</text><text x="300" y="16" class="b" text-anchor="middle">Risk ratio</text><text x="512" y="16" class="b" text-anchor="end">RR (95% CI)</text>
<line x1="8" y1="22" x2="512" y2="22" stroke="#0F1B27"/>
<line x1="300" y1="26" x2="300" y2="150" stroke="#0F1B27"/>
<g stroke="#0F1B27" stroke-width="1.4">
<line x1="236" y1="40" x2="330" y2="40"/><line x1="250" y1="62" x2="318" y2="62"/><line x1="262" y1="84" x2="310" y2="84"/><line x1="228" y1="106" x2="316" y2="106"/><line x1="258" y1="128" x2="298" y2="128"/></g>
<g fill="#1E3A5F"><rect x="278" y="35" width="10" height="10"/><rect x="274" y="57" width="10" height="10"/><rect x="280" y="79" width="10" height="10"/><rect x="266" y="101" width="10" height="10"/><rect x="276" y="123" width="10" height="10"/></g>
<text x="8" y="44">Trial A 2009</text><text x="8" y="66">Trial B 2011</text><text x="8" y="88">Trial C 2012</text><text x="8" y="110">Trial D 2014</text><text x="8" y="132">Trial E 2015</text>
<g class="m" text-anchor="end"><text x="512" y="44">0.66 [0.33; 1.32]</text><text x="512" y="66">0.75 [0.45; 1.22]</text><text x="512" y="88">0.63 [0.28; 1.40]</text><text x="512" y="110">0.83 [0.52; 1.30]</text><text x="512" y="132">0.80 [0.42; 1.52]</text></g>
<polygon points="256,152 276,144 296,152 276,160" fill="#B01E32"/>
<text x="8" y="156" class="b">Random effects</text><text x="512" y="156" class="b" text-anchor="end">0.73 [0.66; 0.79]</text>
<text x="276" y="182" text-anchor="middle" class="m">Favours treatment</text><text x="352" y="182" class="m">Favours control</text></svg>')

km_preview_svg <- function() HTML('<svg viewBox="0 0 520 210" role="img" aria-label="Example of two pooled Kaplan-Meier curves with confidence bands">
<style>text{font:12px "Atkinson Hyperlegible",sans-serif;fill:#52606D}</style>
<line x1="50" y1="14" x2="50" y2="170" stroke="#0F1B27"/><line x1="50" y1="170" x2="500" y2="170" stroke="#0F1B27"/>
<g stroke="#E1E6EA"><line x1="50" y1="55" x2="500" y2="55"/><line x1="50" y1="95" x2="500" y2="95"/><line x1="50" y1="135" x2="500" y2="135"/></g>
<path d="M50 24 H110 V30 H150 V38 H200 V50 H240 V60 H290 V74 H340 V86 H390 V96 H440 V104 H500" fill="none" stroke="#1E3A5F" stroke-width="2.4"/>
<path d="M50 24 H100 V36 H140 V48 H190 V64 H230 V78 H280 V92 H330 V108 H380 V120 H430 V128 H500" fill="none" stroke="#B01E32" stroke-width="2.4"/>
<path d="M50 24 H110 V30 H150 V38 H200 V50 H240 V60 H290 V74 H340 V86 H390 V96 H440 V104 H500 V122 H440 V114 H390 V106 H340 V94 H290 V82 H240 V68 H200 V57 H150 V45 H110 V35 H50Z" fill="#1E3A5F" opacity=".1"/>
<text x="44" y="28" text-anchor="end">100%</text><text x="44" y="98" text-anchor="end">60%</text><text x="44" y="173" text-anchor="end">20%</text>
<text x="275" y="192" text-anchor="middle">Time (months)</text>
<text x="52" y="206">0</text><text x="170" y="206">12</text><text x="290" y="206">24</text><text x="410" y="206">36</text></svg>')

# title, lead sentence, three steps (title, text), preview, ids of the buttons
guided_empty <- function(title, lead, steps, preview = NULL, button_id, button_label = "Load example data", note = NULL, caption = "What you will get") {
  div(class = "empty guided",
    h2(title), p(class = "lede", lead),
    tags$ol(class = "flow", lapply(steps, function(s) tags$li(tags$b(s[[1]]), tags$span(s[[2]])))),
    div(class = "empty-actions", actionButton(button_id, button_label, class = "btn-primary"),
        if (!is.null(note)) span(class = "aside", note)),
    if (!is.null(preview)) tags$figure(class = "empty-preview", preview, tags$figcaption(caption)))
}

# Zip every file under dir into file. Uses the zip package when it is installed, otherwise the zip program of the system.
zip_dir <- function(dir, file) {
  fl <- list.files(dir, recursive = TRUE)
  old <- setwd(dir); on.exit(setwd(old))
  file <- normalizePath(file, mustWork = FALSE)
  if (requireNamespace("zip", quietly = TRUE)) zip::zip(file, fl, mode = "mirror")
  else if (nzchar(Sys.which("zip"))) utils::zip(file, fl, flags = "-q")
  else stop("Making a zip file needs the R package zip. Install it once with install.packages(\"zip\").")
  invisible(file)
}

# Bar above the result tabs with the button that bundles everything
results_bar <- function(id, hint) div(class = "results-bar", span(class = "aside", hint),
  downloadButton(id, "Download everything (.zip)", class = "btn-primary btn-sm"))
