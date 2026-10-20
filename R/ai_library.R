# Session PDF library: the study PDFs a person sends to the AI features, kept for the session only and shared by
# the forest-plot extraction and the risk-of-bias assistant. Each PDF is copied once (an upload input may replace
# its files) and its text read once.

ai_library_server <- function(input, output, session) {
  entries <- reactiveVal(list())
  dir <- tempfile("ponderis_pdfs_"); dir.create(dir)
  session$onSessionEnded(function() unlink(dir, recursive = TRUE))
  n <- 0L

  # files_df: name, size, datapath (as from fileInput); returns the library id of each file, in order
  add <- function(files_df) {
    e <- isolate(entries())
    ids <- character(nrow(files_df))
    for (i in seq_len(nrow(files_df))) {
      size <- file.size(files_df$datapath[i])
      same <- Filter(function(x) x$name == files_df$name[i] && identical(x$size, size), e)
      if (length(same)) { ids[i] <- same[[1]]$id; next }
      n <<- n + 1L
      path <- file.path(dir, sprintf("pdf%03d.pdf", n))
      file.copy(files_df$datapath[i], path)
      pages <- tryCatch(pdftools::pdf_text(path), error = function(err) character(0))
      ids[i] <- paste0("pdf", n)
      e[[ids[i]]] <- list(id = ids[i], name = files_df$name[i], size = size, path = path, pages = pages, study = NA_character_)
    }
    entries(e)
    ids
  }
  set_study <- function(id, study) {
    e <- isolate(entries()); if (is.null(e[[id]])) return(invisible())
    e[[id]]$study <- if (length(study) == 1 && !is.na(study) && nzchar(trimws(study))) trimws(study) else NA_character_
    entries(e)
  }
  get <- function(id) isolate(entries())[[id]]

  list(entries = reactive(unname(entries())), add = add, set_study = set_study, get = get)
}
