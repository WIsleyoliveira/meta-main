# AI panel: where the person gives an API key (or sees that the server has one), agrees that
# files go to Anthropic, picks the model, and sees what the session has used.
# ai_server() returns the object the AI features use: ready(), key(), model(), record_usage(), calls_left(), reason().

AI_CONSENT <- "Files you send to the AI (PDFs, images) are sent to Anthropic for processing. Do not send documents you are not allowed to share."

ai_button_ui <- function() {
  tags$button(id = "ai_open", type = "button", class = "btn action-button ai-btn",
              `aria-label` = "AI features: key, consent and usage",
              span(class = "ai-btn-label", "AI"), uiOutput("ai_badge", inline = TRUE))
}

ai_modal <- function(server_key, model, consent = FALSE) {
  modalDialog(title = "AI features", easyClose = TRUE, size = "m", footer = modalButton("Close"),
    uiOutput("ai_status"),
    if (server_key) div(class = "aside", "This server provides the API key, with a limit of AI calls per session.")
    else tagList(
      passwordInput("ai_key", "Your Anthropic API key", width = "100%", placeholder = "sk-ant-..."),
      div(class = "ai-key-row", actionButton("ai_save_key", "Use this key", class = "btn-primary btn-sm"),
          actionLink("ai_forget_key", "Forget my key")),
      div(class = "aside", "The key stays in this session only: it is not saved, logged or put in any file. Get one at console.anthropic.com.")),
    selectInput("ai_model", "Model", choices = AI_MODELS, selected = model, width = "100%"),
    checkboxInput("ai_consent", AI_CONSENT, value = isTRUE(consent)),
    div(class = "aside", "The AI proposes; you check. Every number it extracts shows the page and the sentence it came from."),
    textOutput("ai_usage")
  )
}

ai_server <- function(input, output, session) {
  server_key <- Sys.getenv("ANTHROPIC_API_KEY")
  has_server_key <- nzchar(server_key)
  max_calls <- if (has_server_key) { m <- suppressWarnings(as.integer(Sys.getenv("PONDERIS_AI_MAX_CALLS", "30"))); if (is.na(m)) 30L else m } else Inf
  user_key <- reactiveVal("")
  usage <- reactiveValues(calls = 0L, input = 0, output = 0, cost = 0)
  missing_pkgs <- ai_available()

  # rebuilt each time it opens, with the choices already made (a fresh unticked box would withdraw consent)
  panel <- function() ai_modal(has_server_key, isolate(model()), isolate(isTRUE(input$ai_consent)))
  observeEvent(input$ai_open, showModal(panel()))
  observeEvent(input$ai_save_key, {
    k <- trimws(input$ai_key %||% "")
    if (nzchar(k)) user_key(k)
    updateTextInput(session, "ai_key", value = "")
  })
  observeEvent(input$ai_forget_key, user_key(""))

  key <- function() if (has_server_key) server_key else isolate(user_key())
  model <- reactive(pick(input$ai_model, unname(AI_MODELS)))
  calls_left <- reactive(max(0, max_calls - usage$calls))
  reason <- reactive({
    if (length(missing_pkgs)) return(paste("The AI features need the R packages", paste(missing_pkgs, collapse = ", "), "(see install_packages.R)."))
    if (!has_server_key && !nzchar(user_key())) return("Add your Anthropic API key in the AI panel (top right) to use the AI.")
    if (!isTRUE(input$ai_consent)) return("Open the AI panel (top right) and agree that files are sent to Anthropic.")
    if (calls_left() <= 0) return("The AI limit for this session was reached.")
    ""
  })
  ready <- reactive(!nzchar(reason()))

  record_usage <- function(u, model_id) {
    usage$calls <- usage$calls + 1L
    if (!is.null(u)) {
      usage$input <- usage$input + u$input_tokens; usage$output <- usage$output + u$output_tokens
      usage$cost <- usage$cost + ai_cost(u, model_id)
    }
    invisible()
  }

  output$ai_usage <- renderText(sprintf("This session: %d AI call%s, %s tokens, about $%.2f (estimate).%s",
    usage$calls, if (usage$calls == 1) "" else "s", format(usage$input + usage$output, big.mark = ","), usage$cost,
    if (is.finite(max_calls)) sprintf(" %d of %d calls left.", calls_left(), max_calls) else ""))
  output$ai_status <- renderUI({
    if (ready()) div(class = "notice info", if (has_server_key) "AI is on (server key)." else "AI is on (your key).")
    else div(class = "notice warn", reason())
  })
  output$ai_badge <- renderUI(span(class = paste("ai-dot", if (ready()) "on" else "off"), `aria-label` = if (ready()) "on" else "off"))
  outputOptions(output, "ai_badge", suspendWhenHidden = FALSE)

  list(ready = ready, key = key, model = model, record_usage = record_usage, calls_left = calls_left, reason = reason, panel = panel)
}
