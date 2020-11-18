#' watch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_watch_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "My Watchlist",
    shinydashboard::box(
      title = "Add to Watchlist",
      width = 4,
      collapsible = FALSE,
      textInput(
        inputId = ns("ticker"),
        label = "Input Ticker",
        placeholder = "eg. AAPL"
      ),
      textAreaInput(
        inputId = ns("notes"),
        label = "Notes/Reasoning",
        placeholder = "I like this stock because ...",
        height = "400px",
        resize = "vertical"
      ),
      actionButton(
        inputId = ns("save"),
        label = "Save to Watchlist"
      )
    ),
    column(
      width = 8,
      shinydashboard::valueBoxOutput(outputId = ns("cards"), width = 12)
    )
  )
}
    
#' watch Server Function
#'
#' @noRd 
mod_watch_server <- function(input, output, session){
  ns <- session$ns
  watchlist <- reactive({
    if (fs::file_exists("data/watchlist.csv"))
    readr::read_csv("data/watchlist.csv")
  })
  output$cards <- renderUI({
    req(watchlist())
    set.seed(16)
    args <- purrr::map2(
      .x = watchlist()$ticker,
      .y = watchlist()$notes,
      .f = ~{
        clickable_value_box(
          value = .x,
          subtitle = paste0(substr(.y, 1, 30), "..."),
          icon = NULL,
          id = ns(.x),
          background = sample(
            c(
              "#00b300",
              "#0066ff",
              "#ff0000",
              "#e68a00",
              "#9933ff"
            ),
            size = 1
          )
        )
      }
    )
    args$cellArgs <- list(style = "
            width: 270px;
            height: auto;
            margin: 1px;
            ")
    do.call(flowLayout, args)
  })
  
  # output$cards <- shinydashboard::renderValueBox({
  #   clickable_value_box(
  #     "AAPL",
  #     subtitle = "A good pick for ther cash and ...",
  #     icon = NULL,
  #     id = ns("AAPL")
  #   )
  # })
  
  observeEvent(input$AAPL, {
    showModal(watchlist_modal("AAPL"))
  })
  
  observeEvent(input$save, {
    if (exists("watchlist")) {
      tmp <- isolate(watchlist()) %>%
        rbind(
          data.frame(
            ticker = input$ticker,
            notes = input$notes
          )
        )
      readr::write_csv(tmp, "data/watchlist.csv")
    } else {
      tmp <- data.frame(
        ticker = input$ticker,
        notes = input$notes
      )
      readr::write_csv(tmp, "data/watchlist.csv")
    }
    updateTextInput(session = session, inputId = "ticker", value = " ")
    updateTextAreaInput(session = session, inputId = "notes", value = " ")
    watchlist <<- reactive({readr::read_csv("data/watchlist.csv")})
    output$cards <- renderUI({
      req(watchlist())
      set.seed(16)
      args <- purrr::map2(
        .x = watchlist()$ticker,
        .y = watchlist()$notes,
        .f = ~{
          clickable_value_box(
            value = .x,
            subtitle = substr(.y, 1, 50),
            icon = NULL,
            id = ns(.x),
            background = sample(
              c(
                "#00b300",
                "#0066ff",
                "#ff0000",
                "#e68a00",
                "#9933ff"
              ),
              size = 1
            )
          )
        }
      )
      do.call(shiny::flowLayout, args)
    })
  })
  
}
    
## To be copied in the UI
# mod_watch_ui("watch_ui_1")
    
## To be copied in the server
# callModule(mod_watch_server, "watch_ui_1")
 
