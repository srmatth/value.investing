#' top_tickers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_top_tickers_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = "Top 5 Companies to Research",
    width = 12,
    uiOutput(outputId = ns("tickers"))
  )
}
    
#' top_tickers Server Function
#'
#' @noRd 
mod_top_tickers_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$tickers <- renderUI({
    HTML(
      stringr::str_c(
        "<ol type = '1'>",
        "<li>AAPL</li>",
        "<li>LUV</li>",
        "<li>VRM</li>",
        "<li>ADBE</li>",
        "<li>T</li>",
        "</ol>"
      )
    )
  })
 
}
    
## To be copied in the UI
# mod_top_tickers_ui("top_tickers_ui_1")
    
## To be copied in the server
# callModule(mod_top_tickers_server, "top_tickers_ui_1")
 
