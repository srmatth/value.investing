#' historical UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_historical_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Historical Data"
  )
}
    
#' historical Server Function
#'
#' @noRd 
mod_historical_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_historical_ui("historical_ui_1")
    
## To be copied in the server
# callModule(mod_historical_server, "historical_ui_1")
 
