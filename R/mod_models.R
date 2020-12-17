#' models UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_models_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabBox(
    tabPanel(
      title = "RF Growth"
    ),
    tabPanel(
      title = "RF Prob F1"
    ),
    tabPanel(
      title = "RF Prob 98"
    ),
    tabPanel(
      title = "GB Growth"
    ),
    tabPanel(
      title = "GB Prob F1"
    ),
    tabPanel(
      title = "GB Prob 98"
    )
  )
}
    
#' models Server Function
#'
#' @noRd 
mod_models_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_models_ui("models_ui_1")
    
## To be copied in the server
# callModule(mod_models_server, "models_ui_1")
 
