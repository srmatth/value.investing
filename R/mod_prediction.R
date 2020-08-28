#' prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prediction_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Model Predictions"
  )
}
    
#' prediction Server Function
#'
#' @noRd 
mod_prediction_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_prediction_ui("prediction_ui_1")
    
## To be copied in the server
# callModule(mod_prediction_server, "prediction_ui_1")
 
