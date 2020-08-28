#' performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_performance_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Model Prediction Performance"
  )
}
    
#' performance Server Function
#'
#' @noRd 
mod_performance_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_performance_ui("performance_ui_1")
    
## To be copied in the server
# callModule(mod_performance_server, "performance_ui_1")
 
