#' training UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_training_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Model Training Results"
  )
}
    
#' training Server Function
#'
#' @noRd 
mod_training_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_training_ui("training_ui_1")
    
## To be copied in the server
# callModule(mod_training_server, "training_ui_1")
 
