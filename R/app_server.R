#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  rv <- reactiveValues()
  callModule(mod_input_server, "input_ui_1", rv = rv)
  callModule(mod_top_vals_server, "top_vals_ui_1", rv = rv)
  # callModule(mod_training_server, "training_ui_1")
  # callModule(mod_prediction_server, "prediction_ui_1")
  # callModule(mod_performance_server, "performance_ui_1")
}
