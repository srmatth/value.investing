#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  callModule(mod_historical_server, "historical_ui_1")
  callModule(mod_training_server, "training_ui_1")
  callModule(mod_prediction_server, "prediction_ui_1")
  callModule(mod_performance_server, "performance_ui_1")
  callModule(mod_watch_server, "watch_ui_1")
}
