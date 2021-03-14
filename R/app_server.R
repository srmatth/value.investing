#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  rv <- reactiveValues()
  callModule(mod_input_server, "input_ui_1", rv = rv)
  callModule(mod_bubble_chart_server, "bubble_chart_ui_1", rv = rv)
  callModule(mod_ind_prices_server, "ind_prices_ui_1", rv = rv)
  callModule(mod_training_server, "training_ui_1", rv = rv)
  callModule(mod_prediction_server, "prediction_ui_1", rv = rv)
  callModule(mod_models_server, "models_ui_1", rv = rv)
}
