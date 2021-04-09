#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  rv <- reactiveValues()
  callModule(mod_ind_reports_server, "ind_reports_ui_1")
}
