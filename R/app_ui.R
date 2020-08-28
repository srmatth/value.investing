#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  navbarPage(
    title = "Stock Analysis and Prediction",
    fluid = TRUE,
    theme = shinythemes::shinytheme("flatly"),
    collapsible = TRUE,
      mod_historical_ui("historical_ui_1"),
      mod_training_ui("training_ui_1"),
      mod_prediction_ui("prediction_ui_1"),
      mod_performance_ui("performance_ui_1"),
      mod_watch_ui("watch_ui_1")
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'stockAnalyzer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

