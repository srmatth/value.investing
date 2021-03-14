#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    theme = shinythemes::shinytheme("flatly"),
    titlePanel("Industry Specific Stock Analysis and Prediction"),
    hr(),
    fluidRow(
      column(
        width = 3,
        mod_input_ui("input_ui_1")
      ),
      column(
        width = 9,
        mod_bubble_chart_ui("bubble_chart_ui_1")
      )
    ),
    fluidRow(
      mod_ind_prices_ui("ind_prices_ui_1")
    ),
    fluidRow(
      # Specific data for each ticker (maybe quarter?) including growth to date
      mod_prediction_ui("prediction_ui_1")
    ),
    hr(),
    h3("Model Analysis"),
    hr(),
    fluidRow(
      mod_models_ui("models_ui_1")
    ),
    hr(),
    h3("Model Tuning Results"),
    hr(),
    fluidRow(
      mod_training_ui("training_ui_1")
      # include parallel coordinate plots in this
    ),
    shinyWidgets::useShinydashboard()
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

