#' top_vals UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_top_vals_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = NULL,
    width = 12,
    # shinydashboard::valueBoxOutput(outputId = ns("ind")),
    column(
      width = 5, offset = 1,
      shinydashboard::valueBoxOutput(outputId = ns("prob"), width = 12)
    ),
    column(
      width = 5,
      shinydashboard::valueBoxOutput(outputId = ns("grow"), width = 12)
    )
  )
}
    
#' top_vals Server Function
#'
#' @noRd 
mod_top_vals_server <- function(input, output, session, rv = rv){
  ns <- session$ns
 
  # output$ind <- shinydashboard::renderValueBox({
  #   shinydashboard::valueBox(
  #     value = "FoodDistribution",
  #     subtitle = "Industry Analysis",
  #     icon = icon("industry"),
  #     color = "olive"
  #   )
  # })
  output$prob <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = 5,
      subtitle = "Probability of Above 40% Industry Growth",
      icon = icon("percent"),
      color = "olive"
    )
  })
  output$grow <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = 5,
      subtitle = "Average Predicted Growth",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
}
    
## To be copied in the UI
# mod_top_vals_ui("top_vals_ui_1")
    
## To be copied in the server
# callModule(mod_top_vals_server, "top_vals_ui_1")
 
