#' input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = NULL,
    width = 12,
    column(
      width = 12,
      selectInput(
        inputId = ns("ind"),
        label = "Select an Industry",
        choices = c("", fs::dir_ls("data/industries") %>% fs::path_file()),
        selected = NULL,
        multiple = FALSE
      )
    ),
    br(),
    column(
      width = 8, 
      offset = 2,
      actionButton(
        inputId = ns("update"),
        label = "View Report",
        width = "100%"
      )
    )
  )
}
    
#' input Server Function
#'
#' @noRd 
mod_input_server <- function(input, output, session, rv){
  ns <- session$ns
 
  ## Add to the reactive values list
  rv$ind <- eventReactive(input$update, {input$ind})
}
    
## To be copied in the UI
# mod_input_ui("input_ui_1")
    
## To be copied in the server
# callModule(mod_input_server, "input_ui_1")
 
