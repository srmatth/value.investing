#' ind_reports UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ind_reports_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Industry Reports",
    fluidRow(
      column(
        width = 4,
        offset = 4,
        selectInput(
          inputId = ns("industry"),
          label = "Select an Industry",
          width = "100%",
          selected = NULL,
          choices = c(fs::dir_ls("inst/output") %>% fs::path_file() %>% stringr::str_remove_all("\\.html"))
        )
      )
    ),
    htmlOutput(outputId = ns("report"))
  )
}
    
#' ind_reports Server Function
#'
#' @noRd 
mod_ind_reports_server <- function(input, output, session){
  ns <- session$ns
  
  output$report <- renderUI({
    req(input$industry)
    path <- stringr::str_c("inst/output/", input$industry, ".html")
    includeHTML(path)
  })
 
}
    
## To be copied in the UI
# mod_ind_reports_ui("ind_reports_ui_1")
    
## To be copied in the server
# callModule(mod_ind_reports_server, "ind_reports_ui_1")
 
