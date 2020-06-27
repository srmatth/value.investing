#' main_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        
      ),
      mainPanel = mainPanel(
        
      )
    )
  )
}
    
#' main_page Server Function
#'
#' @noRd 
mod_main_page_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_main_page_ui("main_page_ui_1")
    
## To be copied in the server
# callModule(mod_main_page_server, "main_page_ui_1")
 
