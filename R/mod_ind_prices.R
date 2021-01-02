#' ind_prices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ind_prices_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = "Historical sum of Prices",
    width = 12,
    plotly::plotlyOutput(outputId = ns("prices"), height = "350px")
  )
}
    
#' ind_prices Server Function
#'
#' @noRd 
mod_ind_prices_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$prices <- plotly::renderPlotly({
    x <- get_price_files_new(stringr::str_c("data/industries/", rv$ind())) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(close = sum(close, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      plotly::plot_ly(
        x = ~lubridate::ymd(date),
        y = ~close,
        type = "scatter",
        mode = "line"
      )
  })
 
}
    
## To be copied in the UI
# mod_ind_prices_ui("ind_prices_ui_1")
    
## To be copied in the server
# callModule(mod_ind_prices_server, "ind_prices_ui_1")
 
