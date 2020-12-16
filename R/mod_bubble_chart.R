#' bubble_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bubble_chart_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = "Plot of Growth vs. Probability of Over 40% Growth",
    width = 12,
    plotly::plotlyOutput(outputId = ns("plt"))
  )
}
    
#' bubble_chart Server Function
#'
#' @noRd 
mod_bubble_chart_server <- function(input, output, session, rv){
  ns <- session$ns
 
  output$plt <- plotly::renderPlotly({
    growth <- readr::read_csv("data/industries/autotruckdealerships/gb_growth_preds.csv") %>%
      dplyr::select(growth_pred = predict, ticker, quarter) %>%
      dplyr::left_join(
        y = readr::read_csv("data/industries/autotruckdealerships/gb_prob_preds.csv") %>%
          dplyr::select(prob = yes, ticker, quarter),
        by = c("ticker", "quarter")
      ) %>%
      dplyr::group_by(ticker, quarter) %>%
      dplyr::summarize(prob = mean(prob), growth_pred = mean(growth_pred)) %>%
      dplyr::ungroup() %>%
      plotly::plot_ly(
        x = ~prob,
        y = ~growth_pred,
        text = ~ticker,
        type = "scatter",
        mode = "markers",
        color = ~as.factor(quarter),
        colors = "Blues",
        size = ~growth_pred,
        sizes = c(10, 50),
        marker = list(
          sizemode = "diameter",
          opacity = 0.6
        )
      ) %>%
      plotly::layout(
        xaxis = list(
          title = "Probability of over 40% Growth",
          showgrid = FALSE
        ),
        yaxis = list(
          title = "Predicted Growth",
          showgrid = FALSE
        )
      )
  })
}
    
## To be copied in the UI
# mod_bubble_chart_ui("bubble_chart_ui_1")
    
## To be copied in the server
# callModule(mod_bubble_chart_server, "bubble_chart_ui_1")
 
