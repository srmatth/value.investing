#' prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prediction_ui <- function(id){
  ns <- NS(id)
  shinydashboard::box(
    title = "Model Predictions and Performance to Date",
    width = 12,
    DT::dataTableOutput(outputId = ns("tbl"))
  )
}
    
#' prediction Server Function
#'
#' @noRd 
mod_prediction_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$tbl <- DT::renderDataTable({
    growth <- readr::read_csv("data/industries/autotruckdealerships/gb_growth_preds.csv") %>%
      dplyr::select(ticker, quarter, growth_pred_gb = predict, avg_price) %>%
      dplyr::full_join(
        y = readr::read_csv("data/industries/autotruckdealerships/gb_prob_preds.csv") %>%
          dplyr::mutate(metric = ifelse(metric == "f1", "f1", "98")) %>%
          dplyr::select(prob_gb = yes, ticker, quarter, predict_gb = predict, metric) %>%
          tidyr::pivot_wider(
            names_from = "metric",
            values_from = c("prob_gb", "predict_gb"),
            values_fn = min
          ),
        by = c("ticker", "quarter")
      ) %>%
      dplyr::full_join(
        y = readr::read_csv("data/industries/autotruckdealerships/rf_growth_preds.csv") %>%
          dplyr::select(ticker, quarter, growth_pred_rf = predict),
        by = c("ticker", "quarter")
      ) %>%
      dplyr::full_join(
        y = readr::read_csv("data/industries/autotruckdealerships/rf_binary_preds.csv") %>%
          dplyr::mutate(metric = ifelse(metric == "f1", "f1", "98")) %>%
          dplyr::select(prob_rf = yes, ticker, quarter, predict_rf = predict, metric) %>%
          tidyr::pivot_wider(
            names_from = "metric",
            values_from = c("prob_rf", "predict_rf"),
            values_fn = min
          ),
        by = c("ticker", "quarter")
      ) %>%
      dplyr::left_join(
        y = get_filtered_stocks(ind = paste0("ind_", "autotruckdealerships"), all_data = FALSE) %>%
          dplyr::select(ticker, price),
        by = "ticker"
      ) %>%
      dplyr::mutate(
        change_to_date = (price - avg_price) / avg_price
      ) %>% 
      dplyr::select(-price, -avg_price) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Ticker",
          "Quarter",
          "GB Predicted Growth",
          "GB F1 Prob",
          "GB 98 Prob",
          "GB F1 Pred",
          "GB 98 Pred",
          "RF Predicted Growth",
          "RF F1 Prob",
          "RF 98 Prob",
          "RF F1 Pred",
          "RF 98 Pred",
          "Change to Date"
        )
      ) %>%
      DT::formatPercentage(
        columns = c(
          "prob_rf_f1",
          "prob_rf_98",
          "prob_gb_f1",
          "prob_gb_98",
          "growth_pred_rf",
          "growth_pred_gb",
          "change_to_date"
        ),
        digits = 2
      )
  })
}
    
## To be copied in the UI
# mod_prediction_ui("prediction_ui_1")
    
## To be copied in the server
# callModule(mod_prediction_server, "prediction_ui_1")
 
