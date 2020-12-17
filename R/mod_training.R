#' training UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_training_ui <- function(id){
  ns <- NS(id)
  shinydashboard::tabBox(
    width = 12,
    tabPanel(
      title = "RF Growth",
      DT::dataTableOutput(outputId = ns("rfg"))
    ),
    tabPanel(
      title = "RF Prob",
      DT::dataTableOutput(outputId = ns("rfp"))
    ),
    tabPanel(
      title = "GB Growth",
      DT::dataTableOutput(outputId = ns("gbg"))
    ),
    tabPanel(
      title = "GB Prob",
      DT::dataTableOutput(outputId = ns("gbp"))
    )
  )
}
    
#' training Server Function
#'
#' @noRd 
mod_training_server <- function(input, output, session, rv){
  ns <- session$ns
  
  output$rfg <- DT::renderDataTable({
    readr::read_csv("data/industries/autotruckdealerships/rf_growth_tuning_results.csv") %>%
      dplyr::select(
        mod_num,
        r_2_test,
        mse_test,
        mae_test,
        ntrees:col_sample_rate_per_tree,
        time_to_create
      ) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Model",
          "R^2",
          "MSE",
          "MAE",
          "Ntrees",
          "Max Depth",
          "Min Rows",
          "Mtries",
          "Min Split",
          "Sample Rate",
          "Nbins",
          "Col Sample Rate",
          "Time"
        )
      ) %>%
      DT::formatRound(
        columns = c(
          "r_2_test",
          "mse_test",
          "mae_test",
          "time_to_create"
        ),
        digits = 4
      )
  })
  output$rfp <- DT::renderDataTable({
    readr::read_csv("data/industries/autotruckdealerships/rf_binary_tuning_results.csv") %>%
      dplyr::select(
        mod_num,
        auc,
        specificity_max_f1,
        accuracy_max_f1,
        sensitivity_98_specificity,
        accuracy_98_specificity,
        ntrees:nbins,
        time_to_create
      ) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Model",
          "AUC",
          "Spc max F1",
          "Acc max F1",
          "Sns 98 Spc",
          "Acc 98 Spc",
          "Ntrees",
          "Max Depth",
          "Min Rows",
          "Mtries",
          "Min Split",
          "Sample Rate",
          "Nbins",
          "Time"
        )
      ) %>%
      DT::formatPercentage(
        columns = c(
          "specificity_max_f1",
          "accuracy_max_f1",
          "sensitivity_98_specificity",
          "accuracy_98_specificity"
        ),
        digits = 2
      ) %>%
      DT::formatRound(
        columns = c("auc", "time_to_create"),
        digits = 4
      )
  })
  output$gbg <- DT::renderDataTable({
    readr::read_csv("data/industries/autotruckdealerships/gb_growth_tuning_results.csv") %>%
      dplyr::select(
        mod_num,
        r_2_test,
        mse_test,
        mae_test,
        ntrees:learn_rate_annealing,
        time_to_create
      ) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Model",
          "R^2",
          "MSE",
          "MAE",
          "Ntrees",
          "Max Depth",
          "Min Rows",
          "Distribution",
          "Learn Rate",
          "Learn Rate Annealing",
          "Time"
        )
      ) %>%
        DT::formatRound(
          columns = c(
            "r_2_test",
            "mse_test",
            "mae_test",
            "time_to_create"
          ),
          digits = 2
        )
  })
  output$gbp <- DT::renderDataTable({
    readr::read_csv("data/industries/autotruckdealerships/gb_prob_tuning_results.csv") %>%
      dplyr::select(
        mod_num,
        auc,
        specificity_max_f1,
        accuracy_max_f1,
        sensitivity_98_specificity,
        accuracy_98_specificity,
        ntrees:learn_rate_annealing,
        time_to_create
      ) %>%
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Model",
          "AUC",
          "Spc max F1",
          "Acc max F1",
          "Sns 98 Spc",
          "Acc 98 Spc",
          "Ntrees",
          "Max Depth",
          "Min Rows",
          "Distribution",
          "Learn Rate",
          "Learn Rate Annealing",
          "Time"
        )
      ) %>%
      DT::formatPercentage(
        columns = c(
          "specificity_max_f1",
          "accuracy_max_f1",
          "sensitivity_98_specificity",
          "accuracy_98_specificity"
        ),
        digits = 2
      ) %>%
      DT::formatRound(
        columns = c("auc", "time_to_create"),
        digits = 4
      )
  })
 
}
    
## To be copied in the UI
# mod_training_ui("training_ui_1")
    
## To be copied in the server
# callModule(mod_training_server, "training_ui_1")
 
