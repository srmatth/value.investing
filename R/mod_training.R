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
      plotly::plotlyOutput(outputId = ns("rfgp")),
      DT::dataTableOutput(outputId = ns("rfg"))
    ),
    tabPanel(
      title = "RF Prob",
      plotly::plotlyOutput(outputId = ns("rfpp")),
      DT::dataTableOutput(outputId = ns("rfp"))
    ),
    tabPanel(
      title = "GB Growth",
      plotly::plotlyOutput(outputId = ns("gbgp")),
      DT::dataTableOutput(outputId = ns("gbg"))
    ),
    tabPanel(
      title = "GB Prob",
      plotly::plotlyOutput(outputId = ns("gbpp")),
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
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/rf_growth_tuning_results.csv")
    ) %>%
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
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/rf_prob_tuning_results.csv")
    ) %>%
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
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/gb_growth_tuning_results.csv")
    ) %>%
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
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/gb_prob_tuning_results.csv")
    ) %>%
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
  
  output$rfgp <- plotly::renderPlotly({
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/rf_growth_tuning_results.csv")
    ) %>%
      plotly::plot_ly(
        type = "parcoords",
        line = list(color = ~mod_num,
                    colorscale = 'Jet',
                    showscale = TRUE,
                    reversescale = TRUE,
                    cmin = 0,
                    cmax = 80),
        dimensions = list(
          list(
            range = c(50, 100),
            label = "ntrees",
            values = ~ntrees
          ),
          list(
            range = c(4, 7),
            label = "mtries",
            values = ~mtries
          ),
          list(
            range = c(4, 20),
            label = "Nbins",
            values = ~nbins
          ),
          list(
            range = c(3, 20),
            label = "max_depth",
            values = ~max_depth
          ),
          list(
            range = c(1e-04, 1e-03),
            label = "min_split_improvement",
            values = ~min_split_improvement
          ),
          list(
            range = c(min(.$mae_test), max(.$mae_test)),
            label = "MAE",
            values = ~mae_test
          ),
          list(
            range = c(min(.$mse_test), max(.$mse_test)),
            label = "MSE",
            values = ~mse_test
          )
        )
      )
  })
  output$rfpp <- plotly::renderPlotly({
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/rf_binary_tuning_results.csv")
    ) %>%
      plotly::plot_ly(
        type = "parcoords",
        line = list(color = ~mod_num,
                    colorscale = 'Jet',
                    showscale = TRUE,
                    reversescale = TRUE,
                    cmin = 0,
                    cmax = 80),
        dimensions = list(
          list(
            range = c(50, 100),
            label = "ntrees",
            values = ~ntrees
          ),
          list(
            range = c(4, 7),
            label = "mtries",
            values = ~mtries
          ),
          list(
            range = c(4, 20),
            label = "Nbins",
            values = ~nbins
          ),
          list(
            range = c(3, 20),
            label = "max_depth",
            values = ~max_depth
          ),
          list(
            range = c(1e-04, 1e-03),
            label = "min_split_improvement",
            values = ~min_split_improvement
          ),
          list(
            range = c(min(.$auc), max(.$auc)),
            label = "AUC",
            values = ~auc
          ),
          list(
            range = c(min(.$specificity_max_f1), max(.$specificity_max_f1)),
            label = "Spc max F1",
            values = ~specificity_max_f1
          ),
          list(
            range = c(min(.$sensitivity_98_specificity), max(.$sensitivity_98_specificity)),
            label = "Sns 98 Spc",
            values = ~sensitivity_98_specificity
          )
        )
      )
  })
  output$gbgp <- plotly::renderPlotly({
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/gb_growth_tuning_results.csv")
    ) %>%
      plotly::plot_ly(
        type = "parcoords",
        line = list(color = ~mod_num,
                    colorscale = 'Jet',
                    showscale = TRUE,
                    reversescale = TRUE,
                    cmin = 0,
                    cmax = 72),
        dimensions = list(
          list(
            range = c(100, 300),
            label = "ntrees",
            values = ~ntrees
          ),
          list(
            range = c(.001, .05),
            label = "Learn Rate",
            values = ~learn_rate
          ),
          list(
            range = c(0.5, 0.99),
            label = "LR Annealing",
            values = ~learn_rate_annealing
          ),
          list(
            range = c(1, 10),
            label = "max_depth",
            values = ~max_depth
          ),
          list(
            range = c(min(.$mae_test), max(.$mae_test)),
            label = "MAE",
            values = ~mae_test
          ),
          list(
            range = c(min(.$mse_test), max(.$mse_test)),
            label = "MSE",
            values = ~mse_test
          )
        )
      )
  })
  output$gbpp <- plotly::renderPlotly({
    readr::read_csv(
      stringr::str_c("data/industries/", rv$ind(), "/gb_prob_tuning_results.csv")
    ) %>%
      plotly::plot_ly(
        type = "parcoords",
        line = list(color = ~mod_num,
                    colorscale = 'Jet',
                    showscale = TRUE,
                    reversescale = TRUE,
                    cmin = 0,
                    cmax = 72),
        dimensions = list(
          list(
            range = c(100, 300),
            label = "ntrees",
            values = ~ntrees
          ),
          list(
            range = c(.001, .05),
            label = "Learn Rate",
            values = ~learn_rate
          ),
          list(
            range = c(0.5, 0.99),
            label = "LR Annealing",
            values = ~learn_rate_annealing
          ),
          list(
            range = c(1, 10),
            label = "max_depth",
            values = ~max_depth
          ),
          list(
            range = c(min(.$auc), max(.$auc)),
            label = "AUC",
            values = ~auc
          ),
          list(
            range = c(min(.$specificity_max_f1), max(.$specificity_max_f1)),
            label = "Spc max F1",
            values = ~specificity_max_f1
          ),
          list(
            range = c(min(.$sensitivity_98_specificity), max(.$sensitivity_98_specificity)),
            label = "Sns 98 Spc",
            values = ~sensitivity_98_specificity
          )
        )
      )
  })
 
}
    
## To be copied in the UI
# mod_training_ui("training_ui_1")
    
## To be copied in the server
# callModule(mod_training_server, "training_ui_1")
 
