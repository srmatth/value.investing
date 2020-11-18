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
  tabPanel(
    title = "Model Predictions",
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        width = 2,
        selectInput(
          inputId = ns("mod_type"),
          label = "Select a Type of Model",
          choices = "Random Forest",
          selected = NULL,
          multiple = FALSE
        ),
        selectInput(
          inputId = ns("industry"),
          label = "Select an Industry",
          choices = c("", fs::dir_ls("data/industries") %>% 
            fs::path_file()),
          selected = NULL,
          multiple = FALSE
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("pred"),
          label = "Prediction",
          choices = c("any", "yes", "no"),
          selected = "any"
        ),
        sliderInput(
          inputId = ns("yes"),
          label = "% Yes Greater Than",
          min = 0,
          max = 100,
          step = 5,
          value = 0
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("mod"),
          label = "Model Type",
          choices = c("any", "f1", "spec"),
          selected = "any"
        ),
        textInput(
          inputId = ns("tickers"),
          label = "Tickers",
          placeholder = "ex: AAPL,LUV,ADBE",
          value = ""
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("quarter"),
          label = "Quarter (2020)",
          choices = 1:3,
          selected = 1:3
        ),
        actionButton(
          inputId = ns("update"),
          label = "Update"
        )
      ),
      mainPanel = mainPanel(
        width = 10,
        shinydashboard::box(
          title = "Predictions",
          collapsible = TRUE,
          width = 12,
          DT::dataTableOutput(ns("preds"))
        )
      )
    )
  )
}
    
#' prediction Server Function
#'
#' @noRd 
mod_prediction_server <- function(input, output, session){
  ns <- session$ns
  
  predictions <- eventReactive(input$update, {
    dat <- readr::read_csv(
      stringr::str_c(
        "data/industries/",
        isolate(input$industry),
        "/rf_preds.csv"
      )
    ) %>%
      dplyr::select(
        -no,
        -mod_num
      ) %>%
      dplyr::mutate(
        metric = dplyr::if_else(
          metric == "98% specificity",
          "spec",
          metric
        ),
        avg_vol = stringr::str_c(
          round(avg_vol / 1000000, 2),
          "M"
        )
      )
    if (input$pred != "any") {
      dat <- dat %>%
        dplyr::filter(predict == input$pred)
    }
    if (input$mod != "any") {
      dat <- dat %>%
        dplyr::filter(metric == input$mod)
    }
    if (input$tickers != "") {
      tickers <- stringr::str_split(input$tickers, ",") %>%
        unlist()
      dat <- dat %>%
        dplyr::filter(ticker %in% tickers)
    }
    dat %>%
      dplyr::filter(
        yes > input$yes / 100,
        quarter %in% (2020 + as.numeric(input$quarter) / 10)
      )
  })
  
  output$preds <- DT::renderDataTable({
    predictions() %>%
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Pred",
          "% Yes",
          "Mod Type",
          "Ticker",
          "Quarter",
          "Average Price",
          "Average Percent Change",
          "Average Range",
          "Average Volume",
          "P/E Ratio",
          "P/S Ratio",
          "P/B Ratio",
          "P/CF Ratio",
          "D/Eq Ratio"
        ),
        extensions = "Scroller",
        options = list(
          columnDefs = list(
            list(
              className = "dt-center",
              targets = "_all"
            )
          ),
          dom = "t",
          scroller = TRUE,
          scrollY = 500
        )
      ) %>%
      DT::formatCurrency(columns = c("avg_price", "avg_range")) %>%
      DT::formatPercentage(
        columns = c("yes", "avg_pct_change"),
        digits = 2
      ) %>%
      DT::formatRound(
        columns = c(
          "pe_ratio",
          "price_to_sales_ratio",
          "price_to_book_ratio",
          "price_to_fcf_ratio",
          "debt_to_equity_ratio"
        ),
        digits = 2
      )
  })
 
}
    
## To be copied in the UI
# mod_prediction_ui("prediction_ui_1")
    
## To be copied in the server
# callModule(mod_prediction_server, "prediction_ui_1")
 
