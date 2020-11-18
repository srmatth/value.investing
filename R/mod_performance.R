#' performance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_performance_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = "Model Prediction Performance",
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
    
#' performance Server Function
#'
#' @noRd 
mod_performance_server <- function(input, output, session){
  ns <- session$ns
  
  curr_prices <- eventReactive(input$update, {
    get_filtered_stocks(ind = paste0("ind_", input$industry), all_data = FALSE) %>%
      dplyr::select(-company)
  })
 
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
      ) %>%
      dplyr::select(
        -pe_ratio,
        -price_to_sales_ratio,
        -price_to_book_ratio,
        -price_to_fcf_ratio,
        -debt_to_equity_ratio,
        -avg_pct_change,
        -avg_range,
        -avg_vol
      ) %>%
      dplyr::left_join(curr_prices(), by = "ticker") %>%
      dplyr::mutate(
        growth = (price - avg_price) / abs(avg_price)
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
          "Current Price",
          "Percent Change"
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
      DT::formatCurrency(columns = c("avg_price", "price")) %>%
      DT::formatPercentage(
        columns = c("yes", "growth"),
        digits = 2
      )
  })
}
    
## To be copied in the UI
# mod_performance_ui("performance_ui_1")
    
## To be copied in the server
# callModule(mod_performance_server, "performance_ui_1")
 
