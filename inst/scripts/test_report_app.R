library(shiny)

ui <- fluidPage(
  title = "Stock Analysis Browser Test",
  selectInput(
    inputId = "industry",
    label = "Select an Industry",
    selected = NULL,
    choices = c("", fs::dir_ls("inst/output") %>% fs::path_file() %>% stringr::str_remove_all("\\.html"))
  ),
  htmlOutput(outputId = "report")
)

server <- function(input, output, session) {
  output$report <- renderUI({
    req(input$industry)
    path <- stringr::str_c("inst/output/", input$industry, ".html")
    includeHTML(path)
  })
}

shinyApp(ui, server)
