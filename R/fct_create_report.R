create_report <- function(industry_name) {
  rmarkdown::render(
    input = "inst/templates/report.Rmd",
    output_format = "html_document",
    output_file = industry_name,
    output_dir = "inst/output",
    params = list(industry = industry_name)
  )
}