devtools::load_all()

# run_report("Auto Parts")

library(fs)

industries <- path_file(dir_ls("data/growth_models"))

for (i in industries[1:5]) {
  tryCatch({
    logger::log_info("Creating Markdown for {i}")
    suppressMessages(
      suppressWarnings(
        rmarkdown::render(
          input = "inst/templates/report.Rmd",
          output_format = "html_document",
          output_file = i,
          output_dir = "inst/output",
          params = list(industry = i),
          quiet = TRUE
        )
      )
    )
  },
  error = function(e) {
    logger::log_error("Failed to create Markdown for {i}: {e}")
  })
}
