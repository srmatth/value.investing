devtools::load_all()
library(dplyr)
library(tictoc)

abbr <- get_industry_abbreviations()


tic("Finish the Industries")
subset <- abbr$industry_name[1:20]
test <- purrr::map(
  .x = subset,
  .f = tune_growth_models
)
beepr::beep(9)
toc() # 6892.15 seconds for first 8, 2468 seconds for the next 2, 

# Need to go fix banks-regional stuff, errored out at the end of get_historical_ratios I think


tic("Finish the Industries")
subset <- abbr$industry_name[68:100]
test <- purrr::map(
  .x = subset,
  .f = tune_growth_models
)
beepr::beep(9)
toc()

tic("Finish the Industries")
subset <- abbr$industry_name[101:149]
test <- purrr::map(
  .x = subset,
  .f = ~{
    tryCatch({
      tune_growth_models(.x)
    },
    error = function(e) {
      logger::log_error("There was an error: {e}")
    })
  }
)
beepr::beep(9)
toc()

tic("Finish the Industries")
subset <- abbr$industry_name[130:149]
test <- purrr::map(
  .x = subset,
  .f = tune_growth_models
)
beepr::beep(9)
toc()