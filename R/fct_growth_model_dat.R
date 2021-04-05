growth_model_data <- function(industry, years = 5) {
  data <- get_industry_data(industry)
  
  response <- data %>%
    dplyr::select(ticker, quarter, future_price = avg_price) %>%
    dplyr::mutate(quarter = quarter - years)
  
  mod_dat <- data %>%
    dplyr::left_join(response, by = c("ticker", "quarter")) %>%
    dplyr::filter(!is.na(future_price)) %>%
    dplyr::mutate(
      growth = future_price / avg_price - 1,
      response = log(growth + 1.1)
    ) %>%
    dplyr::select(
      -future_price,
      -avg_price,
      -quarter,
      -year,
      -ticker,
      -growth,
      -sd_price,
      -avg_range,
      -yearly_amount_per_share,
      -num_per_year,
      -three_year_dividend
    ) %>%
    tidyr::drop_na()
  
  logger::log_info("Saving Model Data for {industry}")
  path <- stringr::str_c("data/growth_models/", industry)
  readr::write_csv(
    mod_dat, 
    stringr::str_c(path, "/", "mod_data.csv")
  )
  mod_dat
}