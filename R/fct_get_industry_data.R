get_industry_data <- function(industry_full) {
  industry_abbr <- get_industry_abbreviations() %>%
    dplyr::filter(industry_name == industry_full) %>%
    dplyr::pull(abbreviation)
  logger::log_info("Getting the tickers for {industry_full}")
  path <- stringr::str_c("data/growth_models/", industry_full)
  tickers <- get_filtered_stocks(
    ind = stringr::str_c("ind_", industry_abbr), 
    geo = "geo_any", 
    all_data = FALSE
  ) %>% 
    dplyr::pull(ticker)
  
  logger::log_info("Downloading Price data from Yahoo.com for {industry_full}")
  if (!fs::dir_exists(path)) fs::dir_create(path)
  download_prices_new(tickers, path)
  
  logger::log_info("Getting Ratio Data for {industry_full}")
  ratios <- get_historical_ratios(tickers) %>%
    dplyr::mutate(
      quarter = lubridate::quarter(date, with_year = TRUE)
    ) %>% 
    dplyr::select(
      ticker,
      quarter,
      pe_ratio,
      price_to_sales_ratio,
      price_to_book_ratio,
      price_to_fcf_ratio,
      debt_to_equity_ratio
    )
  logger::log_info("Saving Ratio Data for {industry_full}")
  readr::write_csv(
    ratios,
    stringr::str_c(path, "/", "ratios.csv")
  )
  
  logger::log_info("Gettig Dividend Data for {industry_full}")
  dividends <- get_historical_dividends(tickers) 
  if (nrow(dividends) == 0) {
    dividends <- data.frame(
      ticker = "I'm not a ticker",
      date = "1665-01-01",
      amount_per_share = 0
    )
  }
  historical_yield <- dividends %>%
    dplyr::mutate(
      quarter = as.numeric(lubridate::quarter(date, with_year = TRUE)),
      three_year_dividend = purrr::map2_dbl(
        .x = ticker,
        .y = quarter,
        .f = ~{
          dividends %>%
            dplyr::filter(
              ticker == .x,
              quarter < .y,
              quarter >= .y - 3
            ) %>%
            dplyr::pull(amount_per_share) %>%
            sum()
        }
      )
    ) %>%
    dplyr::select(-date, -amount_per_share)
  dividends <- dividends %>%
    dplyr::mutate(year = lubridate::year(lubridate::ymd(date))) %>%
    dplyr::group_by(ticker, year) %>%
    dplyr::summarize(
      yearly_amount_per_share = sum(amount_per_share),
      num_per_year = dplyr::n()
    )
  
  logger::log_info("Combining Data for {industry_full}")
  price_dat <- get_price_files_new(path) %>%
    dplyr::mutate(
      pct_change = (close - open) / open,
      year = lubridate::year(date),
      quarter = lubridate::quarter(date, with_year = TRUE)
    ) %>%
    dplyr::filter(quarter >= 2005.1) %>%
    dplyr::group_by(
      ticker,
      year,
      quarter
    ) %>%
    dplyr::summarize(
      avg_price = mean(close),
      sd_price = sd(close),
      avg_pct_change = mean(pct_change),
      sd_pct_change = sd(pct_change),
      avg_range = mean(high - low),
      avg_vol = mean(volume)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ratios, by = c("ticker", "quarter")) %>%
    dplyr::left_join(dividends, by = c("ticker", "year")) %>%
    dplyr::left_join(historical_yield, by = c("ticker", "quarter")) %>%
    dplyr::mutate(
      pays_dividend = ifelse(is.na(num_per_year), 0, 1),
      num_per_year = tidyr::replace_na(num_per_year, 0),
      yearly_amount_per_share = tidyr::replace_na(yearly_amount_per_share, 0),
      dividend_yield = yearly_amount_per_share / avg_price,
      three_year_dividend = tidyr::replace_na(three_year_dividend, 0)
    ) %>%
    dplyr::filter(!is.na(pe_ratio))
  
  logger::log_info("Saving Combined Data for {industry_full}")
  readr::write_csv(
    price_dat, 
    stringr::str_c(path, "/", "combined_data.csv")
  )
  price_dat
}