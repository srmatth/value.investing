get_latest_quarter <- function(tickers) {
  purrr::map_dfr(
    .x = tickers,
    .f = ~{
      prices <- get_historical_prices(.x)
      ratios <- get_historical_ratios(.x) %>%
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
      dividends <- get_historical_dividends(.x) 
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
      
      price_dat <- prices %>%
        dplyr::mutate(
          pct_change = (close - open) / open,
          year = lubridate::year(date),
          quarter = lubridate::quarter(date, with_year = TRUE)
        ) %>%
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
        dplyr::filter(!is.na(pe_ratio)) %>%
        dplyr::left_join(dividends, by = c("ticker", "year")) %>%
        dplyr::left_join(historical_yield, by = c("ticker", "quarter")) %>%
        dplyr::mutate(
          pays_dividend = as.factor(ifelse(is.na(num_per_year), 0, 1)),
          num_per_year = tidyr::replace_na(num_per_year, 0),
          yearly_amount_per_share = tidyr::replace_na(yearly_amount_per_share, 0),
          dividend_yield = yearly_amount_per_share / avg_price,
          three_year_dividend = tidyr::replace_na(three_year_dividend, 0),
          is_profitable = as.factor(ifelse(pe_ratio == 0, 0, 1))
        ) %>%
        dplyr::filter(quarter == max(quarter))
    }
  )
}

get_mod_dat <- function(industry) {
  logger::log_info("Getting the tickers for {industry}")
  path <- stringr::str_c("data/growth_models/", industry)
  tickers <- get_filtered_stocks(
    ind = stringr::str_c("ind_", industry), 
    geo = "geo_any", 
    all_data = FALSE
  ) %>% 
    dplyr::pull(ticker)
  
  logger::log_info("Downloading Price data from Yahoo.com for {industry}")
  if (!fs::dir_exists(path)) fs::dir_create(path)
  download_prices_new(tickers, path)
  
  logger::log_info("Getting Ratio Data for {industry}")
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
  logger::log_info("Saving Ratio Data for {industry}")
  readr::write_csv(
    ratios,
    stringr::str_c(path, "/", industry, "_ratios.csv")
  )
  
  logger::log_info("Gettig Dividend Data for {industry}")
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
  
  logger::log_info("Combining Data for {industry}")
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
      pays_dividend = as.factor(ifelse(is.na(num_per_year), 0, 1)),
      num_per_year = tidyr::replace_na(num_per_year, 0),
      yearly_amount_per_share = tidyr::replace_na(yearly_amount_per_share, 0),
      dividend_yield = yearly_amount_per_share / avg_price,
      three_year_dividend = tidyr::replace_na(three_year_dividend, 0)
    ) %>%
    dplyr::filter(!is.na(pe_ratio))
  
  response <- price_dat %>%
    dplyr::select(ticker, quarter, avg_price, three_year_dividend) %>%
    dplyr::mutate(
      quarter = quarter - 3, 
      avg_future_price_plus_div = avg_price + three_year_dividend
    ) %>%
    dplyr::select(-avg_price, -three_year_dividend)
  
  logger::log_info("Compiling model Data for {industry} DRF growth models")
  price_dat %>%
    dplyr::left_join(response, by = c("ticker", "quarter")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(avg_future_price_plus_div)) %>%
    dplyr::mutate(
      was_profitable = as.factor(ifelse(pe_ratio == 0, 0, 1)),
      three_year_growth = avg_future_price_plus_div / avg_price - 1
    ) %>%
    dplyr::select(
      -ticker, 
      -avg_future_price_plus_div, 
      -quarter, 
      -year,
      -yearly_amount_per_share,
      -three_year_dividend
    ) # %>%
    # center and scale some predictors
    # dplyr::mutate(
    #   avg_vol = (avg_vol - mean(avg_vol)) / sd(avg_vol),
    #   avg_price = (avg_price - mean(avg_price)) / sd(avg_price),
    #   avg_pct_change = (avg_pct_change - mean(avg_pct_change)) / sd(avg_pct_change),
    #   sd_pct_change = (sd_pct_change - mean(sd_pct_change)) / sd(sd_pct_change),
    #   sd_price = (sd_price - mean(sd_price)) / sd(sd_price),
    #   avg_range = (avg_range - mean(avg_range)) / sd(avg_range),
    #   avg_vol = (avg_vol - mean(avg_vol)) / sd(avg_vol),
    #   three_year_growth = log(three_year_growth + 1.1)
    # )
}