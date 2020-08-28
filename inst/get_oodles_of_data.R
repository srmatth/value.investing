stock_data_nyse <- get_filtered_stocks(exchange = "exch_nyse")

stock_data_nyse <- stock_data_nyse %>%
  dplyr::mutate(
    type = dplyr::if_else(
      industry == "Exchange Traded Fund",
      "etf",
      "stocks"
    )
  )

readr::write_csv(stock_data_nyse, "/Users/spencer.matthews/Documents/Investing/data/stock_data_nyse.csv")

tictoc::tic("Get Historical Prices")
historical_prices <- historical_prices %>%
  rbind(purrr::map2_dfr(stock_data$ticker, stock_data$type, ~{
    get_historical_prices(.x, .y)
  }))
tictoc::toc() # 8858.222 seconds

historical_prices <- historical_prices %>% dplyr::distinct()
readr::write_csv(historical_prices, "/Users/spencer.matthews/Documents/Investing/data/historical_prices.csv")

historical_dividends <- historical_dividends %>%
  rbind(purrr::map_dfr(stock_data$ticker, ~{
    get_historical_dividends(.x)
  }))
readr::write_csv(historical_dividends, "/Users/spencer.matthews/Documents/Investing/data/historical_dividends.csv")


tictoc::tic("get ratios")
historical_ratios <- historical_ratios %>% rbind(get_historical_ratios(stock_data$ticker, stock_data$company))
tictoc::toc() # 11732.518 Seconds

readr::write_csv(historical_ratios, "/Users/spencer.matthews/Documents/Investing/data/historical_ratios.csv")


