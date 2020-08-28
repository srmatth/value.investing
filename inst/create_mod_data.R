### READ IN THE DATA ----
stock_data <- readr::read_csv("/Users/spencer.matthews/Documents/Investing/data/stock_data.csv")
historical_dividends <- readr::read_csv("/Users/spencer.matthews/Documents/Investing/data/historical_dividends.csv")
historical_prices <- readr::read_csv("/Users/spencer.matthews/Documents/Investing/data/historical_prices.csv")
historical_ratios <- readr::read_csv("/Users/spencer.matthews/Documents/Investing/data/historical_ratios.csv")

### CLEAN THE DATA ----
old_stocks <- historical_ratios %>% 
  dplyr::mutate(date = lubridate::ymd(date)) %>% 
  dplyr::filter(date < lubridate::ymd("2010-01-01")) %>% 
  dplyr::pull(ticker) %>% 
  unique()

dividends <- historical_dividends %>%
  dplyr::filter(ticker %in% old_stocks) %>%
  dplyr::mutate(date = lubridate::ymd(date))

prices <- historical_prices %>%
  dplyr::filter(ticker %in% old_stocks) %>%
  dplyr::mutate(date = lubridate::ymd(date))

ratios <- historical_ratios %>%
  dplyr::filter(ticker %in% old_stocks) %>%
  dplyr::mutate(date = lubridate::ymd(date))

all_data <- ratios %>%
  dplyr::left_join(
    y = dividends,
    by = c("date", "ticker")
  ) %>%
  dplyr::full_join(
    y = prices,
    by = c("date", "ticker")
  ) %>%
  dplyr::filter(date >= lubridate::ymd("2010-01-01"))

rm(dividends, 
   historical_dividends, 
   historical_prices, 
   historical_ratios, 
   prices, 
   ratios, 
   old_stocks)

### CREATE THE MODEL DATA ----

mod_data <- all_data %>%
  dplyr::mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date),
    pct_day_change = (close - open) / abs(open),
    day_range = high - low
  ) %>%
  dplyr::select(
    -name,
    -day,
    -month,
    -day_of_week,
    -stock_price,
    -date,
    -open,
    -high,
    -low,
    -ttm_fcf_per_share,
    -book_value_per_share,
    -ttm_sales_per_share,
    -long_term_debt,
    -shareholder_s_equity,
    -amount_per_share
  ) 

mod_dat_by_year <- mod_data %>%
  dplyr::group_by(ticker, year) %>%
  dplyr::summarize(
    avg_eps = mean(ttm_net_eps, na.rm = TRUE),
    avg_pe = mean(pe_ratio, na.rm = TRUE),
    avg_pb = mean(price_to_book_ratio, na.rm = TRUE),
    avg_ps = mean(price_to_sales_ratio, na.rm = TRUE),
    avg_pcf = mean(price_to_fcf_ratio, na.rm = TRUE),
    avg_de = mean(debt_to_equity_ratio, na.rm = TRUE),
    avg_price = mean(close, na.rm = TRUE),
    avg_vol = mean(volume, na.rm = TRUE),
    sd_vol = sd(volume, na.rm = TRUE),
    avg_pct_change = mean(pct_day_change, na.rm = TRUE),
    avg_day_range = mean(day_range, na.rm = TRUE),
    sd_day_range = sd(day_range, na.rm = TRUE)
  ) %>% 
  dplyr::filter(
    !is.nan(avg_price), 
    !is.nan(avg_eps),
    year %in% 2010:2013) %>%
  tidyr::pivot_wider(
    names_from = "year",
    values_from = c(
      "avg_eps",
      "avg_pe",
      "avg_pb",
      "avg_ps",
      "avg_pcf",
      "avg_de",
      "avg_price",
      "avg_vol",
      "sd_vol",
      "avg_pct_change",
      "avg_day_range",
      "sd_day_range"
    )
  )

mod_dat_by_quarter <- mod_data %>%
  dplyr::group_by(ticker, year, quarter) %>%
  dplyr::summarize(
    avg_eps = mean(ttm_net_eps, na.rm = TRUE),
    avg_pe = mean(pe_ratio, na.rm = TRUE),
    avg_pb = mean(price_to_book_ratio, na.rm = TRUE),
    avg_ps = mean(price_to_sales_ratio, na.rm = TRUE),
    avg_pcf = mean(price_to_fcf_ratio, na.rm = TRUE),
    avg_de = mean(debt_to_equity_ratio, na.rm = TRUE),
    avg_price = mean(close, na.rm = TRUE),
    avg_vol = mean(volume, na.rm = TRUE),
    sd_vol = sd(volume, na.rm = TRUE),
    avg_pct_change = mean(pct_day_change, na.rm = TRUE),
    avg_day_range = mean(day_range, na.rm = TRUE),
    sd_day_range = sd(day_range, na.rm = TRUE)
  ) %>% 
  dplyr::filter(
    !is.nan(avg_price), 
    !is.nan(avg_eps),
    year %in% 2010:2014
  ) %>%
  tidyr::pivot_wider(
    names_from = c("quarter"),
    values_from = c(
      "avg_eps",
      "avg_pe",
      "avg_pb",
      "avg_ps",
      "avg_pcf",
      "avg_de",
      "avg_price",
      "avg_vol",
      "sd_vol",
      "avg_pct_change",
      "avg_day_range",
      "sd_day_range"
    )
  ) %>%
  dplyr::arrange(ticker, year)


response <- mod_data %>%
  dplyr::group_by(ticker, year, quarter) %>%
  dplyr::summarize(
    avg_eps = mean(ttm_net_eps, na.rm = TRUE),
    avg_pe = mean(pe_ratio, na.rm = TRUE),
    avg_pb = mean(price_to_book_ratio, na.rm = TRUE),
    avg_ps = mean(price_to_sales_ratio, na.rm = TRUE),
    avg_pcf = mean(price_to_fcf_ratio, na.rm = TRUE),
    avg_de = mean(debt_to_equity_ratio, na.rm = TRUE),
    avg_price = mean(close, na.rm = TRUE),
    avg_vol = mean(volume, na.rm = TRUE),
    sd_vol = sd(volume, na.rm = TRUE),
    avg_pct_change = mean(pct_day_change, na.rm = TRUE),
    avg_day_range = mean(day_range, na.rm = TRUE),
    sd_day_range = sd(day_range, na.rm = TRUE)
  ) %>% 
  dplyr::filter(
    !is.nan(avg_price), 
    !is.nan(avg_eps),
    (year %in% 2015:2019 & quarter == 4) | (year %in% 2011:2015 & quarter == 1)
  ) %>%
  tidyr::pivot_wider(
    names_from = c("year", "quarter"),
    values_from = c(
      "avg_eps",
      "avg_pe",
      "avg_pb",
      "avg_ps",
      "avg_pcf",
      "avg_de",
      "avg_price",
      "avg_vol",
      "sd_vol",
      "avg_pct_change",
      "avg_day_range",
      "sd_day_range"
    )
  ) %>% 
  dplyr::mutate(
    response_2010 = (avg_price_2015_4 - avg_price_2011_1) / avg_price_2011_1,
    response_2011 = (avg_price_2016_4 - avg_price_2012_1) / avg_price_2012_1,
    response_2012 = (avg_price_2017_4 - avg_price_2013_1) / avg_price_2013_1,
    response_2013 = (avg_price_2018_4 - avg_price_2014_1) / avg_price_2014_1,
    response_2014 = (avg_price_2019_4 - avg_price_2015_1) / avg_price_2015_1
  ) %>%
  dplyr::select(
    ticker, 
    response_2010:response_2014
  ) %>%
  tidyr::pivot_longer(
    cols = paste("response", 2010:2014, sep = "_"),
    names_to = "year",
    names_prefix = "response_",
    values_to = "growth_next_5"
  ) %>%
  dplyr::mutate(year = as.numeric(year))

# mod_dat_final <- mod_dat_by_year %>%
#   dplyr::left_join(
#     y = mod_dat_by_quarter,
#     by = "ticker"
#   ) %>%
#   dplyr::left_join(
#     y = response,
#     by = "ticker"
#   ) %>%
#   dplyr::filter(!is.na(growth))

mod_dat_final <- mod_dat_by_quarter %>%
  dplyr::left_join(
    y = response,
    by = c("ticker", "year")
  ) %>%
  dplyr::filter(!is.na(growth_next_5))

readr::write_csv(mod_dat_final, "/Users/spencer.matthews/Documents/Investing/data/model_data_1_for_5.csv")

# newdata <- mod_data %>%
#   dplyr::filter(year == 2019) %>%
#   dplyr::group_by(ticker, year, quarter) %>%
#   dplyr::summarize(
#     avg_eps = mean(ttm_net_eps, na.rm = TRUE),
#     avg_pe = mean(pe_ratio, na.rm = TRUE),
#     avg_pb = mean(price_to_book_ratio, na.rm = TRUE),
#     avg_ps = mean(price_to_sales_ratio, na.rm = TRUE),
#     avg_pcf = mean(price_to_fcf_ratio, na.rm = TRUE),
#     avg_de = mean(debt_to_equity_ratio, na.rm = TRUE),
#     avg_price = mean(close, na.rm = TRUE),
#     avg_vol = mean(volume, na.rm = TRUE),
#     sd_vol = sd(volume, na.rm = TRUE),
#     avg_pct_change = mean(pct_day_change, na.rm = TRUE),
#     avg_day_range = mean(day_range, na.rm = TRUE),
#     sd_day_range = sd(day_range, na.rm = TRUE)
#   ) %>% 
#   dplyr::filter(
#     !is.nan(avg_price), 
#     !is.nan(avg_eps)
#   ) %>%
#   tidyr::pivot_wider(
#     names_from = c("quarter"),
#     values_from = c(
#       "avg_eps",
#       "avg_pe",
#       "avg_pb",
#       "avg_ps",
#       "avg_pcf",
#       "avg_de",
#       "avg_price",
#       "avg_vol",
#       "sd_vol",
#       "avg_pct_change",
#       "avg_day_range",
#       "sd_day_range"
#     )
#   ) %>%
#   dplyr::arrange(ticker, year)
# 
# readr::write_csv(newdata, "/Users/spencer.matthews/Documents/Investing/models/final_models/rf_1_for_5/newdata_2019.csv")
