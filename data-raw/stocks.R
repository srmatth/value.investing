## code to prepare `stocks` dataset goes here
library(dplyr)
stock_data <- readr::read_csv("/Users/spencer.matthews/Documents/Investing/stock_data.csv")

colnames(stock_data) <- c("date", "close", "volume", "open", "high", "low", "ticker")

stocks <- stock_data %>%
  dplyr::mutate(
    date = lubridate::mdy(date),
    close = as.numeric(stringr::str_replace_all(close, "\\$", "")),
    ## this next line gives NA's, but that is alright because the values
    ## coerced to NA were the literal string "N/A"
    volume = as.numeric(volume),
    open = as.numeric(stringr::str_replace_all(open, "\\$", "")),
    high = as.numeric(stringr::str_replace_all(high, "\\$", "")),
    low = as.numeric(stringr::str_replace_all(low, "\\$", "")),
    day = lubridate::day(date),
    month = lubridate::month(date),
    year = lubridate::year(date),
    day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)
  )


usethis::use_data(stocks, overwrite = TRUE)
