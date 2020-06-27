#' Get Nasdaq Historical Stock Prices
#' 
#' Retrieves the historical prices for stocks from Nasdaq.com
#'
#' @param ticker a string, the ticker for the company you wish to see data
#' @param years an integer, how many years back you want the data to go (must
#'   be 10 or less)
#'
#' @return a data.frame containing historical information about the stock
#' @export 
#'
#' @examples
get_nasdaq_historical_prices <- function(ticker, years = 10) {
  if (years > 10) {
    usethis::ui_oops("years must be <= 10, you entered {years}")
    return(NULL)
  }
  usethis::ui_info("Retrieving data for {ticker}")
  tryCatch({
    tmp_data <- data.table::fread(
      stringr::str_c(
        "https://www.nasdaq.com/api/v1/historical/",
        ticker,
        "/stocks/",
        lubridate::date(lubridate::now() - lubridate::years(years)), 
        "/",
        Sys.Date() - 1
      )
    ) %>%
      magrittr::set_colnames(c("date", "close", "volume", "open", "high", "low")) %>%
      dplyr::mutate(
        ticker = ticker,
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
    usethis::ui_done("Data retrieved for {ticker}")
    return(tmp_data)
  },
  error = function(e) {
    usethis::ui_oops("{ticker} was not found on NASDAQ")
  })
}