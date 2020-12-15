#' Get Historical Prices
#' 
#' Retrieves the historical prices for stocks from Nasdaq.com
#'
#' @param ticker a string, the ticker for the company you wish to see data
#' @param years an integer, how many years back you want the data to go (must
#'   be 10 or less)
#' @param type a string specifying the type of ticker you have
#' @param verbose logical specifying if the function should be chatty
#'   
#' @details Further specifications regarding the `type` parameter.
#' \itemize{
#'   \item{if stock, `type` = "stocks"}
#'   \item{if ETF, `type` = "etf"}
#'   \item{if mutual fund, `type` = "mutualfunds"}
#' }
#'
#' @return a data.frame containing historical information about the stock
#' @export 
get_historical_prices <- function(ticker, type = "stocks", years = 10, verbose = TRUE) {
  if (years > 10) {
    usethis::ui_oops("years must be <= 10, you entered {years}")
    return(NULL)
  }
  if (verbose) usethis::ui_info("Retrieving data for {ticker}")
  tryCatch({
    tmp_data <- xml2::read_html(
      stringr::str_c(
        "https://www.macrotrends.net/stocks/charts/",
        ticker,
        "/",
        "/stock-price-history"
      )
    ) %>% 
      rvest::html_nodes("table") %>% 
      `[[`(1) %>% 
      rvest::html_table() %>%
      janitor::row_to_names(1) %>%
      magrittr::set_colnames(c("year", "avg_price", "open", "high", "low", "close", "pct_change")) %>%
      dplyr::mutate(
        ticker = ticker,
        # date = lubridate::mdy(date),
        # close = as.numeric(stringr::str_replace_all(close, "\\$", "")),
        ## this next line gives NA's, but that is alright because the values
        ## coerced to NA were the literal string "N/A"
        # volume = as.numeric(volume),
        pct_change = as.numeric(stringr::str_replace_all(pct_change, "%", "")) / 100#,
        # high = as.numeric(stringr::str_replace_all(high, "\\$", "")),
        # low = as.numeric(stringr::str_replace_all(low, "\\$", "")),
        # day = lubridate::day(date),
        # month = lubridate::month(date),
        # year = lubridate::year(date),
        # day_of_week = lubridate::wday(date, label = TRUE, abbr = FALSE)
      )
    if (verbose) usethis::ui_done("Data retrieved for {ticker}")
    return(tmp_data)
  },
  error = function(e) {
    usethis::ui_oops("{ticker} was not found on NASDAQ with '{type}' type: {e}")
  })
}