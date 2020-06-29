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
get_historical_prices <- function(ticker, years = 10, type = "stocks", verbose = TRUE) {
  if (years > 10) {
    usethis::ui_oops("years must be <= 10, you entered {years}")
    return(NULL)
  }
  if (verbose) usethis::ui_info("Retrieving data for {ticker}")
  tryCatch({
    tmp_data <- data.table::fread(
      stringr::str_c(
        "https://www.nasdaq.com/api/v1/historical/",
        ticker,
        "/",
        type,
        "/",
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
    if (verboe) usethis::ui_done("Data retrieved for {ticker}")
    return(tmp_data)
  },
  error = function(e) {
    usethis::ui_oops("{ticker} was not found on NASDAQ with '{type}' type")
  })
}


#' Get Historical Dividends
#' 
#' A function to get historical dividend information from 
#' dividendinformation.com
#'
#' @param ticker A string identifying the ticker of the stock to get 
#'   information on, all caps
#' @param start_date A string identifying the date on which the information 
#'   should start (ymd format), if NULL, returns all available information
#' @param verbose Logical to determine if the function will output info
#'   as it runs
#'
#' @return a data frame with the following columns
#'   \itemize{
#'     \item{date = the date the dividend was paid}
#'     \item{amount_per_share = the amount per share in dollars}
#'     \item{ticker = the ticker of the stock/fund}
#'   }
#' @export
get_historical_dividends <- function(ticker, start_date = NULL, verbose = TRUE) {
  tryCatch({
    if (verbose) usethis::ui_info("Fetching dividend data for {ticker}")
    page <- xml2::read_html(
      stringr::str_c(
        "https://www.dividendinformation.com/search_ticker/?identifier=",
        ticker
      )
    )
    dividend_data <- page %>%
      rvest::html_nodes("table") %>%
      `[[`(4) %>%
      rvest::html_table() %>%
      janitor::row_to_names(row_number = 1) %>%
      dplyr::select(-Note) %>%
      magrittr::set_colnames(c("date", "amount_per_share")) %>%
      dplyr::mutate(
        ticker = ticker,
        amount_per_share = as.numeric(
          stringr::str_replace_all(
            amount_per_share, 
            "\\$", 
            ""
          )
        ),
        date = lubridate::ymd(date)
      )
    
    if (!is.null(start_date)){
      dividend_data <- dividend_data %>%
        dplyr::filter(date >= lubridate::ymd(start_date))
    }
    if (verbose) usethis::ui_done("Dividend data for {ticker} downloaded")
    return(dividend_data)
  },
  error = function(e) {
    usethis::ui_oops("The ticker {ticker} does not have divident history")
  })
}
