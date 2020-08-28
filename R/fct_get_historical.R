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
    usethis::ui_oops("The ticker {ticker} does not have dividend history")
    return(NULL)
  })
}


#' Get Historical Ratios
#' 
#' A function to scrape historical ratios from MacroTrends.com
#'
#' @param tickers a vector of tickers representing the companies we want to
#'   see.  They should be all uppercase string values
#' @param names a vector of names, must be the same length as `tickers`,
#'   they do not need to be clean, will be formatted inside the function
#' @param ratios a character vector identifying the ratios to use.  Can
#'   take on the values "pe-ratio", "price-sales", "price-book", "price-fcf",
#'   "debt-equity-ratio".  Defaults to all of them.  For now it is necessary 
#'   to include the "pe-ratio" in all queries so that the date gets returned
#'
#' @return a data frame detailing historical ratio data for each of the stocks
#'   passed to the function via `tickers` and `names`
#' @export
get_historical_ratios <- function(tickers, ratios = c("pe-ratio",
                                                      "price-sales",
                                                      "price-book",
                                                      "price-fcf",
                                                      "debt-equity-ratio")) {
  purrr::map_dfr(.x = tickers, ~{
    usethis::ui_info("Retrieving historical ratios for ({.x})")
    tryCatch({
      dat <- data.frame(stringsAsFactors = FALSE)
      for (i in ratios) {
        tmp_dat <- stringr::str_c(
          "https://www.macrotrends.net/stocks/charts/",
          .x,
          "/",
          "/",
          i
        ) %>%
          xml2::read_html() %>%
          rvest::html_nodes("table") %>%
          `[[`(1) %>%
          rvest::html_table() %>%
          janitor::row_to_names(1) %>%
          magrittr::set_colnames(snakecase::to_snake_case(colnames(.)))
        if (i %in% c("price-sales", "price-book", "price-fcf")) {
          tmp_dat <- tmp_dat %>%
            dplyr::select(-stock_price)
        }
        if (i == ratios[1]) {
          dat <- tmp_dat
        } else {
          dat <- dplyr::full_join(
            x = dat,
            y = tmp_dat,
            by = "date"
          )
        }
      }
      dat %>%
        dplyr::select(date) %>%
        cbind(
          dat %>%
            dplyr::select(-date) %>%
            purrr::map(string_to_numeric) %>%
            tibble::as_tibble() 
        ) %>%
        dplyr::mutate(ticker = .x)
    },
    error = function(e) {
      usethis::ui_oops("Historical ratios for ({.x}) not found! {e}")
      return(NULL)
    })
  })
}

get_historical <- function(ticker, type) {
  if (type == "Price") {
    get_historical_prices(ticker = ticker)
  }
  else if (type == "Dividends") {
    get_historical_dividends(ticker = ticker)
  }
  else if (type == "P/E Ratio") {
    get_historical_ratios(tickers = ticker, ratios = "pe-ratio")
  }
  else if (type == "P/B Ratio") {
    get_historical_ratios(tickers = ticker, ratios = "price-book")
  }
  else if (type == "P/S Ratio") {
    get_historical_ratios(tickers = ticker, ratios = "price-sales")
  }
  else if (type == "Debt/Equity Ratio") {
    get_historical_ratios(tickers = ticker, ratios = "debt-equity-ratio")
  }
  else if (type == "Price to Free Cash Flow") {
    get_historical_ratios(tickers = ticker, ratios = "price-fcf")
  }
}

