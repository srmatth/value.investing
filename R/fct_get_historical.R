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
get_historical_dividends <- function(tickers, start_date = NULL, verbose = TRUE) {
  purrr::map_dfr(
    .x = tickers,
    .f = ~{
      tryCatch({
        if (verbose) logger::log_info("Fetching dividend data for {.x}")
        page <- xml2::read_html(
          stringr::str_c(
            "https://www.dividendinformation.com/search_ticker/?identifier=",
            .x
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
            ticker = .x,
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
        if (verbose) logger::log_success("Dividend data for {.x} downloaded")
        return(dividend_data)
      },
      error = function(e) {
        logger::log_error("The ticker {.x} does not have dividend history")
        return(NULL)
      })
    })
}

#' Get Historical Ratios
#' 
#' A function to scrape historical ratios from MacroTrends.com
#'
#' @param tickers a vector of tickers representing the companies we want to
#'   see.  They should be all uppercase string values
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
    logger::log_info("Retrieving historical ratios for ({.x})")
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
      logger::log_error("Historical ratios for ({.x}) not found! {e}")
      return(NULL)
    })
  })
}

#' Get Historical Prices
#' 
#' A function to get the historical prices for a character vector of tickers
#'
#' @param tickers the tickers of the stocks that you want to retrieve historical prices for
#'
#' @return a data frame containing historical price information
#' @export
get_historical_prices <- function(tickers) {
  purrr::map_dfr(
    .x = toupper(tickers),
    .f = ~{
      logger::log_info("Downloading price data for {.x}")
      url <- stringr::str_c(
        "https://query1.finance.yahoo.com/v7/finance/download/",
        .x,
        "?period1=1&period2=",
        round(as.numeric(Sys.time())) - (86400 * 4),
        "&interval=1d&events=history"
      )
      readr::read_csv(url, col_types = "Ddddddd") %>%
        dplyr::select(-`Adj Close`) %>%
        magrittr::set_colnames(c("date", "open", "high", "low", "close", "volume")) %>%
        dplyr::mutate(ticker = .x)
    }
  )
}
