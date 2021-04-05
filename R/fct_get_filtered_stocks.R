#' Get Filtered Stocks
#' 
#' Use FinViz.com to filter stocks and get a list of stocks that
#' meet some certain set of specifications.  Can return a dataframe
#' with lots of data, or just the stocks and their names
#'
#' @param exchange 
#' @param cap 
#' @param fa_div 
#' @param fa_pe 
#' @param fa_peg 
#' @param fa_ps 
#' @param fa_pb 
#' @param fa_pc 
#' @param fa_eps5years 
#' @param fa_sales5years 
#' @param fa_debteq 
#' @param fa_payoutratio 
#' @param all_data 
#'
#' @return
#' @export
get_filtered_stocks <- function(geo = "geo_usa",
                                ind = "ind_any",
                                sec = "sec_any",
                                exchange = "exch_any",
                                cap = "cap_any",
                                fa_div = "any",
                                fa_pe = "any",
                                fa_peg = "any",
                                fa_ps = "any",
                                fa_pb = "any",
                                fa_pc = "any",
                                fa_eps5years = "any",
                                fa_sales5years = "any",
                                fa_debteq = "any",
                                fa_payoutratio = "any",
                                all_data = TRUE) {
  # create the data frame that will be populated
  stock_info <- data.frame(stringsAsFactors = FALSE)
  for (i in seq(1, 7531, by = 20)) {
    tryCatch({
      logger::log_info("Reading entries {i} through {i + 19}")
      page <- stringr::str_c(
        "https://www.finviz.com/screener.ashx?v=152&f=",
        exchange,
        ",",
        cap,
        ",",
        geo,
        ",",
        ind,
        ",",
        sec,
        ",fa_div_", fa_div,
        ",fa_pe_", fa_pe,
        ",fa_peg_", fa_peg,
        ",fa_ps_", fa_ps,
        ",fa_pb_", fa_pb,
        ",fa_pc_", fa_pc,
        ",fa_eps5years_", fa_eps5years,
        ",fa_sales5years_", fa_sales5years,
        ",fa_debteq_", fa_debteq,
        ",fa_payoutratio_", fa_payoutratio,
        "&r=",
        i,
        "&c=",
        dplyr::if_else(
          all_data,
          stringr::str_c(0:70, collapse = ","),
          "1,2,65"
        )
      )
      new_data <- xml2::read_html(page) %>%
        rvest::html_nodes("table") %>%
        `[[`(10) %>%
        rvest::html_table(header = TRUE) %>%
        magrittr::set_colnames(snakecase::to_snake_case(colnames(.)))
      
      # check to see if the data has already been saved, if so jump out of the loop
      if (i == 1) {
        stock_info <- rbind(stock_info, new_data)
      } else if (new_data %>% dplyr::slice(dplyr::n()) %>% dplyr::pull(ticker) == 
          stock_info %>% dplyr::slice(dplyr::n()) %>% dplyr::pull(ticker)) {
        logger::log_success("Finished retrieving data, the filters you set returned {nrow(stock_info)} stocks")
        break 
      } else {
        stock_info <- rbind(stock_info, new_data)
      }
      Sys.sleep(.5)
    },
    error = function(e) {
      logger::log_error("There was an error with this page: {e}")
    })
  }
  if (all_data) {
    logger::log_info("Cleaning data...")
    subset <- stock_info %>% 
      dplyr::select(market_cap:volume) %>%
      purrr::map(string_to_numeric) %>%
      tibble::as_tibble()
    stock_info <- stock_info %>%
      dplyr::select(ticker:country) %>%
      cbind(subset)
    logger::log_success("Done!")
  }
  return(stock_info)
}
