#' Clean Names
#' 
#' This function is used internally in get_historic_ratios and is calibrated
#' specifically to take a company name and turn it into something that has
#' meaning in regards to the MacroTrends website url
#'
#' @param x a character vector of names to clean
#'
#' @return a character vector of clean names
#' @export
clean_names <- function(x) {
  x %>%
    stringr::str_replace_all(", Inc.", "") %>%
    stringr::str_replace_all(" Corporation", "") %>%
    stringr::str_replace_all(" Corp.", "") %>%
    stringr::str_replace_all(" Inc.", "") %>%
    stringr::str_replace_all(", L.P.", "") %>%
    stringr::str_replace_all("The ", "") %>%
    stringr::str_replace_all(" Company", "") %>%
    stringr::str_replace_all(" National Association", " na") %>%
    stringr::str_replace_all(" ", "-") %>%
    stringr::str_to_lower()
}