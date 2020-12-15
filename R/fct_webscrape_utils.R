#' String to Dollar
#' 
#' Converts a numeric string (such as '53.5B') to its corresponding 
#' numeric value (eg. 53500000000)
#'
#' @param x a vector of character strings that represent abbreviated
#'   dollar amounts
#'
#' @return a double vector of the same length as the input vector
#' @export
string_to_numeric <- function(x) {
  purrr::map_dbl(.x = x, .f = ~{
    if (is.na(.x)) return(as.numeric(NA))
    .x <- stringr::str_replace_all(.x, "\\$", "")
    .x <- stringr::str_replace_all(.x, ",", "")
    if (stringr::str_detect(.x, stringr::regex("M", ignore_case = TRUE))) {
      y <- stringr::str_replace(.x, stringr::regex("M", ignore_case = TRUE), "")
      y <- as.numeric(y) * 1000000
    } else if (stringr::str_detect(.x, stringr::regex("B", ignore_case = TRUE))) {
      y <- stringr::str_replace(.x, stringr::regex("B", ignore_case = TRUE), "")
      y <- as.numeric(y) * 1000000000
    } else if (stringr::str_detect(.x, stringr::regex("K", ignore_case = TRUE))) {
      y <- stringr::str_replace(.x, stringr::regex("K", ignore_case = TRUE), "")
      y <- as.numeric(y) * 1000
    } else if (stringr::str_detect(.x, "%")) {
      y <- stringr::str_replace(.x, "%", "")
      y <- as.numeric(y) * .01
    } else if (.x == "-") {
      y <- as.numeric(NA)
    } else {
      y <- as.numeric(.x)
    }
    return(y)
  })
}