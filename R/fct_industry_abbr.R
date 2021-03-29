get_industry_abbreviations <- function() {
  
  abbreviation <- xml2::read_html("https://finviz.com/screener.ashx?v=111&f=ind_capitalmarkets") %>%
    rvest::html_node("#fs_ind") %>%
    rvest::html_nodes("option") %>%
    rvest::html_attr("value") %>%
    `[`(4:152)
  
  industry_name <- xml2::read_html("https://finviz.com/screener.ashx?v=111&f=ind_capitalmarkets") %>%
    rvest::html_node("#fs_ind") %>%
    rvest::html_nodes("option") %>%
    rvest::html_text() %>%
    `[`(4:152)
  
  data.frame(
    industry_name = industry_name,
    abbreviation = abbreviation,
    stringsAsFactors = FALSE
  )
}