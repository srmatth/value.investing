get_industry_abbreviations <- function() {
  data.frame(
    industry_name = c(
      "Advertising Agencies",
      "Aerospace & Defense",
      "Agricultural Inputs",
      "Airlines",
      "Airports & Air Services",
      "Aluminum",
      "Apparel Manufacturing"
    ),
    abbreviation = c(
      "advertisingagencies",
      "aerospacedefense",
      "agriculturalinputs",
      "airlines",
      "airportsairservices",
      "aluminum",
      "apparelmanufacturing"
    ),
    stringsAsFactors = FALSE
  )
}