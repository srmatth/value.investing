devtools::load_all()


abbr <- get_industry_abbreviations()


purrr::map(
  .x = abbr$abbreviation[1],
  .f = analyze_industry
)
