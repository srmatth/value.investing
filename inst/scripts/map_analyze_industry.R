purrr::map(
  .x = c("trucking",
         "steel",
         "banksdiversified"),
  .f = analyze_industry
)
