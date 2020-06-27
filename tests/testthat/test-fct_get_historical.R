test_that("get_nasdaq_historical_prices works", {
  aapl_data <- get_nasdaq_historical_prices("AAPL")
  aapl_data_short <- get_nasdaq_historical_prices("AAPL", 2)
  expect_equal(class(aapl_data), "data.frame")
  expect_true(nrow(aapl_data) > nrow(aapl_data_short))
})

test_that("get_nasdaq_historical_prices errors correctly", {
  bad_data <- get_nasdaq_historical_prices("A_fake_company")
  expect_null(bad_data)
})

test_that("year_checking works", {
  bad_data <- get_nasdaq_historical_prices("AAPL", 20)
  expect_null(bad_data)
})
