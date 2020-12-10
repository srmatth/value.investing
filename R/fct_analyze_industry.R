#### analyze_industry ----
analyze_industry <- function(industry) {
  usethis::ui_info("Getting the tickers for {industry}")
  path <- stringr::str_c("data/industries/", industry)
  tickers <- get_filtered_stocks(
    ind = stringr::str_c("ind_", industry), 
    geo = "geo_any", 
    all_data = FALSE
  ) %>% 
    dplyr::pull(ticker)
  
  usethis::ui_info("Downloading Price data from Yahoo.com for {industry}")
  if (!fs::dir_exists(path)) fs::dir_create(path)
  download_prices_new(tickers, path)
  
  usethis::ui_info("Getting Ratio Data for {industry}")
  ratios <- get_historical_ratios(tickers) %>%
    dplyr::mutate(
      quarter = lubridate::quarter(date, with_year = TRUE)
    ) %>% 
    dplyr::select(
      ticker,
      quarter,
      pe_ratio,
      price_to_sales_ratio,
      price_to_book_ratio,
      price_to_fcf_ratio,
      debt_to_equity_ratio
    )
  usethis::ui_info("Saving Ratio Data for {industry}")
  readr::write_csv(
    ratios,
    stringr::str_c(path, "/", industry, "_ratios.csv")
  )
  
  usethis::ui_info("Gettig Dividend Data for {industry}")
  dividends <- get_historical_dividends(tickers) 
  if (nrow(dividends) == 0) {
    dividends <- data.frame(
      ticker = "I'm not a ticker",
      date = "1665-01-01",
      amount_per_share = 0
    )
  }
  dividends <- dividends %>%
    dplyr::mutate(year = lubridate::year(lubridate::ymd(date))) %>%
    dplyr::group_by(ticker, year) %>%
    dplyr::summarize(
      yearly_amount_per_share = sum(amount_per_share),
      num_per_year = dplyr::n()
    )
  
  usethis::ui_info("Combining Data for {industry}")
  price_dat <- get_price_files_new(path) %>%
    dplyr::mutate(
      pct_change = (close - open) / open,
      year = lubridate::year(date),
      quarter = lubridate::quarter(date, with_year = TRUE)
    ) %>%
    dplyr::filter(quarter >= 2005.1) %>%
    dplyr::group_by(
      ticker,
      year,
      quarter
    ) %>%
    dplyr::summarize(
      avg_price = mean(close),
      sd_price = sd(close),
      avg_pct_change = mean(pct_change),
      sd_pct_change = sd(pct_change),
      avg_range = mean(high - low),
      avg_vol = mean(volume)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(ratios, by = c("ticker", "quarter")) %>%
    dplyr::left_join(dividends, by = c("ticker", "year")) %>%
    dplyr::mutate(
      pays_dividend = ifelse(is.na(num_per_year), 0, 1),
      num_per_year = tidyr::replace_na(num_per_year, 0),
      yearly_amount_per_share = tidyr::replace_na(yearly_amount_per_share, 0),
      dividend_yield = yearly_amount_per_share / avg_price
    ) %>%
    dplyr::filter(!is.na(pe_ratio))
  
  usethis::ui_info("Saving Combined Data for {industry}")
  readr::write_csv(
    price_dat, 
    stringr::str_c(path, "/", industry, "_combined_data.csv")
  )
  
  usethis::ui_info("Starting to Create models for {industry}")
  
  random_forest_binary(.data = price_dat, industry = industry)
  
  random_forest_growth(.data = price_dat, industry = industry)
  
  #gradient_boosted_binary(.data = price_dat, industry = industry)
  
  #gradient_boosted_growth(.data = price_dat, industry = industry)
}

#### download_prices ----
download_prices_new <- function(tickers, dir) {
  purrr::map(
    .x = tickers,
    .f = ~{
      usethis::ui_info("Downloading price data for {.x}")
      url <- stringr::str_c(
        "https://query1.finance.yahoo.com/v7/finance/download/",
        .x,
        "?period1=1&period2=",
        round(as.numeric(Sys.time())) - (86400 * 4),
        "&interval=1d&events=history"
      )
      prices <- readr::read_csv(url, col_types = "Ddddddd") %>%
        dplyr::select(-`Adj Close`) %>%
        magrittr::set_colnames(c("date", "open", "high", "low", "close", "volume"))
      readr::write_csv(
        prices,
        stringr::str_c(dir, "/", "historical_prices_", .x, ".csv")
      )
      return(NULL)
    }
  )
}

#### get_price_files ----

get_price_files_new <- function(dir) {
  files <- stringr::str_subset(fs::dir_ls(dir), "historical_prices_")
  purrr::map_dfr(
    .x = files,
    .f = ~{
      ticker <- .x %>%
        fs::path_file() %>%
        stringr::str_remove("historical_prices_") %>%
        stringr::str_remove(".csv")
      data.table::fread(.x) %>%
        dplyr::mutate(ticker = ticker)
    }
  )
}

#### random_forest_binary ----

random_forest_binary <- function(.data, industry) {
  
  path <- stringr::str_c("data/industries/", industry)
  
  response <- .data %>%
    dplyr::select(ticker, quarter, avg_price) %>%
    dplyr::mutate(quarter = quarter - 3, avg_future_price = avg_price) %>%
    dplyr::select(-avg_price)
  
  usethis::ui_info("Compiling model Data for {industry} DRF binary models")
  mod_dat <- .data %>%
    dplyr::left_join(response, by = c("ticker", "quarter")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(avg_future_price)) %>%
    dplyr::mutate(
      three_year_growth = avg_future_price / avg_price - 1,
      over_40_growth = as.factor(dplyr::if_else(three_year_growth > 0.4, "yes", "no"))
    ) %>%
    dplyr::select(
      -ticker, 
      -three_year_growth, 
      -avg_future_price, 
      -quarter, 
      -year,
      -yearly_amount_per_share
    ) %>%
    # center and scale some predictors
    dplyr::mutate(
      avg_vol = (avg_vol - mean(avg_vol)) / sd(avg_vol),
      avg_price = (avg_price - mean(avg_price)) / sd(avg_price),
      avg_pct_change = (avg_pct_change - mean(avg_pct_change)) / sd(avg_pct_change),
      sd_pct_change = (sd_pct_change - mean(sd_pct_change)) / sd(sd_pct_change),
      sd_price = (sd_price - mean(sd_price)) / sd(sd_price),
      avg_range = (avg_range - mean(avg_range)) / sd(avg_range),
      avg_vol = (avg_vol - mean(avg_vol)) / sd(avg_vol)
    )
  
  h2o::h2o.init(enable_assertions = FALSE)
  
  grid <- list(
    ntrees = c(50),
    max_depth = c(3, 7, 10, 15, 20),
    min_rows = c(1),
    mtries = c(7),
    min_split_improvement = c(.0001),
    sample_rate = c(.632),
    nbins = c(4),
    binomial_double_trees = c(TRUE),
    col_sample_rate_per_tree = c(.8),
    balance_classes = c(TRUE),
    seed = 16
  ) %>%
    expand.grid(stringsAsFactors = FALSE)
  
  mod_dat_h <- h2o::as.h2o(mod_dat) %>%
    h2o::h2o.splitFrame()
  
  train <- mod_dat_h[[1]]
  test <- mod_dat_h[[2]]
  
  # initialize the data frames where we will save the results
  results <- data.frame(stringsAsFactors = FALSE)
  # get the counts for each level in each frame
  n_yes_train <- sum(train["over_40_growth"] == "yes")
  n_no_train <- sum(train["over_40_growth"] == "no")
  n_yes_test <- sum(test["over_40_growth"] == "yes")
  n_no_test <- sum(test["over_40_growth"] == "no")
  
  for (i in 1:nrow(grid)) {
    grid_sub <- grid %>% dplyr::slice(i)
    
    tryCatch({
      usethis::ui_info("Starting model {i}")
      tictoc::tic(stringr::str_c("model", i, sep = " "))
      tmp_mod <- h2o::h2o.randomForest(
        y = "over_40_growth",
        training_frame = train,
        nfolds = 5,
        model_id = "temp_forest_mod",
        ntrees = grid_sub$ntrees,
        max_depth = grid_sub$max_depth,
        min_rows = grid_sub$min_rows,
        mtries = grid_sub$mtries,
        min_split_improvement = grid_sub$min_split_improvement,
        sample_rate = grid_sub$sample_rate,
        nbins = grid_sub$nbins,
        binomial_double_trees = grid_sub$binomial_double_trees,
        seed = grid_sub$seed,
        balance_classes = grid_sub$balance_classes,
        col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree
      )
      time <- tictoc::toc()
      
      usethis::ui_info("Model {i} trained")
      
      perf <- h2o::h2o.performance(tmp_mod, test)
      threshold_max_f1 <- h2o::h2o.find_threshold_by_max_metric(perf, "f1")
      
      threshold_98_specificity <- h2o::h2o.metric(perf, metric = "specificity") %>%
        dplyr::filter(specificity > .98) %>%
        dplyr::pull(threshold) %>%
        min()
      
      usethis::ui_info("Model {i} tested")
      
      results_tmp <- data.frame(
        mod_num = i,
        time_to_create = time$toc - time$tic,
        n_yes_train = n_yes_train,
        n_no_train = n_no_train,
        n_yes_test = n_yes_test,
        n_no_test = n_no_test,
        ntrees = grid_sub$ntrees,
        max_depth = grid_sub$max_depth,
        min_rows = grid_sub$min_rows,
        mtries = grid_sub$mtries,
        min_split_improvement = grid_sub$min_split_improvement,
        sample_rate = grid_sub$sample_rate,
        nbins = grid_sub$nbins,
        binomial_double_trees = grid_sub$binomial_double_trees,
        seed = grid_sub$seed,
        balance_classes = grid_sub$balance_classes,
        col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree,
        gini = h2o::h2o.giniCoef(tmp_mod, train = TRUE),
        auc = h2o::h2o.auc(tmp_mod, train = TRUE),
        threshold_max_f1 = threshold_max_f1,
        sensitivity_max_f1 = as.numeric(h2o::h2o.sensitivity(perf, threshold_max_f1)),
        specificity_max_f1 = as.numeric(h2o::h2o.specificity(perf, threshold_max_f1)),
        precision_max_f1 = as.numeric(h2o::h2o.precision(perf, threshold_max_f1)),
        accuracy_max_f1 = as.numeric(h2o::h2o.accuracy(perf, threshold_max_f1)),
        threshold_98_specificity =  threshold_98_specificity,
        sensitivity_98_specificity = as.numeric(h2o::h2o.sensitivity(perf, threshold_98_specificity)),
        specificity_98_specificity = as.numeric(h2o::h2o.specificity(perf, threshold_98_specificity)),
        precision_98_specificity = as.numeric(h2o::h2o.precision(perf, threshold_98_specificity)),
        accuracy_98_specificity = as.numeric(h2o::h2o.accuracy(perf, threshold_98_specificity)),
        stringsAsFactors = FALSE
      )
      
      results <- rbind(results, results_tmp)
      usethis::ui_done("Model {i} finished and data saved")
    },
    error = function(e) {
      usethis::ui_oops("Model {i} failed! {e}")
    })
  }
  
  best_model_98 <- results %>%
    dplyr::arrange(desc(sensitivity_98_specificity)) %>%
    dplyr::slice(1)
  best_model_f1 <- results %>%
    dplyr::arrange(desc(specificity_max_f1)) %>%
    dplyr::slice(1)
  
  mod_dat_h <- h2o::as.h2o(mod_dat)
  
  final_mod_f1 <- h2o::h2o.randomForest(
    y = "over_40_growth",
    training_frame = mod_dat_h,
    model_id = "final_forest_mod_f1",
    nfolds = 5,
    ntrees = best_model_f1$ntrees,
    max_depth = best_model_f1$max_depth,
    min_rows = best_model_f1$min_rows,
    mtries = best_model_f1$mtries,
    min_split_improvement = best_model_f1$min_split_improvement,
    sample_rate = best_model_f1$sample_rate,
    nbins = best_model_f1$nbins,
    binomial_double_trees = best_model_f1$binomial_double_trees,
    seed = best_model_f1$seed,
    balance_classes = best_model_f1$balance_classes,
    col_sample_rate_per_tree = best_model_f1$col_sample_rate_per_tree
  )
  
  final_mod_98 <- h2o::h2o.randomForest(
    y = "over_40_growth",
    training_frame = mod_dat_h,
    model_id = "final_forest_mod_98",
    nfolds = 5,
    ntrees = best_model_98$ntrees,
    max_depth = best_model_98$max_depth,
    min_rows = best_model_98$min_rows,
    mtries = best_model_98$mtries,
    min_split_improvement = best_model_98$min_split_improvement,
    sample_rate = best_model_98$sample_rate,
    nbins = best_model_98$nbins,
    binomial_double_trees = best_model_98$binomial_double_trees,
    seed = best_model_98$seed,
    balance_classes = best_model_98$balance_classes,
    col_sample_rate_per_tree = best_model_98$col_sample_rate_per_tree
  )
  
  f1_thresh <- results %>%
    dplyr::arrange(desc(specificity_max_f1)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(threshold_max_f1)
  
  thresh_98 <- results %>%
    dplyr::arrange(desc(sensitivity_98_specificity)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(threshold_98_specificity)
  
  newdata <- .data %>% 
    dplyr::filter(year == 2020) %>%
    h2o::as.h2o()
  
  preds_f1 <- predict(final_mod_f1, newdata) %>%
    as.data.frame() %>%
    dplyr::mutate(
      predict = dplyr::if_else(yes > f1_thresh, "yes", "no"),
      mod_num = best_model_f1$mod_num,
      metric = "f1"
    ) %>%
    cbind(as.data.frame(newdata))
  
  preds_98 <- predict(final_mod_98, newdata) %>%
    as.data.frame() %>%
    dplyr::mutate(
      predict = dplyr::if_else(yes > thresh_98, "yes", "no"),
      mod_num = best_model_98$mod_num,
      metric = "98% specificity"
    ) %>%
    cbind(as.data.frame(newdata))
  
  all_preds <- rbind(preds_f1, preds_98) %>%
    dplyr::arrange(desc(yes))
  
  h2o::h2o.saveModel(final_mod_f1, path, force = TRUE)
  h2o::h2o.saveModel(final_mod_98, path, force = TRUE)
  
  readr::write_csv(results, paste0(path, "/rf_binary_tuning_results.csv"))
  readr::write_csv(all_preds, paste0(path, "/rf_binary_preds.csv"))
}

#### randm_forest_growth ----

random_forest_growth <- function(.data, industry) {
  
  path <- stringr::str_c("data/industries/", industry)
  
  response <- .data %>%
    dplyr::select(ticker, quarter, avg_price) %>%
    dplyr::mutate(quarter = quarter - 3, avg_future_price = avg_price) %>%
    dplyr::select(-avg_price)
  
  usethis::ui_info("Compiling model Data for {industry} DRF binary models")
  mod_dat <- .data %>%
    dplyr::left_join(response, by = c("ticker", "quarter")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(avg_future_price)) %>%
    dplyr::mutate(three_year_growth = avg_future_price / avg_price - 1) %>%
    dplyr::select(
      -ticker, 
      -avg_future_price, 
      -quarter, 
      -year,
      -yearly_amount_per_share
    ) %>%
    # center and scale some predictors
    dplyr::mutate(
      avg_vol = (avg_vol - mean(avg_vol)) / sd(avg_vol),
      avg_price = (avg_price - mean(avg_price)) / sd(avg_price),
      avg_pct_change = (avg_pct_change - mean(avg_pct_change)) / sd(avg_pct_change),
      sd_pct_change = (sd_pct_change - mean(sd_pct_change)) / sd(sd_pct_change),
      sd_price = (sd_price - mean(sd_price)) / sd(sd_price),
      avg_range = (avg_range - mean(avg_range)) / sd(avg_range),
      avg_vol = (avg_vol - mean(avg_vol)) / sd(avg_vol),
      three_year_growth = log(three_year_growth + 1.1)
    )
  
  h2o::h2o.init(enable_assertions = FALSE)
  
  grid <- list(
    ntrees = c(50),
    max_depth = c(3, 7, 10, 15, 20),
    min_rows = c(1),
    mtries = c(4, 7),
    min_split_improvement = c(.0001),
    sample_rate = c(.632),
    nbins = c(4),
    binomial_double_trees = c(TRUE),
    col_sample_rate_per_tree = c(.8),
    balance_classes = c(TRUE),
    seed = 16
  ) %>%
    expand.grid(stringsAsFactors = FALSE)
  
  mod_dat_h <- h2o::as.h2o(mod_dat) %>%
    h2o::h2o.splitFrame()
  
  train <- mod_dat_h[[1]]
  test <- mod_dat_h[[2]]
  
  # initialize the data frames where we will save the results
  results <- data.frame(stringsAsFactors = FALSE)
  
  for (i in 1:nrow(grid)) {
    grid_sub <- grid %>% dplyr::slice(i)
    
    tryCatch({
      usethis::ui_info("Starting model {i}")
      tictoc::tic(stringr::str_c("model", i, sep = " "))
      tmp_mod <- h2o::h2o.randomForest(
        y = "three_year_growth",
        training_frame = train,
        nfolds = 5,
        model_id = "temp_forest_mod",
        ntrees = grid_sub$ntrees,
        max_depth = grid_sub$max_depth,
        min_rows = grid_sub$min_rows,
        mtries = grid_sub$mtries,
        min_split_improvement = grid_sub$min_split_improvement,
        sample_rate = grid_sub$sample_rate,
        nbins = grid_sub$nbins,
        binomial_double_trees = grid_sub$binomial_double_trees,
        seed = grid_sub$seed,
        balance_classes = grid_sub$balance_classes,
        col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree
      )
      time <- tictoc::toc()
      
      usethis::ui_info("Model {i} trained")
      
      perf <- h2o::h2o.performance(tmp_mod, test)
      
      test_preds <- predict(tmp_mod, test) %>%
        as.data.frame() %>%
        dplyr::mutate(
          predict_unscaled = exp(predict) - 1.1,
          actual_unscaled = test %>%
            as.data.frame() %>%
            dplyr::mutate(three_year_growth = exp(three_year_growth) - 1.1) %>%
            dplyr::pull(three_year_growth)
        )
      usethis::ui_info("Model {i} tested")
      
      results_tmp <- data.frame(
        mod_num = i,
        time_to_create = time$toc - time$tic,
        ntrees = grid_sub$ntrees,
        max_depth = grid_sub$max_depth,
        min_rows = grid_sub$min_rows,
        mtries = grid_sub$mtries,
        min_split_improvement = grid_sub$min_split_improvement,
        sample_rate = grid_sub$sample_rate,
        nbins = grid_sub$nbins,
        binomial_double_trees = grid_sub$binomial_double_trees,
        seed = grid_sub$seed,
        balance_classes = grid_sub$balance_classes,
        col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree,
        r_2 = h2o::h2o.r2(tmp_mod),
        r_2_test = h2o::h2o.r2(perf),
        mae_test = mean(abs(test_preds$actual_unscaled - test_preds$predict_unscaled)),
        mse_test = mean((test_preds$actual_unscaled - test_preds$predict_unscaled)^2),
        stringsAsFactors = FALSE
      )
      
      results <- rbind(results, results_tmp)
      usethis::ui_done("Model {i} finished and data saved")
    },
    error = function(e) {
      usethis::ui_oops("Model {i} failed! {e}")
    })
  }
  
  best_model <- results %>%
    dplyr::arrange(mse_test) %>%
    dplyr::slice(1)
  
  mod_dat_h <- h2o::as.h2o(mod_dat)
  
  final_mod <- h2o::h2o.randomForest(
    y = "three_year_growth",
    training_frame = mod_dat_h,
    model_id = "final_forest_mod_growth",
    nfolds = 5,
    ntrees = best_model$ntrees,
    max_depth = best_model$max_depth,
    min_rows = best_model$min_rows,
    mtries = best_model$mtries,
    min_split_improvement = best_model$min_split_improvement,
    sample_rate = best_model$sample_rate,
    nbins = best_model$nbins,
    binomial_double_trees = best_model$binomial_double_trees,
    seed = best_model$seed,
    balance_classes = best_model$balance_classes,
    col_sample_rate_per_tree = best_model$col_sample_rate_per_tree
  )
  
  newdata <- .data %>% 
    dplyr::filter(year == 2020) %>%
    h2o::as.h2o()
  
  explain_growth <- h2o::h2o.predict_contributions(final_mod, newdata) %>%
    as.data.frame()
  
  preds <- predict(final_mod, newdata) %>%
    as.data.frame() %>%
    dplyr::mutate(
      mod_num = best_model$mod_num,
      predict = exp(predict) - 1.1
    ) %>%
    cbind(as.data.frame(newdata))
  
  preds_98 <- predict(final_mod_98, newdata) %>%
    as.data.frame() %>%
    dplyr::mutate(
      predict = dplyr::if_else(yes > thresh_98, "yes", "no"),
      mod_num = best_model_98$mod_num,
      metric = "98% specificity"
    ) %>%
    cbind(as.data.frame(newdata))
  
  h2o::h2o.saveModel(final_mod, path, force = TRUE)
  
  readr::write_csv(results, paste0(path, "/rf_growth_tuning_results.csv"))
  readr::write_csv(preds, paste0(path, "/rf_growth_preds.csv"))
  readr::write_csv(explain_growth, paste0(path, "/rf_growth_explained.csv"))
}

#### gradient_boosted_binary ----

gradient_boosted_binary <- function(industry) {
  
}

#### gradient_boosted_growth ----

gradient_boosted_growth <- function(industry) {
  
}
