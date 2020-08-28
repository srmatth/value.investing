get_models <- function(industry) {
  usethis::ui_info("Getting the tickers for {industry}")
  path <- stringr::str_c("/Users/spencer.matthews/Documents/Investing/industries/", industry)
  tickers <- get_filtered_stocks(ind = stringr::str_c("ind_", industry), geo = "geo_any")$ticker
  if (!fs::dir_exists(path)) {
    fs::dir_create(path)
    download_prices(tickers, path)
  }
  
  usethis::ui_info("Getting Ratio Data")
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
  
  usethis::ui_info("Getting Price Data")
  price_dat <- get_price_files(path) %>%
    dplyr::mutate(
      pct_change = (close - open) / open,
      year = lubridate::year(date),
      quarter = lubridate::quarter(date, with_year = TRUE)
    ) %>%
    dplyr::filter(year >= 2005) %>%
    dplyr::group_by(
      ticker,
      quarter
    ) %>%
    dplyr::summarize(
      avg_price = mean(close),
      avg_pct_change = mean(pct_change),
      avg_range = mean(high - low),
      avg_vol = mean(volume)
    ) %>%
    dplyr::left_join(ratios, by = c("ticker", "quarter")) %>%
    dplyr::filter(!is.na(pe_ratio))
  
  response <- price_dat %>%
    dplyr::select(ticker, quarter, avg_price) %>%
    dplyr::mutate(quarter = quarter - 3, avg_future_price = avg_price) %>%
    dplyr::select(-avg_price)
  
  usethis::ui_info("getting model data")
  mod_dat <- price_dat %>%
    dplyr::left_join(response, by = c("ticker", "quarter")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(avg_future_price)) %>%
    dplyr::mutate(three_year_growth = avg_future_price / avg_price - 1,
                  over_40_growth = as.factor(dplyr::if_else(three_year_growth > 0.4, "yes", "no"))) %>%
    dplyr::select(-ticker, -three_year_growth, -avg_future_price, -quarter, -avg_price)
  
  h2o::h2o.init()
  
  grid <- list(
    ntrees = c(25, 50, 100, 150, 200, 300, 500, 1000),
    max_depth = c(3, 7, 10, 15, 20, 25, 30, 50),
    min_rows = c(1),
    mtries = c(-1, 3, 6),
    min_split_improvement = c(.0001),
    sample_rate = c(.632),
    nbins = c(4, 10, 20),
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
  varimp <- data.frame(stringsAsFactors = FALSE)
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
      threshold_max_accuracy <-  h2o::h2o.find_threshold_by_max_metric(perf, "accuracy")
      
      threshold_98_specificity <- h2o::h2o.metric(perf, metric = "specificity") %>%
        dplyr::filter(specificity > .98) %>%
        dplyr::pull(threshold) %>%
        min()
      
      threshold_95_specificity <- h2o::h2o.metric(perf, metric = "specificity") %>%
        dplyr::filter(specificity > .95) %>%
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
        r_2 = h2o::h2o.r2(tmp_mod),
        mse = h2o::h2o.mse(tmp_mod, train = TRUE),
        gini = h2o::h2o.giniCoef(tmp_mod, train = TRUE),
        auc = h2o::h2o.auc(tmp_mod, train = TRUE),
        threshold_max_f1 = threshold_max_f1,
        sensitivity_max_f1 = as.numeric(h2o::h2o.sensitivity(perf, threshold_max_f1)),
        specificity_max_f1 = as.numeric(h2o::h2o.specificity(perf, threshold_max_f1)),
        precision_max_f1 = as.numeric(h2o::h2o.precision(perf, threshold_max_f1)),
        accuracy_max_f1 = as.numeric(h2o::h2o.accuracy(perf, threshold_max_f1)),
        threshold_max_accuracy =  threshold_max_accuracy,
        sensitivity_max_accuracy = as.numeric(h2o::h2o.sensitivity(perf, threshold_max_accuracy)),
        specificity_max_accuracy = as.numeric(h2o::h2o.specificity(perf, threshold_max_accuracy)),
        precision_max_accuracy = as.numeric(h2o::h2o.precision(perf, threshold_max_accuracy)),
        accuracy_max_accuracy = as.numeric(h2o::h2o.accuracy(perf, threshold_max_accuracy)),
        threshold_95_specificity =  threshold_95_specificity,
        sensitivity_95_specificity = as.numeric(h2o::h2o.sensitivity(perf, threshold_95_specificity)),
        specificity_95_specificity = as.numeric(h2o::h2o.specificity(perf, threshold_95_specificity)),
        precision_95_specificity = as.numeric(h2o::h2o.precision(perf, threshold_95_specificity)),
        accuracy_95_specificity = as.numeric(h2o::h2o.accuracy(perf, threshold_95_specificity)),
        threshold_98_specificity =  threshold_98_specificity,
        sensitivity_98_specificity = as.numeric(h2o::h2o.sensitivity(perf, threshold_98_specificity)),
        specificity_98_specificity = as.numeric(h2o::h2o.specificity(perf, threshold_98_specificity)),
        precision_98_specificity = as.numeric(h2o::h2o.precision(perf, threshold_98_specificity)),
        accuracy_98_specificity = as.numeric(h2o::h2o.accuracy(perf, threshold_98_specificity)),
        stringsAsFactors = FALSE
      )
      
      varimp_tmp <- h2o::h2o.varimp(tmp_mod) %>% dplyr::mutate(mod_num = i)
      
      results <- rbind(results, results_tmp)
      varimp <- rbind(varimp, varimp_tmp)
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
  
  newdata <- price_dat %>% 
    dplyr::filter(quarter %in% c(2020.1, 2020.2, 2020.3)) %>%
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
  
  h2o::h2o.saveModel(final_mod_f1, path)
  h2o::h2o.saveModel(final_mod_98, path)
  
  readr::write_csv(results, paste0(path, "/rf_tuning_results.csv"))
  readr::write_csv(varimp, paste0(path, "/rf_varimp.csv"))
  readr::write_csv(all_preds, paste0(path, "/rf_preds.csv"))
  
  list(
    results = results,
    varimp = varimp,
    best_model_98 = best_model_98,
    best_model_f1 = best_model_f1,
    preds = preds
  )
}

get_price_files <- function(dir) {
  files <- stringr::str_subset(fs::dir_ls(dir), "MacroTrends")
  purrr::map_dfr(
    .x = files,
    .f = ~{
      ticker <- .x %>%
        fs::path_file() %>%
        stringr::str_remove("MacroTrends_Data_Download_") %>%
        stringr::str_remove(".csv")
      data.table::fread(.x, skip = 14) %>%
        cbind(ticker)
    }
  )
}

download_prices <- function(tickers, dir) {
  usethis::ui_info("setting up the docker container")
  system("docker stop $(docker ps -aq)")
  system("docker rm $(docker ps -aq)")
  system(
    stringr::str_c(
      "docker run -d -v ",
      dir,
      "://home/seluser/Downloads -p 4446:4444 -p 5901:5900 selenium/standalone-firefox-debug:2.53.1"
    )
  )
  Sys.sleep(10)
  
  usethis::ui_info("initializing the selenium remote driver")
  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = "/home/seluser/Downloads",
                                   browser.download.folderList = 2L,
                                   browser.helperApps.neverAsk.saveToDisk = "text/csv")
  )
  
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost", 
    port = 4446L, 
    browserName = "firefox", 
    extraCapabilities = fprof
  )
  remDr$open()
  
  purrr::map(
    .x = tickers,
    .f = ~{
      usethis::ui_info("Downloading historical daily price data for {.x}")
      tryCatch(
        {
          url <- stringr::str_c(
            "https://www.macrotrends.net/assets/php/stock_price_history.php?t=",
            .x
          )
          remDr$navigate(url)
          data_download <- remDr$findElement(using = 'css',  ".dataExport")
          data_download$clickElement()
        },
        error = function(e) {
          usethis::ui_oops("Prices not found for {.x}, \n\n {e}")
        }
      )
    }
  )
  remDr$quit()
}

test <- get_models("autoparts")
test2 <- get_models(industry = "airportsairservices")

# dir <- "/Users/spencer.matthews/Documents/Investing/data/sector_communication_services/ind_publishing"
# tickers <- get_filtered_stocks(ind = "ind_publishing")$ticker
# 
# download_prices(tickers, dir)
# 
# dir <- "/Users/spencer.matthews/Documents/Investing/data/sector_communication_services/ind_advertising_agencies"
# tickers <- get_filtered_stocks(ind = "ind_advertisingagencies", geo = "geo_any")$ticker
# 
# download_prices(tickers, dir)
# 
# dir <- "/Users/spencer.matthews/Documents/Investing/data/sector_communication_services/ind_broadcasting"
# tickers <- get_filtered_stocks(ind = "ind_broadcasting", geo = "geo_any")$ticker
# 
# download_prices(tickers, dir)
