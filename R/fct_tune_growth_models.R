tune_growth_models <- function(industry, years = 5, verbose = FALSE, nfolds = 10) {
  mod_dat <- growth_model_data(industry, years = years)
  
  ## RANDOM FOREST
  logger::log_info("Tuning Random Forest Models")
  
  tune_spec <- parsnip::rand_forest(
    trees = 200,
    mtry = tune::tune(),
    min_n = tune::tune()
  ) %>%
    parsnip::set_engine("ranger") %>% 
    parsnip::set_mode("regression")
  
  grid <- expand.grid(
    list(
      mtry = c(3, 5, 7, 10),
      min_n = c(1, 3, 5, 7)
    )
  )
  
  folds <- rsample::vfold_cv(mod_dat, v = nfolds)
  
  tree_wf <- workflows::workflow() %>%
    workflows::add_model(tune_spec) %>%
    workflows::add_formula(response ~ .)
  
  tuned <- tree_wf %>%
    tune::tune_grid(
      resamples = folds,
      grid = grid,
      control = tune::control_resamples(verbose = verbose),
      metrics = yardstick::metric_set(mae, rmse, rsq)
    )
  tuned %>%
    tune::collect_metrics() %>%
    readr::write_csv(
      stringr::str_c("data/growth_models/", industry, "/rf_metrics.csv")
    )
  
  ## GRADIENT BOOSTING
  logger::log_info("Tuning Gradient Boosted Models")
  
  tune_spec <- parsnip::boost_tree(
    trees = 500,
    mtry = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = tune::tune()
  ) %>%
    parsnip::set_engine("xgboost") %>% 
    parsnip::set_mode("regression")
  
  grid <- expand.grid(
    list(
      mtry = c(3, 5, 7, 10),
      tree_depth = c(1, 3, 5, 7),
      learn_rate = c(0.1, 0.01, 0.001)
    )
  )
  
  folds <- rsample::vfold_cv(mod_dat, v = nfolds)
  
  tree_wf <- workflows::workflow() %>%
    workflows::add_model(tune_spec) %>%
    workflows::add_formula(response ~ .)
  
  tuned <- tree_wf %>%
    tune::tune_grid(
      resamples = folds,
      grid = grid,
      control = tune::control_resamples(verbose = verbose),
      metrics = yardstick::metric_set(mae, rmse, rsq)
    )
  tuned %>%
    tune::collect_metrics() %>%
    readr::write_csv(
      stringr::str_c("data/growth_models/", industry, "/gb_metrics.csv")
    )
  
}