mod_dat <- growth_model_data("Auto Manufacturers")

lm <- lm(response ~ ., data = mod_dat)
summary(lm)
mean(abs((exp(lm$fitted.values) - 1.1) - (exp(mod_dat$response) - 1.1))) # MAE = 1.036


mod_dat <- growth_model_data("Banks - Diversified")

library(tidymodels)

lin_mod <- linear_reg() %>%
  set_engine("lm")
lm_fit <- lin_mod %>%
  fit(response ~ ., data = mod_dat)
tidy(lm_fit)

rf_mod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")
rf_fit <- rf_mod %>%
  fit(response ~ ., data = mod_dat)

folds <- vfold_cv(mod_dat, v = 10)
rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_formula(response ~ .)

rf_wf_fit <- rf_wf %>%
  fit_resamples(folds)

collect_metrics(rf_wf_fit)

tune_spec <- rand_forest(
  trees = 200,
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("ranger") %>% 
  set_mode("regression")

grid <- expand.grid(
  list(
    mtry = c(2:10),
    min_n = c(1, 3, 5, 7, 9, 11)
  )
)
  
tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(response ~ .)

tuned <- tree_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid,
    control = control_resamples(verbose = TRUE),
    metrics = metric_set(mae, rmse, rsq)
  )
tuned %>%
  collect_metrics() %>% View()

tuned %>%
  collect_metrics() %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

best_mod <- tuned %>%
  select_best("mae")

best_model <- rand_forest(
  trees = 200,
  mtry = best_mod$mtry,
  min_n = best_mod$min_n
) %>%
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

final_wf <- tree_wf %>% 
  update_model(best_model)

splits <- initial_split(mod_dat)

final_model <- final_wf %>%
  last_fit(splits)

preds <- predict(final_model, mod_dat) %>%
  bind_cols(actual = mod_dat$response) %>%
  mutate(
    predict_trans = exp(.pred) - 1.1,
    actual_trans = exp(actual) - 1.1,
    difference = actual_trans - predict_trans
  )

preds$difference %>% abs() %>% mean()
preds$difference %>% `^`(2) %>% mean() %>% sqrt()

plot(density(preds$difference)) # most of the big differences are due to under-prediction. Good.

library(vip)
final_model %>%
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip()


tuned %>%
  collect_metrics() %>%
  mutate(learn_rate = factor(learn_rate)) %>%
  ggplot(aes(tree_depth, mean, color = learn_rate)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_grid(mtry ~ .metric) +
  scale_x_log10(labels = scales::label_number()) +
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0)


#######################################################################


tune_growth_models(industry = "Grocery Stores", verbose = TRUE)
