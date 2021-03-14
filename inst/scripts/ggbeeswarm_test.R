library(ggplot2)
library(ggbeeswarm)
explain_growth %>%
  dplyr::select(-BiasTerm) %>%
  tidyr::pivot_longer(
    names_to = "variable",
    values_to = "shap_val",
    cols = colnames(.)
  ) %>%
  ggplot() +
  geom_quasirandom(
    aes(y = variable, x = shap_val, color = shap_val),
    groupOnX = FALSE
  ) + 
  theme_classic() +
  xlab("SHAP Value") +
  ylab("Variable")

dat <- newdata %>% as.data.frame()

ggplot() +
  geom_point(aes(x = dat$avg_range, y = explain_growth$avg_range, color = as.factor(dat$pays_dividend)))
