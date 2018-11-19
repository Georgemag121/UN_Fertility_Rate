library(tidyverse)

## Create duplicate data frame
tfr1950_pred <- tfr1950 %>%
  as.tbl() %>%
  # Create bias variable, adjusted TFR
  mutate(residual = 0, tfr_adj = DataValue, tfr_adj_new = tfr_adj)

## Test out on Afghanistan
afghan1950 <- tfr1950_pred %>%
  filter(Country.or.area == "Afghanistan")

for(i in 1:100) {
  afghan_model <- best_model("Afghanistan", tfr_var = "tfr_adj", df = afghan1950, print.plot = FALSE)
  afghan1950 <- afghan1950 %>%
    mutate(year_ctr = year - mean(year)) %>%
    mutate(predicted = predict(afghan_model$model, newdata = .)) %>%
    group_by(DataProcessType) %>%
    mutate(residual = mean(tfr_adj - predicted)) %>%
    ungroup() %>%
    mutate(tfr_adj = tfr_adj - residual)
}

afghan1950 %>%
  group_by(DataProcessType) %>%
  summarise(mean = mean(tfr_adj - DataValue), sd = sd(tfr_adj - DataValue))

## Test out on Afghanistan
brazil1950 <- tfr1950_pred %>%
  filter(Country.or.area == "Rwanda")

for(i in 1:100) {
  brazil_model <- best_model("Rwanda", tfr_var = "tfr_adj", df = brazil1950, print.plot = FALSE)
  brazil1950 <- brazil1950 %>%
    mutate(year_ctr = year - mean(year)) %>%
    mutate(predicted = predict(brazil_model$model, newdata = .)) %>%
    group_by(DataProcessType) %>%
    mutate(residual = mean(tfr_adj - predicted)) %>%
    ungroup() %>%
    mutate(tfr_adj = tfr_adj - residual)
}

brazil1950 %>%
  group_by(DataProcessType) %>%
  summarise(mean = mean(tfr_adj - DataValue), sd = sd(tfr_adj - DataValue))
