## Plotting all the things (and also evaluating in-sample performance)
plot_list <- models_list <- vector("list", 2*n_distinct(tfr_with_imputations$Country.or.area))
country_list <- unique(tfr_with_imputations$Country.or.area)
for(i in 2*(1:n_distinct(tfr_with_imputations$Country.or.area))) {
  country <- country_list[i/2]
  models_list[[i-1]] <- best_model(country, tfr_data = tfr %>% bind_rows(wpp))
  models_list[[i]] <- best_model(country, tfr_data = tfr_with_imputations, imputed = TRUE)
  plot_list[[i-1]] <- models_list[[i-1]]$plot
  plot_list[[i]] <- models_list[[i]]$plot
}

ggsave(filename = "country_plots.pdf", marrangeGrob(grobs = plot_list, nrow = 2, ncol = 1))

## Calculate MAD/RMSE
models_without_imputation <- models_list[seq(from = 1, to = 401, by = 2)]
models_with_imputation <- models_list[seq(from = 2, to = 402, by = 2)]
country_MADs <- country_RMSEs <- tibble(country = country_list, without_imputations = NA, with_imputations = NA)

for(i in 1:length(country_list)) {
  country <- country_list[i]
  country_MADs$without_imputations[i] <- models_without_imputation[[i]]$predictions %>%
    merge(wpp %>% filter(Country.or.area == country), by = "year", all.x = TRUE) %>% 
    mutate(abs_error = abs(fit - wpp.est)) %>% 
    summarise(mad = round(sum(abs_error)/n(), 2)) %>% 
    pull(mad)
  country_MADs$with_imputations[i] <- models_with_imputation[[i]]$predictions %>%
    merge(wpp %>% filter(Country.or.area == country), by = "year", all.x = TRUE) %>% 
    mutate(abs_error = abs(fit - wpp.est)) %>% 
    summarise(mad = round(sum(abs_error)/n(), 2)) %>% 
    pull(mad)
  country_RMSEs$without_imputations[i] <- models_without_imputation[[i]]$predictions %>%
    merge(wpp %>% filter(Country.or.area == country), by = "year", all.x = TRUE) %>% 
    mutate(sq_error = (fit - wpp.est)^2) %>% 
    summarise(rmse = round(sqrt(sum(sq_error)/n()), 2)) %>% 
    pull(rmse)
  country_RMSEs$with_imputations[i] <- models_with_imputation[[i]]$predictions %>%
    merge(wpp %>% filter(Country.or.area == country), by = "year", all.x = TRUE) %>% 
    mutate(sq_error = (fit - wpp.est)^2) %>% 
    summarise(rmse = round(sqrt(sum(sq_error)/n()), 2)) %>% 
    pull(rmse)
}

## Overall
country_MADs %>%
  summarise(without_imputations = mean(without_imputations, na.rm = TRUE),
            with_imputations = mean(with_imputations, na.rm = TRUE))
country_RMSEs %>%
  summarise(without_imputations = sqrt(mean(without_imputations^2, na.rm = TRUE)),
            with_imputations = sqrt(mean(with_imputations^2, na.rm = TRUE)))
