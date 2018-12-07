#### Full TFR projection pipeline ####
library(data.table)
library(lubridate)
suppressMessages(library(tidyverse))

#### Read in data ####
wpp_raw <- read.csv("wpp2017-fertility-estimates.csv", header = T)
wpp <- wpp_raw %>% 
  filter(Indicator == "TFR") %>% 
  rename(month = Year) %>% 
  mutate(year = substr(month, 1, 4) %>% as.integer,
         Type = "UN WPP 2017") %>% 
  left_join(subregion[, c(2,1)], by = c("LocID" = "ISO.code"))
subregion <- read.csv("un_subregion.csv", header = TRUE, stringsAsFactors = FALSE)
tfr <- read.csv("tfr.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  # Merge in UN geographic subregions/development status
  merge(subregion[, -1], by = "ISO.code", all.x = TRUE) %>%
  
  # Generate usable year variables
  mutate(TimeMid = as.Date.character(format(date_decimal(TimeMid), "%Y-%m-%d")),
         year = lubridate::year(TimeMid)) %>%
  
  # Filter to 1950-2016
  filter(year <= 2016, year >= 1950)

#### Get yearly averages ####
tfr_m <- tfr %>% 
  group_by(year, ISO.code, Country.or.area) %>% 
  summarise(DataValue = mean(DataValue)) %>% 
  arrange(ISO.code, year) %>% 
  ungroup()

#### Run library file ####
source("library.R")

#### Impute missing years ####
## Create 1950:2016 x countries tibble
tfr_imputed <- expand.grid(year = 1950:2016, Country.or.area = unique(tfr$Country.or.area) %>% sort()) %>%
  dplyr::select(Country.or.area, year) %>%
  merge(tfr_m, by = c("Country.or.area", "year"), all.x = TRUE) %>%
  as.tbl() %>%
  group_by(Country.or.area) %>%
  mutate(ISO.code = mean(ISO.code, na.rm = TRUE),
         DataValue.imputed = DataValue,
         sdev = 0) %>%
  ungroup()

## How many neighbors?
n_neighbors <- 5

## Make imputations
for(i in 1:nrow(tfr_imputed)) {
  if(is.na(tfr_imputed$DataValue[i])) {
    imputation <- imputeNearest(tfr_imputed$Country.or.area[i], impute_year = tfr_imputed$year[i], n = n_neighbors)
    tfr_imputed$DataValue.imputed[i] <- imputation$`tfr`
    tfr_imputed$sdev[i] <- imputation$tfr_sd
    if(i %% 25 == 0) {
      cat("Imputation ", i, ": ", as.character(tfr_imputed$Country.or.area[i]), ", ", tfr_imputed$year[i], "\n", sep = "")
    }
  }
}

tfr_with_imputations <- bind_rows(tfr %>% mutate(Type = "Original"), 
                                  tfr_imputed %>% 
                                    filter(is.na(DataValue)) %>%
                                    mutate(DataValue = DataValue.imputed)) %>%
  arrange(Country.or.area, year) %>%
  as.tbl()
tfr_with_imputations$Type[is.na(tfr_with_imputations$Type)] <- "Imputed"

save(tfr_imputed, file = paste0("tfr_imputed_", n_neighbors))

## Restrict imputed values to min/max
tfr_imputed_restricted <- tfr_imputed %>%
  group_by(Country.or.area) %>%
  mutate(max_obs = max(DataValue, na.rm = TRUE),
         min_obs = min(DataValue, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(DataValue.imputed = case_when(
    DataValue.imputed >= min_obs & DataValue.imputed <= max_obs ~ DataValue.imputed,
    DataValue.imputed > max_obs ~ max_obs,
    DataValue.imputed < min_obs ~ min_obs
  ))

#### Comparing imputed and not ####
## Poor quality data (Afghanistan)
afghanistan <- best_model("Afghanistan")
afghanistan_imputed <- best_model("Afghanistan", tfr_data = tfr_with_imputations, imputed = TRUE)

## High quality data (United States)
canada <- best_model("Canada")
canada_imputed <- best_model("Canada", tfr_data = tfr_with_imputations, imputed = TRUE)

## Medium quality data (Brazil)
brazil <- best_model("Brazil")
brazil_imputed <- best_model("Brazil", tfr_data = tfr_with_imputations, imputed = TRUE)

## A lot of data of middling quality (China)
china <- best_model("China")
china_imputed <- best_model("China", tfr_data = tfr_with_imputations, imputed = TRUE)

## Very little data
zambia <- best_model("Zambia")
zambia_imputed <- best_model("Zambia", tfr_data = tfr_with_imputations, imputed = TRUE)

#### Comparing to the "real" estimates ####


tfr_imputed <- tfr_imputed %>%
  merge(wpp, by = c("Country.or.area", "year"), all.x = TRUE) %>%
  as.tbl() %>%
  mutate(error = DataValue.imputed - DataValue.y)

tfr_imputed %>%
  summarise(RMSE = sqrt(sum(error^2, na.rm = TRUE)/sum(!is.na(DataValue.x))))

tfr_imputed %>%
  group_by(Country.or.area) %>%
  summarise(RMSE = sqrt(sum(error^2, na.rm = TRUE)/sum(!is.na(DataValue.x)))) %>%
  View()

## Plotting all the things
plot_list <- vector("list", 2*n_distinct(tfr_with_imputations$Country.or.area))
country_list <- unique(tfr_with_imputations$Country.or.area)
for(i in 2*(1:n_distinct(tfr_with_imputations$Country.or.area))) {
  country <- country_list[i/2]
  plot_list[[i-1]] <- best_model(country, tfr_data = tfr)$plot
  plot_list[[i]] <- best_model(country, tfr_data = tfr_with_imputations, imputed = TRUE)$plot
}
ggsave(filename = "country_plots.pdf", marrangeGrob(grobs = plot_list, nrow = 2, ncol = 2))
