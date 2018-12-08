#### Full TFR projection pipeline ####
library(data.table)
library(lubridate)
suppressMessages(library(tidyverse))

#### Read in data ####
subregion <- read.csv("un_subregion.csv", header = T)
tfr <- read.csv("tfr.csv", stringsAsFactors = FALSE) %>%
  merge(subregion[, -1], by = "ISO.code", all.x = TRUE) %>%
  mutate(TimeMid = as.Date.character(format(date_decimal(TimeMid), "%Y-%m-%d")),
         year = year(TimeMid),
         Type = "Original") %>% 
  filter(year <= 2016, year >= 1950)
wpp_raw <- read.csv("wpp2017-fertility-estimates.csv", header = T)
wpp <- wpp_raw %>% 
  filter(Indicator == "TFR") %>% 
  rename(month = Year) %>% 
  mutate(year = substr(month, 1, 4) %>% as.integer,
         Type = "UN WPP 2017") %>% 
  rename(wpp.est = DataValue) %>%
  left_join(subregion[, c(2,1)], by = c("LocID" = "ISO.code"))

tfr_wpp <- left_join(tfr, wpp[, c(1,5,4)], by = c("ISO.code" = "LocID", "year" = "year"))
wt.recall <- 1

# the higher the weight, the more severe penalty towards recall lag, default is a 1 year recall lag loses 10 points in reliability
tfr_adj <- tfr_wpp %>% 
  mutate(diff = DataValue - wpp.est, pct.diff = abs((DataValue - wpp.est)/wpp.est), 
         reliable = 1 - pct.diff, reliable.adj = reliable * exp(log(10/9) * wt.recall * RecallLag)) %>% 
  group_by(DataProcessType, DataProcess, Source, DataTypeGroupName, DataTypeRecoded, region, development) %>% 
  mutate(bias.m = mean(diff), rel.m = mean(reliable), rel.m.adj = mean(reliable.adj)) %>% 
  ungroup() %>% 
  mutate(DataValue = DataValue - bias.m)

tfr <- tfr_adj

# subregion <- read.csv("un_subregion.csv", header = TRUE, stringsAsFactors = FALSE)
# tfr <- read.csv("tfr.csv", header = TRUE, stringsAsFactors = FALSE) %>%
#   # Merge in UN geographic subregions/development status
#   merge(subregion[, -1], by = "ISO.code", all.x = TRUE) %>%
#   
#   # Generate usable year variables
#   mutate(TimeMid = as.Date.character(format(date_decimal(TimeMid), "%Y-%m-%d")),
#          year = lubridate::year(TimeMid)) %>%
#   
#   # Filter to 1950-2016
#   filter(year <= 2016, year >= 1950)

#### Get yearly averages ####
tfr_m <- tfr_adj %>% 
  group_by(year, ISO.code, Country.or.area) %>% 
  summarise(DataValue = mean(DataValue)) %>% 
  arrange(ISO.code, year) %>% 
  ungroup()

#### Run library file ####
source("library.R")

#### Impute missing years ####
## Create 1950:2016 x countries tibble
tfr_imputed <- expand.grid(year = seq(from = 1950, to = 2016, by = 2), Country.or.area = unique(tfr$Country.or.area) %>% sort()) %>%
  dplyr::select(Country.or.area, year) %>%
  merge(tfr_m, by = c("Country.or.area", "year"), all.x = TRUE) %>%
  as.tbl() %>%
  group_by(Country.or.area) %>%
  mutate(ISO.code = mean(ISO.code, na.rm = TRUE),
         DataValue.imputed = DataValue,
         sdev = 0) %>%
  ungroup()

## How many neighbors?
n_neighbors <- 7

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

## Plotting all the things (and also evaluating in-sample performance)
plot_list <- models_list <- vector("list", 2*n_distinct(tfr_with_imputations$Country.or.area))
country_list <- unique(tfr_with_imputations$Country.or.area)
for(i in 2*(1:n_distinct(tfr_with_imputations$Country.or.area))) {
  country <- country_list[i/2]
  models_list[[i-1]] <- best_model(country, tfr_data = tfr %>% mutate(Type = "Original") %>% bind_rows(wpp))
  models_list[[i]] <- best_model(country, tfr_data = tfr_with_imputations, imputed = TRUE)
  plot_list[[i-1]] <- models_list[[i-1]]$plot
  plot_list[[i]] <- models_list[[i]]$plot
}

ggsave(filename = "country_plots.pdf", marrangeGrob(grobs = plot_list, nrow = 2, ncol = 1))

sapply()