library(tidyverse)
library(reshape2)

## List covariates
covariate_names <- gsub(".csv", "", list.files("Covariates"))
for(covariate in covariate_names) {
  assign(covariate, value = read.csv(paste0("Covariates/", covariate, ".csv"), header = TRUE, stringsAsFactors = FALSE))
}

# ## Reshape long to wide
# wide_vars <- c("crop_prod", "cropland", "electricity", "gdp", "gdppc", "gini", "infant_mortality", "lifeexp", "livestock_prod",
#                "net_exports", "net_fdi", "taxes")
# wide_var_list <- list(crop_prod, cropland, electricity, gdp, gdppc, gini, infant_mortality, lifeexp, livestock_prod, net_exports,
#                       net_fdi, taxes)
# 
# extractStringRight <- function(x, n) {
#   return(substr(x, nchar(x) - n + 1, nchar(x)))
# }
# wide2long_list <- lapply(wide_var_list,
#                          function(df) {
#                            df.melted <- melt(df, id.vars = c("Country.Name", "Country.Code")) %>%
#                              mutate(year = as.character(variable)) %>%
#                              dplyr::select(Country.Name, Country.Code, year, value) 
#                          })
# 
# for(i in 1:length(wide_vars)) {
#   wide2long_list[[i]]$year <- wide2long_list[[i]]$year %>% extractStringRight(4) %>% as.numeric
#   names(wide2long_list[[i]])[4] <- wide_vars[i]
#   write.csv(wide2long_list[[i]], paste0("Covariates/",wide_vars[i],".csv"), row.names = FALSE)
# }

## Merging covariates
UN_covariates <- abortion_legality %>%
  merge(air_pollution, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(avg_slope, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(cigarettes, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(coal_pollution, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(contraception, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(corn, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(diabetes_prevalence, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(fourwheelers, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(haqi, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(health_access, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(majority_muslim, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(pct_rural, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(pct_urban, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(redmeat, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(rice, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(war_mortality_10yr, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(waterprop, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(pct_under30, by = c("location_id", "location_name", "year_id"), all.x = TRUE) %>%
  merge(pct_over65, by = c("location_id", "location_name", "year_id"), all.x = TRUE)

## Merge the World Bank datasets
UN_covariates <- abortion_legality %>%
  merge(air_pollution, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(avg_slope, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(cigarettes, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(coal_pollution, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(contraception, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(corn, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(diabetes_prevalence, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(fourwheelers, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(haqi, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(health_access, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(majority_muslim, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(pct_rural, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(pct_urban, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(redmeat, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(rice, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(war_mortality_10yr, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(waterprop, by = c("location_id", "location_name", "year_id"), all = TRUE) %>%
  merge(pct_under30, by = c("location_id", "location_name", "year_id"), all.x = TRUE) %>%
  merge(pct_over65, by = c("location_id", "location_name", "year_id"), all.x = TRUE)

# UN name-to-World Bank name key
country_key <- read.csv("UN_worldbank_country_key.csv", header = TRUE, stringsAsFactors = FALSE)

WB_covariates <- country_key %>%
  merge(crop_prod, by = "Country.Name", all = TRUE) %>%
  merge(cropland, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(electricity, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(gdp, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(gdppc, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(gini, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(infant_mortality, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(lifeexp, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(livestock_prod, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(net_exports, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(net_fdi, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  merge(taxes, by = c("Country.Name", "Country.Code", "year"), all = TRUE) %>%
  as.tbl()

# Merge World Bank with UN
subregions <- read.csv("un_subregion.csv", header = TRUE, stringsAsFactors = FALSE)

all_covariates <- merge(UN_covariates, WB_covariates, by.x = c("location_name", "year_id"),
                        by.y = c("Country.Name", "year"), all.x = TRUE, all.y = FALSE) %>%
  merge(subregions, by.x = "location_name", by.y = "Country.or.area", all.x = TRUE) %>%
  as.tbl() %>%
  select(-Country.Code, -location_name.y)

all_covariates.scaled <- all_covariates %>%
  mutate_at(4:35, as.numeric) %>%
  mutate_at(4:35, scale) %>%
  group_by(location_name, year_id, location_id, ISO.code, region, development) %>%
  summarise_all(mean) %>%
  arrange(location_name, year_id) %>%
  select(-gini, -taxes) %>%
  na.omit() %>%
  ungroup()

pca_post2000 <- prcomp(~abortion_legality+outdoor_pollution+avg_slope+cigarettes+coal_pollution_pc+contraception_use+corn_grams+
                         diabetes_prev+fourwheelers_pc+haqi+health_access+majority_muslim+pct_under_150_per_sqkm+pct_1000_per_sqkm+
                         redmeat_grams+rice_grams+war_mortality_10yr+water_prop+pct_under30+pct_over_65+crop_prod+cropland+electricity+
                         gdp+gdppc+infant_mortality+lifeexp+livestock_prod+net_exports+net_fdi, 
                       data = all_covariates.scaled %>% filter(year_id >= 2000))

covariates_pcs_post2000 <- bind_cols(all_covariates.scaled %>% filter(year_id >= 2000) %>%
                                       select(location_name, year_id, location_id, ISO.code, region, development),
                                     as.data.frame(pca_post2000$x)) %>%
  dplyr::select(-year_id, -location_id, -ISO.code) %>%
  group_by(location_name, region, development) %>%
  summarise_at(4:33, mean)

mds_post2000 <- cmdscale(dist(covariates_pcs_post2000[,4:33]), k = 5)
mds_post2000_df <- tibble(country = covariates_pcs_post2000$location_name,
                         region = covariates_pcs_post2000$region,
                         development = covariates_pcs_post2000$development,
                         Dim1 = mds_post2000[,1],
                         Dim2 = mds_post2000[,2],
                         Dim3 = mds_post2000[,3],
                         Dim4 = mds_post2000[,4],
                         Dim5 = mds_post2000[,5])

ggplot(mds_post2000_df, aes(x = Dim1, y = Dim2, col = development)) +
  geom_text(aes(label = country), size = 2) +
  labs(title = "2-dimensional scaling on covariate principal components")

k = 10
mds_kmeans <- kmeans(mds_post2000, k)
mds_post2000_df$cluster <- mds_kmeans$cluster

ggplot(mds_post2000_df, aes(x = Dim1, y = Dim2, col = factor(cluster))) +
  geom_text(aes(label = country), size = 3) +
  labs(title = "2-dimensional scaling on covariate principal components")
