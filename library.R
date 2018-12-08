library(openxlsx)
library(tidyverse)
library(lubridate)
library(stats)
library(data.table)
library(gridExtra)
library(gganimate)
library(glmnet)

#### Reliability adjustment ####
a = 0.9
# Back to 8324
dup1 <- tfr %>% 
  mutate(recal = exp(RecallLag*a), 
         dhs = (DataProcess == "DHS-NS" | DataProcess == "DHS" | DataProcess == "DHS/MICS" | DataProcess == "DHS/PAPFAM"), 
         direct = (DataTypeGroupName == "Direct" | DataTypeGroupName == "UN computed")) %>% 
  group_by(Country.or.area, year) %>% 
  mutate(dup = n(), 
         tot_dir = sum(direct), 
         tot_dhs = sum(dhs)) %>% 
  ungroup() %>% 
  filter(dup > 1)

# 4930 with no dups group 1 and 2
group1 <- tfr %>% 
  mutate(recal = exp(RecallLag*a), 
         dhs = (DataProcess == "DHS-NS" | DataProcess == "DHS" | DataProcess == "DHS/MICS" | DataProcess == "DHS/PAPFAM"), 
         direct = (DataTypeGroupName == "Direct" | DataTypeGroupName == "UN computed")) %>% 
  group_by(Country.or.area, year) %>% 
  mutate(dup = n(), 
         tot_dir = sum(direct)) %>% 
  ungroup() %>% 
  filter(dup == 1) %>% 
  mutate(tfr_new = DataValue) %>% 
  mutate(group.num = direct + 1) %>% 
  distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 516 dups with only one direct
group3 <- dup1 %>% 
  filter(tot_dir == 1, direct == TRUE) %>% 
  mutate(tfr_new = DataValue) %>% 
  mutate(group.num = 3) %>% 
  distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 781 dups with no direct, weighted avg of indirect
group4 <- dup1 %>% 
  filter(tot_dir == 0) %>% 
  group_by(Country.or.area, year) %>% 
  mutate(tfr_new = weighted.mean(DataValue, w = exp(RecallLag))) %>% 
  ungroup() %>%  
  mutate(group.num = 4) %>% 
  distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 799 dups with multiple direct and contain dhs sources
group5 <- dup1 %>% 
  filter(tot_dir >= 2,  tot_dhs >= 1, direct == TRUE | dhs == TRUE) %>% 
  group_by(Country.or.area, year) %>% 
  mutate(tfr_new = weighted.mean(DataValue, w = exp(RecallLag))) %>% 
  ungroup() %>% 
  mutate(group.num = 5) %>% 
  distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# 5548 dups with multiple direct and no dhs sources
group6 <- dup1 %>% 
  filter(tot_dir >= 2, direct == TRUE, tot_dhs == 0) %>% 
  group_by(Country.or.area, year) %>% 
  mutate(tfr_new = weighted.mean(DataValue, w = exp(RecallLag))) %>% 
  ungroup() %>% 
  mutate(group.num = 6) %>% 
  distinct(ISO.code, Country.or.area, year, tfr_new, group.num) 

# Combine 8322, 
tfr_adjusted <- rbind(group1, group3, group4, group5, group6) %>% 
  arrange(Country.or.area, year) %>% 
  mutate(indirect = (group.num == 1 | group.num == 4)) %>%
  rename(DataValue = tfr_new)

tfr_adjusted %>% 
  filter(Country.or.area == "Zambia") %>% 
  ggplot(aes(x = year, y = DataValue)) + geom_point(aes(col = indirect)) + geom_line() + lims(x = c(1950,2016)) + scale_y_continuous(limits = c(0, 11.5))

#### Preliminary work for nearestCountries function ####
countries <- unique(tfr$Country.or.area)

## Go by 15-year windows, moving one year at a time
window_size = 25
mid_years <- seq(from = 1950, to = 2016, by = 2)
year_windows <- data.frame(year_start = mid_years - (window_size - 1)/2,
                           year_mid = mid_years,
                           year_end = mid_years + (window_size - 1)/2)

country_windows <- expand.grid(country = countries, year_start = year_windows$year_start) %>%
  merge(year_windows) %>%
  mutate(year_mid = year_start + (window_size - 1)/2,
         country = as.character(country)) %>%
  dplyr::select(country, year_start, year_mid, year_end) %>%
  arrange(country, year_start) %>%
  as.tbl()

country_data <- country_windows %>%
  merge(tfr, by.x = c("country", "year_mid"), by.y = c("Country.or.area", "year"), all = TRUE) %>%
  as.tbl()

## Initialize coefficient variables
country_windows$b2 <- country_windows$b1 <- country_windows$b0 <- country_windows$DataValue <- NA

cat("Computing country-year local quadratic approximations....\n")
for(i in 1:nrow(country_windows)) {
  # Identify country and years
  country <- country_windows$country[i]
  year_start <- country_windows$year_start[i]
  year_mid <- country_windows$year_mid[i]
  year_end <- country_windows$year_end[i]
  
  # Temporary subset of the tfr data
  country_data_temp <- tfr %>%
    dplyr::filter(Country.or.area == country, year >= year_start, year <= year_end) %>%
    # Center years at the midpoint of the window (and weight using Gaussian kernel, sd = 4)
    mutate(year = year - year_mid,
           weight = dnorm(year, mean = 0, sd = 4)) 
  
  country_windows$DataValue[i] <- country_data %>%
    filter(year_mid == country_windows$year_mid[i], country == country_windows$country[i]) %>%
    summarise(DataValue = mean(DataValue)) %>%
    pull(DataValue)
  
  # If there's enough data, fit a quadratic
  if(nrow(country_data_temp) >= 3) {
    quadratic_approximation <- lm(DataValue~year+I(year^2), data = country_data_temp, weights = weight)
    country_windows[i,6:8] <- coefficients(quadratic_approximation)
  }
  if(i %% 67 == 0) {
    cat(country, "\n")
  }
}

#### FUNCTIONS ####
## best_model: find best polynomial fit TFR data
best_model <- function(country, tfr_data, tfr_var = "DataValue", max_degree = 10, print.plot = TRUE, imputed = FALSE, show.wpp = TRUE) {
  ## Load dplyr (required)
  require(dplyr)
  
  ## If country is ISO.code, identify country
  if(is.numeric(country) | is.integer(country)) {
    country <- lookupISO(country)
  }
  
  ## Create country data frame
  country_data <- tfr_data %>%
    filter(Country.or.area == country, year >= 1950, year <= 2016)
  mean_year <- mean(country_data$year)
  country_data <- country_data %>%
    mutate(year_ctr = year - mean_year)
  
  ## Initialize values for loop
  n <- nrow(country_data)
  current_formula <- paste0(tfr_var,"~year_ctr")
  best_model <- lm(formula(current_formula), data = country_data)
  best_degree <- 1
  p_value <- NA
  
  ## Iteratively increase degree of polynomial and test if last degree is significant
  for(i in 2:max_degree) {
    ## Stitch new term onto polynomial
    new_formula <- paste0(current_formula,"+I(year_ctr^",i,")")
    new_model <- lm(formula(new_formula), data = country_data)
    compare_anova <- anova(new_model, best_model)
    
    ## Is it significant? 
    if((compare_anova$`Pr(>F)`[2] < 0.05) & is.na(compare_anova$`Pr(>F)`[2]) == FALSE) {
      best_model <- new_model
      best_degree <- i # If so, save the degree as the best one
      p_value <- compare_anova$`Pr(>F)`[2]
    }
    ## Try the next polynomial degree
    current_formula <- new_formula
  }
  
  year1 <- data.frame(year = 1950:2016, year_ctr = 1950:2016 - mean_year)
  predictions <- predict(best_model, newdata = year1, interval = "prediction")
  country.pred <- tibble(year = 1950:2016) %>%
    mutate(fit = predictions[,1],
           lwr = predictions[,2],
           upr = predictions[,3])
  
  ## Create plots
  if(show.wpp) {
    country_data <- bind_rows(country_data, wpp %>% 
                                filter(Country.or.area == country) %>% 
                                mutate(DataValue = wpp.est)
    )
    if(imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue, col = Type), size = 1) + 
        geom_line(data = country_data %>% filter(Type == "Original"), aes(x = year, y = DataValue, linetype = Series), 
                  col = "green4", show.legend = FALSE) +
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_line(data = country_data %>% filter(Type == "UN WPP 2017"),
                  aes(x = year, y = DataValue), col = "#5B92E5") +
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_colour_manual(name = "Type", values = c("red", "green4", "#5B92E5"), labels = c("Imputed", "Original", "UN WPP 2017")) +
        scale_y_continuous(limits = c(0, 11.5)) + 
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    } else if(!imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue, col = Type), size = 1) + 
        geom_line(data = country_data %>% filter(Type == "Original"), aes(x = year, y = DataValue, linetype = Series), 
                  col = "green4", show.legend = FALSE) +
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_line(data = country_data %>% filter(Type == "UN WPP 2017"),
                  aes(x = year, y = DataValue), col = "#5B92E5") +
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_colour_manual(name = "Type", values = c("green4", "#5B92E5"), labels = c("Original", "UN WPP 2017")) +
        scale_y_continuous(limits = c(0, 11.5)) + 
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    }
  } else if(!show.wpp) {
    if(imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue, col = Type), size = 1) + 
        geom_line(data = country_data %>% filter(Type == "Original"), aes(x = year, y = DataValue, linetype = Series), 
                  col = "green4", show.legend = FALSE) +
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_colour_manual(name = "Type", values = c("red", "green4"), labels = c("Imputed", "Original")) +
        scale_y_continuous(limits = c(0, 11.5)) + 
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    } else if(!imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue), size = 1, col = "green4") + 
        geom_line(data = country_data %>% filter(Type == "Original"), aes(x = year, y = DataValue, linetype = Series), 
                  col = "green4", show.legend = FALSE) +
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_y_continuous(limits = c(0, 11.5)) + 
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    }
  }
  
  ## Print plot if specified
  if(print.plot) {
    print(p)
  }
  
  ## Create the object to be returned
  return_object <- list(model = best_model)
  return_object$data <- as.tbl(country_data)
  return_object$details <- list(degree = best_degree, 
                                R2 = summary(best_model$r.squared),
                                RMSE = sqrt(sum(residuals(best_model)^2)/length(residuals(best_model))))
  return_object$predictions <- country.pred
  return_object$plot <- p
  
  return(return_object)
}

## lookupISO: translate ISO code into country name
lookupISO <- function(code) {
  require(dplyr)
  country <- subregion %>%
    filter(ISO.code == code) %>%
    pull(Country.or.area)
  
  return(country)
}

## imputeNearest: impute year from nearest-neighbor countries
imputeNearest <- function(country, impute_year, compute_changes = FALSE, dist_fun = "manhattan", n = 1, max_years = 3, data = country_windows, 
                          tfr_data = tfr, ...) {
  ## Takes same arguments as neighbors_df, plus arguments for the TFR data
  ## max_years argument: number of nearby years by which to find neighbors
  require(dplyr)
  require(Hmisc)
  require(reshape2)
  require(tidyr)
  
  ## If country is ISO.code, identify country
  if(is.numeric(country) | is.integer(country)) {
    country_name <- lookupISO(country)
  } else {
    country_name <- country
  }
  
  previous_year <- tfr_data %>%
    filter(Country.or.area == country_name, year < impute_year) %>%
    pull(year) %>%
    max()
  
  ## Compute neighbors data frames
  neighbors_df <- nearestCountries(country_name, use_int = !compute_changes, dist_fun = dist_fun, n = n, data = country_windows) %>%
    mutate(year_diff = impute_year - mid_year) %>%
    arrange(abs(year_diff)) %>%
    
    # Take nearest max_years years
    head(max_years) %>%
    dplyr::select(-country, -start_year, -end_year, -year_diff) %>%
    mutate_at(vars(starts_with("Dist")), function(x) return(1/x)) %>%
    reshape2::melt(id.vars = "mid_year") %>%
    mutate(variable = case_when(grepl("Country", variable) ~ "Country",
                                grepl("Dist", variable) ~ "Weight"))
  
  ## Merge in coefficients from nearby countries' quadratic approximations
  country_tbl <- neighbors_df %>%
    filter(variable == "Country") %>%
    dplyr::select(mid_year = mid_year, country = value)
  
  weight_tbl <- neighbors_df %>%
    filter(variable == "Weight") %>%
    dplyr::select(weight = value)
  
  neighbors_df <- cbind(country_tbl, weight_tbl) %>%
    mutate(weight = (as.numeric(weight)/sum(as.numeric(weight)))/abs(mid_year - impute_year))
  
  coefs_df <- merge(neighbors_df, data, by.x = c("mid_year", "country"), by.y = c("year_mid", "country"), all.x = TRUE)
  
  ## Imputations from nearby countries
  if(!compute_changes) { # when imputing using levels
    pred <- Hmisc::wtd.mean(coefs_df$DataValue, weights = coefs_df$weight)
    pred_sd <- sqrt(nrow(coefs_df)*sum(coefs_df$weight*(coefs_df$pred - pred)^2)/(nrow(coefs_df) - 1))
  } else if(compute_changes) { # when imputing using changes
    coefs_df <- coefs_df %>%
      mutate(pred = b1 + b2*(impute_year - mid_year))
    pred <- Hmisc::wtd.mean(coefs_df$pred, weights = coefs_df$weight)
    pred_sd <- sqrt(nrow(coefs_df)*sum(coefs_df$weight*(coefs_df$pred - pred)^2)/(nrow(coefs_df) - 1))
  }
  
  return_obj <- list(tfr = pred, tfr_sd = pred_sd, neighbors = coefs_df)
  return(return_obj)
}

## nearestCountries: identify nearest country-years by local quadratic approximation
nearestCountries <- function(country, use_int = TRUE, dist_fun = "manhattan", n = 1, data = country_windows, tfr_data = tfr) {
  ## Input the country name, and number of nearest neighbors you want
  ## dist_method defaults to Manhattan distance while n defaults to 1
  ## use_int: do you want to compute distance using local slope or local value?
  require(dplyr)
  qa_complete <- data %>% na.omit()
  
  if(is.numeric(country) | is.int) {
    country_name <- lookupISO(country)
  } else {
    country_name <- country
  }
  
  ## Get decades for which enough data exist
  start_years <- qa_complete %>% 
    filter(country == country_name) %>%
    pull(year_start)
  n_decades <- length(start_years)
  
  ## Initialize data frame to store nearest countries and distances
  nn_df <- tibble(country = country_name,
                  start_year = start_years,
                  mid_year = start_year + (window_size - 1)/2,
                  end_year = start_year + (window_size - 1))
  for(i in 1:n) {
    nn_df[, paste0("Country", i)] <- NA
    nn_df[, paste0("Dist", i)] <- NA
  }
  
  ## Loop through those start years
  for(i in 1:n_decades) {
    current_year <- start_years[i]
    current_data <- data %>%
      filter(year_start == current_year)
    
    ## Pull coefficients for the current year
    coefs <- (current_data %>%
                filter(country == country_name))[, c("b0", "b1", "b2")] %>%
      t()
    
    current_data <- current_data %>%
      filter(country != country_name) %>%
      na.omit()
    
    ## Compute distance and arrange
    if(use_int) {
      current_data <- current_data %>%
        mutate(manhattan = abs(b0 - coefs[1]) + abs(b1 - coefs[2]) + abs(b2 - coefs[3]),
               euclidean = (b0 - coefs[1])^2 + (b1 - coefs[2])^2 + (b2 - coefs[3])^2)
    } else if(!use_int) {
      current_data <- current_data %>%
        mutate(manhattan = abs(b1 - coefs[2]) + 2*abs(b2 - coefs[3]),
               euclidean = (b1 - coefs[2])^2 + 4*(b2 - coefs[3])^2)
    }
    
    
    if(grepl("euc", dist_fun, ignore.case = TRUE)) {
      neighbors <- current_data %>%
        arrange(euclidean) %>%
        select(country, distance = euclidean) %>%
        head(n)
    } else if(grepl("manh", dist_fun, ignore.case = TRUE)) {
      neighbors <- current_data %>% 
        arrange(manhattan) %>%
        select(country, distance = manhattan) %>%
        head(n)
    }
    
    ## Use the resulting data frame to fill in nearest neighbors for the current year
    for(j in 1:n) {
      nn_df[i, paste0("Country", j)] <- neighbors$country[j]
      nn_df[i, paste0("Dist", j)] <- neighbors$distance[j]
    }
  }
  
  return(nn_df)
}

## tfr_plot: creates plot of TFR over time for specified country
tfr_plot <- function(country, tfr_data = tfr) {
  require(dplyr)
  require(ggplot2)
  
  return_object <- tfr_data %>%
    dplyr::filter(Country.or.area == country) %>%
    ggplot(aes(x = TimeMid, y = DataValue, col = DataCatalog.ShortName)) + 
    
    #geom_point(aes(size = 7 - sqrt(-RecallLag)/2)) + 
    geom_point() +
    geom_line() + 
    scale_x_date(date_labels = "%Y") + 
    
    #geom_smooth() + 
    ggtitle(paste(country)) + 
    scale_y_continuous(limits = c(0, 11.5)) + 
    guides(fill = F) + 
    theme_bw()
  
  return(return_object)
}
