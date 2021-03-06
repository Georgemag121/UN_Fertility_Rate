#### Local quadratic imputation ####

#### First: obtaining local quadratic approximations
## List countries 
countries <- unique(tfr$Country.or.area)

## Go by 15-year windows, moving one year at a time
window_size = 15
mid_years <- seq(from = 1950 + (window_size - 1)/2, to = 2006, by = 1)
year_windows <- data.frame(year_start = mid_years - (window_size - 1)/2, 
                           
                           year_end = mid_years + (window_size - 1)/2)

country_windows <- expand.grid(country = countries, year_start = year_windows$year_start) %>%
  merge(year_windows) %>%
  mutate(year_mid = year_start + (window_size - 1)/2,
         country = as.character(country)) %>%
  select(country, year_start, year_mid, year_end) %>%
  arrange(country, year_start) %>%
  as.tbl()

## Initialize coefficient variables
country_windows$b2 <- country_windows$b1 <- country_windows$b0 <- NA

for(i in 1:nrow(country_windows)) {
  # Identify country and years
  country <- country_windows$country[i]
  year_start <- country_windows$year_start[i]
  year_mid <- country_windows$year_mid[i]
  year_end <- country_windows$year_end[i]
  
  # Temporary subset of the tfr1950_pred data
  country_data_temp <- tfr1950_pred %>%
    dplyr::filter(Country.or.area == country, year >= year_start, year <= year_end) %>%
    # Center years at the midpoint of the window
    mutate(year = year - year_mid)
  
  # If there's enough data, fit a quadratic
  if(nrow(country_data_temp) >= 3) {
    quadratic_approximation <- lm(tfr_adj~year+I(year^2), data = country_data_temp)
    country_windows[i,5:7] <- coefficients(quadratic_approximation)
  }
  if(i %% 25 == 0) {
    print(paste("Iteration", i, ":", country, year_start, "-", year_end))
  }
}

## nearestCountries() function
nearestCountries <- function(country_name, use_int = TRUE, dist_fun = "manhattan", n = 1, data = country_windows) {
  ## Input the country name, and number of nearest neighbors you want
  ## dist_method defaults to Manhattan distance while n defaults to 1
  ## use_int: do you want to compute distance using local slope or local value?
  require(dplyr)
  qa_complete <- data %>% na.omit()
  
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
      filter(country != country_name)
    
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

#### imputeNearest() function
imputeNearest <- function(country_name, impute_year, compute_changes = FALSE, dist_fun = "manhattan", n = 1, max_years = 3, data = country_windows, 
                          tfr_data = tfr1950_pred) {
  ## Takes same arguments as neighbors_df, plus arguments for the TFR data
  ## max_years argument: number of nearby years by which to find 
  require(dplyr)
  require(Hmisc)
  require(reshape2)
  require(tidyr)
  
  previous_year <- tfr_data %>%
    filter(Country.or.area == country_name, year < impute_year) %>%
    pull(year) %>%
    max()
  
  ## Compute neighbors data frames
  neighbors_df <- nearestCountries(country_name, use_int = !compute_changes, dist_fun = dist_fun, n = n, data = country_windows) %>%
    mutate(year_diff = impute_year - mid_year) %>%
    arrange(abs(year_diff)) %>%
    # Take nearest three years
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
    mutate(weight = as.numeric(weight)/sum(as.numeric(weight)))
  
  coefs_df <- merge(neighbors_df, country_windows, by.x = c("mid_year", "country"), by.y = c("year_mid", "country"), all.x = TRUE)
  
  ## Imputations from nearby countries
  if(!compute_changes) { # when imputing using levels
    coefs_df <- coefs_df %>%
      mutate(pred = b0 + b1*(impute_year - mid_year) + b2*(impute_year - mid_year)^2)
    pred <- Hmisc::wtd.mean(coefs_df$pred, weights = coefs_df$weight)
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