#### Local quadratic imputation ####

#### First: obtaining local quadratic approximations
## List countries 
countries <- unique(tfr1950$Country.or.area)

## Go by ten-year windows, moving five years at a time
mid_years <- seq(from = 1954.5, to = 2004.5, by = 5)
year_windows <- data.frame(year_start = mid_years - 4.5, 
                           year_end = mid_years + 4.5)

country_windows <- expand.grid(country = countries, year_start = year_windows$year_start) %>%
  merge(year_windows) %>%
  mutate(year_mid = year_start + 4.5,
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

## Compute distances on complete data
qa_complete <- country_windows %>%
  na.omit() %>%
  dplyr::select(b0, b1, b2)

coef_dist <- dist(qa_complete, method = "manhattan") %>% as.matrix() # distance matrix; use Manhattan

## nearestCountries() function
nearestCountries <- function(country_name, dist_fun = "manhattan", n = 1, data = country_windows) {
  ## Input the country name, and number of nearest neighbors you want
  ## dist_method defaults to Manhattan distance while n defaults to 1
  require(dplyr)
  qa_complete <- data %>% na.omit()
  
  ## Get decades for which enough data exist
  start_years <- qa_complete %>% 
    filter(country == country_name) %>%
    pull(year_start)
  n_decades <- length(start_years)
  
  ## Initialize data frame to store nearest countries and distances
  nn_df <- tibble(country = country_name,
                  year = start_years)
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
    current_data <- current_data %>%
      mutate(manhattan = abs(b0 - coefs[1]) + abs(b1 - coefs[2]) + abs(b2 - coefs[3]),
             euclidean = (b0 - coefs[1])^2 + (b1 - coefs[2])^2 + (b2 - coefs[3])^2)
    
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

## Test:
nearestCountries("Afghanistan", n = 3)
