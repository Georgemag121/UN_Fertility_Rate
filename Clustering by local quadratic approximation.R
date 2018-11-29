#### Clustering by local quadratic approximation coefficient
## (Idea by Kaushik Mohan)
## The main idea is that for any country, we can fit local quadratic polynomials at 10-year windows (or we can pick a different window), 
## centering the year at the middle of the window. We can then use the coefficients of those polynomials as features when generating 
## clusters; countries with similar coefficients over more periods of time tend to "behave like" each other with respect to TFR, so it
## stands to reason that they would make good clusters from which to impute missing years. 
## 
## Conveniently, it also tends to be the case that the linear coefficient is smaller in magnitude than the intercept, and the quadratic 
## coefficient is smaller still. That's roughly the order we care about, since they correspond to the average TFR at the midpoint of the
## time window, the slope of the TFR there, and the slope of the slope there, which gives a pretty good picture of the shape of the trend
## at the different time windows.

library(tidyverse)

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

#### For each country and time window, fit a quadratic polynomial and store the coefficients
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

#### Clustering
## How many clusters do we want to try?
max_clusters = 30

country_windows_cc <- country_windows %>% 
  na.omit() 

clusters_matrix <- matrix(NA, nrow(country_windows_cc), max_clusters)
kmeans_list <- vector("list", max_clusters)

set.seed(2401)
for(k in 1:max_clusters) { # try up to 30 clusters
  kmeans_list[[k]] <- kmeans(country_windows_cc %>% dplyr::select(b0, b1, b2), centers = k, nstart = 5)
}

## Data frame of clusters
clusters <- lapply(kmeans_list, function(kmeans_obj) return(kmeans_obj$cluster)) %>% 
  bind_cols()
names(clusters) <- paste0("K", 1:max_clusters)
country_windows_cc <- bind_cols(country_windows_cc, clusters)
qa_clusters <- country_windows %>%
  merge(country_windows_cc, by = c("country", "year_start", "year_mid", "year_end", "b0", "b1", "b2"), all.x = TRUE) %>%
  mutate_at(vars(starts_with("K")), as.factor)

# Read in country codes for easy plotting
country_codes <- read.csv("country_codes.csv", stringsAsFactors = FALSE)
qa_clusters <- qa_clusters %>%
  merge(country_codes, by = "country", all.x = TRUE)

## Diagnosing clusters
## Whence the elbow?
var_explained <- sapply(kmeans_list, function(kmeans_obj) return(kmeans_obj$betweenss/kmeans_obj$totss))
plot(var_explained, type = "l", main = "Proportion of variance explained by clusters",
     xlab = "K (number of clusters)", ylab = "Fraction of variance explained")

## Plotting clusters
# 1950-1959
ggplot(qa_clusters %>% filter(year_start == 1950), aes(x = b0, y = b1, col = K8)) +
  geom_text(aes(label = country_abbr), size = 2) +
  scale_colour_discrete(name = "Cluster") +
  labs(title = "Countries clustered by quadratic approximation coefficients",
       subtitle = "1950 to 1959",
       x = "Intercept",
       y = "Linear coefficient")
# 1960-1969
ggplot(qa_clusters %>% filter(year_start == 1960), aes(x = b0, y = b1, col = K8)) +
  geom_text(aes(label = country_abbr), size = 2) +
  scale_colour_discrete(name = "Cluster") +
  labs(title = "Countries clustered by quadratic approximation coefficients",
       subtitle = "1960 to 1969",
       x = "Intercept",
       y = "Linear coefficient")
# 1970-1979
ggplot(qa_clusters %>% filter(year_start == 1970), aes(x = b0, y = b1, col = K8)) +
  geom_text(aes(label = country_abbr), size = 2) +
  scale_colour_discrete(name = "Cluster") +
  labs(title = "Countries clustered by quadratic approximation coefficients",
       subtitle = "1970 to 1979",
       x = "Intercept",
       y = "Linear coefficient")
# 1980-1989
ggplot(qa_clusters %>% filter(year_start == 1980), aes(x = b0, y = b1, col = K8)) +
  geom_text(aes(label = country_abbr), size = 2) +
  scale_colour_discrete(name = "Cluster") +
  labs(title = "Countries clustered by quadratic approximation coefficients",
       subtitle = "1980 to 1989",
       x = "Intercept",
       y = "Linear coefficient")
# 1990-1999
ggplot(qa_clusters %>% filter(year_start == 1990), aes(x = b0, y = b1, col = K8)) +
  geom_text(aes(label = country_abbr), size = 2) +
  scale_colour_discrete(name = "Cluster") +
  labs(title = "Countries clustered by quadratic approximation coefficients",
       subtitle = "1990 to 1999",
       x = "Intercept",
       y = "Linear coefficient")
# 2000-2010
ggplot(qa_clusters %>% filter(year_start == 2000), aes(x = b0, y = b1, col = K8)) +
  geom_text(aes(label = country_abbr), size = 2) +
  scale_colour_discrete(name = "Cluster") +
  labs(title = "Countries clustered by quadratic approximation coefficients",
       subtitle = "2000 to 2009",
       x = "Intercept",
       y = "Linear coefficient")

#### Using "nearest neighbors" in coefficient-space to impute missing values
