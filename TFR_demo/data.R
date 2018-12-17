tfr_p <- read.csv("tfr_plot.csv", header = T)
wpp <- read.csv("wpp_plot.csv", header = T)
imputed_p <- read.csv("imputed_plot.csv", header = T)

best_model <- function(country, tfr_data, tfr_var = "DataValue", max_degree = 10, print.plot = TRUE, imputed = FALSE, show.wpp = TRUE) {
  
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
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_colour_manual(name = "Type", values = c("red", "green4", "#5B92E5"), labels = c("Imputed", "Original", "UN WPP 2017")) +
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    } else if(!imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue, col = Type), size = 1) + 
        geom_line(data = country_data %>% filter(Type == "UN WPP 2017"),
                  aes(x = year, y = DataValue), col = "#5B92E5") +
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_colour_manual(name = "Type", values = c("green4", "#5B92E5"), labels = c("Original", "UN WPP 2017")) +
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    }
  } else if(!show.wpp) {
    if(imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue, col = Type), size = 1) + 
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
        scale_colour_manual(name = "Type", values = c("red", "green4"), labels = c("Imputed", "Original")) +
        ggtitle(paste(country)) +
        lims(x = c(1950,2016))
    } else if(!imputed) {
      p <- ggplot() + 
        geom_point(data = country_data, aes(x = year, y = DataValue), size = 1, col = "green4") + 
        geom_line(data = country.pred, aes(x = year, y = fit)) + 
        geom_ribbon(data = country.pred, aes(x = year, ymin = lwr, ymax = upr), alpha = 0.4) + 
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
