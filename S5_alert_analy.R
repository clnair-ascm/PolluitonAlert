### Load packages
library("dplyr")
library("tidyverse")
library("rsample")
library("h2o")
library("httr")
library("dplyr")
library("doParallel")
library("purrr")

rm(list = ls(all = TRUE))

inc_alert <- readRDS("/Users/yqdai/Downloads/RedAlert/data/chap_era5_daily/aqmet_incalert_predv3.rds")

alert_levels <- names(inc_alert) # Yellow, Orange, Red
pollutants <- c("no2", "so2", "pm25", "pm10")

pcf <- read.csv("/Users/yqdai/Downloads/RedAlert/data/chap_senti/pred_factor_v1.csv")

# Assume pcf is the dataframe with correction factors, and inc_alert is the list containing data for each alert level and city.

# Iterate over each alert level in inc_alert
for (alert_level in names(inc_alert)) {
  
  # Iterate over each city in the current alert level
  for (city in names(inc_alert[[alert_level]])) {
    
    # Get the city's dataframe
    city_df <- inc_alert[[alert_level]][[city]]
    
    # Get the correction factor row for the current city from pcf
    correction_row <- pcf[pcf$City == city, ]
    
    # Apply the correction factor to each pollutant column
    if (nrow(correction_row) == 1) {  # Check if the city has a corresponding row in pcf
      city_df$pm25_modr <- city_df$pm25_mod / (1 + correction_row$pm25)
      city_df$pm10_modr <- city_df$pm10_mod / (1 + correction_row$pm10)
      city_df$so2_modr <- city_df$so2_mod / (1 + correction_row$so2)
      city_df$no2_modr <- city_df$no2_mod / (1 + correction_row$no2)
      
      # Update the city's dataframe in inc_alert with the corrected values
      inc_alert[[alert_level]][[city]] <- city_df
    }
  }
}

plot(inc_alert$Orange$Shijiazhuang$pm25, ylim = c(0,300))
points(inc_alert$Orange$Shijiazhuang$pm25_modr, ylim = c(0,300), col = "red")

# Loop over each alert level
for (alert in alert_levels) {
  
  # Loop over each city in the current alert level
  for (city in names(inc_alert[[alert]])) {
    
    # Loop over each pollutant
    for (pollutant in pollutants) {
      
      # Construct variable names for the pollutant and its modelled counterpart
      pollutant_mod <- paste0(pollutant, "_modr")
      
      # Calculate absolute difference and assign to new column in city data
      abs_diff_col <- paste0("abs_", pollutant, "_dif")
      inc_alert[[alert]][[city]][[abs_diff_col]] <- 
        inc_alert[[alert]][[city]][[pollutant]] - inc_alert[[alert]][[city]][[pollutant_mod]]
      
      # Calculate relative difference and assign to new column in city data
      rlt_diff_col <- paste0("rlt_", pollutant, "_dif")
      inc_alert[[alert]][[city]][[rlt_diff_col]] <- 
        100 * inc_alert[[alert]][[city]][[abs_diff_col]] / inc_alert[[alert]][[city]][[pollutant_mod]]
    }
  }
}
saveRDS(inc_alert, "/Users/yqdai/Downloads/RedAlert/data/chap_era5_daily/aqmet_incalert_predv3.rds")


