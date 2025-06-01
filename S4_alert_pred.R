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

inc_alert <- readRDS("/Users/yqdai/Downloads/RedAlert/data/chap_era5_daily/aqmet_incalert.rds")

inc_alert <- lapply(inc_alert, function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
})

# Pollutant and feature setup
pollutant <- c("pm25", "pm10", "so2", "no2")
x <- c("ws", "wd", "t2m", "sp", "blh", "tp", "trend", "dow", "month", "year")

as_num <- c("ws", "wd", "t2m", "sp", "blh", "tp", "trend", "y", "dow", "month", "year")
as_ftr <- NULL

# Start the H2O cluster
h2o::h2o.init(max_mem_size = "40g")

# Loop through each combination of k, j, and i
for (k in 1:3) {
  for (j in 1:4) {
    y <- pollutant[j]  # Select the pollutant based on j
    
    for (i in 1:length(inc_alert[[k]])) {
      
      # Obtain testing data as a data frame
      testing_data <- as.data.frame(inc_alert[[k]][[i]])
      testing_data_h2o <- h2o::as.h2o(testing_data)
      
      # Load the model and make predictions
      model_path <- paste0("/Volumes/daiyy/2024_paper/RedAlert/output/mod/", y, "/", names(inc_alert[[k]][i]))
      if (file.exists(model_path)) {
        gbm <- h2o.loadModel(model_path)
        predictions <- as.data.frame(h2o.predict(gbm, testing_data_h2o))
        colnames(predictions) <- paste0(y, "_mod")
        
        # Bind predictions to the specific element in inc_alert
        inc_alert[[k]][[i]] <- cbind(inc_alert[[k]][[i]], predictions)
      } else {
        message("Model not found for ", y, " at index ", i, " in group ", k)
      }
    }
  }
}

# `excluded_aqlist` now contains data frames with the specified date ranges removed
saveRDS(inc_alert, "/Volumes/daiyy/2024_paper/RedAlert/data/aqmet_incalert_pred.rds")

