# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(scales)

rm(list = ls(all = T))
### MAP !!!
setwd("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L_SI/mod_stat/")

# Set the directory path where CSV files are located
file_path <- "/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L_obs/mod_stat/pm10/" # Replace with your actual path

# List all CSV files with "_stat" in the name
file_list <- list.files(path = file_path, pattern = "_stat\\.csv$", full.names = TRUE)

# Read each file, add a City column, and bind them together
data_list <- lapply(file_list, function(file) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Extract the city name from the file name
  city_name <- sub("_stat\\.csv$", "", basename(file))
  
  # Add City column
  data$City <- city_name
  
  return(data)
})

# Bind all data frames together
final_data <- do.call(rbind, data_list)

# View the combined data
# write.csv(final_data, "/Volumes/daiyy/2024_paper/RedAlert/output/mod_stat/no2.csv")
