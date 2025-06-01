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

aqlist <- readRDS("/Users/yqdai/Downloads/RedAlert/data/chap_senti/aqmet_def_events1314_pred.rds")

pollutants <- c("no2", "so2", "pm25", "pm10")
pollutants_mod1 <- c("no2", "so2", "pm25", "pm10")
pollutants_mod3 <- c("no2_mod3", "so2_mod3", "pm25_mod3", "pm10_mod3")

# Initialize data frames to store the results
obs <- data.frame(City = character(), 
                  SO2_mean = numeric(), SO2_sd = numeric(),
                  NO2_mean = numeric(), NO2_sd = numeric(),
                  PM25_mean = numeric(), PM25_sd = numeric(),
                  PM10_mean = numeric(), PM10_sd = numeric(),
                  stringsAsFactors = FALSE)

mod3 <- data.frame(City = character(), 
                   SO2_mean = numeric(), SO2_sd = numeric(),
                   NO2_mean = numeric(), NO2_sd = numeric(),
                   PM25_mean = numeric(), PM25_sd = numeric(),
                   PM10_mean = numeric(), PM10_sd = numeric(),
                   stringsAsFactors = FALSE)

# Loop through each city in aqlist
for (city_name in names(aqlist)) {
  # Extract the data for the current city
  city_data <- aqlist[[city_name]]
  
  # Calculate the mean and standard deviation for each observed pollutant
  so2_mean <- mean(city_data$so2, na.rm = TRUE)
  so2_sd <- sd(city_data$so2, na.rm = TRUE)
  
  no2_mean <- mean(city_data$no2, na.rm = TRUE)
  no2_sd <- sd(city_data$no2, na.rm = TRUE)
  
  pm25_mean <- mean(city_data$pm25, na.rm = TRUE)
  pm25_sd <- sd(city_data$pm25, na.rm = TRUE)
  
  pm10_mean <- mean(city_data$pm10, na.rm = TRUE)
  pm10_sd <- sd(city_data$pm10, na.rm = TRUE)
  
  # Append the results to the observed data frame
  obs <- rbind(obs, data.frame(City = city_name,
                               SO2_mean = so2_mean, SO2_sd = so2_sd,
                               NO2_mean = no2_mean, NO2_sd = no2_sd,
                               PM25_mean = pm25_mean, PM25_sd = pm25_sd,
                               PM10_mean = pm10_mean, PM10_sd = pm10_sd))
  
  # Calculate mean and standard deviation for each mod1 pollutant
  
  # Calculate mean and standard deviation for each mod3 pollutant
  so2_mod3_mean <- mean(city_data$so2_mod3, na.rm = TRUE)
  so2_mod3_sd <- sd(city_data$so2_mod3, na.rm = TRUE)
  
  no2_mod3_mean <- mean(city_data$no2_mod3, na.rm = TRUE)
  no2_mod3_sd <- sd(city_data$no2_mod3, na.rm = TRUE)
  
  pm25_mod3_mean <- mean(city_data$pm25_mod3, na.rm = TRUE)
  pm25_mod3_sd <- sd(city_data$pm25_mod3, na.rm = TRUE)
  
  pm10_mod3_mean <- mean(city_data$pm10_mod3, na.rm = TRUE)
  pm10_mod3_sd <- sd(city_data$pm10_mod3, na.rm = TRUE)
  
  # Append the results to the mod3 data frame
  mod3 <- rbind(mod3, data.frame(City = city_name,
                                 SO2_mean = so2_mod3_mean, SO2_sd = so2_mod3_sd,
                                 NO2_mean = no2_mod3_mean, NO2_sd = no2_mod3_sd,
                                 PM25_mean = pm25_mod3_mean, PM25_sd = pm25_mod3_sd,
                                 PM10_mean = pm10_mod3_mean, PM10_sd = pm10_mod3_sd))
}

# Display the results
obs
mod3

# Load required libraries
library(ggplot2)
library(dplyr)

# Add a new column to identify the data source and combine dataframes
obs$source <- "Observed"
mod3$source <- "Modeled 3"

# Combine all dataframes
combined_data <- bind_rows(obs, mod3)

# Plot with different colors for each data source
plot <- ggplot(combined_data, aes(x = City, y = SO2_mean, color = source)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = SO2_mean - SO2_sd, ymax = SO2_mean + SO2_sd), 
                position = position_dodge(width = 0.3), width = 0.2) +
  scale_color_manual(values = c("Observed" = "red", "Modeled 3" = "blue")) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 20, family = "sans"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  labs(x = "", y = "", 
       title = "",
       color = "Data Source")

ggsave(paste0("/Users/yqdai/Downloads/RedAlert/test_SO2.tiff"), plot = plot, width = 20, height = 5, units = "in", dpi = 300)

# Convert non-City columns to numeric in both mod1 and mod2
obs[,-1] <- lapply(obs[,-1], as.numeric)
mod3[,-1] <- lapply(mod3[,-1], as.numeric)
# Calculate the correction factor as (mod2 - mod1) / mod1 for each pollutant
factor_df <- obs
factor_df[,-1] <- (mod3[,-1] - obs[,-1]) / obs[,-1]
names(factor_df)[-1] <- paste0(names(factor_df)[-1], "_factor")

# Display the resulting dataframe
factor_df
write.csv(factor_df, paste0("/Users/yqdai/Downloads/RedAlert/pred_factor_mod3.csv"))
