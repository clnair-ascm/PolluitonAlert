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

aqlist <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/chap_senti/aqmet_def_events1516_predv3.rds")

pollutants <- c("no2", "so2", "pm25", "pm10")
pollutants_mod4 <- c("no2_mod4", "so2_mod4", "pm25_mod4", "pm10_mod4")
pollutants_modr <- c("no2_modr", "so2_modr", "pm25_modr", "pm10_modr")

# Initialize data frames to store the results
obs <- data.frame(City = character(), 
                  SO2_mean = numeric(), SO2_sd = numeric(),
                  NO2_mean = numeric(), NO2_sd = numeric(),
                  PM25_mean = numeric(), PM25_sd = numeric(),
                  PM10_mean = numeric(), PM10_sd = numeric(),
                  stringsAsFactors = FALSE)

mod4 <- data.frame(City = character(), 
                   SO2_mean = numeric(), SO2_sd = numeric(),
                   NO2_mean = numeric(), NO2_sd = numeric(),
                   PM25_mean = numeric(), PM25_sd = numeric(),
                   PM10_mean = numeric(), PM10_sd = numeric(),
                   stringsAsFactors = FALSE)

modr <- data.frame(City = character(), 
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
  so2_mod4_mean <- mean(city_data$so2_mod4, na.rm = TRUE)
  so2_mod4_sd <- sd(city_data$so2_mod4, na.rm = TRUE)
  
  no2_mod4_mean <- mean(city_data$no2_mod4, na.rm = TRUE)
  no2_mod4_sd <- sd(city_data$no2_mod4, na.rm = TRUE)
  
  pm25_mod4_mean <- mean(city_data$pm25_mod4, na.rm = TRUE)
  pm25_mod4_sd <- sd(city_data$pm25_mod4, na.rm = TRUE)
  
  pm10_mod4_mean <- mean(city_data$pm10_mod4, na.rm = TRUE)
  pm10_mod4_sd <- sd(city_data$pm10_mod4, na.rm = TRUE)
  
  # Append the results to the mod4 data frame
  mod4 <- rbind(mod4, data.frame(City = city_name,
                                 SO2_mean = so2_mod4_mean, SO2_sd = so2_mod4_sd,
                                 NO2_mean = no2_mod4_mean, NO2_sd = no2_mod4_sd,
                                 PM25_mean = pm25_mod4_mean, PM25_sd = pm25_mod4_sd,
                                 PM10_mean = pm10_mod4_mean, PM10_sd = pm10_mod4_sd))
  
  ######
  # Calculate mean and standard deviation for each mod3 pollutant
  so2_modr_mean <- mean(city_data$so2_modr, na.rm = TRUE)
  so2_modr_sd <- sd(city_data$so2_modr, na.rm = TRUE)
  
  no2_modr_mean <- mean(city_data$no2_modr, na.rm = TRUE)
  no2_modr_sd <- sd(city_data$no2_modr, na.rm = TRUE)
  
  pm25_modr_mean <- mean(city_data$pm25_modr, na.rm = TRUE)
  pm25_modr_sd <- sd(city_data$pm25_modr, na.rm = TRUE)
  
  pm10_modr_mean <- mean(city_data$pm10_modr, na.rm = TRUE)
  pm10_modr_sd <- sd(city_data$pm10_modr, na.rm = TRUE)
  
  # Append the results to the modr data frame
  modr <- rbind(modr, data.frame(City = city_name,
                                 SO2_mean = so2_modr_mean, SO2_sd = so2_modr_sd,
                                 NO2_mean = no2_modr_mean, NO2_sd = no2_modr_sd,
                                 PM25_mean = pm25_modr_mean, PM25_sd = pm25_modr_sd,
                                 PM10_mean = pm10_modr_mean, PM10_sd = pm10_modr_sd))
}

# Display the results
obs
mod4

# Load required libraries
library(ggplot2)
library(dplyr)

# Add a new column to identify the data source and combine dataframes
obs$source <- "Observed"
mod4$source <- "Modeled 4"
modr$source <- "Modeled R"

# Combine all dataframes
combined_data <- bind_rows(obs, mod4, modr)

# Plot with different colors for each data source
plot <- ggplot(combined_data, aes(x = City, y = PM10_mean, color = source)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = PM10_mean - PM10_sd, 
                    ymax = PM10_mean + PM10_sd), 
                position = position_dodge(width = 0.3), width = 0.2) +
  scale_color_manual(values = c("Observed" = "red", "Modeled 4" = "blue", "Modeled R" = "green3")) +
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

plot

ggsave(paste0("/Volumes/daiyy/2024_paper/RedAlert/test_PM10.tiff"), plot = plot, width = 20, height = 5, units = "in", dpi = 300)

