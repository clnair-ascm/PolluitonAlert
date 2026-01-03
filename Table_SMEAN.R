# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(scales)

rm(list = ls(all = T))
########################################################################################################################

aqlist <- readRDS("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/aqmet_incalert_pred_cor.rds")
meta <- read_xlsx("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/meta/meta.xlsx")

aqlist_processed <- lapply(aqlist, function(city_list) {
  lapply(city_list, function(df) {
    df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    return(df)
  })
})

# Step 1: Process each aqlist (yellow, orange, red) by converting dates, adding city names, and merging with province info
aqlist_with_province <- lapply(aqlist_processed, function(city_list) {
  lapply(names(city_list), function(city_name) {
    city_df <- city_list[[city_name]]
    
    # Convert the date column to POSIXct format
    city_df$date <- as.POSIXct(city_df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # Add a "City" column with the city_name
    city_df <- city_df %>%
      mutate(City = city_name)
    
    # Merge with `meta` to add Province information
    city_with_province <- merge(city_df, meta[, c("City", "Province")], 
                                by = "City", all.x = TRUE)
    
    return(city_with_province)
  })
})

# Step 2: Combine all city DataFrames into one
combined_Yellow <- bind_rows(aqlist_with_province$Yellow)
combined_Orange <- bind_rows(aqlist_with_province$Orange)
combined_Red <- bind_rows(aqlist_with_province$Red)

cleaned_Yellow <- combined_Yellow %>%
  group_by(City) %>%
  filter(
    # for every column of interest, keep only values within [Q1 - 1.5 IQR, Q3 + 1.5 IQR]
    across(
      .cols = c(abs_pm25_dif, abs_pm10_dif, abs_no2_dif, abs_so2_dif),
      .fns = ~ {
        qnts <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr  <- diff(qnts)
        between(
          .x,
          qnts[1] - 1.5 * iqr,
          qnts[2] + 1.5 * iqr
        )
      }
    )
  ) %>%
  ungroup()

cleaned_Orange <- combined_Orange %>%
  group_by(City) %>%
  filter(
    # for every column of interest, keep only values within [Q1 - 1.5 IQR, Q3 + 1.5 IQR]
    across(
      .cols = c(abs_pm25_dif, abs_pm10_dif, abs_no2_dif, abs_so2_dif),
      .fns = ~ {
        qnts <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr  <- diff(qnts)
        between(
          .x,
          qnts[1] - 1.5 * iqr,
          qnts[2] + 1.5 * iqr
        )
      }
    )
  ) %>%
  ungroup()

cleaned_Red <- combined_Red %>%
  group_by(City) %>%
  filter(
    # for every column of interest, keep only values within [Q1 - 1.5 IQR, Q3 + 1.5 IQR]
    across(
      .cols = c(abs_pm25_dif, abs_pm10_dif, abs_no2_dif, abs_so2_dif),
      .fns = ~ {
        qnts <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr  <- diff(qnts)
        between(
          .x,
          qnts[1] - 1.5 * iqr,
          qnts[2] + 1.5 * iqr
        )
      }
    )
  ) %>%
  ungroup()
# Step 3: Aggregate by Province and Date
yellow_aggregated <- cleaned_Yellow %>%
  group_by(City, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

orange_aggregated <- cleaned_Orange %>%
  group_by(City, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

red_aggregated <- cleaned_Red %>%
  group_by(City, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
# Define the legend order for the provinces
# Ensure all provinces in legend_order are factors in the desired order
yellow_aggregated$City <- factor(yellow_aggregated$City)
orange_aggregated$City <- factor(orange_aggregated$City)
red_aggregated$City <- factor(red_aggregated$City)

# Combine the three datasets into one, with an additional column indicating the alert level
yellow_aggregated$Level <- "Yellow"
orange_aggregated$Level <- "Orange"
red_aggregated$Level <- "Red"

# Combine into one data frame
combined_data <- bind_rows(yellow_aggregated, orange_aggregated, red_aggregated)

# Ensure Province and Level are factors with the desired order
combined_data$Level <- factor(combined_data$Level, levels = c("Yellow", "Orange", "Red"))

# Calculate mean and standard error for each variable by Level and Province
summary_data <- combined_data %>%
  group_by(Level, City) %>%
  summarise(
    avg_abs_pm25_dif = mean(abs_pm25_dif, na.rm = TRUE),
    avg_abs_pm10_dif = mean(abs_pm10_dif, na.rm = TRUE),
    avg_abs_so2_dif = mean(abs_so2_dif, na.rm = TRUE),
    avg_abs_no2_dif = mean(abs_no2_dif, na.rm = TRUE)
  )
# List of pollutants to plot
library(dplyr)
library(tidyr)
# --- Ensure clean Level names (capitalize consistently)
summary_data <- summary_data %>%
  mutate(Level = factor(Level, levels = c("Yellow", "Orange", "Red")))

# --- Pivot to wide format
tidy_summary <- summary_data %>%
  select(City, Level,
         PM25 = avg_abs_pm25_dif,
         PM10 = avg_abs_pm10_dif,
         SO2  = avg_abs_so2_dif,
         NO2  = avg_abs_no2_dif) %>%
  pivot_longer(cols = c(PM25, PM10, SO2, NO2),
               names_to = "Pollutant",
               values_to = "Value") %>%
  pivot_wider(names_from = c(Pollutant, Level), values_from = Value) %>%
  arrange(City)

# --- (optional) Reorder columns for readability
tidy_summary <- tidy_summary %>%
  select(City,
         starts_with("PM25_"),
         starts_with("PM10_"),
         starts_with("NO2_"),
         starts_with("SO2_"))

# --- View output
print(tidy_summary)
