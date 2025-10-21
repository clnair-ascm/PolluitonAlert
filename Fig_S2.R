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
# # Part 1
aqlist <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/chap_era5_daily/aqmet_incalert.rds")
meta <- read_xlsx("/Volumes/daiyy/2024_paper/RedAlert/data/meta.xlsx")
# 
aqlist_processed <- lapply(aqlist, function(city_list) {
  lapply(city_list, function(df) {
    df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    return(df)
  })
})
# 
# # Step 1: Process each aqlist (yellow, orange, red) by converting dates, adding city names, and merging with province info
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
# 
combined_Yellow <- bind_rows(aqlist_with_province$Yellow)
combined_Orange <- bind_rows(aqlist_with_province$Orange)
combined_Red <- bind_rows(aqlist_with_province$Red)

# Step 3: Aggregate by Province and Date
yellow_aggregated <- combined_Yellow %>%
  group_by(City, date, Province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

orange_aggregated <- combined_Orange %>%
  group_by(City, date, Province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

red_aggregated <- combined_Red %>%
  group_by(City, date, Province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
# 
# # Define the legend order for the provinces
yellow_aggregated$City <- factor(yellow_aggregated$City)
orange_aggregated$City <- factor(orange_aggregated$City)
red_aggregated$City <- factor(red_aggregated$City)

# Combine the three datasets into one, with an additional column indicating the alert level
yellow_aggregated$Level <- "Yellow"
orange_aggregated$Level <- "Orange"
red_aggregated$Level <- "Red"

# 
# # Combine into one data frame
combined_data <- bind_rows(yellow_aggregated, orange_aggregated, red_aggregated)
# 
# Ensure Province and Level are factors with the desired order
combined_data$Level <- factor(combined_data$Level, levels = c("Yellow", "Orange", "Red"))

# beijing_data <- combined_data %>%
#   filter(Province == c("Beijing", "Tianjin"))

beijing_data <- combined_data %>%
  filter(Province == "Henan")
# 
# # List of pollutants to plot and corresponding y-axis limits
pollutants <- list("pm25", "no2", "pm10", "so2")
y_labels <- list(
  expression(PM[2.5]~(mu*g~m^-3)),
  expression(NO[2]~(mu*g~m^-3)),
  expression(PM[10]~(mu*g~m^-3)),
  expression(SO[2]~(mu*g~m^-3))
)
y_limits <- list(
  c(0, 300),  # PM2.5 range
  c(0, 100),  # NO2 range
  c(0, 300),  # PM10 range
  c(0, 60)    # SO2 range
)
# 
# # Loop through each pollutant, generate plot, and save
for (i in seq_along(pollutants)) {
# i = 1

  pollutant <- pollutants[[i]]
  y_label <- y_labels[[i]]
  y_limit <- y_limits[[i]]

  # beijing_data_2018 <- beijing_data %>% filter(year == 2019)
  # Generate the plot by City instead of Province
  plot <- ggplot(beijing_data, aes(x = City, y = .data[[pollutant]], fill = Level)) +
    geom_boxplot(color = "black", outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("Yellow" = "#FFFF00", "Orange" = "#FFA500", "Red" = "#FF0000")) +
    theme_minimal(base_size = 18, base_family = "Arial") +
    labs(x = "", y = y_label, fill = "Alert Level") +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 16, color = "black"),
      axis.line.x.top = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    coord_cartesian(ylim = y_limit)
  # plot
#   # Save the plot
    ggsave(paste0("/Volumes/daiyy/2024_paper/RedAlert/fig/", pollutant, "_by_city.tiff"), plot = plot, width = 20, height = 10, units = "in", dpi = 300)
}
