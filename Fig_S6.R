# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(zoo)
library(lubridate)
library(RColorBrewer)
library(scales)

rm(list = ls(all = T))

aqlist <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/chap_era5_daily/aqmet_incalert_SI_predv2.rds")
meta <- read_xlsx("/Volumes/daiyy/2024_paper/RedAlert/data/meta.xlsx")

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
combined_Orange <- bind_rows(aqlist_with_province$Orange)

# Step 3: Aggregate by Province and Date
orange_aggregated <- combined_Orange %>%
  group_by(City, date) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Define the legend order for the provinces
legend_order <- c("Handan", "Xingtai",  "Baoding", "Cangzhou","Langfang")

# Ensure all provinces in legend_order are factors in the desired order
orange_aggregated$City <- factor(orange_aggregated$City, levels = legend_order)

# Combine the three datasets into one, with an additional column indicating the alert level
orange_aggregated$Level <- "Orange"

# Combine into one data frame
combined_data <- bind_rows(orange_aggregated)

# Ensure Province and Level are factors with the desired order
combined_data$City <- factor(combined_data$City, levels = legend_order)
combined_data$Level <- factor(combined_data$Level, levels = "Orange")

# Calculate mean and standard error for each variable by Level and Province
summary_data <- combined_data %>%
  group_by(Level, City) %>%
  summarise(
    avg_abs_pm25_dif = mean(abs_pm25_dif, na.rm = TRUE),
    se_abs_pm25_dif = sd(abs_pm25_dif, na.rm = TRUE) / sqrt(n()),
    avg_abs_pm10_dif = mean(abs_pm10_dif, na.rm = TRUE),
    se_abs_pm10_dif = sd(abs_pm10_dif, na.rm = TRUE) / sqrt(n()),
    avg_abs_so2_dif = mean(abs_so2_dif, na.rm = TRUE),
    se_abs_so2_dif = sd(abs_so2_dif, na.rm = TRUE) / sqrt(n()),
    avg_abs_no2_dif = mean(abs_no2_dif, na.rm = TRUE),
    se_abs_no2_dif = sd(abs_no2_dif, na.rm = TRUE) / sqrt(n()),
    avg_rlt_pm25_dif = mean(rlt_pm25_dif, na.rm = TRUE),
    se_rlt_pm25_dif = sd(rlt_pm25_dif, na.rm = TRUE) / sqrt(n()),
    avg_rlt_pm10_dif = mean(rlt_pm10_dif, na.rm = TRUE),
    se_rlt_pm10_dif = sd(rlt_pm10_dif, na.rm = TRUE) / sqrt(n()),
    avg_rlt_so2_dif = mean(rlt_so2_dif, na.rm = TRUE),
    se_rlt_so2_dif = sd(rlt_so2_dif, na.rm = TRUE) / sqrt(n()),
    avg_rlt_no2_dif = mean(rlt_no2_dif, na.rm = TRUE),
    se_rlt_no2_dif = sd(rlt_no2_dif, na.rm = TRUE) / sqrt(n())
  )
# List of pollutants to plot
# # List of pollutants to plot and corresponding y-axis limits
# pollutants <- list("avg_abs_pm25_dif", "avg_abs_pm10_dif", "avg_abs_no2_dif", "avg_abs_so2_dif")
# errors <- list("se_abs_pm25_dif", "se_abs_pm10_dif", "se_abs_no2_dif", "se_abs_so2_dif")
# 
# y_labels <- list(
#   expression(PM[2.5]~(mu*g~m^-3)),
#   expression(NO[2]~(mu*g~m^-3)),
#   expression(PM[10]~(mu*g~m^-3)),
#   expression(SO[2]~(mu*g~m^-3))
# )
# y_limits <- list(
#   c(-5, 50),  # PM2.5 range
#   c(-5, 100),  # NO2 range
#   c(-5, 20) ,  # PM10 range
#   c(-5, 10)    # SO2 range
# )
# 
# i = 1
# # Loop through each pollutant, generate plot, and save
# for (i in seq_along(pollutants)) {
#   pollutant <- pollutants[[i]]
#   error <- errors[[i]]
#   y_label <- y_labels[[i]]
#   y_limit <- y_limits[[i]]
#   
#   plot <- ggplot(summary_data, aes(x = .data[[pollutant]], y = Province, fill = Level)) +
#     geom_errorbarh(aes(xmin = .data[[pollutant]] - (.data[[error]]), 
#                        xmax = .data[[pollutant]] + (.data[[error]])),
#                    height = 0.2, color = "black", size = 1.2) +  # Bolder, wider error bars
#     geom_point(color = "black", size = 10, shape = 21) +  # Larger mean dot
#     geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.2) +  # Bolder vertical line
#     scale_fill_manual(values = c("Yellow" = "#FFFF00", "Orange" = "#FFA500", "Red" = "#FF0000")) +
#     theme_minimal(base_size = 18, base_family = "Arial") +
#     labs(x = y_label, y = "", fill = "Alert Level") +
#     theme(
#       axis.title = element_text(size = 16, face = "bold"),
#       axis.text = element_text(size = 32, color = "black"),
#       axis.line.x.top = element_line(color = "black"),
#       axis.line.y.right = element_line(color = "black"),
#       panel.border = element_rect(color = "black", fill = NA, size = 1),
#       panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
#       panel.grid = element_blank(),
#       legend.position = "top",
#       legend.title = element_text(size = 14),
#       legend.text = element_text(size = 12)
#     ) +
#     coord_cartesian(xlim = y_limit) +
#     geom_hline(yintercept = seq(1.5, length(unique(summary_data$Province)) - 0.5, by = 1), 
#                color = "grey70", linetype = "dashed")
#   
# # Save the plot
#   ggsave(paste0("/Volumes/daiyy/2024_paper/RedAlert/fig/", pollutant, ".tiff"), plot = plot, width = 12, height = 10, units = "in", dpi = 300)
# }

# List of pollutants to plot and corresponding y-axis limits
pollutants <- list("avg_abs_pm25_dif", "avg_abs_pm10_dif", "avg_abs_no2_dif", "avg_abs_so2_dif")
errors <- list("se_abs_pm25_dif", "se_abs_pm10_dif", "se_abs_no2_dif", "se_abs_so2_dif")

y_labels <- list(
  expression(PM[2.5]~(mu*g~m^-3)),
  expression(NO[2]~(mu*g~m^-3)),
  expression(PM[10]~(mu*g~m^-3)),
  expression(SO[2]~(mu*g~m^-3))
)
y_limits <- list(
  c(-5, 100),  # PM2.5 range
  c(-5, 100),  # NO2 range
  c(-20, 50) ,  # PM10 range
  c(-20, 50)    # SO2 range
)

i = 1
# Loop through each pollutant, generate plot, and save
for (i in seq_along(pollutants)) {
  pollutant <- pollutants[[i]]
  error <- errors[[i]]
  y_label <- y_labels[[i]]
  y_limit <- y_limits[[i]]
  
  plot <- ggplot(summary_data, aes(x = .data[[pollutant]], y = City, fill = Level)) +
    geom_errorbarh(aes(xmin = .data[[pollutant]] - (.data[[error]]), 
                       xmax = .data[[pollutant]] + (.data[[error]])),
                   height = 0.2, color = "black", size = 1.2) +  # Bolder, wider error bars
    geom_point(color = "black", size = 10, shape = 21) +  # Larger mean dot
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.2) +  # Bolder vertical line
    scale_fill_manual(values = c("Yellow" = "#FFFF00", "Orange" = "#FFA500", "Red" = "#FF0000")) +
    theme_minimal(base_size = 18, base_family = "Arial") +
    labs(x = y_label, y = "", fill = "Alert Level") +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 32, color = "black"),
      axis.line.x.top = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    coord_cartesian(xlim = y_limit) +
    geom_hline(yintercept = seq(1.5, length(unique(summary_data$City)) - 0.5, by = 1), 
               color = "grey70", linetype = "dashed")
  
  # Save the plot
  ggsave(paste0("/Volumes/daiyy/2024_paper/RedAlert/", pollutant, ".tiff"), plot = plot, width = 12, height = 10, units = "in", dpi = 100)
  # ggsave(paste0("/Volumes/daiyy/2024_paper/RedAlert/", pollutant, ".tiff"), plot = plot, width = 12, height = 10, units = "in", dpi = 300)
}


