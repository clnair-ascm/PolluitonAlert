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

# Step 3: Aggregate by Province and Date
yellow_aggregated <- combined_Yellow %>%
  group_by(Province, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

orange_aggregated <- combined_Orange %>%
  group_by(Province, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

red_aggregated <- combined_Red %>%
  group_by(Province, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Define the legend order for the provinces
legend_order <- c("Beijing", "Tianjin", "Shandong", "Henan", "Hebei", "Shanxi", "Other")
legend_order <- c("Other", "Shanxi",  "Hebei", "Henan","Shandong", "Tianjin", "Beijing")

# The colors are reordered to match the specific assignment you requested
province_colors <- c(
  "Beijing" = hue_pal()(6)[3],    # Shandong's color
  "Tianjin" = hue_pal()(6)[6],    # Shanxi's color
  "Shandong" = hue_pal()(6)[4],   # Henan's color
  "Henan" = hue_pal()(6)[1],      # Beijing's color
  "Hebei" = hue_pal()(6)[2],      # Tianjin's color
  "Shanxi" = hue_pal()(6)[5],     # Hebei's color
  "Other" = "white"               # Keep "Other" as white
)

# Ensure all provinces in legend_order are factors in the desired order
yellow_aggregated$Province <- factor(yellow_aggregated$Province, levels = legend_order)
orange_aggregated$Province <- factor(orange_aggregated$Province, levels = legend_order)
red_aggregated$Province <- factor(red_aggregated$Province, levels = legend_order)

# Combine the three datasets into one, with an additional column indicating the alert level
yellow_aggregated$Level <- "Yellow"
orange_aggregated$Level <- "Orange"
red_aggregated$Level <- "Red"

# Combine into one data frame
combined_data <- bind_rows(yellow_aggregated, orange_aggregated, red_aggregated)

# Ensure Province and Level are factors with the desired order
combined_data$Province <- factor(combined_data$Province, levels = legend_order)
combined_data$Level <- factor(combined_data$Level, levels = c("Yellow", "Orange", "Red"))

# Calculate mean and standard error for each variable by Level and Province
summary_data <- combined_data %>%
  group_by(Level, Province) %>%
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
pollutants <- list("avg_rlt_pm25_dif", "avg_rlt_pm10_dif", "avg_rlt_no2_dif", "avg_rlt_so2_dif")
errors <- list("se_rlt_pm25_dif", "se_rlt_pm10_dif", "se_rlt_no2_dif", "se_rlt_so2_dif")

# Correct order of labels
y_labels <- list(
  expression(PM[2.5]~(mu*g~m^-3)),
  expression(PM[10]~(mu*g~m^-3)),
  expression(NO[2]~(mu*g~m^-3)),
  expression(SO[2]~(mu*g~m^-3))
)

y_limits <- list(
  c(-50, 50),  # PM2.5 range
  c(-50, 50),  # NO2 range
  c(-30, 10) ,  # PM10 range
  c(-30, 10)    # SO2 range
)
# y_limits <- list(
#    c(-80, 50),  # PM2.5 range
#    c(-80, 50),  # NO2 range
#    c(-20, 20) ,  # PM10 range
#    c(-20, 20)    # SO2 range
#  )

i = 1
# Loop through each pollutant, generate plot, and save
for (i in seq_along(pollutants)) {
  pollutant <- pollutants[[i]]
  error <- errors[[i]]
  y_label <- y_labels[[i]]
  y_limit <- y_limits[[i]]
  
  plot <- ggplot(summary_data, aes(x = .data[[pollutant]], y = Province, fill = Level)) +
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
    geom_hline(yintercept = seq(1.5, length(unique(summary_data$Province)) - 0.5, by = 1), 
               color = "grey70", linetype = "dashed")
  
# Save the plot
  ggsave(paste0("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/", pollutant, "11.tiff"), plot = plot, width = 12, height = 10, units = "in", dpi = 300)
}



#   group_by(City, date) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
# 
# orange_aggregated <- combined_Orange %>%
#   group_by(City, date) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
# 
# red_aggregated <- combined_Red %>%
#   group_by(City, date) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
# 
# # Combine the three datasets into one, with an additional column indicating the alert level
# yellow_aggregated$Level <- "Yellow"
# orange_aggregated$Level <- "Orange"
# red_aggregated$Level <- "Red"
# 
# # Combine into one data frame
# combined_data <- bind_rows(yellow_aggregated, orange_aggregated, red_aggregated)
# 
# # List of pollutants to plot and corresponding y-axis limits
# pollutants <- list("pm25", "no2", "pm10", "so2")
# y_labels <- list(
#   expression(PM[2.5]~(mu*g~m^-3)),
#   expression(NO[2]~(mu*g~m^-3)),
#   expression(PM[10]~(mu*g~m^-3)),
#   expression(SO[2]~(mu*g~m^-3))
# )
# y_limits <- list(
#   c(0, 300),  # PM2.5 range
#   c(0, 100),  # NO2 range
#   c(0, 300),  # PM10 range
#   c(0, 60)    # SO2 range
# )
# 
# # Loop through each pollutant, generate plot, and save
# for (i in seq_along(pollutants)) {
#   pollutant <- pollutants[[i]]
#   y_label <- y_labels[[i]]
#   y_limit <- y_limits[[i]]
#   
#   # Generate the plot by City instead of Province
#   plot <- ggplot(combined_data, aes(x = City, y = .data[[pollutant]], fill = Level)) +
#     geom_boxplot(color = "black", outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.75)) +
#     scale_fill_manual(values = c("Yellow" = "#FFFF00", "Orange" = "#FFA500", "Red" = "#FF0000")) +
#     theme_minimal(base_size = 18, base_family = "Arial") +
#     labs(x = "", y = y_label, fill = "Alert Level") +
#     theme(
#       axis.title = element_text(size = 16, face = "bold"),
#       axis.text = element_text(size = 24, color = "black"),
#       axis.line.x.top = element_line(color = "black"),
#       axis.line.y.right = element_line(color = "black"),
#       panel.border = element_rect(color = "black", fill = NA, size = 1),
#       panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
#       panel.grid = element_blank(),
#       legend.position = "top",
#       legend.title = element_text(size = 14),
#       legend.text = element_text(size = 12)
#     ) +
#     coord_cartesian(ylim = y_limit)
#   
#   # Save the plot
#   ggsave(paste0("/Volumes/daiyy/2024_paper/RedAlert/fig/", pollutant, "_by_city.tiff"), plot = plot, width = 40, height = 10, units = "in", dpi = 300)
# }
