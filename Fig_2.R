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
aqlist <- readRDS("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/era5L/aqmet_incalert.rds")
meta <- read_xlsx("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/meta/meta.xlsx")

aqlist_nonalert <- readRDS("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/era5L/aqmet_excalert.rds")

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

# List of pollutants to plot
# List of pollutants to plot and corresponding y-axis limits
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

# Loop through each pollutant, generate plot, and save
for (i in seq_along(pollutants)) {
  pollutant <- pollutants[[i]]
  y_label <- y_labels[[i]]
  y_limit <- y_limits[[i]]

  # Generate the plot
  plot <- ggplot(combined_data, aes(x = Province, y = .data[[pollutant]], fill = Level)) +
    geom_boxplot(color = "black", outlier.shape = NA, width = 0.6, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("Yellow" = "#FFFF00", "Orange" = "#FFA500", "Red" = "#FF0000")) +
    theme_minimal(base_size = 18, base_family = "Arial") +
    labs(x = "", y = y_label, fill = "Alert Level") +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 32, color = "black"),
      axis.line.x.top = element_line(color = "black"),
      axis.line.y.right = element_line(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    coord_cartesian(ylim = y_limit) +
    geom_vline(xintercept = seq(1.5, length(unique(combined_data$Province)) - 0.5, by = 1),
               color = "grey70", linetype = "dashed")

  # Save the plot
  ggsave(paste0("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_trr/", pollutant, ".tiff"), plot = plot, width = 12, height = 10, units = "in", dpi = 300)
}
########################################################################################################################


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)

rm(list = ls(all = TRUE))

# ----------------------------
# Inputs
# ----------------------------
aqlist <- readRDS("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/era5L/aqmet_incalert.rds")
aqlist_nonalert <- readRDS("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/era5L/aqmet_excalert.rds")
meta <- read_xlsx("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/meta/meta.xlsx")

# # ---- Filter helper: keep Nov 1 -> Mar 31 for every year ----
filter_winter_nov_mar <- function(df) {
  # ensure datetime
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  m <- as.integer(format(df$date, "%m"))
  df[m >= 11 | m <= 3, , drop = FALSE]
}

# Apply to each city dataframe inside aqlist_nonalert
aqlist_nonalert <- lapply(aqlist_nonalert, filter_winter_nov_mar)

# Province order you want to show
legend_order <- c("Beijing", "Tianjin", "Shandong", "Henan", "Hebei", "Shanxi", "Other")
core_provinces <- setdiff(legend_order, "Other")

# ----------------------------
# Helper: city-list -> one combined DF with Province
# ----------------------------
process_city_list <- function(city_list, meta, core_provinces) {
  bind_rows(lapply(names(city_list), function(city_name) {
    
    df <- city_list[[city_name]]
    
    # Ensure datetime
    df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    # Add City and join Province
    df <- df %>%
      mutate(City = city_name) %>%
      left_join(meta %>% select(City, Province), by = "City") %>%
      mutate(
        Province = ifelse(is.na(Province), "Other", Province),
        Province = ifelse(Province %in% core_provinces, Province, "Other")
      )
    
    df
  }))
}

aggregate_province_date <- function(df) {
  df %>%
    group_by(Province, date) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
}

# ----------------------------
# Build combined (alert) + non-alert baseline
# ----------------------------
combined_Yellow  <- process_city_list(aqlist$Yellow,  meta, core_provinces)
combined_Orange  <- process_city_list(aqlist$Orange,  meta, core_provinces)
combined_Red     <- process_city_list(aqlist$Red,     meta, core_provinces)
combined_NonAlrt <- process_city_list(aqlist_nonalert, meta, core_provinces)

yellow_aggregated  <- aggregate_province_date(combined_Yellow)  %>% mutate(Level = "Yellow")
orange_aggregated  <- aggregate_province_date(combined_Orange)  %>% mutate(Level = "Orange")
red_aggregated     <- aggregate_province_date(combined_Red)     %>% mutate(Level = "Red")
nonalert_aggregated<- aggregate_province_date(combined_NonAlrt) %>% mutate(Level = "Non-alert")

combined_data <- bind_rows(
  nonalert_aggregated,
  yellow_aggregated,
  orange_aggregated,
  red_aggregated
)

# Factor ordering
combined_data$Province <- factor(combined_data$Province, levels = legend_order)
combined_data$Level <- factor(combined_data$Level, levels = c("Non-alert", "Yellow", "Orange", "Red"))

# ----------------------------
# Plot settings
# ----------------------------
pollutants <- list("pm25", "no2", "pm10", "so2")
y_labels <- list(
  expression(PM[2.5]~(mu*g~m^-3)),
  expression(NO[2]~(mu*g~m^-3)),
  expression(PM[10]~(mu*g~m^-3)),
  expression(SO[2]~(mu*g~m^-3))
)
y_limits <- list(
  c(0, 300),
  c(0, 100),
  c(0, 300),
  c(0, 60)
)

fill_cols <- c(
  "Non-alert" = "white",
  "Yellow"    = "#FFFF00",
  "Orange"    = "#FFA500",
  "Red"       = "#FF0000"
)

# ----------------------------
# Loop and save
# ----------------------------
for (i in seq_along(pollutants)) {
  
  pollutant <- pollutants[[i]]
  y_label <- y_labels[[i]]
  y_limit <- y_limits[[i]]
  
  p <- ggplot(combined_data, aes(x = Province, y = .data[[pollutant]], fill = Level)) +
    geom_boxplot(
      color = "black",
      outlier.shape = NA,
      width = 0.6,
      position = position_dodge(width = 0.80)
    ) +
    scale_fill_manual(values = fill_cols) +
    guides(fill = guide_legend(override.aes = list(color = "black"))) +  # makes white key visible
    theme_minimal(base_size = 18, base_family = "Arial") +
    labs(x = "", y = y_label, fill = "Alert Level") +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 32, color = "black"),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.key = element_rect(fill = NA, color = NA)  # ← no frame
    ) +
    coord_cartesian(ylim = y_limit) +
    geom_vline(
      xintercept = seq(1.5, nlevels(combined_data$Province) - 0.5, by = 1),
      color = "grey70", linetype = "dashed"
    )
  
  ggsave(
    filename = paste0("/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_trr/", pollutant, "1.tiff"),
    plot = p, width = 12, height = 10, units = "in", dpi = 300
  )
}
