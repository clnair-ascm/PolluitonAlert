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
aqlist <- readRDS("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L_obs/aqmet_incalert_pred_cor.rds")
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
  group_by(City, date, Province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

orange_aggregated <- combined_Orange %>%
  group_by(City, date, Province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

red_aggregated <- combined_Red %>%
  group_by(City, date, Province) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

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

dataset_filtered <- combined_data %>%
  select(City, date, Province, pm25, pm25_modr, Level)

# List all objects in the global environment
all_objects <- ls()
keep_objects <- c("dataset_filtered", "meta")
rm(list = setdiff(all_objects, keep_objects))

dataset_filtered <- merge(dataset_filtered, 
                          meta[, c("City", "pop_2018")], 
                          by = "City", 
                          all.x = TRUE)

## # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 867.92
# 671.07
# Define parameters
beta        <- log(1.0065) / 10
zeta        <- 767.90 / (100000 * 365)
zeta_upper  <- 867.92 / (100000 * 365)
zeta_lower  <- 671.07 / (100000 * 365)
theta       <- 2.4
theta_lower <- 0

# Calculate relative risks and mortality estimates in one mutate call
dataset_filtered <- dataset_filtered %>%
  mutate(
    # Calculate relative risks using the simplified exp(-x) form
    RR_cf   = exp(-beta * (pm25_modr - theta)),
    RR_bs   = exp(-beta * (pm25 - theta)),
    RR_cf_0 = exp(-beta * (pm25_modr - theta_lower)),
    RR_bs_0 = exp(-beta * (pm25 - theta_lower)),
    
    # Adjust population value (if needed)
    pop = pop_2018 * 10000,
    
    # Mortality calculations
    mort_cf       = zeta       * pop * (1 - RR_cf),
    mort_bs       = zeta       * pop * (1 - RR_bs),
    mort_cf_upper = zeta_upper * pop * (1 - RR_cf),
    mort_bs_upper = zeta_upper * pop * (1 - RR_bs),
    mort_cf_lower = zeta_lower * pop * (1 - RR_cf),
    mort_bs_lower = zeta_lower * pop * (1 - RR_bs),
    mort_cf_0     = zeta       * pop * (1 - RR_cf_0),
    mort_bs_0     = zeta       * pop * (1 - RR_bs_0)
  )

# Summarize by City
mort_by_city <- dataset_filtered %>%
  group_by(City) %>%
  summarise(
    mort_cf       = sum(mort_cf, na.rm = TRUE),
    mort_bs       = sum(mort_bs, na.rm = TRUE),
    mort_cf_upper = sum(mort_cf_upper, na.rm = TRUE),
    mort_bs_upper = sum(mort_bs_upper, na.rm = TRUE),
    mort_cf_lower = sum(mort_cf_lower, na.rm = TRUE),
    mort_bs_lower = sum(mort_bs_lower, na.rm = TRUE),
    mort_cf_0     = sum(mort_cf_0, na.rm = TRUE),
    mort_bs_0     = sum(mort_bs_0, na.rm = TRUE),
    pop_2018      = first(pop_2018),
    Province      = first(Province)
  )

# Summarize by Province
mort_by_province <- mort_by_city %>%
  group_by(Province) %>%
  summarise(
    mort_cf       = sum(mort_cf, na.rm = TRUE),
    mort_bs       = sum(mort_bs, na.rm = TRUE),
    mort_cf_upper = sum(mort_cf_upper, na.rm = TRUE),
    mort_bs_upper = sum(mort_bs_upper, na.rm = TRUE),
    mort_cf_lower = sum(mort_cf_lower, na.rm = TRUE),
    mort_bs_lower = sum(mort_bs_lower, na.rm = TRUE),
    mort_cf_0     = sum(mort_cf_0, na.rm = TRUE),
    mort_bs_0     = sum(mort_bs_0, na.rm = TRUE),
    pop_2018      = sum(pop_2018, na.rm = TRUE)
  )

# View the results

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)

# Create the data frame
df <- mort_by_province
df$mort <- round(df$mort_cf - df$mort_bs)
df$mort_upper <- round(df$mort_cf_upper - df$mort_bs_upper)
df$mort_lower <- round(df$mort_cf_lower - df$mort_bs_lower)

# Define the desired order of provinces
province_order <- c("Shanxi", "Hebei", "Henan", "Shandong", "Tianjin", "Beijing")

# Convert Province to a factor with the specified order
df$Province <- factor(df$Province, levels = province_order)

# Compute a left-side x-position for the population points.
left_pos <- min(c(df$mort_bs, df$mort_cf)) - 0.1 * diff(range(c(df$mort_bs, df$mort_cf)))

test <- ggplot(df, aes(y = Province)) +
  # Add vertical dashed line at x = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  # Add population points on the left side, mapping both size and shape to indicate Population.
  # Note that we set shape to a constant value "Population".
  geom_point(aes(x = left_pos, size = pop_2018, shape = "Population"),
             fill = "gray70", color = "black", stroke = 0.5) +
  
  # Your original layers for segments and points
  geom_segment(
    aes(x = mort_bs, xend = mort_cf, y = Province, yend = Province),
    color = "grey50", linewidth = 1, 
    position = position_nudge(y = 0.15)
  ) +
  geom_point(
    aes(x = mort_bs, color = "Observed"), 
    size = 4, position = position_nudge(y = 0.15)
  ) +
  geom_point(
    aes(x = mort_cf, color = "Counterfactual"), 
    size = 4, position = position_nudge(y = 0.15)
  ) +
  # Add mortality numbers below the segments, with dynamic positioning
  geom_text(
    aes(
      x = ifelse(mort_cf < 4000, mort_cf + 8000, (mort_bs + mort_cf) / 2),
      y = Province, 
      label = paste0(mort, " [", mort_lower, ", ", mort_upper, "]")
    ),
    position = position_nudge(y = -0.3),
    size = 3.5, color = "black", vjust = 0.5
  ) +
  # Define color scale for Observed and Counterfactual
  scale_color_manual(
    values = c("Observed" = "#0072B2", "Counterfactual" = "#D55E00"),
    guide = guide_legend(title = NULL, override.aes = list(size = 3))
  ) +
  # Remove the shape legend so that only the size legend appears for Population.
  scale_shape_manual(name = "Population (2018)", values = c("Population" = 21), guide = "none") +
  # Define a continuous size scale for the population with custom breaks and labels.
  # Here, we override the default legend appearance to match the point's fill, color, and shape.
  scale_size_continuous(
    name = "Population", 
    breaks = c(min(df$pop_2018), median(df$pop_2018), max(df$pop_2018)),
    labels = c("Low", "Medium", "High"),
    range = c(2, 6),
    guide = guide_legend(override.aes = list(fill = "gray70", color = "black", shape = 21))
  ) +
  labs(
    x = "Mortality count", 
    y = NULL,
    color = ""
  ) +
  theme_classic(base_size = 10) +
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 5)),
    axis.line = element_line(linewidth = 0.5),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(1, "mm"),
    legend.position = c(0.9, 0.7),
    legend.key.height = unit(4, "mm"),
    legend.background = element_blank(),
    legend.text = element_text(
      family = "Helvetica",  
      size = 9,             
      color = "black"       
    ),
    plot.margin = unit(c(2, 10, 2, 2), "mm")
  )

test
head(df)
ggsave("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/fig4.tiff", plot = test, width = 5, height = 4, units = "in", dpi = 300)

df_test <- df %>%
  select(Province, mort, mort_bs, mort_cf)
