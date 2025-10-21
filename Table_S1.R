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
# # Set environment and read GeoJSON data
# Sys.setenv("SHAPE_RESTORE_SHX" = "YES")
# geo_data <- geojson_read("/Volumes/liub-climate-conflict/Alert/aq/china_board/2024_city.json", what = "sp")
# sf_data <- st_as_sf(geo_data)
# 
# # Load metadata and extract unique city names with province information
# meta <- read_xlsx("/Volumes/daiyy/2024_paper/RedAlert/data/meta.xlsx")
# highlight_cities <- unique(meta$City)
# 
# # Merge province information into sf_data based on city names
# sf_data <- sf_data %>%
#   left_join(meta %>% dplyr::select(City, Province), by = c("ENG_NAME" = "City"))
# 
# # Mark highlighted cities and assign province-based colors
# sf_data$highlight <- ifelse(sf_data$ENG_NAME %in% highlight_cities, "highlight", "normal")
# 
# # Define the legend order for the provinces
# legend_order <- c("Beijing", "Tianjin", "Shandong", "Henan", "Hebei", "Shanxi", "Other")
# 
# # Plot the map with PNAS-style aesthetics, coloring highlighted cities by province
# map_plot <- ggplot(data = sf_data) +
#   geom_sf(aes(fill = ifelse(highlight == "highlight", Province, "Other")), 
#           color = "black", size = 0.2) +   # Outline color with thin border
#   scale_fill_manual(
#     values = c(setNames(scales::hue_pal()(length(unique(sf_data$Province[sf_data$highlight == "highlight"]))), 
#                         unique(sf_data$Province[sf_data$highlight == "highlight"])), 
#                "Other" = "white"),         # Set "Other" (non-highlighted cities) to white
#     limits = legend_order                  # Set the order in the legend
#   ) +
#   ggtitle("Map with Highlighted Cities by Province") +
#   coord_sf() +  # Ensures proper lat/lon scales are shown
#   theme_minimal(base_size = 14) +
#   theme(
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "grey80", size = 0.3), # Add major grid lines for lat/lon
#     axis.title = element_blank(),      # Remove axis titles for cleaner look
#     axis.text = element_text(size = 10),       # Show axis text for lat/lon
#     axis.ticks = element_line(color = "grey50") # Show axis ticks for lat/lon
#   )
# 
# # Save the map as a TIFF file
# ggsave("/Volumes/daiyy/2024_paper/RedAlert/map.tiff", plot = map_plot, width = 10, height = 10, units = "in", dpi = 300)
# 
aqlist <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/chap_era5_daily/aqmet_all.rds")
meta <- read_xlsx("/Volumes/daiyy/2024_paper/RedAlert/data/meta.xlsx")

aqlist <- lapply(aqlist, function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
})

# Step 1: Add city name as a column and merge with meta to get Province info
aqlist_with_province <- lapply(names(aqlist), function(city_name) {
  city_df <- aqlist[[city_name]]
  
  # Add a "City" column to each city_df with the city_name
  city_df <- city_df %>%
    mutate(City = city_name)
  
  # Merge with `meta` to add Province information
  city_with_province <- merge(city_df, meta[, c("City", "Province")], 
                              by = "City", all.x = TRUE)
  
  return(city_with_province)
})

# Step 2: Combine all city DataFrames into one
combined_data <- bind_rows(aqlist_with_province)

# Step 3: Aggregate by Province and Date
province_aggregated <- combined_data %>%
  group_by(Province, date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

# Calculate 7-day rolling average for `no2` by `Province`
province_7d <- province_aggregated %>%
  mutate(date = floor_date(date, "week")) %>%  # Create a week column
  group_by(Province, date) %>%  # Group by Province and week
  summarise(across(c(no2, so2, pm25, pm10, trend, u10, v10, t2m, sp, blh, tp, ws, wd), mean, na.rm = TRUE))

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

no2_plot <- ggplot(province_7d, aes(x = date, y = pm10, color = Province)) +
  geom_line(size = 1.2) +  # Slightly thicker main lines for emphasis
  geom_smooth(aes(group = Province), method = "lm", se = FALSE, linetype = "dashed", size = 1.5) +  # Wider trend lines
  labs(
    title = ""
  ) +
  scale_color_manual(values = province_colors, limits = legend_order) +  # Use consistent colors and legend order
  theme_minimal(base_size = 18, base_family = "Arial") +  # Larger base font for PNAS style
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold", margin = margin(t = 10, b = 10)),
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text = element_text(size = 32),
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Full border for whole-frame look
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # Larger margins for balanced framing
  )
ggsave("/Volumes/daiyy/2024_paper/RedAlert/no2.tiff", plot = no2_plot, width = 12, height = 10, units = "in", dpi = 300)
