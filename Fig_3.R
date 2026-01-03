# ============================================================
# Alert vs Non-alert "control" (RELATIVE anomaly) plots
# Adds Non-alert (white) mean ± SE alongside Yellow/Orange/Red
# ============================================================

library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(scales)

rm(list = ls(all = TRUE))

# -----------------------------
# Paths
# -----------------------------
alert_rds    <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/era5L/aqmet_incalert_pred_cor.rds"
nonalert_rds <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_trr/aqmet_excalert_pred.rds"
meta_xlsx    <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/meta/meta.xlsx"
out_dir      <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_trr"

# -----------------------------
# Read data
# -----------------------------
aqlist     <- readRDS(alert_rds)      # list: Yellow/Orange/Red -> city -> df
aqlist_non <- readRDS(nonalert_rds)   # list: city -> df
meta <- read_xlsx(meta_xlsx) %>% select(City, Province)

# -----------------------------
# Province display order
# -----------------------------
legend_order   <- c("Other", "Shanxi", "Hebei", "Henan", "Shandong", "Tianjin", "Beijing")
core_provinces <- setdiff(legend_order, "Other")

# -----------------------------
# Filtering settings (recommended for fair "control")
# -----------------------------
USE_WINTER_ONLY <- F  # Nov–Mar
YEAR_MIN <- 2018
YEAR_MAX <- 2022

# -----------------------------
# Helpers
# -----------------------------
to_posix <- function(df) {
  if (!("date" %in% names(df))) stop("Missing 'date' column.")
  if (!inherits(df$date, "POSIXct")) {
    df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  df
}

filter_winter <- function(df) {
  m <- lubridate::month(df$date)
  df[m >= 11 | m <= 3, , drop = FALSE]
}

filter_years <- function(df, y1, y2) {
  yy <- lubridate::year(df$date)
  df[yy >= y1 & yy <= y2, , drop = FALSE]
}

# Convert a named list of city dataframes into one dataframe with City + Province
add_city_province <- function(city_list, meta, core_provinces,
                              use_winter_only = TRUE, year_min = 2018, year_max = 2022) {
  bind_rows(lapply(names(city_list), function(city_name) {
    
    df <- to_posix(city_list[[city_name]]) %>%
      mutate(City = city_name) %>%
      left_join(meta, by = "City") %>%
      mutate(
        Province = ifelse(is.na(Province) | !(Province %in% core_provinces), "Other", Province)
      )
    
    if (use_winter_only) df <- filter_winter(df)
    df <- filter_years(df, year_min, year_max)
    
    df
  }))
}

# Robust IQR filter within each city (uses ABS dif columns)
keep_iqr <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  i <- diff(q)
  x >= (q[1] - 1.5 * i) & x <= (q[2] + 1.5 * i)
}

clean_by_city_iqr <- function(df) {
  required <- c("City", "abs_pm25_dif", "abs_pm10_dif", "abs_no2_dif", "abs_so2_dif")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing columns for IQR cleaning: ", paste(missing, collapse = ", "))
  
  df %>%
    group_by(City) %>%
    filter(
      keep_iqr(abs_pm25_dif) &
        keep_iqr(abs_pm10_dif) &
        keep_iqr(abs_no2_dif)  &
        keep_iqr(abs_so2_dif)
    ) %>%
    ungroup()
}

# Province × date averaging
agg_prov_date <- function(df) {
  df %>%
    group_by(Province, date) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
}

# -----------------------------
# Build datasets for each Level
# -----------------------------
df_y <- add_city_province(aqlist$Yellow, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Yellow")

df_o <- add_city_province(aqlist$Orange, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Orange")

df_r <- add_city_province(aqlist$Red, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Red")

df_non <- add_city_province(aqlist_non, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Non-alert")

combined_data <- bind_rows(df_non, df_y, df_o, df_r) %>%
  mutate(
    Province = factor(Province, levels = legend_order),
    Level    = factor(Level, levels = c("Non-alert", "Yellow", "Orange", "Red"))
  )

# -----------------------------
# Summary stats (RELATIVE only)
# -----------------------------
summary_data <- combined_data %>%
  group_by(Level, Province) %>%
  summarise(
    avg_rlt_pm25_dif = mean(rlt_pm25_dif, na.rm = TRUE),
    se_rlt_pm25_dif  = sd(rlt_pm25_dif,  na.rm = TRUE) / sqrt(sum(!is.na(rlt_pm25_dif))),
    
    avg_rlt_pm10_dif = mean(rlt_pm10_dif, na.rm = TRUE),
    se_rlt_pm10_dif  = sd(rlt_pm10_dif,  na.rm = TRUE) / sqrt(sum(!is.na(rlt_pm10_dif))),
    
    avg_rlt_no2_dif  = mean(rlt_no2_dif, na.rm = TRUE),
    se_rlt_no2_dif   = sd(rlt_no2_dif,  na.rm = TRUE) / sqrt(sum(!is.na(rlt_no2_dif))),
    
    avg_rlt_so2_dif  = mean(rlt_so2_dif, na.rm = TRUE),
    se_rlt_so2_dif   = sd(rlt_so2_dif,  na.rm = TRUE) / sqrt(sum(!is.na(rlt_so2_dif))),
    .groups = "drop"
  ) %>%
  mutate(
    Province = factor(Province, levels = legend_order),
    Level    = factor(Level, levels = c("Non-alert", "Yellow", "Orange", "Red"))
  )

# -----------------------------
# Plot settings (RELATIVE)
# -----------------------------
pollutants <- list("avg_rlt_pm25_dif", "avg_rlt_pm10_dif", "avg_rlt_no2_dif", "avg_rlt_so2_dif")
errors     <- list("se_rlt_pm25_dif",  "se_rlt_pm10_dif",  "se_rlt_no2_dif",  "se_rlt_so2_dif")

x_labels <- list(
  "Relative anomaly (obs − pred) / pred",
  "Relative anomaly (obs − pred) / pred",
  "Relative anomaly (obs − pred) / pred",
  "Relative anomaly (obs − pred) / pred"
)

x_limits <- list(
  c(-50, 10),  # PM2.5
  c(-50, 10),  # PM10
  c(-30, 10),  # NO2
  c(-30, 10)   # SO2
)

fill_cols <- c(
  "Non-alert" = "white",
  "Yellow"    = "#FFFF00",
  "Orange"    = "#FFA500",
  "Red"       = "#FF0000"
)

# -----------------------------
# Loop and save
# -----------------------------
for (i in seq_along(pollutants)) {
  
  pollutant <- pollutants[[i]]
  error     <- errors[[i]]
  xlab      <- x_labels[[i]]
  xlimv     <- x_limits[[i]]
  
  tick_breaks <- pretty(xlimv, n = 6)
  
  p <- ggplot(summary_data, aes(x = .data[[pollutant]], y = Province, fill = Level)) +
    geom_errorbarh(
      aes(
        xmin = .data[[pollutant]] - .data[[error]],
        xmax = .data[[pollutant]] + .data[[error]]
      ),
      height = 0.2, color = "black", size = 1.2
    ) +
    geom_point(color = "black", size = 9, shape = 21) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
    scale_fill_manual(values = fill_cols) +
    guides(fill = guide_legend(override.aes = list(color = "black"))) +  # keep white visible
    scale_x_continuous(breaks = tick_breaks, expand = c(0, 0)) +
    coord_cartesian(xlim = xlimv) +
    theme_minimal(base_size = 18, base_family = "Arial") +
    labs(x = xlab, y = "", fill = "Alert Level") +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text  = element_text(size = 32, color = "black"),
      axis.text.y = element_text(margin = margin(r = 30)),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12),
      legend.key = element_blank(),
      plot.margin = margin(t = 10, r = 20, b = 20, l = 10, unit = "pt")
    ) +
    geom_hline(
      yintercept = seq(1.5, nlevels(summary_data$Province) - 0.5, by = 1),
      color = "grey70", linetype = "dashed"
    )
  
  ggsave(
    filename = file.path(out_dir, paste0(pollutant, "_with_nonalert_T.tiff")),
    plot = p, width = 12, height = 10, units = "in", dpi = 300
  )
}


# ============================================================
# Alert vs Non-alert "control" (ABSOLUTE anomaly) plots
# Adds Non-alert (white) mean ± SE alongside Yellow/Orange/Red
# ABS anomaly = obs − pred  (units: µg m^-3)
# ============================================================

library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(scales)

rm(list = ls(all = TRUE))

# -----------------------------
# Paths
# -----------------------------
alert_rds    <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/era5L/aqmet_incalert_pred_cor.rds"
nonalert_rds <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_trr/aqmet_excalert_pred.rds"
meta_xlsx    <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_tr/meta/meta.xlsx"
out_dir      <- "/Volumes/shiz-wm-netzero/users/yuqing/daiyy/RedAlert/pnas_trr"

# -----------------------------
# Read data
# -----------------------------
aqlist     <- readRDS(alert_rds)      # list: Yellow/Orange/Red -> city -> df
aqlist_non <- readRDS(nonalert_rds)   # list: city -> df
meta <- read_xlsx(meta_xlsx) %>% select(City, Province)

# -----------------------------
# Province display order
# -----------------------------
legend_order   <- c("Other", "Shanxi", "Hebei", "Henan", "Shandong", "Tianjin", "Beijing")
core_provinces <- setdiff(legend_order, "Other")

# -----------------------------
# Filtering settings (recommended for fair "control")
# -----------------------------
USE_WINTER_ONLY <- F  # Nov–Mar
YEAR_MIN <- 2018
YEAR_MAX <- 2022

# -----------------------------
# Helpers
# -----------------------------
to_posix <- function(df) {
  if (!("date" %in% names(df))) stop("Missing 'date' column.")
  if (!inherits(df$date, "POSIXct")) {
    df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  df
}

filter_winter <- function(df) {
  m <- lubridate::month(df$date)
  df[m >= 11 | m <= 3, , drop = FALSE]
}

filter_years <- function(df, y1, y2) {
  yy <- lubridate::year(df$date)
  df[yy >= y1 & yy <= y2, , drop = FALSE]
}

add_city_province <- function(city_list, meta, core_provinces,
                              use_winter_only = TRUE, year_min = 2018, year_max = 2022) {
  bind_rows(lapply(names(city_list), function(city_name) {
    
    df <- to_posix(city_list[[city_name]]) %>%
      mutate(City = city_name) %>%
      left_join(meta, by = "City") %>%
      mutate(
        Province = ifelse(is.na(Province) | !(Province %in% core_provinces), "Other", Province)
      )
    
    if (use_winter_only) df <- filter_winter(df)
    df <- filter_years(df, year_min, year_max)
    
    df
  }))
}

# Robust IQR filter within each city (uses ABS dif columns)
keep_iqr <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  i <- diff(q)
  x >= (q[1] - 1.5 * i) & x <= (q[2] + 1.5 * i)
}

clean_by_city_iqr <- function(df) {
  required <- c("City", "abs_pm25_dif", "abs_pm10_dif", "abs_no2_dif", "abs_so2_dif")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0) stop("Missing columns for IQR cleaning: ", paste(missing, collapse = ", "))
  
  df %>%
    group_by(City) %>%
    filter(
      keep_iqr(abs_pm25_dif) &
        keep_iqr(abs_pm10_dif) &
        keep_iqr(abs_no2_dif)  &
        keep_iqr(abs_so2_dif)
    ) %>%
    ungroup()
}

agg_prov_date <- function(df) {
  df %>%
    group_by(Province, date) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
}

# -----------------------------
# Build datasets for each Level
# -----------------------------
df_y <- add_city_province(aqlist$Yellow, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Yellow")

df_o <- add_city_province(aqlist$Orange, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Orange")

df_r <- add_city_province(aqlist$Red, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Red")

df_non <- add_city_province(aqlist_non, meta, core_provinces, USE_WINTER_ONLY, YEAR_MIN, YEAR_MAX) %>%
  clean_by_city_iqr() %>%
  agg_prov_date() %>%
  mutate(Level = "Non-alert")

combined_data <- bind_rows(df_non, df_y, df_o, df_r) %>%
  mutate(
    Province = factor(Province, levels = legend_order),
    Level    = factor(Level, levels = c("Non-alert", "Yellow", "Orange", "Red"))
  )

# -----------------------------
# Summary stats (ABSOLUTE only)
# -----------------------------
summary_data <- combined_data %>%
  group_by(Level, Province) %>%
  summarise(
    avg_abs_pm25_dif = mean(abs_pm25_dif, na.rm = TRUE),
    se_abs_pm25_dif  = sd(abs_pm25_dif,  na.rm = TRUE) / sqrt(sum(!is.na(abs_pm25_dif))),
    
    avg_abs_pm10_dif = mean(abs_pm10_dif, na.rm = TRUE),
    se_abs_pm10_dif  = sd(abs_pm10_dif,  na.rm = TRUE) / sqrt(sum(!is.na(abs_pm10_dif))),
    
    avg_abs_no2_dif  = mean(abs_no2_dif, na.rm = TRUE),
    se_abs_no2_dif   = sd(abs_no2_dif,  na.rm = TRUE) / sqrt(sum(!is.na(abs_no2_dif))),
    
    avg_abs_so2_dif  = mean(abs_so2_dif, na.rm = TRUE),
    se_abs_so2_dif   = sd(abs_so2_dif,  na.rm = TRUE) / sqrt(sum(!is.na(abs_so2_dif))),
    .groups = "drop"
  ) %>%
  mutate(
    Province = factor(Province, levels = legend_order),
    Level    = factor(Level, levels = c("Non-alert", "Yellow", "Orange", "Red"))
  )

# -----------------------------
# Plot settings (ABSOLUTE)
# -----------------------------
pollutants <- list("avg_abs_pm25_dif", "avg_abs_pm10_dif", "avg_abs_no2_dif", "avg_abs_so2_dif")
errors     <- list("se_abs_pm25_dif",  "se_abs_pm10_dif",  "se_abs_no2_dif",  "se_abs_so2_dif")

x_labels <- list(
  expression("Absolute anomaly (obs - pred) ("*mu*"g "*m^{-3}*")"),
  expression("Absolute anomaly (obs - pred) ("*mu*"g "*m^{-3}*")"),
  expression("Absolute anomaly (obs - pred) ("*mu*"g "*m^{-3}*")"),
  expression("Absolute anomaly (obs - pred) ("*mu*"g "*m^{-3}*")")
)

# Your requested x ranges
x_limits <- list(
  c(-90, 20),  # PM2.5
  c(-90, 20),  # PM10
  c(-20, 10),  # NO2
  c(-20, 10)   # SO2
)

fill_cols <- c(
  "Non-alert" = "white",
  "Yellow"    = "#FFFF00",
  "Orange"    = "#FFA500",
  "Red"       = "#FF0000"
)

# -----------------------------
# Loop and save
# -----------------------------
for (i in seq_along(pollutants)) {
  
  pollutant <- pollutants[[i]]
  error     <- errors[[i]]
  xlab      <- x_labels[[i]]
  xlimv     <- x_limits[[i]]
  
  tick_breaks <- pretty(xlimv, n = 6)
  
  p <- ggplot(summary_data, aes(x = .data[[pollutant]], y = Province, fill = Level)) +
    geom_errorbarh(
      aes(
        xmin = .data[[pollutant]] - .data[[error]],
        xmax = .data[[pollutant]] + .data[[error]]
      ),
      height = 0.2, color = "black", size = 1.2
    ) +
    geom_point(color = "black", size = 9, shape = 21) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1.2) +
    scale_fill_manual(values = fill_cols) +
    guides(fill = guide_legend(override.aes = list(color = "black"))) +
    scale_x_continuous(breaks = tick_breaks, expand = c(0, 0)) +
    coord_cartesian(xlim = xlimv) +
    theme_minimal(base_size = 18, base_family = "Arial") +
    labs(x = xlab, y = "", fill = "Alert Level") +
    theme(
      axis.title = element_text(size = 16, face = "bold"),
      axis.text  = element_text(size = 32, color = "black"),
      axis.text.y = element_text(margin = margin(r = 30)),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text  = element_text(size = 12),
      legend.key = element_blank(),
      plot.margin = margin(t = 10, r = 20, b = 20, l = 10, unit = "pt")
    ) +
    geom_hline(
      yintercept = seq(1.5, nlevels(summary_data$Province) - 0.5, by = 1),
      color = "grey70", linetype = "dashed"
    )
  
  ggsave(
    filename = file.path(out_dir, paste0(pollutant, "_with_nonalert_ABS.tiff")),
    plot = p, width = 12, height = 10, units = "in", dpi = 300
  )
}
