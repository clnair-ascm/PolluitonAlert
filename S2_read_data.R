### Load packages
library("dplyr")
library("tidyverse")

setwd("/Volumes/daiyy/2024_paper/RedAlert/code")

rm(list = ls(all = TRUE))

aqlist <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/aqmet_all.rds")
inc_alert <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/aqmet_incalert.rds")
exc_alert <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/aqmet_excalert.rds")

meta   <- read.csv("/Volumes/daiyy/2024_paper/RedAlert/data/EventsTime_formatC.csv")

aqlist <- lapply(aqlist, function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
})

inc_alert <- lapply(inc_alert, function(alert_list) {
  lapply(alert_list, function(df) {
    df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    return(df)
  })
})

exc_alert <- lapply(exc_alert, function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
})

meta$start <- as.POSIXct(meta$start, format = "%d/%m/%Y", tz = "UTC")
meta$end   <- as.POSIXct(meta$end,   format = "%d/%m/%Y", tz = "UTC")

# Sort the data by date to ensure proper plotting
exc_alert$Baoding <- exc_alert$Tangshan %>% arrange(date)

# Plot the "trend" and "no2" values for Tangshan with excluded data in black
plot(exc_alert$Tangshan$date, exc_alert$Tangshan$pm25, type = "l", col = "black", xlab = "Trend", ylab = "NO2", main = "NO2 Levels in Tangshan")

# Add points for different alert levels in color
if ("Yellow" %in% names(inc_alert)) {
  points(inc_alert$Yellow$Tangshan$trend, inc_alert$Yellow$Tangshan$pm25, type = "l", col = "yellow")
}
if ("Orange" %in% names(inc_alert)) {
  points(inc_alert$Orange$Tangshan$trend, inc_alert$Orange$Tangshan$pm25, type = "l", col = "orange")
}
if ("Red" %in% names(inc_alert)) {
  points(inc_alert$Red$Tangshan$trend, inc_alert$Red$Tangshan$pm25, type = "l", col = "red")
}

