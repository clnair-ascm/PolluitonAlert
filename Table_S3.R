### Load packages
library("dplyr")
library("tidyverse")

setwd("/Volumes/shiz-wm-netzero/users/yuqing/PNAS_TR/data/Revised/TableS3")

rm(list = ls(all = TRUE))

data <- readRDS("aqmet_def_events1316.rds")
data <- readRDS("aqmet_incalert.rds")

combined_list <- data
# Show structure
str(combined_list)

# Show as list output with indices
# print(data)

# Get all unique city names across Yellow, Orange, Red
city_names <- unique(
  c(names(combined_list$Yellow),
    names(combined_list$Orange),
    names(combined_list$Red))
)

# For each city, collect all data frames and rbind them
merged <- lapply(city_names, function(city) {
  dfs <- list(
    combined_list$Yellow[[city]],
    combined_list$Orange[[city]],
    combined_list$Red[[city]]
  )
  # Drop NULLs (city not present in one of the groups)
  dfs <- dfs[!sapply(dfs, is.null)]
  do.call(rbind, dfs)
})

names(merged) <- city_names

all_cities <- merged

merged <- combined_list

# Merge by city: rbind if the city appears multiple times
merged <- lapply(split(all_cities, names(all_cities)), function(x) {
  if (length(x) > 1) {
    do.call(rbind, x)
  } else {
    x[[1]]
  }
})

# Now merged is a single list, one entry per city
str(merged, max.level = 1)

# Your ranking
city_order <- c(
  "Beijing","Tianjin","Shijiazhuang","Tangshan","Qinhuangdao","Handan","Baoding",
  "Chengde","Cangzhou","Langfang","Hengshui","Zhangjiakou","Taiyuan","Datong",
  "Yangquan","Changzhi","Jincheng","Shuozhou","Jinzhong","Yuncheng","Xinzhou",
  "Linfen","Lvliang","Jinan","Qingdao","Zibo","Zaozhuang","Dongying","Yantai",
  "Weifang","Jining","Taian","Weihai","Rizhao","Linyi","Dezhou","Liaocheng",
  "Binzhou","Heze","Zhengzhou","Kaifeng","Luoyang","Pingdingshan","Anyang",
  "Hebi","Xinxiang","Jiaozuo","Puyang","Xuchang","Luohe","Sanmenxia","Nanyang",
  "Shangqiu","Xinyang","Zhoukou","Zhumadian","Xingtai"
)

# Reorder the merged list
merged_ranked <- merged[city_order]

# If you want to keep only cities that exist in your merged list:
merged_ranked <- merged_ranked[!sapply(merged_ranked, is.null)]

# Or if you want one big data frame with ranking:
library(dplyr)
merged_df <- bind_rows(merged, .id = "city")

merged_df <- merged_df %>%
  mutate(city = factor(city, levels = city_order)) %>%
  arrange(city)

# Check
table(merged_df$city)


# Extract minimum pm25 for each city
min_pm25 <- sapply(merged, function(df) mean(df$pm25, na.rm = TRUE))

# Make into a data frame
min_pm25_df <- data.frame(
  city = names(min_pm25),
  min_pm25 = min_pm25,
  row.names = NULL
)

# Apply your ranking order
city_order <- c(
  "Beijing","Tianjin","Shijiazhuang","Tangshan","Qinhuangdao","Handan","Baoding",
  "Chengde","Cangzhou","Langfang","Hengshui","Zhangjiakou","Taiyuan","Datong",
  "Yangquan","Changzhi","Jincheng","Shuozhou","Jinzhong","Yuncheng","Xinzhou",
  "Linfen","Lvliang","Jinan","Qingdao","Zibo","Zaozhuang","Dongying","Yantai",
  "Weifang","Jining","Taian","Weihai","Rizhao","Linyi","Dezhou","Liaocheng",
  "Binzhou","Heze","Zhengzhou","Kaifeng","Luoyang","Pingdingshan","Anyang",
  "Hebi","Xinxiang","Jiaozuo","Puyang","Xuchang","Luohe","Sanmenxia","Nanyang",
  "Shangqiu","Xinyang","Zhoukou","Zhumadian","Xingtai"
)

min_pm25_df$city <- factor(min_pm25_df$city, levels = city_order)
min_pm25_df <- min_pm25_df[order(min_pm25_df$city), ]

# View the summary table
print(min_pm25_df)

