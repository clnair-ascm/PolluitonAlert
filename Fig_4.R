
rm(list = ls(all = T))
########################################################################################################################

library(tidyverse)
library(lubridate)
library(viridis)
library(ggridges)
library(ggTimeSeries)
library(patchwork)

df <- read.csv("/Volumes/daiyy/2024_paper/RedAlert/data/EventsTime_formatC.csv")
  # 转换日期格式
# 假设原始日期格式为"dd-mm-yyyy"，使用dmy()函数解析
df_clean <- df %>%
  mutate(
    start = dmy(start),  # 根据实际格式选用 mdy()/ymd()/dmy()
    end = dmy(end),      # 若含时间则用 dmy_hm()/dmy_hms()
    
    # 添加日期有效性检查
    start_valid = is.Date(start),
    end_valid = is.Date(end)
  ) %>%
  filter(start_valid & end_valid) %>%  # 移除非日期数据
  select(-start_valid, -end_valid) %>%
  
  # 计算持续时间和间隔
  mutate(
    duration = as.numeric(end - start) + 1,
    year = year(start),
    month = month(start, label = TRUE),
    week = isoweek(start)
  ) %>%
  
  # 按城市分组计算间隔
  group_by(Cities) %>%
  arrange(start) %>%
  mutate(
    gap_days = as.numeric(start - lag(end)),
    gap_flag = case_when(
      is.na(gap_days) ~ "First Alert",
      gap_days > 180 ~ "Over 6mo",
      TRUE ~ "Normal"
    )
  ) %>%
  ungroup()

ggplot(df_clean) +
  geom_segment(
    aes(x = start, xend = end, 
        y = Cities, yend = Cities,
        color = Level, alpha = gap_flag),
    linewidth = 4,
    lineend = "round"
  ) +
  scale_color_manual(values = c("Yellow" = "#FFFF00", 
                                "Orange" = "#FFA500",
                                "Red" = "#FF0000")) +
  scale_alpha_manual(
    values = c("First Alert" = 0.7, "Normal" = 0.9, "Over 6mo" = 0.3),
    guide = guide_legend(title = "Gap Type")
  ) +
  labs(title = "Pollution Alert Timeline with Adaptive Opacity",
       subtitle = "Opacity reflects interval length between alerts",
       x = "Date", y = "City") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
