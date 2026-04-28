# ==========================================
# StressBeat 项目完整脚本 (2026 Spring Final)
# ==========================================

# 1. 加载基础库
library(tidyverse)
library(lubridate)
library(readxl)

# 强制设置环境为英文，确保日期显示为 Mar, Apr, Mon, Tue 等
Sys.setlocale("LC_TIME", "English")

# 2. 读取并清洗 Excel 真实数据 (从 3/23 开始的部分)
raw_data <- read_excel("data.xlsx", col_names = FALSE)
dates_raw <- as.character(unlist(raw_data[1, ]))
stress_raw <- as.numeric(as.character(unlist(raw_data[2, ])))

mapping_table <- tibble(col_name = dates_raw, stress_val = stress_raw) %>%
  mutate(clean_date = if_else(str_detect(col_name, "^[0-9]+$"),
                              as.Date(as.numeric(col_name), origin = "1899-12-30"),
                              ymd(col_name)))

rr_data <- raw_data[-(1:2), ] 
colnames(rr_data) <- dates_raw 

df_real <- rr_data %>%
  pivot_longer(cols = everything(), names_to = "time_raw", values_to = "rr_value") %>%
  filter(!is.na(rr_value)) %>%
  mutate(rr_value = as.numeric(rr_value)) %>%
  group_by(time_raw) %>%
  summarise(RMSSD = sqrt(mean(diff(rr_value)^2, na.rm = TRUE)), .groups = 'drop') %>%
  left_join(mapping_table, by = c("time_raw" = "col_name")) %>%
  mutate(Date = as.Date(clean_date)) %>%
  rename(Stress_Score = stress_val)

# 3. 模拟数据部分 (保持你要求的数值不动)
# --- 第一阶段：Spring Break (3/8 - 3/15) ---
d_vacation <- seq(as.Date("2026-03-08"), as.Date("2026-03-15"), by = "day")
df_vacation <- tibble(
  Date = d_vacation, 
  RMSSD = c(82, 98, 105, 88, 110, 95, 102, 78), 
  Stress_Score = c(3, 1, 1, 2, 1, 2, 1, 4) 
)

# --- 第二阶段：返校过渡周 (3/16 - 3/22) ---
d_return <- seq(as.Date("2026-03-16"), as.Date("2026-03-22"), by = "day")
df_return <- tibble(
  Date = d_return, 
  RMSSD = c(75, 62, 80, 58, 65, 50, 68), 
  Stress_Score = c(5, 7, 5, 8, 6, 9, 7) 
)

df_baseline <- bind_rows(df_vacation, df_return)

# 4. 整合数据集并处理 3/26 异常值
# ==========================================
df_final <- bind_rows(df_baseline, df_real) %>% 
  arrange(Date) %>%
  # 使用 complete 函数补全时间序列，确保 3月26日 存在于数据框中
  complete(Date = seq(min(Date), max(Date), by = "day")) %>%
  # 针对 3/26 赋予一个明显的异常值（根据图 2 设定为 268 或 197 左右）
  mutate(RMSSD = if_else(Date == as.Date("2026-03-26") & is.na(RMSSD), 260, RMSSD),
         Stress_Score = if_else(Date == as.Date("2026-03-26") & is.na(Stress_Score), 5, Stress_Score)) %>%
  # 重新生成带标签的因子，确保 X 轴排序正确
  mutate(Date_Label = format(Date, "%b %d"),
         Date_Label = factor(Date_Label, levels = unique(Date_Label)))

# ==========================================
# 5. 绘图修正 (增加 na.rm = FALSE 确保断点或异常点可见)
# ==========================================

# 图 1 现在会看到 3/26 有一个突出的尖峰
plot1 <- ggplot(df_final, aes(x = Date_Label, y = RMSSD, group = 1)) +
  geom_area(fill = "steelblue", alpha = 0.2) + 
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  # 增加 Y 轴上限以容纳异常值
  scale_y_continuous(limits = c(0, 300)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.minor = element_blank()) +
  labs(title = "Daily Morning RMSSD Trend",
       subtitle = "Observed transition including marked sensor artifacts",
       x = "Date", y = "RMSSD (ms)")

# 图 2 的标注将精准对应到这个补全的点上
print(plot1)

# 图 2: Stress vs. Recovery (保持原样)
plot2 <- ggplot(df_final, aes(x = Date_Label)) +
  geom_col(aes(y = RMSSD), fill = "skyblue", alpha = 0.7, na.rm = TRUE) +
  geom_line(aes(y = Stress_Score * 10, group = 1), color = "firebrick", linewidth = 1, na.rm = TRUE) +
  geom_point(aes(y = Stress_Score * 10), color = "firebrick", size = 1, na.rm = TRUE) +
  annotate("text", x = "Mar 26", y = 145, label = "Artifact", color = "orange", fontface = "bold", size = 3) +
  annotate("text", x = "Apr 05", y = 195, label = "Artifact", color = "orange", fontface = "bold", size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  labs(title = "StressBeat: Academic Stress vs. Physiological Recovery", 
       subtitle = "Higher Red Line = Greater Stress | Higher Blue Bar = Better Recovery",
       x = "Date", y = "Value (RMSSD / Stress x 10)")

# 图 3: Recovery Heatmap (修正：从上往下看)
df_heatmap <- df_final %>%
  mutate(Weekday = wday(Date, label = TRUE, abbr = TRUE),
         Week_Index = floor(as.numeric(Date - min(Date)) / 7) + 1)

plot3 <- ggplot(df_heatmap, aes(x = Weekday, y = factor(Week_Index, levels = rev(sort(unique(Week_Index)))), fill = RMSSD)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "#FFFFCC", high = "#006837", name = "RMSSD", na.value = "grey90") + 
  geom_text(aes(label = ifelse(is.na(RMSSD), "", round(RMSSD, 0))), size = 2.5, color = "grey30") +
  coord_fixed() + 
  theme_minimal() +
  labs(title = "The Recovery Fingerprint",
       subtitle = "W1: Spring Break (Top) | W2: Transition | W3+: Work Season",
       x = "Day of the Week", y = "Week Index") +
  theme(panel.grid = element_blank())

print(plot1)
print(plot2)
print(plot3)