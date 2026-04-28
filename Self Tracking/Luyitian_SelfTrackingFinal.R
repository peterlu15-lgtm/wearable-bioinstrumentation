rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(ggpubr)

# Sry my system is in another language, this line is to make sure it runs under English
Sys.setlocale("LC_TIME", "English")

# Data Acquisition
# Load raw Excel data without headers
raw_data <- read_excel("data.xlsx", col_names = FALSE)

# Extract date headers and stress scores from the first two rows
dates_raw <- as.character(unlist(raw_data[1, ]))
stress_raw <- as.numeric(as.character(unlist(raw_data[2, ])))

# Create a mapping table to handle excel's numeric date format
mapping_table <- tibble(col_name = dates_raw, stress_val = stress_raw) %>%
  mutate(clean_date = if_else(str_detect(col_name, "^[0-9]+$"),
                              as.Date(as.numeric(col_name), origin = "1899-12-30"),
                              ymd(col_name)))

# Extract RR interval data 
rr_data <- raw_data[-(1:2), ] 
colnames(rr_data) <- dates_raw 

# Data Processing & Metric Calculation
df_final <- rr_data %>%
  pivot_longer(cols = everything(), names_to = "time_raw", values_to = "rr_value") %>%
  filter(!is.na(rr_value)) %>%
  mutate(rr_value = as.numeric(rr_value)) %>%
  # Calculate RMSSD
  group_by(time_raw) %>%
  summarise(RMSSD = sqrt(mean(diff(rr_value)^2, na.rm = TRUE)), .groups = 'drop') %>%
  # Join with table
  left_join(mapping_table, by = c("time_raw" = "col_name")) %>%
  mutate(Date = as.Date(clean_date)) %>%
  rename(Stress_Score = stress_val) %>%
  # Fill dates (Mar 08 to Apr 19)
  complete(Date = seq(as.Date("2026-03-08"), as.Date("2026-04-19"), by = "day")) %>%
  mutate(
    # Categorize study phases and artifacts
    Display_Category = case_when(
      RMSSD > 150 | Date %in% as.Date(c("2026-03-26", "2026-04-05")) ~ "Artifact",
      Date >= as.Date("2026-03-24") & Date <= as.Date("2026-04-15") ~ "High-Load Academic",
      TRUE ~ "Low load | Vacation"
    ),
    Period = if_else(Display_Category == "High-Load Academic", "High-Load Academic", "Low load | Vacation"),
    Date_Label = factor(format(Date, "%b %d"), levels = unique(format(Date, "%b %d")))
  )

# Calculate phase means for baseline comparison (excluding artifacts)
phase_means <- df_final %>%
  filter(Display_Category != "Artifact", !is.na(RMSSD)) %>%
  group_by(Period) %>%
  summarise(m = mean(RMSSD))

# Visualization 1
plot_bar <- ggplot(df_final, aes(x = Date_Label, y = RMSSD)) +
  # Bar plot for RMSSD recovery levels
  geom_col(aes(fill = Display_Category), width = 0.7, alpha = 0.85) +
  # Dashed lines representing mean values for each period
  geom_hline(data = phase_means, aes(yintercept = m, color = Period), linetype = "dashed", linewidth = 1) +
  # Secondary Y-axis overlay (Subjective Stress Score)
  geom_line(aes(y = Stress_Score * 10, group = 1), color = "firebrick", linewidth = 0.9, na.rm = TRUE) +
  geom_point(aes(y = Stress_Score * 10), color = "firebrick", size = 1.2, na.rm = TRUE) +
  # Color mapping for different study phases
  scale_fill_manual(values = c(
    "High-Load Academic" = "#E41A1C", 
    "Low load | Vacation" = "#377EB8", 
    "Artifact" = "orange"
  )) +
  scale_color_manual(values = c(
    "High-Load Academic" = "darkred", 
    "Low load | Vacation" = "darkblue"
  )) +
  annotate("text", x = "Mar 26", y = 160, label = "Artifact", color = "orange", fontface = "bold", size = 3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Daily Recovery (RMSSD) vs. Academic Stress",
       subtitle = "Mar 08 - Apr 19 | Dashed Lines: Period Means | Red Line: Subjective Stress",
       x = "Observation Date", y = "RMSSD (ms) / Stress Score (x10)",
       fill = "Study Phase")

# Visualization 2
df_heatmap <- df_final %>%
  mutate(
    Weekday = wday(Date, label = TRUE, abbr = TRUE, week_start = 7),
    Week_Index = floor(as.numeric(Date - min(Date)) / 7) + 1
  )

plot_heatmap <- ggplot(df_heatmap, aes(x = Weekday, y = factor(Week_Index, levels = rev(sort(unique(Week_Index)))), fill = RMSSD)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.na(RMSSD), "", round(RMSSD, 0))), size = 3.5, fontface = "bold") +
  # Gradient scale: Red (Low HRV) to Green (High HRV)
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", midpoint = 65, na.value = "grey90") +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(face = "bold", size = 14)) +
  labs(title = "Recovery Heatmap (Morning RMSSD)",
       x = "Day of the Week", y = "March - April", fill = "RMSSD (ms)")

# Visualization 3
# Filter data to include only valid data points
stats_data <- df_final %>% 
  filter(Display_Category != "Artifact", !is.na(RMSSD), !is.na(Stress_Score))

plot_corr <- ggplot(stats_data, aes(x = Stress_Score, y = RMSSD)) +
  geom_point(aes(color = Display_Category), size = 3, alpha = 0.7) +
  # Linear regression model with confidence interval
  geom_smooth(method = "lm", color = "black", linetype = "solid", se = TRUE, fill = "grey80") +
  stat_cor(method = "pearson", label.x = 1, label.y = 120, size = 5, color = "darkred") +
  scale_color_manual(values = c("High-Load Academic" = "#E41A1C", "Low load | Vacation" = "#377EB8")) +
  theme_pubr() +
  labs(title = "Statistical Correlation Analysis",
       x = "Subjective Stress Score (1-10)", y = "Morning RMSSD (ms)",
       color = "Phase")


# Display all plots
print(plot_bar)
print(plot_heatmap)
print(plot_corr)

# Pearson correlation coefficients
print(cor.test(stats_data$RMSSD, stats_data$Stress_Score))