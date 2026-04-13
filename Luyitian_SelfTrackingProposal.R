# Load Libraries
library(tidyverse)
library(lubridate)
library(readxl)

# 1. Set Path and Read Data
setwd("C:/Users/peter/Desktop/穿戴设备/self")
raw_data <- read_excel("data.xlsx", col_names = FALSE)

# 2. Extract Date and Stress Info
dates_raw <- as.character(unlist(raw_data[1, ]))
stress_raw <- as.numeric(as.character(unlist(raw_data[2, ])))

# Mapping table to fix date formats
mapping_table <- tibble(
  col_name = dates_raw,
  stress_val = stress_raw
) %>%
  mutate(
    clean_date = if_else(
      str_detect(col_name, "^[0-9]+$"),
      as.Date(as.numeric(col_name), origin = "1899-12-30"),
      ymd(col_name)
    )
  )

# 3. Handle RR data and calculate RMSSD
rr_data <- raw_data[-(1:2), ] 
colnames(rr_data) <- dates_raw 

tidy_data <- rr_data %>%
  pivot_longer(cols = everything(), names_to = "time_raw", values_to = "rr_value") %>%
  filter(!is.na(rr_value)) %>%
  # RMSSD calculation
  group_by(time_raw) %>%
  mutate(
    diff_rr = rr_value - lag(rr_value),
    diff_sq = diff_rr^2
  ) %>%
  summarise(
    RMSSD = sqrt(mean(diff_sq, na.rm = TRUE)),
    .groups = 'drop'
  ) %>%
  # Match with stress scores
  left_join(mapping_table, by = c("time_raw" = "col_name")) %>%
  # Final format for tidy_data
  mutate(time = as.POSIXct(clean_date)) %>%
  rename(Stress_Score = stress_val) %>%
  pivot_longer(cols = c(RMSSD, Stress_Score), names_to = "feature", values_to = "value") %>%
  select(time, feature, value) %>%
  as_tibble()

# 4. Check data
print("--- Check Tibble ---")
print(head(tidy_data, 10))

# 5. Summary Table
summary_stats <- tidy_data %>%
  group_by(feature) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  ) %>%
  # Add Units
  mutate(Units = case_when(
    feature == "RMSSD" ~ "ms",
    feature == "Stress_Score" ~ "Scale 1-10",
    TRUE ~ NA_character_
  )) %>%
  # Format for report
  select(Feature = feature, Units, Mean, `Standard Deviation` = SD)

print("--- Summary Table ---")
print(summary_stats)