# Mini-Project 6 Validation

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(magrittr)

# load data
data <- read.csv('C:/Users/peter/Desktop/穿戴设备/Mini-Project 6/rrData.csv') # adjust path to where your .csv file is, data should be 250 obs. x 4 variables
data$participant <- factor(data$participant) # make participant variable a factor
table(data$participant) # should be 10 repeats per participant


# LINE PLOT ----
# reshape the data into long format so that there are 4 columns: participant, time, feature (rr or rr_fft), and value
data_long <- data %>% gather(key = "feature", value = "value", rr, rr_fft) # fill gather() to create data_long which should be 500 obs. x 4 variables

# line plot
ggplot(data_long, aes(x = time, y = value, color = feature)) +
  geom_line() +
  geom_point() +
  facet_wrap(~participant) +
  labs(x = "Elapsed Time (s)", y = "RR (brpm)", color = "Feature") +
  ggtitle("Figure 1: Line Plot") 

# BAR PLOT ----
# find the mean and standard deviation within each participant-feature
summary_data <- data_long %>% group_by(participant, feature) %>% summarize(mean_val = mean(value, na.rm = TRUE), sd_val = sd(value, na.rm = TRUE), .groups = 'drop') # fill in group_by() and summarize() functions, should be 50 obs. x 4 variables

# bar plot
ggplot(summary_data, aes(x = participant, y = mean_val, fill = feature)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), position = position_dodge(0.9), width = 0.25) +
  labs(x = "Participant", y = "RR (brpm)", fill = "Feature") +
  ggtitle("Figure 2: Bar Plot")


# SCATTER PLOT ----
# fit linear model to data, y = rr_fft, x = rr)
fit <- lm(data$rr_fft ~ data$rr)

# combine text for equation
eq <- substitute(italic(y) == a + b %.% italic(x)*", "~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(fit)[1]), digits = 2),
                      b = format(unname(coef(fit)[2]), digits = 2),
                      r2 = format(summary(fit)$r.squared, digits = 2)))
text <- as.character(as.expression(eq));

# scatter plot
ggplot(data, aes(x = rr, y = rr_fft)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "RR (brpm)", y = "RR FFT (brpm)") +
  ggtitle("Figure 3: Scatter Plot") +
  annotate("text", x = 18, y = 28, label = text, parse = TRUE) 


# BLAND-ALTMAN PLOT ----
# calculate and save the differences between the two measures and the averages of the two measures
data %<>% mutate(diff = rr - rr_fft, avg = (rr + rr_fft) / 2)

#compute the mean and limits of agreement (LoA)
mean_bias <- mean(data$diff, na.rm = TRUE)
sd_diff <- sd(data$diff, na.rm = TRUE)
loa_upper <- mean_bias + 1.96 * sd_diff
loa_lower <- mean_bias - 1.96 * sd_diff

# Bland-Altman plot
ggplot(data, aes(x = avg, y = diff)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = mean_bias, color = "green", linetype = "solid") +
  geom_hline(yintercept = loa_upper, color = "orange", linetype = "dashed") +
  geom_hline(yintercept = loa_lower, color = "orange", linetype = "dashed") +
  labs(x = "Average of Measures (brpm)", y = "Difference Between Measures (rr - rr_fft) (brpm)") +
  annotate("text", x = 20, y = mean_bias + 0.5, label = paste("Mean:", round(mean_bias, 2)), color = "green") +
  annotate("text", x = 20, y = loa_upper + 0.5, label = paste("Upper LoA:", round(loa_upper, 2)), color = "orange") +
  annotate("text", x = 20, y = loa_lower - 0.5, label = paste("Lower LoA:", round(loa_lower, 2)), color = "orange") +
  ggtitle("Figure 4: Bland-Altman Plot") 


# BOX PLOT ----
# box plot
ggplot(data, aes(x = participant, y = diff, color = participant)) +
  geom_boxplot() +
  labs(x = "Participant", y = "Difference Between Measures (rr - rr_fft) (brpm)") +
  theme(legend.position = "none") +
  ggtitle("Figure 5: Box Plot")