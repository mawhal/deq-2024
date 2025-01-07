# Generate a calendar showing all sampled dates in the year for the main project sites

# This code was first generated using the following call to chatgpt
# help me write R code using ggplot to make a calendar of 2024 with highlights for specified dates:
# dates <- c("2024-03-20", "2024-04-03", "2024-04-10", "2024-04-17", "2024-04-24", "2024-04-25", "2024-05-01", "2024-05-08", "2024-05-23", "2024-05-30", "2024-06-06", "2024-06-13", "2024-06-20", "2024-06-27", "2024-07-02", "2024-07-11" ,"2024-07-18", "2024-07-26", "2024-08-15", "2024-08-22","2024-09-05", "2024-09-26", "2024-10-10", "2024-10-24","2024-12-13")
library(tidyverse)
library(lubridate)

# Specified dates
dates <- as.Date(c("2024-03-20", "2024-04-03", "2024-04-10", "2024-04-17", 
                   "2024-04-24", "2024-04-25", "2024-05-01", "2024-05-08", 
                   "2024-05-23", "2024-05-30", "2024-06-06", "2024-06-13", 
                   "2024-06-20", "2024-06-27", "2024-07-02", "2024-07-11", 
                   "2024-07-18", "2024-07-26", "2024-08-15", "2024-08-22",
                   "2024-09-05", "2024-09-26", "2024-10-10", "2024-10-17", 
                   "2024-10-24", "2024-11-01", "2024-11-14", "2024-11-21",
                   "2024-12-13"))

# Generate a data frame for all dates in 2024
all_dates <- data.frame(date = seq.Date(from = as.Date("2024-01-01"), 
                                        to = as.Date("2024-12-31"), by = "day"))

# Add columns for year, month, week, and day of the week
all_dates <- all_dates %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE),
    week = isoweek(date),
    wday = wday(date, label = TRUE, abbr = TRUE),
    is_highlighted = date %in% dates # Highlight specified dates
  )

# Adjust for calendar-style plotting
all_dates <- all_dates %>%
  group_by(month) %>%
  mutate(week_of_month = as.numeric(format(date, "%U")) - min(as.numeric(format(date, "%U"))) + 1) %>%
  ungroup()

# Plot calendar
ggplot(all_dates, aes(x = wday, y = -week_of_month, fill = is_highlighted)) +
  geom_tile(color = "white", size = 0.5) +
  facet_wrap(~month, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = c("FALSE" = "lightgray", "TRUE" = "steelblue")) +
  labs(title = "2024 Sampling Dates",
       x = NULL, y = NULL, fill = "Highlighted") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

# 28 weeks of sampling from 20 March to 5 December
# 12 weeks without sampling

