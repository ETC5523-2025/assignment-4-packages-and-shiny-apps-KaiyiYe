library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(usethis)

yarra_raw <- readxl::read_excel("data-raw/yarra_wq.xls")

# Tidy names % data types; split datetime
yarra_tidy <- yarra_raw |>
  janitor::clean_names() |>
  mutate(
    site_id = as.factor(site_id),
    name = as.factor(name),
    data_type = as.factor(data_type),
    parameter_id = as.factor(parameter_id),
    date = as.Date(datetime),
    year = as.integer(format(datetime, "%Y")),
    month = as.integer(format(datetime, "%m")),
    weekday = format(date, "%A"),
    time = format(datetime, "%H:%M:%S"),
    hour = as.integer(format(datetime, "%H"))
  ) |>
  select(-datetime)

# Keep site with sufficient post-1990 coverage
yarra_chandler <- yarra_tidy |>
  filter(name == "YARRA @ CHANDLER HWY", year >= 1990)

# Select parameters available in both periods 
# 210: pH; 450: Water Temperature; 810: Turbidity; 820: Salinity (EC)
yarra_wq_clean <- yarra_chandler |>
  filter(parameter_id %in% c(210, 450, 810, 820))

# Drop quality==180 (not recorded)
yarra_wq_clean <- yarra_wq_clean |>
  filter(quality != 180) |>
  select(-resolution)

# Label periods & keep target windows
yarra_wq_period <- yarra_wq_clean |>
  mutate(
    period = case_when(
      year <= 1999 ~ "1990s",
      year >= 2015 ~ "recent",
      TRUE ~ "other"
    )
  ) |>
  filter(period %in% c("1990s","recent"))

# Calculate hourly medians + IQR
wq_hourly_median <- yarra_wq_period |>
  group_by(parameter, period, hour) |>
  summarise(
    median = median(value, na.rm = TRUE),
    q1     = quantile(value, 0.25, na.rm = TRUE),
    q3     = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

usethis::use_data(yarra_wq_period, wq_hourly_median, overwrite = TRUE)
