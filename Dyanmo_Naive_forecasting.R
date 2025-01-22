
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

setwd("C:/Users/girir/OneDrive - The University of Texas at Austin/Desktop/Fall McCombs/Supply Chain Management Genaro/Project")

dynamos_data <- read.csv("dynamo_games_initial.csv")

print(dynamos_data)

# Add Year to Dates where it is missing
dynamos_data <- dynamos_data %>%
  mutate(Date = if_else(str_detect(Date, as.character(Year)), Date, paste(Date, Year)))

# Convert to date format
dynamos_data <- dynamos_data %>%
  mutate(FullDate = as.Date(Date, format = "%A %B %d %Y"))

# Basic EDA and transformations omitted here for brevity, as previously shown.

# Prepare Monthly Data for Time Series
library(fpp3)
library(tsibble)

dynamos_monthly <- dynamos_data %>%
  mutate(date = parse_date_time(Date, orders = "A B d Y")) %>%
  mutate(yearmonth = yearmonth(date)) %>%
  group_by(yearmonth) %>%
  summarise(Attendance = mean(Attendance)) %>%
  as_tsibble(index = yearmonth) %>%
  fill_gaps() %>%
  mutate(
    Attendance = zoo::na.approx(Attendance, na.rm = FALSE)
  ) %>%
  mutate(
    Attendance = if_else(is.na(Attendance),
                         mean(Attendance, na.rm = TRUE),
                         Attendance)
  )

# Split into training and testing
dynamos_train <- dynamos_monthly %>% filter(yearmonth <= yearmonth("2023-12"))
dynamos_test <- dynamos_monthly %>% filter(yearmonth > yearmonth("2023-12"))

# Fit ETS models
models <- dynamos_train %>%
  model(
    auto = ETS(Attendance),                                             
    anm = ETS(Attendance ~ error("A") + trend("N") + season("M")),     
    aam = ETS(Attendance ~ error("A") + trend("A") + season("M"))      
  )

# Model Performance Metrics
print("ETS Model Performance Metrics:")
print(models %>% glance())

# Generate ETS forecasts for 2024
forecasts <- models %>%
  forecast(h = "10 months")

# Plot ETS forecasts
forecasts %>%
  autoplot(dynamos_monthly) +
  labs(title = "Monthly Attendance Forecasts (ETS Models)",
       subtitle = "Including months with no games",
       y = "Attendance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check ETS forecast accuracy
print("ETS Forecast Accuracy Metrics:")
accuracy_metrics <- forecasts %>%
  accuracy(dynamos_test)
print(accuracy_metrics)

# ---- ADDING NAIVE FORECASTING MODEL ----

# Fit a naive model on the training data
naive_model <- dynamos_train %>%
  model(naive = NAIVE(Attendance))

# Generate forecasts from the naive model
naive_forecasts <- naive_model %>%
  forecast(h = "10 months")

# Calculate and print accuracy metrics of the naive forecasts
print("Naive Forecast Accuracy Metrics:")
naive_accuracy <- naive_forecasts %>%
  accuracy(dynamos_test)
print(naive_accuracy)

# Optional: Plot naive forecasts
naive_forecasts %>%
  autoplot(dynamos_monthly) +
  labs(title = "Monthly Attendance Forecasts (Naive Model)",
       subtitle = "Simple Naive Forecast vs Actual",
       y = "Attendance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The rest of your code for EDA or MLR can remain as is.
