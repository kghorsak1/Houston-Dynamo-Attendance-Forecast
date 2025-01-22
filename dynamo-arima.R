library(fpp3)
library(tseries)
library(ggplot2)
library(tsibble)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

dynamos_data <- read.csv("New_dynamo_data.csv")
dynamos_data <- dynamos_data %>%
  mutate(Date = if_else(str_detect(Date, as.character(Year)), Date, paste(Date, Year)))
dynamos_data <- dynamos_data %>%
  mutate(FullDate = as.Date(Date, format = "%A %B %d %Y"),
         days_since_previous = c(NA, diff(FullDate)),
         season = Year - 2019,
         Competition = as.factor(Competition),
         Opponent = as.factor(Opponent),
         Month = as.factor(Month),
         DOW = as.factor(DOW),
         Year = as.factor(Year)
  )

# Convert to tsibble
dynamos_data <- dynamos_data %>%
  mutate(index = row_number()) %>%
  as_tsibble(index = index)

# Compute first and second differences
dynamos_data %>% 
  mutate(diff.A = difference(Attendance),
         diff2.A = difference(diff.A)) -> D

D2 <- D %>% 
  filter(yearmonth(FullDate) > yearmonth("2023-12"))
D <- D %>% 
  filter(yearmonth(FullDate) <= yearmonth("2023-12"))

# Plot the original and differenced series
D %>% autoplot(Attendance) + labs(title = "Original Time Series")
D %>% autoplot(diff.A) + labs(title = "First Difference")
D %>% autoplot(diff2.A) + labs(title = "Second Difference")

D %>% ACF(Attendance) %>%
  autoplot() + labs(title = "ACF of Original Series", x = "Game Number")

D %>% PACF(Attendance) %>%
  autoplot() + labs(title = "PACF of Original Series", x = "Game Number")

D %>% ACF(diff.A) %>%
  autoplot() + labs(title = "ACF of First Difference", x = "Game Number")

D %>% PACF(diff.A) %>%
  autoplot() + labs(title = "PACF of First Difference", x = "Game Number")

D %>% ACF(diff2.A) %>%
  autoplot() + labs(title = "ACF of Second Difference", x = "Game Number")

D %>% PACF(diff2.A) %>%
  autoplot() + labs(title = "PACF of Second Difference", x = "Game Number")

D %>% features(Attendance, unitroot_kpss)
D %>% features(diff.A, unitroot_kpss)
D %>% features(diff2.A, unitroot_kpss)

D %>% features(Attendance, unitroot_ndiffs)

D$Attendance %>% adf.test()

D$diff.A %>%
  na.omit() %>%
  adf.test()

D$diff2.A %>%
  na.omit() %>%
  adf.test()

D %>% gg_tsdisplay(Attendance, plot_type = "partial")  
D %>% gg_tsdisplay(diff.A, plot_type = "partial")
D %>% gg_tsdisplay(diff2.A, plot_type = "partial")

m <- D %>% model(m1 = ARIMA(Attendance),
                 m2 = ARIMA(Attendance ~ pdq(1,1,1)),
                 m3 = ARIMA(Attendance ~ pdq(1,2,2))
                 )

m %>% select(m1) %>% report()
m %>% select(m2) %>% report()
m %>% select(m3) %>% report()

m %>% augment() %>%
  features(.resid, ljung_box, lag = 10)

m %>% select(m1) %>% gg_tsresiduals()
m %>% select(m2) %>% gg_tsresiduals()
m %>% select(m3) %>% gg_tsresiduals()

'''
D <- D %>%
  mutate(
    Competition = as.factor(Competition),
    Opponent = as.factor(Opponent),
    Month = as.factor(Month),
    DOW = as.factor(DOW)
  )
  
'''

m2 <- D %>% model(m1 = ARIMA(Attendance ~ temperature + Month + DOW + rain),
                  m2 = ARIMA(Attendance ~ temperature + Month + DOW + rain + season),
                  m3 = ARIMA(Attendance ~ temperature + Month + DOW + rain + days_since_previous),
                  m4 = ARIMA(Attendance ~ temperature + Month + DOW + rain + days_since_previous + Opponent),
                  m5 = ARIMA(Attendance),
                  m6 = ARIMA(Attendance ~ pdq(1, 1, 1) + Month + DOW + rain))

m2 %>% select(m1) %>% report()
m2 %>% select(m2) %>% report()
m2 %>% select(m3) %>% report()
m2 %>% select(m4) %>% report()
m2 %>% select(m5) %>% report()
m2 %>% select(m6) %>% report()

m %>% glance
m %>% accuracy
m2 %>% glance
m2 %>% accuracy
m2 %>% gg_arma

forecast_results <- m2 %>%
  select(m6) %>%  # Select the second model
  forecast(new_data = D2)

# Plot the forecast
autoplot(forecast_results)

# Plot the training set, forecast results, and test set
autoplot(D, Attendance, series = "Training Set") +
  autolayer(D2, Attendance, series = "Test Set", color = "blue") +
  autolayer(forecast_results, series = "Forecast", color = "red", alpha = 0.5) + # Adjust opacity
  labs(title = "ARIMAX Forecast w/o Opponent",
       x = "Time",
       y = "Attendance",
       color = "Legend") +  # Add a legend title
  scale_color_manual(values = c("Training Set" = "black", "Test Set" = "blue", "Forecast" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

rmse <- sqrt(mean((D2$Attendance - forecast_results$.mean)^2))
rmse

