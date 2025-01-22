
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)


dynamos_data <- read.csv("/Users/Kimble/Downloads/New_dynamo_data.csv")

print(dynamos_data)

# Add Year to Dates dates where it is missing
dynamos_data <- dynamos_data %>%
  mutate(Date = if_else(str_detect(Date, as.character(Year)), Date, paste(Date, Year)))

# Convert to date format
dynamos_data <- dynamos_data %>%
  mutate(FullDate = as.Date(Date, format = "%A %B %d %Y"))


# Verify the Processed Data
print(dynamos_data %>% select(FullDate, Attendance))

# Plot Attendance Over Time
ggplot(dynamos_data, aes(x = FullDate, y = Attendance)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Attendance Over Time",
    x = "Date",
    y = "Attendance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Average Monthly Attendance 
dynamos_data %>%
  mutate(Month = floor_date(FullDate, "month")) %>%
  group_by(Month) %>%
  summarize(MonthlyAttendance = mean(Attendance)) %>%
  ggplot(aes(x = Month, y = MonthlyAttendance)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Attendance Trends", x = "Month", y = "Average Attendance")

# Average attendance by day of the week
dynamos_data %>%
  mutate(Weekday = wday(FullDate, label = TRUE)) %>%
  group_by(Weekday) %>%
  summarize(AverageAttendance = mean(Attendance)) %>%
  ggplot(aes(x = Weekday, y = AverageAttendance, fill = Weekday)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Attendance by Day of the Week", x = "Day", y = "Average Attendance")

# attendance distributions
ggplot(dynamos_data, aes(x = Attendance)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "white") +
  labs(title = "Distribution of Attendance", x = "Attendance", y = "Frequency")

# attendance by match type 
dynamos_data %>%
  group_by(Competition) %>%
  summarize(AverageAttendance = mean(Attendance)) %>%
  ggplot(aes(x = Competition, y = AverageAttendance, fill = Competition)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Attendance by Competition", x = "Competition", y = "Average Attendance")

# Average Attendance by Opponent
dynamos_data %>%
  group_by(Opponent) %>%
  summarize(AverageAttendance = mean(Attendance)) %>%
  arrange(desc(AverageAttendance)) %>%
  ggplot(aes(x = reorder(Opponent, -AverageAttendance), y = AverageAttendance, fill = Opponent)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Attendance by Opponent",
    x = "Opponent",
    y = "Average Attendance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )





#Additional Goal EDA, Want to see if there is a correlation between attendance and goals or attendance and match result
# Pie chart with percentages
result_plot <- dynamos_data %>%
  count(Result) %>%
  mutate(percentage = n/sum(n) * 100) %>%
  ggplot(aes(x = "", y = percentage, fill = Result)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("D" = "#FDB45C", "L" = "#FF5733", "W" = "#46BFBD")) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Match Results Distribution") +
  theme_minimal() +
  theme(axis.text = element_blank())

print(result_plot)

# Create three separate plots
goals_scored_plot <- ggplot(dynamos_data) +
  geom_point(aes(x = Attendance, y = Goals_Scored), color = "#46BFBD", size = 3) +
  geom_smooth(aes(x = Attendance, y = Goals_Scored), method = "lm", se = FALSE, color = "#46BFBD", linetype = "dashed") +
  labs(title = "Goals Scored vs Attendance",
       x = "Attendance",
       y = "Goals Scored") +
  theme_minimal()

goals_received_plot <- ggplot(dynamos_data) +
  geom_point(aes(x = Attendance, y = Goals_Received), color = "#FF5733", size = 3) +
  geom_smooth(aes(x = Attendance, y = Goals_Received), method = "lm", se = FALSE, color = "#FF5733", linetype = "dashed") +
  labs(title = "Goals Received vs Attendance",
       x = "Attendance",
       y = "Goals Received") +
  theme_minimal()

total_goals_plot <- ggplot(dynamos_data) +
  geom_point(aes(x = Attendance, y = Goals_Scored + Goals_Received), color = "#FDB45C", size = 3) +
  geom_smooth(aes(x = Attendance, y = Goals_Scored + Goals_Received), method = "lm", se = FALSE, color = "#FDB45C", linetype = "dashed") +
  labs(title = "Total Goals vs Attendance",
       x = "Attendance",
       y = "Total Goals") +
  theme_minimal()

print(total_goals_plot)
print(goals_scored_plot)
print(goals_received_plot)

# Arrange plots in a grid
library(gridExtra)
grid.arrange(goals_scored_plot, goals_received_plot, total_goals_plot, ncol = 1)

# Calculate average attendance by result
attendance_plot <- dynamos_data %>%
  group_by(Result) %>%
  summarise(avg_attendance = mean(Attendance)) %>%
  ggplot(aes(x = Result, y = avg_attendance, fill = Result)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("D" = "#FDB45C", "L" = "#FF5733", "W" = "#46BFBD")) +
  geom_text(aes(label = sprintf("%.0f", avg_attendance)), 
            position = position_stack(vjust = 0.85)) +
  labs(title = "Average Attendance by Match Result",
       x = "Result",
       y = "Average Attendance") +
  theme_minimal()

print(attendance_plot)



# Print duplicate dates with their details
dynamos_data %>%
  mutate(date = parse_date_time(Date, orders = "A B d Y")) %>%
  group_by(date) %>%
  filter(n() > 1) %>%
  arrange(date) %>%
  select(Date, Goals_Scored, Goals_Received, Opponent, Competition) %>%
  print(n = Inf)  # Print all rows


#Here we create the ETS models

# View the structure of dates and intervals
dynamos_data %>%
  mutate(date = parse_date_time(Date, orders = "A B d Y")) %>%
  arrange(date) %>%
  mutate(days_between = c(NA, diff(date))) %>%
  select(Date, date, days_between)

print(dynamos_data)

library(fpp3)
library(tsibble)
library(dplyr)

dynamos_monthly <- dynamos_data %>%
  mutate(date = parse_date_time(Date, orders = "A B d Y")) %>%
  mutate(yearmonth = yearmonth(date)) %>%
  group_by(yearmonth) %>%
  summarise(
    Attendance = mean(Attendance)
  ) %>%
  as_tsibble(index = yearmonth) %>%
  fill_gaps() %>%
  # Use linear interpolation for attendance
  mutate(
    Attendance = zoo::na.approx(Attendance, na.rm = FALSE)    # Have to interpolate bc ETS models need consistent time frame? Might be a different way to do this
  ) %>%
  # Handle any remaining NAs at the start or end of the series
  mutate(
    Attendance = if_else(is.na(Attendance),
                         mean(Attendance, na.rm = TRUE),
                         Attendance)
  )


print(dynamos_monthly)

# First visualization of gaps
ggplot(dynamos_monthly, aes(x = yearmonth)) +
  geom_line(aes(y = Attendance)) +
  geom_point(aes(y = Attendance)) +
  labs(title = "Monthly Attendance (Including Months with No Games)",
       x = "Month",
       y = "Average Attendance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Split into training and test
dynamos_train <- dynamos_monthly %>% 
  filter(yearmonth <= yearmonth("2023-12"))
dynamos_test <- dynamos_monthly %>% 
  filter(yearmonth > yearmonth("2023-12"))

print(dynamos_train)

# Visualize train-test split
ggplot() +
  geom_line(data = dynamos_train, aes(x = yearmonth, y = Attendance, color = "Training"), size = 1) +
  geom_point(data = dynamos_train, aes(x = yearmonth, y = Attendance, color = "Training"), size = 3) +
  geom_line(data = dynamos_test, aes(x = yearmonth, y = Attendance, color = "Test"), size = 1) +
  geom_point(data = dynamos_test, aes(x = yearmonth, y = Attendance, color = "Test"), size = 3) +
  scale_color_manual(values = c("Training" = "blue", "Test" = "red")) +
  labs(title = "Monthly Attendance: Training vs Test Sets",
       subtitle = "Training: 2019-2023, Test: 2024",
       x = "Month",
       y = "Average Attendance",
       color = "Dataset") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Fit expanded set of ETS models
#models <- dynamos_train %>%
#  model(
#    auto = ETS(Attendance),                                              # Automatic selection
#    ana = ETS(Attendance ~ error("A") + trend("N") + season("A")),      # Additive error, No trend, Additive seasonal
#    anm = ETS(Attendance ~ error("A") + trend("N") + season("M")),      # Additive error, No trend, Multiplicative seasonal
#    aan = ETS(Attendance ~ error("A") + trend("A") + season("N")),      # Additive error, Additive trend, No seasonal
#    aan_damped = ETS(Attendance ~ error("A") + trend("Ad") + season("N")), # Additive error, Damped trend, No seasonal
#    aaa = ETS(Attendance ~ error("A") + trend("A") + season("A")),      # Additive error, Additive trend, Additive seasonal
#    aam = ETS(Attendance ~ error("A") + trend("A") + season("M")),      # Additive error, Additive trend, Multiplicative seasonal
#    mnn = ETS(Attendance ~ error("M") + trend("N") + season("N")),      # Multiplicative error, No trend, No seasonal
#    mna = ETS(Attendance ~ error("M") + trend("N") + season("A")),      # Multiplicative error, No trend, Additive seasonal
#    mnm = ETS(Attendance ~ error("M") + trend("N") + season("M")),      # Multiplicative error, No trend, Multiplicative seasonal
#    mmm = ETS(Attendance ~ error("M") + trend("M") + season("M")),      # Multiplicative error, Multiplicative trend, Multiplicative seasonal
#    man = ETS(Attendance ~ error("M") + trend("A") + season("N")),      # Multiplicative error, Additive trend, No seasonal
#    mam = ETS(Attendance ~ error("M") + trend("A") + season("M")),      # Multiplicative error, Additive trend, Multiplicative seasonal
#    maa = ETS(Attendance ~ error("M") + trend("A") + season("A"))       # Multiplicative error, Additive trend, Additive seasonal
#  )

# Fit expanded set of ETS models
models <- dynamos_train %>%
  model(
    auto = ETS(Attendance),                                              # Automatic selection
    anm = ETS(Attendance ~ error("A") + trend("N") + season("M")),      # Additive error, No trend, Multiplicative seasonal
    aam = ETS(Attendance ~ error("A") + trend("A") + season("M")),      # Additive error, Additive trend, Multiplicative seasonal
  )

# Model summary
print("Model Performance Metrics:")
print(models %>% glance())

# Generate forecasts for 2024
forecasts <- models %>%
  forecast(h = "10 months")

# Plot forecasts
forecasts %>%
  autoplot(dynamos_monthly) +
  labs(title = "Monthly Attendance Forecasts",
       subtitle = "Including months with no games",
       y = "Attendance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check forecast accuracy
print("Forecast Accuracy Metrics:")
accuracy_metrics <- forecasts %>%
  accuracy(dynamos_test)
print(accuracy_metrics)

# 1. Components Plot for both models
models %>%
  select(auto, anm, aam) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposition of Components for MNM Model") +
  theme_minimal()

# 2. Comparison of Fitted Values vs Actual
augment(models) %>%
  filter(.model %in% c("auto", "anm", "aam")) %>%
  ggplot(aes(x = yearmonth)) +
  geom_line(aes(y = Attendance, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  facet_wrap(~.model, ncol = 1) +
  scale_color_manual(values = c("Actual" = "black", "Fitted" = "blue")) +
  labs(title = "Actual vs Fitted Values",
       y = "Attendance",
       color = "Series") +
  theme_minimal()

# 3. Forecast Fan Charts
forecasts %>%
  filter(.model %in% c("auto", "anm", "aam")) %>%
  autoplot(dynamos_monthly, level = c(80, 95)) +
  facet_wrap(~.model, ncol = 1) +
  labs(title = "Forecast Fan Charts",
       subtitle = "80% and 95% prediction intervals",
       y = "Attendance") +
  theme_minimal()








#Multi-Linear Regression as a baseline to see variable importance and because ETS models are univariate so I wanted to use other variable
library(tidyverse)
library(caret)
library(corrplot)
library(car)
library(fastDummies)

# Create MLR dataframe with one-hot encoding
dynamos_MLR_df <- dynamos_data %>%
  # Convert categorical variables to factors
  mutate(
    Competition = as.factor(Competition),
    Opponent = as.factor(Opponent),
    Month = as.factor(Month),
    DOW = as.factor(DOW)
  ) %>%
  # One-hot encode categorical variables
  select(Competition, Opponent, Attendance, Month, DOW, temperature, rain) %>%
  dummy_cols(c("Competition", "Opponent", "Month", "DOW"), remove_selected_columns = TRUE)

# Split data into training and testing sets
set.seed(123)
train_index <- createDataPartition(dynamos_MLR_df$Attendance, p = 0.8, list = FALSE)
train_data <- dynamos_MLR_df[train_index, ]
test_data <- dynamos_MLR_df[-train_index, ]

# Fit MLR model
mlr_model <- lm(Attendance ~ ., data = train_data)

# Model summary
summary_stats <- summary(mlr_model)
print(summary_stats)

# Actual vs Predicted Plot
predictions <- predict(mlr_model, test_data)
ggplot(data = data.frame(actual = test_data$Attendance, predicted = predictions)) +
  geom_point(aes(x = actual, y = predicted)) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Attendance",
       x = "Actual Attendance",
       y = "Predicted Attendance") +
  theme_minimal()

# Variable Importance Plot
var_importance <- abs(coef(mlr_model)[-1])  # Exclude intercept
var_importance_df <- data.frame(
  Variable = names(var_importance),
  Importance = as.numeric(var_importance)
) %>%
  arrange(desc(Importance)) %>%
  head(20)  # Top 20 most important variables

ggplot(var_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Most Important Variables",
       x = "Variables",
       y = "Absolute Coefficient Value") +
  theme_minimal()

# Model performance metrics
performance_metrics <- data.frame(
  RMSE = sqrt(mean((test_data$Attendance - predictions)^2)),
  MAE = mean(abs(test_data$Attendance - predictions)),
  R2 = summary_stats$r.squared
)
print(performance_metrics)

