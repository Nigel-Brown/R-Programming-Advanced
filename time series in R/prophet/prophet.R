library(readr)
library(dplyr)
library(lubridate)
library(prophet)

data <- read_csv(here::here('time series in R', 'data', 'DHS_Daily_Report.csv'))

data <- data %>% 
  select(Date, `Total Individuals in Shelter`, Easter, Thanksgiving, Christmas) %>% 
  rename(
    y = `Total Individuals in Shelter`
  )

data$Date <- mdy(data$Date)

future <- subset(data, data$Date > '2020-11-11')
dataset <- subset(data, data$Date <= '2020-11-11')

# Training and test set -----
train_set <-  subset(dataset, dataset$Date <= '2020-09-30')
test_set <-  subset(dataset, dataset$Date > '2020-09-30')

# Holidays - Easter ----
easter_dates <-  subset(data$Date, data$Easter == 1)
easter <- tibble(holiday = 'Easter',
                 ds = as.Date(easter_dates),
                 lower_window = -3,
                 upper_window = 1)

# Holidays - Thanksgiving ----
thanksgiving_dates <-  subset(data$Date, data$Thanksgiving == 1)
thanksgiving <- tibble(holiday = 'Thanksgiving',
                 ds = as.Date(thanksgiving_dates),
                 lower_window = -7,
                 upper_window = 4)

# Holidays - Christmas ----
christmas_dates <-  subset(data$Date, data$Christmas == 1)
christmas <- tibble(holiday = 'Christmas',
                       ds = as.Date(christmas_dates),
                       lower_window = -4,
                       upper_window = 3)

holidays <- bind_rows(easter, thanksgiving, christmas)

# dataframe prep ----
df <- train_set %>% 
  rename(ds = Date) %>% 
  select(ds, y)

m <- prophet(growth = 'linear',
             yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE, 
             holidays = holidays,
             seasonality.mode = 'additive',
             seasonality.prior.scale = 10,
             holidays.prior.scale = 10,
             changepoint.prior.scale = 0.005
             )

m  <-  fit.prophet(m, df)


# create preds dataframe ----
test_period <- make_future_dataframe(m, periods = nrow(test_set))
tail(test_period)

# predictions ----
prophet_forecast <-  predict(m, test_period)

# plots ----
plot(m, prophet_forecast)
prophet_plot_components(m, prophet_forecast)
plot(m, prophet_forecast) + add_changepoints_to_plot(m)

# accuracy ----
predictions_prophet <- tail(prophet_forecast$yhat, nrow(test_set))
forecast::accuracy(predictions_prophet, test_set$y)

# cross validation ----
cv = cross_validation(m,
                      horizon = 31,
                      initial = 2100,
                      units = "days")

forecast::accuracy(cv$yhat, cv$y)

# full data ----

df <- dataset %>% rename(ds = Date) %>% select(ds, y)

m <- prophet(growth = 'linear',
             yearly.seasonality = TRUE,
             weekly.seasonality = TRUE,
             daily.seasonality = FALSE,
             holidays = holidays,
             seasonality.mode = 'additive',
             seasonality.prior.scale = 10,
             holidays.prior.scale = 10,
             changepoint.prior.scale = 0.005
)

m  <-  fit.prophet(m, df)

# create preds dataframe ----
future_period <- make_future_dataframe(m, periods = nrow(future))

prophet_forecast <-  predict(m, future_period)

# plots ----
plot(m, prophet_forecast)

#getting future forecasts
future_prophet <- tail(prophet_forecast$yhat, nrow(future))


# save the forecasts -----
write.csv(predictions_prophet,
          here::here('time series in R', 'ensemble', 'forecast', 'predictions_prophet.csv'),
          row.names = FALSE)

write.csv(future_prophet, 
          here::here('time series in R', 'ensemble', 'future', 'future_prophet.csv'),
          row.names = FALSE)
