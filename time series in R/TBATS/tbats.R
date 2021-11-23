library(readr)
library(dplyr)
library(lubridate)
library(forecast)

data <- read_csv(here::here('time series in R', 'data', 'DHS_Daily_Report.csv'))

data <- data %>% 
  select(Date, `Total Individuals in Shelter`, Easter, Thanksgiving, Christmas) %>% 
  rename(
    y = `Total Individuals in Shelter`
  )

data$Date <- mdy(data$Date)

future <- subset(data, data$Date > '2020-11-11')
dataset <- subset(data, data$Date <= '2020-11-11')

dataset$y <- ts(dataset$y, 
                frequency = 365,
                start =c(2013, yday(min(dataset$Date))))

# Training and test set -----
train_set = subset(dataset, dataset$Date <= '2020-09-30')
test_set = subset(dataset, dataset$Date > '2020-09-30')

# Transforming the Y variables into Timeseries ----
train_set$y <- ts(train_set$y, 
                  frequency = 365,
                  start = c(2013, yday(min(train_set$Date))))

# Modelling ----
tbats_model = tbats(train_set$y, 
                    seasonal.periods = c(7, 365.25))


# Forecasting ----
preds_tbats = forecast(tbats_model, 
                       h = nrow(test_set))

#Plotting
plot(preds_tbats,
     main = 'TBATS',
     ylab = 'Demand',
     xlab = 'Time')

#accuracy
accuracy(preds_tbats$mean, test_set$y)

# future -----


# Model -----
tbats_model = tbats(dataset$y,
                    seasonal.periods = c(7, 365.25))

# Forecast ----
future_tbats = forecast(tbats_model, 
                        h = nrow(test_set))

# Plot ----
plot(future_tbats, 
     ylab = 'Demand',
     xlab = 'Time',
     main = 'TBATS')

# save the forecasts -----
write.csv(preds_tbats$mean,
          here::here('time series in R', 'ensemble', 'forecast', 'preds_tbats.csv'),
          row.names = FALSE)

write.csv(future_tbats$mean, 
          here::here('time series in R', 'ensemble', 'future', 'future_tbats.csv'),
          row.names = FALSE)


