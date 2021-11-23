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

plot.ts(dataset$y,
        ylab = "Demand",
        xlab = 'Time',
        main = 'Homeless Shelter Occupancy')


# Training and test set -----
train_set = subset(dataset, dataset$Date <= '2020-09-30')
test_set = subset(dataset, dataset$Date > '2020-09-30')

# Transforming the Y variables into Timeseries ----
train_set$y <- ts(train_set$y, 
                  frequency = 365,
                  start = c(2013, yday(min(train_set$Date))))

# Stationarity check ----
ndiffs(train_set$y, alpha = 0.05, test = c("adf"))

# Set the regressors ----
train_reg = as.matrix(train_set[,3:5])
test_reg = as.matrix(test_set[,3:5])

# Modelling ----
sarimax_model = auto.arima(train_set$y, 
                           xreg = train_reg)
summary(sarimax_model)

# Forecasting ----
preds_sarimax = forecast(sarimax_model, xreg = test_reg)

#Plotting
plot(preds_sarimax,
     main = 'SARMIMAX',
     ylab = 'Demand',
     xlab = 'Time')

#accuracy
accuracy(preds_sarimax$mean, test_set$y)

# future -----

# Regressors ----
train_reg = as.matrix(dataset[,3:5])
test_reg = as.matrix(future[,3:5])

# Model -----
sarimax_model = auto.arima(dataset$y, 
                           xreg = train_reg)
summary(sarimax_model)

# Forecast ----
future_sarimax = forecast(sarimax_model, 
                          xreg = test_reg)

# Plot ----
plot(future_sarimax, 
     ylab = 'Demand',
     xlab = 'Time',
     main = 'SARIMAX')

# save the forecasts -----
write.csv(preds_hw$mean,
          here::here('time series in R', 'ensemble', 'forecast', 'preds_sarimax.csv'),
          row.names = FALSE)

write.csv(future_hw$mean, 
          here::here('time series in R', 'ensemble', 'future', 'future_sarimax.csv'),
          row.names = FALSE)











