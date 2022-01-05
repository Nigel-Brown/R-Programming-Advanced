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


yday(min(dataset$Date))
# transform Y variables into TS ----
dataset$y <-  ts(dataset$y,
                 frequency = 365,
                 start = c(2013, yday(min(dataset$Date))))

future$y <-  ts(future$y,
                 frequency = 365,
                 start = c(2020, yday(min(dataset$Date))))

plot.ts(dataset$y, 
        ylab = "Demand")

# Training & test sets ----
training_set = subset(dataset, dataset$Date <= '2020-09-30')
test_set = subset(dataset, dataset$Date > '2020-09-30')

#transforming the Y variables into TS
training_set$y <- ts(training_set$y, 
                     frequency = 365,
                     start = c(2013, yday(head(training_set$Date, 1))))

test_set$y <- ts(test_set$y, 
                 frequency = 365,
                 start = c(2020, yday(head(test_set$Date, 1))))

# get the regressors ----
training_reg = as.matrix(training_set[,3:5])
test_reg = as.matrix(test_set[,3:5])

# Auto Regressive Neural Networks ----
nnetar_model <- nnetar(y = training_set$y,
                       P = 1,
                       decay = 0.1,
                       size = 2,
                       xreg = training_reg)

# forecasting ----
predictions_nnetar = forecast(nnetar_model, xreg = test_reg)
plot(predictions_nnetar)

# accuracy ----
accuracy(predictions_nnetar$mean, test_set$y)

# get the regressors full data ----
training_reg = as.matrix(dataset[,3:5])
test_reg = as.matrix(future[,3:5])

nnetar_model <- nnetar(y = dataset$y,
                       P = 1,
                       decay = 0.1,
                       size = 2,
                       xreg = training_reg)

# forecasting full data ----
future_nnetar = forecast(nnetar_model, xreg = test_reg)
plot(future_nnetar)

#save forecasts
write.csv(predictions_nnetar$mean, 
          here::here('time series in R', 'ensemble', 'forecast', 'predictions_nnetar.csv'),
          row.names = FALSE)

write.csv(future_nnetar$mean, 
          here::here('time series in R', 'ensemble', 'future', 'future_nnetar.csv'),
          row.names = FALSE)  

