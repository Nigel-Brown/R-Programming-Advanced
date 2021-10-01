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

glimpse(data)

future <- subset(data, data$Date > '2020-11-11')
dataset <- subset(data, data$Date <= '2020-11-11')

dataset$y <- ts(dataset$y, 
                frequency = 365,
                start =c(2013, yday(min(dataset$Date))))

plot.ts(dataset$y,
        ylab = "Demand",
        xlab = 'Time',
        main = 'Homeless Shelter Occupancy')

# decomposition ----
decomposition = decompose(dataset$y, type = "additive")
plot(decomposition)

ggseasonplot(dataset$y)


#training and test set
train_set = subset(dataset, dataset$Date <= '2020-09-30')
test_set = subset(dataset, dataset$Date > '2020-09-30')

#tranforming the Y variables into Timeseries
train_set$y <- ts(train_set$y, 
                     frequency = 365,
                     start = c(2013, yday(min(train_set$Date))))

test_set$y <- ts(test_set$y, 
                 frequency = 365,
                 start = c(2020, yday(min(test_set$Date))))

#Exponential smoothing
hw_model = HoltWinters(train_set$y, 
                       seasonal = "multiplicative")

#forecasting
preds_hw = forecast(hw_model, 
                    h = nrow(test_set))

# Plot HW -----
plot(preds_hw,
     main = "Holt-Winters",
     ylab = 'Demand',
     xlab = 'Time')

# HW accuracy -----
accuracy(preds_hw$mean, test_set$y)



# Exponential smoothing -----
hw_model = HoltWinters(dataset$y, 
                       seasonal = "multiplicative")

# Forecasting ----
future_hw = forecast(hw_model, 
                     h = nrow(future))

# Future HW Plot ----
plot(future_hw)

# save HW forecasts -----
write_csv(preds_hw$mean,
          here::here('time series in R', 'ensemble', 'preds_hw.csv'))

write_csv(future_hw$mean, 
          here::here('time series in R', 'ensemble', 'future_hw.csv'))











