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

# Load datasets
data_hw = read.csv(here::here('time series in R', 'ensemble', 'forecast', 'preds_hw.csv'))
colnames(data_hw)[1] = "holt_winters"

data_sarimax = read.csv(here::here('time series in R', 'ensemble', 'forecast', "preds_sarimax.csv"))
colnames(data_sarimax)[1] = "sarimax"

data_tbats = read.csv(here::here('time series in R', 'ensemble', 'forecast', "preds_tbats.csv"))
colnames(data_tbats)[1] = "tbats"

data_prophet = read.csv(here::here('time series in R', 'ensemble', 'forecast',"predictions_prophet.csv"))
colnames(data_prophet)[1] = "prophet"

data_nnetar = read.csv(here::here('time series in R', 'ensemble', 'forecast',"predictions_nnetar.csv"))
colnames(data_nnetar)[1] = "nnetar"

# cleaning test set ----
test_set <- test_set[, 1:2]

# merging everything ----
ensemble <- cbind(test_set,
                  data_hw,
                  data_sarimax,
                  data_tbats,
                  data_prophet,
                  data_nnetar)

# creating average forecast ----
ensemble <- transform(ensemble,
                      ensemble_forecast = rowMeans(ensemble[,3:ncol(ensemble)]))

# accuracy ----
accuracy(ensemble$ensemble_forecast, ensemble$y)


#plotting
library(ggplot2)
ensemble %>% 
  tidyr::gather(key = "forecast", value = "value", -Date) %>% 
  ggplot(aes(x = Date, y = value)) + 
  geom_line(aes(color = forecast), size = 1) +
  theme_minimal()