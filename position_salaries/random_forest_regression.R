library(readr)
library(magrittr)
library(rsample)
library(ggplot2)
library(randomForest)

data <-  read_csv(here::here('position_salaries',"Position_Salaries.csv"))

data <- data[,-1]

set.seed(1234)
#split <- initial_split(data, prop = 0.8)

# fit decision tree model
regressor <- randomForest(x = data[1],
                          y = data$Salary,
                          ntree = 500)
summary(regressor)


# predicting a salary
y_pred <-  predict(regressor, data.frame(Level = 6.5))
y_pred

#plot the results
x_grid <- seq(min(data$Level), max(data$Level), 0.01)
ggplot() +
  geom_point(aes(data$Level, data$Salary), color = 'blue') +
  geom_line(aes(x_grid, predict(regressor, data.frame(Level = x_grid))), color = 'red') +
  theme_light() +
  labs(
    title = "Truth or Bluff",
    subtitle = "Random Forest Regression"
  )
  

