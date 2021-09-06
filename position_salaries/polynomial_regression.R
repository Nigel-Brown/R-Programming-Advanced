library(readr)
library(magrittr)
library(rsample)
library(ggplot2)

data <-  read_csv(here::here('position_salaries',"Position_Salaries.csv"))

data <- data[,-1]
 
#set.seed(123)
#split <- initial_split(data, prop = 0.8)

# Fit Linear Regression
lin_reg <- lm(Salary ~ ., data)
summary(lin_reg)

# fit Polynomial Regression
data$Level2 <- data$Level^2
data$Level3 <- data$Level^3
data$Level4 <- data$Level^4
poly_reg <- lm(Salary ~., data)
summary(poly_reg)

#plot the results
data %>% 
  ggplot() +
  geom_point(aes(Level, Salary), color = 'blue') +
  geom_line(aes(data$Level, predict(lin_reg, data)), color = 'red') +
  theme_light() +
  labs(
    title = "Truth or Bluff",
    subtitle = "Linear Regression"
  )

data %>% 
  ggplot() +
  geom_point(aes(Level, Salary), color = 'blue') +
  geom_line(aes(Level, predict(poly_reg, data)), color = 'red') +
  theme_light() +
  labs(
    title = "Truth or Bluff",
    subtitle = "Polynomial Regression"
  )


# predicting a salary
# linear regression

y_pred <-  predict(lin_reg, data.frame(Level = 6.5))
y_pred

# polynomial regression
y_pred <-  predict(poly_reg, data.frame(Level = 6.5, Level2 = 6.5^2, Level3 = 6.5^3, Level4 = 6.5^4))
y_pred
