library(readr)
library(rsample)
library(ggplot2)
library(parsnip)
library(dplyr)


# fit logisitic regression model to training set
data <- read_csv(here::here('social_network_ads','Social_Network_Ads.csv'))
dataset <- data[3:5] |>
  janitor::clean_names()

# split the data into train and test sets
set.seed(1234)


test_train_split <- initial_split(dataset, prop = 0.75)
training_set <- training(test_train_split)
test_set <- testing(test_train_split)

training_set %>% 
  head() %>% 
  knitr::kable()

# estimate linear model
lin_fit <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(purchased ~ age, data = training_set)

age_vals <- tibble(age = 10:70)

plot_data <- bind_cols(age_vals, 
          predict(lin_fit, new_data = age_vals))


plot_data %>% 
  ggplot(aes(x = age, y = .pred)) +
  geom_line() +
  geom_point(data = training_set, aes(x = age, y = purchased))

#convert purchased into factor
training_set <- training_set %>% 
  mutate(purchased = factor(purchased, 
                           levels = 0:1,
                           labels = c("no", "yes")))


#  create logistic regression model

log_mod <- logistic_reg() %>% 
  set_engine("glm")

# fit the model to the training data
log_fit <- log_mod %>% 
  fit(purchased ~ age, data = training_set)

broom::tidy(log_fit)


