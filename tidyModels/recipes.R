library(tidymodels)
library(nycflights13)
library(skimr)

flight_data <- flights %>% 
  mutate(
    arr_delay = factor(ifelse(arr_delay >= 30 , 'late', 'ontime')),
    date = lubridate::as_date(time_hour)
  ) %>% 
inner_join(weather, by = c('origin', 'time_hour')) %>% 
  select(dep_time, flight, origin, dest, air_time, distance, carrier, date, arr_delay, time_hour) %>% 
  na.omit() %>% 
  mutate_if(is.character, as.factor)

glimpse(flight_data)


flight_data %>% 
  skim(dest, carrier)

# split the data ----
set.seed(222)

data_split <-  initial_split(flight_data, prop = 3/4)

train_data <- training(data_split)
test_data <- testing(data_split)


flights_rec <- recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>% 
  step_holiday(date,
               holidays = timeDate::listHolidays("US"),
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())

lr_mod <- logistic_reg() %>% 
  set_engine("glm")

flights_wflow <- workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)

flights_fit <- flights_wflow %>% 
  fit(data = train_data)

flights_fit %>% 
  extract_fit_parsnip() %>% 
  tidy


predict(flights_fit, test_data)

flights_aug <- augment(flights_fit, test_data)

flights_aug %>% 
  select(arr_delay, time_hour, flight, .pred_class, .pred_ontime)

flights_aug %>% 
  roc_curve(truth = arr_delay,
            .pred_late) %>% 
  autoplot()

flights_aug %>% 
  roc_auc(truth = arr_delay,
            .pred_late)


