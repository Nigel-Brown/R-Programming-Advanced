library(tidymodels)
library(rpart.plot)
library(vip)

data(cells, package = "modeldata")

set.seed(123)

cell_split <- initial_split(cells %>% select(-case), strata = class)

cell_train <-  training(cell_split)
cell_test <- testing(cell_split)

tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

# tune_spec
  
tree_grid <- grid_regular(
  cost_complexity(),
  tree_depth(),
  levels = 5
)

# tree_grid

tree_grid %>% count(tree_depth)

set.seed(234)

cell_folds <- vfold_cv(cell_train)

set.seed(345)

tree_wf <- workflow() %>% 
  add_model(tune_spec) %>% 
  add_formula(class ~ .)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid
  )

tree_res

tree_res %>% collect_metrics()

tree_res %>% 
  collect_metrics() %>% 
  mutate(tree_depth = factor(tree_depth)) %>% 
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) +
  scale_x_log10(labels = scales::label_number()) + 
  scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
  theme_light()

tree_res %>%
  show_best("accuracy")

best_tree <- tree_res %>%
  select_best("accuracy")

best_tree

final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)

final_fit <- 
  final_wf %>% 
  last_fit(cell_split)

final_fit %>% 
  collect_metrics()

final_fit %>% 
  collect_predictions() %>% 
  roc_curve(class, .pred_PS) %>% 
  autoplot()

final_tree <-  extract_workflow(final_fit)
final_tree

final_tree %>% 
  extract_fit_engine() %>% 
  rpart.plot(roundint = FALSE)

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()

args(decision_tree)

