library(tidymodels)
library(tidyverse)
library(vroom)
library(glmnet)
library(lubridate)

# 0. Load CSV files
train <- vroom("train.csv")
test  <- vroom("test.csv")

# 1. Remove casual, registered variables
train <- train %>%
  select(-any_of(c("registered", "casual")))

# Define regression tree model
my_mod <- decision_tree(
  tree_depth = tune(),
  cost_complexity = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# 2. Recipe: encode categoricals + normalize
bike_recipe <- recipe(count ~ ., data = train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(
    hour_sin = sin(2 * pi * datetime_hour / 24),
    hour_cos = cos(2 * pi * datetime_hour / 24)
  ) %>%
  step_date(datetime, features = "dow") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_rm(datetime)

# 3. Cross-validation folds
folds <- vfold_cv(data = train, v = 10, repeats = 1)

# --- Regression Tree Section ---
tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

tree_grid <- grid_regular(
  tree_depth(),
  cost_complexity(),
  min_n(),
  levels = 5
)

tree_results <- tree_wf %>%
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    metrics = metric_set(rmse, mae)
  )

best_tree <- tree_results %>%
  select_best(metric = "rmse")

final_tree_wf <- tree_wf %>%
  finalize_workflow(best_tree) %>%
  fit(data = train)

tree_preds <- final_tree_wf %>%
  predict(new_data = test)

# --- Kaggle Submission ---
tree_submission <- tree_preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(tree_submission, file = "./TreePreds.csv", delim = ",")
