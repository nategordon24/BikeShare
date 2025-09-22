library(tidymodels)
library(tidyverse)
library(vroom)
library(glmnet)
library(lubridate)

# Load CSV
train <- vroom("train.csv")
test  <- vroom("test.csv")

# Remove casual/registered
train <- train %>%
  select(-any_of(c("registered", "casual")))

# Recipe
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

# Define model
my_mod <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Workflow
tree_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(my_mod)

# Grid
tree_grid <- grid_regular(
  mtry(range = c(1, 17)),
  min_n(),
  levels = 5
)

# CV folds
folds <- vfold_cv(train, v = 10)

# Tune
tree_results <- tree_wf %>%
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    metrics = metric_set(rmse, mae)
  )

best_tree <- tree_results %>% select_best(metric = "rmse")

final_tree_wf <- tree_wf %>% finalize_workflow(best_tree) %>% fit(train)

tree_preds <- predict(final_tree_wf, new_data = test)

# Kaggle submission
tree_submission <- tree_preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count),
         datetime = as.character(format(datetime)))

vroom_write(tree_submission, "./TreePreds.csv", delim = ",")
