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
  step_rm(datetime)   # remove original datetime column

# 3. Penalized regression model (glmnet)
preg_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

# 4. Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model)

# 5. Grid of values to tune over
grid_of_tuning_params <- grid_regular(
  penalty(),
  mixture(),
  levels = 5
)

folds <- vfold_cv(data = train, v = 10, repeats = 1)

V_results <- preg_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params,
    metrics = metric_set(rmse, mae)
  )

# 6. Plot Results
collect_metrics(V_results) %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line()

# 7. Find Best Tuning Parameters
bestTune <- V_results %>%
  select_best(metric = "rmse")

# 8. Finalize the Workflow & fit it
final_wf <- preg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = train)

# 9. Predict on test set
lin_preds <- final_wf %>%
  predict(new_data = test)

# 10. Kaggle submission (exactly as before)
kaggle_submission <- lin_preds %>%
  bind_cols(., test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

# 11. Write out the file
vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")
