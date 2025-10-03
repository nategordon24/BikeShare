library(tidymodels)
library(tidyverse)
library(vroom)
library(glmnet)
library(lubridate)
library(bonsai)
library(lightgbm)
library(agua)
library(h2o)

h2o::h2o.init()

# Load CSV
train <- vroom("train.csv")
test  <- vroom("test.csv")

# Remove casual/registered
train <- train %>%
  select(-any_of(c("registered", "casual")))

# -----------------------
# Recipe
# -----------------------
bike_recipe <- recipe(count ~ ., data = train) %>%
  step_log(all_outcomes(), offset = 1) %>%   # log1p(count), only during training
  
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

# -----------------------
# Model
# -----------------------
auto_model <- auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 240, max_models = 50) %>% 
  set_mode("regression")

# -----------------------
# Workflow
# -----------------------
automl_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(auto_model) %>%
  fit(data = train)

# -----------------------
# Predictions (back-transform)
# -----------------------
tree_preds <- predict(automl_wf, new_data = test) %>%
  mutate(.pred = pmax(0, expm1(.pred)))   # undo log1p()

# -----------------------
# Kaggle submission
# -----------------------
tree_submission <- tree_preds %>%
  bind_cols(test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = round(count),                # optional: round to integer
         datetime = as.character(format(datetime)))

vroom_write(tree_submission, "./TreePreds.csv", delim = ",")

