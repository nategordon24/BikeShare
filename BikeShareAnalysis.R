library(tidymodels)
library(tidyverse)
library(dplyr)
library(vroom)
library(glmnet)
library(lubridate)

# 0. Load CSV files
train <- vroom("train.csv")
test  <- vroom("test.csv")

# 1. Remove casual, registered variables, change count to log1p(count)
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
  penalty = 0.75,
  mixture = 1
) %>%
  set_engine("glmnet")

# 4. Workflow
preg_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data = train)

# 5. Predict on test and back-transform with expm1
lin_preds <- predict(preg_wf, new_data = test) %>%
  mutate(count = pmax(0, .pred))

# 6. Kaggle submission (exactly as before)
kaggle_submission <- lin_preds %>%
  bind_cols(., test) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

# 7. Write out the file
vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")
