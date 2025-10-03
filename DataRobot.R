library(tidymodels)
library(tidyverse)
library(vroom)

# Load train
train <- vroom("train.csv") %>%
  select(-any_of(c("registered", "casual")))

# Recipe (no log transform so DataRobot sees raw count)
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

# Prep and bake
prepped_recipe <- prep(bike_recipe)
baked_train <- bake(prepped_recipe, new_data = NULL)

# Export for DataRobot
write.csv(baked_train, "train_preprocessed.csv", row.names = FALSE)

# Load test
test <- vroom("test.csv")

# Bake test set with the same recipe
baked_test <- bake(prepped_recipe, new_data = test)

# Export for DataRobot predictions
write.csv(baked_test, "test_preprocessed.csv", row.names = FALSE)




# Load original test with datetime
test <- vroom("test.csv")

# Load predictions (assume single column, e.g. "prediction")
datarobot_preds <- vroom("datarobotresults.csv", delim = ",")


tree_submission <- datarobot_preds %>%
  bind_cols(test) %>%
  select(datetime, count_PREDICTION) %>%
  rename(count = count_PREDICTION) %>%
  mutate(count = pmax(0, count),
         datetime = as.character(format(datetime)))

vroom_write(tree_submission, "./DataRobot.csv", delim = ",")
