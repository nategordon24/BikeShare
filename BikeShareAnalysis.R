library(tidymodels)
library(tidyverse)
library(dplyr)
library(vroom)

train <- vroom("train.csv")
test <- vroom("test.csv")

#1. Remove casual, registered variables, change count to log(count)
train <- train |>
  select(-c(registered, casual)) |>
  mutate(count = log(count))

#2. Feature engineering (define recipe)
bike_recipe <- recipe(count~.,data=train) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather)) %>%
  step_date(datetime,features = "hour") %>%
  step_mutate(season = factor(season)) %>%
  step_date(timestamp, features="dow")
  
my_linear_model <- linear_reg() %>%
set_engine("lm") %>%
set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
add_recipe(bike_recipe) %>%
add_model(lin_model) %>%
fit(data=train)

## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = test) %>%
  mutate(count = exp(.pred))

# Prep and bake the recipe
prepped_recipe <- prep(bike_recipe)
baked_train <- bake(prepped_recipe, new_data = train)

head(baked_train, 5)



kaggle_submission <- bike_predictions %>%
bind_cols(., test) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")