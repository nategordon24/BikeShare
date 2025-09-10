library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(GGally)

sample <- vroom("sampleSubmission.csv")
train <- vroom("train.csv")
test <- vroom("test.csv")

train$weather <- factor(train$weather, levels = 1:4)
train$season <- factor(
  train$season,
  levels = 1:4,
  labels = c("Spring", "Summer", "Fall", "Winter")
)


temperature <- ggplot(data=train, aes(x=temp, y=count)) + geom_point() + geom_smooth() + labs(x="Temparature (celsius)", y = "Count")
weather <- ggplot(data=train, aes(x=weather,y=count)) + geom_col() + labs(x = "Weather Type", y = "Total Rental Count")

avg_season <- train |>
  group_by(season) |>
  summarize(avg_count = mean(count))

season <- ggplot(avg_season, aes(x=season, y = avg_count)) + geom_col() + labs(x="Season", y = "Average Count")

library(dplyr)
library(ggplot2)
library(lubridate)

avg_hour <- train |>
  mutate(hour = hour(datetime)) |>
  group_by(hour) |>
  summarise(avg_count = mean(count), .groups = "drop")

hours <- ggplot(avg_hour, aes(x = hour, y = avg_count)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(x = "Hour of Day", y = "Average Rentals")

library(patchwork)
(temperature + weather) / (season + hours)

