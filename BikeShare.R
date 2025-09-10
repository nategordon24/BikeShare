library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(GGally)

sample <- vroom("sampleSubmission.csv")
train <- vroom("train.csv")
test <- vroom("test.csv")

glimpse(train)
ggplot(data=train, aes(x=temp, y=count)) + geom_point()
