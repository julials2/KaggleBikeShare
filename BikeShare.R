library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(glmnet)

#####
## Read in and clean train and test data
#####
bike_share <- vroom("train.csv") %>% 
  mutate(count = log(count)) %>% 
  select(-casual, -registered)

test_data <- vroom("test.csv") 

#####
## EDA
#####
weather_bar <- ggplot(bike_share, aes(x = weather)) +
  geom_bar() +
  labs(
    title = "Bike Rentals By Weather"
  )

temp_plot <- ggplot(bike_share, aes(x = temp, y = count)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(
    title = "Bike Rentals By Temp"
  )

season_bar <- ggplot(bike_share, aes(x = season)) +
  geom_bar() +
  labs(
    title = "Bike Rentals By Season"
  )

humid_plot <- ggplot(bike_share, aes(x = humidity)) +
  geom_boxplot() +
  labs(
    title = "Bike Rentals By Humidity"
  )

(weather_bar + temp_plot) / (season_bar + humid_plot)

ggsave("Bike_EDA.png")


#####
## Feature Engineering Recipe
##### 
bike_recipe <- recipe(count ~., data = bike_share) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = as.factor(weather)) %>% 
  step_mutate(season = as.factor(season)) %>% 
  step_mutate(holiday = as.factor(holiday)) %>% 
  step_mutate(workingday = as.factor(workingday)) %>% 
  step_time(datetime, features = "hour") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_rm(datetime)

prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = bike_share)

#####
## Set up and fit a penalized regression model
#####
# five combinations of penalty and mixture

# 1.41 kaggle score
# bike_share_model <- linear_reg(penalty = 5, mixture = 0.2) %>% 
#   set_engine("glmnet")

# 1.41 kaggle score
# bike_share_model <- linear_reg(penalty = 12, mixture = 0.8) %>% 
#   set_engine("glmnet")

#1.28 kaggle score
# bike_share_model <- linear_reg(penalty = 1, mixture = 0.5) %>%
#   set_engine("glmnet")

#1.19 kaggle score
# bike_share_model <- linear_reg(penalty = 2, mixture = 0.1) %>%
#   set_engine("glmnet")

#1.022 kaggle score
bike_share_model <- linear_reg(penalty = 0.1, mixture = 0.1) %>%
  set_engine("glmnet")

## Combine into a workflow and fit
bike_workflow <- workflow() %>% 
  add_recipe(bike_recipe) %>% 
  add_model(bike_share_model) %>% 
  fit(data = bike_share)

## Generate predictions using penalized linear model
bike_predictions <- predict(bike_workflow, new_data = test_data)

## Look at the predictions
bike_predictions

#####
## Format the predictions for submission to kaggle
#####
kaggle_submission <- exp(bike_predictions) %>% 
  bind_cols(., test_data) %>% 
  select(datetime, .pred) %>% 
  rename(count = .pred) %>% 
  mutate(count = pmax(0, count)) %>% 
  mutate(datetime = as.character(format(datetime)))

## Write out the file
vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")
