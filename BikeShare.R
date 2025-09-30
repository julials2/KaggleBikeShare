library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(glmnet)
library(bonsai)
library(lightgbm)
library(agua)

#####
## Read in and clean train and test data
#####
training_data <- vroom("train.csv") %>% 
  select(-casual, -registered) %>% 
  mutate(count = log(count))

test_data <- vroom("test.csv") 

#####
## EDA
#####
# weather_bar <- ggplot(training_data, aes(x = weather)) +
#   geom_bar() +
#   labs(
#     title = "Bike Rentals By Weather"
#   )
# 
# temp_plot <- ggplot(training_data, aes(x = temp, y = count)) +
#   geom_point() +
#   geom_smooth(se = F) +
#   labs(
#     title = "Bike Rentals By Temp"
#   )
# 
# season_bar <- ggplot(training_data, aes(x = season)) +
#   geom_bar() +
#   labs(
#     title = "Bike Rentals By Season"
#   )
# 
# humid_plot <- ggplot(training_data, aes(x = humidity)) +
#   geom_boxplot() +
#   labs(
#     title = "Bike Rentals By Humidity"
#   )
# 
# (weather_bar + temp_plot) / (season_bar + humid_plot)
# 
# ggsave("Bike_EDA.png")

#####
## Create models
#####
# tree_mod <- rand_forest(mtry = tune(), 
#                         min_n = tune(), 
#                         trees = 1000) %>% 
#   set_engine("ranger") %>% 
#   set_mode("regression")

bart_model <- bart(trees = tune()) %>%
  set_engine("dbarts") %>%
  set_mode("regression")

#####
## Feature Engineering Recipe
##### 
bike_recipe <- recipe(count ~., data = training_data) %>% 
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>% 
  step_mutate(weather = as.factor(weather)) %>% 
  step_mutate(holiday = as.factor(holiday)) %>% 
  step_mutate(workingday = as.factor(workingday)) %>% 
  step_time(datetime, features = "hour")%>% 
  step_mutate(season = as.factor(season)) %>% 
  step_date(datetime, features = "dow") %>%
  step_mutate(dec_date = decimal_date(date(datetime))) %>% 
  step_mutate(sin_temp = sin(temp)) %>% 
  step_mutate(sin_atemp = sin(atemp)) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_rm(datetime)

prepped_recipe <- prep(bike_recipe)
bake(prepped_recipe, new_data = training_data)

#####
## Set up and fit a penalized regression model
#####
## Penalized regression model
# bike_share_model <- linear_reg(penalty = tune(),
#                                mixture = tune()) %>%
#   set_engine("glmnet")

# h2o::h2o.init()
# 
# auto_model <- auto_ml() %>%
#   set_engine("h2o", max_runtime_secs = 300, max_models = 5) %>%
#   set_mode("regression")

## Combine into a workflow and fit
bart_workflow <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(bart_model)

# automl_wf <- workflow() %>%
#   add_recipe(bike_recipe) %>%
#   add_model(auto_model) %>%
#   fit(data = training_data)

#####
## Create cross-validation
#####
L <- 5
K <- 10

# ## Grid of values to tune over
grid_of_tuning_params <- grid_regular(trees())

# ## Split data for CV
folds <- vfold_cv(training_data, v = K, repeats = 1)

# ## Run the CV
CV_results <- bart_workflow %>%
  tune_grid(resamples = folds,
  grid = grid_of_tuning_params,
  metrics = metric_set(rmse, mae))

grid_of_tuning_params <- grid_regular(trees())

# ## Split data for CV
folds <- vfold_cv(training_data, v = K, repeats = 1)

# ## Run the CV
CV_results <- bart_workflow %>%
  tune_grid(resamples = folds,
            grid = grid_of_tuning_params,
            metrics = metric_set(rmse, mae))

# ## Plot results
# collect_metrics(CV_results) %>%
#   filter(.metric == "rmse") %>%
#   ggplot(data = ., aes(x = penalty, y = mean, color = factor(mixture))) +
#   geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best(metric="rmse")

# ## Finalize the workflow and fit it
final_wf <- bart_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data = training_data)

#####
## Create predictions
#####
## Generate predictions using penalized linear model
bike_predictions <- predict(final_wf, new_data = test_data)

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
vroom_write(x = kaggle_submission, file = "./autoPredictions.csv", delim = ",")
