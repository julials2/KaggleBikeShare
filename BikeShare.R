library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)

bike_share <- vroom("train.csv") %>% 
  mutate(weather = as.factor(weather), 
         holiday = as.factor(holiday), 
         season = as.factor(season), 
         workingday = as.factor(workingday)) %>% 
  select(-casual, -registered)

test_data <- vroom("test.csv") %>% 
  mutate(weather = as.factor(weather), 
         holiday = as.factor(holiday), 
         season = as.factor(season), 
         workingday = as.factor(workingday)) 

GGally::ggpairs(bike_share)

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

## Set up and fit the Linear Regression Model
bike_share_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression") %>% 
  fit(formula = count ~ ., data = bike_share)

## Generate predictions using linear model
bike_predictions <- predict(bike_share_model,
                            new_data = test_data)
## Look at the predictions
bike_predictions

## Format the predictions for submission to kaggle
kaggle_submission <- bike_predictions %>% 
  bind_cols(., test_data) %>% 
  select(datetime, .pred) %>% 
  rename(count = .pred) %>% 
  mutate(count = pmax(0, count)) %>% 
  mutate(datetime = as.character(format(datetime)))

## Write out the file
vroom_write(x = kaggle_submission, file = "./LinearPreds.csv", delim = ",")
