library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)

bike_share <- vroom("train.csv") %>% 
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
