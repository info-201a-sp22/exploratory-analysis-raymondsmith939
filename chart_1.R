# This chart plots the number of disasters that occurred each year and finds the year during which maximum number of disasters occurred

# Load packages
library ("dplyr")
library("ggplot2")

# Load datasets
natural_disasters <- read.csv("us_disaster_declarations.csv")
disasters_each_year <- natural_disasters %>% group_by(fy_declared) %>% summarise(number_disasters = n())

# Find year during which most disasters occurred 
year_max_disasters <- disasters_each_year %>% filter(number_disasters == max(number_disasters)) %>% pull(fy_declared)

# Plot values from all years
ggplot(disasters_each_year) + 
  geom_line(mapping = aes(x = fy_declared, y = number_disasters), color = "blue") +
  geom_point(mapping = aes(x = fy_declared, y = number_disasters), color  = "red") + 
  labs(title = "Number of disasters by year", x = "Year", y = "Number of diasters") +
  scale_x_continuous(breaks = round(seq(1953, 2022, by = 1))) +
  scale_y_continuous(breaks = round(seq(0, 9500, by = 1000))) +
  theme(axis.text.x = element_text(angle = 90))
  