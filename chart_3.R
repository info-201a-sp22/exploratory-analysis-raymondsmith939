library("readr")
library("dplyr")
library("ggplot2")
library("maps")
library("mapdata")

States <- map_data('state')
disaster_data <- read.csv("us_disaster_declarations.csv")
state_abbreviations <- read.csv("abbreviations.csv")

## 2000 DATA  

# Filter Disasters in 2000
dis_by_st_2000 <- disaster_data %>% 
  group_by(state) %>% 
  filter(fy_declared == 2000)

# Frequency of Disasters in 2000
frequency_2000 <- dis_by_st_2000 %>% 
  group_by(disaster_number) %>% 
  mutate(frequency_2000 = n())


## 2020 DATA
  
# Filter for 2020 Disasters
dis_by_st_2020 <- disaster_data %>% 
  group_by(state) %>% 
  filter(fy_declared == 2020)

# Frequency of Disasters in 2020
frequency_2020 <- dis_by_st_2020 %>% 
  group_by(disaster_number) %>% 
  mutate(frequency_2020 = n())

## Combining and Comparing


# Combine the 2000 and 2020 data into a single dataframe
freq_2000_vs_2020 <- left_join(frequency_2020, frequency_2000, by = 'state')

# Create new variables: Change in Disasters, % Change in Disasters
freq_2000_vs_2020 <- freq_2000_vs_2020 %>% 
  mutate(net_change = (frequency_2020 - frequency_2000)) %>% 
  mutate(pct_change = (net_change / frequency_2020)*100)

# Renaming columns to left_join later
colnames(state_abbreviations) <- c('region', 'abbrev', 'state')
state_abbreviations$region = tolower(state_abbreviations$region)

# Combining the state names with their 2-digit code
state_code <- left_join(States, state_abbreviations, by = 'region')

# The data for the graph! States template + frequency data
graph_df3 <- left_join(state_code, freq_2000_vs_2020, by = 'state')

# The Map!
library(scales)

percent_change_map <- ggplot(States) + 
  geom_polygon(data = graph_df3,
               aes(x = long, y = lat, group=group, fill = pct_change),
               color = "white", size = 0) + 
  labs(title = "Increase in Natural Disasters",
       subtitle = 'by %, 2000-2020',
       x = "Longitude",
       y = "Latitude") +
  scale_fill_gradientn(colors = c("white", "goldenrod2", "darkred"), values = rescale(c(0, 50, 100))
                       )

# Interactive Map
library(plotly)
ggplotly(percent_change_map)

# Which U.S. state experienced the greatest increase in natural disasters since 2000?
largest_pct_increase <- graph_df3 %>% 
  filter(pct_change == max(pct_change)) %>% 
  pull(region)
