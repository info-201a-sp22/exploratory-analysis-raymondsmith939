library("readr")
library("dplyr")
library("ggplot2")
library("maps")
library("mapdata")
library("scales")
library("plotly")

States <- map_data('state')
disaster_data <- read.csv("us_disaster_declarations.csv")
state_abbreviations <- read.csv("abbreviations.csv")

# Frequency of Disasters in 2000

freq_2000_disasters <- disaster_data %>% 
  group_by(state, disaster_number) %>% 
  filter(fy_declared == 2000) %>% 
  mutate(freq_2000 = n())

# Frequency of Disasters in 2020

freq_2020_disasters <- disaster_data %>% 
  group_by(state, disaster_number) %>% 
  filter(fy_declared == 2020) %>% 
  mutate(freq_2020 = n())


# Combine the 2000 and 2020 data into a single dataframe
freq_2000_vs_2020 <- left_join(freq_2020_disasters, freq_2000_disasters, by = 'state')

# Create new variables: Change in Disasters, Change in Disasters
freq_2000_vs_2020 <- freq_2000_vs_2020 %>% 
  mutate(net_change = (freq_2020 - freq_2000)) # %>% 


# Unique States
test_freq_2000_vs_2020 <- freq_2000_vs_2020 %>% 
  distinct(state, net_change) %>% 
  select(state, net_change) 

# Renaming columns to left_join later
colnames(state_abbreviations) <- c('region', 'abbrev', 'state')
state_abbreviations$region = tolower(state_abbreviations$region)

# Combining the state names with their 2-digit code
state_code <- left_join(States, state_abbreviations, by = 'region')

# The data for the graph! States template + frequency data
graph_df3 <- left_join(state_code, test_freq_2000_vs_2020, by = 'state')

# The Map!

percent_change_map <- ggplot(States) + 
  geom_polygon(data = graph_df3,
               aes(x = long, y = lat, group=group, fill = net_change),
               color = "white") + 
  labs(title = "Increase in Natural Disasters by Frequency",
       x = " ",
       y = " ",
       fill = "Increase in # of Disasters") +
  scale_fill_gradientn(colors = c("white", "goldenrod2", "darkred"))
                       

# Interactive Map
ggplotly(percent_change_map)

# Which U.S. state experienced the greatest increase in natural disasters since 2000?
# Maine increased by 438 occurrences.
