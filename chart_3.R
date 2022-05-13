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
  count('state')

# Renaming to Left_Join Later
colnames(frequency_2000) <- c('code', 'class', 'freq')
colnames(state_abbreviations) <- c('state', 'abbr.', 'code')

# Combining Frequency with Abbreviations 
frequency_code_2000 <- left_join(frequency_2000, state_abbreviations, by = 'code')

# Renaming for future Left_Join
colnames(frequency_code_2000) <- c('code', 'class', 'freq_2000', 'region', 'abbrev')

## 2020 DATA
  
# Filter for 2020 Disasters
dis_by_st_2020 <- disaster_data %>% 
  group_by(state) %>% 
  filter(fy_declared == 2020)

# Frequency of Disasters in 2020
frequency_2020 <- dis_by_st_2020 %>% 
  count('state')

# Renaming Columns for Join
colnames(frequency_2020) <- c('code', 'class', 'freq')

# Combining Frequency and State Names for future join
frequency_code_2020 <- left_join(frequency_2020, state_abbreviations, by = 'code')

# Renaming columns for future join
colnames(frequency_code_2020) <- c('code', 'class', 'freq_2020', 'region', 'abbrev')

## Combining and Comparing

# Combine the 2000 and 2020 data into a single dataframe
freq_2000_vs_2020 <- left_join(frequency_2020, frequency_2000, by = 'code')

# Rename columns to join into graph dataset
colnames(freq_2000_vs_2020) <- c('code', 'class_2020', 'freq_2020', 'class_2000', 'freq_2000')

# Create new variables: Change in Disasters, % Change in Disasters
freq_2000_vs_2020 <- freq_2000_vs_2020 %>% 
  mutate(net_change = (freq_2020 - freq_2000)) %>% 
  mutate(pct_change = (net_change / freq_2020)*100)

# Renaming columns to left_join later
colnames(state_abbreviations) <- c('region', 'abbrev', 'code')
state_abbreviations$region = tolower(state_abbreviations$region)

# Combining the state names with their 2-digit code
state_code <- left_join(States, state_abbreviations, by = 'region')

# The data for the graph! States template + frequency data
graph_df3 <- left_join(state_code, freq_2000_vs_2020, by = 'code')

# Filtering out NA, negative values to focus on INCREASE
graph_df3[is.na(graph_df3)] = 0

graph_df3_increase <- graph_df3 %>% 
  filter(pct_change >= 0) 

# The Map!
ggplot(States) + 
  geom_polygon(data = graph_df3_increase,
               aes(x = long, y = lat, group=group, fill = pct_change),
               color = "white") + 
  labs(title = "Increase in Natural Disasters",
       subtitle = 'by %, 2000-2020',
       x = "Longitude",
       y = "Latitude") +
  scale_fill_fermenter(
    type = "seq",
    palette = "Oranges",
    direction = 1,
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill"
  )
