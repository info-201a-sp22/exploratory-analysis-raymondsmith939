natural_disasters <- read.csv("us_disaster_declarations.csv")

library(DescTools)
library(tidyverse)
library(knitr)

summary_info <- list()

#Find percent change for Washington State
summary_info$pct_change <- max(natural_disasters %>% filter(state == 'WA') %>% group_by(fy_declared) %>% summarise(number_disasters = n()) %>% pull(number_disasters)) / min(natural_disasters %>% filter(state == 'WA') %>% group_by(fy_declared) %>% summarise(number_disasters = n())%>% pull(number_disasters))

# Find year during which most disasters occurred 
summary_info$year_max_disasters <- natural_disasters %>% group_by(fy_declared) %>% summarise(number_disasters = n()) %>% filter(number_disasters == max(number_disasters)) %>% pull(fy_declared)

#Find the state that had the greatest increase natural disasters in the most recent year.

States <- map_data('state')
disaster_data <- read.csv("us_disaster_declarations.csv")
state_abbreviations <- read.csv("abbreviations.csv")

freq_2000_disasters <- disaster_data %>% 
  group_by(state, disaster_number) %>% 
  filter(fy_declared == 2000) %>% 
  mutate(freq_2000 = n())

freq_2020_disasters <- disaster_data %>% 
  group_by(state, disaster_number) %>% 
  filter(fy_declared == 2020) %>% 
  mutate(freq_2020 = n())

freq_2000_vs_2020 <- left_join(freq_2020_disasters, freq_2000_disasters, by = 'state')

freq_2000_vs_2020 <- freq_2000_vs_2020 %>% 
  mutate(net_change = (freq_2020 - freq_2000))

test_freq_2000_vs_2020 <- freq_2000_vs_2020 %>% 
  distinct(state, net_change) %>% 
  select(state, net_change) 

colnames(state_abbreviations) <- c('region', 'abbrev', 'state')
state_abbreviations$region = tolower(state_abbreviations$region)

state_code <- left_join(States, state_abbreviations, by = 'region')

graph_df3 <- left_join(state_code, test_freq_2000_vs_2020, by = 'state')

summary_info$greatest_increase <- graph_df3 %>% 
  summarize(grestest_increase = max(net_change, na.rm = TRUE)) %>% 
  pull()


#set range parameters
year_interval <- 5
start_year <-RoundTo(min(natural_disasters$fy_declared), multiple = year_interval, FUN = floor)
end_year <- RoundTo(max(natural_disasters$fy_declared), multiple = year_interval, FUN = ceiling)

#find number of disasters per range of years
summary_table <- natural_disasters %>% 
  group_by(Years = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>% 
  summarize("Number of Disasters" = length(fy_declared)) 

#find overall number of each unique disaster by range of years
number_of_disasters_by_type <- natural_disasters %>% 
  group_by(incident_type,Years = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>% 
  summarize(number_of_incidents = length(incident_type)) %>%
  pivot_wider(names_from = incident_type, values_from = number_of_incidents) %>% 
  select(Years, Flood, Hurricane, Tornado, Earthquake, Fire, 'Severe Storm(s)')

#change NA values to 0
number_of_disasters_by_type[is.na(number_of_disasters_by_type)] = 0

#find state with most natural disasters per range of years
most_common_state <- natural_disasters %>% 
  group_by(state, Years = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>%
  summarize(number_of_disasters = length(incident_type)) %>% 
  group_by(Years) %>% 
  filter(number_of_disasters == max(number_of_disasters)) %>% 
  summarize('State With Most Disasters' = state)

#join number of unique disaster types per range to overall disasters per range
summary_table <- left_join(summary_table, most_common_state)
summary_table <- left_join(summary_table, number_of_disasters_by_type)
summary_table <- summary_table[-1,]

#Find Year range with most disasters
summary_info$most_disasters_range <- paste0(summary_table %>% filter(`Number of Disasters` == max(`Number of Disasters`)) %>% pull(Years))

#Fix most_disasters formatting
summary_info$most_disasters_range <- gsub("\\(|\\]|","", summary_info$most_disasters_range)
summary_info$most_disasters_range <- gsub(",", "-", summary_info$most_disasters_range)

#Find State that most commonly has the most disasters
summary_info$state_most_common <- summary_table %>% count(`State With Most Disasters`) %>% filter(n == max(n)) %>% pull(`State With Most Disasters`)



