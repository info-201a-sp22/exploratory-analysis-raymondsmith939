natural_disasters <- read.csv("us_disaster_declarations.csv")

library(DescTools)
library(tidyverse)
library(knitr)

summary_info <- list()

#Find percent change for Washington State
summary_info$pct_change <- max(natural_disasters %>% filter(state == 'WA') %>% group_by(fy_declared) %>% summarise(number_disasters = n()) %>% pull(number_disasters)) / min(natural_disasters %>% filter(state == 'WA') %>% group_by(fy_declared) %>% summarise(number_disasters = n())%>% pull(number_disasters))

# Find year during which most disasters occurred 
summary_info$year_max_disasters <- natural_disasters %>% group_by(fy_declared) %>% summarise(number_disasters = n()) %>% filter(number_disasters == max(number_disasters)) %>% pull(fy_declared)

#Find the state that had the greatest total natural disasters in the most recent year.
summary_info$greatest_increase <- natural_disasters %>% 
  group_by(state, disaster_number) %>% 
  filter(fy_declared == 2020) %>% 
  mutate(freq_2020 = n()) %>% 
  summarize(max_in_state = max(freq_2020))
  pull(max_in_state)

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
summary_info$most_disasters_range <- gsub("\\(|\\]|","", most_disasters)
summary_info$most_disasters_range <- gsub(",", "-", most_disasters)

#Find State that most commonly has the most disasters
summary_info$state_most_common <- summary_table %>% count(`State With Most Disasters`) %>% filter(n == max(n)) %>% pull(`State With Most Disasters`)



