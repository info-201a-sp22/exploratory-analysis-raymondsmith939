us_disaster_declarations <- read.csv("/Users/raymondsmith/Info201code/exploratory-analysis-raymondsmith939/us_disaster_declarations.csv")

library(DescTools)
library(tidyverse)

#set range parameters
year_interval <- 5
start_year <-RoundTo(min(us_disaster_declarations$fy_declared), multiple = year_interval, FUN = floor)
end_year <- RoundTo(max(us_disaster_declarations$fy_declared), multiple = year_interval, FUN = ceiling)

#find number of disasters per range of years
summary_table <- us_disaster_declarations %>% 
  group_by(Years = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>% 
  summarize("Number of Disasters" = length(fy_declared)) 

#find overall number of each unique disaster by range of years
number_of_disasters_by_type <- us_disaster_declarations %>% 
  group_by(incident_type,Years = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>% 
  summarize(number_of_incidents = length(incident_type)) %>%
  pivot_wider(names_from = incident_type, values_from = number_of_incidents) %>% 
  select(Years, Flood, Hurricane, Tornado, Earthquake, Fire, 'Severe Storm(s)')

#change NA values to 0
number_of_disasters_by_type[is.na(number_of_disasters_by_type)] = 0

#find state with most natural disasters per range of years
most_common_state <- us_disaster_declarations %>% 
  group_by(state, Years = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>%
  summarize(number_of_disasters = length(incident_type)) %>% 
  group_by(Years) %>% 
  filter(number_of_disasters == max(number_of_disasters)) %>% 
  summarize('State With Most Disasters' = state)

#join number of unique disaster types per range to overall disasters per range
summary_table <- left_join(summary_table, most_common_state)
summary_table <- left_join(summary_table, number_of_disasters_by_type)

#reorder columns
summary_table <- summary_table %>% relocate('State With Most Disasters', .after = 'Severe Storm(s)')
