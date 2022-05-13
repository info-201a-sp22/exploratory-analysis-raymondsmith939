us_disaster_declarations <- read.csv("https://raw.githubusercontent.com/info-201a-sp22/exploratory-analysis-raymondsmith939/main/us_disaster_declarations.csv?token=GHSAT0AAAAAABTAT5A56U6NBZVOLSS2RNOCYT6DGHQ")

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
  pivot_wider(names_from = incident_type, values_from = number_of_incidents) %>% select(Years, Flood, Hurricane, Tornado, Earthquake, Fire, 'Severe Storm(s)')

#join number of unique disaster types per range to overall disasters per range
summary_table <- left_join(summary_table, number_of_disasters_by_type)
