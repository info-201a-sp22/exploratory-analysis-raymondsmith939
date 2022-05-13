library("readr")
library("dplyr")
library("ggplot2")
us_disaster_declarations <- read_csv("Desktop/info201/us_disaster_declarations.csv")

WA_disaster <- us_disaster_declarations %>% 
  filter(state == 'WA') 


ggplot (WA_disaster, aes(incident_begin_date)) +
  geom_histogram() +  
  labs(title = "Number of Disasters in Washington by year", x = "Year", y = "Number of diasters") 
 
disasters_each_year <- WA_disaster %>% group_by(fy_declared) %>% summarise(number_disasters = n())

pct_change <- max(disasters_each_year$number_disasters) / min(disasters_each_year$number_disasters)
pct_change 
