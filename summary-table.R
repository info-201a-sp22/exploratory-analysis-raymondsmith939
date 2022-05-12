us_disaster_declarations <- read.csv("https://raw.githubusercontent.com/info-201a-sp22/exploratory-analysis-raymondsmith939/main/us_disaster_declarations.csv?token=GHSAT0AAAAAABTAT5A5EWE53KLHZKVTX2EGYT42V5A")

library(DescTools)
library(tidyverse)

year_interval <- 5
start_year <-RoundTo(min(us_disaster_declarations$fy_declared), multiple = year_interval, FUN = floor)
end_year <- RoundTo(max(us_disaster_declarations$fy_declared), multiple = year_interval, FUN = ceiling)

types_of_disasters =  unique(us_disaster_declarations$incident_type)

summary_table <- us_disaster_declarations %>% group_by(ranges = cut(fy_declared, breaks = seq(start_year, end_year, year_interval))) %>% summarize(disasters_per_range= length(fy_declared))



