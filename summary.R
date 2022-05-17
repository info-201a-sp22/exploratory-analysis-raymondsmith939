natural_disasters <- read.csv("us_disaster_declarations.csv")

summary_info <- list()

#Find percent change for Washington State
summary_info$pct_change <- max(disasters_each_year$number_disasters) / min(disasters_each_year$number_disasters)

# Find year during which most disasters occurred 
summary_info$year_max_disasters <- natural_disasters %>% group_by(fy_declared) %>% summarise(number_disasters = n()) %>% filter(number_disasters == max(number_disasters)) %>% pull(fy_declared)

#Find the state that had the great total increase in natural disasters
summary_info <- append(summary_info, greatest_increase)

#Find 5 year range that had the most disaster declarations
summary_info <- append(summary_info, most_disasters)

#Find state that most commonly had the most disasters for every 5 year period 
summary_info <- append(summary_info, state_most_common)
