natural_disasters <- read.csv("us_disaster_declarations.csv")

summary_info <- list()

#Find percent change for Washington State
summary_info$pct_change <- max(disasters_each_year$number_disasters) / min(disasters_each_year$number_disasters)

# Find year during which most disasters occurred 
summary_info$year_max_disasters <- natural_disasters %>% group_by(fy_declared) %>% summarise(number_disasters = n()) %>% filter(number_disasters == max(number_disasters)) %>% pull(fy_declared)

#Find the state that had the greatest total natural disasters in the most recent year.
summary_info$greatest_increase <- disaster_data %>% 
  group_by(state, disaster_number) %>% 
  filter(fy_declared == 2020) %>% 
  mutate(freq_2020 = n()) %>% 
  pull(max(freq_2020))
  
#Find 5 year range that had the most disaster declarations
summary_info <- append(summary_info, most_disasters)

#Find state that most commonly had the most disasters for every 5 year period 
summary_info <- append(summary_info, state_most_common)
