us_disaster_declarations <- read.csv("us_disaster_declarations.csv")

summary_info <- list()

#Find percent change for Washington State
summary_info <- append(summary_info, pct_change)

# Find year during which most disasters occurred 
summary_info <- append(summary_info, year_max_disasters)

#Find State that that had the largest percent increase 
summary_info <- append(summary_info, largest_pct_state_increase)

#Find 5 year range that had the most disaster declarations
summary_info <- append(summary_info, most_disasters)

#Find state that most commonly had the most disasters for every 5 year period 
summary_info <- append(summary_info, state_most_common)
