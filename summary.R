us_disaster_declarations <- read.csv("https://raw.githubusercontent.com/info-201a-sp22/exploratory-analysis-raymondsmith939/main/us_disaster_declarations.csv?token=GHSAT0AAAAAABTAT5A5JXG5ZL2R432RMLO2YT62FMA")

summary_info <- list()
summary_info$num_obversations <- nrow(summary_table)
summary_info$disasters_per_every_5_years_from_1950 <- summary_table %>% filter(`Number of Disasters`)
select(`Number of Disasters`)

# A function that takes in a dataset and returns a list of info about it:
summary_info <- list()
summary_info$num_observations <- nrow(my_dataframe)
summary_info$some_max_value <- my_dataframe %>%
  filter(some_var == max(some_var, na.rm = T)) %>%
  select(some_label)
