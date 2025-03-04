# Install the 'librarian' package if not already installed
install.packages(c("librarian"))

# Load the 'librarian' package, which helps manage and load other packages
library(librarian)

# Use the 'shelf' function from librarian to load multiple packages at once
shelf(tidyverse, tidycensus)

# Load the ACS variables for 2018
vars_acs5_2022 <- load_variables(2022, 'acs5', cache = TRUE)
View(vars_acs5_2022)

variables <- c(
  "B25064_001", # Median rent
  "B08303_001", # Travel time to work
  "B08134_001", # Commute mode
  "B19013_001", # Median Income
  "B02001_001"  # Race/ethnicity count
)

df <- get_acs(
  geography = "tract",
  variables = variables,
  year = 2022,
  state = "CA",
  county = "Santa Clara",
  geometry = TRUE,
  survey = "acs5"
)

# Rename variables and perform initial processing
df <- df %>%
  mutate(
    variable = case_when(
      variable == "B25064_001" ~ "median_rent",
      variable == "B08303_001" ~ "travel_time",
      variable == "B08134_001" ~ "commute_mode",
      variable == "B19013_001" ~ "median_income",
      variable == "B02001_001" ~ "race_ethnicity_count",
      TRUE ~ variable  # Keep original name if no match
    )
  )