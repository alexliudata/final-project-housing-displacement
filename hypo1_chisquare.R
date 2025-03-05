rm(list = ls())
getwd()

setwd("C:/Users/sangm/OneDrive/문서/spring_stat")
options(device = "RStudioGD")

install.packages("tidyverse")  # Install the tidyverse package
install.packages("tidycensus")  # Install the tidycensus package

library(tidyverse)  # Load the tidyverse package
library(tidycensus)  # Load the tidycensus package
census_api_key("949e0a4d00d49a4b45874d986730a80c80652f80", install = TRUE, overwrite= TRUE)

# Santa Clara County Settings (FIPS Code)
state_fips <- "06"  # California
county_fips <- "085"  # Santa Clara County

# Commute-related Variables (Tract Level)
acs_vars_commute <- c(
  "B08012_001",  # Total workers commuting
  "B08012_002", "B08012_003", "B08012_004", "B08012_005",  # Commute <40 min
  "B08012_006", "B08012_007", "B08012_008", "B08012_009",  # Commute <40 min (additional)
  "B08012_010", "B08012_011",  # Commute 40~60 min
  "B08012_012", "B08012_013"   # Commute 60+ min
)

# Rent Burden-related Variables (Tract Level)
acs_vars_rent <- c(
  "B25070_001",  # Total households paying rent (Base)
  "B25070_002", "B25070_003", "B25070_004", "B25070_005", # Rent Burden <25%
  "B25070_006", "B25070_007", "B25070_008", "B25070_009", # Rent Burden 25~49%
  "B25070_010"  # Rent Burden 50%+
)   

# Retrieve Commute Data (Tract Level)
santa_clara_commute <- get_acs(
  geography = "tract",  
  variables = acs_vars_commute,
  state = state_fips,
  county = county_fips,
  year = 2023
)

# Retrieve Rent Burden Data (Tract Level)
santa_clara_rent <- get_acs(
  geography = "tract",  
  variables = acs_vars_rent,
  state = state_fips,
  county = county_fips,
  year = 2023
)

# Convert Commute Data to Wide Format (Tract Level)
santa_clara_commute <- santa_clara_commute %>%
  pivot_wider(names_from = variable, values_from = estimate, values_fill = list(estimate = 0)) %>%
  mutate(
    total_commuters = B08012_001,
    commute_40_less = B08012_002 + B08012_003 + B08012_004 + B08012_005 +
      B08012_006 + B08012_007 + B08012_008 + B08012_009,  # Less than 40 minutes
    commute_40_60 = B08012_010 + B08012_011,  # 40~60 minutes
    commute_60_plus = B08012_012 + B08012_013   # More than 60 minutes
  ) %>%
  mutate(GEOID = as.character(GEOID))  # Ensure character type

# Convert Rent Burden Data to Wide Format
santa_clara_rent <- santa_clara_rent %>%
  pivot_wider(names_from = variable, values_from = estimate, values_fill = list(estimate = 0)) %>%
  mutate(
    total_renters = B25070_001,
    rent_burden_25_less = B25070_002 + B25070_003 + B25070_004 + B25070_005,
    rent_burden_25_50 = B25070_006 + B25070_007 + B25070_008 + B25070_009,
    rent_burden_50_plus = B25070_010
  ) %>%
  mutate(GEOID = as.character(GEOID))  # Ensure character type

santa_clara_rent <- santa_clara_rent %>%
  group_by(GEOID) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup()

santa_clara_commute <- santa_clara_commute %>%
  group_by(GEOID) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  ungroup()

# Merge Rent Burden and Commute Data at Tract Level
santa_clara_final <- left_join(santa_clara_rent, santa_clara_commute, by = "GEOID")

# Reshape Data for Rent Burden Visualization
rent_burden_long <- santa_clara_final %>%
  select(GEOID, total_renters, rent_burden_25_less, rent_burden_25_50, rent_burden_50_plus) %>%
  pivot_longer(cols = starts_with("rent_burden_"), names_to = "Rent Burden Category", values_to = "Households") %>%
  mutate(`Rent Burden Category` = factor(case_when(
    `Rent Burden Category` == "rent_burden_25_less" ~ "<25%",
    `Rent Burden Category` == "rent_burden_25_50" ~ "25~50%",
    `Rent Burden Category` == "rent_burden_50_plus" ~ "50%+"
  ), levels = c("<25%", "25~50%", "50%+"))) %>%
  group_by(`Rent Burden Category`) %>%
  summarize(Households = sum(Households), Total_Renters = sum(total_renters), .groups = "drop") %>%
  mutate(Percentage = (Households / Total_Renters) * 100)  # Calculate percentage

# Rent Burden Bar Chart
ggplot(rent_burden_long, aes(x = `Rent Burden Category`, y = Households, fill = `Rent Burden Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 5) +
  labs(title = "Santa Clara County Rent Burden Distribution", x = "Rent Burden Category", y = "Number of Households") +
  theme_minimal() +
  theme(legend.position = "none")

# Reshape Data for Commute Time Group Visualization
commute_long <- santa_clara_final %>%
  select(GEOID, total_commuters, commute_40_less, commute_40_60, commute_60_plus) %>%
  pivot_longer(cols = starts_with("commute_"), names_to = "Commute Time Category", values_to = "Workers") %>%
  mutate(`Commute Time Category` = factor(case_when(
    `Commute Time Category` == "commute_40_less" ~ "<40 min",
    `Commute Time Category` == "commute_40_60" ~ "40-60 min",
    `Commute Time Category` == "commute_60_plus" ~ "60+ min"
  ), levels = c("<40 min", "40-60 min", "60+ min"))) %>%
  group_by(`Commute Time Category`) %>%
  summarize(Workers = sum(Workers), Total_Commuters = sum(total_commuters), .groups = "drop") %>%
  mutate(Percentage = (Workers / Total_Commuters) * 100)

#  **Commute Time Histogram**
ggplot(commute_long, aes(x = `Commute Time Category`, y = Workers, fill = `Commute Time Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.3, size = 8) +
  labs(title = "Santa Clara County Commute Time Distribution", x = "Commute Time Category", y = "Number of Workers") +
  theme_minimal() +
  theme(legend.position = "none")


# Chi-Square Test on Rent Burden vs. Commute Time
contingency_matrix <- matrix(
  c(
    sum(santa_clara_final$rent_burden_25_less * santa_clara_final$commute_40_less, na.rm = TRUE),
    sum(santa_clara_final$rent_burden_25_less * santa_clara_final$commute_40_60, na.rm = TRUE),
    sum(santa_clara_final$rent_burden_50_plus * santa_clara_final$commute_40_less, na.rm = TRUE),
    sum(santa_clara_final$rent_burden_50_plus * santa_clara_final$commute_40_60, na.rm = TRUE)
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    "Rent Burden" = c("Less than 50%", "50% or More"),
    "Commute Time" = c("Less than 40 min", "40+ min")
  )
)

# Perform Chi-Square Test
chi_sq_test <- chisq.test(contingency_matrix, correct = FALSE)

# Print Results
print(chi_sq_test)
