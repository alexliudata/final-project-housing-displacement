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
      B08012_006 + B08012_007 + B08012_008 + B08012_009,  # Commute less than 40 min
    commute_40_60 = B08012_010 + B08012_011,  # Commute 40~60 min
    commute_60_plus = B08012_012 + B08012_013  # Commute 60+ min
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

# Merge Tract-Level Rent Burden Data and Commute Data
santa_clara_final <- left_join(santa_clara_rent, santa_clara_commute, by = "GEOID")

# Load Required Packages
library(tidyverse)
library(tidycensus)
library(broom)

# Create Dependent Variable (Y): Percentage of Workers Commuting More Than 40 Minutes
santa_clara_final <- santa_clara_final %>%
  mutate(
    commute_40plus_pct = ((B08012_010 + B08012_011 + B08012_012 + B08012_013) / B08012_001) * 100
  )

# Create Independent Variable (X): Rent Burden 50% or More vs. Below 50%
santa_clara_final <- santa_clara_final %>%
  mutate(
    rent_burden_50_plus_pct = (B25070_010 / B25070_001) * 100,  # Percentage of Households with Rent Burden >50%
    rent_burden_below_50_pct = ((B25070_002 + B25070_003 + B25070_004 + B25070_005 + 
                                   B25070_006 + B25070_007 + B25070_008 + B25070_009) / B25070_001) * 100  # Percentage of Households with Rent Burden <50%
  )

# Run OLS Regression Analysis
ols_model <- lm(
  commute_40plus_pct ~ rent_burden_50_plus_pct + rent_burden_below_50_pct,
  data = santa_clara_final
)

# Display Regression Results
summary(ols_model)

# Visualization: Rent Burden vs. Commute Time (40+ min)
ggplot(santa_clara_final, aes(x = rent_burden_50_plus_pct, y = commute_40plus_pct)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    title = "Effect of Rent Burden on Long Commute (40+ min)",
    x = "Percentage of Households with Rent Burden >50%",
    y = "Percentage of Workers Commuting >40 min"
  ) +
  theme_minimal()
