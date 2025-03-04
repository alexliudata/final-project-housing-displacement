# Install the 'librarian' package if not already installed
install.packages(c("librarian"))

# Load the 'librarian' package, which helps manage and load other packages
library(librarian)

# Use the 'shelf' function from librarian to load multiple packages at once
shelf(tidyverse, tidycensus)

# Load the ACS variables for 2022
vars_acs5_2022 <- load_variables(2023, 'acs5', cache = TRUE)
View(vars_acs5_2022)


variables <- c(
  "B25064_001", # Median Gross rent
  "B08303_001", # total workers (overall)
  "B08012_002", # Travel time less than 5 minutes
  "B08012_003", # Travel time 5 to 9 minutes
  "B08012_004", # Travel time 10 to 14 minutes
  "B08012_005", # Travel time 15 to 19 minutes
  "B08012_006", # Travel time 20 to 24 minutes
  "B08012_007", # Travel time 25 to 29 minutes
  "B08012_008", # Travel time 30 to 34 minutes
  "B08012_009", # Travel time 35 to 39 minutes
  "B08012_010", # Travel time 40 to 44 minutes
  "B08012_011", # Travel time 45 to 59 minutes
  "B08012_012", # Travel time 60 to 89 minutes
  "B08012_013", # Travel time 90 or more minutes
  "B08105B_001", # Commute Black (total workers)
  "B08105I_001", # Commute Hispanic (total workers)
  "B08105A_001", # Commute White (total workers)
  "B08105D_001", # Commute Asian (total workers)
  "B19013_001",  # Median Household Income
  "B19013B_001", # Median Household Income Black
  "B19013I_001", # Median Household Income Hispanic
  "B19013A_001", # Median Household Income White
  "B19013D_001"  # Median Household Income Asian
)

# Get county-level data
df_county <- get_acs(
  geography = "county",
  variables = variables,
  year = 2022,  # Changed from 2023 to 2022 to match the vars_acs5_2022
  state = "CA",
  geometry = FALSE,
  county = "Santa Clara",
  survey = "acs5"
)

df_county <- df_county %>%
  mutate(
    variable = case_when(
      variable == "B25064_001" ~ "Median_Gross_Rent",
      variable == "B08303_001" ~ "Total_Workers",
      variable == "B08012_002" ~ "Travel_Time_Less_Than_5_Minutes",
      variable == "B08012_003" ~ "Travel_Time_5_to_9_Minutes",
      variable == "B08012_004" ~ "Travel_Time_10_to_14_Minutes",
      variable == "B08012_005" ~ "Travel_Time_15_to_19_Minutes",
      variable == "B08012_006" ~ "Travel_Time_20_to_24_Minutes",
      variable == "B08012_007" ~ "Travel_Time_25_to_29_Minutes",
      variable == "B08012_008" ~ "Travel_Time_30_to_34_Minutes",
      variable == "B08012_009" ~ "Travel_Time_35_to_39_Minutes",
      variable == "B08012_010" ~ "Travel_Time_40_to_44_Minutes",
      variable == "B08012_011" ~ "Travel_Time_45_to_59_Minutes",
      variable == "B08012_012" ~ "Travel_Time_60_to_89_Minutes",
      variable == "B08012_013" ~ "Travel_Time_90_Plus_Minutes",
      variable == "B08105B_001" ~ "Black_Workers_Total",
      variable == "B08105I_001" ~ "Hispanic_Workers_Total",
      variable == "B08105A_001" ~ "White_Workers_Total",
      variable == "B08105D_001" ~ "Asian_Workers_Total",
      variable == "B19013_001" ~ "Median_Household_Income",
      variable == "B19013B_001" ~ "Median_Household_Income_Black",
      variable == "B19013I_001" ~ "Median_Household_Income_Hispanic",
      variable == "B19013A_001" ~ "Median_Household_Income_White",
      variable == "B19013D_001" ~ "Median_Household_Income_Asian"
    )
  )

df_county_wide <- df_county %>% 
  select(-moe) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

df_county_wide <- df_county_wide %>%
  mutate(
    pct_black = Black_Workers_Total / Total_Workers,
    pct_hispanic = Hispanic_Workers_Total / Total_Workers,
    pct_white = White_Workers_Total / Total_Workers,
    pct_asian = Asian_Workers_Total / Total_Workers
  )

df_county_wide <- df_county_wide %>%
  mutate(
    pct_Travel_Time_Less_Than_5_Minutes = Travel_Time_Less_Than_5_Minutes / Total_Workers,
    pct_Travel_Time_5_to_9_Minutes = Travel_Time_5_to_9_Minutes / Total_Workers,
    pct_Travel_Time_10_to_14_Minutes = Travel_Time_10_to_14_Minutes / Total_Workers,
    pct_Travel_Time_15_to_19_Minutes = Travel_Time_15_to_19_Minutes / Total_Workers,
    pct_Travel_Time_20_to_24_Minutes = Travel_Time_20_to_24_Minutes / Total_Workers,
    pct_Travel_Time_25_to_29_Minutes = Travel_Time_25_to_29_Minutes / Total_Workers,
    pct_Travel_Time_30_to_34_Minutes = Travel_Time_30_to_34_Minutes / Total_Workers,
    pct_Travel_Time_35_to_39_Minutes = Travel_Time_35_to_39_Minutes / Total_Workers,
    pct_Travel_Time_40_to_44_Minutes = Travel_Time_40_to_44_Minutes / Total_Workers,
    pct_Travel_Time_45_to_59_Minutes = Travel_Time_45_to_59_Minutes / Total_Workers,
    pct_Travel_Time_60_to_89_Minutes = Travel_Time_60_to_89_Minutes / Total_Workers,
    pct_Travel_Time_90_Plus_Minutes = Travel_Time_90_Plus_Minutes / Total_Workers
  )

# Create the simplified and polished table
df_county_wide <- df_county_wide %>%
  select(
    Median_Gross_Rent,
    Median_Household_Income,
    Median_Household_Income_Black,
    Median_Household_Income_Hispanic,
    Median_Household_Income_White,
    Median_Household_Income_Asian,
    pct_black, pct_hispanic, pct_white, pct_asian,
    pct_Travel_Time_Less_Than_5_Minutes,
    pct_Travel_Time_5_to_9_Minutes,
    pct_Travel_Time_10_to_14_Minutes,
    pct_Travel_Time_15_to_19_Minutes,
    pct_Travel_Time_20_to_24_Minutes,
    pct_Travel_Time_25_to_29_Minutes,
    pct_Travel_Time_30_to_34_Minutes,
    pct_Travel_Time_35_to_39_Minutes,
    pct_Travel_Time_40_to_44_Minutes,
    pct_Travel_Time_45_to_59_Minutes,
    pct_Travel_Time_60_to_89_Minutes,
    pct_Travel_Time_90_Plus_Minutes, 
    
  ) %>%
  mutate(
    pct_black = round(pct_black * 100, 2),
    pct_hispanic = round(pct_hispanic * 100, 2),
    pct_white = round(pct_white * 100, 2),
    pct_asian = round(pct_asian * 100, 2),
    across(starts_with("pct_Travel_Time"), ~ round(. * 100, 2))  # Round all travel time percentages
  )

# Clean column names for better readability
colnames(df_county_wide) <- c(
  "Median Gross Rent ($)", "Median Household Income ($)",
  "Median Household Income Black ($)", "Median Household Income Hispanic ($)",
  "Median Household Income White ($)", "Median Household Income Asian ($)",
  "% Black Workers", "% Hispanic Workers", "% White Workers", "% Asian Workers",
  "% Travel <5 Min", "% Travel 5–9 Min", "% Travel 10–14 Min", "% Travel 15–19 Min",
  "% Travel 20–24 Min", "% Travel 25–29 Min", "% Travel 30–34 Min", "% Travel 35–39 Min",
  "% Travel 40–44 Min", "% Travel 45–59 Min", "% Travel 60–89 Min", "% Travel 90+ Min"
)

# Categorize travel times - fixed the syntax error in the Short_Commute calculation
df_county_wide <- df_county_wide %>%
  mutate(
    Short_Commute = `% Travel <5 Min` + `% Travel 5–9 Min` + `% Travel 10–14 Min` + 
      `% Travel 15–19 Min` + `% Travel 20–24 Min` + `% Travel 25–29 Min` + 
      `% Travel 30–34 Min` + `% Travel 35–39 Min`, 
    Long_Commute = `% Travel 40–44 Min` + `% Travel 45–59 Min` + `% Travel 60–89 Min` + `% Travel 90+ Min`
  )

# Create a new dataframe with the categorized commute times
df_county_wide <- df_county_wide %>%
  select(
    "Median Gross Rent ($)", 
    "Median Household Income ($)",
    "Median Household Income Black ($)",
    "Median Household Income Hispanic ($)",
    "Median Household Income White ($)",
    "Median Household Income Asian ($)",
    "% Black Workers", 
    "% Hispanic Workers", 
    "% White Workers", 
    "% Asian Workers",
    Short_Commute,
    Long_Commute
  ) %>%
  mutate(
    Short_Commute = round(Short_Commute, 2),
    Long_Commute = round(Long_Commute, 2)
  )

# Rename columns for clarity
colnames(df_county_wide) <- c(
  "Median Gross Rent ($)", 
  "Median Household Income ($)",
  "Median Household Income Black ($)",
  "Median Household Income Hispanic ($)",
  "Median Household Income White ($)",
  "Median Household Income Asian ($)",
  "% Black Workers", 
  "% Hispanic Workers", 
  "% White Workers", 
  "% Asian Workers",
  "% Short Commute (0-40 min)",
  "% Long Commute (40+ min)"
)

glimpse(df_county_wide)

commute_times <-  df_county_wide %>% select(`% Short Commute (0-40 min)`, `% Long Commute (40+ min)`)



# Get tract-level data
df_tract <- get_acs(
  geography = "tract",
  variables = variables,
  year = 2022,
  state = "CA",
  geometry = TRUE,
  county = "Santa Clara",
  survey = "acs5"
)

df_tract <- df_tract %>%
  mutate(
    variable = case_when(
      variable == "B25064_001" ~ "Median_Gross_Rent",
      variable == "B08303_001" ~ "Total_Workers",
      variable == "B08012_002" ~ "Travel_Time_Less_Than_5_Minutes",
      variable == "B08012_003" ~ "Travel_Time_5_to_9_Minutes",
      variable == "B08012_004" ~ "Travel_Time_10_to_14_Minutes",
      variable == "B08012_005" ~ "Travel_Time_15_to_19_Minutes",
      variable == "B08012_006" ~ "Travel_Time_20_to_24_Minutes",
      variable == "B08012_007" ~ "Travel_Time_25_to_29_Minutes",
      variable == "B08012_008" ~ "Travel_Time_30_to_34_Minutes",
      variable == "B08012_009" ~ "Travel_Time_35_to_39_Minutes",
      variable == "B08012_010" ~ "Travel_Time_40_to_44_Minutes",
      variable == "B08012_011" ~ "Travel_Time_45_to_59_Minutes",
      variable == "B08012_012" ~ "Travel_Time_60_to_89_Minutes",
      variable == "B08012_013" ~ "Travel_Time_90_Plus_Minutes",
      variable == "B08105B_001" ~ "Black_Workers_Total",
      variable == "B08105I_001" ~ "Hispanic_Workers_Total",
      variable == "B08105A_001" ~ "White_Workers_Total",
      variable == "B08105D_001" ~ "Asian_Workers_Total",
      variable == "B19013_001" ~ "Median_Household_Income",
      variable == "B19013B_001" ~ "Median_Household_Income_Black",
      variable == "B19013I_001" ~ "Median_Household_Income_Hispanic",
      variable == "B19013A_001" ~ "Median_Household_Income_White",
      variable == "B19013D_001" ~ "Median_Household_Income_Asian"
    )
  )

df_tract_wide <- df_tract %>% 
  select(-moe) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

df_tract_wide <- df_tract_wide %>%
  mutate(across(c(Black_Workers_Total, Hispanic_Workers_Total, White_Workers_Total, Asian_Workers_Total), 
                ~ . / Total_Workers, 
                .names = "pct_{.col}")) %>%
  mutate(across(starts_with("Travel_Time"), 
                ~ . / Total_Workers, 
                .names = "pct_{.col}"))

# Handle potential NA or infinite values
df_tract_wide <- df_tract_wide %>%
  mutate(across(starts_with("pct_"), ~ifelse(is.infinite(.) | is.nan(.), NA, .)))

# Calculate average commute time
df_tract_wide <- df_tract_wide %>%
  mutate(
    avg_commute_time = (2.5 * Travel_Time_Less_Than_5_Minutes +
                          7 * Travel_Time_5_to_9_Minutes +
                          12 * Travel_Time_10_to_14_Minutes +
                          17 * Travel_Time_15_to_19_Minutes +
                          22 * Travel_Time_20_to_24_Minutes +
                          27 * Travel_Time_25_to_29_Minutes +
                          32 * Travel_Time_30_to_34_Minutes +
                          37 * Travel_Time_35_to_39_Minutes +
                          42 * Travel_Time_40_to_44_Minutes +
                          52 * Travel_Time_45_to_59_Minutes +
                          75 * Travel_Time_60_to_89_Minutes +
                          100 * Travel_Time_90_Plus_Minutes) / Total_Workers
  )

# Create the final dataframe
# Fixed the commute time category ranges to match the county level analysis
df_tract_wide <- df_tract_wide %>%
  transmute(
    `Median Gross Rent ($)` = Median_Gross_Rent,
    `Median Household Income ($)` = Median_Household_Income,
    `Median Household Income Black ($)` = Median_Household_Income_Black,
    `Median Household Income Hispanic ($)` = Median_Household_Income_Hispanic,
    `Median Household Income White ($)` = Median_Household_Income_White,
    `Median Household Income Asian ($)` = Median_Household_Income_Asian,
    `% Black Workers` = round(pct_Black_Workers_Total * 100, 2),
    `% Hispanic Workers` = round(pct_Hispanic_Workers_Total * 100, 2),
    `% White Workers` = round(pct_White_Workers_Total * 100, 2),
    `% Asian Workers` = round(pct_Asian_Workers_Total * 100, 2),
    `% Short Commute (0-40 min)` = round((pct_Travel_Time_Less_Than_5_Minutes + 
                                            pct_Travel_Time_5_to_9_Minutes + 
                                            pct_Travel_Time_10_to_14_Minutes + 
                                            pct_Travel_Time_15_to_19_Minutes + 
                                            pct_Travel_Time_20_to_24_Minutes + 
                                            pct_Travel_Time_25_to_29_Minutes + 
                                            pct_Travel_Time_30_to_34_Minutes +
                                            pct_Travel_Time_35_to_39_Minutes) * 100, 2),
    `% Long Commute (40+ min)` = round((pct_Travel_Time_40_to_44_Minutes + 
                                          pct_Travel_Time_45_to_59_Minutes + pct_Travel_Time_60_to_89_Minutes + 
                                          pct_Travel_Time_90_Plus_Minutes) * 100, 2),
    geometry = geometry
  )

# Prepare data for race map
race_long <- df_tract_wide %>%
  select(geometry, contains("Workers")) %>%  # Select relevant columns
  pivot_longer(
    cols = contains("Workers"),                    
    names_to = "race",                                  # New column for race names
    values_to = "percentage",                             # New column for percentage values
  ) %>%
  mutate(
    race = str_replace(race, "% ", ""),              # Remove "% " from race names
    race = str_replace(race, " Workers", "")        # Remove "Workers" from race names
  )

# Create race distribution maps
ggplot(race_long) +
  geom_sf(aes(fill = percentage)) +
  facet_wrap(~race, ncol = 2) +                           # Changed to 2 columns for better display
  scale_fill_distiller(                                    # Customize the color scale
    palette = "Blues",  # Use the Blues palette
    direction = 1,      # Light to dark
    name = "Population %",
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +                                        # Use a minimal theme
  labs(                                                    # Add labels
    title = "Racial Distribution of Workers in Santa Clara County (2022)",
    subtitle = "Percentage of population by race/ethnicity",
    caption = "Source: US Census Bureau, American Community Survey 5-Year Estimates"
  ) +
  theme(                                                    # Customize the theme
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    axis.title.x = element_blank(),                             # Remove axis titles
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),                              # Remove axis text
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),                             # Remove axis ticks
    axis.ticks.y = element_blank()
  )

# Create commute time distribution map
commute_long <- df_tract_wide %>%
  select(geometry, contains("Commute")) %>%
  pivot_longer(
    cols = contains("Commute"),
    names_to = "commute_type",
    values_to = "percentage" 
  ) %>%
  mutate(
    commute_type = str_replace(commute_type, "% ", "")
  )

ggplot(commute_long) +
  geom_sf(aes(fill = percentage)) +
  facet_wrap(~commute_type, ncol = 3) +
  scale_fill_distiller(
    palette = "Blues",
    direction = 1,
    name = "Population %",
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  labs(
    title = "Commute Time Distribution in Santa Clara County (2022)",
    subtitle = "Percentage of workers by commute duration",
    caption = "Source: US Census Bureau, American Community Survey 5-Year Estimates"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )


ggplot(commute_long) + 
  geom_boxplot(aes(x = commute_type, y = percentage))



# Reshape the data
df_long <- df_tract_wide %>%
  pivot_longer(
    cols = c("% Short Commute (0-40 min)", "% Long Commute (40+ min)"),
    names_to = "commute_type",
    values_to = "percentage"
  ) %>%
  mutate(commute_type = str_replace(commute_type, "% ", ""))


ggplot(df_tract_wide, aes(x = `Median Household Income ($)`, y = `% Long Commute (40+ min)`)) +
  geom_point(alpha = 0.5, color = "steelblue") +  # Changed point color for better contrast
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  theme_minimal() +
  labs(
    x = "Median Household Income",
    y = "Percentage of Long Commutes (40+ min)",
    title = "Relationship between Median Household Income and Long Commutes",
    subtitle = "Each point represents a census tract in Santa Clara County"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, NA)) +  # Ensure y-axis starts at 0
  scale_x_continuous(labels = scales::dollar_format(scale = 1/1000, suffix = "K")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "none"
  )


model <- lm(`% Long Commute (40+ min)` ~ `Median Household Income ($)`, data = df_tract_wide)

summary(model)


model_with_controls <- lm(`% Long Commute (40+ min)` ~ `Median Household Income ($)` +
                            `% Black Workers` + `% Hispanic Workers` + 
                            `% White Workers` + `% Asian Workers`, data = df_tract_wide)

# View the summary of the regression
summary(model_with_controls)
