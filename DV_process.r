# London Street-Level Crime 2024
# Data Exploration, Cleaning, Preprocessing and Visualization
# Giorgia Brioschi mat 894407 g.brioschi12@campus.unimib.it
############################################

setwd("/Users/giorgiabrioschi/Desktop/Data Visualization/ProjectDV")

library(tidyverse)
library(ggplot2)
library(lubridate)

# DATA ACQUISITION ############################################

# Load the dataset and keep a raw copy for reference
crime <- readr::read_csv("London_Crimes_2024_Expanded.csv")
crime_raw <- crime

# DATA EXPLORATION ############################################

# Check monthly completeness
crime %>%
  mutate(month_num = month(month)) %>%
  count(month_num)

# Check on missing data
missing_summary <- crime %>%
  summarise(across(everything(), ~ sum(is.na(.))))
missing_summary

# DATA CLEANING E PREPROCESSING ############################################

# Remove fully duplicated rows
crime <- crime %>%
  distinct()

# Convert Month to Date format
crime <- crime %>%
  mutate(month = ym(Month))

# Handle records with Crime ID duplicated
# Keep only the latest record per ID
# Retain records without ID
crime_with_id <- crime %>% filter(!is.na(`Crime ID`))
crime_no_id   <- crime %>% filter(is.na(`Crime ID`))

crime_latest_id <- crime_with_id %>%
  arrange(`Crime ID`, desc(month)) %>%
  group_by(`Crime ID`) %>%
  slice_head(n = 1) %>%
  ungroup()

crime <- bind_rows(
  crime_latest_id,
  crime_no_id)

# Keep only records within Greater London coordinates
crime <- crime %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>% 
  filter(
    Longitude >= -0.510 & Longitude <= 0.334,
    Latitude  >= 51.28  & Latitude  <= 51.70
  )

# Remove December 2024 due to incomplete coverage
crime <- crime %>%
  filter(month < as.Date("2024-12-01"))

# TEMPORAL ANALYSIS ############################################

# Monthly crime trend for Datawrapper
monthly_crime <- crime %>%
  count(month)
write.csv(monthly_crime, "monthly_crime.csv", row.names = FALSE)

# Monthly crime trend by top 20 crime types for Flourish
top_crimes <- crime %>%
  count(`Crime type`, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(`Crime type`)
crime_by_type_month <- crime %>%
  filter(`Crime type` %in% top_crimes) %>%
  count(month, `Crime type`)
crime_wide <- crime_by_type_month %>%
  pivot_wider(
    names_from = `Crime type`,
    values_from = n,
    values_fill = 0
  )
write.csv(crime_wide, "crime_by_type_month_2024_wide.csv", row.names = FALSE)


# SPATIAL ANALYSIS ############################################

# Hexagonal binning map for Central London
crime_central <- crime %>%
  filter(
    Longitude >= -0.25 & Longitude <= 0.0,
    Latitude  >= 51.45 & Latitude  <= 51.58
  )
ggplot(crime_central, aes(x = Longitude, y = Latitude)) +
  geom_hex(bins = 70) +
  scale_fill_viridis_c(option = "plasma") +
  coord_fixed(ratio = 1.3) +
  labs(
    title = "Crime Clusters in Central London (2024)",
    x = "Longitude",
    y = "Latitude",
    fill = "Number of crimes"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 20))

# Top 10 LSOAs and top 6 crime types for Flourish
top_6_crimes <- crime %>%
  count(`Crime type`, sort = TRUE) %>%
  slice_head(n = 6) %>%
  pull(`Crime type`)
top_10_lsoa <- crime %>%
  count(`LSOA name`, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(`LSOA name`)
crime_wide_flourish <- crime %>%
  filter(
    `LSOA name` %in% top_10_lsoa,
    `Crime type` %in% top_6_crimes
  ) %>%
  group_by(`LSOA name`, `Crime type`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = `Crime type`,
    values_from = Count,
    values_fill = 0
  )
write.csv(crime_wide_flourish, "top10_LSOA_top6_crimes_wide.csv", row.names = FALSE, fileEncoding = "UTF-8")

# OUTCOME ANALYSIS ############################################

# Percentage distribution of outcomes for top 6 crime types
crime_outcome_pct <- crime %>%
  filter(`Crime type` %in% top_6_crimes) %>%
  count(`Crime type`, `Last outcome category`) %>%
  group_by(`Crime type`) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()
ggplot(crime_outcome_pct,
       aes(x = `Crime type`, y = percent, fill = `Last outcome category`)) +
  geom_col(width = 0.75) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(
    title = "Percentage Distribution of Crime Outcomes by Crime Type in London (2024)",
    x = NULL,
    y = NULL,
    fill = "Outcome"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 20))
