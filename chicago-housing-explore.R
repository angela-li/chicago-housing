# This script loads and explores affordable housing data from the City of Chicago data portal
# Link: https://data.cityofchicago.org/Community-Economic-Development/Affordable-Rental-Housing-Developments/s6ha-ppgi


# Load packages -----------------------------------------------------------
library(RSocrata)
library(dplyr)
library(ggplot2)

# Import data -------------------------------------------------------------
socrata_url <- "https://data.cityofchicago.org/resource/uahe-iimk.csv"
housing_tbl <- read.socrata(socrata_url) %>% 
  as_tibble()
# Ctrl-Shift-M to produce "pipe" symbol


# Explore data ------------------------------------------------------------
class(housing_tbl)
head(housing_tbl)
View(housing_tbl)
str(housing_tbl)

# Ctrl-Enter to run one line
# press Tab to autocomplete!


# How many properties in each area have affordable housing in them? -------
properties_per_area <- housing_tbl %>% 
  group_by(community_area_number) %>% 
  tally()

areas_key <- housing_tbl %>% 
  group_by(community_area_number) %>% 
  slice(1) %>% 
  select(community_area, community_area_number)

left_join(properties_per_area, 
          areas_key, 
          by = "community_area_number") %>% 
  select(community_area, community_area_number, properties = n) %>% 
  arrange(desc(properties))


# Challenge: find neighborhoods with most housing UNITS -------------------

units_per_area <- housing_tbl %>% 
  group_by(community_area_number) %>% 
  summarize(total_units = sum(units)) %>% 
  left_join(units_per_area, 
          areas_key, 
          by = "community_area_number") %>% 
  select(community_area, community_area_number, total_units) %>% 
  arrange(desc(total_units))

# Ctrl-Shift R to insert a new section

# Visualize data ----------------------------------------------------------
ggplot(units_per_area, aes(x = total_units)) +
  geom_histogram(binwidth = 100) +
  labs(x = "Total Units By Community Area", y = "Count")

# make a histogram for the number of properties per area


names(units_per_area)
