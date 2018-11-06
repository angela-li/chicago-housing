# This script loads and explores affordable housing data from the City of Chicago data portal
# Link: https://data.cityofchicago.org/Community-Economic-Development/Affordable-Rental-Housing-Developments/s6ha-ppgi


# Load packages -----------------------------------------------------------
library(RSocrata)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)

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

# option  -
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
ggplot(properties_per_area, aes(x = n)) +
  geom_histogram() +
  labs(x = "Total Properties By Community Area", y = "Count")

# Make a leaflet map ------------------------------------------------------
# Ctrl shift m for pipe
leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(lng = housing_tbl$longitude, 
             lat = housing_tbl$latitude, 
             popup = housing_tbl$property_name,
             radius = housing_tbl$units)

housing_sf <- st_as_sf(housing_tbl, coords = c("longitude", "latitude"))

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(data = housing_sf)
