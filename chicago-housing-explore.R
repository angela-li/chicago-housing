library(RSocrata)
library(dplyr)

# Import data
socrata_url <- "https://data.cityofchicago.org/resource/uahe-iimk.csv"
housing <- read.socrata(socrata_url) %>% 
  as_tibble()

#Ctrl Shift M

# Explore data
class(housing)
head(housing)
View(housing)
str(housing)

# Ctrl-Enter to run
# press Tab to autocomplete!

# How many properties in each area have affordable housing in them?
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

# Challenge: find neighborhoods with most housing UNITS

housing_tbl %>% 
  group_by(community_area_number) %>% 
  summarize(total_units = sum(units))
