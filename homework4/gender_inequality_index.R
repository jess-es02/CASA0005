#Data Sources:
#Country shapefile data: ArcGIS Hub, World Countries Generalised
#GII data: https://hdr.undp.org/data-center/documentation-and-downloads
    #Filters: GII, GII, 2010/2019, no geographical restrictions

#Load libraries
library(tidyverse)
library(countrycode)
library(sf)
library(janitor)
library(here)

#Load in data
shapefile <- st_read(here("homework4", "world_countries.geojson")) %>%
  clean_names(.) #WGS84
plot(shapefile)

gii <- read_csv(here("homework4", "hdr-data.csv")) %>%
  clean_names()
view(gii)

# ====== Inspect and clean GII data ========

# Consider variable types
gii_data_types <- gii %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
    names_to = "variable",
    values_to = "variable_type")
# All looks sensible
rm(gii_data_types)

# Drop redundant columns
gii_clean <- gii %>%
  dplyr::select(-index_code, -index, -dimension, -indicator_code,
                -indicator, -note)

# Remove any ISO codes beginning with "ZZ"
# These are statistics for whole continents, HDI levels, or the globe
# (ChatGPT helped with the str_starts function)
gii_clean <- gii_clean %>%
  filter(., !str_starts(country_iso_code, "ZZ"))

# Pivot table, adding nulls if there isn't a row for that year
gii_pivoted <- gii_clean %>%
  pivot_wider(., names_from = year, values_from = value, names_prefix = "gii_")
# Count distinct countries in original dataset, to make sure this has been done properly
gii %>%
  filter(., !str_starts(country_iso_code, "ZZ")) %>%
  distinct(country_iso_code) %>%
  nrow() #165 - this matches. Note that many countries are missing

# Create difference column
# Some countries have null values for one year
# As difference cannot be calculated, I have kept these values null
# But whoever is mapping can choose to change this
gii_final <- gii_pivoted %>%
  mutate(diff = gii_2019 - gii_2010)
# Note: negative numbers constitute a DECREASE in gender inequality over time

# ========= Join to shapefile ==========
#shapefile has 2 digit ISOs
# gii_final has 3 digit ISOs

# Create a 2-digit ISO column in the gii_final df
gii_final <- gii_final %>%
  mutate(iso2 = countrycode(country_iso_code, origin = "iso3c", destination = "iso2c"))

# Left join to shapefile df
shapefile_joined <- shapefile %>%
  left_join(., gii_final, by = c("iso" = "iso2"))%>%
  select(country.x, iso, gii_2010, gii_2019, diff, geometry)%>%
  rename(country = country.x)

# The countrycode package could also be used to connect countries to their parent countries (e.g. Puerto Rico -> US), but I'm not sure whether this is needed in the context of this homework