################################################################################
## Project: Richard_NCAA_BB_Data_Query_Prep
## Script purpose: Prepare Data to Replicate FiveThirtyEight NCAA College Basketball Map
## Date: 26-Sep-2019
## Author:Roland Richard
################################################################################

library(bigrquery)
library(DBI)
library(tidyverse)
library(ggmap)
library(sf)


# Query NCAA Database -----------------------------------------------------

# Project ID
mbb <- " ncaa-bb-254118"

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "ncaa_basketball",
  billing = mbb
)


# Load All Division I Men's Basketball Teams
ncaa_mbb <- dbReadTable(con,"mbb_teams")



# Filter Power 5 (and Big East) Conferences
power6 <- ncaa_mbb %>% filter(conf_alias %in% c("ACC","BIG10","BIG12","BIGEAST","PAC12","SEC"))

# Select Addresses of Power 6 Program Venues for Geocoding ---------------------

# Register Google Geocoder API Key
register_google(key = 'AIzaSyCmbbwTolhJmXVi5bIuILHQbx89rj8DnDQ')

power6Geo <- power6 %>%
  unite(col = 'addresses', c(venue_address,venue_city,venue_state),sep = ",",remove = FALSE) %>%
  mutate(type = "school")


# loop through addresses to geocode lat/lon
for(i in 1:nrow(power6Geo))
{
  # Print("Working...")
  result <- geocode(power6Geo$addresses[i], output = "latlona")
  power6Geo$lon[i] <- as.numeric(result[1])
  power6Geo$lat[i] <- as.numeric(result[2])
  power6Geo$geoAddress[i] <- as.character(result[3])
}

# Geocode Conference Conference Geographic Centers, Suggested, and Actual Tournament Locations
conf_loc <- read_csv("data/conf_tourn_locations.csv")

# rename columns to bind with Power6 data
confGeo <- conf_loc %>%
  select(-c(sugg_dist, tourney_dist)) %>%
  gather(alias, city, -conf_alias) %>%
  # create type variable for easier grouping in ggplot
  mutate(
    type = case_when(
      alias == "geog_center" ~ "center",
      alias == "tourney_loc" ~ "tourney",
      alias == "sugg_loc" ~ "suggested"
    )
  )

for(i in 1:nrow(confGeo))
{
  # Print("Working...")
  result <- geocode(confGeo$city[i], output = "latlon")
  confGeo$lon[i] <- as.numeric(result[1])
  confGeo$lat[i] <- as.numeric(result[2])
}

confGeo <- confGeo %>% filter(type != 'suggested')

# Bind Power6 School geographic data with conference geographic data

typ <- list(`1` = "school", `2` = "center", `3` = "tourney")
conf <- list(`1` = "ACC", `2` = "BIG10", `3` = "BIGEAST", `4` = "PAC12", `5` ="SEC",`6` = "BIG12")

power6sf <- power6Geo %>%
  select(conf_alias, type,alias,venue_city,venue_state,lon,lat) %>%
  unite('city',c(venue_city,venue_state),sep = ", ",remove = T) %>%
  bind_rows(confGeo) %>%
  st_as_sf( coords = c('lon','lat'),agr = 'constant',crs = 4326, remove = FALSE) %>%
  mutate(type = recode_factor(type,!!!typ),
         conf_alias = recode_factor(conf_alias, !!!conf)
         ) %>%
  mutate(conf_name = factor(conf_alias,
                            labels = c(
                              "Atlantic Coast Conference",
                              "Big Ten",
                              "Big East",
                              "Pacific 12",
                              "Southeastern Conference",
                              "Big 12"
                            ))
  )


# save prepared data
write_rds(ncaa_mbb,"data/ncaa_mbb_all_d1.rds")
write_rds(power6, "data/ncaa_mbb_power6.rds")
write_rds(power6sf,"data/ncaa_mbb_power6_geocoded.rds")


