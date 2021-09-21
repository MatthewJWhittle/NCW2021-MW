rm(list = ls())

require(sf)
require(tidyverse)
require(tidygeocoder)


weather <- read_csv("project_data/place-monthly-climate-averages.csv")

weather_xy <-
  weather %>% 
  select(continent, country, place) %>% 
  distinct() %>% 
  mutate(address = paste(place, country, continent)) %>% 
  geocode(address = address, method = "arcgis")


weather_points <-  weather_xy %>% st_as_sf(coords = c("long", "lat"))
weather_points %>% st_write("project_data/location/weather-average-points.geojson")
