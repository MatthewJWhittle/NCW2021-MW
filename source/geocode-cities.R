rm(list = ls())

library(tidygeocoder)
library(tidyverse)

temps <-
  # List files
  fs::dir_ls("project_data/", regexp = "daily*") %>%
  # Load them into a dataframe
  vroom::vroom(show_col_types = FALSE) %>% 
  # tidy the column names
  janitor::clean_names()





city_xy <-
  temps %>%
  select(city, country, region) %>% 
  distinct() %>% 
  mutate(address = paste(city, country, region, sep = ", ")) %>%
  geocode(
    address = address,
    method = 'arcgis',
    lat = latitude ,
    long = longitude
  )


city_xy %>% write_csv("project_data/location/city-xy.csv")