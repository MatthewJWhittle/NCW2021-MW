rm(list = ls())

require(tidyverse)
require(sf)
require(getarc)


ep_world_boundaries <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/World_Countries_%28Generalized%29/FeatureServer/0"
world_boundaries <- query_layer(endpoint = ep_world_boundaries) %>% janitor::clean_names()


world_grid <- st_make_grid(world_boundaries, cellsize = 4, what = "centers")
world_grid <- st_filter(world_grid %>% st_as_sf(), world_boundaries)

world_grid %>% st_write("project_data/location/world-grid.geojson", delete_dsn = TRUE)
world_boundaries %>% st_write("project_data/location/world-boundaries.geojson", delete_dsn = TRUE)
