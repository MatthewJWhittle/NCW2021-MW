rm(list = ls())

require(sf)
require(gstat)
require(tidyverse)
require(getarc)

weather <- read_csv("project_data/place-monthly-climate-averages.csv")
weather_p <- st_read("project_data/location/weather-average-points.geojson")

weather_spat <- 
  weather_p %>% 
  right_join(weather)

ep_continents <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/World_Continents/FeatureServer/0"
continents <- query_layer(endpoint = ep_continents)

continent_grid <- st_make_grid(continents, 2, what = "centers")

continent_grid <- continent_grid %>% st_as_sf() %>%  st_filter(continents)



# Apply kriging function across months
months <- unique(weather_spat$month)

weather_spat <- weather_spat  %>% st_transform(crs = 3857) 
continent_grid <- continent_grid %>% st_transform(crs = 3857)


# Kriging ----
humidity_grids <-
  map(
    months,
    ~ automap::autoKrige(
      formula = humidity ~ 1,
      input_data = weather_spat %>%
        filter(month == .x) %>%
        as_Spatial(),
      new_data =
        continent_grid %>%
        as_Spatial()
    )
  ) %>% set_names(months)





add_coords <-
  function(x, names = c("x", "y")) {
    xy <- sf::st_coordinates(x)
    xy <- tibble::as_tibble(xy)
    colnames(xy) <- names
    dplyr::bind_cols(x, xy)
  }


  


# Create rasters & combine to a brick
rasters <- 
  map(months, 
    ~humidity_grids[[.x]]$krige_output  %>% 
      st_as_sf() %>% 
      st_transform(4326) %>% 
      add_coords() %>% 
      select(X = x, Y = y, Z = var1.pred) %>% 
      st_drop_geometry() %>%
      as.data.frame() %>% 
      raster::rasterFromXYZ()
 )  


rasters <- 
  map(rasters, 
    function(x){
      raster::crs(x) <- sp::CRS("EPSG:4326")
      return(x)
    })

brick <- raster::brick(rasters)


names(brick) <- months


raster::writeRaster(brick, filename = "project_data/humidity-monthly-average.grd")




krige_to_raster <-
  function(krige, crs = 4326) {
    raster <-
      krige$krige_output  %>%
      st_as_sf() %>%
      st_transform(crs) %>%
      add_coords() %>%
      select(X = x, Y = y, Z = var1.pred) %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      raster::rasterFromXYZ()
    
    
    
    raster::crs(raster) <- sp::CRS(glue::glue("EPSG:{crs}"))
    return(raster)
  }