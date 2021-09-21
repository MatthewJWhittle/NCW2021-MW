rm(list = ls())

require(tidyverse)
require(rvest)
require(xml2)

subset_i <-
  function(x, i, list = TRUE) {
    if (list) {
      x[[i]]
    } else{
      x[i]
    }
  }


select_from <-
  function(x, i) {
    x[i:length(x)]
  }


# Continents -------
scrape_continents <-
  function() {
    continents <-
      "https://en.climate-data.org/" %>%
      read_html() %>%
      html_element("li") %>%
      html_element("ul") %>%
      html_elements("a")
    
    continent_url <-
      continents %>% html_attr("href") %>% str_remove_all("/")
    continent_text <- continents %>% html_text()
    
    return(tibble(continent = continent_text, continent_url = continent_url))
  }


scrape_countries <-
  function(continent_url, return_input = TRUE) {
    countries_a <-
      "https://en.climate-data.org/{continent_url}/" %>%
      glue::glue() %>%
      read_html() %>%
      html_element("body") %>%
      html_element(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "f16", " " ))]') %>%
      html_elements("a")
    
    country <- countries_a %>% html_text()
    country_url <-
      countries_a %>% html_attr("href") %>% str_split("/") %>% map_chr( ~ .x[[3]])
    
    countries_table <-
      tibble(continent_url = continent_url,
             country = country,
             country_url = country_url)
    
    if (return_input) {
      return(countries_table)
    }
    countries_table %>% select(-continent_url)
  }


# Missing a step to get regions


# Cities

scrape_places <-
  function(continent_url,
           country_url,
           verbose = FALSE,
           return_input = TRUE) {
    if (verbose) {
      message(
        glue::glue(
          "Scraping continent_url: {continent_url} and country_url: {country_url}"
        )
      )
    }
    # Get the main menu
    main_menu <-
      "https://en.climate-data.org/{continent_url}/{country_url}/" %>%
      glue::glue() %>%
      read_html() %>%
      html_elements("ul") %>%
      subset_i(1)
    
    # find the places menu by where the string 'PlacesSelect a place' occurs
    menu_text <-
      main_menu %>% html_elements(css = "a") %>% html_text2()
    place_menu_index <-
      which(menu_text %in%  "PlacesSelect a place")
    
    places_menu <-
      main_menu %>% html_elements(css = "a") %>% select_from(place_menu_index + 1) %>%
      # Drop the 'more...' option (last element)
      head(-1)
    
    # Sometimes there aren't any places for a given country & continent
    # This code returns an empty tibble if thie is the case
    if (length(places_menu) > 0) {
      # If there are places, continue with function
      # Extract components of url
      places_text <- places_menu %>% html_text2()
      places_url <- places_menu %>% html_attr("href")
      
      # Generate and format a tibble to return
      places_table <-
        places_url %>% str_split("/") %>% do.call(rbind, .)
      # Where there is only one place, just adding column names to the matrix doesn't work
      # Instead I'm setting it to be a matrix with 6 cols and converting to a dataframe to ensure
      # it is composed by row
      places_table <- as.data.frame(matrix(places_table, ncol = 6))
      # Drop null columns
      places_table <- places_table[,c(-1,-6)]
      colnames(places_table) <-
        c("continent_url", "country_url", "region_url", "place_url")
      places_table <-
        bind_cols(as_tibble(places_table), place = places_text)
    } else{
      # Otherwise generate an empty table
      places_table <-
        tibble(
          continent_url = character(0),
          country_url = character(0),
          region_url = character(0),
          place_url = character(0)
        )
    }
    if (return_input) {
      return(places_table)
    }
    places_table %>% select(-continent_url, -country_url)
  }


places <-
  scrape_continents()  %>%
  mutate(countries = map(continent_url, ~ scrape_countries(.x, return_input = FALSE))) %>%
  unnest(countries) %>%
  mutate(places = map2(
    continent_url,
    country_url,
    ~ scrape_places(
      continent_url = .x,
      country_url = .y,
      return_input = FALSE
    )
  )) %>% unnest(places)



table <- 
  "https://en.climate-data.org/africa/nigeria/kano/kano-46648/#climate-table" %>% 
  read_html() %>% 
  html_elements("table") %>% 
  html_table() %>% 
  subset_i(2)


scrape_climate_table <- 
  function(url = NULL, 
           continent_url = NULL, country_url = NULL, region_url = NULL, place_url = NULL,
           verbose = FALSE){
    
    
    if (is.null(url)) {
      if (any(c(
        is.null(continent_url),
        is.null(country_url),
        is.null(region_url),
        is.null(place_url)
      ))) {
        message("Need either url or components")
        stop()
      }
      url <- glue::glue("https://en.climate-data.org/{continent_url}/{country_url}/{region_url}/{country_url}/#climate-table")
    }
    if(verbose){message(glue::glue("Getting: {url}"))}
    table <- 
      url %>%
      read_html() %>% 
      html_elements("table") %>% 
      html_table() %>% 
      subset_i(2)
    table <- janitor::clean_names(table)
    table <- table %>% rename(climate_variable = x)
    
  }


null_tibble <- 
  tibble(climate_variable = character(0), january = character(0), 
                 february = character(0), march = character(0), april = character(0), 
                 may = character(0), june = character(0), july = character(0), 
                 august = character(0), september = character(0), october = character(0), 
                 november = character(0), december = character(0))


safely_scrape_climate_table <- safely(scrape_climate_table, otherwise = null_tibble)
climate_data <- 
  places %>% 
  mutate(country_url_text = str_remove_all(country_url, "-[0-9]+$"),
         url = glue::glue("https://en.climate-data.org/{continent_url}/{country_url_text}/{region_url}/{country_url}/#climate-table"),
         climate_table = map(url, ~safely_scrape_climate_table(url = .x))) 


climate_data_transp <- 
  climate_data %>% 
  mutate(climate_table = map(climate_table, ~.x$result)) %>% 
  unnest(climate_table) %>% 
  pivot_longer(cols = c(january:december), names_to = "month", values_to = "climate_value") %>% 
  pivot_wider(names_from = climate_variable, values_from = climate_value) %>% 
  janitor::clean_names()


climate_data_tidy <- 
  climate_data_transp %>% 
  mutate(avg_temperature_c = str_extract(avg_temperature_c_f, "^[0-9]+(\\.[0-9]+)?") %>% parse_number(),
         min_temperature_c = str_extract(min_temperature_c_f, "^[0-9]+(\\.[0-9]+)?") %>% parse_number(),
         max_temperature_c = str_extract(max_temperature_c_f, "^[0-9]+(\\.[0-9]+)?") %>% parse_number(),
         precipitation_rainfall_mm = str_extract(precipitation_rainfall_mm_in, "^[0-9]+(\\.[0-9]+)?") %>% parse_number(),
         rainy_days_d = rainy_days_d %>% parse_number(),
         humidity = (str_extract(humidity_percent, "^[0-9]+(\\.[0-9]+)?") %>% parse_number()) / 100,
         avg_sun_hours_hours = avg_sun_hours_hours %>%  parse_number(),
         month = month %>% str_to_title()
         ) %>%
  select(-matches("*_temperature_c_f"), -humidity_percent, -precipitation_rainfall_mm_in,
         
         -matches("url")) %>%
  glimpse()


climate_data_tidy %>% 
  write_csv("project_data/place-monthly-climate-averages.csv")
