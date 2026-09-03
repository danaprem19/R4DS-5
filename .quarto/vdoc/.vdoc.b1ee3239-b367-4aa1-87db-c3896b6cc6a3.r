#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(purrr)
library(leaflet)
library(rvest)
library(httr2)
library(jsonlite)
#
#
#
raw <- read_json("data/wildfires.geojson")
cat("Number of features:", length(raw$features), "\n")
#
#
#
first_fire <- raw$features[[1]]
names(first_fire)
#
#
#
first_coordinate <- first_fire$geometry$coordinates[[1]][[1]]
first_coordinate
#
#
#
features_tbl <- tibble(features = raw$features)
features_tbl
#
#
#
features_wide <- features_tbl |> 
  unnest_wider(features)
features_wide
#
#
#
properties_wide <- features_wide |> 
  unnest_wider(properties)
properties_wide
#
#
#
#| cache: true
fires <- properties_wide |> 
  unnest_wider(geometry, names_sep = "_") |> 
  select(incident, gis_acres, fire_year, agency, state, geometry_coordinates) |> 
  mutate(
    gis_acres = as.numeric(gis_acres),
    fire_year = as.integer(fire_year)
  )
#
#
#
august_fires <- fires |> 
  filter(str_detect(incident, regex("\\bAugust\\b", ignore_case = TRUE)))
fires |> 
  group_by(state) |> 
  summarize(total_acres = sum(gis_acres, na.rm = TRUE)) |> 
  slice_max(order_by = total_acres, n = 10) |> 
  arrange(desc(total_acres))
#
#
#
big_fires <- fires |> 
  filter(gis_acres >= 100000)

big_fires <- big_fires |> 
  mutate(
    lon = map_dbl(geometry_coordinates, \(boundary) {
      mean(map_dbl(boundary[[1]], \(coordinate) coordinate[[1]]))
    }),
    lat = map_dbl(geometry_coordinates, \(boundary) {
      mean(map_dbl(boundary[[1]], \(coordinate) coordinate[[2]]))
    })
  )
#
#
#
big_fires |> 
  count(agency, sort = TRUE) |> 
  ggplot(aes(x = reorder(agency, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Big Fires by Managing Agency",
    x = "Managing agency",
    y = "Number of fires"
  )
#
#
#
leaflet(big_fires) |> 
  addTiles() |> 
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = ~sqrt(gis_acres) / 50,
    stroke = FALSE,
    fillOpacity = 0.7
  ) |> 
  setView(lng = -115, lat = 40, zoom = 4)
#
#
#
#
