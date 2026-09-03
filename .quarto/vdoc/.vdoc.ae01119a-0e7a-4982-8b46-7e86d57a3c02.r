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
largest_fires <- big_fires |> 
  slice_max(gis_acres, n = 10, with_ties = FALSE)

longitudes <- map(largest_fires$geometry_coordinates, \(boundary) {
  map_dbl(boundary[[1]], \(coordinate) coordinate[[1]])
})
latitudes <- map(largest_fires$geometry_coordinates, \(boundary) {
  map_dbl(boundary[[1]], \(coordinate) coordinate[[2]])
})

fire_map <- reduce(
  seq_len(nrow(largest_fires)),
  \(map, i) {
    addPolygons(
      map,
      lng = longitudes[[i]],
      lat = latitudes[[i]],
      popup = paste0(
        "<strong>", largest_fires$incident[[i]], "</strong><br>",
        "Year: ", largest_fires$fire_year[[i]], "<br>",
        "Acres: ", format(largest_fires$gis_acres[[i]], big.mark = ",", scientific = FALSE)
      )
    )
  },
  .init = addTiles(leaflet())
)

fire_map |> 
  fitBounds(
    lng1 = min(unlist(longitudes)),
    lat1 = min(unlist(latitudes)),
    lng2 = max(unlist(longitudes)),
    lat2 = max(unlist(latitudes))
  )
#
#
#
imdb_snapshots <- readRDS("data/imdb_snapshots.rds") |> 
  as_tibble()
imdb_snapshots

imdb_snapshots |> 
  count(snap_year)
#
#
#
rank_changes <- imdb_snapshots |> 
  filter(snap_year %in% c(2015, 2022)) |> 
  select(title, year, snap_year, rank) |> 
  pivot_wider(names_from = snap_year, values_from = rank) |> 
  drop_na(`2015`, `2022`) |> 
  mutate(
    rank_change = abs(`2015` - `2022`),
    release_decade = 10 * floor(year / 10)
  )

rank_changes |> 
  arrange(desc(rank_change)) |> 
  slice_head(n = 10)
#
#
#
rank_changes |> 
  group_by(release_decade) |> 
  summarize(average_rank_change = mean(rank_change)) |> 
  arrange(release_decade)
#
#
#
imdb_snapshots |> 
  filter(title %in% c(
    "The Dark Knight",
    "Inception",
    "Interstellar",
    "The Dark Knight Rises",
    "Memento",
    "The Prestige",
    "Batman Begins"
  )) |> 
  select(title, snap_year, rank) |> 
  pivot_wider(names_from = snap_year, values_from = rank)
#
#
#
#
