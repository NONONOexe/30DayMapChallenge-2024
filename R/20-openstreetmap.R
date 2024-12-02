# Load required libraries
library(osmdata)
library(sf)
library(dplyr)
library(mapgl)

# Define helper function to create a buffered area
create_buffered_area <- function(lon, lat, buffer_radius) {
  st_point(c(lon, lat)) |>
    st_sfc(crs = 4326) |>
    st_transform(crs = 3857) |>
    st_buffer(buffer_radius) |>
    st_transform(crs = 4326) |>
    st_as_sf()
}

# Define central coordinates (Tokyo Station)
lon <- 139.764516
lat <- 35.6811124


# Create buffered areas:
# Small buffer for target area
target_area <- create_buffered_area(lon, lat, 1000)

# Large buffer for the surrounding area
surrounding_area <- create_buffered_area(lon, lat, 100000)

# Fetch OSM data for different features:
# Buildings
osm_buildings <- opq_around(
    lon = lon, lat = lat, radius = 1000, key = "building"
  ) |>
  osmdata_sf()
buildings_data <- osm_buildings$osm_polygons |>
  mutate(
    render_height = if_else(is.na(height), 10.0, as.numeric(height))
  )

# Highways
osm_highways <- opq_around(
    lon = lon, lat = lat, radius = 1000, key = "highway"
  ) |>
  osmdata_sf()

# Water features
osm_waters <- opq_around(
    lon = lon, lat = lat, radius = 1000, key = "natural", value = "water"
  ) |>
  osmdata_sf()

# Railways
osm_railways <- opq_around(
    lon = lon, lat = lat, radius = 1000, key = "railway"
  ) |>
  osmdata_sf()

# Initialize the map with a specified style and view
map <- maplibre(
  style   = carto_style("positron-no-labels"),
  center  = c(139.764, 35.681),
  zoom    = 13.5,
  pitch   = 60,
  bearing = -50
)

# Add layers to the map
map |>
  # Surrounding area
  add_source(
    id   = "surrounding-area",
    data = surrounding_area
  ) |>
  add_layer(
    id     = "surrounding-area-fill",
    type   = "fill",
    source = "surrounding-area",
    paint  = list(
      "fill-color" = "#eeebd8"
    )
  ) |>
  # Target area
  add_source(
    id   = "target-area",
    data = target_area
  ) |>
  add_layer(
    id     = "target-area-fill",
    type   = "fill",
    source = "target-area",
    paint  = list(
      "fill-color" = "#ffffff"
    )
  ) |>
  # Railways
  add_source(
    id   = "railways",
    data = osm_railways$osm_lines
  ) |>
  add_layer(
    id     = "railways-line",
    type   = "line",
    source = "railways",
    paint = list(
      "line-width" = 3,
      "line-color" = "#24878a"
    )
  ) |>
  # Water bodies
  add_source(
    id   = "water-features",
    data = osm_waters$osm_polygons
  ) |>
  add_layer(
    id     = "water-fill",
    type   = "fill",
    source = "water-features",
    paint = list(
      "fill-color" = "#a8c5e6"
    )
  ) |>
  # Highways
  add_source(
    id   = "highways",
    data = osm_highways$osm_lines
  ) |>
  add_layer(
    id     = "highways-line",
    type   = "line",
    source = "highways",
    paint = list(
      "line-width" = 3,
      "line-color" = "#6a6464"
    )
  ) |>
  # Buildings with 3D extrusion
  add_source(
    id   = "buildings",
    data = buildings_data
  ) |>
  add_layer(
    id     = "buildings-3d",
    type   = "fill-extrusion",
    source = "buildings",
    paint = list(
      "fill-extrusion-color"   = "white",
      "fill-extrusion-height"  = get_column("render_height"),
      "fill-extrusion-base"    = 0,
      "fill-extrusion-opacity" = 1.0
    )
  )
