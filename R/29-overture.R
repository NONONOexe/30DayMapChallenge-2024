# Load required libraries
library(sf)
library(jsonlite)
library(mapgl)

# Define a function to extract building names
extract_building_name <- function(json_string) {
  if (is.na(json_string)) {
    return(NA_character_)
  }

  name_data <- fromJSON(json_string)$common$zh
  building_name <- ifelse(is.null(name_data), NA_character_, name_data)

  return(building_name)
}

# Load geographic data
# Source: https://explore.overturemaps.org/#16.48/22.287615/114.154714/96.2/60
geojson_file <- "data-raw/overture-16.477975872902217-22.287614746370608-114.15471397850456.geojson"
overture_buildings <- read_sf(geojson_file)

# Extract building names using the defined function
overture_buildings$building_name <- sapply(
  overture_buildings$names,
  extract_building_name,
  USE.NAMES = FALSE
)

# Calculate the center coordinates
bbox <- st_bbox(overture_buildings)
center_longitude <- mean(c(bbox[c("xmin", "xmax")]))
center_latitude <- mean(c(bbox[c("ymin", "ymax")]))

# Create map
map <-
  maplibre(
    center  = c(center_longitude, center_latitude),
    zoom    = 14,
    pitch   = 60,
    bearing = -50
  ) |>
  add_source(
    id   = "buildings",
    data = overture_buildings
  ) |>
  add_layer(
    id     = "building_layer",
    source = "buildings",
    type   = "fill-extrusion",
    paint  = list(
      "fill-extrusion-color"   = "#2c2e7f",
      "fill-extrusion-height"  = get_column("height"),
      "fill-extrusion-base"    = 0,
      "fill-extrusion-opacity" = 0.8
    )
  ) |>
  add_layer(
    id     = "building_name_layer",
    source = "buildings",
    type   = "symbol",
    layout = list(
      "text-field" = get_column("building_name"),
      "text-font"  = list("Open Sans Regular"),
      "text-size"  = 12
    ),
    paint = list(
      "text-color"      = "#000000",
      "text-halo-color" = "#ffffff",
      "text-halo-width" = 1
    )
  )

map
