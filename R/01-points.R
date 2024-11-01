# Load required libraries
library(osmdata)
library(sf)
library(tmap)

# Fetch temples and shrines data from OpenStreetMap in Aichi-ken
temples_data <- opq(bbox = "Aichi-ken") |>
  add_osm_feature(key = "building", value = "temple") |>
  osmdata_sf()
shrines_data <- opq(bbox = "Aichi-ken") |>
  add_osm_feature(key = "building", value = "shrine") |>
  osmdata_sf()

# Calculate centroids for temple and shrine polygons
temples_centroid <- st_centroid(temples_data$osm_polygons$geometry)
shrines_centroid <- st_centroid(shrines_data$osm_polygons$geometry)

# Set tmap mode to view for interactive map visualization
tmap_mode("view")

# Plot temples and shrines on the map with different symbols and colors
tm_shape(temples_point) +
  tm_dots(col = "#5BAD92", shape = 22, size = 0.05, border.col = "transparent") +
  tm_shape(shrine_point) +
  tm_dots(col = "#F26522", shape = 23, size = 0.05, border.col = "transparent")
