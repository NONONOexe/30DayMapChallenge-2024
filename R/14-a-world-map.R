# Load required libraries
library(marmap)
library(stars)
library(dplyr)
library(tmap)

# Fetch bathymetric data
bathymetry <- getNOAA.bathy(
  lon1 = -180, lon2 = 180,
  lat1 = -90, lat2 = 90,
  resolution = 20
)

# Convert bathymetric data to a stars object
bathymetry_stars <- bathymetry |>
  as.raster() |>
  st_as_stars()

# Load land elevation
data(land)

# Combine bathymetry and land elevation data
elevation_combined <- land |>
  transmute(height = ifelse(is.na(elevation), bathymetry_stars$layer, elevation))

# Define a bounding box for cropping
bounding_box <- st_bbox(
  c(xmin = -90, xmax = 90, ymin = -90, ymax = 90),
  crs = st_crs(elevation_combined)
)

# Crop and transform the combined data to an orthographic projection
elevation_transformed <- elevation_combined |>
  st_crop(bounding_box) |>
  st_transform(crs = "+proj=ortho +lat_0=0 +lon_0=0 +datum=WGS84")

# Create a globe-like map showing elevation and bathymetry
world_map <- tm_shape(elevation_transformed) +
  tm_raster(
    "height",
    breaks = c(
      -Inf, -8000, -5000, -2000, -1000, 0, 250, 500,
      1000, 1500, 2000, 2500, 3000, 4000, Inf
    ),
    palette = c(
      "#442b55", "#1b2757", "#083e7b", "#516ab2",
      "#4a64a2", terrain.colors(9)
    )
  ) +
  tm_layout(
    legend.show = FALSE,
    bg.color    = "black",
    frame       = FALSE
  )

# Save the map
tmap_save(
  world_map,
  "plot/14-a-world-map.png",
  width = 1000, height = 1000, dpi = 300,
  bg = "black"
)
