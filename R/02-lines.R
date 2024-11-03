# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)

# Download river data
# The dataset is available at HydroSHEDS website.
# To download the Africa river data, visit: https://www.hydrosheds.org/

# Load river shapefile data from the local directory
rivers <- st_read("data-raw/HydroRIVERS_v10_af_shp/HydroRIVERS_v10_af.shp")

# Define the bounding box for the target area
target_bbox <- c(xmax = 32.90, xmin = 32.30, ymax = 15.95, ymin = 15.25) |>
  st_bbox() |>
  st_as_sfc() |>
  st_set_crs(st_crs(rivers))

# Filter Nile River data within target area
nile_river <- rivers |>
  st_filter(target_bbox) |>
  st_transform(crs = 3857)

# Define the location of Khartoum city as point geometry
khartoum_location <- st_sfc(st_point(c(32.52, 15.58)), crs = st_crs(rivers)) |>
  st_transform(crs = 3857)

# Plot Nile River and Khartoum city
ggplot(nile_river) +
  geom_sf(colour = "#91d4e6", linewidth = 1.5) +
  geom_sf(data = khartoum_location, colour = "#888888") +
  geom_sf_text(data = khartoum_location, label = "Khartoum", colour = "#666666",
               nudge_y = 3000, fontface = "bold") +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.grid = element_line(linetype = "dashed"))
