# Load required libraries
library(sf)
library(osmdata)
library(dplyr)
library(raster)
library(elevatr)
library(ggplot2)
library(ggspatial)

# Get the centroid of "Just Room Enough Island"
island_centroid <- getbb(
  place_name = "Just Room Enough Island",
  format     = "sf_polygon"
) |>
  st_centroid()

# Extract centroid coordinates
centroid_coordinates <- st_coordinates(island_centroid)

# Obtain OpenStreetMap data around the centroid
island_data <- opq_around(
  lon    = centroid_coordinates[1],
  lat    = centroid_coordinates[2],
  radius = 30
) |>
  osmdata_sf()

# Extract relevant OpenStreetMap data
island_shape <- island_data$osm_polygons |>
  filter(name == "Just Room Enough Island")

island_house <- island_data$osm_polygons |>
  filter(name == "Just Room Enough")

island_wood <- island_data$osm_polygons |>
  st_filter(island_shape) |>
  filter(natural == "wood")

island_tree <- island_data$osm_points |>
  st_filter(island_shape) |>
  filter(natural == "tree")

# Create a buffer around the island for elevation data extraction
island_boundary <- island_centroid |>
  st_buffer(dist = 30) |>
  st_bbox() |>
  st_as_sfc()

# Retreive elevation data for the buffered area
elevation_data <- get_elev_raster(
  locations = st_sf(island_boundary),
  z         = 14,
  src       = "aws",
  clip      = "locations"
) |>
  as.data.frame(xy = TRUE)

# Rename elevation column for clarity
colnames(elevation_data)[3] <- "elevation"

# Create the plot
island_map <- ggplot() +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradient(low = "#597b8d", high = "#7fa1d8") +
  geom_sf(
    data      = island_shape,
    fill      = "#ffe0d1",
    colour    = "#756f6ebb",
    linewidth = 0.5
  ) +
  geom_sf(
    data   = island_house,
    fill   = "#b5323b",
    colour = "transparent"
  ) +
  geom_sf(
    data   = island_wood,
    fill   = "#a18f52ee",
    colour = "transparent"
  ) +
  geom_sf(
    data   = island_tree,
    shape  = 19,
    size   = 1.5,
    colour = "#483b23",
    stroke = FALSE
  ) +
  coord_sf(
    xlim = st_bbox(island_boundary)[c("xmin", "xmax")],
    ylim = st_bbox(island_boundary)[c("ymin", "ymax")]
  ) +
  ggtitle("Just Room Enough Island") +
  annotation_scale(
    location    = "br",
    line_width  = 0,
    height      = unit(1, "mm"),
    pad_x       = unit(4, "mm"),
    pad_y       = unit(4, "mm"),
    text_cex    = 0.45,
    text_family = "serif"
  ) +
  theme_void(base_size = 5) +
  theme(
    legend.position     = "none",
    plot.title.position = "plot",
    plot.title          = element_text(
      family = "serif",
      hjust  = 0.12,
      margin = margin(t = 5, b = -2)
    ),
    plot.margin = margin(l = -1)
  )

# Save the map as an image file
ggsave(
  plot     = island_map,
  filename = "plot/27-micromapping.png",
  bg       = "white",
  width    = 600,
  height   = 600,
  dpi      = 300,
  units    = "px"
)
