# Load required libraries
library(sf)
library(osmdata)
library(showtext)
library(sysfonts)
library(ggplot2)

# Get the centroid coordinates for the White House
white_house_centre <- getbb(
    place_name = "White House",
    format_out = "sf_polygon"
  ) |>
  _$polygon |>
  st_centroid()

centre_coords <- st_coordinates(white_house_centre)

# Fetch buildings and roads data within a 500m radius
buildings_data <- opq_around(
    lon    = centre_coords[1],
    lat    = centre_coords[2],
    radius = 500,
    key    = "building"
  ) |>
  osmdata_sf()

roads_data <- opq_around(
    lon    = centre_coords[1],
    lat    = centre_coords[2],
    radius = 500,
    key    = "highway"
  ) |>
  osmdata_sf()

# Create a bounding box for the map area
bounding_box <-
  white_house_centre |>
  st_transform(crs = 32618) |>
  st_buffer(dist = 300) |>
  st_transform(crs = 4326) |>
  st_bbox()

# Set Google Fonts for the plot
font_add_google("Noto Serif", "Noto Serif")
showtext_auto()

# Define plot colours
foreground_colour <- "#FFFFFF"
background_colour <- "#3C3B6E"

# Create the map plot
map_plot <- ggplot() +
  geom_sf(
    data   = buildings_data$osm_polygons,
    fill   = foreground_colour,
    colour = background_colour
  ) +
  geom_sf(
    data      = roads_data$osm_lines,
    colour    = foreground_colour,
    linewidth = 0.5,
    lineend   = "round"
  ) +
  geom_text(
    aes(label = "White House"),
    x      = centre_coords[1],
    y      = centre_coords[2] + 0.0014,
    colour = foreground_colour,
    family = "Noto Serif",
    size = 10
  ) +
  coord_sf(
    xlim = bounding_box[c("xmin", "xmax")],
    ylim = bounding_box[c("ymin", "ymax")]
  ) +
  theme_void() +
  theme(panel.background = element_rect(fill = background_colour))

# Save the plot as an image file
ggsave(
  plot     = map_plot,
  filename = "plot/22-2-colours.png",
  bg       = "#3c3b6e",
  width    = 1000,
  height   = 1000,
  dpi      = 300,
  units    = "px"
)
