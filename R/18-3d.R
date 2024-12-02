# Load required libraries
library(tmap)
library(sf)
library(ggplot2)
library(stars)
library(rayshader)

# Load dataset: elevation data and world boundaries
data(land, World)

# Create a regular grid with 1x1 degree resolution for raster projection
regular_grid <- st_as_stars(st_bbox(land), dx = 1, dy = 1)

# Reproject elevation data to Eckert IV projection
elevation_data <- st_warp(land, regular_grid) |>
  st_transform(crs = "+proj=eck4")

# Create a ggplot map with elevation and world borders
elevation_plot <- ggplot() +
  geom_stars(
    data     = elevation_data,
    mapping  = aes(fill = elevation)
  ) +
  geom_sf(
    data     = World,
    fill     = "transparent",
    colour   = "black",
    size     = 0.1
  ) +
  scale_fill_gradientn(
    colours  = terrain.colors(9),
    values   = c(0, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, 6000) / 6000,
    na.value = "#108ec1"
  ) +
  theme_void() +
  theme(
    legend.position  = "top",
    plot.margin      = margin(t = 10, b = 10, l = 0, r = 0),
    plot.background  = element_rect(
      fill   = "white",
      colour = "transparent"
    )
  )

# Create a 3D render of the elevation plot
plot_gg(
  elevation_plot,
  multicore = TRUE,
  width     = 8,
  height    = 5
)

# Adjust the camera view for 3D rendering
render_resize_window(width = 1288, height = 800)
render_camera(theta = -20, phi = 35, zoom = 0.60, shift_vertical = -100)

# Save the 3D rendered image
render_snapshot(filename = "plot/18-3d.png")
