# Load required libraries
library(ggplot2)
library(sf)
library(rnaturalearth)

# Load world map data as an sf object
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Create a set of points with city names, coordinates, and timestamps
city_points <- st_as_sf(
  data.frame(
    city = c("Nagoya", "Helsinki", "Geneve", "Madrid"),
    lon  = c(136.81, 24.96, 6.11, -3.58),
    lat  = c(34.86, 60.32, 46.23, 40.48),
    date = c("Sep 9, 11 PM", "Sep 10, 6 AM", "Sep 10, 10 AM", "Sep 10, 3 PM")
  ),
  coords = c(x = "lon", y = "lat"),
  crs = 4326
)

# Create a linestring connecting the points
route_line <- city_points |>
  st_combine() |>
  st_cast("LINESTRING")

# Plotting the map, route line, and city points with labels
ggplot() +
  geom_sf(data = world_map, fill = "#efefef", colour = "gray", linewidth = 0.1) +
  geom_sf(data = route_line, colour = "#a4caff", linetype = "dashed") +
  geom_sf(data = city_points, size = 1, colour = "#3670aa") +
  geom_sf_text(
    data = city_points,
    aes(label = paste(city, " - ", date)),
    nudge_y = 450000,
    size = 2
  ) +
  ggtitle("From Nagoya to Madrid") +
  coord_sf(
    crs = "+proj=laea +lat_0=34.86 +lon_0=136.81 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs",
    xlim = c(-3000000, 8000000),
    ylim = c(2500000, 12500000)) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title = element_text(size = 9.5),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# Save the plot as an image file
ggsave("plot/05-a-journey.png", bg = "white", width = 1000, height = 1000,
       dpi = 300, units = "px")
