# Load required libraries
library(sf)
library(tidycensus)
library(dplyr)
library(units)
library(ggplot2)

# Download the hexagonal grid map of US states:
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

# Read the hexagonal grid map of US states
states <- st_read("data-raw/us_states_hexgrid.geojson")

# Fetch population data
population <- get_acs(
  geography = "state",
  variables = "B01001_001",
  year      = 2022,
  geometry  = TRUE
)

# Calculate area in square kilometers for each state
population_with_area <- population |>
  mutate(area = set_units(st_area(geometry), km^2)) |>
  st_drop_geometry()

# Join population and area data with hexagonal state map
# Calculate population density and categorize it into bins
states_with_density <- states |>
  mutate(name = sub(" \\(United States\\)", "", states$google_name)) |>
  left_join(population_with_area, by = join_by("name" == "NAME")) |>
  mutate(
    density = as.numeric(estimate / area),
    density_bin = cut(
      density,
      breaks = c(0, 15, 30, 60, 120, Inf),
      labels = c("0-15", "15-30", "30-60", "60-120", "120+")
    )
  )

# Create the plot showing population density by state
ggplot(states_with_density) +
  geom_sf(aes(fill = density_bin), colour = "white", linewidth = 1) +
  geom_sf_text(aes(label = iso3166_2), colour = "white", size = 2.2) +
  coord_sf(crs = 3857) +
  scale_fill_viridis_d(
    option = "inferno", begin = 0.2, end = 0.6,
    guide = guide_legend(keywidth = 1.5, keyheight = 1.0)) +
  ggtitle("Population Density by State") +
  theme_void() +
  theme(
    plot.margin = margin(r = 20, l = 20),
    plot.title = element_text(size = 10, face = "bold", margin = margin(b = 5)),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 7.5)
  )

# Save the plot as an image file
ggsave("plot/04-hexagons.png", bg = "white", width = 1200, height = 800,
       dpi = 300, units = "px")
