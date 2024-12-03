# Load required libraries
library(sf)
library(jpndistrict)
library(osmdata)
library(tidyr)
library(dplyr)
library(ggplot2)
library(showtext)
library(sysfonts)

# Retrieve city boundaries for all prefectures in Japan
japan_cities <- do.call(rbind, lapply(1:47, jpn_pref))
bounding_box <- st_bbox(japan_cities)

# Function to fetch locations from OpenStreetMap based on a name
fetch_osm_locations <- function(location_name) {
  osmdata <- opq(bounding_box) |>
    add_osm_feature(key = "name:en", value = location_name) |>
    osmdata_sf() |>
    unique_osmdata()

  unique_geometries <- c(
    osmdata$osm_points$geometry,
    st_centroid(osmdata$osm_polygons$geometry)
  )

  st_as_sf(name = location_name, unique_geometries)
}

# Function to count points grouped by city
count_points_by_city <- function(points) {
  points |>
    st_join(japan_cities) |>
    st_drop_geometry() |>
    drop_na() |>
    group_by(city_code, name) |>
    summarise(store_count = n(), .groups = "drop")
}

# Fetch locations of Domino's Pizza and Pizza-La
sf_use_s2(TRUE)
domino_pizza <- fetch_osm_locations("Domino's Pizza")
pizza_la <- fetch_osm_locations("Pizza-La")
sf_use_s2(FALSE)

# Count the number of each store by city
domino_pizza_count <- count_points_by_city(domino_pizza)
pizza_la_count <- count_points_by_city(pizza_la)

# Combine store counts with city data and compute store status
store_counts <- japan_cities |>
  left_join(domino_pizza_count, by = join_by("city_code")) |>
  left_join(
    pizza_la_num,
    by     = join_by("city_code"),
    suffix = c("_domino", "_pizza_la")
  ) |>
  replace_na(list(store_count_domino = 0L, store_count_pizza_la = 0L)) |>
  mutate(
    store_status = case_when(
        0 < store_count_domino & 0 < store_count_pizza_la
                                 ~ "Both",
        0 < store_count_domino   ~ "Domino's Pizza Only",
        0 < store_count_pizza_la ~ "Pizza-La Only",
        TRUE                     ~ "None"
      ) |>
      factor(
        levels = c("Both", "Domino's Pizza Only", "Pizza-La Only", "None")
      )
  )

# Set Google Fonts for the plot
font_add_google("Roboto", "Roboto", regular.wt = 900)
showtext_auto()

# Define colour palette
colour_palette <- c(
  "Both"                = "#673b61",
  "Domino's Pizza Only" = "#0063a7",
  "Pizza-La Only"       = "#ce131c",
  "None"                = "lightgray"
)

# Create map plot
map_plot <- ggplot(store_counts) +
  geom_text(
    label = "Locations of\nDomino's Pizza / Pizza-La\non OpenStreetMap",
    x = 128, y = 40.5, size = 11, colour = "black", hjust = 0,
    family = "Roboto", lineheight = 0.3
  ) +
  geom_sf(aes(fill = store_status, colour = store_status)) +
  scale_fill_manual(values = colour_palette) +
  scale_colour_manual(values = colour_palette) +
  coord_sf(xlim = c(125, 150), ylim = c(31, 45)) +
  theme_void() +
  theme(
    legend.title           = element_blank(),
    legend.text            = element_text(size = 15, face = "bold"),
    legend.key.size        = unit(0.3, "cm"),
    legend.position        = "inside",
    legend.position.inside = c(0.78, 0.13),
    plot.margin            = margin(t = -10, r = -8, b = -20, l = -25)
  )

# Save the plot as an image file
ggsave(
  plot     = map_plot,
  filename = "plot/21-conflict.png",
  bg       = "white",
  width    = 1000,
  height   = 1000,
  dpi      = 300,
  units    = "px"
)
