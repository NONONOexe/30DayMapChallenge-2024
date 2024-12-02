# Load required libraries
library(osmdata)
library(dplyr)
library(purrr)
library(ggplot2)
library(sf)
library(geomtextpath)

# Fetch street data around a specific location
street_data <- opq_around(
    lon    = -0.185722,
    lat    = 51.538594,
    radius = 1000,
    key    = "highway"
  ) |> osmdata_sf()

# Generate repeated labels according to the length of the street
named_streets <- street_data$osm_lines |>
  filter(!is.na(name)) |>
  transmute(
    street_name = toupper(name),
    name_length = nchar(street_name),
    road_length = st_length(geometry),
    char_count  = as.numeric(road_length) %/% 25 + 1,
    label       = map2_chr(
      street_name, char_count,
      \(name, count) substr(paste(rep(name, 10), collapse = "  "), 1, count)
    )
  )

# Create a ggplot object with street labels
street_plot <- ggplot() +
  geom_textsf(
    data        = named_streets,
    mapping     = aes(label = label),
    family      = "sans",
    fontface    = "bold",
    linetype    = "blank",
    colour      = "#3a2416",
    size        = 1
  ) +
  theme_void() +
  theme(plot.margin = margin(-10, -10, -10, -10))

# Save the plot as an image file
ggsave(
  plot     = street_plot,
  filename = "plot/19-typography.png",
  bg       = "#c3b8a6",
  width    = 1000,
  height   = 1000,
  dpi      = 300,
  units    = "px"
)
