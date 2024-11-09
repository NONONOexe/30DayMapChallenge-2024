# Load required libraries
library(eurostat)
library(ggplot2)
library(sf)
library(dplyr)
library(showtext)
library(sysfonts)
library(units)

# Set font from Google Fonts
font_add_google("Charm", "Charm")
showtext_auto()

# Load Eurostat geospatial data at NUTS 0 level for 2016
euro_map <- get_eurostat_geospatial(
  resolution = 10,
  nuts_level = 0,
  year = 2016
)

# Process data generate labels for countries
# Attach only large areas per country
euro_labels <- euro_map |>
  st_set_agr("constant") |>
  st_cast("POLYGON") |>
  select(name = NAME_LATN) |>
  filter(
    set_units(60 * 10^9, m^2) < st_area(geometry) &
      lengths(strsplit(name, "/")) == 1
  ) |>
  st_centroid()

# Create a bounding polygon for the map outline
euro_bounding_polygon <- euro_map |>
  st_union() |>
  st_cast("POLYGON") |>
  st_as_sf() |>
  filter(set_units(10^9, m^2) < st_area(x)) |>
  st_union()

# Define color palette for the plot
background_colour <- "#d5c2a6"
light_fill_colour <- "#edd8b9"
outline_colour <- "#4c3c23"
border_colour <- "#bca485"

# Create a vintage-style map plot
vintage_plot <- ggplot() +
  # Plot the bounding polygon with outline color
  # Outline the coastal edges of Europe
  geom_sf(
    data = euro_bounding_polygon,
    colour = border_colour,
    linewidth = 3,
    fill = "transparent"
  ) +
  # Plot the main map data
  geom_sf(
    data = euro_map,
    fill = light_fill_colour,
  ) +
  # Add country labels
  geom_sf_text(
    data = euro_labels,
    aes(label = name),
    family = "Charm",
    na.rm = TRUE
  ) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_minimal(base_size = 15, base_family = "Charm") +
  theme(
    plot.background  = element_rect(fill = background_colour, colour = background_colour),
    plot.margin      = margin(t = 8, r = 3, b = 10, l = 8),
    panel.background = element_rect(fill = background_colour, colour= outline_colour),
    panel.grid       = element_line(colour = alpha(outline_colour, 0.2), linewidth = 0.5),
    axis.title       = element_blank(),
    axis.text.x      = element_text(margin = margin(t = -10, b = 0)),
    axis.text.y      = element_text(margin = margin(r = -14, l = 0))
  )

# Save the plot as an image file
ggsave(
  plot     = vintage_plot,
  filename = "plot/07-vintage-style.png",
  bg       = background_colour,
  width    = 1000,
  height   = 1000,
  dpi      = 300,
  units    = "px"
)
