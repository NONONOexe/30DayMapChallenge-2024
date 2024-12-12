# Load required libraries
library(tmap)
library(sf)
library(ggplot2)
library(patchwork)

# Load land elevation data
data(World)

# Define base map theme
base_map <- function() {
  ggplot() +
    geom_sf(
      data      = World,
      colour    = "#2177ac",
      fill      = "#f6f4e0",
      linewidth = 0.1,
    ) +
    scale_x_continuous(breaks = seq(-180, 180, by = 10)) +
    scale_y_continuous(breaks = seq(-90, 90, by = 10)) +
    theme(
      plot.title          = element_text(size = 8),
      plot.title.position = "plot",
      panel.background    = element_rect(fill = "#caeaf2"),
      panel.grid          = element_line(
        colour    = "#2177ac",
        linewidth = 0.05
      ),
      axis.text           = element_blank(),
      axis.ticks          = element_blank()
    )
}

# Function to create map with specified projection
create_map <- function(projection_string, title, xlim = NULL, ylim = NULL) {
  map <- base_map() +
    coord_sf(crs = projection_string, xlim = xlim, ylim = ylim) +
    ggtitle(title)

  return(map)
}

# Create map projections
wgs84_map <- base_map() + ggtitle("WGS84")

orthographic_map <- create_map(
  "+proj=ortho +lat_0=0 +lon_0=0 +datum=WGS84",
  "Orthographic projection (14 BC)",
  xlim = c(-6200000, 6200000),
  ylim = c(-6200000, 6200000)
)

mercator_map <- create_map(
  "+proj=merc +lat_ts=56.5 +ellps=WGS84",
  "Mercator projection (1569)"
)

lambert_map <- create_map(
  "+proj=laea",
  "Lambert azimuthal\nequal-area projection (1772)",
  xlim = c(-12000000, 12000000),
  ylim = c(-12000000, 12000000)
)

mollweide_map <- create_map(
  "+proj=moll",
  "Mollweide projection (1805)"
)

gall_peters_map <- create_map(
  "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  "Gall-Peters projection (1855)"
)

van_der_grinten_map <- create_map(
  "+proj=vandg4",
  "Van der Grinten IV projection (1904)"
)

eckert4_map <- create_map(
  "+proj=eck4",
  "Eckert IV projection (1906)"
)

goode_homolosine_map <- create_map(
  "+proj=igh",
  "Goode's homolosine projection (1923)"
)

miller_cylindrical_map <- create_map(
  "+proj=mill",
  "Miller cylindrical projection (1942)"
)

robinson_map <- create_map(
  "+proj=robin",
  "Robinson projection (1963)"
)

equal_earth_map <- create_map(
  "+proj=eqearth",
  "Equal Earth projection (2018)"
)

# Combine all maps
map_projections <- wrap_plots(
  list(
    wgs84_map,
    orthographic_map,
    mercator_map,
    lambert_map,
    mollweide_map,
    gall_peters_map,
    van_der_grinten_map,
    eckert4_map,
    goode_homolosine_map,
    miller_cylindrical_map,
    robinson_map,
    equal_earth_map
  ),
  ncol   = 4,
  widths = c(1, 1, 1, 1)
)

# Save the combined map
ggsave(
  plot     = map_projections,
  filename = "plot/26-map-projections.png",
  bg       = "white",
  width    = 2400,
  height   = 1600,
  dpi      = 300,
  units    = "px"
)
