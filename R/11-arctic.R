# Load required libraries
library(rgbif)
library(sf)
library(stringr)
library(dplyr)
library(ggplot2)
library(rnaturalearth)

# Fetch species data for polar bears from GBIF
species_data <- name_backbone("Ursus maritimus")
taxon_key <- species_data$usageKey

# Download occurrence data for polar bears
occ_info <- occ_download(
  pred("taxonKey", taxon_key),
  pred("hasCoordinate", TRUE)
)

# Retrieve metadata and citation information
meta <- occ_download_meta(occ_info)
citation <- gbif_citation(meta)$download |>
  str_wrap(width = 80)

# Download and import occurrence data as a spatial data frame
occ_download_wait(occ_info)
occ_data <- occ_download_get(occ_info, path = tempdir())
polar_bear_data <- occ_download_import(occ_data) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
  st_transform(crs = "+proj=laea +lat_0=90 +ellps=WGS84 +no_defs") |>
  mutate(
    x = st_coordinates(geometry)[, 1],
    y = st_coordinates(geometry)[, 2],
  )

# Load world map data as an sf object
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Plot the polar bear locations on a map
ggplot() +
  geom_sf(data = world_map, fill = "#ffedd1", colour = "transparent") +
  geom_sf(
    data   = df,
    alpha  = 0.5,
    shape  = 20,
    colour = "white",
    size   = 2,
    stroke = FALSE
  ) +
  ggtitle("Observed Locations of Polar Bears") +
  coord_sf(
    crs  = "+proj=laea +lat_0=90 +lon_0=0",
    xlim = c(-3500000, 3500000),
    ylim = c(-3000000, 3000000)
  ) +
  labs(caption = citation) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title      = element_text(size = 7),
    plot.background = element_rect(fill = "#9fc6e3", colour = "transparent"),
    plot.caption    = element_text(size = 5),
    panel.grid      = element_line(colour = "#171717", linewidth = 0.2),
    axis.text       = element_blank(),
    axis.title      = element_blank()
  )


# Save the plot as an image file
ggsave(
  plot     = trip_plot,
  filename = "plot/11-arctic.png",
  bg       = "white",
  width    = 1000,
  height   = 1000,
  dpi      = 300,
  units    = "px"
)
