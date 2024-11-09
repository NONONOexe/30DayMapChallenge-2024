# Load required libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(showtext)
library(sysfonts)

# Set font from Google Fonts
font_add_google("Tinos", "Tinos")
showtext_auto()

# Load world map data with medium resolution
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Load earthquake data for Turkey
turkey_eq_data <- st_read("data-raw/turkey-earthquake/Earthquake.shp")

# Define the bounding box for Turkey
turkey_bounds <- st_bbox(turkey_earthquake)

# Plot earthquake data on world map
eq_plot <- ggplot() +
  # Plot world map
  geom_sf(
    data   = world_map,
    fill   = "white",
    colour = "black"
  ) +
  # Overlay Turkey earthquake data
  geom_sf(
    data   = turkey_eq_data,
    alpha  = 0.3,
    stroke = FALSE,
    colour = "#DD0000",
    aes(size = mag)
  ) +
  ggtitle("Turkey Earthquake") +
  scale_size_continuous(range = c(1, 5)) +
  labs(size = "Magnitude", colour = "Magnitude") +
  theme_void(base_family = "Tinos", base_size = 20) +
  coord_sf(
    xlim = turkey_bounds[c("xmin", "xmax")] + c(-5, 5),
    ylim = turkey_bounds[c("ymin", "ymax")] + c(-1, 1)
  ) +
  guides(colour = guide_legend()) +
  theme(
    plot.title       = element_text(margin = margin(b = 3)),
    plot.margin      = margin(5, 5, 5, 5),
    panel.background = element_rect(fill = "#a9d2dc",colour = "transparent"),
    legend.key       = element_blank()
  )

# Save the plot as an image file
ggsave(
  plot     = eq_plot,
  filename = "plot/08-humanitarian-data-exchange.png",
  bg       = "white",
  width    = 1000,
  height   = 480,
  dpi      = 300,
  units    = "px"
)
