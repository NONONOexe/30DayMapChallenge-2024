# Load required libraries
library(jpndistrict)
library(sf)
library(ggplot2)
library(showtext)
library(sysfonts)

# Turn off s2 processing
sf_use_s2(FALSE)

# Fetch geometry data for all 47 prefectures in japan
japan_prefectures <- lapply(1:47, jpn_pref, district = FALSE) |>
  do.call(what = rbind)

# Add visit count as a factor with specific levels
japan_prefectures$count <- c(
  "3 Times", "Never", "1 Time", "Never", "Never", "Never", "1 Time",
  "Never", "Never", "1 Time", "Never", "4+ Times", "4+ Times", "2 Times",
  "Never", "Never", "1 Time", "Never", "1 Time", "4+ Times", "4+ Times",
  "4+ Times", "Living", "4+ Times", "Never", "4+ Times", "4+ Times",
  "4+ Times", "1 Time", "Never", "Never", "Never", "Never", "2 Times",
  "Never", "Never", "Never", "Never", "Never", "1 Time", "Never", "Never",
  "1 Time", "Never", "Never", "Never", "Never"
) |> factor(levels = c(
  "Never", "1 Time", "2 Times", "3 Times", "4+ Times", "Living"
))

# Simplify geometry and apply transformations
prefectures_simplified <- japan_prefectures |>
  st_transform(crs = 3875) |>
  st_simplify(dTolerance = 30000) |>
  st_buffer(10000)

# Set Google Fonts for the plot
font_add_google("Hurricane", "Hurricane")
showtext_auto()

# Create the map visualization
map_plot <- ggplot() +
  geom_sf(
    data      = prefectures_simplified,
    mapping   = aes(fill = count),
    colour    = "white",
    linewidth = 0.3,
  ) +
  coord_sf(
    xlim = c(125, 155),
    ylim = c(31, 45),
    crs  = 4326
  ) +
  ggtitle("Prefectures Visited and the Number of Visits") +
  scale_fill_manual(
    values = c(
      "#dde1e4", "#b9e3f9", "#87c9a4", "#facd88", "#f7c7c7", "#ef908a"
    ),
    guide = guide_legend(keywidth = 0.5, keyheight = 0.5)
  ) +
  theme_void(base_size = 30, base_family = "Hurricane") +
  theme(
    plot.title.position = "panel",
    plot.title = element_text(
      size = 45, hjust = 0.5, face = "bold", vjust = 3, colour = "#273373"
    ),
    legend.text = element_text(
      size = 25, hjust = 0, margin = margin(l = 3), colour = "#273373"
    ),
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.73, 0.28),
    legend.key.spacing.y = unit(0.1, "cm"),
  )

# Save the plot as an image file
ggsave(
  plot     = map_plot,
  filename = "plot/15-my-data.png",
  bg       = "white",
  width    = 1000,
  height   = 800,
  dpi      = 300,
  units    = "px"
)

