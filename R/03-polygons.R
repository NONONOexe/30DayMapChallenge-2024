# Load required libraries
library(jpndistrict)
library(sf)
library(ggplot2)
library(scales)

# Obtain geometric data for all 47 prefectures in Japan
sf_use_s2(FALSE)
prefectures <- lapply(1:47, jpn_pref, district = FALSE)
prefectures <- do.call(rbind, prefectures)

# Difine the forest cover rates for each prefecture
# MAFF 2022 data: https://www.rinya.maff.go.jp/j/keikaku/genkyou/r4/1.html
forest_cover_rate <- c(
  0.71, 0.66, 0.77, 0.57, 0.72, 0.72,
  0.71, 0.31, 0.54, 0.67, 0.31, 0.29,
  0.36, 0.39, 0.68, 0.67, 0.68, 0.74,
  0.78, 0.79, 0.81, 0.64, 0.42, 0.64,
  0.51, 0.74, 0.30, 0.67, 0.77, 0.77,
  0.74, 0.78, 0.68, 0.72, 0.71, 0.76,
  0.47, 0.71, 0.84, 0.45, 0.45, 0.59,
  0.62, 0.71, 0.76, 0.65, 0.45
)

# Create the map using ggplot2
ggplot(prefectures) +
  geom_sf(aes(fill = forest_cover_rate), colour = "#daf1de", linewidth = 0.3) +
  scale_fill_gradient(
    low = "#e2ddaa",
    high = "#0c5a3e",
    limits = c(0, 1),
    labels = label_percent()
  ) +
  coord_sf(xlim = c(125, 150), ylim = c(30, 46)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#daf1de", colour = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, colour = "#0c5a3e"),
    legend.ticks = element_line(linewidth = 0.3, colour = "#daf1de"),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "inside",
    legend.position.inside = c(0.80, 0.20)
  )

# Save the plot as an image file
ggsave("plot/03-polygons.png", bg = "#daf1de", width = 1200, height = 1200,
       dpi = 300, units = "px")
