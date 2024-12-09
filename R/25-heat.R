# Load required libraries
library(jmastats)
library(tidyverse)
library(sf)
library(spatstat)
library(gstat)
library(jpndistrict)
library(scales)

# Extract unique block numbers for specific station types
selected_blocks <- stations |>
  filter(station_type %in% c("四", "三", "官")) |>
  pull(block_no) |>
  unique()

# Define a function to fetch October temperature data for a block
fetch_temperature_data <- function(block_number) {
  jma_collect(
    item     = "monthly",
    block_no = block_number,
    year     = 2024,
    cache    = FALSE,
    pack     = FALSE
  ) |>
    filter(month == 10) |>
    select(starts_with("temperature"))
}

# Collect temprature data for all selected blocks
# Note: This process fetches data for each block.
#       It takes a considerable amount of time.
temperature_data_list <- lapply(selected_blocks, fetch_temperature_data)

# Combine and preprocess temperature data
temperature_sf <- do.call(rbind, temperature_data_list) |>
  mutate(block_no = selected_blocks) |>
  inner_join(stations, by = join_by("block_no")) |>
  select(
    temperature = "temperature_average_max(℃)",
    geometry
  ) |>
  distinct(geometry, .keep_all = TRUE) |>
  drop_na() |>
  st_as_sf()

# Create a unified geometry for Japan
sf_use_s2(FALSE)
japan_geometries <- lapply(1:47, jpn_pref, district = FALSE) |>
  do.call(what = rbind)
sf_use_s2(TRUE)
japan_boundary <- st_union(japan_geometries$geometry)

# Create a grid for spartial interpolation within Japan's boundary
grid <- temperature_sf |>
  st_make_grid(cellsize = 0.2) |>
  st_as_sf() |>
  st_filter(japan_boundary)

# Perform inverse distance weighting interpolation
idw_result <- idw(
  formula   = temperature ~ 1,
  locations = temperature_sf,
  newdata   = grid,
  idp       = 2
)

# Plot interpolated temperature data with Japan's boundary
temperature_plot <- ggplot(idw_result) +
  geom_sf(aes(fill = var1.pred), colour = NA) +
  geom_sf(data = japan_boundary, colour = "black", fill = NA) +
  scale_fill_gradientn(
    name    = "Max Temperature\nin October",
    colours = c("#f0f0f0", "#f26d5d", "#c5161e"),
    labels  = label_number(suffix = "°C")
  ) +
  theme_bw() +
  theme(
    legend.position        = "inside",
    legend.justification   = c(1, 0),
    legend.position.inside = c(0.98, 0.02),
    legend.background      = element_rect(
      colour    = "black",
      fill      = "white",
      linewidth = 0.2
    ),
    legend.title    = element_text(size = 8),
    legend.text     = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),
    panel.grid      = element_line(
      colour    = "black",
      linetype  = "dashed",
      linewidth = 0.2
    ),
    plot.margin = margin(r = 10, l = 10)
  )

# Save the plot as an image file
ggsave(
  plot     = temperature_plot,
  filename = "plot/25-heat.png",
  bg       = "white",
  width    = 1200,
  height   = 1100,
  dpi      = 300,
  units    = "px"
)
