# Load required libraries
library(eurostat)
library(dplyr)
library(lubridate)
library(sf)
library(forcats)
library(showtext)
library(sysfonts)
library(ggplot2)
library(viridisLite)

# Fetch data on ISCED 8 (Doctorate or equivalent level)
educ_data <- get_eurostat(id = "educ_uoe_enrt06")

# Process the data for the year 2022
isced_data_2022 <- educ_data |>
  filter(
    year(TIME_PERIOD) == 2022,
    isced11 == "ED8"
  ) |>
  group_by(geo) |>
  summarise(total_values = sum(values)) |>
  mutate(
    value_category = cut(
        total_values,
        breaks = c(0, 30000, 50000, 100000, 200000, Inf),
        labels = c(
          "0 to 30000", "30000 to 50000", "50000 to 100000",
          "100000 to 200000", "200000+"
        ),
        include.lowest = TRUE
      ) |>
      fct_na_value_to_level("No data")
  )

# Retrieve geographic  data at NUTS level 2
geo_data <- get_eurostat_geospatial(nuts_level = 2)

# Merge geographic data with ISCED 8 data
isced_sf_data <- geo_data |>
  left_join(isced_data_2022, by = c("CNTR_CODE" = "geo"))

# Define colour palette and labels
colour_palette <- viridis(n = 5, begin = 0.2, end = 0.8)

# Set font from Google Fonts
font_add_google("Roboto", "Roboto")
showtext_auto()

# Create the choropleth map
chorepleth_plot <- ggplot(isced_sf_data) +
  geom_sf(aes(fill = value_category, colour = value_category)) +
  scale_fill_manual(
    name     = "Number of People in Europe\nCorresponding to ISCED 8 in 2022",
    values   = colour_palette,
    na.value = "lightgray",
    labels   = levels(isced_data_2022$value_category)
  ) +
  scale_colour_manual(
    name     = "Number of People in Europe\nCorresponding to ISCED 8 in 2022",
    values   = colour_palette,
    na.value = "lightgray",
    labels   = levels(isced_data_2022$value_category)
  ) +
  coord_sf(xlim = c(-25, 45), ylim = c(30, 70), expand = FALSE) +
  theme_void() +
  theme(
    legend.position        = "inside",
    legend.position.inside = c(0.0, 0.5),
    legend.key.size        = unit(2.5, "mm"),
    legend.key.spacing.y   = unit(0.5, "mm"),
    legend.title           = element_text(
      family     = "Roboto",
      lineheight = 0.4,
      size       = 15
    ),
    legend.text            = element_text(family = "Roboto", size = 12),
    plot.margin            = margin(r = -45, t = 5, b = 5)
  )

# Save the plot to a file
ggsave(
  plot     = chorepleth_plot,
  filename = "plot/16-choropleth.png",
  bg       = "white",
  width    = 1200,
  height   = 800,
  dpi      = 300,
  units    = "px"
)
