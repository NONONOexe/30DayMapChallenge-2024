# Load required libraries
library(arrow)
library(sf)
library(lubridate)
library(dplyr)
library(tidyr)
library(showtext)
library(sysfonts)
library(ggplot2)

# Read the data
taxi_zones <- read_sf("data-raw/taxi_zones/taxi_zones.shp")
trip_data <- bind_rows(
  read_parquet("data-raw/yellow_tripdata_2024-06.parquet"),
  read_parquet("data-raw/yellow_tripdata_2024-07.parquet"),
  read_parquet("data-raw/yellow_tripdata_2024-08.parquet")
)

# Filter data for weekends and target hours
weekend_trip_data <- trip_data |>
  filter(month(tpep_dropoff_datetime) %in% 6:8,
         wday(tpep_dropoff_datetime) %in% c(1, 7),
         hour(tpep_dropoff_datetime) %in% c(7:16, 18:22))

trip_summary <- weekend_trip_data |>
  transmute(
    month = factor(recode(
      month(tpep_dropoff_datetime),
      `6` = "Jun", `7` = "Jul", `8` = "Aug"
    ), levels = c("Jun", "Jul", "Aug")),
    time_period = factor(case_when(
      hour(tpep_dropoff_datetime) %in% 7:10 ~ "Morning\n6:00 a.m.\n - 8:00 a.m.",
      hour(tpep_dropoff_datetime) %in% 11:16 ~ "Daytime\n11:00 a.m.\n - 4:00 p.m.",
      hour(tpep_dropoff_datetime) %in% 18:22 ~ "Evening\n 6:00 p.m.\n - 10:00 p.m."
    ), levels = c(
      "Morning\n6:00 a.m.\n - 8:00 a.m.",
      "Daytime\n11:00 a.m.\n - 4:00 p.m.",
      "Evening\n 6:00 p.m.\n - 10:00 p.m."
    )),
    location_id = DOLocationID
  ) |>
  count(month, time_period, location_id, name = "trip_count") |>
  complete(month, time_period, location_id, fill = list(trip_count = 0)) |>
  mutate(trip_count_bin = cut(
    trip_count,
    breaks = c(-1, 20, 40, 100, 500, Inf),
    labels = c("0 - 20", "20 - 40", "40 - 100", "100 - 500", "500+")
  ))

# Merge with spatial data and convert to sf object
trip_summary_sf <- trip_summary |>
  group_nest(location_id) |>
  left_join(taxi_zones, by = join_by("location_id" == "LocationID")) |>
  unnest(cols = "data") |>
  st_sf()


# Set font from Google Fonts
font_add_google("Inter", "Inter", regular.wt = 600)
showtext_auto()

# Plot taxis drop-off locations
trip_plot <- ggplot() +
  geom_sf(
    data   = trip_summary_sf,
    colour = "transparent",
    aes(fill = trip_count_bin)
  ) +
  facet_grid(time_period ~ month, switch = "y") +
  scale_fill_viridis_d(option = "mako") +
  ggtitle("Drop-off Locations of Taxis in New York on Weekends") +
  theme_void(base_size = 14, base_family = "Inter") +
  theme(
    plot.title.position  = "plot",
    legend.position      = "top",
    legend.location      = "plot",
    legend.key.height    = unit(0.1, "cm"),
    legend.key.width     = unit(0.6, "cm"),
    legend.title         = element_blank(),
    legend.justification = c(0, 1),
    legend.margin        = margin(t = 2),
    strip.text.x         = element_text(margin = margin(b = 5)),
    strip.text.y         = element_text(hjust = 1, lineheight = 0.4),
    plot.margin          = margin(b = 5)
  )

# Save the plot as an image file
ggsave(
  plot     = trip_plot,
  filename = "plot/12-time-and-space.png",
  bg       = "white",
  width    = 1100,
  height   = 1000,
  dpi      = 300,
  units    = "px"
)
