# Load necessary library
library(simplecensus)
library(viridisLite)

# Define the prefecture code for Aichi
aichi_code <- 23

# Load census and shapefile data for Aichi
aichi_census <- smc.read_census_2015(aichi_code)
aichi_shapefile <- smc.read_census_shp(aichi_code)

# Merge shapefile and census data, compute centroids, and clean the dataset
aichi_census_all <- aichi_shapefile |>
  left_join(aichi_census, by = join_by(KEY_CODE)) |>
  transmute(population = as.integer(T000848001)) |>
  st_set_agr("constant") |>
  st_centroid() |>
  drop_na()

# Add bubble geometries based on population and sort by population
aichi_census_all <- aichi_census_all |>
  mutate(bubble = st_buffer(geometry, population * 0.1)) |>
  arrange(desc(population))

# Plot the bubbles with a colour gradient based on population
plot(
  aichi_census_all$bubble,
  col = viridis(
    n = max(aichi_census_all$population)
  )[aichi_census_all$population],
  border = NA
)
