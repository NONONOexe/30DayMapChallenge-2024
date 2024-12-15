# Load required libraries
library(pavement)
library(jpaccidents)
library(sf)
library(mapgl)
library(dplyr)
library(osmdata)

# Define the central coordinates and dimensions of the bounding box
center_longitude <- 139.728187
center_latitude <- 35.679636
bbox_width <- bbox_height <- 0.02

# Create a bounding box for data extraction
bounding_box <- create_bbox(
  center_lon = center_longitude,
  center_lat = center_latitude,
  width      = bbox_width,
  height     = bbox_height
)

# Fetch road network data within the bounding box and transform to JDG2011
roads <- bounding_box |>
  fetch_roads() |>
  st_transform(crs = 6677)

# Download and read accident data
accident_file_path <- download_accident_data("main")
accident_data <- read_accident_data(accident_file_path)

# Filter accident data within the bounding box area
filtered_accidents <- accident_data$accident |>
  st_filter(
    convert_bbox_to_polygon(bounding_box, st_crs(accident_data$accident))
  ) |>
  st_transform(crs = 6677)

# Create and segment the road network, incorporating accident data
road_network <- roads |>
  create_road_network() |>
  set_events(filtered_accidents)
segmented_network <- road_network |>
  create_segmented_network(segment_length = 10)

# Convolute the segmented network for heatmap analysis
convoluted_network <- segmented_network |>
  convolute_segmented_network(bandwidth = 100)

# Function to generate heatmap colours based on segment density
generate_heatmap_colours <- function(segment_densities) {
  # Generate heatmap colours
  heatmap_colours <- paste0(heat.colors(100, rev = TRUE), "CD")

  # Normalize density values
  normalized_values <- segment_densities / max(segment_densities, na.rm = TRUE)

  # Assign colours based on normalized values
  colours <- heatmap_colours[as.numeric(cut(normalized_values, breaks = 100))]

  return(colours)
}

# Prepare segments for map visualisation
segments <- convoluted_network$segments |>
  st_transform(4326) |>
  mutate(
    colour = generate_heatmap_colours(convoluted_network$segments$density)
  )

# Calculate bounding box and centre for the heatmap visualisation
bbox <- st_bbox(segments)
centre_longitude <- mean(bbox[c("xmin", "xmax")])
centre_latitude <- mean(bbox[c("ymin", "ymax")])

# Fetch building data withion a radius of 1000m around centre point
osm_buildings <-
  opq_around(
    lon    = centre_longitude,
    lat    = centre_latitude,
    radius = 1000,
    key    = "building"
  ) |>
  osmdata_sf()

# Prepare building data with rendering heights
tokyo_buildings <- osm_buildings$osm_polygons |>
  mutate(
    render_height = if_else(is.na(height), 10.0, as.numeric(height))
  )

# Render the map with layers for roads and 3D buildings
map <-
  maplibre(
    style  = carto_style("positron-no-labels"),
    center = c(centre_longitude, centre_latitude),
    zoom   = 14
  ) |>
  # Add road segments with density-based colours
  add_source(
    id   = "segments",
    data = segments
  ) |>
  add_line_layer(
    id         = "density-layer",
    source     = "segments",
    line_width = 3,
    line_color = get_column("colour")
  ) |>
  # Add 3D buildings with extrusion based on height
  add_source(
    id   = "tokyo-buildings",
    data = tokyo_buildings
  ) |>
  add_layer(
    id     = "3d-buildings",
    type   = "fill-extrusion",
    source = "tokyo-buildings",
    paint  = list(
      "fill-extrusion-color"   = "white",
      "fill-extrusion-height"  = get_column("render_height"),
      "fill-extrusion-base"    = 0,
      "fill-extrusion-opacity" = 0.5
    )
  )

map
