# Load required libraries
library(marmap)
library(stars)
library(dplyr)
library(tmap)
library(magick)

# Define constants
TOTAL_FRAMES    <- 108
OUTPUT_DIR      <- tempdir()
FILE_PREFIX     <- "28-the-blue-planet-"
EARTH_AXIS_TILT <- 23.4

# Fetch bathymetric data
bathymetry_data <- getNOAA.bathy(
  lon1 = -180, lon2 = 180,
  lat1 = -90, lat2 = 90,
  resolution = 20
)

# Convert bathymetric data to a stars object
bathymetry_stars <- bathymetry_data |>
  as.raster() |>
  st_as_stars() |>
  mutate(depth = if_else(0 < layer, 0, layer))

# Define bounding box for cropping
bounding_box <- st_bbox(
  c(xmin = -90, xmax = 90, ymin = -90, ymax = 90),
  crs = st_crs(bathymetry_stars)
)

# Function to create and save a single frame of the animation
create_frame <- function(frame_number) {
  # Shift the bathymetry data for animation effect
  bathymetry_stars$depth <<- rbind(
    bathymetry_stars$depth[-(1:10),],
    bathymetry_stars$depth[1:10,]
  )

  # Define orthographic projection
  ortho_crs <- "+proj=ortho +lat_0=0 +lon_0=0 +datum=WGS84"

  # Crop and transform to orthographic projection
  croped_raster <- bathymetry_stars |>
    st_crop(bounding_box) |>
    st_transform(crs = ortho_crs)

  # Create the map
  world_map <- tm_shape(croped_raster) +
    tm_raster(
      "depth",
      col.scale = tm_scale_continuous(
        ticks   = c(-10000, -6000, -4000,
                    -2000, -500, -0.0001, 0),
        values  = c("#1a1f53", "#062243", "#0247bc",
                    "#1a5799", "#2c74b5", "#ffffff")
      )
    ) +
    tm_layout(
      legend.show   = FALSE,
      bg.color      = "#1a1f53",
      frame         = FALSE,
      inner.margins = c(1, 0, 1, 0)
    )

  # Construct file name
  file_name <- paste0(
    OUTPUT_DIR, "/",
    FILE_PREFIX, formatC(frame_number, width = 3, flag = "0"), ".png"
  )

  # Save the map as an image file
  tmap_save(
    world_map, file_name,
    width = 1200, height = 1200, dpi = 300,
    bg = "#1a1f53"
  )

  return(file_name)
}

# Create animation frames
file_names <- lapply(1:TOTAL_FRAMES, create_frame)

# Process images for animation
animation_frames <- list()
for (file_name in file_names) {
  image <- image_read(file_name)
  rotated_image <- image_rotate(image, EARTH_AXIS_TILT)
  cropped_image <- image_crop(rotated_image, "600x600+300+300")
  annotated_image <- image_annotate(
    cropped_image, "Global Bathymetry",
    size     = 30,
    color    = "white",
    location = "+20+545"
  )
  animation_frames <- append(animation_frames, annotated_image)
}

# Create and save the animation
animation <- image_animate(animation_frames, fps = 10)
image_write(animation, paste0("plot/28-the-blue-planet2.gif"))
