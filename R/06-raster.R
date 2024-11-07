#' Prepare the environment:
#' 1. Terminate R session
#'
#' 2. Install miniconda
#' reticulate::install_miniconda()
#'
#' 3. Set miniconda Python Env
#' use_python("MINICONDA_PYTHON_PATH")
#'
#' 4. Install rgee from GitHub
#' remotes::install_github("r-spatial/rgee")
#'
#' 5. Run `ee_install()` use newest version
#'
#'
#' rgee::ee_install()
#'
#' library(rgee)
#' library(reticulate)
#' ee_check()
#'
#'
#' rgee::ee_install_upgrade()
library(rgee)
library(reticulate)

ee_Authenticate()
rgee:::ee_check_init()
ee_Initialize(project = "thirty-day-map-challenge-2024", drive = TRUE)

viirs_collection <- ee$ImageCollection("NOAA/VIIRS/DNB/ANNUAL_V22")

viirs_data <- viirs_collection$
  filterDate("2023-01-01", "2023-12-31")$
  select("average")

ee_crs <- viirs_data$first()$projection()$getInfo()$crs
geometry <- viirs_data$first()$geometry(proj = ee_crs)$bounds()
ee_imagecollection_to_local(
  ic = viirs_data,
  region = geometry
)

region <- ee$Geometry$Rectangle(c(-180, -90, 180, 90))
viirs_image <- ee_as_rast(image = viirs_data$first(), region = region, scale = 500)

plot(viirs_image)

task <- ee$batch$Export$image$toDrive(
  image = viirs_data$first(),  # 取得したVIIRSデータの最初の画像を選択
  description = "VIIRS_nightlights",
  folder = "GEE_exports",       # Google Driveのフォルダ
  fileNamePrefix = "viirs_nightlights",
  region = region,
  scale = 500,
  fileFormat = "GeoTIFF"
)
task$start()
