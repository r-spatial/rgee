#' Convert a stars or stars-proxy object into an EE Image object
#'
#' @param x stars or stars-proxy object to be converted into an ee$Image.
#' @param assetId Character. Destination asset ID for the uploaded file.
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool. If NULL, rgee assumes is saved in the same path that your Python
#' environment (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param overwrite Logical. If TRUE, the assetId will be overwritten.
#' @param bucket Character. Name of the GCS bucket.
#' @param monitoring Logical. If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#'
#' @return An ee$Image object
#' @family image upload functions
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize(gcs = TRUE)
#'
#' # Get the filename of a image
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # # Method 1
#' # # 1. Move from local to gcs
#' # gs_uri <- local_to_gcs(x = tif, bucket = 'rgee_dev')
#'
#' # # 2. Pass from gcs to asset
#' # gcs_to_ee_image(
#' #   x = x,
#' #   overwrite = TRUE,
#' #   gs_uri = gs_uri,
#' #   assetId = assetId
#' #)
#'
#' # # OPTIONAL: Monitoring progress
#' # ee_monitoring()
#'
#' # # OPTIONAL: Display results
#' # ee_stars_01 <- ee$Image(assetId)
#' # Map$centerObject(ee_stars_01)
#' # Map$addLayer(ee_stars_01)
#'
#' # # Method 2
#' # ee_stars_02 <- stars_as_ee(
#' #   x = x,
#' #   overwrite = TRUE,
#' #   assetId = assetId,
#' #   bucket = "rgee_dev"
#' # )
#' # Map$centerObject(ee_stars_02)
#' # Map$addLayer(ee_stars_02)
#' }
#' @export
stars_as_ee <- function(x,
                        assetId,
                        command_line_tool_path = NULL,
                        overwrite = FALSE,
                        monitoring = TRUE,
                        bucket = NULL,
                        quiet = FALSE) {
  # Folder to save intermediate upload files
  ee_temp <- tempdir()

  if (is.null(command_line_tool_path)) {
    command_line_tool_path <- dirname(Sys.getenv("EARTHENGINE_PYTHON"))
  }

  message("1. Converting stars (raster) object to GeoTIFF ... saving in /tmp")
  stars_proxy <- ee_as_proxystars(x, temp_dir = ee_temp)

  message("2. From local to GCS")
  gcs_filename <- local_to_gcs(
    x = stars_proxy[[1]],
    bucket = bucket,
    quiet = quiet
  )

  message("3. From GCS to Earth Engine")
  # Verify is the EE assets path is valid
  assetId <- ee_verify_filename(
    path_asset = assetId,
    strict = FALSE
  )

  gcs_to_ee_image(
    x = x,
    gs_uri = gcs_filename,
    overwrite = overwrite,
    assetId = assetId,
    command_line_tool_path = command_line_tool_path
  )

  if (isTRUE(monitoring)) {
    ee_monitoring()
    ee$Image(assetId)
  } else {
    assetId
  }
}


#' Convert a Raster* object into an EE Image object
#'
#' @param x RasterLayer, RasterStack or RasterBrick object to be converted into
#' an ee$Image.
#' @param assetId Character. Destination asset ID for the uploaded file.
#' @param command_line_tool_path Character. Path to the Earth Engine command line
#' tool. If NULL, rgee assumes is saved in the same path that your Python
#' environment (ignore if \code{via} is not defined as "gcs_to_asset").
#' @param overwrite Logical. If TRUE, the assetId will be overwritten.
#' @param bucket Character. Name of the GCS bucket.
#' @param monitoring Logical. If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#'
#' @return An ee$Image object
#' @family image upload functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize(gcs = TRUE)
#'
#' # Get the filename of a image
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # # Method 1
#' # # 1. Move from local to gcs
#' # gs_uri <- local_to_gcs(x = tif, bucket = 'rgee_dev')
#' #
#' # # 2. Pass from gcs to asset
#' # gcs_to_ee_image(
#' #   x = x,
#' #   overwrite = TRUE,
#' #   gs_uri = gs_uri,
#' #   assetId = assetId
#' # )
#' #
#' # # OPTIONAL: Monitoring progress
#' # ee_monitoring()
#' #
#' # # OPTIONAL: Display results
#' # ee_raster_01 <- ee$Image(assetId)
#' # Map$centerObject(ee_raster_01)
#' # Map$addLayer(ee_raster_01)
#' #
#' # # Method 2
#' # ee_raster_02 <- raster_as_ee(
#' #   x = x,
#' #   assetId = assetId,
#' #   overwrite = TRUE,
#' #   bucket = "rgee_dev"
#' # )
#' # Map$centerObject(ee_raster_02)
#' # Map$addLayer(ee_raster_02)
#' }
#' @export
raster_as_ee <- stars_as_ee
