#' Creates a task to export an EE Image to Drive.
#'
#' Creates a task to export an EE Image to Drive.
#' This function is a wrapper around \code{ee$batch$Export$image$toDrive(...)}.
#'
#' @param image The image to be exported.
#' @param description Human-readable name of the task.
#' @param folder The name of a unique folder in your Drive account to be exported
#' into. Defaults to the root of the drive.
#' @param fileNamePrefix The Google Drive filename for the export. Defaults to
#' the name of the task.
#' @param dimensions The dimensions of the exported image. Takes either a
#' single positive integer as the maximum dimension or "WIDTHxHEIGHT" where
#' WIDTH and HEIGHT are each positive integers.
#' @param region The lon,lat coordinates for a LinearRing or Polygon specifying
#' the region to export. Can be specified as a nested lists of numbers or a
#' serialized string. Defaults to the image's region.
#' @param scale The resolution in meters per pixel. Defaults to the native
#' resolution of the image assset unless a crsTransform is specified.
#' @param crs The coordinate reference system of the exported image's
#' projection. Defaults to the image's default projection.
#' @param crsTransform A comma-separated string of 6 numbers describing
#' the affine transform of the coordinate reference system of the exported
#' image's projection, in the order: xScale, xShearing, xTranslation,
#' yShearing, yScale and yTranslation. Defaults to the image's native
#' CRS transform.
#' @param maxPixels The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param shardSize Size in pixels of the shards in which this image
#' will be computed. Defaults to 256.
#' @param fileDimensions The dimensions in pixels of each image file,
#' if the image is too large to fit in a single file. May specify a
#' single number to indicate a square shape, or a list of two dimensions
#' to indicate (width,height). Note that the image will still be clipped
#' to the overall image dimensions. Must be a multiple of shardSize.
#' @param skipEmptyTiles If TRUE, skip writing empty (i.e. fully-masked)
#' image tiles. Defaults to FALSE.
#' @param fileFormat The string file format to which the image is exported.
#' Currently only 'GeoTIFF' and 'TFRecord' are supported, defaults to 'GeoTIFF'.
#' @param formatOptions A dictionary of string keys to format specific
#' options. **kwargs: Holds other keyword arguments that may have been
#' deprecated such as 'crs_transform', 'driveFolder', and 'driveFileNamePrefix'.
#'
#' @return An unstarted Task that exports the image to Drive.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf',drive = TRUE)
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#'   mask2 <- image$mask()$reduce(ee$Reducer$min())
#'   image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'   image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_ROI)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#'
#' # Create simple composite
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_ROI)
#'
#' # Move results from Earth Engine to Drive
#' task_img <- ee_image_to_drive(
#'   image = mean_l5_Amarakaeri,
#'   folder = "Amarakaeri",
#'   fileFormat = "GEO_TIFF",
#'   fileNamePrefix = "my_image"
#' )
#'
#' task_img$start()
#' ee_monitoring(task_img)
#'
#' # Move results from Drive to local
#' img <- ee_drive_to_local(task = task_img)
#' plot(img)
#' }
#' @export
ee_image_to_drive <- function(image,
                              description = "myExportImageTask",
                              folder = NULL,
                              fileNamePrefix = NULL,
                              dimensions = NULL,
                              region = NULL,
                              scale = NULL,
                              crs = NULL,
                              crsTransform = NULL,
                              maxPixels = NULL,
                              shardSize = NULL,
                              fileDimensions = NULL,
                              skipEmptyTiles = NULL,
                              fileFormat = NULL,
                              formatOptions = NULL) {
  ee$batch$Export$image$toDrive(
    image = image,
    description = description,
    folder = folder,
    fileNamePrefix = fileNamePrefix,
    dimensions = dimensions,
    region = region,
    scale = scale,
    crs = crs,
    crsTransform = crsTransform,
    maxPixels = maxPixels,
    shardSize = shardSize,
    fileDimensions = fileDimensions,
    skipEmptyTiles = skipEmptyTiles,
    fileFormat = fileFormat,
    formatOptions = formatOptions
  )
}

#' Creates a task to export an EE Image to Google Cloud Storage.
#'
#' Creates a task to export an EE Image to Google Cloud Storage.
#' This function is a wrapper around
#' \code{ee$batch$Export$image$toCloudStorage(...)}.
#'
#' @param image The image to be exported.
#' @param description Human-readable name of the task.
#' @param bucket The name of a Cloud Storage bucket for the export.
#' @param fileNamePrefix Cloud Storage object name prefix for the export.
#' Defaults to the name of the task.
#' @param dimensions The dimensions of the exported image. Takes either a
#' single positive integer as the maximum dimension or "WIDTHxHEIGHT"
#' where WIDTH and HEIGHT are each positive integers.
#' @param region The lon,lat coordinates for a LinearRing or Polygon
#' specifying the region to export. Can be specified as a nested lists
#' of numbers or a serialized string. Defaults to the image's region.
#' @param scale The resolution in meters per pixel. Defaults to the native
#' resolution of the image assset unless a crsTransform is specified.
#' @param crs The coordinate reference system of the exported image's
#' projection. Defaults to the image's default projection.
#' @param crsTransform A comma-separated string of 6 numbers describing
#' the affine transform of the coordinate reference system of the exported
#' image's projection, in the order:
#' xScale, xShearing, xTranslation, yShearing, yScale and yTranslation.
#' Defaults to the image's native CRS transform.
#' @param maxPixels The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers more
#' pixels in the specified projection. Defaults to 100,000,000.
#' @param shardSize Size in pixels of the shards in which this image
#' will be computed. Defaults to 256.
#' @param fileDimensions The dimensions in pixels of each image file, if
#' the image is too large to fit in a single file. May specify a single
#' number to indicate a square shape, or a list of two dimensions to
#' indicate (width,height). Note that the image will still be clipped to
#' the overall image dimensions. Must be a multiple of shardSize.
#' @param skipEmptyTiles If TRUE, skip writing empty (i.e. fully-masked)
#' image tiles. Defaults to FALSE.
#' @param fileFormat The string file format to which the image is exported.
#' Currently only 'GeoTIFF' and 'TFRecord' are supported, defaults
#' to 'GeoTIFF'.
#' @param formatOptions A dictionary of string keys to format specific
#' options. **kwargs: Holds other keyword arguments that may have been
#' deprecated such as 'crs_transform'.
#'
#' @return An unstarted Task that exports the image to Google Cloud Storage.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf',gcs = TRUE)
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#'   mask2 <- image$mask()$reduce(ee$Reducer$min())
#'   image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'   image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_ROI)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#'
#' # Create simple composite
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_ROI)
#'
#' # Move results from Earth Engine to Drive
#' task_img <- ee_image_to_gcs(
#'     image = mean_l5_Amarakaeri,
#'     bucket = "rgee_dev",
#'     fileFormat = "GEO_TIFF",
#'     fileNamePrefix = "my_image"
#' )
#'
#' task_img$start()
#' ee_monitoring(task_img)
#'
#' # Move results from Drive to local
#' img <- ee_gcs_to_local(task = task_img)
#' plot(img)
#' }
#' @export
ee_image_to_gcs <- function(image,
                            description = "myExportImageTask",
                            bucket = NULL,
                            fileNamePrefix = NULL,
                            dimensions = NULL,
                            region = NULL,
                            scale = NULL,
                            crs = NULL,
                            crsTransform = NULL,
                            maxPixels = NULL,
                            shardSize = NULL,
                            fileDimensions = NULL,
                            skipEmptyTiles = NULL,
                            fileFormat = NULL,
                            formatOptions = NULL) {
  ee$batch$Export$image$toCloudStorage(
    image = image,
    description = description,
    bucket = bucket,
    fileNamePrefix = fileNamePrefix,
    dimensions = dimensions,
    region = region,
    scale = scale,
    crs = crs,
    crsTransform = crsTransform,
    maxPixels = maxPixels,
    shardSize = shardSize,
    fileDimensions = fileDimensions,
    skipEmptyTiles = skipEmptyTiles,
    fileFormat = fileFormat,
    formatOptions = formatOptions
  )
}

#' Creates a task to export an EE Image to an EE Asset.
#'
#' Creates a task to export an EE Image to an EE Asset.
#' This function is a wrapper around \code{ee$batch$Export$image$toAsset(...)}.
#'
#'
#' @param image The image to be exported.
#' @param description Human-readable name of the task.
#' @param assetId The destination asset ID.
#' @param pyramidingPolicy The pyramiding policy to apply to each band
#' in the image, a dictionary keyed by band name. Values must be one
#' of: "mean", "sample", "min", "max", or "mode". Defaults to "mean".
#' A special key, ".default", may be used to change the default for all bands.
#' @param dimensions The dimensions of the exported image. Takes either a
#' single positive integer as the maximum dimension or "WIDTHxHEIGHT" where
#' WIDTH and HEIGHT are each positive integers.
#' @param region The lon,lat coordinates for a LinearRing or Polygon
#' specifying the region to export. Can be specified as a nested lists
#' of numbers or a serialized string. Defaults to the image's region.
#' @param scale The resolution in meters per pixel. Defaults to the native
#' resolution of the image assset unless a crsTransform is specified.
#' @param crs The coordinate reference system of the exported image's
#' projection. Defaults to the image's default projection.
#' @param crsTransform A comma-separated string of 6 numbers describing
#' the affine transform of the coordinate reference system of the exported
#' image's projection, in the order:
#' xScale, xShearing, xTranslation, yShearing, yScale and yTranslation.
#' Defaults to the image's native CRS transform.
#' @param maxPixels The maximum allowed number of pixels in the exported
#' image. The task will fail if the exported region covers more pixels
#' in the specified projection. Defaults to 100,000,000. **kwargs: Holds
#' other keyword arguments that may have been deprecated such
#' as 'crs_transform'.
#'
#' @return An unstarted Task that exports the image to Earth Engine Asset
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf')
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#'   mask2 <- image$mask()$reduce(ee$Reducer$min())
#'   image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'   image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_ROI)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#'
#' # Create simple composite
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_ROI)
#'
#' # Move results from Earth Engine to Drive
#' assetid <- paste0(ee_get_assethome(), '/l5_Amarakaeri')
#' task_img <- ee_image_to_asset(
#'   image = mean_l5_Amarakaeri,
#'   assetId = assetid,
#'   scale = 500
#' )
#'
#' task_img$start()
#' ee_monitoring(task_img)
#'
#' ee_l5 <- ee$Image(assetid)
#' Map$centerObject(ee_l5)
#' Map$addLayer(ee_l5)
#' }
#' @export
ee_image_to_asset <- function(image,
                              description = "myExportImageTask",
                              assetId = NULL,
                              pyramidingPolicy = NULL,
                              dimensions = NULL,
                              region = NULL,
                              scale = NULL,
                              crs = NULL,
                              crsTransform = NULL,
                              maxPixels = NULL) {
  ee$batch$Export$image$toAsset(
    image = image,
    description = description,
    assetId = assetId,
    pyramidingPolicy = pyramidingPolicy,
    dimensions = dimensions,
    region = region,
    scale = scale,
    crs = crs,
    crsTransform = crsTransform,
    maxPixels = maxPixels
  )
}

#' Creates a task to export a FeatureCollection to Google Drive.
#'
#' Creates a task to export a FeatureCollection to Google Drive.
#' This function is a wrapper around \code{ee$batch$Export$table$toDrive(...)}.
#'
#' @param collection The feature collection to be exported.
#' @param description Human-readable name of the task.
#' @param folder The name of a unique folder in your Drive
#' account to export into. Defaults to the root of the drive.
#' @param fileNamePrefix The Google Drive filename for the
#' export. Defaults to the name of the task.
#' @param fileFormat The output format: "CSV" (default), "GeoJSON",
#' "KML", "KMZ", "SHP", or "TFRecord".
#' @param selectors The list of properties to include in the output,
#' as a list of strings or a comma-separated string. By default, all
#' properties are included. **kwargs: Holds other keyword arguments
#' that may have been deprecated such as 'driveFolder' and
#' 'driveFileNamePrefix'.
#'
#' @return An unstarted Task that exports the table to Google Drive.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf',drive = TRUE)
#'
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#' amk_fc <- ee$FeatureCollection(
#'   list(ee$Feature(ee_ROI, list(name = "Amarakaeri")))
#' )
#'
#' task_vector <- ee_table_to_drive(
#'   collection = amk_fc,
#'   folder = "Amarakaeri",
#'   fileFormat = "GEO_JSON",
#'   fileNamePrefix = "geom_Amarakaeri"
#' )
#' task_vector$start()
#' ee_monitoring(task_vector) # optional
#' amk_geom <- ee_drive_to_local(task = task_vector)
#' plot(amk_geom$geometry, border = "red", lwd = 10)
#' }
#' @export
ee_table_to_drive <- function(collection,
                              description = "myExportTableTask",
                              folder = NULL,
                              fileNamePrefix = NULL,
                              fileFormat = NULL,
                              selectors = NULL) {
  ee$batch$Export$table$toDrive(
    collection = collection,
    description = description,
    folder = folder,
    fileNamePrefix = fileNamePrefix,
    fileFormat = fileFormat,
    selectors = selectors
  )
}

#' Creates a task to export a FeatureCollection to Google Cloud Storage.
#'
#' Creates a task to export a FeatureCollection to Google Cloud Storage.
#' This function is a wrapper around
#' \code{ee$batch$Export$table$toCloudStorage(...)}.
#'
#' @param collection The feature collection to be exported.
#' @param description Human-readable name of the task.
#' @param bucket The name of a Cloud Storage bucket for the export.
#' @param fileNamePrefix Cloud Storage object name prefix
#' for the export. Defaults to the name of the task.
#' @param fileFormat The output format: "CSV" (default),
#' "GeoJSON", "KML", "KMZ", "SHP", or "TFRecord".
#' @param selectors The list of properties to include in the output,
#' as a list of strings or a comma-separated string. By default, all
#' properties are included. **kwargs: Holds other keyword arguments
#' that may have been deprecated such as 'outputBucket'.
#'
#' @return An unstarted Task that exports the table to Google Cloud Storage.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf',gcs = TRUE)
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#' amk_fc <- ee$FeatureCollection(
#'   list(ee$Feature(ee_ROI, list(name = "Amarakaeri")))
#' )
#'
#' task_vector <- ee_table_to_gcs(
#'     collection = amk_fc,
#'     bucket = "rgee_dev",
#'     fileFormat = "SHP",
#'     fileNamePrefix = "geom_Amarakaeri"
#' )
#' task_vector$start()
#' ee_monitoring(task_vector) # optional
#' amk_geom <- ee_gcs_to_local(task = task_vector)
#' plot(amk_geom$geometry, border = "red", lwd = 10)
#' }
#' @export
ee_table_to_gcs <- function(collection,
                            description = "myExportTableTask",
                            bucket = NULL,
                            fileNamePrefix = NULL,
                            fileFormat = NULL,
                            selectors = NULL) {
  ee$batch$Export$table$toCloudStorage(
    collection = collection,
    description = description,
    bucket = bucket,
    fileNamePrefix = fileNamePrefix,
    fileFormat = fileFormat,
    selectors = selectors
  )
}

#' Creates a task to export a FeatureCollection to an EE table asset.
#'
#' Creates a task to export a FeatureCollection to an EE table asset.
#' This function is a wrapper around \code{ee$batch$Export$table$toAsset(...)}.
#'
#' @param collection The feature collection to be exported.
#' @param description Human-readable name of the task.
#' @param assetId The destination asset ID. **kwargs: Holds other
#' keyword arguments that may have been deprecated.
#'
#' @return An unstarted Task that exports the table to Earth Engine Asset.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf')
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#' amk_fc <- ee$FeatureCollection(
#'   list(ee$Feature(ee_ROI, list(name = "Amarakaeri")))
#' )
#'
#' assetid <- paste0(ee_get_assethome(), '/geom_Amarakaeri')
#' task_vector <- ee_table_to_asset(
#'   collection = amk_fc,
#'   assetId = assetid
#' )
#' task_vector$start()
#' ee_monitoring(task_vector) # optional
#'
#' ee_fc <- ee$FeatureCollection(assetid)
#' Map$centerObject(ee_fc)
#' Map$addLayer(ee_fc)
#' }
#' @export
ee_table_to_asset <- function(collection,
                              description = "myExportTableTask",
                              assetId = NULL) {
  ee$batch$Export$table$toAsset(
    collection = collection,
    description = description,
    assetId = assetId
  )
}

#' Move results from Google Drive to a local directory
#'
#' Move results of an EE saved in Google Drive to a local directory.
#'
#' @param task List generated after finished correctly a EE task. See details.
#' @param directory Character. Output directory. If missing, a temporary
#' directory will be assigned.
#' @param overwrite A boolean argument which indicates indicating
#' whether "filename" should be overwritten. By default it is TRUE.
#' @param consider Character. See details.
#' @param quiet logical. Suppress info message
#'
#' @details
#' The task argument needs a status as task "COMPLETED" to work, since the
#' parameters necessary to locate the file into google drive are obtained
#' from ee$batch$Export$*$toDrive(...)$start()$status().
#'
#' Google Drive permits users to create files with the same name. The
#' argument "consider", which use an interactive R session by default,
#' is created to help users to deal with it. If it is specified
#' as 'last', and exist multiple files (images or vectors) with
#' the same filename, ee_drive_to_local will download just the last
#' file saved. On the another hand, if it is specified as 'all', all
#' files will be downloaded.
#' @return
#' An sf, stars, or character depending on the retunclass argument.
#' @importFrom stars read_stars
#' @importFrom utils menu
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf',drive = TRUE)
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#'   mask2 <- image$mask()$reduce(ee$Reducer$min())
#'   image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'   image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_ROI)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#'
#' # Create simple composite
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_ROI)
#'
#' # Move results from Earth Engine to Drive
#' task_img <- ee_image_to_drive(
#'   image = mean_l5_Amarakaeri,
#'   folder = "Amarakaeri",
#'   fileFormat = "GEO_TIFF",
#'   fileNamePrefix = "my_image"
#' )
#'
#' task_img$start()
#' ee_monitoring(task_img)
#'
#' # Move results from Drive to local
#' img <- ee_drive_to_local(task = task_img)
#' plot(img)
#' }
#' @export
ee_drive_to_local <- function(task,
                              directory,
                              overwrite = TRUE,
                              consider = interactive(),
                              quiet = FALSE) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("The googledrive package is required to use rgee::ee_download_drive",
      call. = FALSE
    )
  } else {
    ee_user <- ee_exist_credentials()
    if (is.na(ee_user$gcs_cre)) {
      stop(
        "Google Drive credentials were not loaded.",
        ' Run ee_Initialize(email = "myemail", gcs = TRUE)',
        " to fix it"
      )
    }
    # global parameter of a task
    gd_folder <- basename(task$status()$destination_uris)
    gd_ExportOptions <- task$config$fileExportOptions
    gd_filename <- gd_ExportOptions$driveDestination$filenamePrefix

    # Select a google drive file considering the filename and folder
    count <- 1
    files_gd <- try(googledrive::drive_find(
      q = sprintf("'%s' in parents", gd_folder),
      q = sprintf("name contains '%s'", gd_filename)
    ), silent = TRUE)
    while (any(class(files_gd) %in% "try-error") & count < 5) {
      files_gd <- try(googledrive::drive_find(
        q = sprintf("'%s' in parents", gd_folder),
        q = sprintf("name contains '%s'", gd_filename)
      ), silent = TRUE)
      count <- count + 1
    }

    # (Problem) Google Drive support files with the same name
    if (nrow(files_gd) > 0) {
      ee_getTime <- function(x) {
        gd_file_date <- files_gd$drive_resource[[x]]$createdTime
        as.POSIXct(gd_file_date)
      }
      createdTime <- vapply(seq_len(nrow(files_gd)), ee_getTime, 0)
      files_gd <- files_gd[order(createdTime, decreasing = TRUE), ]
      if (isTRUE(consider)) {
        choices <- c(files_gd$name,'last','all')
        if (nrow(files_gd) == 1) {
          file_selected <- 1
        } else {
          file_selected <- menu(
            choices = choices,
            title = paste0(
              "Multiple files with the same name",
              " (sorted according to the created time argument):"
            )
          )
        }
        if (choices[file_selected] == 'last') {
          files_gd <- files_gd[1,]
        } else if (choices[file_selected] == 'all') {
          files_gd <- files_gd
        } else {
          files_gd <- files_gd[file_selected, ]
        }
      } else if (consider == "last") {
        files_gd <- files_gd[1, ]
      } else if (consider == "all") {
        files_gd <- files_gd
      } else {
        stop("consider argument was not defined properly.")
      }
    } else {
      stop(
        "File does not exist in Google Drive.",
        " Please verify if the task finished properly."
      )
    }

    # Choose the right file using the driver_resource["originalFilename"]
    fileformat <- toupper(gd_ExportOptions$fileFormat)
    if (missing(directory)) ee_tempdir <- tempdir()

    filenames_local <- sprintf("%s/%s", ee_tempdir, basename(files_gd$name))

    # it is necessary for ESRI shapefiles
    to_download <- sort_drive_files(files_gd, fileformat)
    filenames_local <- ee_sort_localfiles(filenames_local, fileformat)

    # if (nrow(to_download) > 4) {
    #   stop(
    #     "Impossible to download multiple geometries as SHP.",
    #     " Try to define the fileFormat argument as GEO_JSON"
    #   )
    # }
    for (index in seq_len(nrow(to_download))) {
      googledrive::drive_download(
        file = to_download[index, ],
        path = filenames_local[index],
        overwrite = overwrite,
        verbose = !quiet
      )
    }
    read_filenames(filenames_local, fileformat, quiet = quiet)
  }
}

#' Move results from Google Cloud Storage to a local directory
#'
#' Move results of an EE task saved in Google Cloud Storage to a local
#' directory.
#'
#' @param task List generated after finished correctly a EE task. See details.
#' @param directory Character. Output directory. If missing, a temporary
#' directory will be assigned.
#' @param overwrite A boolean indicating whether the file should
#' be overwritten.
#' @param quiet Logical. Suppress info message
#' @details
#' The best way to use `rgee::ee_download_gcs` is save the Google Cloud
#' Project JSON file into `ee_get_earthengine_path()` with the name
#' GCS_AUTH_FILE.json. It is necessary in order to attain that rgee can
#' read the credentials automatically.
#'
#' The task argument needs "COMPLETED" task state to work, since the parameters
#' necessaries to locate the file into google cloud storage are obtained from
#' ee$batch$Export$*$toCloudStorage(...)$start()$status().
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_users()
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize(email = 'data.colec.fbf',gcs = TRUE)
#'
#' # Define study area (local -> earth engine)
#' # Communal Reserve Amarakaeri - Peru
#' rlist <- list(xmin = -71.13, xmax = -70.95,ymin = -12.89, ymax = -12.73)
#' ROI <- c(rlist$xmin, rlist$ymin,
#'          rlist$xmax, rlist$ymin,
#'          rlist$xmax, rlist$ymax,
#'          rlist$xmin, rlist$ymax,
#'          rlist$xmin, rlist$ymin)
#' ee_ROI <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon() %>%
#'   st_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#'
#' # Get the mean annual NDVI for 2011
#' cloudMaskL457 <- function(image) {
#'   qa <- image$select("pixel_qa")
#'   cloud <- qa$bitwiseAnd(32L)$
#'     And(qa$bitwiseAnd(128L))$
#'     Or(qa$bitwiseAnd(8L))
#'   mask2 <- image$mask()$reduce(ee$Reducer$min())
#'   image <- image$updateMask(cloud$Not())$updateMask(mask2)
#'   image$normalizedDifference(list("B4", "B3"))
#' }
#'
#' ic_l5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
#'   filterBounds(ee_ROI)$
#'   filterDate("2011-01-01", "2011-12-31")$
#'   map(cloudMaskL457)
#'
#' # Create simple composite
#' mean_l5 <- ic_l5$mean()$rename("NDVI")
#' mean_l5 <- mean_l5$reproject(crs = "EPSG:4326", scale = 500)
#' mean_l5_Amarakaeri <- mean_l5$clip(ee_ROI)
#'
#' # Move results from Earth Engine to Drive
#' task_img <- ee_image_to_gcs(
#'     image = mean_l5_Amarakaeri,
#'     bucket = "rgee_dev",
#'     fileFormat = "GEO_TIFF",
#'     fileNamePrefix = "my_image"
#' )
#'
#' task_img$start()
#' ee_monitoring(task_img)
#'
#' # Move results from Drive to local
#' img <- ee_gcs_to_local(task = task_img)
#' plot(img)
#' }
#' @export
ee_gcs_to_local <- function(task,
                            directory,
                            overwrite = TRUE,
                            quiet = FALSE) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    stop(
      "The googleCloudStorageR package is required to use",
      " rgee::ee_download_gcs",
      call. = FALSE
    )
  } else {
    ee_user <- ee_exist_credentials()
    if (is.na(ee_user$gcs_cre)) {
      stop(
        "Google Drive credentials were not loaded.",
        ' Run ee_Initialize(email = "myemail", gcs = TRUE)',
        " to fix it"
      )
    }
    # Getting bucket name and filename
    gcs_ExportOptions <- task$config$fileExportOptions
    gcs_bucket <- gcs_ExportOptions$gcsDestination$bucket
    gcs_filename <- gcs_ExportOptions$gcsDestination$filenamePrefix
    gcs_fileFormat <- gcs_ExportOptions$fileFormat

    # Select a gcs file considering the filename and bucket
    count <- 1
    files_gcs <- try(
      expr = googleCloudStorageR::gcs_list_objects(
        bucket = gcs_bucket,
        prefix = gcs_filename
      ),
      silent = TRUE
    )
    while (any(class(files_gcs) %in% "try-error") & count < 5) {
      files_gcs <- try(
        expr = googleCloudStorageR::gcs_list_objects(
          bucket = gcs_bucket,
          prefix = gcs_filename
        ),
        silent = TRUE
      )
      count <- count + 1
    }

    # Choose the right file using the driver_resource["originalFilename"]
    fileformat <- toupper(gcs_fileFormat)
    if (missing(directory)) ee_tempdir <- tempdir()

    filenames_local <- sprintf("%s/%s", ee_tempdir, basename(files_gcs$name))

    # It is necessary for ESRI shapefiles
    to_download <- sort_drive_files(files_gcs, fileformat)
    filenames_local <- ee_sort_localfiles(filenames_local, fileformat)

    for (index in seq_along(filenames_local)) {
      if (isTRUE(quiet)) {
        suppressMessages(
          googleCloudStorageR::gcs_get_object(
            object_name = to_download[index,]$name,
            bucket = gcs_bucket,
            saveToDisk = filenames_local[index],
            overwrite = TRUE
          )
        )
      } else {
        googleCloudStorageR::gcs_get_object(
            object_name = to_download[index,]$name,
            bucket = gcs_bucket,
            saveToDisk = filenames_local[index],
            overwrite = TRUE
        )
      }
    }
    read_filenames(filenames_local, fileformat, quiet = quiet)
  }
}

#' Monitoring Earth Engine task progress
#'
#' @param task List generated after an EE task has been successfully completed.
#' @param eeTaskList Logical, if \code{TRUE}, all Earth Engine tasks will
#' @param quiet logical. Suppress info message
#' be listed.
#'
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_initialize()
#' ee_download_monitoring(eelist = TRUE)
#' }
#' @export
ee_monitoring <- function(task, quiet = FALSE, eeTaskList = FALSE) {
  if (missing(task)) {
    task <- ee$batch$Task$list()[[1]]
  }
  if (eeTaskList) {
    if (!quiet) {
      cat("EETaskList:\n")
    }
    task_list <- mapply(function(x) {
      sprintf("<Task %s: %s (%s)>", x$task_type, x$config, x$state)
    }, ee$batch$Task$list())
    if (!quiet) {
      cat("", paste0(task_list, "\n"))
      cat("\n")
    }
  }
  while (task$active() & task$state != "CANCEL_REQUESTED") {
    if (!quiet) {
      print(sprintf("Polling for task (id: %s).", task$id))
    }
    Sys.sleep(5)
  }
  if (!quiet) {
    print(sprintf("State: %s", task$status()$state))
  }
  if (!is.null(task$status()$error_message)) {
    print(task$status()$error_message)
  }
}

#' Create a download file according to its format
#' @noRd
read_filenames <- function(filename, fileformat, quiet) {
  if (fileformat == "GEO_TIFF") {
    read_stars(filename, proxy = TRUE, quiet = quiet)
  } else if (fileformat %in% "SHP") {
    st_read(filename[grep("\\.shp$", filename)], quiet = quiet)
  } else if (fileformat %in% c("GEO_JSON", "KML", "KMZ")) {
    st_read(filename, quiet = quiet)
  } else {
    if (!quiet) {
      print(sprintf(
        "Download completed:%s (%s)",
        filename,
        fileformat
      ))
    }
    invisible(filename)
  }
}

#' Sort google drives files
#' @noRd
sort_drive_files <- function(drive_files, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep(
      pattern = "(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)",
      x = drive_files[["name"]]
    )
    selected_drive_files <- drive_files[shp_file, ]
    drive_files_sort <- selected_drive_files[order(selected_drive_files$name), ]
  } else {
    drive_files_sort <- drive_files[order(drive_files[["name"]]), ]
  }
  drive_files_sort
}

#' Sort local files
#' @noRd
ee_sort_localfiles <- function(filenames, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep("(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)", filenames)
    shp_file <- filenames[shp_file]
    shp_file[order(shp_file)]
  } else {
    filenames[order(filenames)]
  }
}


#' GCS or Google Drive Exist credentials?
#' @noRd
ee_exist_credentials <- function() {
  ee_path <- path.expand("~/.config/earthengine")
  read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )
}
