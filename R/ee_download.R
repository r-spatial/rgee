#' Creates a task to export an EE Image to Drive.
#'
#' Creates a task to export an EE Image to Drive.
#' This function is a wrapper around \code{ee$batch$Export$image$toDrive(...)}.
#'
#' @param image The image to be exported.
#' @param description Human-readable name of the task.
#' @param folder The name of a unique folder in your Drive account to export
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
#' @param skipEmptyTiles If true, skip writing empty (i.e. fully-masked)
#' image tiles. Defaults to false.
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
#' @param skipEmptyTiles If true, skip writing empty (i.e. fully-masked)
#' image tiles. Defaults to false.
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
#' @param filename Character. Output filename. If missing, a temporary
#' file is created.
#' @param overwrite A boolean argument which indicates indicating
#' whether "filename" should be overwritten.
#' @param st logical. By default it is TRUE, returning a sf (stars)
#' object for EE tables (images). If FALSE, the output filename is returned.
#' @param quiet logical. Suppress info message
#'
#' @details
#' The task argument needs a status as task "COMPLETED" to work, since the
#' parameters necessary to locate the file into google drive are obtained
#' from ee$batch$Export$*$toDrive(...)$start()$status().
#' @return
#' An sf, stars or character depending on the retunclass argument.
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
ee_drive_to_local <- function(task, filename, overwrite = FALSE, st = TRUE,
                              quiet = FALSE) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("The googledrive package is required to use rgee::ee_download_drive",
      call. = FALSE
    )
  } else {
    gd_folder <- basename(task$status()$destination_uris)
    gd_filename <- task$
      config$
      fileExportOptions$
      driveDestination$
      filenamePrefix
    # Selecting a file considering the name
    files_gd <- try(googledrive::drive_find(
      q = sprintf("'%s' in parents", gd_folder),
      q = sprintf("name contains '%s'", gd_filename)
    ))
    count <- 1
    while (any(class(files_gd) %in% "try-error") & count < 5) {
      files_gd <- try(googledrive::drive_find(
        q = sprintf("'%s' in parents", gd_folder),
        q = sprintf("name contains '%s'", gd_filename)
      ))
      count <- count + 1
    }

    fileformat <- get_fileformat(task)

    if (nrow(files_gd) > 1 & !(fileformat %in% c("SHP", "TF_RECORD_IMAGE"))) {
      show_files <- files_gd
      getTime <- function(x) files_gd$drive_resource[[x]]$createdTime
      createdTime <- vapply(seq_len(nrow(files_gd)), getTime, "")
      show_files$drive_resource <- NULL
      show_files$createdTime <- createdTime
      print(show_files, width = Inf)
      file_selected <- menu(
        choices = files_gd$id,
        title = "Multiple files with the same name:"
      )
      files_gd_todownload <- files_gd[file_selected, ]
    } else {
      files_gd_todownload <- files_gd
    }

    # Choose the right file using the driver_resource["originalFilename"]
    fileformat <- toupper(task$config$fileExportOptions$fileFormat)
    if (missing(filename)) filename <- tempfile()

    file_suffix <- get_format_suffix(fileformat)
    filenames_hd <- create_filenames(filename, file_suffix, fileformat)

    # it is necessary for ESRI shapefiles
    to_download <- sort_drive_files(files_gd_todownload, fileformat)
    filenames_hd <- sort_harddisk_files(filenames_hd, fileformat)

    if (nrow(to_download) > 4) {
      stop(
        "Impossible to download multiple geometries as SHP.",
        " Try to define the fileFormat argument as GEO_JSON"
      )
    }

    for (z in seq_len(nrow(to_download))) {
      googledrive::drive_download(
        file = to_download[z, ],
        path = filenames_hd[z],
        overwrite = overwrite,
        verbose = !quiet
      )
    }
    read_filenames(filenames_hd, fileformat, quiet = quiet)
  }
}

#' Move results from Google Cloud Storage to a local directory
#'
#' Move results of an EE task saved in Google Cloud Storage to a local
#' directory.
#'
#' @param task List generated after finished correctly a EE task. See details.
#' @param filename Output filename.
#' @param overwrite A boolean indicating whether "filename" should
#' be overwritten.
#' @param GCS_AUTH_FILE Authentication json file you have downloaded from
#' your Google Cloud Project
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
ee_gcs_to_local <- function(task, filename, overwrite = FALSE,
                            GCS_AUTH_FILE = getOption("rgee.gcs.auth"),
                            quiet = TRUE) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    stop("The googleCloudStorageR package is required to use",
      "rgee::ee_download_gcs",
      call. = FALSE
    )
  } else {
    gcs_bucket <- task$config$fileExportOptions$gcsDestination$bucket
    gd_filename <- task$config$fileExportOptions$gcsDestination$filenamePrefix
    if (missing(filename)) filename <- tempfile()

    fileformat <- get_fileformat(task)
    file_suffix <- get_format_suffix(fileformat)
    filenames_gcs <- create_filenames(gd_filename, file_suffix, fileformat)
    filenames_hd <- create_filenames(filename, file_suffix, fileformat)

    # it is necessary for ESRI shapefiles
    filenames_gcs <- sort_harddisk_files(filenames_gcs, fileformat)
    filenames_hd <- sort_harddisk_files(filenames_hd, fileformat)

    for (z in seq_along(filenames_hd)) {
      intent <- try(googleCloudStorageR::gcs_get_object(
        object_name = filenames_gcs[z],
        bucket = gcs_bucket,
        saveToDisk = filenames_hd[z],
        overwrite = TRUE
      ), silent = TRUE)
      if (class(intent) == "try-error") {
        googleCloudStorageR::gcs_get_object(
          object_name = gsub("ee_export", "", filenames_gcs[z]),
          bucket = gcs_bucket,
          saveToDisk = filenames_hd[z],
          overwrite = TRUE
        )
      }
    }
    read_filenames(filenames_hd, fileformat, quiet = quiet)
  }
}


#' Monitoring Earth Engine task progress
#'
#' @param task List generated after an EE task has been successfully completed.
#' @param eeTaskList Logical, if \code{TRUE}, all Earth Engine tasks will
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
ee_monitoring <- function(task, eeTaskList = FALSE) {
  if (missing(task)) {
    task <- ee$batch$Task$list()[[1]]
  }
  if (eeTaskList) {
    cat("EETaskList:\n")
    task_list <- mapply(function(x) {
      sprintf("<Task %s: %s (%s)>", x$task_type, x$config, x$state)
    }, ee$batch$Task$list())
    cat("", paste0(task_list, "\n"))
  }
  cat("\n")
  while (task$active() & task$state != "CANCEL_REQUESTED") {
    print(sprintf("Polling for task (id: %s).", task$id))
    Sys.sleep(5)
  }
  print(sprintf("State: %s", task$status()$state))
  if (!is.null(task$status()$error_message)) {
    print(task$status()$error_message)
  }
}


#' get format file suffix - GCS
#' @noRd
get_format_suffix <- function(x) {
  shp_sx <- list(
    "ee_export.dbf", "ee_export.shx", "ee_export.prj",
    "ee_export.shp"
  )
  # ctf_sx <- list(".json",sprintf("-%05d.tfrecord.gz",0:(length(gd_folder)-2)))
  image_ctf_sx <- list(".json", ".tfrecord.gz")
  # tf_sx <- list(".json", sprintf("-%05d.tfrecord",0:(length(gd_folder)-2)))
  image_tf_sx <- list(".json", ".tfrecord")
  suffix <- list(
    ".tif", "ee_export.csv", "ee_export.GEO_JSON", "ee_export.kml",
    "ee_export.kmz", shp_sx, image_tf_sx, image_ctf_sx,
    "ee_export.gz", "ee_export.gz"
  )
  names(suffix) <- c(
    "GEO_TIFF", "CSV", "GEO_JSON", "KML",
    "KMZ", "SHP", "TF_RECORD_IMAGE", "CTF_RECORD_IMAGE",
    "TF_RECORD_VECTOR", "CTF_RECORD_VECTOR"
  )
  return(as.character(unlist(suffix[x])))
}

#'  Get the file format
#'  Format available: "GEO_TIFF", "CSV", "GEO_JSON", "KML", "KMZ",
#'                    "SHP", "TF_RECORD_IMAGE", "CTF_RECORD_IMAGE",
#'                    "TF_RECORD_VECTOR", "CTF_RECORD_VECTOR"
#' @noRd
get_fileformat <- function(x) {
  if (x$config$fileExportOptions$fileFormat == "TFRECORD") {
    if (x$task_type == "EXPORT_FEATURES") {
      if (x$config$fileExportOptions$formatOptions$compressed == TRUE) {
        return("CTF_RECORD_VECTOR")
      } else {
        return("TF_RECORD_VECTOR")
      }
    } else {
      if (x$config$fileExportOptions$tfrecordCompressed == TRUE) {
        return("CTF_RECORD_IMAGE")
      } else {
        return("TFRECORD_IMAGE")
      }
    }
  } else {
    return(x$config$fileExportOptions$fileFormat)
  }
}

#' Create file (or files) to save - GCS
#' @noRd
create_filenames <- function(basename, suffix, fileformat) {
  if (fileformat %in% c("GEO_TIFF", "TF_RECORD_IMAGE", "CTF_RECORD_IMAGE")) {
    filename <- sprintf("%s%s", basename, suffix)
  } else {
    filename <- sprintf("%s%s", basename, suffix)
  }
  return(filename)
}

#' Create a download file according to its format
#' @noRd
read_filenames <- function(filename, fileformat, quiet) {
  if (fileformat == "GEO_TIFF") {
    fread <- read_stars(filename, quiet = quiet)
    return(fread)
  } else if (fileformat %in% "SHP") {
    fread <- st_read(filename[grep("\\.shp$", filename)], quiet = quiet)
    return(fread)
  } else if (fileformat %in% c("GEO_JSON", "KML", "KMZ")) {
    fread <- st_read(filename, quiet = quiet)
    return(fread)
  } else {
    if (!quiet) {
      print(sprintf(
        "Download completed:%s (%s)",
        filename,
        fileformat
      ))
    }
    return(TRUE)
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

#' Sort hard disk files
#' @noRd
sort_harddisk_files <- function(harddisk_files, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep("(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)", harddisk_files)
    shp_file <- harddisk_files[shp_file]
    harddisk_files_sort <- shp_file[order(shp_file)]
  } else {
    harddisk_files_sort <- harddisk_files[order(harddisk_files)]
  }
  return(harddisk_files_sort)
}
