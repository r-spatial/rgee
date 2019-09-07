#' Upload sf or stars objects to GEE asset
#'
#' Send either images or vectors to Google Earth Engine asset using Selenium
#'
#' @param x character, sf or stars object.
#' @param filename character. images destination path, full path for upload to Google Earth Engine,
#' e.g. users/pinkiepie/myponycollection.
#' @param bucket bucketname you are uploading to
#' @param properties data.frame; \href{https://developers.google.com/earth-engine/image_manifest}{Manifest upload}.
#' @param selenium_params character; determines how each pixel at a given level of the pyramid is
#' computed from the aggregation. "MEAN" (default), "MODE" and "SAMPLE".
#' @param quiet logical; suppress info message
#' @param ... ignored
#' @importFrom methods is as
#' @importFrom sf write_sf
#' @details  TALKING ABOUT SELENIUM
#' @name ee_upload
#' @export
ee_upload <- function(x, ...) {
  UseMethod("ee_upload")
}

#' @name ee_upload
#' @export
ee_upload.character <- function(x, ...,
                                filename,
                                bucket = NULL,
                                properties = getOption("rgee.upload.properties"),
                                selenium_params = getOption("rgee.selenium.params"),
                                quiet = FALSE) {

  filename <- ee_verify_filename(filename)
  gs_uri <- ee_upload_file_to_gcs(x, bucket = bucket, selenium_params = selenium_params)
  if (image_or_vector(x) == "sf") {
    ee_gcs_to_asset(gs_uri, filename, type = 'table' ,properties=NULL)
  } else if (image_or_vector(x) == "stars") {
    ee_gcs_to_asset(gs_uri, filename, type = 'image' ,properties=properties)
  } else {
    stop(sprintf("%s needs to be either a GeoTIFF or ESRI SHAPEFILE file", x))
  }
  return(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.sf <- function(x, ...,
                         filename,
                         bucket = NULL,
                         selenium_params = getOption("rgee.selenium.params"),
                         quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(filename)
  shp_dir <- sprintf("%s/%s.shp", ee_temp, basename(filename))
  write_sf(x,shp_dir)
  gs_uri <- ee_upload_file_to_gcs(shp_dir, bucket = bucket, selenium_params = selenium_params)
  ee_gcs_to_asset(gs_uri, filename, type = 'table' ,properties=NULL)
  return(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.stars <- function(x, ...,
                            filename,
                            bucket = NULL,
                            properties = getOption("rgee.upload.properties"),
                            selenium_params = getOption("rgee.selenium.params"),
                            quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(filename)
  tif_dir <- sprintf("%s/%s.tif", ee_temp, basename(filename))
  write_stars(x, tif_dir)
  gs_uri <- ee_upload_file_to_gcs(tif_dir, bucket = bucket, selenium_params = selenium_params)
  ee_gcs_to_asset(gs_uri, filename, type = 'image' , properties=properties)
  return(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.stars_proxy <- function(x, ...,
                                  filename,
                                  bucket = NULL,
                                  properties = getOption("rgee.upload.properties"),
                                  selenium_params = getOption("rgee.selenium.params"),
                                  quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(filename)
  tif_dir <- sprintf("%s/%s.tif", ee_temp, basename(filename))
  x <- x[[1]]
  gs_uri <- ee_upload_file_to_gcs(x, bucket = bucket, selenium_params = selenium_params)
  ee_gcs_to_asset(gs_uri, filename, type = 'image' , properties=properties)
  return(TRUE)
}
