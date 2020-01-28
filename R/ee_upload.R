#' Upload sf or stars objects into a GEE asset
#'
#' Upload either images or vectors to Google Earth Engine asset
#'
#' @param x Character, sf or stars object to upload into a GEE asset.
#' @param filename Character. Asset destination path,
#' e.g. users/pinkiepie/myponycollection.
#' @param bucket bucketname you are uploading to. See details.
#' @param properties List. Set of parameters to established as a property of
#' an EE object. See details.
#' @param start_time Character. The timestamp associated with the asset. The
#' initial time stamp is set to the nominal image acquisition time for single
#' scenes.
#' @param end_time Character. Useful for assets that correspond to an
#' interval of time. The ending time stamp is set to the nominal image
#' acquisition time for single scenes.
#' @param selenium_params List. Optional parameters when bucket is NULL.
#' Parameters for setting selenium. See details.
#' @param clean Logical; whether is TRUE cache will cleaned, see Description.
#' @param reinit Logical; run ee_Initialize(gcs=TRUE) before start to upload
#' @param quiet Logical. Suppress info message.
#' @param ... ignored
#' @importFrom methods is as
#' @importFrom sf write_sf
#' @details
#' It is necessary, for uploading process, get authorization to read & write
#' into a Google Cloud Storage (GCS) bucket. Earth Engine provides a
#' provisional for free space into GCS through gs://earthengine-uploads/. If
#' the bucket argument is absent, this function will use Selenium driver for
#' getting access to the URI mentioned bellow, see \link{ee_upload_file_to_gcs}
#' for details. Install and check the Selenium drivers for Google Chrome is
#' possible as follow:\cr
#' - rgee::ee_install_drivers()\cr
#' - rgee::ee_check_drivers()\cr
#'
#' The properties argument is just available for image uploads, is you are
#' interesting in setting properties in FeatureCollection. Please use
#' \link{ee_manage_set_properties} after \link{ee_upload}.
#'
#' The selenium_params argument is a three-element list consisting of:\cr
#'  - gmail_account: The google account. If it is not specified, it will
#'  obtained from ee$data$getAssetRoots().\cr
#'  - showpassword: Logical. After put the google account into
#'  \link[getPass]{getPass}, should be shown?.
#'  - cache: Logical. TRUE will use the cookies saved on the /temp directory.
#'
#' With respect to the variables time_start and time_end, both needs
#' to be specified as seconds since the epoch (1970-01-01). Assumed to
#' be in the UTC time zone.
#' @name ee_upload
#' @examples
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' username <- 'aybar1994' #change according to username.
#' gcs_bucket <- 'bag_csaybar'
#' ee_check_drivers()
#' ee_Initialize(email = 'aybar1994', gcs = TRUE)
#'
#' # Create a folder in Earth Engine Asset
#' filename <- sprintf("users/%s/rgee_upload/", username)
#' ee_manage_create(filename)
#'
#' # Select image to upload
#' tif = system.file("tif/geomatrix.tif", package = "stars")
#' geomatrix = read_stars(tif) %>% st_warp(crs=st_crs(4326))
#'
#' # Upload to earth egnine
#' ee_upload(x = geomatrix,
#'           filename = paste0(filename,"geomatrix"),
#'           bucket = gcs_bucket)
#'
#' # Read uploaded image
#' asset_geomatrix <- paste0(filename,"geomatrix")
#' ee_geomatrix <- ee$Image(asset_geomatrix)
#' ee_map(ee_geomatrix, zoom_start = 18)
#' ## OPTIONAL: add properties
#' ee_manage_set_properties(
#'   path_asset = asset_geomatrix,
#'   add_properties = list(message='hello-world',language = 'R'))
#'
#' # Clean EE asset and GCS
#' ee_manage_delete(dirname(asset_geomatrix))
#' googleCloudStorageR::gcs_global_bucket(gcs_bucket)
#' buckets <- googleCloudStorageR::gcs_list_objects()
#' mapply(googleCloudStorageR::gcs_delete_object, buckets$name)
#' @export
ee_upload <- function(x, ...) {
  UseMethod("ee_upload")
}

#' @name ee_upload
#' @export
ee_upload.character <- function(x, ... ,
                                filename,
                                bucket = NULL,
                                properties = NULL,
                                start_time = '1970-01-01',
                                end_time = '1970-01-01',
                                selenium_params = getOption(
                                  "rgee.selenium.params"
                                ),
                                clean = FALSE,
                                reinit = FALSE,
                                quiet = FALSE) {
  email <- getOption("rgee.selenium.params")$email
  if (is.null(email)) {
    stop('ee_upload needs that "email" ',
         'argument be specified in ee$Initialize().',
         "\nExample: ee_Initialize(email = 'XXXX@gmail.com')")
  }

  filename <- ee_verify_filename(path_asset = filename,
                                        strict = FALSE)
  gs_uri <- ee_upload_file_to_gcs(x = x,
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  clean = clean,
                                  reinit = reinit)

  if (image_or_vector(x) == "sf") {
    ee_gcs_to_asset(x = x,
                    gs_uri = gs_uri,
                    filename = filename,
                    type = 'table' ,
                    properties=NULL)
  } else if (image_or_vector(x) == "stars") {
    ee_gcs_to_asset(x = read_stars(x),
                    gs_uri = gs_uri,
                    filename = filename,
                    type = 'image',
                    properties=properties,
                    start_time = '1970-01-01',
                    end_time = '1970-01-01')
  } else {
    stop(sprintf("%s needs to be either a GeoTIFF or ESRI SHAPEFILE file", x))
  }
  invisible(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.sf <- function(x, ...,
                         filename,
                         bucket = NULL,
                         selenium_params = getOption("rgee.selenium.params"),
                         clean = FALSE,
                         reinit = FALSE,
                         quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  shp_dir <- sprintf("%s/%s.shp", ee_temp, basename(filename))
  write_sf(x,shp_dir)
  gs_uri <- ee_upload_file_to_gcs(x = shp_dir,
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  clean = clean,
                                  reinit = reinit)
  ee_gcs_to_asset(x = x,
                  gs_uri = gs_uri,
                  filename = filename,
                  type = 'table',
                  properties=NULL)
  invisible(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.stars <- function(x, ...,
                            filename,
                            bucket = NULL,
                            properties = NULL,
                            start_time = '1970-01-01',
                            end_time = '1970-01-01',
                            selenium_params = getOption("rgee.selenium.params"),
                            clean = FALSE,
                            reinit = FALSE,
                            quiet = FALSE) {
  ee_temp <- tempdir()
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  tif_dir <- sprintf("%s/%s.tif", ee_temp, basename(filename))
  write_stars(x, tif_dir)
  gs_uri <- ee_upload_file_to_gcs(x = tif_dir,
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  clean = clean,
                                  reinit = reinit)
  ee_gcs_to_asset(x = x,
                  gs_uri = gs_uri,
                  filename = filename,
                  type = 'image',
                  properties = properties)
  invisible(TRUE)
}

#' @name ee_upload
#' @export
ee_upload.stars_proxy <- function(x, ...,
                                  filename,
                                  bucket = NULL,
                                  properties = NULL,
                                  start_time = '1970-01-01',
                                  end_time = '1970-01-01',
                                  selenium_params = getOption(
                                    "rgee.selenium.params"
                                  ),
                                  clean = FALSE,
                                  reinit = FALSE,
                                  quiet = FALSE) {
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  #x <- x[[1]]
  gs_uri <- ee_upload_file_to_gcs(x = x[[1]],
                                  bucket = bucket,
                                  selenium_params = selenium_params,
                                  reinit = reinit)
  ee_gcs_to_asset(x = x,
                  gs_uri = gs_uri,
                  filename = filename,
                  type = 'image',
                  properties=properties)
  invisible(TRUE)
}
