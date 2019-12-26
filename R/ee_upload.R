#' Upload sf or stars objects into a GEE asset
#'
#' Upload either images or vectors to Google Earth Engine asset
#'
#' @param x Character, sf or stars object to upload into a GEE asset.
#' @param filename Character. Asset destination path, e.g. users/pinkiepie/myponycollection.
#' @param bucket bucketname you are uploading to. See details.
#' @param properties List. Set of parameters to established as a property of an EE object. See details.
#' @param selenium_params List. Optional parameters when bucket is NULL. Parameters for setting selenium. See details.
#' @param quiet Logical. Suppress info message.
#' @param ... ignored
#' @importFrom methods is as
#' @importFrom sf write_sf
#' @details
#' It is necessary, for uploading process, get authorization to read & write into a Google Cloud Storage
#' (GCS) bucket. Earth Engine provides a provisional for free space into GCS through
#' gs://earthengine-uploads/. If the bucket argument is absent, this function will use Selenium driver
#' for getting access to the URI mentioned bellow, see \link{ee_upload_file_to_gcs} for details.
#' Install and check the Selenium drivers for Google Chrome is possible as follow:\cr
#' - rgee::ee_install_drivers()\cr
#' - rgee::ee_check_drivers()\cr
#'
#' The properties argument is just available for image uploads, is you are interesting in setting
#' properties in FeatureCollection. Please use \link{ee_manage_set_properties} after \link{ee_upload}.
#'
#' The selenium_params argument is a three-element list consisting of:\cr
#'  - gmail_account: The google account. If it is not specified, it will obtained from
#'  ee$data$getAssetRoots().\cr
#'  - showpassword: Logical. After put the google account into \link[getPass]{getPass}, should be shown?.
#'  - cache: Logical. TRUE will use the cookies saved on the /temp directory.
#' @name ee_upload
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' library(sf)
#'
#' ee_Initialize(user_gmail = "csaybar")
#' ee_check_drivers()
#'
#' filename <- "users/csaybar/rgee_upload/"
#' ee_manage_create(filename)
#'
#' tif = system.file("tif/geomatrix.tif", package = "stars")
#' geomatrix = read_stars(tif) %>% st_warp(crs=st_crs(4326))
#' delta_geomatrix <- c(attr(geomatrix,'dimensions')$x$delta,attr(geomatrix,'dimensions')$y$delta*-1)
#' plot(geomatrix)
#'
#' ee_upload(x = geomatrix,filename = paste0(filename,"geomatrix"))
#' ee_geomatrix <- ee$Image(paste0(filename,"geomatrix"))
#' geomatrix_stars <- ee_as_thumbnail(x = ee_geomatrix,
#'                                    scale = delta_geomatrix,
#'                                    vizparams = list(min = 0, max = 255))
#' geomatrix_stars[geomatrix_stars<=0]=NA
#' names(geomatrix_stars) <- 'geomatrix.tif'
#' plot(geomatrix_stars)
#'
#' #ee_manage_delete(filename) # Remove the folder created at the beginning
#'
#' nc <- st_read(system.file("shp/arequipa.shp", package="rgee"))
#' nc_s <- suppressWarnings(st_simplify(nc, preserveTopology = TRUE, dTolerance = 0.05))
#' ee_upload(x = nc_s,filename = paste0(filename,"arequipa"))
#' ee_monitoring()
#' ee_manage_set_properties(path_asset = paste0(filename,"arequipa"),
#'                          properties = list(message='hello-world',language = 'R'))
#' }
#' @export
ee_upload <- function(x, ...) {
  UseMethod("ee_upload")
}

#' @name ee_upload
#' @export
ee_upload.character <- function(x, ... ,
                                filename,
                                bucket = NULL,
                                properties = getOption("rgee.upload.properties"),
                                selenium_params = getOption("rgee.selenium.params"),
                                quiet = FALSE) {
  user_gmail <- getOption("rgee.selenium.params")$user_gmail
  if (is.null(user_gmail)) {
    stop('ee_upload needs that "user_gmail" ',
         'argument be specified in ee$Initialize().',
         "\nExample: ee_Initialize(user_gmail = 'XXXX@gmail.com')")
  }

  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
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
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
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
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  tif_dir <- sprintf("%s/%s.tif", ee_temp, basename(filename))
  write_stars(x, tif_dir)
  gs_uri <- ee_upload_file_to_gcs(x = tif_dir,
                                  bucket = bucket,
                                  selenium_params = selenium_params)
  ee_gcs_to_asset(gs_uri, filename, type = 'image', properties=properties)
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
  filename <- ee_verify_filename(path_asset = filename,strict = FALSE)
  tif_dir <- sprintf("%s/%s.tif", ee_temp, basename(filename))
  x <- x[[1]]
  gs_uri <- ee_upload_file_to_gcs(x, bucket = bucket, selenium_params = selenium_params)
  ee_gcs_to_asset(gs_uri, filename, type = 'image' , properties=properties)
  return(TRUE)
}
