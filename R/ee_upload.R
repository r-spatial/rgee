#' Upload sf or stars objects into a GEE asset
#'

#' Upload either images or vectors to Google Earth Engine asset
#'
#' @param x Character, sf or stars object to upload into a GEE asset.
#' @param filename Character. Asset destination path, e.g. users/pinkiepie/myponycollection.
#' @param bucket bucketname you are uploading to. See details.
#' @param properties List. Set of parameters to established as a property of an EE object. See details.
#' @param selenium_params TODO
#' @param quiet Logical. Suppress info message.
#' @param ... ignored
#' @importFrom methods is as
#' @importFrom sf write_sf
#' @details  TALKING ABOUT SELENIUM
#' @name ee_upload
#' @examples
#' library(rgee)
#' library(stars)
#' library(sf)
#' ee_Initialize(user_gmail = 'csaybar@gmail.com')
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
#' \dontrun{
#' nc <- st_read(system.file("shp/arequipa.shp", package="rgee"))
#' nc_s <- suppressWarnings(st_simplify(nc, preserveTopology = TRUE, dTolerance = 0.05))
#' ee_upload(x = nc_s,filename = paste0(filename,"arequipa"))
#' ee_monitoring()
#' help(ee_upload)
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
  gs_uri <- ee_upload_file_to_gcs(tif_dir, bucket = bucket, selenium_params = selenium_params)
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
