#' Convert between Python and R objects
#' @param x A python object
#' @export
ee_utils_py_to_r <- function(x) {
  p_r <- suppressWarnings(try(py_to_r(x),silent = TRUE))
  if (class(p_r) %in% 'try-error') {
    return(x)
  } else {
    return(p_r)
  }
}


#' Create a zip file from a sf object
#'
#' @param x sf object
#' @param filename data source name
#' @param SHP_EXTENSIONS file extension of the files to save
#' into the zip file. By default: "dbf", "prj", "shp", "shx".
#' @importFrom utils zip
#' @importFrom sf write_sf
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # Create sf object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
#'
#' # Method 1
#' # 1. Pass the sf to a zip file
#' zipfile <- ee_utils_shp_to_zip(nc)
#'
#' # 2. From local to gcs
#' gs_uri <- local_to_gcs(x = zipfile, bucket = 'rgee_dev')
#'
#' # 3. Pass the sf to a zip file
#' gcs_to_ee_table(
#'   gs_uri = gs_uri,
#'   assetId = assetId
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_sf_01 <- ee$FeatureCollection(assetId)
#' Map$centerObject(ee_sf_01)
#' Map$addLayer(ee_sf_01)
#'
#' # Method 2
#' ee_sf_02 <- sf_as_ee(x = nc,
#'                      assetId = assetId,
#'                      bucket = "rgee_dev")
#' Map$centerObject(ee_sf_02)
#' Map$addLayer(ee_sf_02)
#' }
#' @export
ee_utils_shp_to_zip <- function(x,
                                filename,
                                SHP_EXTENSIONS = c("dbf", "prj", "shp", "shx")) {
  if (missing(filename)) {
    filename <- sprintf("%s%s",tempfile(),'.shp')
  }
  write_sf(obj = x, dsn = filename)
  shp_basename <- gsub("\\.shp$", "", filename)
  shp_filenames <- sprintf("%s.%s", shp_basename, SHP_EXTENSIONS)
  zipname <- sprintf("%s.zip", shp_basename)
  zip(zipfile = zipname, files = shp_filenames, flags = "-j -q")
  zipname
}
