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


#' Wrap an R function in a Python function with the same signature.
#' @author Yuan Tang and J.J. Allaire
#'
#' @description This function could wrap an R function in a Python
#' function with the same signature. Note that the signature of the
#' R function must not contain esoteric Python-incompatible constructs.
#'
#' @note \code{\link[reticulate]{py_func}} has been renamed to ee_utils_pyfunc
#' just to maintain the rgee functions name's style. All recognition
#' for this function must always be given to \pkg{reticulate}.
#' @return A Python function that calls the R function `f` with the same
#' signature.
#' @param f An R function
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # Earth Engine List
#' ee_SimpleList <- ee$List$sequence(0, 12)
#' ee_NewList <- ee_SimpleList$map(
#'   ee_utils_pyfunc(
#'     function(x) {
#'       ee$Number(x)$add(x)
#'     }
#'   )
#' )
#'
#' ee_NewList$getInfo()
#'
#' # Earth Engine ImageCollection
#' constant1 <- ee$Image(1)
#' constant2 <- ee$Image(2)
#' ee_ic <- ee$ImageCollection(c(constant2, constant1))
#' ee_newic <- ee_ic$map(
#'   ee_utils_pyfunc(
#'     function(x) ee$Image(x)$add(x)
#'   )
#' )
#' ee_newic$mean()$getInfo()$type
#' }
#' @export
ee_utils_pyfunc <- reticulate::py_func
