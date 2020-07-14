#' Convert between Python and R objects
#' @param x A python object
#' @family ee_utils functions
#' @return An R object
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
#'
#' @return Character. The full path of the zip file created.
#' @family ee_utils functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # Create sf object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' zipfile <- ee_utils_shp_to_zip(nc)
#' }
#' @export
ee_utils_shp_to_zip <- function(x,
                                filename,
                                SHP_EXTENSIONS = c("dbf", "prj", "shp",
                                                   "shx")) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  if (missing(filename)) {
    filename <- sprintf("%s%s",tempfile(),'.shp')
  }
  sf::write_sf(obj = x, dsn = filename)
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
#'
#' @family ee_utils functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
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
