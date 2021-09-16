#' Main Earth Engine module
#'
#' Interface to main Earth Engine module. Provides access to the top level
#' classes and functions as well as sub-modules (e.g. \code{ee$Image},
#' \code{ee$FeatureCollection$first}, etc.).
#'
#' @format Earth Engine module
#'
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize()
#'
#' ee_img <- ee$Image(0)
#' ee_ic <- ee$ImageCollection(ee_img)
#'
#' print(ee_img$getInfo())
#' print(ee_ic$getInfo())
#' }
#' @export
ee <- NULL
