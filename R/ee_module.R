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


#' Extra Earth Engine dictionary
#'
#' Interface to Extra Earth Engine module. Provides access to the top level
#' classes and functions as well as sub-modules (e.g. \code{ee$Image},
#' \code{ee$FeatureCollection$first}, etc.).
#'
#' @format Earth Engine module
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(rgeeExtra)
#'
#' # ee_install_extra()
#' ee_Initialize()
#'
#' ee_img1 <- ee$Image(0)
#' ee_img2 <- ee$Image(0)
#'
#' ee_img3 <- ee_img1 * ee_img2 +  ee_img2
#' print(ee_img3)
#' }
#' @export
.__Extra__ <- NULL


#' Extra Earth Engine module
#'
#' Interface to Extra Earth Engine module. Provides access to the top level
#' classes and functions as well as sub-modules (e.g. \code{ee$Image},
#' \code{ee$FeatureCollection$first}, etc.).
#'
#' @format Earth Engine module
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(rgeeExtra)
#'
#' # ee_install_extra()
#' ee_Initialize()
#'
#' ee_img1 <- ee$Image(0)
#' ee_img2 <- ee$Image(0)
#'
#' ee_img3 <- ee_img1 * ee_img2 +  ee_img2
#' print(ee_img3)
#' }
#' @export
.__Extra_module__ <- NULL
