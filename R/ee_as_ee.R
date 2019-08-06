#' Convert foreign R spatial object to an Earth Engine object
#' @param x objet to be converted into an Earth Engine object.
#' @importFrom geojsonio geojson_json
#' @importFrom geojsonio geojson_list
#' @export
ee_as_ee = function(x, ...) UseMethod("ee_as_ee")

#' @name ee_as_ee
#' @param ... additional \code{\link[sf]{st_as_sf.dataframe}} arguments could be passed.
#' @param coords in case of point data: names or numbers of the numeric columns holding coordinates
#' @export
ee_as_ee.data.frame <- function(x, coords, ...) {
  eex <- st_as_sf(x,coords=coords,...)
  geojson_list <- geojson_list(eex)$features
  ee$FeatureCollection(geojson_list)
}

#' @name ee_as_ee
#' @export
ee_as_ee.character <- function(x,...){
  oauth_func_path <- system.file("Python/ee_as_ee.py", package = "rgee")
  ee_source_python(oauth_func_path)
  eex <- st_read(x, quiet=TRUE, ...)
  geojson_list <- geojson_json(eex)
  ee_as_ee_py(geojson_list)
}

#' @name ee_as_ee
#' @export
ee_as_ee.sf <- function(x){}

#' @name ee_as_ee
#' @export
ee_as_ee.sfc <- function(x){}

#' @name ee_as_ee
#' @export
ee_as_ee.sp <- function(x){}
