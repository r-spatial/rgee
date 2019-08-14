#' Convert foreign R spatial object to an Earth Engine object
#' @param x objet to be converted into an Earth Engine object.
#' @param ... if x is a character additional \code{\link[sf]{st_read}} arguments could be passed.
#' @importFrom sf st_as_sf
#' @importFrom geojsonio geojson_json
#' @importFrom geojsonio geojson_list
#' @examples
#' \dontrun{
#' library(rgee)
#' library(raster)
#' ee_initialize()
#'
#' # character
#' x <- system.file("shape/nc.shp", package="sf")
#' ee$FeatureCollection(ee_as_ee(x),proj="EPSG:4326")
#'
#' # matrix
#' ee_df <- matrix(rnorm(100),ncol = 5) %>% as.data.frame()
#' ee_df <- ee_as_ee(ee_df,c("V1","V2"))
#'
#' ee_df$geometry()
#' # sf
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))
#' st_crs
#' ee_as_ee(x)
#'
#' #sfc
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))$geometry
#' ee_as_ee(x)
#'
#' #sfg
#' x <- read_sf(system.file("shape/nc.shp", package="sf"))$geometry[[1]]
#' ee_as_ee(x)
#'
#' #sp
#' x <- shapefile(system.file("shape/nc.shp", package="sf"))
#' ee_as_ee(x)
#' }
#' @export
ee_as_ee <- function(x, ...) UseMethod("ee_as_ee")

#' @name ee_as_ee
#' @param coords in case of point data: names or numbers of the numeric columns holding coordinates
#' @export
ee_as_ee.data.frame <- function(x, coords, ...) {
  eex <- st_as_sf(x, coords = coords)
  geojson_list <- geojson_list(eex)$features
  ee$FeatureCollection(geojson_list)
}

#' @name ee_as_ee
#' @importFrom sf st_read
#' @export
ee_as_ee.character <- function(x, ...) {
  oauth_func_path <- system.file("Python/ee_as_ee.py", package = "rgee")
  ee_source_python(oauth_func_path)
  eex <- st_read(x, quiet = TRUE, ...)
  if (!st_crs(eex)$epsg == 4326){
    eex <- st_transform(eex, 4326)
  }
  geojson_list <- geojson_json(eex)
  ee_sf_as_ee_py(geojson_list)
}

#' @name ee_as_ee
#' @export
ee_as_ee.sf <- function(x,...) {
  oauth_func_path <- system.file("Python/ee_as_ee.py", package = "rgee")
  ee_source_python(oauth_func_path)
  if (!st_crs(x)$epsg == 4326) x <- st_transform(x, 4326)
  geojson_list <- geojson_json(x)
  ee_sf_as_ee_py(geojson_list)
}

#' @name ee_as_ee
#' @export
ee_as_ee.sfc <- function(x,...) {
  oauth_func_path <- system.file("Python/ee_as_ee.py", package = "rgee")
  ee_source_python(oauth_func_path)
  if (!st_crs(x)$epsg == 4326) x <- st_transform(x, 4326)
  geojson_list <- geojson_json(x)
  ee_sfc_as_ee_py(geojson_list)
}

#' @name ee_as_ee
#' @export
ee_as_ee.sfg <- function(x,...) {
  oauth_func_path <- system.file("Python/ee_as_ee.py", package = "rgee")
  ee_source_python(oauth_func_path)
  geojson_list <- geojson_json(x)
  ee_sfg_as_ee_py(geojson_list)
}

#' @name ee_as_ee
#' @export
ee_as_ee.Spatial <- function(x,...) {
  ee_source_python(oauth_func_path)
  x <- as(x,"sf")
  if (!st_crs(x)$epsg == 4326) x <- st_transform(x, 4326)
  geojson_list <- geojson_json(x)
  ee_sf_as_ee_py(geojson_list)
}
