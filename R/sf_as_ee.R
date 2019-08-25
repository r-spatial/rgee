#' Convert R spatial object to an Google Earth Engine
#'
#' @name sf_as_ee
#' @param x object to be converted into.
#' @param ... if x is a character additional \code{\link[sf]{st_read}} arguments could be passed.
#' @importFrom sf st_as_sf read_sf write_sf
#' @importFrom geojsonio geojson_json geojson_list
#' @importFrom sf st_transform
#' @details
#' \code{sf_as_ee} try to transform R objects (sf) into a GeoJSON format. By the time a user sends an HTTP
#' request to the Earth Engine Web REST APIs (by, e.g. using *$getInfo()), the GeoJSON will be
#' encrusted to the message. Therefore, it is expected that large spatial objects (>500Kb) cause
#' bottlenecks and plodding connections. See
#' \href{https://developers.google.com/earth-engine/client_server#client-and-server-functions}{Client vs Server}
#' documentation for details. For leading with very large spatial objects, is a good practice firstly
#' importing it to the GEE assets. See \code{\link[rgee]{ee_upload_toasset}} for details.
#'
#' @examples
#' \dontrun{
#' library(rgee)
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
sf_as_ee <- function(x, ...) UseMethod("sf_as_ee")

#' @rdname sf_as_ee
#' @importFrom sf st_read
#' @export
sf_as_ee.character <- function(x, ...) {
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  eex <- st_read(x, quiet = TRUE, ...)
  if (!st_crs(eex)$epsg == 4326){
    eex <- st_transform(eex, 4326)
  }
  geojson_list <- geojson_json(eex)
  sf_as_ee$sf_as_ee_py(geojson_list)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sf <- function(x,...) {
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  if (!st_crs(x)$epsg == 4326) x <- st_transform(x, 4326)
  geojson_list <- geojson_json(x)
  sf_as_ee$sf_as_ee_py(geojson_list)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sfc <- function(x,...) {
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  if (!st_crs(x)$epsg == 4326) x <- st_transform(x, 4326)
  geojson_list <- geojson_json(x)
  sf_as_ee$sfc_as_ee_py(geojson_list)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sfg <- function(x,...) {
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  geojson_list <- geojson_json(x)
  sf_as_ee$sfg_as_ee_py(geojson_list)
}

