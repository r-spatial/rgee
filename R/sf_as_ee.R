#' Convert an sf object to EE object
#'
#' @name sf_as_ee
#' @param x sf object to be converted into a EE object.
#' @param check_ring_dir logical. See \link[sf]{st_read} and details.
#' @param ... \link[sf]{st_read} arguments might be included.
#' @importFrom sf st_read st_sf st_sfc
#' @importFrom geojsonio geojson_json
#' @details
#' The conversion from sf to EE is a two-step process. Firstly, \code{sf_as_ee} transform
#' sf objects into a GeoJSON format using \link[geojsonio]{geojson_json}. Secondly, the GeoJSON
#' generated will be encrusted in an HTTP request using the server-side objects (ee$Geometry$*).
#' If the sf object is large spatial objects (>1Mb) it is expected that cause bottlenecks and
#' plodding connections. See
#' \href{https://developers.google.com/earth-engine/client_server#client-and-server-functions}{Client vs Server}
#' documentation for details. For leading with very large spatial objects, is recommended firstly
#' importing it to the GEE assets. See \link[rgee]{ee_upload} for creating uploading pipelines.
#'
#' Earth Engine is strict with polygon ring directions (exterior ring counter-clockwise, holes clockwise). When
#' `check_ring_dir` is TRUE, it check every ring and revert them to counter clockwise for outer, and clockwise for
#' inner (hole) rings. By default this is FALSE because it is an expensive operation.
#'
#' @examples
#' library(rgee)
#' library(sf)
#'
#' ee_reattach() # reattach ee as a reserve word
#' ee_Initialize()
#'
#' # sf
#' x <- st_read(system.file("shape/nc.shp", package = "sf")) %>% st_transform(4326)
#' ee_x <- sf_as_ee(x, check_ring_dir = TRUE)
#' ee_map(ee_x)
#'
#' # sfc
#' x <- st_read(system.file("shape/nc.shp", package = "sf"))$geometry %>% st_transform(4326)
#' ee_x <- sf_as_ee(x, check_ring_dir = TRUE)
#' ee_map(ee_x)
#'
#' # sfg
#' x <- st_read(system.file("shape/nc.shp", package = "sf"))$geometry[[1]]
#' ee_x <- sf_as_ee(x, check_ring_dir = TRUE)
#' ee_map(ee_x)
#' @export
sf_as_ee <- function(x, check_ring_dir) UseMethod("sf_as_ee")

#' @rdname sf_as_ee
#' @export
sf_as_ee.character <- function(x, check_ring_dir = FALSE, ...) {
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  eex <- st_read(x, check_ring_dir = check_ring_dir, quiet = TRUE, ...)
  if (!st_crs(eex)$epsg == 4326) {
    stop("The x EPSG needs to be 4326, use sf::st_transform.")
  }
  geojson_list <- geojson_json(eex)
  sf_as_ee$sf_as_ee_py(geojson_list)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sf <- function(x, check_ring_dir = FALSE) {
  x <- st_sf(x, check_ring_dir = TRUE)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  if (!st_crs(x)$epsg == 4326) {
    stop("The x EPSG needs to be 4326, use sf::st_transform.")
  }
  geojson_list <- geojson_json(x)
  sf_as_ee$sf_as_ee_py(geojson_list)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sfc <- function(x, check_ring_dir = FALSE) {
  x <- st_sfc(x, check_ring_dir = check_ring_dir)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  if (!st_crs(x)$epsg == 4326) {
    stop("The x EPSG needs to be 4326, use sf::st_transform.")
  }
  geojson_list <- geojson_json(x)
  sf_as_ee$sfc_as_ee_py(geojson_list)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sfg <- function(x, check_ring_dir = FALSE) {
  # message("EPSG:4326 assigned as an coordinate reference system")
  x <- st_sfc(x, check_ring_dir = check_ring_dir)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  geojson_list <- geojson_json(x)
  sf_as_ee$sfg_as_ee_py(geojson_list)
}
