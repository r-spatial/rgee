#' Convert an sf object to EE object
#'
#' @name sf_as_ee
#' @param x sf object to be converted into a EE object.
#' @param check_ring_dir logical. See \link[sf]{st_read} and details.
#' @param ... \link[sf]{st_read} arguments might be included.
#' @importFrom sf st_read st_sf st_sfc
#' @importFrom geojsonio geojson_json
#' @details
#' The conversion from sf to EE is a two-step process. First,
#' \code{sf_as_ee} transform sf objects into a GeoJSON format using
#' \link[geojsonio]{geojson_json}. Second, the GeoJSON generated will be
#' encrusted in an HTTP request using the server-side objects (ee$Geometry$*).
#' If the sf object is a large spatial object (>1Mb) it is likely to cause
#' bottlenecks and plodding connections. See
#' \href{https://developers.google.com/earth-engine/client_server}{Client
#' vs Server} documentation for more details. For dealing with very large spatial
#' objects, it is recommended to import it into the GEE asset. See
#' \link[rgee]{ee_upload} for creating uploaded pipelines.
#'
#' Earth Engine is strict on polygon ring directions (outer ring
#' counter-clockwise, and the inner one clockwise). If `check_ring_dir` is TRUE,
#' it check every ring, and revert them if necessary, to counter clockwise for outer,
#' and clockwise for inner (hole) ones. By default this is FALSE because
#' it is an expensive operation.
#'
#' @examples
#' library(rgee)
#' library(sf)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # sf
#' x <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
#'   st_transform(4326)
#' ee_x <- sf_as_ee(x, check_ring_dir = TRUE)
#' Map$centerObject(eeObject = ee_x)
#' Map$addLayer(ee_x)
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
  x <- st_sf(x, check_ring_dir = check_ring_dir)
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
  # message("EPSG:4326 assigned as a coordinate reference system")
  x <- st_sfc(x, check_ring_dir = check_ring_dir)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  geojson_list <- geojson_json(x)
  sf_as_ee$sfg_as_ee_py(geojson_list)
}
