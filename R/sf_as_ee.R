#' Convert an sf object to EE object
#'
#' @name sf_as_ee
#' @param x sf object to be converted into a EE object.
#' @param check_ring_dir logical. See \link[sf]{st_read} for details.
#' @param evenOdd If TRUE, polygon interiors will be determined
#' by the even/odd rule, where a point is inside if it crosses
#' an odd number of edges to reach a point at infinity. Otherwise
#' polygons use the left-inside rule, where interiors are on the
#' left side of the shell's edges when walking the vertices in
#' the given order. If unspecified, defaults to TRUE.
#' *args: For convenience, varargs may be used when all
#' arguments are numbers. This allows creating geodesic
#' EPSG:4326 Polygons with a single LinearRing given an even
#' number of arguments, e.g.
#' ee.Geometry.Polygon(aLng, aLat, bLng, bLat, ..., aLng, aLat).
#' @param proj An optional projection specification, either as a CRS ID
#' code or as a WKT string. If specified, overrides any CRS found in
#' the GeoJSON parameter. If unspecified and the GeoJSON does not
#' declare a CRS, defaults to "EPSG:4326" (x=longitude, y=latitude).
#' @param geodesic Whether line segments should be interpreted as spherical
#' geodesics. If FALSE, indicates that line segments should be interpreted
#' as planar lines in the specified CRS. If absent, defaults to TRUE if
#' the CRS is geographic (including the default EPSG:4326), or to FALSE
#' if the CRS is projected.
#' @param ... \link[sf]{st_read} arguments might be included.
#' @importFrom sf st_read st_sf st_sfc st_is_longlat
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
#' \dontrun{
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
#'
#' # Create a right-inside polygon.
#' toy_poly <- matrix(data = c(-35,-10,-35,10,35,10,35,-10,-35,-10),
#'                    ncol = 2,
#'                    byrow = TRUE) %>%
#'   list() %>%
#'   st_polygon()
#' holePoly <- sf_as_ee(toy_poly, evenOdd = FALSE)
#'
#' # Create an even-odd version of the polygon.
#' evenOddPoly <- sf_as_ee(toy_poly, evenOdd = TRUE)
#'
#' # Create a point to test the insideness of the polygon.
#' pt <- ee$Geometry$Point(c(1.5, 1.5))
#'
#' # Check insideness with a contains operator.
#' print(holePoly$contains(pt)$getInfo() %>% ee_py_to_r())     # FALSE
#' print(evenOddPoly$contains(pt)$getInfo() %>% ee_py_to_r())  # TRUE
#' }
#' @export
sf_as_ee <- function(x, check_ring_dir,evenOdd, proj, geodesic) UseMethod("sf_as_ee")

#' @rdname sf_as_ee
#' @export
sf_as_ee.character <- function(x,
                               check_ring_dir = FALSE,
                               evenOdd = TRUE,
                               proj = 4326,
                               geodesic = TRUE,
                               ...) {
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  eex <- st_read(dsn = x,
                 stringsAsFactors =  FALSE,
                 check_ring_dir = check_ring_dir,
                 quiet = TRUE, ...)
  if (proj != 4326) {
    eex <- eex %>% st_transform(proj)
  }
  eex_crs <- st_crs(eex)$epsg
  if (is.na(eex_crs)) {
    stop("The x EPSG needs to be defined, use sf::st_set_crs to",
         " set, replace or retrieve.")
  }
  fc <- list()
  for (index in seq_len(nrow(eex))) {
    feature <- eex[index,]
    py_geometry <- geojson_json(feature$geometry,type = 'skip')
    ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
                                         opt_proj = paste0('EPSG:', eex_crs),
                                         opt_geodesic = geodesic,
                                         opt_evenOdd = evenOdd)
    feature$geometry <- NULL
    fc[[index]] <- ee$Feature(ee_geometry, as.list(feature))
  }
  ee$FeatureCollection(fc)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sf <- function(x,
                        check_ring_dir = FALSE,
                        evenOdd = TRUE,
                        proj = 4326,
                        geodesic = TRUE,
                        ...) {
  if (proj != 4326) {
    x <- x %>% st_transform(proj)
  }
  x <- st_sf(x, check_ring_dir = check_ring_dir)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  eex_crs <- st_crs(x)$epsg
  if (is.na(eex_crs)) {
    stop("The x EPSG needs to be defined, use sf::st_set_crs to",
         " set, replace or retrieve.")
  }
  fc <- list()
  for (index in seq_len(nrow(x))) {
    feature <- x[index,]
    py_geometry <- geojson_json(feature$geometry,type = 'skip')
    ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
                                         opt_proj = paste0('EPSG:', eex_crs),
                                         opt_geodesic = st_is_longlat(x),
                                         opt_evenOdd = evenOdd)
    feature$geometry <- NULL
    fc[[index]] <- ee$Feature(ee_geometry, as.list(feature))
  }
  ee$FeatureCollection(fc)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sfc <- function(x,
                         check_ring_dir = FALSE,
                         evenOdd = TRUE,
                         proj = 4326,
                         geodesic = TRUE,
                         ...) {
  if (proj != 4326) {
    x <- x %>% st_transform(proj)
  }
  x <- st_sfc(x, check_ring_dir = check_ring_dir)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  eex_crs <- st_crs(x)$epsg

  if (is.na(eex_crs)) {
    stop("The x EPSG needs to be defined, use sf::st_set_crs to",
         " set, replace or retrieve.")
  }

  fc <- list()
  for (index in seq_len(length(x))) {
    geometry <- x[index]
    py_geometry <- geojson_json(geometry,type = 'skip')
    ee_geometry <- sf_as_ee$sfg_as_ee_py(x = py_geometry,
                                         opt_proj = paste0('EPSG:', eex_crs),
                                         opt_geodesic = geodesic,
                                         opt_evenOdd = evenOdd)
    fc[[index]] <- ee$Feature(ee_geometry, NULL)
  }
  ee$FeatureCollection(fc)
}

#' @rdname sf_as_ee
#' @export
sf_as_ee.sfg <- function(x,
                         check_ring_dir = FALSE,
                         evenOdd = TRUE,
                         proj = 4326,
                         geodesic = TRUE,
                         ...) {
  x <- st_sfc(x, crs =  proj, check_ring_dir = check_ring_dir)
  geodesic <- st_is_longlat(x)
  oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
  sf_as_ee <- ee_source_python(oauth_func_path)
  geojson_list <- geojson_json(x)
  sf_as_ee$sfg_as_ee_py(x = geojson_list,
                        opt_proj = paste0('EPSG:', proj),
                        opt_geodesic = geodesic,
                        opt_evenOdd = evenOdd)
}
