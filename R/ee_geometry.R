ee_as_eegeom = function(x, ...) UseMethod("st_as_sf")

#' Convert foreign geometry object to an Earth Engine object
#' @param x objet to be converted into an ee_Geometry.
#' @importFrom geojsonio geojson_json
#' @export
ee_as_eegeom <- function(x) {
  oauth_func_path <- system.file("Python/ee_aux.py", package = "rgee")
  ee_source_python(oauth_func_path)
  geojson_sf <- geojson_json(x)
  ee_geometry2_py(geojson_sf)
}
